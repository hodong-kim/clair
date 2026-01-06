-- clair-event_loop.adb
-- Copyright (c) 2021-2026 Hodong Kim <hodong@nimfsoft.art>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Clair.Errno;
with Clair.Log;
with Clair.Exceptions;
with Ada.Exceptions;
with System.Storage_Elements;

package body Clair.Event_Loop is
  use type Interfaces.C.short;
  use type Interfaces.C.unsigned_short;
  use type Clair.Platform.int64_t;
  use type Interfaces.C.long;
  use type Clair.Time.time_t;
  use type Clair.Time.Timespec;
  -- kqueue filter constants
  EVFILT_READ   : constant Interfaces.C.short := -1;
  EVFILT_WRITE  : constant Interfaces.C.short := -2;
  EVFILT_SIGNAL : constant Interfaces.C.short := -6;
  EVFILT_TIMER  : constant Interfaces.C.short := -7;
  -- kqueue flag constants
  EV_ADD     : constant Interfaces.C.unsigned_short := 16#0001#;
  EV_DELETE  : constant Interfaces.C.unsigned_short := 16#0002#;
  EV_ENABLE  : constant Interfaces.C.unsigned_short := 16#0004#;
  EV_ONESHOT : constant Interfaces.C.unsigned_short := 16#0010#;
  EV_CLEAR   : constant Interfaces.C.unsigned_short := 16#0020#;
  EV_RECEIPT : constant Interfaces.C.unsigned_short := 16#0040#;
  EV_EOF     : constant Interfaces.C.unsigned_short := 16#8000#;
  EV_ERROR   : constant Interfaces.C.unsigned_short := 16#4000#;

  type Kevent_Ext_Array is array (1 .. 4) of Clair.Platform.uint64_t
  with convention => c;

  type Kevent_Type is record
    ident  : Clair.Platform.uintptr_t    := 0;
    filter : Interfaces.C.short          := 0;
    flags  : Interfaces.C.unsigned_short := 0;
    fflags : Interfaces.C.unsigned       := 0;
    data   : Clair.Platform.int64_t      := 0;
    udata  : System.Address              := System.NULL_ADDRESS;
    ext    : Kevent_Ext_Array            := [others => 0];
  end record with convention => c;

  for Kevent_Type use record
    ident  at Clair.Platform.KEVENT_IDENT_OFFSET
           range 0 .. Clair.Platform.uintptr_t'size - 1;
    filter at Clair.Platform.KEVENT_FILTER_OFFSET
           range 0 .. Interfaces.C.short'size - 1;
    flags  at Clair.Platform.KEVENT_FLAGS_OFFSET
           range 0 .. Interfaces.C.unsigned_short'size - 1;
    fflags at Clair.Platform.KEVENT_FFLAGS_OFFSET
           range 0 .. Interfaces.C.unsigned'size - 1;
    data   at Clair.Platform.KEVENT_DATA_OFFSET
           range 0 .. Clair.Platform.int64_t'size - 1;
    udata  at Clair.Platform.KEVENT_UDATA_OFFSET
           range 0 .. Standard'address_size - 1;
    ext    at Clair.Platform.KEVENT_EXT_OFFSET
           range 0 .. (Clair.Platform.uint64_t'size * 4) - 1;
  end record;

  for Kevent_Type'size use Clair.Platform.KEVENT_SIZE * System.Storage_Unit;
  for Kevent_Type'alignment use Clair.Platform.KEVENT_ALIGNMENT;

  type Kevent_Array is array (Positive range <>) of Kevent_Type
  with convention => c;

  function kqueuex (flags : Interfaces.C.unsigned) return Interfaces.C.int
  with import, convention => c, external_name => "kqueuex";

  function kevent_c (kq         : Interfaces.C.int;
                     changelist : System.Address;
                     nchanges   : Interfaces.C.int;
                     eventlist  : System.Address;
                     nevents    : Interfaces.C.int;
                     timeout    : access constant Clair.Time.Timespec)
  return Interfaces.C.int
  with import, convention => c, external_name => "kevent";

  function close (Fd : Interfaces.C.int) return Interfaces.C.int
  with import, convention => c, external_name => "close";

  function handle_to_addr is new Ada.Unchecked_Conversion
    (source => Handle, target => System.Address);

  function addr_to_handle is new Ada.Unchecked_Conversion
    (source => System.Address, target => Handle);

  function handle_to_uintptr is new Ada.Unchecked_Conversion
    (source => Handle, target => Clair.Platform.uintptr_t);

  procedure free_context is new Ada.Unchecked_Deallocation
    (object => Context, name => Context_Access);

  procedure free_source (src : Handle) is
    use type System.Storage_Elements.Storage_Offset;
  begin
    Clair.Arena.deallocate (source_pool,
                            handle_to_addr (src),
                            src.all'size / System.Storage_Unit,
                            System.Storage_Unit);
  end free_source;

  procedure retain (src : Handle) is
  begin
    src.ref_count := src.ref_count + 1;
  end retain;

  procedure release (self : in out Context;
                     src  : Handle) is
  begin
    src.ref_count := src.ref_count - 1;

    if src.ref_count = 0 then
      if self.call_depth > 0 then
        src.next_garbage  := self.garbage_head;
        self.garbage_head := src;
      else
        free_source (src);
      end if;

    end if;
  end release;

  procedure initialize (self : in out Context) is
    fd : Interfaces.C.int;
  begin
    fd := kqueuex (Clair.Platform.KQUEUE_CLOEXEC);
    if fd < 0 then
      Clair.Errno.raise_from_errno (Clair.Errno.get_errno,
                                    "Failed to create kqueue instance");
    end if;
    Clair.Arena.initialize (source_pool);
    self.fd := Clair.File.Descriptor(fd);
    self.is_running := False;
    self.cached_timeout := INFINITE;
  end initialize;

  -- [Helper] Append to the Active list (Watch/Timer/Signal)
  procedure link_active (self : in out Context; src : Handle) is
  begin
    if self.active_tail /= null then
      self.active_tail.next_source := src;
      src.prev_source := self.active_tail;
    else
      self.active_head := src;
    end if;
    self.active_tail := src;
    src.next_source := null;
  end link_active;

  -- [Helper] Unlink from the Active list
  procedure unlink_active (self : in out Context; src : Handle) is
  begin
    if src.prev_source /= null then
      src.prev_source.next_source := src.next_source;
    else
      self.active_head := src.next_source;
    end if;

    if src.next_source /= null then
      src.next_source.prev_source := src.prev_source;
    else
      self.active_tail := src.prev_source;
    end if;

    src.prev_source := null;
    src.next_source := null;
  end unlink_active;

  procedure finalize (self : in out Context) is
    use type Clair.File.Descriptor;
    unused  : Interfaces.C.int;
    pragma unreferenced (unused);
    current : Handle;
    next    : Handle;
  begin
    if self.fd /= Clair.File.INVALID_DESCRIPTOR then
      quit (self);
      unused := close (Interfaces.C.int(self.fd));
      self.fd := Clair.File.INVALID_DESCRIPTOR;
    end if;

    current := self.garbage_head;
    while current /= null loop
      next := current.next_garbage;
      free_source (current);
      current := next;
    end loop;
    self.garbage_head := null;

    current := self.idle_head;
    while current /= null loop
      next := current.next_source;
      free_source (current);
      current := next;
    end loop;

    self.idle_head  := null;
    self.idle_tail  := null;
    self.idle_count := 0;

    -- Cleanup Active List (Watch/Timer/Signal)
    current := self.active_head;
    while current /= null loop
      next := current.next_source;
      free_source (current);
      current := next;
    end loop;
    self.active_head := null;
    self.active_tail := null;
    Clair.Arena.finalize (source_pool);
  end finalize;

  -- Allocate memory on the heap, initialize, and return the pointer
  function create return Context_Access is
    ptr : constant Context_Access := new Context;
  begin
    initialize (ptr.all);
    return ptr;
  end create;

  procedure destroy (self : in out Context_Access) is
  begin
    if self /= null then
      finalize (self.all);
      free_context (self);
    end if;
  end destroy;

  function iterate (self    : in out Context;
                    timeout : Milliseconds := INFINITE) return Boolean is
    -- Timeout-related variables
    ts_access      : access constant Clair.Time.Timespec := null;
    actual_timeout : Milliseconds := timeout;
    -- Define variables for POSIX time calculations
    current_ts     : aliased Clair.Time.Timespec;
    deadline_ts    : Clair.Time.Timespec;
    remaining_ts   : Clair.Time.Timespec;
    -- Event loop related variables
    MAX_EVENTS : constant Interfaces.C.int := 128;
    events     : Kevent_Array(1 .. Integer(MAX_EVENTS));
    pragma suppress_initialization (events);
    -- Cache array address (to avoid recalculation within the loop)
    events_ptr : constant System.Address := events(events'first)'address;
    count      : Interfaces.C.int;
    src        : Handle;
    out_mask   : Event_Mask;
    -- EINTR handling related variables
    retry_count : Natural := 0;
    -- Indicates whether any work (I/O events, timers, or signals) was processed
    -- during the current iteration. Used to determine if the loop should
    -- continue immediately or handle spurious wakeups.
    activity_performed : Boolean := False;
    -- Counter and constant to prevent Spurious Wakeup
    Spurious_Count : Natural := 0;
    MAX_SPURIOUS   : constant Natural := 5;
    -- Constant and variable for Idle batch processing
    MAX_IDLE_BATCH : constant Natural := 64;
    idle_processed : Natural := 0;
  begin
    self.call_depth := self.call_depth + 1;

    -- Adjust timeout (Do not wait if there are Idle tasks)
    if self.idle_count > 0 then
      actual_timeout := IMMEDIATE;
    end if;

    -- Set deadline (if not infinite wait)
    if actual_timeout > 0 then
      -- Measure current time
      if Clair.Time.get_time (Clair.Platform.CLOCK_MONOTONIC,
                              current_ts'address) /= 0 then
        Clair.Errno.raise_from_errno (
          Clair.Errno.get_errno,
          "iterate: clock_gettime failed at initialization"
        );
      end if;

      declare
        -- Separate timeout into seconds and nanoseconds
        add_sec  : constant Clair.Time.time_t :=
          Clair.Time.time_t(actual_timeout / 1000);
        add_nsec : constant Interfaces.C.long :=
          Interfaces.C.long((actual_timeout rem 1000) * 1_000_000);
      begin
        -- Calculate absolute deadline time (simple addition)
        deadline_ts.tv_sec  := current_ts.tv_sec  + add_sec;
        deadline_ts.tv_nsec := current_ts.tv_nsec + add_nsec;
        -- Normalize nanosecond overflow
        Clair.Time.normalize (deadline_ts);
      end;
      -- Set initial wait time
      self.cached_timespec.tv_sec  := Clair.Time.time_t(actual_timeout / 1000);
      self.cached_timespec.tv_nsec :=
        Interfaces.C.long((actual_timeout rem 1000) * 1_000_000);
      ts_access := self.cached_timespec'access;

    elsif actual_timeout = IMMEDIATE then
      self.cached_timespec.tv_sec  := 0;
      self.cached_timespec.tv_nsec := 0;
      ts_access := self.cached_timespec'access;

    else -- INFINITE (< 0)
      ts_access := null;
    end if;

    <<Wait_For_Event>> -- Label for re-waiting
    retry_count := 0;  -- Initialize EINTR counter upon re-entry

    loop
      count := kevent_c (Interfaces.C.int(self.fd),
                         System.NULL_ADDRESS,
                         0,
                         events_ptr,
                         MAX_EVENTS,
                         ts_access);
      if count >= 0 then
        -- Successfully received events or timed out (0) → Exit loop
        exit;
      else
        declare
          errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
        begin

          if errno_code = Clair.Errno.EINTR then

            if actual_timeout <= 0 then
              goto Continue_Retry;
            end if;

            -- Recalculate remaining time
            if Clair.Time.get_time (Clair.Platform.CLOCK_MONOTONIC,
                                    current_ts'address) /= 0 then
              Clair.Errno.raise_from_errno (
                Clair.Errno.get_errno, "clock_gettime failed inside EINTR"
              );
            end if;

            remaining_ts := deadline_ts - current_ts;

            -- Check timeout
            if remaining_ts.tv_sec < 0 then
              count := 0;
              exit;
            end if;

            self.cached_timespec := remaining_ts;

            <<Continue_Retry>>
            retry_count := retry_count + 1;
            if retry_count > Clair.Max_EINTR_Retries then
              raise Clair.Exceptions.Interrupted_System_Call with
                "kevent: Aborted after " & Clair.Max_EINTR_Retries'image &
                " consecutive EINTR signals";
            end if;

          else
            Clair.Errno.raise_from_errno (errno_code, "kevent failed");
          end if;

        end;
      end if;
    end loop;

    -- Process events and execute associated callbacks
    if count > 0 then
      for i in 1 .. count loop
        declare
          pragma Suppress (Index_Check);
          pragma Suppress (Range_Check);
          ev : Kevent_Type renames events(Integer(i));
        begin
          -- The 'data' field contains the system errno code
          if (ev.flags and EV_ERROR) /= 0 then
            Clair.Log.warning ("kevent error (ident:" & ev.ident'image &
                               ", errno:" & ev.data'image & ")");
            goto Continue_Loop; -- Skip processing for the failed event
          end if;
          -- Restore the Source pointer from the user data field
          src := addr_to_handle (ev.udata);

          if src /= null then
            activity_performed := True;
            retain (src);

            begin
              if not src.is_removed then
                case src.kind is
                  when Kind_Watch =>
                    out_mask := 0;

                    if ev.filter = EVFILT_READ then
                      out_mask := out_mask or EVENT_INPUT;
                    elsif ev.filter = EVFILT_WRITE then
                      out_mask := out_mask or EVENT_OUTPUT;
                    end if;

                    if (ev.flags and EV_EOF) /= 0 then
                      out_mask := out_mask or EVENT_HANG_UP;
                    end if;

                    src.io_cb (src,
                               Clair.File.Descriptor(src.ident),
                               out_mask,
                               src.user_data);

                  when Kind_Timer =>
                    -- Timers do not require event mask calculations.
                    src.timer_cb (src, src.user_data);

                  when Kind_Idle =>
                    -- Idle sources are handled in a separate phase.
                    null;

                  when Kind_Signal =>
                    -- A notification from kqueue indicates that the signal is
                    -- pending. Therefore, calling `sigwait` here consumes the
                    -- signal immediately without blocking.
                    declare
                      -- Prevent double initialization of sigset_t
                      local_set : aliased Clair.Signal.Set;
                      pragma suppress_initialization (local_set);
                      taken_sig : aliased Clair.Signal.Number;
                      pragma suppress_initialization (taken_sig);
                    begin
                      -- Configure the set with the signal to be consumed
                      Clair.Signal.set_empty (local_set);
                      Clair.Signal.set_add   (local_set,
                                              Clair.Signal.Number(src.ident));
                      -- Retrieve (consume) the signal.
                      -- Since the signal is already blocked and pending,
                      -- this call returns immediately.
                      Clair.Signal.set_wait (local_set, taken_sig);
                    end;

                    src.sig_cb (src,
                                Clair.Signal.Number(src.ident),
                                src.user_data);
                end case;
              end if;

            exception
              when exc : others =>
                -- [Option 1] Robust Server Pattern:
                -- Log the error and continue the loop
                Clair.Log.error ("Unhandled exception in event callback: " &
                                  Ada.Exceptions.exception_message (exc));
                -- [Option 2] Strict Pattern:
                -- Release and re-propagate the exception
                -- release (self, src);
                -- raise;
            end;

            -- Safety: Ensure `release` is always called to prevent memory
            -- leaks, regardless of whether an exception occurred in the
            -- callback.
            release (self, src);
          end if;

        end;
        <<Continue_Loop>> -- Jump here to skip the current event on error
      end loop; -- for
    end if;

    -- Process Idle callbacks (Only at the outermost iteration)
    -- To prevent recursion and ensure consistent behavior, idle tasks
    -- are executed only when the call depth is 1.
    if self.call_depth = 1 and then self.idle_count > 0 then
      activity_performed := True;
      -- Traverse and execute the idle list
      declare
        current : Handle;
        next    : Handle;
      begin
        -- Resume from the last stopped position (Round-Robin scheduling)
        if self.idle_cursor /= null then
          current := self.idle_cursor;
        else
          current := self.idle_head;
        end if;

        while current /= null loop
          -- Prefetch the next node to allow the current source
          -- to be safely removed during its own callback execution.
          next := current.next_source;

          -- Execute the callback with safety checks
          if not current.is_removed then
            current.idle_cb (current, current.user_data);
            idle_processed := idle_processed + 1;
          end if;

          -- Stop processing if the batch limit is reached (prevent starvation)
          if idle_processed >= MAX_IDLE_BATCH then
            self.idle_cursor := next; -- Save the resumption point
            exit;
          end if;

          current := next;
        end loop;

        -- If reached the end of the list, reset the cursor
        if current = null then
          self.idle_cursor := null;
        end if;
      end;
    end if;

    -- Spurious Wakeup Logic
    -- Condition: (No activity performed) AND
    --            ((Finite timeout remains) OR (Infinite wait))
    if not activity_performed
      and then
        ( (actual_timeout > 0
          and then
            (Clair.Time.get_time (Clair.Platform.CLOCK_MONOTONIC,
                                  current_ts'address) = 0
              and then
                (current_ts.tv_sec < deadline_ts.tv_sec
                  or else (current_ts.tv_sec = deadline_ts.tv_sec
                           and then current_ts.tv_nsec < deadline_ts.tv_nsec))))
        or else (actual_timeout < 0) ) -- Support INFINITE case
    then
      Spurious_Count := Spurious_Count + 1;

      if Spurious_Count <= MAX_SPURIOUS then
        delay 0.001;

        -- Recheck deadline after delay (EINVAL prevention)
        if actual_timeout > 0 then
          -- Measure current time again
          if Clair.Time.get_time (Clair.Platform.CLOCK_MONOTONIC,
                                  current_ts'address) = 0 then
            remaining_ts := deadline_ts - current_ts;

            if remaining_ts.tv_sec >= 0 then
              self.cached_timespec := remaining_ts;
              goto Wait_For_Event;
            end if;
          else
            Clair.Log.error (
              "iterate: clock_gettime failed during spurious check"
            );
          end if;
        else
          goto Wait_For_Event;
        end if;

      else
        Clair.Log.warning ("Iterate: Excessive spurious wakeups detected.");
      end if;
    end if;

    -- Restore the call depth
    self.call_depth := self.call_depth - 1;

    -- Batch cleanup of deferred sources (Garbage Collection)
    -- Memory deallocation is performed only when returning to the outermost
    -- call (Depth = 0) to prevent use-after-free errors in reentrant calls
    -- where upper stack frames might still be referencing the source.
    if self.call_depth = 0 and then self.garbage_head /= null then
      declare
        current_node : Handle := self.garbage_head;
        next_node    : Handle;
      begin
        -- Reset the list head to prevent redundant execution
        self.garbage_head := null;

        while current_node /= null loop
          next_node := current_node.next_garbage;

          -- If the node being freed is the current cursor,
          -- reset the cursor to prevent a dangling pointer.
          if self.idle_cursor = current_node then
            -- Reset to null to safely restart from the beginning.
            self.idle_cursor := null;
          end if;

          -- Perform the actual memory deallocation
          free_source (current_node);
          current_node := next_node;
        end loop;
      end;
    end if;

    return activity_performed;

  exception
    when others =>
      self.call_depth := self.call_depth - 1;
      raise;
  end iterate;

  function get_depth (self : in out Context) return Natural is
  begin
    return self.call_depth;
  end get_depth;

  procedure run (self : in out Context) is
    ignore : Boolean;
  begin
    self.is_running := True;

    while self.is_running loop
      -- Although 'Iterate' returns a boolean indicating activity,
      -- the main loop simply continues its execution, so the result is ignored.
      ignore := iterate (self, INFINITE);
    end loop;
  end run;

  procedure quit (self : in out Context) is
  begin
    self.is_running := False;
  end quit;

  function add_watch (
    self      : in out Context;
    fd        : Clair.File.Descriptor;
    events    : Event_Mask;
    callback  : IO_Callback;
    user_data : System.Address := System.NULL_ADDRESS) return Handle
  is
    changes : Kevent_Array(1 .. 2);
    results : Kevent_Array(1 .. 2);
    count   : Interfaces.C.int := 0;
    retval  : Interfaces.C.int;
    src     : Handle;

    fail_read     : Boolean := False;
    fail_write    : Boolean := False;
    has_error     : Boolean := False;

    cleanup_count : Interfaces.C.int := 0;
    unused        : Interfaces.C.int;

    -- [SAFETY] Use EV_RECEIPT to prevent "Event Stealing".
    -- With EV_CLEAR, checking 'results' might consume a pending IO event
    -- which we would then accidentally ignore. EV_RECEIPT ensures kevent
    -- only returns status codes, not IO events.
    -- (Assuming EV_RECEIPT is defined as 0x0040 or similar in your binding)
    Common_Flags : constant Interfaces.C.unsigned_short :=
       EV_ADD or EV_ENABLE or EV_CLEAR or EV_RECEIPT;
  begin
    src := new Source (kind => Kind_Watch);
    src.ref_count  := 1;
    src.is_removed := False;
    src.ident      := Clair.Platform.uintptr_t(fd);
    src.io_cb      := callback;
    src.user_data  := user_data;
    src.events     := events;

    -- 1. Construct the changelist
    if (events and EVENT_INPUT) /= 0 then
      count := count + 1;
      changes(Integer(count)) := (
        ident  => src.ident,
        filter => EVFILT_READ,
        flags  => Common_Flags,
        fflags => 0, data => 0,
        udata  => handle_to_addr(src),
        ext    => [others => 0]
      );
    end if;

    if (events and EVENT_OUTPUT) /= 0 then
      count := count + 1;
      changes(Integer(count)) := (
        ident  => src.ident,
        filter => EVFILT_WRITE,
        flags  => Common_Flags,
        fflags => 0, data => 0,
        udata  => handle_to_addr(src),
        ext    => [others => 0]
      );
    end if;

    if count = 0 then
      free_source (src);
      return null;
    end if;

    -- 2. Apply changes
    retval := kevent_c (Interfaces.C.int(self.fd),
                        changes(changes'first)'address,
                        count,
                        results(results'first)'address,
                        count,
                        null);

    -- 3. Error Handling
    if retval < 0 then
      free_source (src);
      return null;
    end if;

    if retval > 0 then
      for i in 1 .. Integer(retval) loop
        -- With EV_RECEIPT, EV_ERROR is ALWAYS set.
        -- We check 'data' to see if it's a real error (data != 0).
        if (results(i).flags and EV_ERROR) /= 0
          and then results(i).data /= 0 then
          has_error := True;

          if results(i).filter = EVFILT_READ then
            fail_read := True;
          elsif results(i).filter = EVFILT_WRITE then
            fail_write := True;
          end if;
        end if;
      end loop;
    end if;

    -- [CRITICAL ROLLBACK]
    -- Optimized Logic: Check failures using boolean algebra.
    if has_error then
      cleanup_count := 0;

      for J in 1 .. Integer(count) loop
        -- If (READ and READ failed) OR (WRITE and WRITE failed), we SKIP.
        -- Otherwise (it succeeded), we ADD to cleanup list.
        if not ((changes(J).filter = EVFILT_READ  and then fail_read) or else
                (changes(J).filter = EVFILT_WRITE and then fail_write))
        then
          cleanup_count := cleanup_count + 1;
          changes(Integer(cleanup_count)) := changes(J);
          changes(Integer(cleanup_count)).flags := EV_DELETE;
        end if;
      end loop;

      if cleanup_count > 0 then
        unused := kevent_c (Interfaces.C.int(self.fd),
                            changes(changes'first)'address,
                            cleanup_count,
                            System.NULL_ADDRESS,
                            0,
                            null);
      end if;

      free_source (src);
      return null;
    end if;

    link_active (self, src);
    return src;
  end add_watch;

  procedure modify_watch (self   : in out Context;
                          watch  : Handle;
                          events : Event_Mask)
  is
    -- kqueue does not have a dedicated modification command. Instead,
    -- EV_ADD acts as an overwrite, or filters must be explicitly deleted.
    -- To handle state changes (e.g., Read OFF, Write ON), we use EV_ADD
    -- for enabling and EV_DELETE for disabling filters.
    --
    -- [CRITICAL ORDERING]
    -- We must place EV_ADD operations *before* EV_DELETE operations in the
    -- changes array. If a DELETE operation comes first and
    -- fails (e.g., ENOENT), kevent might stop processing the remaining changes,
    -- causing the subsequent ADD operation to be skipped while we effectively
    -- update the internal state.
    changes : Kevent_Array(1 .. 2);
    count   : Interfaces.C.int := 0;
    retval  : Interfaces.C.int;

    -- [Diff Logic] Detect state transitions
    -- Old: Current state, New: Requested state
    old_input  : Boolean;
    new_input  : Boolean;
    old_output : Boolean;
    new_output : Boolean;
  begin
    if watch = null or else watch.is_removed then
      return;
    end if;

    old_input  := (watch.events and EVENT_INPUT)  /= 0;
    new_input  := (events       and EVENT_INPUT)  /= 0;
    old_output := (watch.events and EVENT_OUTPUT) /= 0;
    new_output := (events       and EVENT_OUTPUT) /= 0;

    -----------------------------------------------------------------------
    -- 1. Push ADD (Enable) operations first
    -----------------------------------------------------------------------
    -- Check READ ADD
    if not old_input and new_input then
      count := count + 1;
      changes(Integer(count)) := (
        ident  => watch.ident,
        filter => EVFILT_READ,
        flags  => EV_ADD or EV_ENABLE or EV_CLEAR,
        fflags => 0, data => 0,
        udata  => handle_to_addr (watch),
        others => <>
      );
    end if;
    -- Check WRITE ADD
    if not old_output and new_output then
      count := count + 1;
      changes(Integer(count)) := (
        ident  => watch.ident,
        filter => EVFILT_WRITE,
        flags  => EV_ADD or EV_ENABLE or EV_CLEAR,
        fflags => 0, data => 0,
        udata  => handle_to_addr (watch),
        others => <>
      );
    end if;

    -----------------------------------------------------------------------
    -- 2. Push DELETE (Disable) operations next
    -----------------------------------------------------------------------
    -- Check READ DELETE
    if old_input and not new_input then
      count := count + 1;
      changes(Integer(count)) := (
        ident  => watch.ident,
        filter => EVFILT_READ,
        flags  => EV_DELETE,
        udata  => handle_to_addr (watch),
        others => <>
      );
    end if;
    -- Check WRITE DELETE
    if old_output and not new_output then
      count := count + 1;
      changes(Integer(count)) := (
        ident  => watch.ident,
        filter => EVFILT_WRITE,
        flags  => EV_DELETE,
        udata  => handle_to_addr (watch),
        others => <>
      );
    end if;
    -- Apply changes
    if (count > 0) then
      retval := kevent_c (Interfaces.C.int(self.fd),
                          changes(changes'first)'address,
                          count,
                          System.NULL_ADDRESS,
                          0,
                          null);
      declare
        errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
      begin
        -- Update state if successful (0) or if deletion failed due to
        -- the event already being missing (ENOENT).
        -- Since we processed ADDs first, an ENOENT here (likely from the
        -- trailing DELETEs) implies the ADDs succeeded.
        if retval = 0 or else errno_code = Clair.Errno.ENOENT then
          watch.events := events;
        elsif retval = -1 then
          -- Log genuine system errors
          Clair.Log.warning ("modify_watch failed: " & errno_code'image);
        end if;
      end;
    end if;
  end modify_watch;

  function add_timer (self      : in out Context;
                      interval  : Milliseconds;
                      callback  : Timer_Callback;
                      one_shot  : Boolean := False;
                      user_data : System.Address := System.NULL_ADDRESS)
  return Handle is
    -- Use arrays to handle EV_RECEIPT results gracefully
    changes : Kevent_Array(1 .. 1);
    results : Kevent_Array(1 .. 1);

    retval   : Interfaces.C.int;
    src      : Handle;
    ms_value : Clair.Platform.int64_t;
    flags    : Interfaces.C.unsigned_short;
  begin
    -- 1. Validate Interval (Strict Check)
    if interval <= 0 then
      raise Constraint_Error with "Timer interval must be greater than 0";
    end if;

    ms_value := Clair.Platform.int64_t(interval);

    -- 2. Allocate Source
    src := new Source (kind => Kind_Timer);
    src.ref_count  := 1;
    src.is_removed := False;
    src.ident      := handle_to_uintptr (src); -- Unique ID based on address
    src.timer_cb   := callback;
    src.user_data  := user_data;

    -- 3. Configure Flags
    -- [SAFETY] Use EV_RECEIPT to ensure we receive a status code. This prevents
    -- any ambiguity if a timer expires immediately upon registration.
    flags := EV_ADD or EV_ENABLE or EV_RECEIPT;

    if one_shot then
      flags := flags or EV_ONESHOT;
    end if;

    -- 4. Prepare Change
    changes(1) := (ident  => src.ident,
                   filter => EVFILT_TIMER,
                   flags  => flags,
                   fflags => Clair.Platform.NOTE_MSECONDS,
                   data   => ms_value,
                   udata  => handle_to_addr(src),
                   ext    => [others => 0]);

    -- 5. Register with Receipt Check
    retval := kevent_c (Interfaces.C.int(self.fd),
                        changes(1)'address,
                        1,
                        results(1)'address, -- Receive receipt here
                        1,
                        null);

    -- Check for system error (< 0) OR logical error in receipt (data != 0)
    -- If kevent returns > 0, results(1).data contains the error code (errno).
    if retval < 0 or else (retval > 0 and then results(1).data /= 0) then
      free_source (src);
      return null;
    end if;

    link_active (self, src);
    return src;

    exception
      when exc : others =>
        Clair.Log.error ("Ada Exception in add_timer: " &
                         Ada.Exceptions.Exception_Information (exc));
        return null;
  end add_timer;

  function add_idle (
    self      : in out Context;
    callback  : Idle_Callback;
    user_data : System.Address := System.NULL_ADDRESS) return Handle
  is
    src : Handle;
  begin
    -- Create Source object
    src := new Source (kind => Kind_Idle);
    src.ref_count   := 1;
    -- Must explicitly initialize is_removed. Otherwise it might contain
    -- garbage (True), causing the task to be ignored.
    src.is_removed  := False;
    -- Assign a unique identifier for consistency.
    -- Even though Idle tasks aren't managed by kqueue, having a unique ID
    -- is crucial for debugging/logging or generic 'remove' logic.
    src.ident       := handle_to_uintptr (src);
    src.idle_cb     := callback;
    src.user_data   := user_data;
    src.next_source := null;
    src.prev_source := self.idle_tail; -- Link backward

    -- Append to the Idle list (Standard Doubly Linked List Append)
    if self.idle_tail /= null then
      self.idle_tail.next_source := src;
    else
      self.idle_head := src; -- First element in the list
    end if;

    self.idle_tail  := src;
    self.idle_count := self.idle_count + 1;

    return src;
  end add_idle;

  function add_unix_signal (
    self      : in out Context;
    signum    : Clair.Signal.Number;
    callback  : Signal_Callback;
    user_data : System.Address := System.NULL_ADDRESS
  ) return Handle is
    -- Use arrays for EV_RECEIPT consistency
    changes : Kevent_Array(1 .. 1);
    results : Kevent_Array(1 .. 1);

    retval  : Interfaces.C.int;
    src     : Handle;

    -- Signal sets for masking and restoring
    set     : aliased Clair.Signal.Set;
    old_set : aliased Clair.Signal.Set;
  begin
    -- 1. Prepare Signal Mask
    Clair.Signal.set_empty (set);
    Clair.Signal.set_add (set, signum);
    -- Block the signal to prevent the OS from delivering it to any existing
    -- handlers. This allows kqueue (EVFILT_SIGNAL) to safely capture
    -- and queue the signal for our event loop to process.
    Clair.Signal.block (set, old_set);

    -- 2. Allocate Source
    src := new Source (kind => Kind_Signal);
    src.ref_count  := 1;
    src.is_removed := False;
    src.ident      := Clair.Platform.uintptr_t(signum);
    src.sig_cb     := callback;
    src.user_data  := user_data;
    src.sig_fd     := Clair.File.INVALID_DESCRIPTOR;

    -- Check the previous signal mask to determine if this signal was
    -- already blocked before our registration.
    if Clair.Signal.is_member (old_set, signum) then
      src.originally_blocked := True;
    else
      src.originally_blocked := False;
    end if;

    -- 3. Configure Kevent with Receipt
    changes(1) := (ident  => Clair.Platform.uintptr_t(signum),
                   filter => EVFILT_SIGNAL,
                   -- EV_RECEIPT ensures we get a status code return.
                   flags  => EV_ADD or EV_ENABLE or EV_RECEIPT,
                   fflags => 0,
                   data   => 0,
                   udata  => handle_to_addr (src),
                   ext    => [others => 0]);

    -- 4. Register
    retval := kevent_c (Interfaces.C.int(self.fd),
                        changes(1)'address,
                        1,
                        results(1)'address, -- Get receipt
                        1,
                        null);

    -- 5. Error Handling & State Restoration
    -- Check for system error (<0) OR logical error in receipt (data != 0)
    if retval < 0 or else (retval > 0 and then results(1).data /= 0) then
      -- [ROLLBACK] Restore the previous signal mask.
      Clair.Signal.set_mask (old_set);

      free_source (src);
      return null;
    end if;

    link_active (self, src);

    return src;
  end add_unix_signal;

  procedure remove (self : in out Context;
                    item : Handle)
  is
    change : Kevent_Type;
    retval : Interfaces.C.int;
  begin
    if item = null or else item.is_removed then
      return;
    end if;
    -- Transition to a logically removed state (zombie state)
    item.is_removed := True;
    -- Unlink from the list.
    if item.kind = Kind_Idle then
      -- Remove from the Idle list.
      if item.prev_source /= null then
        item.prev_source.next_source := item.next_source;
      else
        self.idle_head := item.next_source;
      end if;

      if item.next_source /= null then
        item.next_source.prev_source := item.prev_source;
      else
        self.idle_tail := item.prev_source;
      end if;
      self.idle_count := self.idle_count - 1;
    else
      -- Remove from the Active list (Watch, Timer, or Signal).
      unlink_active (self, item);
    end if;

    case item.kind is
      when Kind_Watch =>
        declare
          changes : Kevent_Array(1 .. 2);
          count   : Integer := 0;
        begin
          if (item.events and EVENT_INPUT) /= 0 then
            count := count + 1;
            changes(count) := (
              ident  => item.ident,
              filter => EVFILT_READ,
              flags  => EV_DELETE,
              others => <>
            );
          end if;

          if (item.events and EVENT_OUTPUT) /= 0 then
            count := count + 1;
            changes(count) := (
              ident  => item.ident,
              filter => EVFILT_WRITE,
              flags  => EV_DELETE,
              others => <>
            );
          end if;

          if count > 0 then
            retval := kevent_c (Interfaces.C.int(self.fd),
                                changes(changes'first)'address,
                                Interfaces.C.int(count),
                                System.NULL_ADDRESS,
                                0,
                                null);
            if retval < 0 then
              declare
                errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
              begin
                -- It is normal for ENOENT to occur if the file descriptor `fd`
                -- has already been closed, as the kernel automatically removes
                -- associated events upon closing.
                if errno_code /= Clair.Errno.ENOENT then
                    Clair.Log.Warning ("kevent watch removal failed (errno:" &
                                       errno_code'image & ")");
                end if;
              end;
            end if;
          end if;
        end;
      when Kind_Timer =>
        -- Configure the event to be deleted `change`.
        -- When deleting, both the filter and ident must match the existing
        -- event. `udata` is not used by the kernel to locate events, so we pass
        -- NULL.
        change := (ident  => item.ident,
                   filter => EVFILT_TIMER,
                   flags  => EV_DELETE,
                   fflags => 0,
                   data   => 0,
                   udata  => System.NULL_ADDRESS,
                   ext    => [others => 0]);

        retval := kevent_c (Interfaces.C.int(self.fd),
                            change'address,
                            1,
                            System.NULL_ADDRESS,
                            0,
                            null);
        if retval < 0 then
          declare
            errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
          begin
          -- ENOENT is normal for one-shot timers that have already expired
          -- and been automatically removed by the kernel. We verify the error
          -- code to avoid logging false positives.
            if errno_code /= Clair.Errno.ENOENT then
              -- Other errors (e.g., EBADF) may indicate serious system issues,
              -- so we log them for diagnostic purposes.
              Clair.Log.Warning ("kevent timer removal failed (errno:" &
                                  errno_code'image & ")");
            end if;
          end;
        end if;

      when Kind_Idle =>
        -- Leave this block empty.
        -- Idle tasks do not use OS kernel resources, so there is nothing to
        -- release. The list connection has already been severed above.
        null;

      when Kind_Signal =>
        -- For BSD (kqueue): Requires calling EV_DELETE
        -- For Linux (epoll): Requires close(item.sig_fd)
        change := (ident  => item.ident,
                   filter => EVFILT_SIGNAL,
                   flags  => EV_DELETE,
                   fflags => 0,
                   data   => 0,
                   udata  => System.NULL_ADDRESS,
                   ext    => [others => 0]);

        retval := kevent_c (Interfaces.C.int(self.fd),
                            change'address,
                            1,
                            System.NULL_ADDRESS,
                            0,
                            null);
        if retval < 0 then
          declare
            errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
          begin
            -- ENOENT is ignored because the filter might have been
            -- automatically deleted by the kernel if EV_ONESHOT was used, or
            -- if removed concurrently.
            if errno_code /= Clair.Errno.ENOENT then
              Clair.Log.Warning ("kevent signal removal failed (errno:" &
                                 errno_code'image & ")");
            end if;
          end;
        end if;

        -- If the signal was not blocked at the time of registration,
        -- we must unblock it now to restore the previous state.
        if not item.originally_blocked then
          declare
            set : aliased Clair.Signal.Set;
          begin
            Clair.Signal.set_empty (set);
            Clair.Signal.set_add   (set, Clair.Signal.Number(item.ident));
            -- We call `unblock` instead of `set_mask(oset)` because restoring
            -- the entire previous mask would overwrite and lose any
            -- signal changes made by other threads or objects in the meantime.
            Clair.Signal.unblock (set);
          end;
        end if;

    end case;
    -- Release ownership (decrement ref_count -> free if 0).
    release (self, item);
  end remove;

end Clair.Event_Loop;
