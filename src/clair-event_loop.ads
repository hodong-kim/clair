-- clair-event_loop.ads
-- Copyright (c) 2021-2026 Hodong Kim <hodong@nimfsoft.com>
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
with Interfaces.C;
with System;
with Clair.Signal;
with Clair.Platform;
with Clair.Time;
with Clair.Arena;
with Clair.IO;

package Clair.Event_Loop is
  use type Interfaces.C.int;

  type Milliseconds is new Clair.Platform.int64_t;

  INFINITE  : constant Milliseconds := -1;
  IMMEDIATE : constant Milliseconds :=  0;

  type Context is limited private;
  type Context_Access is access all Context;
  type Handle is private;

  NULL_HANDLE : constant Handle;

  type Event_Mask is new Interfaces.C.unsigned;

  EVENT_INPUT   : constant Event_Mask := Clair.Platform.POLLIN;
  EVENT_OUTPUT  : constant Event_Mask := Clair.Platform.POLLOUT;
  EVENT_ERROR   : constant Event_Mask := Clair.Platform.POLLERR;
  EVENT_HANG_UP : constant Event_Mask := Clair.Platform.POLLHUP;

  type IO_Callback is access procedure (
    io        : Handle;
    fd        : Clair.IO.Descriptor;
    events    : Event_Mask;
    user_data : System.Address)
  with convention => c;

  type Timer_Callback is access procedure (
    timer     : Handle;
    user_data : System.Address)
  with convention => c;

  type Idle_Callback is access procedure (
    idle      : Handle;
    user_data : System.Address)
  with convention => c;

  type Signal_Callback is access procedure (
    signal    : Handle;
    signum    : Clair.Signal.Number;
    user_data : System.Address)
  with convention => c;

  procedure initialize (self : in out Context);
  procedure finalize   (self : in out Context);

  function  create return Context_Access;
  procedure destroy (Self : in out Context_Access);

  procedure run  (self : in out Context);
  procedure quit (self : in out Context);

  -- Executes a single cycle of the event loop.
  -- Checks for pending events and triggers their associated callbacks.
  --
  -- @param self The event loop context to operate on.
  -- @param timeout Timeout in milliseconds. Negative values (e.g., INFINITE)
  --                will cause the loop to wait indefinitely.
  --
  -- @return True if any I/O events were processed or Idle tasks were executed.
  -- @return False if the operation timed out with no activity performed.
  function iterate (self    : in out Context;
                    timeout : Milliseconds := INFINITE) return Boolean;

  function get_depth (self : in out Context) return Natural;

  function add_watch (
    self      : in out Context;
    fd        : Clair.IO.Descriptor;
    events    : Event_Mask;
    callback  : IO_Callback;
    user_data : System.Address := System.NULL_ADDRESS
  ) return Handle;

  function add_timer (
    self      : in out Context;
    interval  : Milliseconds;
    callback  : Timer_Callback;
    one_shot  : Boolean := False;
    user_data : System.Address := System.NULL_ADDRESS
  ) return Handle;

  function add_idle (
    self      : in out Context;
    callback  : Idle_Callback;
    user_data : System.Address := System.NULL_ADDRESS
  ) return Handle;

  procedure remove (self : in out Context;
                    item : Handle);

  procedure modify_watch (self   : in out Context;
                          watch  : Handle;
                          events : Event_Mask);

  function add_unix_signal (
    self      : in out Context;
    signum    : Clair.Signal.Number;
    callback  : Signal_Callback;
    user_data : System.Address := System.NULL_ADDRESS
  ) return Handle;

private

  -- Dedicated memory pool for `Source` objects
  arena_pool : Clair.Arena.Pool;

  type Source;
  type Handle is access all Source;
  for Handle'storage_pool use arena_pool;

  pragma convention (c, Handle);

  NULL_HANDLE : constant Handle := null;

  type Source_Kind is (Kind_Watch, Kind_Timer, Kind_Idle, Kind_Signal);

  type Source (kind : Source_Kind := Kind_Watch) is record
    -- lifecycle management
    ref_count    : Integer := 1;
    is_removed   : Boolean := False; -- flag for logical deletion (zombie state)
    user_data    : System.Address;

    -- linked list pointers for active (watch/timer/signal) or idle lists
    prev_source  : Handle := null;
    next_source  : Handle := null;

    -- [garbage collection] pointer for the delayed deallocation list
    next_garbage : Handle := null;

    -----------------------------------------------------------------------
    -- [Platform-Specific Resource Identifier]
    -----------------------------------------------------------------------
    -- Stores the unique value (`ident`) required by the OS kernel (kqueue)
    -- to identify and control (e.g., delete) the event.
    -- The usage depends on the source kind:
    --
    -- 1. I/O Watch:
    --    Stores the target 'File Descriptor (fd)'.
    --    (kqueue requires the fd as the `ident` for EVFILT_READ/WRITE)
    --
    -- 2. Timer:
    --    Uses the memory address of this Source object (Handle) cast to an
    --    integer (unsigned_long_long) instead of generating a separate ID.
    --    - Benefit: Guarantees uniqueness and allows direct backtracking
    --      to the source object using `ident` alone without checking udata.
    --
    -- 3. Signal:
    --    Stores the target 'Signal Number'.
    --
    -- * type: unsigned_long_long is used to safely hold 64-bit pointer
    --         addresses.
    -----------------------------------------------------------------------
    ident     : Clair.Platform.uintptr_t := 0;

    -- variant part (memory layout optimized to minimize padding)
    case kind is
      when Kind_Watch =>
        events : Event_Mask;
        io_cb  : IO_Callback;

      when Kind_Timer =>
        timer_cb : Timer_Callback;

      when Kind_Idle =>
        idle_cb   : Idle_Callback;

      when Kind_Signal =>
        sig_cb  : Signal_Callback;
        -- [Linux Compatibility]
        -- Stores the fd created by signalfd (required for epoll/close).
        -- Unused in kqueue (BSD) but kept for structural consistency.
        sig_fd  : Clair.IO.Descriptor := Clair.IO.INVALID_DESCRIPTOR;
        originally_blocked : Boolean := False;
    end case;
  end record;

  type Context is limited record
    fd              : Clair.IO.Descriptor := Clair.IO.INVALID_DESCRIPTOR;
    is_running      : Boolean  := False;
    -- Cached timeout (64-bit integer, used for comparison)
    cached_timeout  : Milliseconds := INFINITE;
    cached_timespec : aliased Clair.Time.Timespec
                    := (tv_sec => 0, tv_nsec => 0);
    -- Recursion depth of the event loop
    call_depth      : Natural := 0;
    -- Cursor for Round-Robin scheduling of Idle tasks
    idle_cursor     : Handle  := null;
    -- [Idle List] List dedicated to idle callbacks
    idle_head       : Handle  := null;
    idle_tail       : Handle  := null; -- Tail of the list (for fast appending)
    idle_count      : Natural := 0;
    -- [Active List] List for managing Watch, Timer, and Signal sources
    active_head     : Handle  := null;
    active_tail     : Handle  := null;
    -- [Garbage List] List of sources pending deallocation
    garbage_head    : Handle  := null;
  end record;

end Clair.Event_Loop;
