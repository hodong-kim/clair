-- clair-io.adb
-- Copyright (c) 2025,2026 Hodong Kim <hodong@nimfsoft.com>
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
with Clair.Errno;
with Clair.Time;
with Clair.Exceptions;

package body Clair.IO is

  use type Interfaces.C.int;
  use type Clair.Platform.ssize_t;

  -----------------------------------------------------------------------------
  -- Poll Event Conversion Helpers
  -----------------------------------------------------------------------------
  function to_ushort is new Ada.Unchecked_Conversion
    (source => Clair.IO.Poll_Event, target => Interfaces.C.unsigned_short);
  function to_event is new Ada.Unchecked_Conversion
    (source => Interfaces.C.unsigned_short, target => Clair.IO.Poll_Event);

  function "and" (left  : Clair.IO.Poll_Event;
                  right : Clair.IO.Poll_Event) return Clair.IO.Poll_Event
  is
    use type Interfaces.C.unsigned_short;
  begin
    return to_event (to_ushort (left) and to_ushort (right));
  end "and";

  function "or" (left  : Clair.IO.Poll_Event;
                 right : Clair.IO.Poll_Event) return Clair.IO.Poll_Event
  is
    use type Interfaces.C.unsigned_short;
  begin
    return to_event (to_ushort (left) or to_ushort (right));
  end "or";

  -----------------------------------------------------------------------------
  -- C Imports
  -----------------------------------------------------------------------------
  function c_close (fd : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_close, "close");

  function c_read (fd    : Interfaces.C.int;
                   buf   : System.Address;
                   count : Interfaces.C.size_t) return Clair.Platform.ssize_t;
  pragma import (c, c_read, "read");

  function c_write (fd    : Interfaces.C.int;
                    buf   : System.Address;
                    count : Interfaces.C.size_t) return Clair.Platform.ssize_t;
  pragma import (c, c_write, "write");

  function c_dup (oldfd : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_dup, "dup");

  function c_dup2 (oldfd : Interfaces.C.int;
                   newfd : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_dup2, "dup2");

  function c_poll (fds     : System.Address;
                   nfds    : Interfaces.C.unsigned_long;
                   timeout : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_poll, "poll");

  -- Used for ensure_standard_descriptors
  function fcntl2 (fd  : Descriptor;
                   cmd : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, fcntl2, "fcntl");

  function c_open2 (path  : Interfaces.C.char_array;
                    flags : Interfaces.C.int) return Descriptor;
  pragma import (c, c_open2, "open");

  F_GETFL : constant := Clair.Platform.F_GETFL;
  O_RDWR  : constant := Clair.Platform.O_RDWR;

  -----------------------------------------------------------------------------
  -- Implementations
  -----------------------------------------------------------------------------

  procedure close (fd : in Descriptor) is
    retval : Interfaces.C.int;
  begin
    if fd = -1 then
      return;
    end if;

    retval := c_close (Interfaces.C.int(fd));

    if retval = -1 then
      declare
        errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
        error_msg  : constant String :=
          Clair.Errno.format_posix_error_message
            (errno_code    => errno_code,
             function_name => "Clair.IO.close",
             context_info  => "on fd " & fd'image);
      begin
        -- In principle, `close` should not be retried even if EINTR occurs.
        -- (This is because the fd has likely already been released by the
        -- kernel.)
        -- Therefore, all errors are passed to raise_from_errno without
        -- separate branching.
        -- If EINTR occurs, the Interrupted_System_Call exception is raised.
        Clair.Errno.raise_from_errno (errno_code, error_msg);
      end;
    end if;
  end close;

  function read (fd     : in Descriptor;
                 buffer : in out System.Storage_Elements.Storage_Array)
  return Natural is
    bytes_read  : Clair.Platform.ssize_t;
    retry_count : Natural := 0;
  begin
    loop
      bytes_read :=
        c_read (Interfaces.C.int(fd), buffer'address, buffer'length);

      -- [Success or EOF]
      -- bytes_read > 0: Data read
      -- bytes_read = 0: EOF (End Of File) reached -> Normal termination
      if bytes_read >= 0 then
        exit;
      end if;

      -- [Error handling]
      declare
        errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
      begin
        if errno_code = Clair.Errno.EINTR then
          retry_count := retry_count + 1;
          if retry_count > Clair.Max_EINTR_Retries then
            raise Clair.Exceptions.Interrupted_System_Call with
              "Clair.IO.read: Aborted after " &
              Clair.Max_EINTR_Retries'image &
              " consecutive EINTR signals on fd " & fd'image;
          end if;
        else
          declare
            error_msg : constant String :=
              Clair.Errno.format_posix_error_message
                (errno_code    => errno_code,
                 function_name => "Clair.IO.read",
                 context_info  => "on fd " & fd'image);
          begin
            Clair.Errno.raise_from_errno (errno_code, error_msg);
          end;
        end if;
      end;
    end loop;

    return Natural(bytes_read);
  end read;

  function write (fd     : in Descriptor;
                  buffer : in System.Storage_Elements.Storage_Array)
  return Natural is
    bytes_written : Clair.Platform.ssize_t;
    retry_count   : Natural := 0;
  begin
    loop
      bytes_written := c_write (Interfaces.C.int(fd),
                                buffer'address,
                                buffer'length);

      if bytes_written /= -1 then
        exit;
      end if;

      declare
        errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
      begin
        if errno_code = Clair.Errno.EINTR then
          retry_count := retry_count + 1;
          if retry_count > Clair.Max_EINTR_Retries then
            raise Clair.Exceptions.Interrupted_System_Call with
              "Clair.IO.write: Aborted after " &
              Clair.Max_EINTR_Retries'image &
              " consecutive EINTR signals on fd " & fd'image;
          end if;
        else
          declare
            error_msg : constant String :=
              Clair.Errno.format_posix_error_message
                (errno_code    => errno_code,
                 function_name => "Clair.IO.write",
                 context_info  => "on fd " & fd'image);
          begin
            Clair.Errno.raise_from_errno (errno_code, error_msg);
          end;
        end if;
      end;
    end loop;

    return Natural(bytes_written);
  end write;

  function duplicate (fd : in Descriptor) return Descriptor is
    new_fd : constant Descriptor :=
      Descriptor(c_dup (Interfaces.C.int(fd)));
  begin
    if new_fd = -1 then
      declare
        errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
        error_msg  : constant String :=
          Clair.Errno.format_posix_error_message
            (errno_code    => errno_code,
             function_name => "Clair.IO.duplicate",
             context_info  => "on fd " & fd'image);
      begin
        Clair.Errno.raise_from_errno (errno_code, error_msg);
      end;
    end if;

    return new_fd;
  end duplicate;

  function duplicate_to (fd     : in Descriptor;
                         new_fd : Descriptor) return Descriptor is
    result_fd   : Descriptor;
    retry_count : Natural := 0;
  begin
    loop
      result_fd :=
        Descriptor(c_dup2 (Interfaces.C.int(fd), Interfaces.C.int(new_fd)));
      if result_fd /= -1 then
        -- Success
        exit;
      end if;

      -- An error occurred (result_fd = -1), check errno.
      declare
        errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
      begin
        if errno_code = Clair.Errno.EINTR then
          -- Interrupted by a signal, prepare to retry.
          retry_count := retry_count + 1;
          if retry_count > Clair.Max_EINTR_Retries then
            raise Clair.Exceptions.Interrupted_System_Call with
              "Clair.IO.duplicate_to: Aborted after " &
              Clair.Max_EINTR_Retries'image &
              " consecutive EINTR signals for fd " & fd'image & " to " &
              new_fd'image;
          end if;
        else
          -- A real, unrecoverable error occurred.
          declare
            error_msg : constant String :=
              Clair.Errno.format_posix_error_message
                (errno_code    => errno_code,
                 function_name => "Clair.IO.duplicate_to",
                 context_info  => "from fd " & fd'image &
                                  " to new_fd " & new_fd'image);
          begin
            Clair.Errno.raise_from_errno (errno_code, error_msg);
          end;
        end if;
      end;
    end loop;

    return result_fd;
  end duplicate_to;

  -- Ensure that standard I/O descriptors (0, 1, 2) are open.
  -- If they are closed, redirect them to /dev/null.
  procedure ensure_standard_descriptors is
    null_fd : Descriptor;
    flags   : Interfaces.C.int;
    retval  : Interfaces.C.int;
    pragma unreferenced (retval);
  begin
    -- Open /dev/null once.
    -- Direct call to c_open2 (prevents exceptions).
    null_fd := c_open2 (Interfaces.C.to_c ("/dev/null"), O_RDWR);

    if null_fd = INVALID_DESCRIPTOR then
      -- If open fails, no further action can be taken,
      -- so the result is ignored.
      return;
    end if;

    for fd in 0 .. 2 loop
      -- Use fcntl to check if the file descriptor is valid (open).
      -- If F_GETFL returns -1 and errno is EBADF, the descriptor is closed.
      flags := fcntl2 (Descriptor(fd), F_GETFL);

      if flags = -1 and then Clair.Errno.get_errno = Clair.Errno.EBADF then
        -- If closed, duplicate the previously opened null_fd to this slot (fd).
        -- dup2 copies the file descriptor to the specified fd number.
        -- Direct call to c_dup2 (prevents exceptions, ignores failure).
        retval := c_dup2 (Interfaces.C.int(null_fd), Interfaces.C.int(fd));
        -- If dup2 fails, no further action can be taken,
        -- so the result is ignored.
      end if;
    end loop;

    -- Close null_fd if it is greater than 2. (i.e., if it was opened as
    -- a temporary fd and not used to fill 0, 1, or 2).
    if null_fd > 2 then
      -- Direct call to c_close instead of Ada wrapper (prevents exceptions).
      retval := c_close (Interfaces.C.int(null_fd));
    end if;
  end ensure_standard_descriptors;

  function poll (fds     : in out Poll_FD_Array;
                 timeout : Integer := -1) return Natural
  is
    use type Clair.Time.time_t;
    use type Clair.Time.Timespec;
    use type Interfaces.C.long;

    retval      : Interfaces.C.int;
    fds_ptr     : System.Address;
    retry_count : Natural  := 0;

    actual_timeout : Integer := timeout;
    current_ts     : aliased Clair.Time.Timespec;
    deadline_ts    : Clair.Time.Timespec;
    remaining_ts   : Clair.Time.Timespec;

  begin
    if fds'length = 0 then
      fds_ptr := System.NULL_ADDRESS;
    else
      fds_ptr := fds(fds'first)'address;
    end if;

    -- Set the deadline (Performed before entering the loop)
    -- The absolute expiration time must be calculated before the first c_poll
    -- call. This ensures that the total timeout duration does not drift
    -- (increase) even if multiple EINTR signals occur.
    if actual_timeout > 0 then
      if Clair.Time.get_time (Clair.Platform.CLOCK_MONOTONIC,
                              current_ts'address) /= 0
      then
        Clair.Errno.raise_from_errno (Clair.Errno.get_errno,
          "poll: clock_gettime failed at start");
      end if;

    declare
        timeout_ts : constant Clair.Time.Timespec :=
          (tv_sec  => Clair.Time.time_t(actual_timeout / 1000),
           tv_nsec => Interfaces.C.long((actual_timeout rem 1000) * 1_000_000));
      begin
        deadline_ts := current_ts + timeout_ts;
      end;
    end if;

    loop
      retval := c_poll (fds_ptr,
                        Interfaces.C.unsigned_long(fds'length),
                        Interfaces.C.int(actual_timeout));

      if retval >= 0 then
        return Natural(retval);
      end if;

      declare
        errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
      begin
        if errno_code = Clair.Platform.EINTR then
          retry_count := retry_count + 1;
          if retry_count > Clair.Max_EINTR_Retries then
            Clair.Errno.raise_from_errno (
              errno_code, "poll failed: EINTR limit exceeded"
            );
          end if;

          -- Recalculate remaining time only if the timeout is finite and
          -- blocking. Infinite (-1) or non-blocking (0) calls do not require
          -- adjustment.
          if actual_timeout > 0 then
            if Clair.Time.get_time (Clair.Platform.CLOCK_MONOTONIC,
                                    current_ts'address) /= 0
            then
              Clair.Errno.raise_from_errno (Clair.Errno.get_errno,
                "poll: clock_gettime failed during retry");
            end if;

            remaining_ts := deadline_ts - current_ts;

            if remaining_ts.tv_sec < 0 then
              return 0; -- Timeout (no events)
            end if;

            actual_timeout := Integer(remaining_ts.tv_sec) * 1000 +
                              Integer(remaining_ts.tv_nsec / 1_000_000);

            -- Guard against premature termination:
            -- If the calculated remaining time is 0ms (due to integer division)
            -- but there are still nanoseconds remaining, round up to 1ms.
            -- This prevents the call from accidentally becoming
            -- non-blocking (0).
            if actual_timeout = 0 and then remaining_ts.tv_nsec > 0 then
              actual_timeout := 1;
            end if;
          end if;

        else
          Clair.Errno.raise_from_errno (errno_code, "poll failed");
        end if;
      end;
    end loop;
  end poll;

end Clair.IO;
