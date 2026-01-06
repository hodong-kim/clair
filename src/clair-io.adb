-- clair-io.adb
-- Copyright (c) 2025,2026 Hodong Kim <hodong@nimfsoft.art>
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
with Clair.Errno;
with Clair.Time;

package body Clair.IO is

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

  function c_close (fd : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_close, "close");

  function c_poll (fds     : System.Address;
                   nfds    : Interfaces.C.unsigned_long;
                   timeout : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_poll, "poll");

  procedure close (fd : in Descriptor) is
    use type Interfaces.C.int;
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
             function_name => "Clair.File.close",
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

  function poll (fds     : in out Poll_FD_Array;
                 timeout : Integer := -1) return Natural
  is
    use type Interfaces.C.int;
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
      fds_ptr := fds (fds'first)'address;
    end if;

    -- Set the deadline (Performed before entering the loop)
    -- The absolute expiration time must be calculated before the first c_poll
    -- call. This ensures that the total timeout duration does not drift
    -- (increase) even if multiple EINTR signals occur.
    if actual_timeout > 0 then
      if Clair.Time.get_time (Clair.Platform.CLOCK_MONOTONIC,
                              current_ts'address) /= 0 then
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
                                    current_ts'address) /= 0 then
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
