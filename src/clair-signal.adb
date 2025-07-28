-- clair-signal.adb
-- Copyright (c) 2025 Hodong Kim <hodong@nimfsoft.art>
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

with Interfaces.C;
with System;
with Clair.Error;
with Clair.Types;

package body Clair.Signal is
  use type Interfaces.C.int;
  use type Interfaces.C.long;
  use type Interfaces.C.unsigned;
  use type Interfaces.C.unsigned_long;

  -- C functions from <signal.h> are now imported directly.
  function c_sigemptyset (sig_set : access Set)
    return Interfaces.C.int;
  pragma import (c, c_sigemptyset, "sigemptyset");

  function c_raise (signo : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_raise, "raise");

  function c_sigaction (signum     : Interfaces.C.int;
                        new_action : access constant Action;
                        old_action : access Action) return Interfaces.C.int;
  pragma import (c, c_sigaction, "sigaction");

  function c_sigsuspend (mask : access constant Set)
    return Interfaces.C.int;
  pragma import (c, c_sigsuspend, "sigsuspend");

  procedure empty_set (sig_set : aliased out Set) is
    retval : constant Interfaces.C.int := c_sigemptyset (sig_set'access);
  begin
    if retval /= 0 then
      declare
        errno_code : constant Interfaces.C.int := Clair.Error.get_errno;
        error_msg  : constant String :=
          Clair.Error.format_posix_error_message
            (errno_code    => errno_code,
             function_name => "Clair.Signal.empty_set (sigemptyset)",
             context_info  => "Failed to initialize signal set");
      begin
        raise Clair.Error.Invalid_Argument with error_msg;
      end;
    end if;
  end empty_set;

  -- Implementation of the raise(3) wrapper
  procedure send_signal (signo : Number) is
    retval : constant Interfaces.C.int := c_raise (signo);
  begin
    if retval /= 0 then
      declare
        errno_code : constant Interfaces.C.int := Clair.Error.get_errno;
        error_msg  : constant String :=
          Clair.Error.format_posix_error_message
            (errno_code    => errno_code,
             function_name => "Clair.Signal.send_signal",
             context_info  => "for signal " & signo'image);
      begin
        case errno_code is
          -- According to the POSIX standard for raise(3), EINVAL is the
          -- primary documented error.
          when Clair.Error.EINVAL =>
            raise Clair.Error.Invalid_Argument with error_msg;
          when others =>
            raise Clair.Error.Unmapped_Error with error_msg;
        end case;
      end;
    end if;
  end send_signal;

  procedure set_action (signo      : in          Number;
                        new_action : aliased in  Action;
                        old_action : aliased out Action)
  is
    retval : Interfaces.C.int := c_sigaction (signo,
                                              new_action'access,
                                              old_action'access);
  begin
    if retval = -1 then
      declare
        errno_code : constant Interfaces.C.int := Clair.Error.get_errno;
        error_msg  : constant String :=
          Clair.Error.format_posix_error_message
            (errno_code    => errno_code,
             function_name => "Clair.Signal.set_action",
             context_info  => "for signal " & signo'image);
      begin
        case errno_code is
          when Clair.Error.EINVAL =>
            raise Clair.Error.Invalid_Argument with error_msg;
          when Clair.Error.EFAULT =>
            raise Clair.Error.Bad_Address with error_msg;
          when others =>
            raise Clair.Error.Unmapped_Error with error_msg;
        end case;
      end;
    end if;
  end set_action;

  procedure suspend (mask : aliased in Set) is
    function c_sigsuspend (sigmask : access constant Set)
      return Interfaces.C.int;
    pragma import (c, c_sigsuspend, "sigsuspend");

    retval : Interfaces.C.int;
  begin
    retval := c_sigsuspend (mask'access);

    -- sigsuspend always returns -1 and sets errno to EINTR.
    -- If it fails for any other reason, it is an unexpected and serious error.
    if Clair.Error.get_errno /= Clair.Error.EINTR then
      declare
        error_msg  : constant String :=
          Clair.Error.format_posix_error_message
            (errno_code    => Clair.Error.get_errno,
             function_name => "Clair.Signal.suspend (sigsuspend)",
             context_info  => "Unexpected failure");
      begin
        raise Program_Error with error_msg;
      end;
    end if;
  end suspend;

  procedure pause is
    empty_mask : aliased Clair.Signal.Set;
  begin
    empty_set (empty_mask);
    suspend (empty_mask);
  end pause;

end Clair.Signal;
