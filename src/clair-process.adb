-- clair-process.adb
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

with Clair.Error;
with Clair.Signal;
with Interfaces.C;

package body Clair.Process is
  use type Interfaces.C.int;
  use type Clair.Types.pid_t;

  -- C functions are now imported directly into the package body.
  function c_getpid return Clair.Types.pid_t;
  pragma import (c, c_getpid, "getpid");

  function c_setsid return Clair.Types.pid_t;
  pragma import (c, c_setsid, "setsid");

  function c_fork return Clair.Types.pid_t;
  pragma import (c, c_fork, "fork");

  function c_kill (pid : Clair.Types.pid_t;
                   sig : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_kill, "kill");

  procedure exit_process (status : Integer := EXIT_SUCCESS) is
    procedure c_exit (status_code : Interfaces.C.int);
    pragma import (c, c_exit, "exit");
    pragma no_return (c_exit);
  begin
    c_exit (Interfaces.C.int (status));
  end exit_process;

  function get_pid return Clair.Types.pid_t is
  begin
    return Clair.Types.pid_t (c_getpid);
  end get_pid;

  function set_sid return Clair.Types.pid_t is
    retval : constant Clair.Types.pid_t := Clair.Types.pid_t (c_setsid);
  begin
    if retval = -1 then
      declare
        errno_code : constant Interfaces.C.int := Clair.Error.get_errno;
        error_msg  : constant String :=
          Clair.Error.format_posix_error_message
            (errno_code    => errno_code,
             function_name => "Clair.Process.set_sid",
             context_info  => "call failed");
      begin
        case errno_code is
          when Clair.Error.EPERM =>
            raise Clair.Error.Operation_Not_Permitted with error_msg;
          when others =>
            raise Clair.Error.Unmapped_Error with error_msg;
        end case;
      end;
    end if;
    return retval;
  end set_sid;

  function fork return Fork_Result is
    retval : constant Clair.Types.pid_t := Clair.Types.pid_t (c_fork);
  begin
    case retval is
      when 0 =>
        -- In the child process
        return (status => Child);

      when -1 =>
        -- Error occurred
        declare
          errno_code : constant Interfaces.C.int := Clair.Error.get_errno;
          error_msg  : constant String :=
            Clair.Error.format_posix_error_message
              (errno_code    => errno_code,
               function_name => "Clair.Process.fork",
               context_info  => "call failed");
        begin
          case errno_code is
            when Clair.Error.EAGAIN =>
              raise Clair.Error.Resource_Temporarily_Unavailable with error_msg;
            when Clair.Error.ENOMEM =>
              raise Clair.Error.Cannot_Allocate_Memory with error_msg;
            when Clair.Error.ENOSYS => -- May occur on some systems
              raise Clair.Error.Function_Not_Implemented with error_msg;
            when others =>
              raise Clair.Error.Unmapped_Error with error_msg;
          end case;
        end;

      when others =>
        -- In the parent process
        return (status => Parent, child_pid => retval);
    end case;
  end fork;

  procedure send_signal_to (pid   : Clair.Types.pid_t;
                            signo : Clair.Signal.Number)
  is
    retval : constant Interfaces.C.int := c_kill (pid, signo);
  begin
    if retval /= 0 then
      declare
        errno_code : constant Interfaces.C.int := Clair.Error.get_errno;
        error_msg  : constant String :=
          Clair.Error.format_posix_error_message
            (errno_code    => errno_code,
             function_name => "Clair.Process.send_signal_to",
             context_info  => "to pid " & pid'image & " with signal "
                              & signo'image);
      begin
        case errno_code is
          when Clair.Error.EINVAL =>
            raise Clair.Error.Invalid_Argument with error_msg;
          when Clair.Error.ESRCH =>
            raise Clair.Error.No_Such_Process with error_msg;
          when Clair.Error.EPERM =>
            raise Clair.Error.Operation_Not_Permitted with error_msg;
          when others =>
            raise Clair.Error.Unmapped_Error with error_msg;
        end case;
      end;
    end if;
  end send_signal_to;
end Clair.Process;
