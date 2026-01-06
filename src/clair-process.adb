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
--
with Clair.Errno;
with Interfaces.C;

package body Clair.Process is
  use type Clair.Platform.pid_t;
  use type Interfaces.C.int;

  -- C functions are now imported directly into the package body.
  function c_getpid return Clair.Platform.pid_t;
  pragma import (c, c_getpid, "getpid");

  function c_setsid return Clair.Platform.pid_t;
  pragma import (c, c_setsid, "setsid");

  function c_fork return Clair.Platform.pid_t;
  pragma import (c, c_fork, "fork");

  function c_kill (pid : Clair.Platform.pid_t;
                   sig : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_kill, "kill");

  function c_daemon (nochdir : Interfaces.C.int;
                     noclose : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_daemon, "daemon");

  procedure exit_process (status : Integer := EXIT_SUCCESS) is
    procedure c_exit (status_code : Interfaces.C.int);
    pragma import (c, c_exit, "exit");
    pragma no_return (c_exit);
  begin
    c_exit (Interfaces.C.int(status));
  end exit_process;

  function get_pid return Clair.Platform.pid_t is
  begin
    return c_getpid;
  end get_pid;

  function set_sid return Clair.Platform.pid_t is
    retval : constant Clair.Platform.pid_t := c_setsid;
  begin
    if retval = -1 then
      declare
        errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
        error_msg  : constant String :=
          Clair.Errno.format_posix_error_message
            (errno_code    => errno_code,
             function_name => "Clair.Process.set_sid",
             context_info  => "call failed");
      begin
        Clair.Errno.raise_from_errno (errno_code, error_msg);
      end;
    end if;
    return retval;
  end set_sid;

  function fork return Fork_Result is
    retval : constant Clair.Platform.pid_t := c_fork;
  begin
    case retval is
      when 0 =>
        -- In the child process
        return (status => Child);

      when -1 =>
        -- Error occurred
        declare
          errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
          error_msg  : constant String :=
            Clair.Errno.format_posix_error_message
              (errno_code    => errno_code,
               function_name => "Clair.Process.fork",
               context_info  => "call failed");
        begin
          Clair.Errno.raise_from_errno  (errno_code, error_msg);
          -- Statement to satisfy the compiler's syntax rules
          -- For compilers that do not consider raise_from_errno as no_return,
          -- explicitly indicate "If execution reaches here, it's a program
          -- error" or insert a dummy return.
          raise Program_Error;
        end;

      when others =>
        -- In the parent process
        return (status => Parent, child_pid => retval);
    end case;
  end fork;

  procedure send_signal_to (pid   : Clair.Platform.pid_t;
                            signo : Clair.Signal.Number)
  is
    retval : constant Interfaces.C.int := c_kill (pid, Interfaces.C.int(signo));
  begin
    if retval /= 0 then
      declare
        errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
        error_msg  : constant String :=
          Clair.Errno.format_posix_error_message
            (errno_code    => errno_code,
             function_name => "Clair.Process.send_signal_to",
             context_info  => "to pid " & pid'image & " with signal "
                              & signo'image);
      begin
        Clair.Errno.raise_from_errno (errno_code, error_msg);
      end;
    end if;
  end send_signal_to;

  procedure daemonize (stay_in_current_dir : Boolean := False;
                       keep_standard_io    : Boolean := False) is
    c_nochdir : constant Interfaces.C.int
              := (if stay_in_current_dir then 1 else 0);
    c_noclose : constant Interfaces.C.int
              := (if keep_standard_io then 1 else 0);

    retval : Interfaces.C.int;
  begin
    retval := c_daemon (c_nochdir, c_noclose);
    if retval = -1 then
      Clair.Errno.raise_from_errno (Clair.Errno.get_errno,
        "Clair.Process.daemonize: " &
        "failed to detach from the controlling terminal (fork/open/setsid)");
    end if;

  end daemonize;

end Clair.Process;
