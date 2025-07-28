-- test_clair_process.adb
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

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Clair.Process;
with Clair.Signal;
with Clair.Types;
with Interfaces.C;
use type Clair.Types.pid_t;

procedure Test_Clair_Process is

  procedure test_get_pid is
    pid : Clair.Types.pid_t;
  begin
    Ada.Text_IO.put ("Testing Clair.Process.get_pid... ");
    pid := Clair.Process.get_pid;
    if pid > 0 then
       Ada.Text_IO.put_line ("OK (PID: " & pid'image & ")");
    else
       Ada.Text_IO.put_line ("FAIL");
       Ada.Command_Line.set_exit_status (Ada.Command_Line.Failure);
    end if;
  end test_get_pid;

  procedure test_fork_and_send_signal_to is
  begin
    Ada.Text_IO.put ("Testing Clair.Process.fork and .send... ");
    declare
      result : Clair.Process.Fork_Result := Clair.Process.fork;
    begin
      case result.status is
        when Clair.Process.Parent =>
          delay 0.1;
          Clair.Process.send_signal_to (result.child_pid, Clair.Signal.SIGTERM);
          Ada.Text_IO.put_line ("OK (Parent sent signal)");

        when Clair.Process.Child =>
          loop
            declare
              retval : constant Interfaces.C.int := Clair.Signal.pause;
            begin
              pragma Unreferenced (retval);
            end;
          end loop;
      end case;
    end;
  end test_fork_and_send_signal_to;

begin
  Ada.Text_IO.put_line ("==== Running Clair.Process Tests (Single File) ====");
  test_get_pid;
  test_fork_and_send_signal_to;

exception
  when E : others =>
    Ada.Text_IO.put_line ("An unexpected error occurred: " & Ada.Exceptions.Exception_Message(E));
    Ada.Command_Line.set_exit_status (Ada.Command_Line.Failure);
end test_clair_process;
