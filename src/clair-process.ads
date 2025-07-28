-- clair-process.ads
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

with Clair.Signal;
with Clair.Types;

package Clair.Process is

  EXIT_SUCCESS : constant := 0;
  EXIT_FAILURE : constant := 1;

  -- Wrapper for the C exit(3) function.
  -- With a default status of EXIT_SUCCESS, a simple call like
  -- Clair.Process.exit_process; results in a normal termination.
  procedure exit_process (status : Integer := EXIT_SUCCESS);
  pragma no_return (exit_process);

  -- Enumeration for the outcome of a fork call.
  type Fork_Status is (Parent, Child);

  -- Discriminated record for the result of a fork call.
  -- The child_pid field exists only when the status is Parent.
  type Fork_Result (status : Fork_Status) is record
    case status is
      when Parent =>
        child_pid : Clair.Types.pid_t;
      when Child =>
        null;
    end case;
  end record;

  function get_pid return Clair.Types.pid_t;
  function set_sid return Clair.Types.pid_t;
  function fork return Fork_Result;
  procedure send_signal_to (pid   : Clair.Types.pid_t;
                            signo : Clair.Signal.Number);

end Clair.Process;
