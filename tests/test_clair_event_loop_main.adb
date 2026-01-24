-- test_clair_event_loop_main.adb
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
with Ada.Exceptions;
with Ada.Text_IO;
with Clair.Event_Loop;
with Clair.Signal;
with Test_Clair_Event_Loop;

procedure Test_Clair_Event_Loop_Main is
  use type Clair.Event_Loop.Handle;
  mainloop : aliased Clair.Event_Loop.Context;
  timer    : Clair.Event_Loop.Handle;
  signal   : Clair.Event_Loop.Handle;
  watch    : Clair.Event_Loop.Handle;
  STDIN_FD : constant := 0;
begin
  Ada.Text_IO.put_line ("==================================================");
  Ada.Text_IO.put_line ("     Clair Event Loop: The Ultimate Test Case     ");
  Ada.Text_IO.put_line ("==================================================");

  Clair.Event_Loop.initialize (mainloop);

  timer := Clair.Event_Loop.add_timer (
    self      => mainloop,
    interval  => 5000,
    callback  => Test_Clair_Event_Loop.on_timeout'access,
    one_shot  => True,
    user_data => mainloop'address
  );

  signal := Clair.Event_Loop.add_unix_signal
    (self      => mainloop,
    signum    => Clair.Signal.INTERRUPT,
    callback  => Test_Clair_Event_Loop.on_signal'access,
    user_data => mainloop'address);

  watch := Clair.Event_Loop.add_watch (
    self      => mainloop,
    fd        => STDIN_FD,
    events    => Clair.Event_Loop.EVENT_INPUT,
    callback  => Test_Clair_Event_Loop.on_stdin'access,
    user_data => mainloop'address
  );

  if timer = Clair.Event_Loop.Null_Handle or
    signal = Clair.Event_Loop.Null_Handle or
    watch  = Clair.Event_Loop.Null_Handle
  then
    Ada.Text_IO.put_line ("[Error] Some event sources failed to register.");
    Clair.Event_Loop.finalize (mainloop);
    return;
  end if;

  Ada.Text_IO.put_line ("[Main] Loop is now active. You can:");
  Ada.Text_IO.put_line ("  1. Wait 10s for the timer.");
  Ada.Text_IO.put_line ("  2. Type something and press Enter.");
  Ada.Text_IO.put_line ("  3. Type 'exit' to quit.");
  Ada.Text_IO.put_line ("  4. Press Ctrl+C to interrupt.");
  Ada.Text_IO.new_line;

  Clair.Event_Loop.run (mainloop);

  Ada.Text_IO.put_line ("[Main] Loop terminated. Cleaning up all resources...");
  Clair.Event_Loop.remove   (mainloop, timer);
  Clair.Event_Loop.remove   (mainloop, signal);
  Clair.Event_Loop.remove   (mainloop, watch);
  Clair.Event_Loop.remove   (mainloop, watch);
  Clair.Event_Loop.finalize (mainloop);

exception
  when exc : others =>
    Ada.Text_IO.put_line (Ada.Exceptions.exception_name (exc) & ": " &
                          Ada.Exceptions.exception_message (exc));
    begin
      Clair.Event_Loop.finalize (mainloop);
    exception
      when others => null;
    end;
end Test_Clair_Event_Loop_Main;
