-- test_clair_event_loop.ads
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
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

package body Test_Clair_Event_Loop is
  use Ada.Text_IO;
  use type System.Address;

  -- 1. Define local access type for accessing Context
  type Local_Context_Ptr is access all Clair.Event_Loop.Context;

  -- 2. Address -> Pointer conversion function
  function to_context_ptr is new Ada.Unchecked_Conversion
    (source => System.Address,
     target => Local_Context_Ptr);

  procedure on_timeout
    (timer     : Clair.Event_Loop.Handle;
     user_data : System.Address) is

    -- 3. Restore context pointer from user_data
    self : constant Local_Context_ptr := to_context_ptr (user_data);
  begin
    new_line;
    put_line ("[Timer Callback] 5 second elapsed.");

    if user_data /= System.NULL_ADDRESS then
      put_line ("[Timer] Cleaning up myself (Safe removal)...");
      Clair.Event_Loop.remove (self.all, Timer);
      put_line ("[Timer Callback] Stopping the event loop via user_data...");
      Clair.Event_Loop.quit (self.all);
    else
      put_line ("[Error] user_data is Null. Cannot stop loop.");
    end if;
  end on_timeout;

  ----------------------------------------------------------------------------
  -- STDIN Callback: Quit loop on "exit" input
  ----------------------------------------------------------------------------
  procedure on_stdin (io        : Clair.Event_Loop.Handle;
                      fd        : Clair.File.Descriptor;
                      events    : Clair.Event_Loop.Event_Mask;
                      user_data : System.Address)
  is
    pragma unreferenced (io, fd, events);
    self : constant Local_Context_ptr := to_context_ptr (user_data);
    buf  : String (1 .. 1024);
    last : Natural;
  begin
    -- [Note] Get_Line is a blocking function. Since this is for testing,
    -- be aware it will wait here until Enter is pressed.
    begin
      get_line (buf, last);
    exception
      when Ada.Text_IO.End_Error =>
        put_line ("[Stdin] End of stream. Quitting...");
        Clair.Event_Loop.quit (self.all);
        return;
    end;

    put_line ("[Stdin Callback] You entered: " & Buf (1 .. Last));

    if Buf (1 .. Last) = "exit" then
      put_line ("[Stdin Callback] Exit command received. Stopping...");
      Clair.Event_Loop.quit (self.all);
    end if;
  end on_stdin;

  procedure on_signal (signal    : Clair.Event_Loop.Handle;
                       signum    : Clair.Signal.Number;
                       user_data : System.Address)
  is
    pragma unreferenced (signal);
    -- Restore context pointer from user_data
    self : constant Local_Context_ptr := to_context_ptr (user_data);
  begin
    new_line;
    put_line ("[Signal Callback] Caught signal (" & signum'image & ").");

    if user_data /= System.NULL_ADDRESS then
      put_line ("[Signal Callback] Requesting loop stop...");
      Clair.Event_Loop.quit (self.all);
    else
      put_line ("[Error] user_data is Null. System might not stop cleanly.");
    end if;
  end on_signal;

end Test_Clair_Event_Loop;
