-- test_clair_event_loop.ads
-- Copyright (c) 2025 Hodong Kim <hodong@nimfsoft.com>
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
with System;
with Clair.File;
with Clair.Event_Loop;
with Clair.Signal;

package Test_Clair_Event_Loop is

  procedure on_timeout (timer     : Clair.Event_Loop.Handle;
                        user_data : System.Address)
  with convention => c;

  procedure on_stdin (io        : Clair.Event_Loop.Handle;
                      fd        : Clair.File.Descriptor;
                      events    : Clair.Event_Loop.Event_Mask;
                      user_data : System.Address)
  with convention => c;

  procedure on_signal (signal    : Clair.Event_Loop.Handle;
                       signum    : Clair.Signal.Number;
                       user_data : System.Address)
  with convention => c;

end Test_Clair_Event_Loop;
