-- clair-x.ads
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
with Interfaces.C.Strings;
with System;
with Clair.File;

package Clair.X is

   Display_Open_Error     : exception;
   Display_Close_Error    : exception;
   Connection_Fd_Error    : exception;
   Invalid_Display_Handle : exception;

   subtype Display_Handle is System.Address;

   function open_display (display_name : Interfaces.C.Strings.chars_ptr :=
                          Interfaces.C.Strings.NULL_PTR)
   return Display_Handle;

   procedure close_display (display : Display_Handle);

   function get_connection_fd (display : Display_Handle)
   return Clair.File.Descriptor;

end Clair.X;
