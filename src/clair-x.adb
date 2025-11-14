-- clair-x.adb
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

with Ada.Exceptions;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with Clair.File;

package body Clair.X is
  use type System.Address;
  use type Interfaces.C.Strings.chars_ptr;
  use type Interfaces.C.int;

  function XOpenDisplay (display_name : Interfaces.C.Strings.chars_ptr :=
                         Interfaces.C.Strings.NULL_PTR)
  return Display_Handle;
  pragma import (c, XOpenDisplay, "XOpenDisplay");

  function XCloseDisplay (display : Display_Handle) return Interfaces.C.int;
  pragma import (c, XCloseDisplay, "XCloseDisplay");

  function XConnectionNumber (display : Display_Handle)
  return Interfaces.C.int;
  pragma import (c, XConnectionNumber, "XConnectionNumber");

  -- Ada-style wrappers
  function open_display (display_name : Interfaces.C.Strings.chars_ptr :=
                         Interfaces.C.Strings.NULL_PTR)
  return Display_Handle is
    display : Display_Handle := XOpenDisplay (display_name);
  begin
    if display = System.NULL_ADDRESS then
      declare
        message : constant String :=
          (if display_name = Interfaces.C.Strings.NULL_PTR
           then "XOpenDisplay failed (DISPLAY environment variable not set " &
                "or inaccessible)"
           else "XOpenDisplay failed for display_name:" &
             Interfaces.C.Strings.Value (display_name));
      begin
        raise Display_Open_Error with message;
      end;
    end if;
    return display;
  end open_display;

  procedure close_display (display : Display_Handle) is
    retval : Interfaces.C.int;
  begin
    if display /= System.NULL_ADDRESS then
      retval := XCloseDisplay (display);
      if retval /= 0 then
        raise Display_Close_Error with
          "XCloseDisplay failed (BadGC error possible)";
      end if;
    end if;
  end close_display;

  function get_connection_fd (display : Display_Handle)
  return Clair.File.Descriptor is
    fd : Interfaces.C.int;
  begin
    if display = System.NULL_ADDRESS then
      raise Invalid_Display_Handle with
        "get_connection_fd called with NULL display handle";
    end if;
    fd := XConnectionNumber (display);

    if fd < 0 then
      raise Connection_Fd_Error with
        "get_connection_fd returned invalid file descriptor: " &
        Interfaces.C.int'image (fd);
    end if;

    return Clair.File.Descriptor(fd);
  end get_connection_fd;

end Clair.X;
