-- clair-dl.adb
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
with Interfaces.C;
with Interfaces.C.Strings;
with Clair.Exceptions;

package body Clair.DL is
  use type Interfaces.C.int;
  use type Interfaces.C.Strings.chars_ptr;
  use type System.Address;

  function c_dlopen (path : Interfaces.C.Strings.chars_ptr;
                     mode : Interfaces.C.int) return System.Address;
  pragma import (c, c_dlopen, "dlopen");

  function c_dlsym (handle : System.Address;
                    symbol : Interfaces.C.Strings.chars_ptr)
  return System.Address;
  pragma import (c, c_dlsym, "dlsym");

  function c_dlerror return Interfaces.C.Strings.chars_ptr;
  pragma import (c, c_dlerror, "dlerror");

  function c_dlclose (handle : System.Address) return Interfaces.C.int;
  pragma import (c, c_dlclose, "dlclose");

  function get_dl_error return String is
    errmsg_ptr : constant Interfaces.C.Strings.chars_ptr := c_dlerror;
  begin
    if errmsg_ptr /= Interfaces.C.Strings.NULL_PTR then
      return Interfaces.C.Strings.value (errmsg_ptr);
    else
      return "";
    end if;
  end get_dl_error;

  function open (path : String; mode : Integer) return Handle is
    c_path : aliased constant Interfaces.C.char_array
           := Interfaces.C.to_c (path);
    lib    : System.Address;
  begin
    lib := c_dlopen (sys_addr_to_chars_ptr (c_path'address),
                     Interfaces.C.int(mode));
    if lib = System.NULL_ADDRESS then
      raise Clair.Exceptions.Library_Load_Error with
            "dlopen(" & path & "," & mode'image & ") failed: " & get_dl_error;
    end if;
    return Handle(lib);
  end open;

  procedure close (lib : in out Handle) is
    retval : Interfaces.C.int;
  begin
    if lib = NULL_HANDLE then
      return;
    end if;
    retval := c_dlclose (System.Address(lib));

    if retval /= 0 then
      raise Clair.Exceptions.Library_Close_Error with
            "dlclose failed: " & get_dl_error;
    end if;

    lib := NULL_HANDLE;
  end close;

  function find_symbol (lib      : in Handle;
                        sym_name : in String) return System.Address is
    sym_addr     : System.Address;
    c_sym_name   : aliased constant Interfaces.C.char_array
                 := Interfaces.C.to_c (sym_name);
    dummy_result : Interfaces.C.Strings.chars_ptr;
  begin
    if lib = NULL_HANDLE then
      raise Program_Error
        with "Attempt to call dlsym with a null library handle.";
    end if;

    -- Clear any old error conditions before calling dlsym.
    dummy_result := c_dlerror;
    pragma unreferenced (dummy_result);
    sym_addr     := c_dlsym (System.Address(lib),
                             sys_addr_to_chars_ptr (c_sym_name'address));
    -- If dlsym returns NULL, check dlerror to see if it was a real error.
    if sym_addr = System.NULL_ADDRESS then
      declare
        errmsg : constant String := get_dl_error;
      begin
        if errmsg'length > 0 then
          raise Clair.Exceptions.Symbol_Lookup_Error
            with "dlsym lookup for '" & sym_name & "' failed: " & errmsg;
        end if;
      end;
    end if;
    return sym_addr;
  end find_symbol;

end Clair.DL;
