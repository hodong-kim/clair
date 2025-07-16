-- -*- Mode: Ada; indent-tabs-mode: nil; c-basic-offset: 2; tab-width: 2 -*-
-- dl.adb
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;

package body Dl is
  use Interfaces.C;
  use Interfaces.C.Strings;
  use System;

  function dlopen (path : Interfaces.C.Strings.Chars_Ptr;
                   flag : Interfaces.C.Int) return System.Address;
  pragma import (c, dlopen, "dl_open");

  function dlclose (handle : System.Address) return Interfaces.C.Int;
  pragma import (c, dlclose, "dlclose");

  function dlerror return Interfaces.C.Strings.Chars_Ptr;
  pragma import (c, dlerror, "dlerror");

  function dlsym (handle : System.Address;
                  symbol : Interfaces.C.Strings.Chars_Ptr)
    return System.Address;
  pragma import (c, dlsym, "dlsym");

  function to_chars_ptr is new Ada.Unchecked_Conversion (
    Source => System.Address,
    Target => Interfaces.C.Strings.Chars_Ptr
  );

  function open (path : String;
                 mode : Integer) return System.Address is
    handle : System.Address;
    c_path : constant Interfaces.C.Char_Array :=
      Interfaces.C.to_c (path) & Interfaces.C.Nul;
  begin
    handle := dlopen (to_chars_ptr (c_path'address),
                      Interfaces.C.Int (mode));
    if handle = System.Null_Address then
      declare
        errmsg_ptr : constant Interfaces.C.Strings.Chars_Ptr := dlerror;
        errmsg : constant String :=
          (if errmsg_ptr /= Interfaces.C.Strings.Null_Ptr then
             Interfaces.C.Strings.value (errmsg_ptr)
           else
             "Unknown dlopen error (dlerror returned NULL)");
      begin
        raise Library_Load_Error with "dlopen(" & path & "," & mode'image &
                                      ") failed: " & errmsg;
      end;
    end if;
    return handle;
  end open;

  procedure close (handle : in System.Address) is
    retval : Interfaces.C.Int;
  begin
    retval := dlclose (handle);

    if retval /= 0 then
      declare
        errmsg_ptr : constant Interfaces.C.Strings.chars_ptr := dlerror;
        errmsg     : constant String := (
          if errmsg_ptr /= Interfaces.C.Strings.Null_Ptr then
            Interfaces.C.Strings.value (errmsg_ptr)
          else "Unknown dlclose() error; dlerror() returns NULL");
      begin
        raise Library_Close_Error with "dlclose failed: " & errmsg;
      end;
    end if;
  end close;

  function get_symbol (handle   : in System.Address;
                       sym_name : in String) return System.Address is
    sym_addr : System.Address;
    c_sym_name : constant Interfaces.C.Char_Array :=
      Interfaces.C.to_c (sym_name) & Interfaces.C.Nul;
    errmsg_ptr : Interfaces.C.Strings.Chars_Ptr;
    dummy_ptr  : Interfaces.C.Strings.Chars_Ptr;

  begin
    if handle = System.Null_Address then
      raise Program_Error
        with "Attempt to call dlsym with a null library handle.";
    end if;

    dummy_ptr := dlerror;
    sym_addr  := dlsym (handle, to_chars_ptr (c_sym_name'address));

    if sym_addr = System.Null_Address then
      errmsg_ptr := dlerror;

      if errmsg_ptr /= Interfaces.C.Strings.Null_Ptr then
        declare
          errmsg : constant String := Interfaces.C.Strings.Value (errmsg_ptr);
        begin
          raise Symbol_Lookup_Error
            with "dlsym lookup for '" & sym_name & "' failed: " & errmsg;
        end;
      end if;
    end if;

    return sym_addr;
  end get_symbol;
end Dl;
