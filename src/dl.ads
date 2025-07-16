-- -*- Mode: Ada; indent-tabs-mode: nil; c-basic-offset: 2; tab-width: 2 -*-
-- dl.ads
with System;

package Dl is

  Library_Load_Error  : Exception;
  Library_Close_Error : Exception;
  Symbol_Lookup_Error : Exception;

  RTLD_LAZY   : constant Integer := 1;
  RTLD_NOW    : constant Integer := 2;
  RTLD_LOCAL  : constant Integer := 4;
  RTLD_GLOBAL : constant Integer := 8;

  function  open       (path     : in String;
                        mode     : in Integer)
                        return System.Address;
  procedure close      (handle   : in System.Address);
  function  get_symbol (handle   : in System.Address;
                        sym_name : in String)
                        return System.Address;
end Dl;
