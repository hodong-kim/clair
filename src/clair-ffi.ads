-- clair-ffi.ads
-- Copyright (c) 2026 Hodong Kim <hodong@nimfsoft.com>
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
with Interfaces.C;

package Clair.FFI is
  pragma preelaborate;

  generic
    type Context_Type (<>) is limited private;
    type Context_Access is access all Context_Type;

    -- [Marshal] Raw Address -> Ada Access
    with function to_context (addr : System.Address) return Context_Access;

    -- [Handle] Ada Handle -> Raw Address
    type Handle_Type is private;
    with function to_address (h : Handle_Type) return System.Address;

  package Bindings is

    ---------------------------------------------------------------------------
    -- Group 1: Integer Return Bindings (Status Codes)
    ---------------------------------------------------------------------------

    -- clair_event_loop_run (1)
    -- clair_event_loop_quit (1)
    generic
      with function action (ctx : in out Context_Type) return Interfaces.C.int;
      function bind_int_1 (self : System.Address) return Interfaces.C.int;

    -- clair_event_loop_iterate (2)
    -- clair_event_loop_remove (2)
    generic
      type Arg1_Type is private;
      with function action (ctx : in out Context_Type;
                            a1  : Arg1_Type) return Interfaces.C.int;
      function bind_int_2 (self : System.Address;
                           a1   : Arg1_Type) return Interfaces.C.int;

    -- clair_event_loop_modify_watch (3)
    generic
      type Arg1_Type is private;
      type Arg2_Type is private;
      with function action (ctx : in out Context_Type;
                            a1  : Arg1_Type;
                            a2  : Arg2_Type) return Interfaces.C.int;
      function bind_int_3 (self : System.Address;
                           a1   : Arg1_Type;
                           a2   : Arg2_Type) return Interfaces.C.int;

    ---------------------------------------------------------------------------
    -- Group 2: Handle Return Bindings (Creators)
    -- Action returns 'Handle_Type', Binding converts it to 'System.Address'.
    ---------------------------------------------------------------------------

    -- clair_event_loop_add_idle (3)
    -- C Signature: void* func(void* self, cb, data);
    generic
      type Arg1_Type is private;
      type Arg2_Type is private;
      with function action (ctx : in out Context_Type;
                            a1  : Arg1_Type;
                            a2  : Arg2_Type) return Handle_Type;
      function bind_handle_3 (self : System.Address;
                              a1   : Arg1_Type;
                              a2   : Arg2_Type) return System.Address;

    -- clair_event_loop_add_unix_signal (4)
    -- C Signature: void* func(void* self, signum, cb, data);
    generic
      type Arg1_Type is private;
      type Arg2_Type is private;
      type Arg3_Type is private;
      with function action (ctx : in out Context_Type;
                            a1  : Arg1_Type;
                            a2  : Arg2_Type;
                            a3  : Arg3_Type) return Handle_Type;
      function bind_handle_4 (self : System.Address;
                              a1   : Arg1_Type;
                              a2   : Arg2_Type;
                              a3   : Arg3_Type) return System.Address;

    -- clair_event_loop_add_watch (5)
    -- clair_event_loop_add_timer (5)
    -- C Signature: void* func(void* self, a1, a2, a3, a4);
    generic
      type Arg1_Type is private;
      type Arg2_Type is private;
      type Arg3_Type is private;
      type Arg4_Type is private;
      with function action (ctx : in out Context_Type;
                            a1  : Arg1_Type;
                            a2  : Arg2_Type;
                            a3  : Arg3_Type;
                            a4  : Arg4_Type) return Handle_Type;
      function bind_handle_5 (self : System.Address;
                              a1   : Arg1_Type;
                              a2   : Arg2_Type;
                              a3   : Arg3_Type;
                              a4   : Arg4_Type) return System.Address;

  generic
    with function action (ptr : Context_Access) return Interfaces.C.int;
    function bind_destructor (self : System.Address) return Interfaces.C.int;

  end Bindings;

end Clair.FFI;
