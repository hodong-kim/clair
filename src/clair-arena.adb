-- clair-arena.adb
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
with Interfaces.C;
with System;
with Ada.Strings.Fixed;
with Clair.Log;
with Clair.Errno;

package body Clair.Arena is
  use type Interfaces.C.int;
  use type Interfaces.C.size_t;
  use type Interfaces.C.unsigned;
  use type System.Storage_Elements.Storage_Offset;
  use type System.Address;

  -- void *mallocx (size_t size, int flags);
  function mallocx (
    size  : Interfaces.C.size_t;
    flags : Interfaces.C.int
  ) return System.Address
  with import, convention => c, external_name => "mallocx";

  -- void sdallocx (void *ptr, size_t size, int flags);
  procedure sdallocx (
    ptr   : System.Address;
    size  : Interfaces.C.size_t;
    flags : Interfaces.C.int
  )
  with import, convention => c, external_name => "sdallocx";

  -- int mallctl (
  --   const char *name, void *oldp, size_t *oldlenp, void *newp, size_t newlen
  -- );
  function mallctl (
    name    : System.Address;
    oldp    : System.Address;
    oldlenp : access Interfaces.C.size_t;
    newp    : System.Address;
    newlen  : Interfaces.C.size_t
  ) return Interfaces.C.int
  with import, convention => c, external_name => "mallctl";

  -- [helper functions] flag generation
  function get_arena_flag (index : Interfaces.C.unsigned)
  return Interfaces.C.unsigned
  with import, convention => c, external_name => "clair_get_arena_flag";

  function get_align_flag (alignment : Interfaces.C.size_t)
  return Interfaces.C.unsigned
  with import, convention => c, external_name => "clair_get_align_flag",
       pre => (alignment > 0) and then ((alignment and (alignment - 1)) = 0);

  overriding procedure initialize (self : in out Pool) is
    index   : aliased Interfaces.C.unsigned := 0;
    size    : aliased Interfaces.C.size_t
            := Interfaces.C.unsigned'size / System.Storage_Unit;
    retval  : Interfaces.C.int;
    key     : aliased constant Interfaces.C.char_array
            := Interfaces.C.to_c ("arenas.create");
  begin
    if self.is_initialized then
      return;
    end if;

    retval := mallctl (key(key'first)'address,
                       index'address,
                       size'access,
                       System.NULL_ADDRESS,
                       0);

    if retval /= 0 then
      raise Storage_Error with
        Clair.Errno.format_posix_error_message (
          errno_code    => retval,
          function_name => "Clair.Arena.initialize",
          context_info  => "Failed to initialize arena " &
                           Ada.Strings.Fixed.trim (index'image,
                                                   Ada.Strings.Both)
        );
    end if;

    self.arena_index    := index;
    self.is_initialized := True;
  end initialize;

  overriding procedure finalize (self : in out Pool) is
    retval : Interfaces.C.int;
  begin
    if not self.is_initialized then
      return;
    end if;

    declare
      key_flush : aliased constant Interfaces.C.char_array
                := Interfaces.C.to_c ("thread.tcache.flush");
      -- index string without leading/trailing spaces
      index_str : constant String
                := Ada.Strings.Fixed.trim (self.arena_index'image,
                                           Ada.Strings.Both);
      key_destroy : aliased constant Interfaces.C.char_array
                  := Interfaces.C.to_c ("arena." & index_str & ".destroy");
    begin
      retval := mallctl (key_flush(key_flush'first)'address,
                         System.NULL_ADDRESS,
                         null,
                         System.NULL_ADDRESS,
                         0);

      if retval /= 0 then
        Clair.Log.warning (
          Clair.Errno.format_posix_error_message (
            errno_code    => retval,
            function_name => "Clair.Arena.finalize",
            context_info  => "Failed to flush tcache " &
                            Ada.Strings.Fixed.trim (self.arena_index'image,
                                                    Ada.Strings.Both)
          )
        );
      end if;

      -- Convert the array address to a pointer and pass it
      retval := mallctl (key_destroy(key_destroy'first)'address,
                         System.NULL_ADDRESS,
                         null,
                         System.NULL_ADDRESS,
                         0);

      if retval = 0 then
        self.is_initialized := False;
      else
        -- Instead of raising an exception, log the failure so that a
        -- potential memory leak is visible. According to jemalloc docs,
        -- destruction can fail if a thread is still bound to the arena.
        Clair.Log.warning (
          Clair.Errno.format_posix_error_message (
            errno_code    => retval,
            function_name => "Clair.Arena.finalize",
            context_info  => "Failed to destroy arena " &
                            Ada.Strings.Fixed.trim (self.arena_index'image,
                                                    Ada.Strings.Both)
          )
        );
      end if;
    end;
  end finalize;

  overriding procedure allocate (
    self      : in out Pool;
    address   : out System.Address;
    size      : System.Storage_Elements.Storage_Count;
    alignment : System.Storage_Elements.Storage_Count)
  is
    flags    : Interfaces.C.unsigned;
    pointer  : System.Address;
    req_size : constant Interfaces.C.size_t := Interfaces.C.size_t(size);
  begin
    flags := get_arena_flag (self.arena_index);

    if alignment > 0 then
      flags := flags or get_align_flag (Interfaces.C.size_t(alignment));
    end if;

    pointer := mallocx (req_size, uint_to_int (flags));

    if pointer = System.NULL_ADDRESS then
        raise Storage_Error with
          Clair.Errno.format_posix_error_message (
            errno_code    => Clair.Errno.get_errno,
            function_name => "Clair.Arena.allocate",
            context_info  => "jemalloc: Out of memory in arena " &
                             self.arena_index'image
          );
    end if;

    address := pointer;
  end allocate;

  overriding procedure deallocate (
    self      : in out Pool;
    address   : System.Address;
    size      : System.Storage_Elements.Storage_Count;
    alignment : System.Storage_Elements.Storage_Count)
  is
    flags : Interfaces.C.unsigned;
  begin
    flags := get_arena_flag (self.arena_index);

    if alignment > 0 then
      flags := flags or get_align_flag (Interfaces.C.size_t(alignment));
    end if;

    if address /= System.NULL_ADDRESS then
        sdallocx (address, Interfaces.C.size_t(size),
                  uint_to_int (flags));
    end if;
  end deallocate;

  overriding function storage_size (self : Pool)
    return System.Storage_Elements.Storage_Count is
  begin
    return System.Storage_Elements.Storage_Count'last;
  end storage_size;

end Clair.Arena;
