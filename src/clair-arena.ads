-- clair-arena.ads
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
--
with System.Storage_Pools;
with System.Storage_Elements;

package Clair.Arena is
  pragma preelaborate;

  type Pool is new System.Storage_Pools.Root_Storage_Pool with private;

  overriding procedure initialize (self : in out Pool);
  overriding procedure finalize   (self : in out Pool);

  overriding procedure allocate (
    self      : in out Pool;
    address   : out System.Address;
    size      : System.Storage_Elements.Storage_Count;
    alignment : System.Storage_Elements.Storage_Count
  );

  overriding procedure deallocate (
    self      : in out Pool;
    address   : System.Address;
    size      : System.Storage_Elements.Storage_Count;
    alignment : System.Storage_Elements.Storage_Count
  );

  overriding function storage_size (self : Pool)
  return System.Storage_Elements.Storage_Count;

private

  type Pool is new System.Storage_Pools.Root_Storage_Pool with record
    arena_index    : Interfaces.C.unsigned;
    is_initialized : Boolean := False;
  end record;

end Clair.Arena;
