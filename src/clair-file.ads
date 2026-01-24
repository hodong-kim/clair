-- clair-file.ads
-- Copyright (c) 2025,2026 Hodong Kim <hodong@nimfsoft.com>
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
with Clair.Platform;
with Ada.Streams;
with Clair.IO;

package Clair.File is

  use type Clair.IO.Descriptor;
  subtype Descriptor is Clair.IO.Descriptor;

  type Flags is new Interfaces.C.int;
  function "or" (left, right : Flags) return Flags;

  O_RDONLY    : constant := Clair.Platform.O_RDONLY;
  O_WRONLY    : constant := Clair.Platform.O_WRONLY;
  O_RDWR      : constant := Clair.Platform.O_RDWR;
  O_ACCMODE   : constant := Clair.Platform.O_ACCMODE;
  O_NONBLOCK  : constant := Clair.Platform.O_NONBLOCK;
  O_APPEND    : constant := Clair.Platform.O_APPEND;
  O_SYNC      : constant := Clair.Platform.O_SYNC;
  O_NOFOLLOW  : constant := Clair.Platform.O_NOFOLLOW;
  O_CREAT     : constant := Clair.Platform.O_CREAT;
  O_TRUNC     : constant := Clair.Platform.O_TRUNC;
  O_EXCL      : constant := Clair.Platform.O_EXCL;
  O_NOCTTY    : constant := Clair.Platform.O_NOCTTY;
  O_DIRECTORY : constant := Clair.Platform.O_DIRECTORY;
  O_EXEC      : constant := Clair.Platform.O_EXEC;
  O_TTY_INIT  : constant := Clair.Platform.O_TTY_INIT;
  O_CLOEXEC   : constant := Clair.Platform.O_CLOEXEC;
  O_DSYNC     : constant := Clair.Platform.O_DSYNC;

  F_GETFL     : constant := Clair.Platform.F_GETFL;
  F_SETFL     : constant := Clair.Platform.F_SETFL;
  FD_CLOEXEC  : constant := Clair.Platform.FD_CLOEXEC;

  function open (path  : String;
                 flags : File.Flags) return Descriptor;
  function open (path  : String;
                 flags : File.Flags;
                 mode  : Clair.Platform.mode_t) return Descriptor;

  function umask (new_mask : Clair.Platform.mode_t)
  return Clair.Platform.mode_t;

  type Lock_Kind is (Shared, Exclusive);
  for  Lock_Kind use (
    Shared    => Clair.Platform.LOCK_SH,
    Exclusive => Clair.Platform.LOCK_EX
  );

  procedure lock (fd       : Descriptor;
                  kind     : Lock_Kind := Exclusive;
                  blocking : Boolean   := False);

  procedure unlock (fd : Descriptor);

  procedure truncate (fd     : Descriptor;
                      length : Ada.Streams.Stream_Element_Offset := 0);

  procedure unlink (path : String);

end Clair.File;
