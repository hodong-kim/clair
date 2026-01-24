-- clair-file.adb
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
with Clair.Errno;
with Clair.Exceptions;
with Ada.Unchecked_Conversion;

package body Clair.File is

  use type Interfaces.C.int;

  -----------------------------------------------------------------------------
  -- C Imports
  -----------------------------------------------------------------------------
  function c_open2 (path  : Interfaces.C.char_array;
                    flags : Interfaces.C.int) return Descriptor;
  function c_open3 (path  : Interfaces.C.char_array;
                    flags : Interfaces.C.int;
                    mode  : Clair.Platform.mode_t) return Descriptor;
  pragma import (c, c_open2, "open");
  pragma import (c, c_open3, "open");

  function c_flock (fd : Interfaces.C.int; operation : Interfaces.C.int)
    return Interfaces.C.int;
  pragma import (c, c_flock, "flock");

  subtype Off_T is Clair.Platform.Off_T;

  function c_ftruncate (fd     : Interfaces.C.int;
                        length : Off_T) return Interfaces.C.int;
  pragma import (c, c_ftruncate, "ftruncate");

  function c_unlink (pathname : Interfaces.C.char_array) return Interfaces.C.int
  with import, convention => c, external_name => "unlink";

  -----------------------------------------------------------------------------
  -- Helpers
  -----------------------------------------------------------------------------
  function flags_to_uint is new Ada.Unchecked_Conversion
    (source => Flags, target => Interfaces.C.unsigned);

  function uint_to_flags is new Ada.Unchecked_Conversion
    (source => Interfaces.C.unsigned, target => Flags);

  function uint_to_int is new Ada.Unchecked_Conversion
    (source => Interfaces.C.unsigned, target => Interfaces.C.int);

  function "or" (left, right : Flags) return Flags is
    use type Interfaces.C.unsigned;
  begin
    return uint_to_flags (flags_to_uint (left) or flags_to_uint (right));
  end "or";

  procedure handle_open_error (errno_code : in Interfaces.C.int;
                               path       : in String) is
    error_msg : constant String :=
      Clair.Errno.format_posix_error_message
        (errno_code    => errno_code,
         function_name => "Clair.File.open",
         context_info  => "on path """ & path & """");
  begin
    Clair.Errno.raise_from_errno (errno_code, error_msg);
  end handle_open_error;

  -----------------------------------------------------------------------------
  -- Implementations
  -----------------------------------------------------------------------------

  function open (path : String;  flags : File.Flags) return Descriptor is
    new_fd      : Descriptor;
    retry_count : Natural := 0;
    c_path_str  : constant Interfaces.C.char_array := Interfaces.C.to_c (path);
  begin
    loop
      new_fd := c_open2 (c_path_str,
                         Interfaces.C.int(flags));

      if new_fd /= -1 then
        exit;
      end if;

      declare
        errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
      begin
        if errno_code = Clair.Errno.EINTR then
          retry_count := retry_count + 1;
          if retry_count > Clair.Max_EINTR_Retries then
            raise Clair.Exceptions.Interrupted_System_Call with
              "Clair.File.open: Aborted after " &
              Clair.Max_EINTR_Retries'image &
              " consecutive EINTR signals on path """ & path & """";
          end if;
        else
          handle_open_error (errno_code, path);
        end if;
      end;
    end loop;

    return new_fd;
  end open;

  function open (path  : String;
                 flags : File.Flags;
                 mode  : Clair.Platform.mode_t) return Descriptor
  is
    new_fd      : Descriptor;
    retry_count : Natural := 0;
    c_path_str  : constant Interfaces.C.char_array := Interfaces.C.to_c (path);
  begin
    loop
      new_fd := c_open3 (c_path_str,
                         Interfaces.C.int(flags),
                         mode);

      -- [Success]
      if new_fd /= -1 then
        exit;
      end if;

      declare
        errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
      begin
        if errno_code = Clair.Errno.EINTR then
          -- Signal interrupt: Increment counter
          retry_count := retry_count + 1;
          if retry_count > Clair.Max_EINTR_Retries then
            raise Clair.Exceptions.Interrupted_System_Call with
                    "Clair.File.open: Aborted after " &
                    Clair.Max_EINTR_Retries'image &
                    " consecutive EINTR signals on path """ & path & """";
          end if;
        else
          -- Handle other errors via specific handler or raise exception
          -- immediately
          handle_open_error (errno_code, path);
        end if;
      end;
    end loop;

    return new_fd;
  end open;

  -- The umask() system call is always successful.
  function umask (new_mask : Clair.Platform.mode_t)
  return Clair.Platform.mode_t is
    function c_umask (mask : Clair.Platform.mode_t)
    return Clair.Platform.mode_t;
    pragma import (c, c_umask, "umask");
  begin
    return c_umask (new_mask);
  end umask;

  procedure lock (fd        : Descriptor;
                  kind      : Lock_Kind := Exclusive;
                  blocking  : Boolean   := False)
  is
    use type Interfaces.C.unsigned;

    op : Interfaces.C.unsigned
       := Interfaces.C.unsigned(Lock_Kind'enum_rep(kind));

    errno_code   : Interfaces.C.int;
    retry_count  : Natural := 0;
  begin
    if not blocking then
      op := op or Clair.Platform.LOCK_NB;
    end if;

    loop
      if c_flock (Interfaces.C.int(fd), uint_to_int (op)) = 0 then
        exit;
      end if;

      errno_code := Clair.Errno.get_errno;

      if errno_code = Clair.Errno.EINTR then
        retry_count := retry_count + 1;
        if retry_count > Clair.Max_EINTR_Retries then
          Clair.Errno.raise_from_errno (errno_code,
            "File lock (flock) failed: EINTR limit exceeded on fd " & fd'image);
        end if;

      -- EWOULDBLOCK (File is already locked; occurs only in non-blocking mode)
      -- Treat this as a failure and raise an exception.
      else
        if errno_code = Clair.Platform.EWOULDBLOCK then
          Clair.Errno.raise_from_errno (errno_code,
            "File lock busy (EWOULDBLOCK) on fd " & fd'image);
        else
          Clair.Errno.raise_from_errno (errno_code,
            "File lock (flock) operation failed on fd " & fd'image);
        end if;
      end if;
    end loop;
  end lock;

  procedure unlock (fd : Descriptor) is
    errno_code   : Interfaces.C.int;
    retry_count  : Natural := 0;
  begin
    loop
      if c_flock (Interfaces.C.int(fd), Clair.Platform.LOCK_UN) = 0 then
        exit;
      end if;

      errno_code := Clair.Errno.get_errno;

      if errno_code = Clair.Errno.EINTR then
        retry_count := retry_count + 1;
        if retry_count > Clair.Max_EINTR_Retries then
          Clair.Errno.raise_from_errno (errno_code,
            "File unlock (flock) failed: EINTR limit exceeded on fd " &
            fd'image);
        end if;

      else
        Clair.Errno.raise_from_errno (errno_code,
          "File unlock (flock) operation failed on fd " & fd'image);
      end if;
    end loop;
  end unlock;

  procedure truncate (fd      : Descriptor;
                      length : Ada.Streams.Stream_Element_Offset := 0) is
  begin
    if c_ftruncate (Interfaces.C.int(fd), Off_T(length)) = -1 then
      Clair.Errno.raise_from_errno (Clair.Errno.get_errno,
                                    "Clair.File.truncate failed");
    end if;
  end truncate;

  procedure unlink (path : String) is
  begin
    if c_unlink (Interfaces.C.to_c (path)) = -1 then
        declare
          err : constant Interfaces.C.int := Clair.Errno.get_errno;
        begin
          -- Ignore ENOENT (file not found) and proceed.
          if err /= Clair.Errno.ENOENT then
              Clair.Errno.raise_from_errno (err, "unlink failed for: " & path);
          end if;
        end;
    end if;
  end unlink;

end Clair.File;
