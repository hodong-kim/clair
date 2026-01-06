-- clair-file.adb
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
with System;
with Clair.Errno;
with Clair.Exceptions;

package body Clair.File is
  use type Interfaces.C.int;
  use type Clair.Platform.ssize_t;

  -- C functions from <unistd.h> are now imported directly.
  function c_close (fd : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_close, "close");

  function c_read (fd    : Interfaces.C.int;
                   buf   : System.Address;
                   count : Interfaces.C.size_t) return Clair.Platform.ssize_t;
  pragma import (c, c_read, "read");

  function c_write (fd    : Interfaces.C.int;
                    buf   : System.Address;
                    count : Interfaces.C.size_t) return Clair.Platform.ssize_t;
  pragma import (c, c_write, "write");

  function c_dup (oldfd : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_dup, "dup");

  function c_dup2 (oldfd : Interfaces.C.int;
                   newfd : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_dup2, "dup2");

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

  -- Version with two arguments (Void Argument)
  -- Usage: Used to retrieve values such as F_GETFL, F_GETFD, F_GETOWN, etc.
  function fcntl2 (fd  : Descriptor;
                   cmd : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, fcntl2, "fcntl");

  function flags_to_uint is new Ada.Unchecked_Conversion
    (source => Flags, target => Interfaces.C.unsigned);

  function uint_to_flags is new Ada.Unchecked_Conversion
    (source => Interfaces.C.unsigned, target => Flags);

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

  procedure close (fd : in Descriptor) renames Clair.IO.close;

  function read (fd     : in Descriptor;
                 buffer : in out System.Storage_Elements.Storage_Array)
  return Natural is
    bytes_read  : Clair.Platform.ssize_t;
    retry_count : Natural := 0;
  begin
    loop
      bytes_read :=
        c_read (Interfaces.C.int(fd), buffer'address, buffer'length);

      -- [Success or EOF]
      -- bytes_read > 0: Data read
      -- bytes_read = 0: EOF (End Of File) reached -> Normal termination
      if bytes_read >= 0 then
        exit;
      end if;

      -- [Error handling]
      declare
        errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
      begin
        if errno_code = Clair.Errno.EINTR then
          retry_count := retry_count + 1;
          if retry_count > Clair.Max_EINTR_Retries then
            raise Clair.Exceptions.Interrupted_System_Call with
              "Clair.File.read: Aborted after " &
              Clair.Max_EINTR_Retries'image &
              " consecutive EINTR signals on fd " & fd'image;
          end if;
        else
          declare
            error_msg : constant String :=
              Clair.Errno.format_posix_error_message
                (errno_code    => errno_code,
                 function_name => "Clair.File.read",
                 context_info  => "on fd " & fd'image);
          begin
            Clair.Errno.raise_from_errno (errno_code, error_msg);
          end;
        end if;
      end;
    end loop;

    return Natural(bytes_read);
  end read;

  function write (fd     : in Descriptor;
                  buffer : in System.Storage_Elements.Storage_Array)
  return Natural is
    bytes_written : Clair.Platform.ssize_t;
    retry_count   : Natural := 0;
  begin
    loop
      bytes_written := c_write (Interfaces.C.int(fd),
                                buffer'address,
                                buffer'length);

      if bytes_written /= -1 then
        exit;
      end if;

      declare
        errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
      begin
        if errno_code = Clair.Errno.EINTR then
          retry_count := retry_count + 1;
          if retry_count > Clair.Max_EINTR_Retries then
            raise Clair.Exceptions.Interrupted_System_Call with
              "Clair.File.write: Aborted after " &
              Clair.Max_EINTR_Retries'image &
              " consecutive EINTR signals on fd " & fd'image;
          end if;
        else
          declare
            error_msg : constant String :=
              Clair.Errno.format_posix_error_message
                (errno_code    => errno_code,
                 function_name => "Clair.File.write",
                 context_info  => "on fd " & fd'image);
          begin
            Clair.Errno.raise_from_errno (errno_code, error_msg);
          end;
        end if;
      end;
    end loop;

    return Natural(bytes_written);
  end write;

  function duplicate (fd : in Descriptor) return Descriptor is
    new_fd : constant File.Descriptor :=
      File.Descriptor(c_dup (Interfaces.C.int(fd)));
  begin
    if new_fd = -1 then
      declare
        errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
        error_msg  : constant String :=
          Clair.Errno.format_posix_error_message
            (errno_code    => errno_code,
             function_name => "Clair.File.duplicate",
             context_info  => "on fd " & fd'image);
      begin
        Clair.Errno.raise_from_errno (errno_code, error_msg);
      end;
    end if;

    return new_fd;
  end duplicate;

function duplicate_to (fd     : in Descriptor;
                       new_fd : Descriptor) return Descriptor is
    result_fd   : Descriptor;
    retry_count : Natural := 0;
  begin
    loop
      result_fd :=
        Descriptor(c_dup2 (Interfaces.C.int(fd), Interfaces.C.int(new_fd)));
      if result_fd /= -1 then
        -- Success
        exit;
      end if;

      -- An error occurred (result_fd = -1), check errno.
      declare
        errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
      begin
        if errno_code = Clair.Errno.EINTR then
          -- Interrupted by a signal, prepare to retry.
          retry_count := retry_count + 1;
          if retry_count > Clair.Max_EINTR_Retries then
            raise Clair.Exceptions.Interrupted_System_Call with
              "Clair.File.duplicate_to: Aborted after " &
              Clair.Max_EINTR_Retries'image &
              " consecutive EINTR signals for fd " & fd'image & " to " &
              new_fd'image;
          end if;
        else
          -- A real, unrecoverable error occurred.
          declare
            error_msg : constant String :=
              Clair.Errno.format_posix_error_message
                (errno_code    => errno_code,
                 function_name => "Clair.File.duplicate_to",
                 context_info  => "from fd " & fd'image &
                                  " to new_fd " & new_fd'image);
          begin
            Clair.Errno.raise_from_errno (errno_code, error_msg);
          end;
        end if;
      end;
    end loop;

    return result_fd;

  end duplicate_to;

  -- The umask() system call is always successful.
  function umask (new_mask : Clair.Platform.mode_t)
  return Clair.Platform.mode_t is
    function c_umask (mask : Clair.Platform.mode_t)
    return Clair.Platform.mode_t;
    pragma import (c, c_umask, "umask");
  begin
    return c_umask (new_mask);
  end umask;

  -- Ensure that standard I/O descriptors (0, 1, 2) are open.
  -- If they are closed, redirect them to /dev/null.
  procedure ensure_standard_descriptors is
    null_fd : Descriptor;
    flags   : Interfaces.C.int;
    retval  : Interfaces.C.int;
    pragma unreferenced (retval);
  begin
    -- Open /dev/null once.
    -- Direct call to c_open2 (prevents exceptions).
    null_fd := c_open2 (Interfaces.C.to_c ("/dev/null"), O_RDWR);

    if null_fd = INVALID_DESCRIPTOR then
      -- If open fails, no further action can be taken,
      -- so the result is ignored.
      return;
    end if;

    for fd in 0 .. 2 loop
      -- Use fcntl to check if the file descriptor is valid (open).
      -- If F_GETFL returns -1 and errno is EBADF, the descriptor is closed.
      flags := fcntl2 (Descriptor(fd), F_GETFL);

      if flags = -1 and then Clair.Errno.get_errno = Clair.Errno.EBADF then
        -- If closed, duplicate the previously opened null_fd to this slot (fd).
        -- dup2 copies the file descriptor to the specified fd number.
        -- Direct call to c_dup2 (prevents exceptions, ignores failure).
        retval := c_dup2 (Interfaces.C.int(null_fd), Interfaces.C.int(fd));
        -- If dup2 fails, no further action can be taken,
        -- so the result is ignored.
      end if;
    end loop;

    -- Close null_fd if it is greater than 2. (i.e., if it was opened as
    -- a temporary fd and not used to fill 0, 1, or 2).
    if null_fd > 2 then
      -- Direct call to c_close instead of Ada wrapper (prevents exceptions).
      retval := c_close (Interfaces.C.int(null_fd));
    end if;
  end ensure_standard_descriptors;

  procedure lock (fd       : Descriptor;
                  kind     : Lock_Kind := Exclusive;
                  blocking : Boolean   := False)
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

  procedure truncate (fd     : Descriptor;
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
