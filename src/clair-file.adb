-- clair-file.adb
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

with System;
with Interfaces.C;
with Clair.Types;
with Clair.Error;
with System.Storage_Elements;

package body Clair.File is
  use type Interfaces.C.int;
  use type Interfaces.C.long;

  type C_FD_Array is array (0 .. 1) of aliased Interfaces.C.int;

  -- C functions from <unistd.h> are now imported directly.
  function c_close (fd : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_close, "close");

  function c_read (fd    : Interfaces.C.int;
                   buf   : System.Address;
                   count : Interfaces.C.size_t) return Clair.Types.ssize_t;
  pragma import (c, c_read, "read");

  function c_write (fd    : Interfaces.C.int;
                    buf   : System.Address;
                    count : Interfaces.C.size_t) return Clair.Types.ssize_t;
  pragma import (c, c_write, "write");

  function c_pipe (pipefd : access Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_pipe, "pipe");

  function c_dup (oldfd : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_dup, "dup");

  function c_dup2 (oldfd : Interfaces.C.int;
                   newfd : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_dup2, "dup2");

  function c_open2 (path  : Interfaces.C.char_array;
                    flags : Interfaces.C.int) return Descriptor;
  function c_open3 (path  : Interfaces.C.char_array;
                    flags : Interfaces.C.int;
                    mode  : Clair.Types.mode_t) return Descriptor;
  pragma import (c, c_open2, "open");
  pragma import (c, c_open3, "open");

  procedure handle_open_error (errno_code : in Interfaces.C.int;
                               path       : in String) is
    error_msg : constant String :=
      Clair.Error.format_posix_error_message
        (errno_code    => errno_code,
         function_name => "Clair.File.open",
         context_info  => "on path """ & path & """");
  begin
    case errno_code is
      when Clair.Error.ENOTDIR =>
        raise Clair.Error.Not_A_Directory with error_msg;
      when Clair.Error.ENAMETOOLONG =>
        raise Clair.Error.File_Name_Too_Long with error_msg;
      when Clair.Error.ENOENT =>
        raise Clair.Error.No_Such_File_Or_Directory with error_msg;
      when Clair.Error.EACCES =>
        raise Clair.Error.Permission_Denied with error_msg;
      when Clair.Error.EPERM =>
        raise Clair.Error.Operation_Not_Permitted with error_msg;
      when Clair.Error.ELOOP =>
        raise Clair.Error.Too_Many_Levels_Of_Symbolic_Links with error_msg;
      when Clair.Error.EISDIR =>
        raise Clair.Error.Is_A_Directory with error_msg;
      when Clair.Error.EROFS =>
        raise Clair.Error.Read_Only_File_System with error_msg;
      when Clair.Error.EMFILE =>
        raise Clair.Error.Too_Many_Open_Files with error_msg;
      when Clair.Error.EMLINK =>
        raise Clair.Error.Too_Many_Links with error_msg;
      when Clair.Error.ENXIO =>
        raise Clair.Error.Device_Not_Configured with error_msg;
      when Clair.Error.EOPNOTSUPP =>
        raise Clair.Error.Operation_Not_Supported with error_msg;
      when Clair.Error.EAGAIN =>
        raise Clair.Error.Resource_Temporarily_Unavailable with error_msg;
      when Clair.Error.ENOSPC =>
        raise Clair.Error.No_Space_Left_On_Device with error_msg;
      when Clair.Error.EDQUOT =>
        raise Clair.Error.Disc_Quota_Exceeded with error_msg;
      when Clair.Error.EIO =>
        raise Clair.Error.Input_Output_Error with error_msg;
      when Clair.Error.EINTEGRITY =>
        raise Clair.Error.Integrity_Check_Failed with error_msg;
      when Clair.Error.ETXTBSY =>
        raise Clair.Error.Text_File_Busy with error_msg;
      when Clair.Error.EFAULT =>
        raise Clair.Error.Bad_Address with error_msg;
      when Clair.Error.EEXIST =>
        raise Clair.Error.File_Exists with error_msg;
      when Clair.Error.EINVAL =>
        raise Clair.Error.Invalid_Argument with error_msg;
      when Clair.Error.EBADF =>
        raise Clair.Error.Bad_File_Descriptor with error_msg;
      when Clair.Error.ECAPMODE =>
        raise Clair.Error.Not_Permitted_In_Capability_Mode with error_msg;
      when Clair.Error.ENOTCAPABLE =>
        raise Clair.Error.Capabilities_Insufficient with error_msg;
      when Clair.Error.EBUSY =>
        raise Clair.Error.Device_Busy with error_msg;
      when Clair.Error.EFBIG =>
        raise Clair.Error.File_Too_Large with error_msg;
      when Clair.Error.ENFILE =>
        raise Clair.Error.Too_Many_Open_Files_In_System with error_msg;
      when Clair.Error.ENODEV =>
        raise Clair.Error.Operation_Not_Supported_By_Device with error_msg;
      when Clair.Error.ENOMEM =>
        raise Clair.Error.Cannot_Allocate_Memory with error_msg;
      when Clair.Error.EOVERFLOW =>
        raise Clair.Error.
              Value_Too_Large_To_Be_Stored_In_Data_Type with error_msg;
      when others =>
        raise Clair.Error.Unmapped_Error with error_msg;
    end case;
  end handle_open_error;

  function open (path : String;  flags : File.Flags) return Descriptor is
    new_fd      : Descriptor;
    retry_count : Natural := 0;
    MAX_RETRIES : constant Natural := 10;
  begin
    loop
      new_fd := c_open2 (Interfaces.C.to_c (path), Interfaces.C.int (flags));

      if new_fd /= -1 then
        -- Success
        exit;
      end if;

      declare
        errno_code : constant Interfaces.C.int := Clair.Error.get_errno;
      begin
        if errno_code = Clair.Error.EINTR then
          retry_count := retry_count + 1;
          if retry_count > MAX_RETRIES then
            raise Clair.Error.Interrupted_System_Call with
              "Clair.File.open: Aborted after " & MAX_RETRIES'image &
              " consecutive EINTR signals on path """ & path & """";
          end if;
          delay 0.0; -- Yield to prevent busy-waiting
        else
          -- A real, unrecoverable error occurred.
          handle_open_error (errno_code, path);
        end if;
      end;
    end loop;

    return new_fd;
  end open;

  function open (path  : String;
                 flags : File.Flags;
                 mode  : Clair.Types.mode_t) return Descriptor is
    new_fd      : Descriptor;
    retry_count : Natural := 0;
    MAX_RETRIES : constant Natural := 10;
  begin
    loop
      new_fd := c_open3 (Interfaces.C.to_c  (path),
                         Interfaces.C.int   (flags),
                         Clair.Types.mode_t (mode));

      if new_fd /= -1 then
        exit;
      end if;

      declare
        errno_code : constant Interfaces.C.int := Clair.Error.get_errno;
      begin
        if errno_code = Clair.Error.EINTR then
          retry_count := retry_count + 1;
          if retry_count > MAX_RETRIES then
            raise Clair.Error.Interrupted_System_Call with
              "Clair.File.open: Aborted after " & MAX_RETRIES'image &
              " consecutive EINTR signals on path """ & path & """";
          end if;
          delay 0.0;
        else
          handle_open_error (errno_code, path);
        end if;
      end;
    end loop;

    return new_fd;
  end open;

  procedure close (fd : in Descriptor) is
    retval : Interfaces.C.int;
  begin
    -- If the descriptor is already closed, do nothing (ensures idempotency).
    if fd = -1 then
      return;
    end if;

    retval := c_close (Interfaces.C.int (fd));

    if retval = -1 then
      declare
        errno_code : constant Interfaces.C.int := Clair.Error.get_errno;
        error_msg  : constant String :=
          Clair.Error.format_posix_error_message
            (errno_code    => errno_code,
             function_name => "Clair.File.close",
             context_info  => "on fd " & fd'image);
      begin
        case errno_code is
          when Clair.Error.EBADF =>
            raise Clair.Error.Bad_File_Descriptor with error_msg;
          -- NOTE: close(2) should NOT be retried on EINTR because the state of
          -- the file descriptor is left undefined. We report it as an error.
          when Clair.Error.EINTR =>
            raise Clair.Error.Interrupted_System_Call with error_msg;
          when Clair.Error.EIO =>
            raise Clair.Error.Input_Output_Error with error_msg;
          when Clair.Error.ENOSPC =>
            raise Clair.Error.No_Space_Left_On_Device with error_msg;
          when Clair.Error.EDQUOT =>
            raise Clair.Error.Disc_Quota_Exceeded with error_msg;
          when others =>
            raise Clair.Error.Unmapped_Error with error_msg;
        end case;
      end;
    end if;

  end close;

  function read (fd     : in Descriptor;
                 buffer : in out System.Storage_Elements.Storage_Array)
  return Natural is
    bytes_read  : Clair.Types.ssize_t;
    retry_count : Natural := 0;
    MAX_RETRIES : constant Natural := 10;
  begin
    loop
      bytes_read :=
        c_read (Interfaces.C.int (fd), buffer'address, buffer'length);

      if bytes_read >= 0 then
        -- Success (bytes_read > 0) or End-of-File (bytes_read = 0).
        exit;
      end if;
      -- An error occurred (bytes_read = -1), check errno.
      declare
        errno_code : constant Interfaces.C.int := Clair.Error.get_errno;
      begin
        if errno_code = Clair.Error.EINTR then
          -- Interrupted by a signal, prepare to retry.
          retry_count := retry_count + 1;
          if retry_count > MAX_RETRIES then
            raise Clair.Error.Interrupted_System_Call with
              "Clair.File.read: Aborted after " & MAX_RETRIES'image &
              " consecutive EINTR signals on fd " & fd'image;
          end if;
          delay 0.0;
        else
          -- A real, unrecoverable error occurred.
          declare
            error_msg : constant String :=
              Clair.Error.format_posix_error_message
                (errno_code    => errno_code,
                 function_name => "Clair.File.read",
                 context_info  => "on fd " & fd'image);
          begin
            case errno_code is
              when Clair.Error.EBADF =>
                raise Clair.Error.Bad_File_Descriptor with error_msg;
              when Clair.Error.ECONNRESET =>
                raise Clair.Error.Connection_Reset_By_Peer with error_msg;
              when Clair.Error.EFAULT =>
                raise Clair.Error.Bad_Address with error_msg;
              when Clair.Error.EIO =>
                raise Clair.Error.Input_Output_Error with error_msg;
              when Clair.Error.EINTEGRITY =>
                raise Clair.Error.Integrity_Check_Failed with error_msg;
              when Clair.Error.EBUSY =>
                raise Clair.Error.Device_Busy with error_msg;
              when Clair.Error.EINVAL =>
                raise Clair.Error.Invalid_Argument with error_msg;
              when Clair.Error.EAGAIN =>
                raise Clair.Error.
                      Resource_Temporarily_Unavailable with error_msg;
              when Clair.Error.EISDIR =>
                raise Clair.Error.Is_A_Directory with error_msg;
              when Clair.Error.EOPNOTSUPP =>
                raise Clair.Error.Operation_Not_Supported with error_msg;
              when Clair.Error.EOVERFLOW =>
                raise Clair.Error.
                      Value_Too_Large_To_Be_Stored_In_Data_Type with error_msg;
              when others =>
                raise Clair.Error.Unmapped_Error with error_msg;
            end case;
          end;
        end if;
      end;
    end loop;

    return Natural (bytes_read);
  end read;

  function write (fd     : in Descriptor;
                  buffer : in System.Storage_Elements.Storage_Array)
  return Natural is
    bytes_written : Clair.Types.ssize_t;
    retry_count   : Natural := 0;
    MAX_RETRIES   : constant Natural := 10;
  begin
    loop
      bytes_written := c_write (Interfaces.C.int (fd),
                                buffer'address,
                                buffer'length);
      if bytes_written /= -1 then
        -- Success (including writing 0 bytes).
        exit;
      end if;
      -- An error occurred (bytes_written = -1), check errno.
      declare
        errno_code : constant Interfaces.C.int := Clair.Error.get_errno;
      begin
        if errno_code = Clair.Error.EINTR then
          -- Interrupted by a signal, prepare to retry.
          retry_count := retry_count + 1;
          if retry_count > MAX_RETRIES then
            -- Too many consecutive interruptions, give up.
            raise Clair.Error.Interrupted_System_Call with
              "Clair.File.write: Aborted after " & MAX_RETRIES'image &
              " consecutive EINTR signals on fd " & fd'image;
          end if;

          -- Yield CPU to prevent busy-waiting before retrying.
          delay 0.0;

        else
          -- A real, unrecoverable error occurred.
          declare
            error_msg : constant String :=
              Clair.Error.format_posix_error_message
                (errno_code    => errno_code,
                 function_name => "Clair.File.write",
                 context_info  => "on fd " & fd'image);
          begin
            case errno_code is
              when Clair.Error.EBADF =>
                raise Clair.Error.Bad_File_Descriptor with error_msg;
              when Clair.Error.EPIPE =>
                raise Clair.Error.Broken_Pipe with error_msg;
              when Clair.Error.EFBIG =>
                raise Clair.Error.File_Too_Large with error_msg;
              when Clair.Error.EFAULT =>
                raise Clair.Error.Bad_Address with error_msg;
              when Clair.Error.EINVAL =>
                raise Clair.Error.Invalid_Argument with error_msg;
              when Clair.Error.ENOSPC =>
                raise Clair.Error.No_Space_Left_On_Device with error_msg;
              when Clair.Error.EDQUOT =>
                raise Clair.Error.Disc_Quota_Exceeded with error_msg;
              when Clair.Error.EIO =>
                raise Clair.Error.Input_Output_Error with error_msg;
              when Clair.Error.EAGAIN =>
                raise Clair.Error.
                      Resource_Temporarily_Unavailable with error_msg;
              when Clair.Error.EROFS =>
                raise Clair.Error.Read_Only_File_System with error_msg;
              when Clair.Error.EINTEGRITY =>
                raise Clair.Error.Integrity_Check_Failed with error_msg;
              when Clair.Error.EDESTADDRREQ =>
                raise Clair.Error.Destination_Address_Required with error_msg;
              when Clair.Error.EPERM =>
                raise Clair.Error.Operation_Not_Permitted with error_msg;
              when others =>
                raise Clair.Error.Unmapped_Error with error_msg;
            end case;
          end;
        end if;
      end;
    end loop;

    return Natural (bytes_written);
  end write;

  function pipe return Pipe_Ends is
    pipe_fds : C_FD_Array;
    retval   : constant Interfaces.C.int := c_pipe (pipe_fds(0)'access);
  begin
    if retval = -1 then
      declare
        errno_code : constant Interfaces.C.int := Clair.Error.get_errno;
        error_msg  : constant String :=
          Clair.Error.format_posix_error_message
            (errno_code    => errno_code,
             function_name => "Clair.File.pipe",
             context_info  => "call failed");
      begin
        case errno_code is
          when Clair.Error.EMFILE =>
            raise Clair.Error.Too_Many_Open_Files with error_msg;
          when Clair.Error.ENFILE =>
            raise Clair.Error.Too_Many_Open_Files_In_System with error_msg;
          when others =>
            raise Clair.Error.Unmapped_Error with error_msg;
        end case;
      end;
    end if;

    return ret : Pipe_Ends do
      ret.read_end  := Descriptor (pipe_fds (0));
      ret.write_end := Descriptor (pipe_fds (1));
    end return;
  end pipe;

  function duplicate (fd : in Descriptor) return Descriptor is
    new_fd : constant File.Descriptor :=
      File.Descriptor (c_dup (Interfaces.C.int (fd)));
  begin
    if new_fd = -1 then
      declare
        errno_code : constant Interfaces.C.int := Clair.Error.get_errno;
        error_msg  : constant String :=
          Clair.Error.format_posix_error_message
            (errno_code    => errno_code,
             function_name => "Clair.File.duplicate",
             context_info  => "on fd " & fd'image);
      begin
        case errno_code is
          when Clair.Error.EBADF =>
            raise Clair.Error.Bad_File_Descriptor with error_msg;
          when Clair.Error.EMFILE =>
            raise Clair.Error.Too_Many_Open_Files with error_msg;
          when Clair.Error.ENOMEM =>
            raise Clair.Error.Cannot_Allocate_Memory with error_msg;
          when others =>
            raise Clair.Error.Unmapped_Error with error_msg;
        end case;
      end;
    end if;

    return new_fd;
  end duplicate;

function duplicate_to (fd     : in Descriptor;
                       new_fd : Descriptor) return Descriptor is
    result_fd   : Descriptor;
    retry_count : Natural := 0;
    MAX_RETRIES : constant Natural := 10;
  begin
    loop
      result_fd :=
        Descriptor (c_dup2 (Interfaces.C.int (fd), Interfaces.C.int (new_fd)));
      if result_fd /= -1 then
        -- Success
        exit;
      end if;

      -- An error occurred (result_fd = -1), check errno.
      declare
        errno_code : constant Interfaces.C.int := Clair.Error.get_errno;
      begin
        if errno_code = Clair.Error.EINTR then
          -- Interrupted by a signal, prepare to retry.
          retry_count := retry_count + 1;
          if retry_count > MAX_RETRIES then
            raise Clair.Error.Interrupted_System_Call with
              "Clair.File.duplicate_to: Aborted after " & MAX_RETRIES'Image &
              " consecutive EINTR signals for fd " & fd'image & " to " &
              new_fd'image;
          end if;
          delay 0.0; -- Yield to prevent busy-waiting.
        else
          -- A real, unrecoverable error occurred.
          declare
            error_msg : constant String :=
              Clair.Error.format_posix_error_message
                (errno_code    => errno_code,
                 function_name => "Clair.File.duplicate_to",
                 context_info  => "from fd " & fd'image &
                                  " to new_fd " & new_fd'image);
          begin
            case errno_code is
              when Clair.Error.EBADF =>
                raise Clair.Error.Bad_File_Descriptor with error_msg;
              when Clair.Error.EBUSY =>
                raise Clair.Error.Device_Busy with error_msg;
              when Clair.Error.EMFILE =>
                raise Clair.Error.Too_Many_Open_Files with error_msg;
              when Clair.Error.ENOMEM =>
                raise Clair.Error.Cannot_Allocate_Memory with error_msg;
              when others =>
                raise Clair.Error.Unmapped_Error with error_msg;
            end case;
          end;
        end if;
      end;
    end loop;

    return result_fd;
  end duplicate_to;

  function umask (new_mask : Clair.Types.mode_t) return Clair.Types.mode_t is
    function c_umask (mask : Clair.Types.mode_t) return Clair.Types.mode_t;
    pragma import (c, c_umask, "umask");
  begin
    return c_umask (new_mask);
  end umask;

end Clair.File;
