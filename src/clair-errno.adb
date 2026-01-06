-- clair-errno.adb
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
with Interfaces.C.Strings;
with Clair.Exceptions;

package body Clair.Errno is
  use type Interfaces.C.size_t;
  use type Interfaces.C.int;

  function strerror_r (errno_code : in  Interfaces.C.int;
                       buffer     : out Interfaces.C.char_array;
                       buf_len    : in  Interfaces.C.size_t)
  return Interfaces.C.int;
  pragma import (c, strerror_r, "clair_errno_strerror_r");

  function get_error_message (errno_code : Interfaces.C.int) return String is
    BUFFER_SIZE : constant Interfaces.C.size_t := 256;
    buffer      : Interfaces.C.char_array (0 .. BUFFER_SIZE - 1);
    retval      : constant Interfaces.C.int :=
                    strerror_r (errno_code, buffer, BUFFER_SIZE);
  begin
    if retval = 0 then
      -- Success: return the content of the buffer.
      return Interfaces.C.Strings.value (
             sys_addr_to_chars_ptr (buffer'address));
    else
      -- Handle strerror_r's own failure without recursion.
      case retval is
        when Clair.Platform.ERANGE =>
          raise Clair.Exceptions.Result_Too_Large with
            "Clair.Errno.get_error_message: Buffer (size " &
            BUFFER_SIZE'image & ") is too small for errno " & errno_code'image;
        when Clair.Platform.EINVAL =>
          raise Clair.Exceptions.Invalid_Argument with
            "Clair.Errno.get_error_message: Invalid argument (errno code " &
            errno_code'image & ") passed to strerror_r.";
        when others =>
          -- Per POSIX, strerror_r should only return 0, EINVAL, or ERANGE.
          -- Any other value is a violation of its contract.
          raise Program_Error with
            "Clair.Errno.get_error_message: Unexpected return value " &
            retval'image & " from underlying strerror_r call.";
      end case;
    end if;
  end get_error_message;

  function format_posix_error_message (errno_code    : in Interfaces.C.int;
                                       function_name : in String;
                                       context_info  : in String)
  return String is
    errno_text : constant String := get_error_message (errno_code);
    -- Standard Format:
    -- "<function name>: <Contextual info>; <errno message> (errno <code>)"
    -- Example: "Clair.File.duplicate: on fd 3; Bad file descriptor (errno 9)"
    full_message : constant String :=
      function_name & ": " & context_info & "; " & errno_text &
      " (errno " & errno_code'image & ")";
  begin
    return full_message;
  end format_posix_error_message;

  procedure raise_from_errno (errno        : Interfaces.C.int;
                              context_info : String) is separate;

end Clair.Errno;
