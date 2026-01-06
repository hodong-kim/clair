-- clair-network.adb
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
with Interfaces.C;
with Clair.Errno;
with Ada.IO_Exceptions;

package body Clair.Network is

  type Sockaddr_Un is record
    sun_family : Clair.Platform.sa_family_t;
    sun_path   : Interfaces.C.char_array (
      0 .. Interfaces.C."-" (Clair.Platform.SOCKADDR_UN_PATH_LENGTH, 1));
  end record;
  for Sockaddr_Un use record
    sun_family at Clair.Platform.SOCKADDR_UN_SUN_FAMILY_OFFSET
               range 0 .. Clair.Platform.sa_family_t'size - 1;
    sun_path   at Clair.Platform.SOCKADDR_UN_SUN_PATH_OFFSET
               range 0 .. (Clair.Platform.SOCKADDR_UN_PATH_LENGTH *
                           System.Storage_Unit) - 1;
  end record;
  for Sockaddr_Un'size use Clair.Platform.SOCKADDR_UN_SIZE *
                           System.Storage_Unit;
  for Sockaddr_Un'alignment use Clair.Platform.SOCKADDR_UN_ALIGNMENT;
  pragma convention (c, Sockaddr_Un);

  function c_socket (domain    : Interfaces.C.int;
                     sock_type : Interfaces.C.int;
                     protocol  : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_socket, "socket");

  function connect_c (sockfd : Clair.IO.Descriptor;
                      addr   : System.Address;
                      addrlen : Clair.Platform.socklen_t)
  return Interfaces.C.int
  with import, convention => c, external_name => "connect";

  function c_send (fd     : Interfaces.C.int;
                   buffer : System.Address;
                   length : Interfaces.C.size_t;
                   flags  : Interfaces.C.int) return Clair.Platform.ssize_t;
  pragma import (c, c_send, "send");

  function c_recv (s      : Interfaces.C.int;
                    buf   : System.Address;
                    len   : Interfaces.C.size_t;
                    flags : Interfaces.C.int) return Clair.Platform.ssize_t;
  pragma import (c, c_recv, "recv");

  function open_socket (
    domain    : Address_Family;
    sock_type : Socket_Kind;
    option    : Socket_Options;
    protocol  : Protocol_Identifier := 0
  ) return Clair.IO.Descriptor is
    use type Interfaces.C.unsigned;
    use type Interfaces.C.int;
    retval : Interfaces.C.int;
  begin
    -- Pass sock_type(enum) and option(flags) combined via bitwise OR
    retval := c_socket (Interfaces.C.int(domain'enum_rep),
                        uint_to_int (
                          Interfaces.C.unsigned(sock_type'enum_rep) or
                          Interfaces.C.unsigned(option)),
                        Interfaces.C.int(protocol));

    if retval = -1 then
      Clair.Errno.raise_from_errno (
        Clair.Errno.get_errno,
        "Failed to create socket"
      );
    end if;

    return Clair.IO.Descriptor(retval);
  end open_socket;

  procedure connect (socket : Clair.IO.Descriptor;
                     path   : String)
  is
    use type Interfaces.C.int;

    addr   : aliased Sockaddr_Un;
    retval : Interfaces.C.int;
    count  : Interfaces.C.size_t;
    errno_code   : Interfaces.C.int := 0;
    MAX_ATTEMPTS : constant := 10;
  begin
    addr := (sun_family => Clair.Platform.AF_UNIX,
             sun_path   => [others => Interfaces.C.nul]);

    -- Prevent Constraint_Error within To_C by checking length beforehand.
    if path'length >= addr.sun_path'length then
      raise Constraint_Error with "Unix socket path too long: " & path;
    end if;

    -- String copy operation (similar to C's strncpy).
    -- Converts the Ada string path to a C-compatible char_array and copies it
    -- to Sun_Path.
    Interfaces.C.to_c (item       => path,
                       target     => addr.sun_path,
                       count      => count,
                       append_nul => True);

    for attempt in 1 .. MAX_ATTEMPTS loop
      retval := connect_c (socket,
                           addr'address,
                           Clair.Platform.SOCKADDR_UN_SIZE);

      -- [Success] A return value of 0 indicates a successful connection; exit
      -- normally.
      if retval = 0 then
        return;
      end if;

      errno_code := Clair.Errno.get_errno;

      case errno_code is
        -- 1. Already connected
        when Clair.Platform.EISCONN =>
          return; -- Treat as success and return.

        -- 2. EINTR (Interrupted by a signal)
        when Clair.Platform.EINTR =>
          -- [Safety] If the last attempt fails, raise an exception.
          if Attempt = MAX_ATTEMPTS then
              Clair.Errno.raise_from_errno (errno_code,
                "Connect failed (Interrupted too many times): " & path);
          end if;
          -- Retry immediately.

        -- 3. In Progress (Non-blocking mode, etc.)
        when Clair.Platform.EINPROGRESS |
             Clair.Platform.EALREADY    |
             Clair.Platform.EAGAIN      =>

          if Attempt = MAX_ATTEMPTS then
            Clair.Errno.raise_from_errno (errno_code, "Connect timeout");
          end if;
          delay 0.01; -- Wait briefly to allow the connection to proceed.

        -- 4. Explicit Failure (Fail Fast)
        when others =>
          Clair.Errno.raise_from_errno (errno_code, "Connect failed: "
                                                    & path);
      end case;
    end loop;

    -- Theoretically, execution should have returned or raised an exception
    -- within the loop. Raise an exception with the last error code as a
    -- safeguard against logic errors.
    Clair.Errno.raise_from_errno (errno_code, "Connect failed (Unknown State): "
                                              & path);
  end connect;

  function send (fd     : Clair.IO.Descriptor;
                 buffer : System.Storage_Elements.Storage_Array;
                 option : Send_Options := MSG_No_Signal)
  return System.Storage_Elements.Storage_Count is
    use type Clair.Platform.ssize_t;
    use type Interfaces.C.int;

    retval     : Clair.Platform.ssize_t;
    error_code : Interfaces.C.int;
  begin
    if buffer'length = 0 then
      return 0;
    end if;

    loop
      retval := c_send (Interfaces.C.int(fd),
                        buffer (buffer'first)'address,
                        Interfaces.C.size_t (buffer'length),
                        Interfaces.C.int(option));

      if retval >= 0 then
        return System.Storage_Elements.Storage_Count(retval);
      end if;

      error_code := Clair.Errno.get_errno;

      -- [Case 1] Retry immediately (EINTR)
      if error_code = Clair.Platform.EINTR then
        null;

      -- [Case 2] Temporarily unavailable -> Delegate to caller (Resource Error)
      -- ENOBUFS: "Transient congestion" (FreeBSD Manual)
      elsif error_code = Clair.Platform.EAGAIN or else
            error_code = Clair.Platform.EWOULDBLOCK or else
            error_code = Clair.Platform.ENOBUFS then

        Clair.Errno.raise_from_errno (error_code,
                                      "Resource temporarily unavailable");
      -- [Case 3] Connection lost -> Raise exception
      elsif error_code = Clair.Platform.EPIPE or else
            error_code = Clair.Platform.ECONNREFUSED then

        Clair.Errno.raise_from_errno (error_code,
                                      "Connection closed or refused");
      -- [Case 4] Fatal error
      else
        Clair.Errno.raise_from_errno (error_code, "Send failed fatal error");
      end if;
    end loop;
  end send;

  procedure send_all (fd     : Clair.IO.Descriptor;
                      buffer : System.Storage_Elements.Storage_Array;
                      option : Send_Options := MSG_No_Signal)
  is
    use type System.Storage_Elements.Storage_Offset;
    chunk_sent : System.Storage_Elements.Storage_Count;
    cursor     : System.Storage_Elements.Storage_Offset := buffer'first;
    remaining  : System.Storage_Elements.Storage_Offset := buffer'length;
  begin
    while remaining > 0 loop
      -- Call Send for the remaining part (cursor .. buffer'last)
      chunk_sent := send (fd, buffer(cursor .. buffer'last), option);
      -- If Send returns 0 (Connection closed, etc.)
      if chunk_sent = 0 then
        Clair.Errno.raise_from_errno (Clair.Platform.EPIPE,
                                      "Connection closed during send");
      end if;
      -- Advance pointer
      cursor    := cursor + System.Storage_Elements.Storage_Offset(chunk_sent);
      remaining := remaining -
                   System.Storage_Elements.Storage_Offset(chunk_sent);
    end loop;
  end send_all;

  procedure recv_all (socket : in Clair.IO.Descriptor;
                      buffer : out System.Storage_Elements.Storage_Array)
  is
    use type Interfaces.C.int;
    use type Clair.Platform.ssize_t;
    use type System.Storage_Elements.Storage_Offset;

    left   : System.Storage_Elements.Storage_Offset := buffer'length;
    offset : System.Storage_Elements.Storage_Offset := 0;
    retval : Clair.Platform.ssize_t;
  begin
    while left > 0 loop
      -- flags=0: In blocking mode, this blocks until at least one byte is
      -- available.
      retval := c_recv (Interfaces.C.int(socket),
                        buffer (buffer'first + offset)'address,
                        Interfaces.C.size_t (left),
                        0);

      if retval > 0 then
        -- [Success] Decrement the remaining count and advance the buffer
        -- offset.
        left   := left   - System.Storage_Elements.Storage_Offset(retval);
        offset := offset + System.Storage_Elements.Storage_Offset(retval);

      elsif retval = 0 then
        -- [Error] Connection closed (EOF) while data is still expected
        -- (left > 0).
        -- It is recommended to raise 'End_Error' to indicate a premature end of
        -- stream.
        raise Ada.IO_Exceptions.End_Error
              with "Connection closed by peer prematurely";

      else
        -- [Error] System call failed (retval < 0).
        declare
          errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
        begin
          -- [Mandatory] EINTR: Interrupted by a signal; ignore and retry.
          if errno_code /= Clair.Platform.EINTR then
            -- EAGAIN/EWOULDBLOCK: Should not occur on a blocking socket
            -- without a timeout.
            -- Therefore, treat all errors other than EINTR as fatal.
            Clair.Errno.raise_from_errno (errno_code, "recv_all failed");
          end if;
        end;
      end if;
    end loop;
  end recv_all;

end Clair.Network;
