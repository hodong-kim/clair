-- clair-network.ads
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
with Clair.IO;
with Clair.Platform;
with System.Storage_Elements;

package Clair.Network is

  type Socket_Options      is new Interfaces.C.unsigned;
  type Send_Options        is new Interfaces.C.unsigned;
  type Protocol_Identifier is new Interfaces.C.int;

  type Address_Family is (Unix);
  for Address_Family use (
    Unix => Clair.Platform.AF_UNIX
  );

  CLOSE_ON_EXEC : constant Socket_Options := Clair.Platform.SOCK_CLOEXEC;
  CLOSE_ON_FORK : constant Socket_Options := Clair.Platform.SOCK_CLOFORK;
  NON_BLOCKING  : constant Socket_Options := Clair.Platform.SOCK_NONBLOCK;

  -- Flags for send(2)
  MSG_Out_Of_Band    : constant Send_Options := Clair.Platform.MSG_OOB;
  MSG_Bypass_Routing : constant Send_Options := Clair.Platform.MSG_DONTROUTE;
  MSG_End_Of_Record  : constant Send_Options := Clair.Platform.MSG_EOR;
  MSG_Non_Blocking   : constant Send_Options := Clair.Platform.MSG_DONTWAIT;
  MSG_End_Of_File    : constant Send_Options := Clair.Platform.MSG_EOF;
  MSG_No_Signal      : constant Send_Options := Clair.Platform.MSG_NOSIGNAL;

  type Socket_Kind is (Stream);
  for Socket_Kind use (
    Stream => Clair.Platform.SOCK_STREAM
  );

  function open_socket (
    domain    : Address_Family;
    sock_type : Socket_Kind;
    option    : Socket_Options;
    protocol  : Protocol_Identifier := 0
  ) return Clair.IO.Descriptor;

  procedure connect (
    socket : Clair.IO.Descriptor;
    path   : String
  );

  function send (fd     : Clair.IO.Descriptor;
                 buffer : System.Storage_Elements.Storage_Array;
                 option : Send_Options := MSG_No_Signal)
  return System.Storage_Elements.Storage_Count;

  procedure send_all (fd     : Clair.IO.Descriptor;
                      buffer : System.Storage_Elements.Storage_Array;
                      option : Send_Options := MSG_No_Signal);

  procedure recv_all (socket : in Clair.IO.Descriptor;
                      buffer : out System.Storage_Elements.Storage_Array);
end Clair.Network;
