-- clair-io.ads
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
with System.Storage_Elements;
with Clair.Platform;

package Clair.IO is

  type Descriptor is new Interfaces.C.int;

  STDIN_FILENO  : constant := Clair.Platform.STDIN_FILENO;
  STDOUT_FILENO : constant := Clair.Platform.STDOUT_FILENO;
  STDERR_FILENO : constant := Clair.Platform.STDERR_FILENO;

  INVALID_DESCRIPTOR : constant Descriptor := -1;

  procedure close (fd : in Descriptor);

  function read (fd     : in Descriptor;
                 buffer : in out System.Storage_Elements.Storage_Array)
  return Natural;

  function write (fd     : in Descriptor;
                  buffer : in System.Storage_Elements.Storage_Array)
  return Natural;

  function duplicate    (fd : in Descriptor) return Descriptor;
  function duplicate_to (fd     : in Descriptor;
                         new_fd : in Descriptor) return Descriptor;

  procedure ensure_standard_descriptors;

  type Poll_Event is new Interfaces.C.short;

  POLLIN   : constant Poll_Event := Poll_Event(Clair.Platform.POLLIN);
  POLLOUT  : constant Poll_Event := Poll_Event(Clair.Platform.POLLOUT);
  POLLERR  : constant Poll_Event := Poll_Event(Clair.Platform.POLLERR);
  POLLHUP  : constant Poll_Event := Poll_Event(Clair.Platform.POLLHUP);
  POLLNVAL : constant Poll_Event := Poll_Event(Clair.Platform.POLLNVAL);

  type Poll_FD is record
    fd      : Descriptor;
    events  : Poll_Event;
    revents : Poll_Event;
  end record;
  pragma convention (c, Poll_FD);

  for Poll_FD use record
    fd      at Clair.Platform.POLLFD_FD_OFFSET
            range 0 .. Descriptor'size - 1;
    events  at Clair.Platform.POLLFD_EVENTS_OFFSET
            range 0 .. Poll_Event'size - 1;
    revents at Clair.Platform.POLLFD_REVENTS_OFFSET
            range 0 .. Poll_Event'size - 1;
  end record;

  for Poll_FD'size use Clair.Platform.POLLFD_SIZE * System.Storage_Unit;
  for Poll_FD'alignment use Clair.Platform.POLLFD_ALIGNMENT;

  type Poll_FD_Array is array (Positive range <>) of aliased Poll_FD;
  pragma convention (c, Poll_FD_Array);

  function "and" (left, right : Clair.IO.Poll_Event) return Clair.IO.Poll_Event;
  function "or"  (left, right : Clair.IO.Poll_Event) return Clair.IO.Poll_Event;

  function poll (fds     : in out Poll_FD_Array;
                 timeout : Integer := -1) return Natural;

end Clair.IO;
