-- clair-log.ads
-- Copyright (c) 2020-2025 Hodong Kim <hodong@nimfsoft.art>
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
with GNAT.Source_Info;
with Clair.Platform;

package Clair.Log is
  pragma preelaborate;
  type Options is mod 2 ** Interfaces.C.int'size;
  for Options'size use Interfaces.C.int'size;
  pragma convention (c, Options);

  PID        : constant Options := Clair.Platform.LOG_PID;
  CONSOLE    : constant Options := Clair.Platform.LOG_CONS;
  NO_DELAY   : constant Options := Clair.Platform.LOG_NDELAY;
  STDERR     : constant Options := Clair.Platform.LOG_PERROR;
  DELAY_OPEN : constant Options := Clair.Platform.LOG_ODELAY;
  NO_WAIT    : constant Options := Clair.Platform.LOG_NOWAIT;

  type Levels is (
    Emergency, -- System is unusable
    Alert,     -- Action must be taken immediately
    Critical,  -- Critical conditions
    Error,     -- Error conditions
    Warning,   -- Warning conditions
    Notice,    -- Normal but significant condition
    Info,      -- Informational messages
    Debug      -- Debug-level messages
  );

  for Levels use (
    Emergency => 0,
    Alert     => 1,
    Critical  => 2,
    Error     => 3,
    Warning   => 4,
    Notice    => 5,
    Info      => 6,
    Debug     => 7
  );

  for Levels'size use Interfaces.C.int'size;
  pragma convention (c, Levels);

  type Facilities is (
    Kernel,
    User,
    Mail,
    Daemon,
    Authorization,
    Syslog,
    Printer,
    News,
    UUCP,
    Cron
  );

  for Facilities use (
    Kernel        => 0 * 8,
    User          => 1 * 8,
    Mail          => 2 * 8,
    Daemon        => 3 * 8,
    Authorization => 4 * 8,
    Syslog        => 5 * 8,
    Printer       => 6 * 8,
    News          => 7 * 8,
    UUCP          => 8 * 8,
    Cron          => 9 * 8
  );

  for Facilities'size use Interfaces.C.int'size;

  pragma convention (c, Facilities);

  procedure open (ident    : String;
                  option   : Options;
                  facility : Facilities);

  procedure write (level    : Levels;
                   facility : Facilities;
                   message  : String);

  procedure write (level   : Levels;
                   message : String);

  procedure close;

  procedure critical (
     message : String;
     file    : String  := GNAT.Source_Info.file;
     line    : Integer := GNAT.Source_Info.line;
     func    : String  := GNAT.Source_Info.enclosing_entity
  );

  procedure error (
     message : String;
     file    : String  := GNAT.Source_Info.file;
     line    : Integer := GNAT.Source_Info.line;
     func    : String  := GNAT.Source_Info.enclosing_entity
  );

  procedure warning (
     message : String;
     file    : String  := GNAT.Source_Info.file;
     line    : Integer := GNAT.Source_Info.line;
     func    : String  := GNAT.Source_Info.enclosing_entity
  );

  procedure info (message : String);

  procedure debug (
     message : String;
     file    : String  := GNAT.Source_Info.file;
     line    : Integer := GNAT.Source_Info.line;
     func    : String  := GNAT.Source_Info.enclosing_entity
  );

end Clair.Log;
