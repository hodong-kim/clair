-- clair-log.adb
-- Copyright (c) 2020-2026 Hodong Kim <hodong@nimfsoft.art>
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
with Interfaces.C.Strings;
with Ada.Strings.Fixed;
with Clair.Errno;

package body Clair.Log is
  use type Interfaces.C.Strings.chars_ptr;
  use type Interfaces.C.int;

  procedure openlog (ident    : Interfaces.C.Strings.chars_ptr;
                     option   : Interfaces.C.int;
                     facility : Interfaces.C.int)
  with import, convention => c, external_name => "openlog";

  -- syslog receives variadic arguments in C, but is bound with a fixed
  -- signature in Ada. For security, it is defined to accept two arguments: a
  -- format string ("%s") and the actual message string to prevent format string
  -- vulnerabilities.
  procedure syslog (priority : Interfaces.C.int;
                    format   : Interfaces.C.char_array;
                    message  : Interfaces.C.char_array)
  with import, convention => c, external_name => "syslog";

  procedure closelog
  with import, convention => c, external_name => "closelog";

  protected State_Manager is
    procedure safe_open (ident    : String;
                         option   : Options;
                         facility : Facilities);
    procedure safe_close;
  private

    current_ident :  Interfaces.C.Strings.chars_ptr
                  := Interfaces.C.Strings.NULL_PTR;
  end State_Manager;

  protected body State_Manager is

    procedure safe_open (ident    : String;
                         option   : Options;
                         facility : Facilities) is
    begin
      if current_ident /= Interfaces.C.Strings.NULL_PTR then
        Interfaces.C.Strings.free (current_ident);
        current_ident := Interfaces.C.Strings.NULL_PTR;
      end if;

      current_ident := Interfaces.C.Strings.new_string (ident);

      openlog (current_ident,
               Interfaces.C.int(option),
               Facilities'enum_rep(facility));
    end safe_open;

    procedure safe_close is
    begin
      closelog;

      if current_ident /= Interfaces.C.Strings.NULL_PTR then
        Interfaces.C.Strings.free (current_ident);
        current_ident := Interfaces.C.Strings.NULL_PTR;
      end if;
    end safe_close;

  end State_Manager;

  procedure open (ident    : String;
                  option   : Options;
                  facility : Facilities) is
  begin
    State_Manager.safe_open (ident, option, facility);
  end open;

  procedure close is
  begin
    State_Manager.safe_close;
  end close;

  procedure write (level   : Levels;
                   message : String)
  is
    saved_errno : constant Interfaces.C.int := Clair.Errno.get_errno;
  begin
    syslog (Levels'enum_rep(level),
            Interfaces.C.to_c ("%s"),
            Interfaces.C.to_c (message));

    Clair.Errno.set_errno (saved_errno);
  end write;

  procedure write (level    : Levels;
                   facility : Facilities;
                   message  : String)
  is
    saved_errno : constant Interfaces.C.int := Clair.Errno.get_errno;
  begin
    syslog (Levels'enum_rep(level) + Facilities'enum_rep(facility),
            Interfaces.C.to_c ("%s"),
            Interfaces.C.to_c (message));

    Clair.Errno.set_errno (saved_errno);
  end write;

  procedure write_internal (level   : Levels;
                            message : String;
                            file    : String;
                            line    : Integer;
                            func    : String)
  is
    prefix : constant String := (case level is
      when Log.Emergency => "EMERGENCY",
      when Log.Alert     => "ALERT",
      when Log.Critical  => "CRITICAL",
      when Log.Error     => "ERROR",
      when Log.Warning   => "WARNING",
      when Log.Notice    => "NOTICE",
      when Log.Info      => "INFO",
      when Log.Debug     => "DEBUG"
    );

    -- Example: "[ERROR] file.adb:10:my_func: Message content"
    line_str : constant String
             := Ada.Strings.Fixed.trim (line'image, Ada.Strings.Left);

    full_msg : constant String :=
      "[" & prefix & "] " & file & ":" & line_str & ":" & func & ": " & message;
  begin
    write (level, full_msg);
  end write_internal;

  procedure critical (message : String;
                      file    : String  := GNAT.Source_Info.file;
                      line    : Integer := GNAT.Source_Info.line;
                      func    : String  := GNAT.Source_Info.enclosing_entity) is
  begin
    write_internal (Log.Critical, message, file, line, func);
  end critical;

  procedure error (message : String;
                   file    : String  := GNAT.Source_Info.file;
                   line    : Integer := GNAT.Source_Info.line;
                   func    : String  := GNAT.Source_Info.enclosing_entity) is
  begin
    write_internal (Log.Error, message, file, line, func);
  end error;

  procedure warning (message : String;
                     file    : String  := GNAT.Source_Info.file;
                     line    : Integer := GNAT.Source_Info.line;
                     func    : String  := GNAT.Source_Info.enclosing_entity) is
  begin
    write_internal (Log.Warning, message, file, line, func);
  end warning;

  procedure info (message : String) is
  begin
    write (Log.Info, message);
  end info;

  procedure debug (message : String;
                   file    : String  := GNAT.Source_Info.file;
                   line    : Integer := GNAT.Source_Info.line;
                   func    : String  := GNAT.Source_Info.enclosing_entity) is
  begin
    write_internal (Log.Debug, message, file, line, func);
  end debug;

end Clair.Log;
