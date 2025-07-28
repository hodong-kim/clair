-- test_clair_signal.adb
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

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with System;
with Clair.Signal;
with Clair.Process;
with Test_Clair_Signal;
with Interfaces.C;
with Clair.File;
with System.Storage_Elements;
with Clair.Types;

package body Test_Clair_Signal is
  use type Interfaces.C.size_t;
  procedure custom_sigint_handler (signo   : in Interfaces.C.int;
                                   siginfo : access Clair.Signal.Info;
                                   context : System.Address);
  pragma convention (c, custom_sigint_handler);

  procedure c_exit (status : Interfaces.C.int) with
    convention => c, import => true, external_name => "_exit";

  procedure safe_int_to_string (num    : in  Interfaces.C.int;
                                buffer : out Interfaces.C.Char_Array;
                                last   : out Natural)
  is
     temp_buffer : array (1 .. 12) of Character;
     temp_last   : Natural := 0;
     n           : Natural := Natural (num);
  begin
    if n = 0 then
      temp_last := 1;
      temp_buffer (1) := '0';
    else
      while n > 0 loop
        temp_last := temp_last + 1;
        temp_buffer (temp_last) :=
          Character'val (Character'pos ('0') + (n mod 10));
        n := n / 10;
      end loop;
    end if;
    last := temp_last;
    for I in 1 .. last loop
      buffer (buffer'first + Interfaces.C.Size_t (i - 1)) :=
        Interfaces.C.to_c (temp_buffer (temp_last - i + 1));
    end loop;
  end safe_int_to_string;

  -- Pre-define all constant strings for use in the handler.
  DASHES      : constant String := "---------------------------------" & ASCII.LF;
  PREFIX_SIG  : constant String := "Caught signal: ";
  PREFIX_CODE : constant String := "Signal Code (si_code): ";
  PREFIX_PID  : constant String := "Sending Process ID (si_pid): ";
  SUFFIX      : constant String := "Shutting down gracefully..." & ASCII.LF;
  NEWLINE     : constant String := "" & ASCII.LF;

  DASHES_C      : constant Interfaces.C.Char_Array := Interfaces.C.to_c (DASHES);
  PREFIX_SIG_C  : constant Interfaces.C.Char_Array := Interfaces.C.to_c (PREFIX_SIG);
  PREFIX_CODE_C : constant Interfaces.C.Char_Array := Interfaces.C.to_c (PREFIX_CODE);
  PREFIX_PID_C  : constant Interfaces.C.Char_Array := Interfaces.C.to_c (PREFIX_PID);
  SUFFIX_C      : constant Interfaces.C.Char_Array := Interfaces.C.to_c (SUFFIX);
  NEWLINE_C     : constant Interfaces.C.Char_Array := Interfaces.C.to_c (NEWLINE);

  -- Message to be printed if an exception occurs within the handler.
  FATAL_ERROR_MSG : constant String :=
    "Fatal: Error in signal handler. Terminating immediately." & ASCII.LF;
  FATAL_ERROR_MSG_C : constant Interfaces.C.Char_Array :=
    Interfaces.C.to_c (FATAL_ERROR_MSG);

  procedure custom_sigint_handler (signo   : in Interfaces.C.int;
                                   siginfo : access Clair.Signal.Info;
                                   context : System.Address) is
     pragma Unreferenced (context);
     bytes_written : Clair.Types.ssize_t;
     pragma Unreferenced (bytes_written);

     num_buffer : Interfaces.C.Char_Array (0 .. 11) := (others => Interfaces.C.Nul);
     num_Len    : Natural;

     -- Local procedure for writing safely to standard error (fd = 2).
     procedure safe_write (msg : Interfaces.C.Char_Array; len : Natural) is
     begin
        if len > 0 then
           bytes_written := Clair.File.c_write (Clair.File.STDERR_FILENO,
                                                msg, msg'length);
        end if;
     end safe_write;

  begin
    safe_write (DASHES_C, DASHES'length);

    safe_write (PREFIX_SIG_C, PREFIX_SIG'length);
    safe_int_to_string (signo, num_buffer, num_Len);
    safe_write (num_buffer, num_Len);
    safe_write (NEWLINE_C, NEWLINE'length);

    if siginfo /= null then
      safe_write (PREFIX_CODE_C, PREFIX_CODE'length);
      safe_int_to_string (siginfo.all.si_code,
                          num_buffer, num_Len);
      safe_write (num_buffer, num_Len);
      safe_write (NEWLINE_C, NEWLINE'length);

      safe_write (PREFIX_PID_C, PREFIX_PID'length);
      safe_int_to_string (siginfo.all.si_pid,
                          num_buffer, num_Len);
      safe_write (num_buffer, num_Len);
      safe_write (NEWLINE_C, NEWLINE'length);
    end if;

     safe_write (SUFFIX_C, SUFFIX'length);
     safe_write (DASHES_C, DASHES'length);

     c_exit (Clair.Process.EXIT_SUCCESS);
  exception
    -- Prevent any exception from propagating out of the handler.
    when others =>
      -- Print a safe error message and terminate immediately.
      bytes_written := Clair.File.c_write (2, FATAL_ERROR_MSG_C,
                                           FATAL_ERROR_MSG_C'length);
      c_exit (127);
  end custom_sigint_handler;

  procedure run is
    handler_ptr : constant Clair.Signal.Handler3_Access :=
                           custom_sigint_handler'access;
    new_act     : aliased Clair.Signal.Action;
    old_act     : aliased Clair.Signal.Action;
    signal_mask : aliased Clair.Signal.Set;
  begin
    Ada.Text_IO.put_line ("Setting up the SIGINT handler...");
    Clair.Signal.empty_set (signal_mask);
    new_act.sa_mask  := Clair.Signal.Set (signal_mask);
    new_act.sa_flags := Clair.Signal.SA_SIGINFO;
    new_act.uu_sigaction_u.uu_sa_sigaction := handler_ptr;

    Clair.Signal.set_action (signo      => Clair.Signal.SIGINT,
                             new_action => new_act,
                             old_action => old_act);

    Ada.Text_IO.put_line ("Handler active. Press Ctrl+C. Waiting indefinitely");

    delay 10.0;

  exception
    when e : others =>
      Ada.Text_IO.put_line ("Error: " & Ada.Exceptions.exception_message (e));
      Clair.Process.exit_process (Clair.Process.EXIT_FAILURE);
  end run;

end Test_Clair_Signal;
