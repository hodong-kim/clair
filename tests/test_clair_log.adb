-- test_clair_log.adb
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
--
-- Description:
--   Test program to verify the functionality of the Clair.Log package.
--   Results recorded in syslog must be verified in system log files
--   (e.g., /var/log/syslog, /var/log/messages).

with Ada.Text_IO; use Ada.Text_IO;
with Clair.Log;

procedure Test_Clair_Log is

  -- Auxiliary procedure for testing.
  -- Intended to verify that the call stack (Function Name) is correctly
  -- recorded in the log.
  procedure Log_From_Subprogram is
  begin
    Put_Line ("  [Subprogram] Writing 'Notice' level log...");
    Clair.Log.write (Clair.Log.Notice,
                     "Log entry called from within a subprogram.");
  end Log_From_Subprogram;

begin
  Put_Line ("=== Starting Clair.Log Test ===");

  ----------------------------------------------------------------------------
  -- 1. Initialize Log System (Open)
  ----------------------------------------------------------------------------
  Put_Line ("1. Opening log system (Ident: Test_App)");
  Clair.Log.open ("Test_App", Clair.Log.PID, Clair.Log.User);

  ----------------------------------------------------------------------------
  -- 2. Log Recording at Various Levels (Write)
  ----------------------------------------------------------------------------
  Put_Line ("2. Sending log messages");

  -- (1) Info Level
  Clair.Log.write (Clair.Log.Info, "Application started.");
  Put_Line ("  - Info log sent.");

  -- (2) Debug Level
  Clair.Log.write (Clair.Log.Debug, "Debug variable value: X = 42");
  Put_Line ("  - Debug log sent.");

  -- (3) Warning Level
  Clair.Log.write (Clair.Log.Warning, "Disk space may be low.");
  Put_Line ("  - Warning log sent.");

  -- (4) Error Level
  Clair.Log.write (Clair.Log.Error, "Unable to open file.");
  Put_Line ("  - Error log sent.");

  ----------------------------------------------------------------------------
  -- 3. Source Info Automatic Capture Test
  ----------------------------------------------------------------------------
  Put_Line ("3. Source Info capture test");
  -- Call from main procedure
  Clair.Log.write (Clair.Log.Info, "Log from Main procedure");

  -- Call from sub-procedure (Function name in log should change to
  -- Log_From_Subprogram)
  Log_From_Subprogram;

  ----------------------------------------------------------------------------
  -- 4. Reset Log System (Re-open)
  ----------------------------------------------------------------------------
  -- Verify if State_Manager correctly frees existing memory and allocates a new
  -- Ident.
  Put_Line ("4. Changing log identifier (Test_App -> New_Identity)");
  Clair.Log.open ("New_Identity", Clair.Log.Console, Clair.Log.Syslog);
  Clair.Log.write (Clair.Log.Alert,
                   "Critical alert log after identifier change.");

  Clair.Log.critical ("This is critical");
  Clair.Log.warning  ("This is warning");
  Clair.Log.debug    ("This is debug");

  ----------------------------------------------------------------------------
  -- 5. Terminate Log System (Close)
  ----------------------------------------------------------------------------
  Put_Line ("5. Closing log system");
  Clair.Log.close;

  Put_Line ("=== Test Finished ===");
  Put_Line ("Note: Check actual log content in '/var/log/debug.log' " &
            "or '/var/log/messages.log'.");
  Put_Line ("Example command: tail -f /var/log/messages");

end Test_Clair_Log;
