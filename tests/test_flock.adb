-- test_flock.adb
with Ada.Text_IO;
with Ada.Calendar;
with POSIX.File_Control;
with Interfaces.C;
with Ada.Exceptions; -- Add this missing line

procedure Test_Flock is
    -- The temporary C bindings for open() and close() have been removed.

    use Ada.Text_IO;
    use POSIX.File_Control;
    use Interfaces.C;

    PID_File_Name : constant String := "flock.lock";
    Lock_FD       : File_Descriptor := -1; -- Initialize to an invalid descriptor
begin
    -- This inner block handles the main logic and exceptions
    begin
       Put_Line ("Attempting to open and lock '" & PID_File_Name & "'...");

       -- 1. Open the lock file using our new Ada 'Open' function.
       Lock_FD := Open (Path      => PID_File_Name,
                          Flags     => O_RDWR + O_CREAT,
                          File_Mode => 8#644#);

       -- 2. Try to acquire an exclusive, non-blocking lock.
       Flock (Lock_FD, LOCK_EX + LOCK_NB);
       Put_Line ("Lock acquired successfully. This process is now the 'daemon'.");
       Put_Line ("Waiting for 15 seconds... Try running this program again now.");

       delay 15.0; -- Hold the lock for 15 seconds.

       Put_Line ("... Time is up. Releasing lock and shutting down.");

    exception
       when E : File_Control_Error =>
          Put_Line ("Could not acquire lock or perform file operation.");
          Put_Line ("  -> Details: " & Ada.Exceptions.Exception_Message(E));
          Put_Line ("  -> This is expected if another instance is running.");
       when others =>
          Put_Line ("An unexpected error occurred.");
    end;

    -- 3. Cleanup: This code runs after the inner block.
    if Lock_FD >= 0 then
       -- Use our new Ada 'Close' procedure for cleanup.
       Close(Lock_FD);
       Put_Line("Lock file descriptor closed.");
    end if;

end Test_Flock;
