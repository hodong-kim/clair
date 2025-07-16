-- test_fcntl.adb
with Ada.Text_IO;
with Ada.Calendar;
with POSIX.File_Control;
with Ada.Exceptions;
with Interfaces.C; -- Add this missing line
use Interfaces.C;   -- And this one

procedure Test_Fcntl is
    use Ada.Text_IO;
    use POSIX.File_Control;

    Lock_File_Name : constant String := "fcntl.lock";
    Lock_FD        : File_Descriptor := -1;
    Lock_Record    : Flock_Record;
begin
    -- This inner block isolates the main logic from the cleanup.
    begin
       -- 1. Open the lock file
       Lock_FD := Open (Lock_File_Name, Flags => O_WRONLY + O_CREAT, File_Mode => 8#644#);
       Put_Line ("Lock file opened. FD: " & File_Descriptor'Image(Lock_FD));

       -- 2. Prepare the lock record to lock the entire file
       Lock_Record.L_Type   := F_WRLCK;      -- Exclusive write lock
       Lock_Record.L_Whence := SEEK_SET;     -- From the beginning of the file
       Lock_Record.L_Start  := 0;
       Lock_Record.L_Len    := 0;            -- 0 means lock until EOF

       -- 3. Attempt to set the lock
       Put_Line ("Attempting to acquire fcntl lock...");
       Set_Lock (Lock_FD, Lock_Record);
       Put_Line ("Lock acquired successfully!");
       Put_Line ("Waiting 15 seconds... Run this test again in another terminal to see it fail.");

       delay 15.0;

       Put_Line ("... Time is up. Releasing lock.");

    exception
       when E : File_Control_Error =>
          Put_Line ("ERROR: " & Ada.Exceptions.Exception_Message(E));
          Put_Line ("(This is expected if another instance is already running)");
    end; -- End of the inner logic block.

    -- 4. Cleanup: This code runs after the inner block completes,
    --    whether an exception occurred or not.
    if Lock_FD >= 0 then
       begin
          Lock_Record.L_Type := F_UNLCK; -- Set lock type to Unlock
          Set_Lock(Lock_FD, Lock_Record);
          Close(Lock_FD);
          Put_Line ("Lock released and file closed.");
       exception
          when File_Control_Error =>
             -- Cleanup itself might fail, but we don't want the program to crash.
             Put_Line("Error during cleanup, but shutting down anyway.");
       end;
    end if;

end Test_Fcntl;
