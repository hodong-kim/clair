-- test_open_close.adb
with Ada.Text_IO;
with POSIX.File_Control;
with Ada.Exceptions;
with Interfaces.C; -- Add this
use Interfaces.C;   -- And this

procedure Test_Open_Close is
    use Ada.Text_IO;
    use POSIX.File_Control;

    Test_File_Name : constant String := "/tmp/cada_open_close_test.txt";
    Test_FD        : File_Descriptor;
begin
    Put_Line ("--- Testing Open and Close functions together ---");

    begin
       -- 1. Create and open a new file.
       Put_Line ("Attempting to create: " & Test_File_Name);
       Test_FD := Open (Test_File_Name, Flags => O_WRONLY + O_CREAT, File_Mode => 8#644#);
       Put_Line ("Success! File opened with descriptor: " & File_Descriptor'Image(Test_FD));

       -- 2. Close the file descriptor.
       Put_Line ("Now, closing the file descriptor...");
       Close (Test_FD);
       Put_Line ("Success! File closed.");

    exception
       when E : File_Control_Error => -- Corrected exception handling
          Put_Line ("ERROR: A file operation failed: " & Ada.Exceptions.Exception_Message(E));
    end;

end Test_Open_Close;
