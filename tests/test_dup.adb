-- test_dup.adb
with Ada.Text_IO;
with POSIX.File_Control;
with Ada.Exceptions;
with Interfaces.C;

procedure Test_Dup is
    use POSIX.File_Control; -- FIX: Add this 'use' clause back
    use Interfaces.C;

    Log_File_Name : constant String := "/tmp/dup_test.log";
    Log_FD        : File_Descriptor := -1;
    Dup_FD        : File_Descriptor := -1;
    Dummy_Return  : Interfaces.C.size_t;
begin
    Ada.Text_IO.Put_Line ("--- Testing Dup2 and Dup functions ---");

    -- Test 1: Using Dup2 for I/O Redirection
    Ada.Text_IO.Put_Line ("1. Testing Dup2 to redirect stdout...");
    begin
       Log_FD := Open (Log_File_Name, Flags => O_WRONLY + O_CREAT + O_TRUNC, File_Mode => Mode(8#644#));
       Dup2 (Log_FD, STDOUT_FD);

       Dummy_Return := Write (STDOUT_FD, "This message should be in the log file." & ASCII.LF);
       Ada.Text_IO.Put_Line ("Dup2 test successful (Check " & Log_File_Name & ").");

    exception
       when E : File_Control_Error =>
          Ada.Text_IO.Put_Line("ERROR during Dup2 test: " & Ada.Exceptions.Exception_Message(E));
    end;
    Close(Log_FD);


    -- Test 2: Using Dup for creating a shared descriptor
    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put_Line ("2. Testing Dup for shared file offset...");
    begin
       Log_FD := Open (Log_File_Name, Flags => O_WRONLY + O_APPEND, File_Mode => Mode(0));
       Dup_FD := Dup(Log_FD);
       Ada.Text_IO.Put_Line ("  Original FD: " & File_Descriptor'Image(Log_FD));
       Ada.Text_IO.Put_Line ("  Duplicated FD: " & File_Descriptor'Image(Dup_FD));

       Dummy_Return := Write(Log_FD, "Message1;");
       Dummy_Return := Write(Dup_FD, "Message2;");

       Ada.Text_IO.Put_Line("  Wrote to both descriptors. Check the log file.");

    exception
       when E : File_Control_Error =>
          Ada.Text_IO.Put_Line("ERROR during Dup test: " & Ada.Exceptions.Exception_Message(E));
    end;
    Close(Log_FD);

end Test_Dup;
