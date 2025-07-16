with Ada.Text_IO;
with POSIX.Process;

procedure Test_SetSID is
    use Ada.Text_IO;
    use POSIX.Process;
begin
    Put_Line ("--- Demonstrating correct usage of Set_SID procedure ---");

    declare
       PID : Process_ID;
    begin
       PID := Fork;

       if PID = 0 then
          -- Child Process: This is where setsid should be called.
          Put_Line ("Child:  Process started. Now calling Set_SID...");
          begin
             Set_SID;
             Put_Line ("Child:  Set_SID successful. Now a session leader.");
             -- In a real daemon, the child would continue its main task here.
          exception
             when Set_SID_Error =>
                Put_Line ("Child:  ERROR! Could not become a session leader.");
          end;
          Put_Line("Child:  Exiting.");

       else
          -- Parent Process
          Put_Line ("Parent: Forked child with PID: " & PID'Image);
          Put_Line ("Parent: My work is done.");
          -- In a real daemon, the parent often exits immediately.
       end if;

    exception
       when Fork_Error =>
          Put_Line ("ERROR: Fork failed.");
    end;
end Test_SetSID;
