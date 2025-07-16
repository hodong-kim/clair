-- test_fork.adb
with Ada.Text_IO;
with POSIX.Process;

procedure Test_Fork is
    use Ada.Text_IO;
    use POSIX.Process;

    PID : Process_ID;
begin
    Put_Line ("-- Forking Tests Program Start --");
    New_Line;

    ----------------------------------
    -- Test 1: Standard fork()
    ----------------------------------
    Put_Line ("1. Testing standard Fork...");
    PID := Fork;

    if PID = 0 then
       -- This is the child from fork(). It prints and exits.
       Put_Line ("    -> Fork Child:  Hello! My work is done.");
       return;
    else
       -- This is the parent process.
       Put_Line ("    -> Fork Parent: Created child with PID: " & Process_ID'Image (PID));
    end if;

    -- Only the original parent process reaches this point.
    New_Line;
    Put_Line ("-----------------------------------");
    New_Line;

    ----------------------------------
    -- Test 2: _Fork() (async-signal-safe)
    ----------------------------------
    Put_Line ("2. Testing Underscore_Fork...");
    PID := Underscore_Fork;

    if PID = 0 then
       -- This is the child from _Fork(). It prints and exits.
       Put_Line ("    -> _Fork Child: Hello from the async-safe child! Done.");
       return;
    else
       -- This is the parent process.
       Put_Line ("    -> _Fork Parent: Created async-safe child with PID: " & Process_ID'Image (PID));
    end if;

    New_Line;
    Put_Line ("-- All tests finished successfully in parent process --");

exception
    when Fork_Error =>
       Put_Line ("Error: A fork operation failed.");

end Test_Fork;
