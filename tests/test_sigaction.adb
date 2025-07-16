-- test_sigaction.adb
with Ada.Text_IO;
with Interfaces.C;
with POSIX.Signal;
with POSIX.Process;
with System;
with Signal_Handler_Package; -- with the separated package.

procedure Test_Sigaction is
    use Ada.Text_IO;
    use POSIX.Signal;
    use POSIX.Process;
    use Interfaces.C;
    use Signal_Handler_Package;

    -- Since it's a simple record without a discriminant, declare without constraints.
    new_action : Sigaction_Record;
    old_action : Sigaction_Record;

begin
    put_line ("--- Testing sigaction ---");
    put_line ("My PID is: " & Process_ID'Image(getpid));

    -- 1. Set up sigaction record
    sigemptyset (new_action.sa_mask);
    new_action.sa_flags := SA_RESTART;
    -- Directly assign the handler's address using the 'Address attribute
    new_action.sa_handler_address := handle_sigusr1'Address;

    -- 2. Register handler for SIGUSR1
    put_line ("Registering handler for SIGUSR1...");
    sigaction (SIGUSR1, new_action, old_action);
    put_line ("Handler registered.");

    -- 3. Send signal to self
    put_line ("Raising SIGUSR1 in 3 seconds...");
    delay 3.0;
    raise_signal (SIGUSR1);

    -- Allow enough time for the handler to execute.
    delay 0.1;

    -- 4. Check handler invocation result
    if signal_caught then
        put_line ("SUCCESS: Signal handler was correctly invoked.");
    else
        put_line ("FAILURE: Signal handler was not invoked.");
    end if;

    -- 5. Restore original handler
    put_line ("Restoring original signal handler for SIGUSR1.");
    sigaction (SIGUSR1, old_action);
    put_line ("Test finished.");

exception
    when e : Signal_Error =>
        put_line ("ERROR: A signal operation failed.");
end Test_Sigaction;
