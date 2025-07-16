with Interfaces.C;

package Signal_Handler_Package is
    -- Declare as volatile to prevent issues due to optimization.
    signal_caught : Boolean := False;
    pragma Volatile (signal_caught);

    procedure handle_sigusr1 (sig : in Interfaces.C.int);
    pragma Convention (C, handle_sigusr1);
end Signal_Handler_Package;
