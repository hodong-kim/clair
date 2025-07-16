-- signal_handler_package.adb
package body Signal_Handler_Package is
    procedure handle_sigusr1 (sig : in Interfaces.C.int) is
        pragma Unreferenced (sig); -- Prevent warning since parameter is not used
    begin
        -- !!! Important: Only perform safe operations (e.g., setting flags) inside the handler.
        signal_caught := True;
    end handle_sigusr1;
end Signal_Handler_Package;
