-- -*- Mode: Ada; indent-tabs-mode: nil; c-basic-offset: 2; tab-width: 2 -*-
with Ada.Text_IO;
with System;
with Interfaces.C;
with Dl;
with Ada.Exceptions;
with Ada.Unchecked_Conversion;

procedure Test_Dl is
    use Ada.Text_IO;
    use System;
    use Ada.Exceptions;

    handle : System.Address  := System.Null_Address;
    path   : constant String := "./libexample.so";

    -- === Function pointer declarations ===
    func_addr : System.Address;
    type my_function_access is access function (Arg : Interfaces.C.int) return Interfaces.C.int;
    pragma Convention (C, my_function_access);
    function to_my_function_access is new Ada.Unchecked_Conversion (
       Source => System.Address, Target => my_function_access);
    my_func_pointer : my_function_access := null;
    result : Interfaces.C.int;

    -- === Global variable pointer declarations ===
    var_addr  : System.Address;

    -- 1. Define access type for the global variable's type (assuming int)
    type int_access is access all Interfaces.C.int;
    -- pragma Convention (C, int_access); -- Usually unnecessary for simple type access

    -- 2. Create a function instance to convert System.Address to Int_Access
    function to_int_access is new Ada.Unchecked_Conversion (
       Source => System.Address,
       Target => int_access
    );

    -- 3. Variable to store the converted variable pointer
    global_var_pointer : int_access := null;

    -- 4. Variable to store the global variable's value
    global_value : Interfaces.C.int;

begin
    put_line ("Loading " & path);
    handle := dl.open (path, dl.rtld_lazy);
    put_line ("Success: loaded " & path);

    -- === Get function symbol ===
    put_line ("Looking up symbol 'my_function'");
    func_addr := dl.get_symbol (handle, "my_function");
    put_line ("Success: got address for 'my_function'");
    if func_addr /= System.Null_Address then
       my_func_pointer := to_my_function_access (func_addr);
    else
       put_line("Error: Symbol 'my_function' resolved to a Null Address!");
       raise Program_Error with "Function Symbol resolved to Null Address";
    end if;

    -- === Get global variable symbol ===
    put_line ("Looking up symbol 'my_global_variable'");
    var_addr := dl.get_symbol (handle, "my_global_variable");
    put_line ("Success: got address for 'my_global_variable'");
    if var_addr /= System.Null_Address then
        global_var_pointer := to_int_access(var_addr);
    else
        put_line("Error: Symbol 'my_global_variable' resolved to a Null Address!");
        raise Program_Error with "Variable Symbol resolved to Null Address";
    end if;

    -- === Access global variable (read) ===
    if global_var_pointer /= null then
        global_value := global_var_pointer.all;
        put_line ("Initial value of my_global_variable: " & Interfaces.C.int'Image (global_value));
    else
        put_line("Error: Cannot read global variable, pointer is null.");
    end if;

    -- === Call function (may modify global variable) ===
    if my_func_pointer /= null then
       put_line ("Calling my_function(10)...");
       result := my_func_pointer (10);
       put_line ("Result from my_function(10): " & Interfaces.C.int'Image (result));
    else
        put_line ("Error: Cannot call function, pointer is null.");
    end if;

    -- === Access global variable again (read) - check change after function call ===
    if global_var_pointer /= null then
        global_value := global_var_pointer.all;
        put_line ("Value of my_global_variable after function call: " & Interfaces.C.int'Image (global_value));
    else
        put_line("Error: Cannot read global variable, pointer is null.");
    end if;

    -- === Access global variable (write) - change value from Ada code ===
    if global_var_pointer /= null then
        put_line ("Setting my_global_variable to 777 from Ada...");
        global_var_pointer.all := 777;
        -- Read again to confirm change
        global_value := global_var_pointer.all;
        put_line ("Value of my_global_variable after Ada write: " & Interfaces.C.int'Image (global_value));
    else
        put_line("Error: Cannot write global variable, pointer is null.");
    end if;


    -- Cleanup (close library)
    if handle /= System.Null_Address then
       put_line ("Closing library");
       dl.close (handle);
    end if;

exception
    -- Exception handler variable E -> e
    when e : Program_Error =>
        put_line ("Program Error: " & Exception_Message(e));
        put_line ("Details: " & Exception_Information(e));
         if handle /= System.Null_Address then
          put_line ("Attempting to close library after program error...");
          begin dl.close (handle); exception when others => null; end;
         end if;

    when e : dl.library_load_error =>
       put_line ("Error loading library: " & Exception_Message (e));
       put_line ("Details: " & Exception_Information (e));

    when e : dl.symbol_lookup_error =>
       put_line ("Error looking up symbol: " & Exception_Message (e));
       put_line ("Details: " & Exception_Information (e));
       if handle /= System.Null_Address then
          put_line ("Attempting to close library after symbol lookup error...");
          begin dl.close (handle); exception when others => null; end;
       end if;

    when e : dl.library_close_error =>
       put_line ("Error closing library: " & Exception_Message (e));
       put_line ("Details: " & Exception_Information (e));
    when e : Constraint_Error =>
       put_line ("Constraint Error occurred.");
       if my_func_pointer = null and global_var_pointer = null then
            put_line ("Reason: Pointers might be null.");
       else
            put_line ("Reason: Possibly issue inside C function/library or stack problem.");
       end if;
       put_line ("Details: " & Ada.Exceptions.Exception_Information(e));
       if handle /= System.Null_Address then
          put_line ("Attempting to close library after constraint error...");
          begin dl.close (handle); exception when others => null; end;
       end if;

    when e : others =>
       put_line ("An unexpected error occurred: " & Exception_Name(e));
       put_line ("Details: " & Exception_Information (e));
       if handle /= System.Null_Address then
          begin
             put_line ("Attempting to close library after unexpected error...");
             dl.close (handle);
          exception
               when dl.library_close_error => null;
          end;
       end if;

end Test_Dl;
