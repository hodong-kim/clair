-- clair-ffi.adb
-- Copyright (c) 2026 Hodong Kim <hodong@nimfsoft.com>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--
with Ada.Exceptions;
with Clair.Errors;
with Clair.Exceptions;

package body Clair.FFI is
  use type Interfaces.C.int;
  use type System.Address;

  ---------------------------------------------------------------------------
  -- Internal: Exception Mapper
  -- Extracts the mapping logic from the generic to prevent code bloat.
  ---------------------------------------------------------------------------
  function exception_to_code (occurrence : Ada.Exceptions.Exception_Occurrence)
  return Interfaces.C.int is
  begin
    -- Reraise the exception to use the standard exception matching mechanism
    Ada.Exceptions.reraise_occurrence (occurrence);
  exception
    -- Ada predefined exceptions to error codes
    when Constraint_Error =>
      return Clair.Errors.Constraint_Error;
    when Program_Error =>
      return Clair.Errors.Program_Error;
    when Storage_Error =>
      return Clair.Errors.Storage_Error;
    when Tasking_Error =>
      return Clair.Errors.Tasking_Error;
    -- Mapping Logic: Convert Exception IDs defined in Clair.Exceptions
    when Clair.Exceptions.Operation_Not_Permitted =>
      return Clair.Errors.Operation_Not_Permitted;
    when Clair.Exceptions.No_Such_File_Or_Directory =>
      return Clair.Errors.No_Such_File_Or_Directory;
    when Clair.Exceptions.No_Such_Process =>
      return Clair.Errors.No_Such_Process;
    when Clair.Exceptions.Interrupted_System_Call =>
      return Clair.Errors.Interrupted_System_Call;
    when Clair.Exceptions.Input_Output_Error =>
      return Clair.Errors.Input_Output_Error;
    when Clair.Exceptions.Device_Not_Configured =>
      return Clair.Errors.Device_Not_Configured;
    when Clair.Exceptions.Argument_List_Too_Long =>
      return Clair.Errors.Argument_List_Too_Long;
    when Clair.Exceptions.Exec_Format_Error =>
      return Clair.Errors.Exec_Format_Error;
    when Clair.Exceptions.Bad_File_Descriptor =>
      return Clair.Errors.Bad_File_Descriptor;
    when Clair.Exceptions.No_Child_Processes =>
      return Clair.Errors.No_Child_Processes;
    when Clair.Exceptions.Resource_Deadlock_Avoided =>
      return Clair.Errors.Resource_Deadlock_Avoided;
    when Clair.Exceptions.Cannot_Allocate_Memory =>
      return Clair.Errors.Cannot_Allocate_Memory;
    when Clair.Exceptions.Permission_Denied =>
      return Clair.Errors.Permission_Denied;
    when Clair.Exceptions.Bad_Address =>
      return Clair.Errors.Bad_Address;
    when Clair.Exceptions.Block_Device_Required =>
      return Clair.Errors.Block_Device_Required;
    when Clair.Exceptions.Device_Busy =>
      return Clair.Errors.Device_Busy;
    when Clair.Exceptions.File_Exists =>
      return Clair.Errors.File_Exists;
    when Clair.Exceptions.Cross_Device_Link =>
      return Clair.Errors.Cross_Device_Link;
    when Clair.Exceptions.Operation_Not_Supported_By_Device =>
      return Clair.Errors.Operation_Not_Supported_By_Device;
    when Clair.Exceptions.Not_A_Directory =>
      return Clair.Errors.Not_A_Directory;
    when Clair.Exceptions.Is_A_Directory =>
      return Clair.Errors.Is_A_Directory;
    when Clair.Exceptions.Invalid_Argument =>
      return Clair.Errors.Invalid_Argument;
    when Clair.Exceptions.Too_Many_Open_Files_In_System =>
      return Clair.Errors.Too_Many_Open_Files_In_System;
    when Clair.Exceptions.Too_Many_Open_Files =>
      return Clair.Errors.Too_Many_Open_Files;
    when Clair.Exceptions.Inappropriate_Ioctl_For_Device =>
      return Clair.Errors.Inappropriate_Ioctl_For_Device;
    when Clair.Exceptions.Text_File_Busy =>
      return Clair.Errors.Text_File_Busy;
    when Clair.Exceptions.File_Too_Large =>
      return Clair.Errors.File_Too_Large;
    when Clair.Exceptions.No_Space_Left_On_Device =>
      return Clair.Errors.No_Space_Left_On_Device;
    when Clair.Exceptions.Illegal_Seek =>
      return Clair.Errors.Illegal_Seek;
    when Clair.Exceptions.Read_Only_File_System =>
      return Clair.Errors.Read_Only_File_System;
    when Clair.Exceptions.Too_Many_Links =>
      return Clair.Errors.Too_Many_Links;
    when Clair.Exceptions.Broken_Pipe =>
      return Clair.Errors.Broken_Pipe;
    when Clair.Exceptions.Numerical_Argument_Out_Of_Domain =>
      return Clair.Errors.Numerical_Argument_Out_Of_Domain;
    when Clair.Exceptions.Result_Too_Large =>
      return Clair.Errors.Result_Too_Large;
    when Clair.Exceptions.Resource_Temporarily_Unavailable =>
      return Clair.Errors.Resource_Temporarily_Unavailable;
    when Clair.Exceptions.Operation_Would_Block =>
      return Clair.Errors.Operation_Would_Block;
    when Clair.Exceptions.Operation_Now_In_Progress =>
      return Clair.Errors.Operation_Now_In_Progress;
    when Clair.Exceptions.Operation_Already_In_Progress =>
      return Clair.Errors.Operation_Already_In_Progress;
    when Clair.Exceptions.Socket_Operation_On_Non_Socket =>
      return Clair.Errors.Socket_Operation_On_Non_Socket;
    when Clair.Exceptions.Destination_Address_Required =>
      return Clair.Errors.Destination_Address_Required;
    when Clair.Exceptions.Message_Too_Long =>
      return Clair.Errors.Message_Too_Long;
    when Clair.Exceptions.Protocol_Wrong_Type_For_Socket =>
      return Clair.Errors.Protocol_Wrong_Type_For_Socket;
    when Clair.Exceptions.Protocol_Not_Available =>
      return Clair.Errors.Protocol_Not_Available;
    when Clair.Exceptions.Protocol_Not_Supported =>
      return Clair.Errors.Protocol_Not_Supported;
    when Clair.Exceptions.Socket_Type_Not_Supported =>
      return Clair.Errors.Socket_Type_Not_Supported;
    when Clair.Exceptions.Operation_Not_Supported =>
      return Clair.Errors.Operation_Not_Supported;
    when Clair.Exceptions.Protocol_Family_Not_Supported =>
      return Clair.Errors.Protocol_Family_Not_Supported;
    when Clair.Exceptions.Address_Family_Not_Supported =>
      return Clair.Errors.Address_Family_Not_Supported;
    when Clair.Exceptions.Address_Already_In_Use =>
      return Clair.Errors.Address_Already_In_Use;
    when Clair.Exceptions.Cannot_Assign_Requested_Address =>
      return Clair.Errors.Cannot_Assign_Requested_Address;
    when Clair.Exceptions.Network_Is_Down =>
      return Clair.Errors.Network_Is_Down;
    when Clair.Exceptions.Network_Is_Unreachable =>
      return Clair.Errors.Network_Is_Unreachable;
    when Clair.Exceptions.Network_Dropped_Connection_On_Reset =>
      return Clair.Errors.Network_Dropped_Connection_On_Reset;
    when Clair.Exceptions.Software_Caused_Connection_Abort =>
      return Clair.Errors.Software_Caused_Connection_Abort;
    when Clair.Exceptions.Connection_Reset_By_Peer =>
      return Clair.Errors.Connection_Reset_By_Peer;
    when Clair.Exceptions.No_Buffer_Space_Available =>
      return Clair.Errors.No_Buffer_Space_Available;
    when Clair.Exceptions.Socket_Is_Already_Connected =>
      return Clair.Errors.Socket_Is_Already_Connected;
    when Clair.Exceptions.Socket_Is_Not_Connected =>
      return Clair.Errors.Socket_Is_Not_Connected;
    when Clair.Exceptions.Cannot_Send_After_Socket_Shutdown =>
      return Clair.Errors.Cannot_Send_After_Socket_Shutdown;
    when Clair.Exceptions.Too_Many_References =>
      return Clair.Errors.Too_Many_References;
    when Clair.Exceptions.Operation_Timed_Out =>
      return Clair.Errors.Operation_Timed_Out;
    when Clair.Exceptions.Connection_Refused =>
      return Clair.Errors.Connection_Refused;
    when Clair.Exceptions.Too_Many_Levels_Of_Symbolic_Links =>
      return Clair.Errors.Too_Many_Levels_Of_Symbolic_Links;
    when Clair.Exceptions.File_Name_Too_Long =>
      return Clair.Errors.File_Name_Too_Long;
    when Clair.Exceptions.Host_Is_Down =>
      return Clair.Errors.Host_Is_Down;
    when Clair.Exceptions.No_Route_To_Host =>
      return Clair.Errors.No_Route_To_Host;
    when Clair.Exceptions.Directory_Not_Empty =>
      return Clair.Errors.Directory_Not_Empty;
    when Clair.Exceptions.Too_Many_Processes =>
      return Clair.Errors.Too_Many_Processes;
    when Clair.Exceptions.Too_Many_Users =>
      return Clair.Errors.Too_Many_Users;
    when Clair.Exceptions.Disc_Quota_Exceeded =>
      return Clair.Errors.Disc_Quota_Exceeded;
    when Clair.Exceptions.Stale_Nfs_File_Handle =>
      return Clair.Errors.Stale_Nfs_File_Handle;
    when Clair.Exceptions.Too_Many_Levels_Of_Remote_In_Path =>
      return Clair.Errors.Too_Many_Levels_Of_Remote_In_Path;
    when Clair.Exceptions.RPC_Struct_Is_Bad =>
      return Clair.Errors.RPC_Struct_Is_Bad;
    when Clair.Exceptions.RPC_Version_Wrong =>
      return Clair.Errors.RPC_Version_Wrong;
    when Clair.Exceptions.RPC_Prog_Not_Avail =>
      return Clair.Errors.RPC_Prog_Not_Avail;
    when Clair.Exceptions.Program_Version_Wrong =>
      return Clair.Errors.Program_Version_Wrong;
    when Clair.Exceptions.Bad_Procedure_For_Program =>
      return Clair.Errors.Bad_Procedure_For_Program;
    when Clair.Exceptions.No_Locks_Available =>
      return Clair.Errors.No_Locks_Available;
    when Clair.Exceptions.Function_Not_Implemented =>
      return Clair.Errors.Function_Not_Implemented;
    when Clair.Exceptions.Inappropriate_File_Type_Or_Format =>
      return Clair.Errors.Inappropriate_File_Type_Or_Format;
    when Clair.Exceptions.Authentication_Error =>
      return Clair.Errors.Authentication_Error;
    when Clair.Exceptions.Need_Authenticator =>
      return Clair.Errors.Need_Authenticator;
    when Clair.Exceptions.Identifier_Removed =>
      return Clair.Errors.Identifier_Removed;
    when Clair.Exceptions.No_Message_Of_Desired_Type =>
      return Clair.Errors.No_Message_Of_Desired_Type;
    when Clair.Exceptions.Value_Too_Large_To_Be_Stored_In_Data_Type =>
      return Clair.Errors.Value_Too_Large_To_Be_Stored_In_Data_Type;
    when Clair.Exceptions.Operation_Canceled =>
      return Clair.Errors.Operation_Canceled;
    when Clair.Exceptions.Illegal_Byte_Sequence =>
      return Clair.Errors.Illegal_Byte_Sequence;
    when Clair.Exceptions.Attribute_Not_Found =>
      return Clair.Errors.Attribute_Not_Found;
    when Clair.Exceptions.Programming_Error =>
      return Clair.Errors.Programming_Error;
    when Clair.Exceptions.Bad_Message =>
      return Clair.Errors.Bad_Message;
    when Clair.Exceptions.Multihop_Attempted =>
      return Clair.Errors.Multihop_Attempted;
    when Clair.Exceptions.Link_Has_Been_Severed =>
      return Clair.Errors.Link_Has_Been_Severed;
    when Clair.Exceptions.Protocol_Error =>
      return Clair.Errors.Protocol_Error;
    when Clair.Exceptions.Capabilities_Insufficient =>
      return Clair.Errors.Capabilities_Insufficient;
    when Clair.Exceptions.Not_Permitted_In_Capability_Mode =>
      return Clair.Errors.Not_Permitted_In_Capability_Mode;
    when Clair.Exceptions.State_Not_Recoverable =>
      return Clair.Errors.State_Not_Recoverable;
    when Clair.Exceptions.Previous_Owner_Died =>
      return Clair.Errors.Previous_Owner_Died;
    when Clair.Exceptions.Integrity_Check_Failed =>
      return Clair.Errors.Integrity_Check_Failed;
    -- Dynamic library related errors
    when Clair.Exceptions.Library_Load_Error =>
      return Clair.Errors.Library_Load_Error;
    when Clair.Exceptions.Library_Close_Error =>
      return Clair.Errors.Library_Close_Error;
    when Clair.Exceptions.Symbol_Lookup_Error =>
      return Clair.Errors.Symbol_Lookup_Error;
    when others =>
      -- Unmapped Error
      return Clair.Errors.Unmapped_Error;
  end exception_to_code;

  package body Bindings is
    ---------------------------------------------------------------------------
    -- Internal: Safe Executors (Zero-Overhead Exception Barriers)
    -- These generic functions wrap the actual logic to trap Ada exceptions.
    -- Pragma inline ensures they are flattened into the caller.
    ---------------------------------------------------------------------------
    -- Executor for int-returning functions
    generic
      with function job return Interfaces.C.int;
    function safe_run_int return Interfaces.C.int;
    pragma inline (safe_run_int);

    function safe_run_int return Interfaces.C.int is
    begin
      return job;
    exception
      when exc : others =>
        return exception_to_code (exc);
    end safe_run_int;

    ---------------------------------------------------------------------------
    -- Group 1: Integer Return Bindings Implementation
    ---------------------------------------------------------------------------

    function bind_int_1 (self : System.Address) return Interfaces.C.int is
      ctx : Context_Access;

      function worker return Interfaces.C.int is
      begin
        ctx := to_context (self);
        return action (ctx.all);
      end worker;
      pragma inline (worker);

      function run is new safe_run_int (job => worker);
    begin
      if self = System.Null_Address then
        return Clair.Errors.Constraint_Error;
      end if;

      return run;
    end bind_int_1;

    function bind_int_2 (self : System.Address;
                         a1   : Arg1_Type)
    return Interfaces.C.int is
      ctx : Context_Access;

      function worker return Interfaces.C.int is
      begin
        ctx := to_context (self);
        return action (ctx.all, a1);
      end worker;
      pragma inline (worker);

      function run is new safe_run_int (job => worker);
    begin
      if self = System.Null_Address then
        return Clair.Errors.Constraint_Error;
      end if;

      return run;
    end bind_int_2;

    function bind_int_3 (self : System.Address;
                         a1   : Arg1_Type;
                         a2   : Arg2_Type)
    return Interfaces.C.int is
      ctx : Context_Access;

      function worker return Interfaces.C.int is
      begin
        ctx := to_context (self);
        return action (ctx.all, a1, a2);
      end worker;
      pragma inline (worker);

      function run is new safe_run_int (job => worker);
    begin
      if self = System.Null_Address then
        return Clair.Errors.Constraint_Error;
      end if;

      return run;
    end bind_int_3;

    ---------------------------------------------------------------------------
    -- Group 2: Handle Return Bindings Implementation
    ---------------------------------------------------------------------------

    function bind_handle_3 (self : System.Address;
                            a1   : Arg1_Type;
                            a2   : Arg2_Type)
    return System.Address is
      ctx : Context_Access;
    begin

      if self = System.Null_Address then
        return System.Null_Address;
      end if;

      ctx := to_context (self);
      return to_address (action (ctx.all, a1, a2));
    exception
      when others =>
        return System.Null_Address;
    end bind_handle_3;

    function bind_handle_4 (self : System.Address;
                            a1   : Arg1_Type;
                            a2   : Arg2_Type;
                            a3   : Arg3_Type)
    return System.Address is
      ctx : Context_Access;
    begin

      if self = System.Null_Address then
        return System.Null_Address;
      end if;

      ctx := to_context (self);
      return to_address (action (ctx.all, a1, a2, a3));
    exception
      when others =>
        return System.Null_Address;
    end bind_handle_4;

    function bind_handle_5 (self : System.Address;
                            a1   : Arg1_Type;
                            a2   : Arg2_Type;
                            a3   : Arg3_Type;
                            a4   : Arg4_Type)
    return System.Address is
      ctx : Context_Access;
    begin

      if self = System.Null_Address then
        return System.Null_Address;
      end if;

      ctx := to_context (self);
      return to_address (action (ctx.all, a1, a2, a3, a4));
    exception
      when others =>
        return System.Null_Address;
    end bind_handle_5;

  function bind_destructor (self : System.Address) return Interfaces.C.int is
    ctx : Context_Access;

    function worker return Interfaces.C.int is
    begin
      ctx := to_context (self);
      return action (ctx);
    end worker;
    pragma inline (worker);

    function run is new safe_run_int (job => worker);
  begin
    if self = System.Null_Address then
      return Clair.Errors.No_Error;
    end if;

    return run;
  end bind_destructor;

  end Bindings;

end Clair.FFI;
