with Interfaces.C;
use Interfaces.C;

package body POSIX.Process is

    -- C function imports
    function C_Fork return Interfaces.C.int;
    pragma Import (C, C_Fork, "fork");

    function C_Underscore_Fork return Interfaces.C.int;
    pragma Import (C, C_Underscore_Fork, "_Fork");

    function C_Set_SID return Interfaces.C.int;
    pragma Import (C, C_Set_SID, "setsid");


    -- (Implementations for Fork and Underscore_Fork)
    function Fork return Process_ID is
       Result : constant Interfaces.C.int := C_Fork;
    begin
       if Result = -1 then
          raise Fork_Error with "fork(2) failed to create a new process.";
       end if;
       return Process_ID (Result);
    end Fork;

    function Underscore_Fork return Process_ID is
       Result : constant Interfaces.C.int := C_Underscore_Fork;
    begin
       if Result = -1 then
          raise Fork_Error with "_Fork(2) failed to create a new process.";
       end if;
       return Process_ID (Result);
    end Underscore_Fork;


    -- Implementation for the Set_SID procedure
    procedure Set_SID is
       Result : constant Interfaces.C.int := C_Set_SID;
    begin
       if Result = -1 then
          -- setsid(2) failed. This usually happens if the process
          -- is already a process group leader.
          raise Set_SID_Error with "setsid(2) failed.";
       end if;
       -- On success, the action is complete.
    end Set_SID;

    function c_getpid return int;
    pragma Import (C, c_getpid, "getpid");

    function getpid return Process_ID is
    begin
       return Process_ID(c_getpid);
    end getpid;

end POSIX.Process;
