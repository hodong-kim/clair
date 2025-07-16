-- posix-file_control.adb
with System;
with Interfaces.C;
use Interfaces.C;

package body POSIX.File_Control is

    -- Import the C open function privately.
    -- The mode argument is ignored by the OS if O_CREAT is not in flags.
    function C_Open (Path : char_array; Flags : int; Mode : int) return int;
    pragma Import (C, C_Open, "open");

    function C_Close (fd : int) return int;
    pragma Import (C, C_Close, "close");

    -- Open implementation (2 arguments)
    function Open (Path : String; Flags : Interfaces.C.int) return File_Descriptor is
       Result : constant Interfaces.C.int := C_Open (To_C(Path), Flags, 0);
    begin
       if Result = -1 then
          raise File_Control_Error with "open(2) failed for path: " & Path;
       end if;
       return File_Descriptor(Result);
    end Open;

    -- Open implementation (3 arguments)
    function Open (Path : String; Flags : Interfaces.C.int; File_Mode : Mode) return File_Descriptor is
       Result : constant Interfaces.C.int := C_Open (To_C(Path), Flags, Interfaces.C.int(File_Mode));
    begin
       if Result = -1 then
          raise File_Control_Error with "open(2) failed for path: " & Path;
       end if;
       return File_Descriptor(Result);
    end Open;

    -- Implementation for Close
    procedure Close (FD : in File_Descriptor) is
       Result : constant Interfaces.C.int := C_Close(Interfaces.C.int(FD));
    begin
       if Result = -1 then
          raise File_Control_Error with "close(2) failed.";
       end if;
    end Close;

    function C_Write (fd : int; buf : System.Address; count : size_t) return long;
    pragma Import (C, C_Write, "write");

    -- Implementation for Write function
    function Write (FD : in File_Descriptor; Buffer : in String) return Interfaces.C.size_t is
       C_Buffer      : aliased Interfaces.C.char_array := To_C (Buffer);
       Bytes_Written : constant Interfaces.C.long := C_Write (int (FD), C_Buffer'Address, Buffer'Length);
    begin
       if Bytes_Written = -1 then
          raise File_Control_Error with "write(2) failed.";
       end if;
       -- Return the number of bytes written as an unsigned type
       return Interfaces.C.size_t(Bytes_Written);
    end Write;

    -- Import the C flock function privately
    function C_Flock (fd : Interfaces.C.int; operation : Interfaces.C.int) return Interfaces.C.int;
    pragma Import (C, C_Flock, "flock");

    procedure Flock (FD : in File_Descriptor; Operation : in Interfaces.C.int) is
       Result : constant Interfaces.C.int := C_Flock (Interfaces.C.int(FD), Operation);
    begin
       if Result = -1 then
          raise File_Control_Error with "flock(2) failed.";
       end if;
    end Flock;

    ----------------------------------
    -- Private C imports for fcntl
    ----------------------------------
    -- For fcntl commands with no third argument (returns an int)
    function C_Fcntl_Get (fd : int; cmd : int) return int;
    pragma Import (C, C_Fcntl_Get, "fcntl");

    -- For fcntl commands with an integer third argument
    function C_Fcntl_Set_Int (fd : int; cmd : int; arg : int) return int;
    pragma Import (C, C_Fcntl_Set_Int, "fcntl");

    -- For fcntl commands with a pointer third argument
    function C_Fcntl_Set_Ptr (fd : int; cmd : int; arg : System.Address) return int;
    pragma Import (C, C_Fcntl_Set_Ptr, "fcntl");

    ----------------------------------
    -- fcntl wrapper implementations
    ----------------------------------
    function Get_FL_Flags (FD : in File_Descriptor) return Flags is
       Result : constant Interfaces.C.int := C_Fcntl_Get (int(FD), F_GETFL);
    begin
       if Result = -1 then
          raise File_Control_Error with "fcntl(F_GETFL) failed.";
       end if;
       return Flags(Result);
    end Get_FL_Flags;

    procedure Set_FL_Flags (FD : in File_Descriptor; Fl_Flags : in Flags) is
       Result : constant Interfaces.C.int := C_Fcntl_Set_Int (int(FD), F_SETFL, int(Fl_Flags));
    begin
       if Result = -1 then
          raise File_Control_Error with "fcntl(F_SETFL) failed.";
       end if;
    end Set_FL_Flags;

    procedure Get_Lock (FD : in File_Descriptor; Lock : in out Flock_Record) is
       Result : constant Interfaces.C.int := C_Fcntl_Set_Ptr (int(FD), F_GETLK, Lock'Address);
    begin
       if Result = -1 then
          raise File_Control_Error with "fcntl(F_GETLK) failed.";
       end if;
    end Get_Lock;

    procedure Set_Lock (FD : in File_Descriptor; Lock : in Flock_Record) is
       -- We pass a constant record, so we need a local variable to get its address.
       Lock_Copy : Flock_Record := Lock;
       Result    : constant Interfaces.C.int := C_Fcntl_Set_Ptr (int(FD), F_SETLK, Lock_Copy'Address);
    begin
       if Result = -1 then
          raise File_Control_Error with "fcntl(F_SETLK) failed.";
       end if;
    end Set_Lock;

    procedure Set_Lock_Wait (FD : in File_Descriptor; Lock : in Flock_Record) is
       Lock_Copy : Flock_Record := Lock;
       Result    : constant Interfaces.C.int := C_Fcntl_Set_Ptr (int(FD), F_SETLKW, Lock_Copy'Address);
    begin
       if Result = -1 then
          raise File_Control_Error with "fcntl(F_SETLKW) failed.";
       end if;
    end Set_Lock_Wait;

    function C_Dup2 (oldfd : int; newfd : int) return int;
    pragma Import (C, C_Dup2, "dup2");

    function C_Dup (oldfd : int) return int;
    pragma Import (C, C_Dup, "dup");

    -- Implementation for Dup
    function Dup (Old_FD : in File_Descriptor) return File_Descriptor is
       Result : constant Interfaces.C.int := C_Dup(Interfaces.C.int(Old_FD));
    begin
       if Result = -1 then
          raise File_Control_Error with "dup(2) failed.";
       end if;
       return File_Descriptor(Result);
    end Dup;

    -- Implementation for Dup2
    procedure Dup2 (Old_FD : in File_Descriptor; New_FD : in File_Descriptor) is
       Result : constant Interfaces.C.int := C_Dup2(Interfaces.C.int(Old_FD), Interfaces.C.int(New_FD));
    begin
       if Result = -1 then
          raise File_Control_Error with "dup2(2) failed.";
       end if;
    end Dup2;

end POSIX.File_Control;
