-- posix-file_control.ads
with Interfaces.C;
with POSIX.Process;

-- @summary
--  Provides an interface to POSIX functions for applying advisory
--  locks on open files.
package POSIX.File_Control is

    type File_Descriptor is new Interfaces.C.int;
    type Mode is new Interfaces.C.unsigned; -- For open() mode
    type Flags is new Interfaces.C.int; -- For file status flags
    type C_Offset is new Interfaces.C.long; -- Corresponds to C's off_t

    -- Add these standard descriptors
    STDIN_FD  : constant File_Descriptor := 0;
    STDOUT_FD : constant File_Descriptor := 1;
    STDERR_FD : constant File_Descriptor := 2;

    -- Common flags for open(2)
    O_RDONLY : constant Interfaces.C.int := 0;   -- Read only
    O_WRONLY : constant Interfaces.C.int := 1;   -- Write only
    O_RDWR   : constant Interfaces.C.int := 2;   -- Read/Write
    O_CREAT  : constant Interfaces.C.int := 512; -- Create if it doesn't exist
    O_TRUNC  : constant Interfaces.C.int := 1024; -- Add this line
    O_APPEND : constant Interfaces.C.int := 8;

    -- Operations for flock(2)
    LOCK_SH : constant Interfaces.C.int := 16#01#; -- Shared file lock
    LOCK_EX : constant Interfaces.C.int := 16#02#; -- Exclusive file lock
    LOCK_NB : constant Interfaces.C.int := 16#04#; -- Do not block when locking
    LOCK_UN : constant Interfaces.C.int := 16#08#; -- Unlock file

    ----------------------------------
    -- Constants for fcntl(2)
    ----------------------------------
    -- Commands
    F_GETFL  : constant Interfaces.C.int := 3;  -- Get file status flags
    F_SETFL  : constant Interfaces.C.int := 4;  -- Set file status flags
    F_GETLK  : constant Interfaces.C.int := 7;  -- Get lock
    F_SETLK  : constant Interfaces.C.int := 8;  -- Set lock
    F_SETLKW : constant Interfaces.C.int := 9;  -- Set lock and wait

    -- Lock types for Flock_Record
    F_RDLCK : constant Interfaces.C.short := 1; -- Shared or read lock
    F_WRLCK : constant Interfaces.C.short := 2; -- Exclusive or write lock
    F_UNLCK : constant Interfaces.C.short := 3; -- Unlock

    -- Whence values for Flock_Record
    SEEK_SET : constant Interfaces.C.short := 0; -- Relative to beginning of file
    SEEK_CUR : constant Interfaces.C.short := 1; -- Relative to current position
    SEEK_END : constant Interfaces.C.short := 2; -- Relative to end of file

    -- Corresponds to C's 'struct flock'
    type Flock_Record is record
       L_Start  : C_Offset;
       L_Len    : C_Offset;
       L_PID    : POSIX.Process.Process_ID;
       L_Type   : Interfaces.C.short;
       L_Whence : Interfaces.C.short;
    end record;

    -- Exception for file control errors
    File_Control_Error : exception;

    -- @description For opening files without the O_CREAT flag.
    function Open (Path : String; Flags : Interfaces.C.int) return File_Descriptor;

    -- @description For opening files with the O_CREAT flag, which requires a mode.
    function Open (Path : String; Flags : Interfaces.C.int; File_Mode : Mode) return File_Descriptor;

    -- @description
    --  Writes data to a file descriptor. Note: This may write fewer
    --  bytes than the buffer's length (a "partial write").
    -- @return
    --  The number of bytes actually written.
    -- @raises
    --  File_Control_Error : Raised on a system call error (returns -1).
    function Write (FD : in File_Descriptor; Buffer : in String) return Interfaces.C.size_t;

    -- @description
    --  Closes a file descriptor.
    -- @param FD
    --  The file descriptor to close.
    -- @raises
    --  File_Control_Error : Raised on failure.
    procedure Close (FD : in File_Descriptor);

    -- @description
    --  Duplicates an existing file descriptor to the lowest numbered
    --  unused descriptor. Wrapper for dup(2).
    -- @return
    --  The new file descriptor.
    -- @raises
    --  File_Control_Error : Raised on failure.
    function Dup (Old_FD : in File_Descriptor) return File_Descriptor;

    -- @description
    --  Duplicates a file descriptor to a specific descriptor number.
    --  Wrapper for dup2(2).
    -- @raises
    --  File_Control_Error : Raised on failure.
    procedure Dup2 (Old_FD : in File_Descriptor; New_FD : in File_Descriptor);

    -- @description
    --  Applies or removes an advisory lock on an open file.
    --  Wrapper for flock(2).
    -- @param FD
    --  The file descriptor of the file to lock.
    -- @param Operation
    --  The lock operation (e.g., LOCK_EX or'd with LOCK_NB).
    -- @raises
    --  File_Control_Error : Raised on failure (e.g., if the lock is
    --                       already held and LOCK_NB is specified).
    procedure Flock (FD : in File_Descriptor; Operation : in Interfaces.C.int);

    -- fcntl wrappers
    function  Get_FL_Flags (FD : in File_Descriptor) return Flags;
    procedure Set_FL_Flags (FD : in File_Descriptor; Fl_Flags : in Flags);
    procedure Get_Lock     (FD : in File_Descriptor; Lock : in out Flock_Record);
    procedure Set_Lock     (FD : in File_Descriptor; Lock : in Flock_Record);
    procedure Set_Lock_Wait(FD : in File_Descriptor; Lock : in Flock_Record);

end POSIX.File_Control;
