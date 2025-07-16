with Interfaces.C;

-- @summary
--  Provides features for POSIX process creation and control.
package POSIX.Process is

    -- Corresponds to the C pid_t type.
    type Process_ID is new Interfaces.C.int;

    -- Exception raised when a fork(2) call fails.
    Fork_Error : exception;

    -- @description
    --  Creates a new child process by duplicating the current process.
    --  On success, returns the child's PID to the parent and 0 to the child.
    -- @raises
    --  Fork_Error : Raised if process creation fails.
    function Fork return Process_ID;

    -- @description
    --  Similar to fork, but is an async-signal-safe version that
    --  can be safely called from within a signal handler.
    -- @raises
    --  Fork_Error : Raised if process creation fails.
    function Underscore_Fork return Process_ID;

    -- Exception raised when a setsid(2) call fails.
    Set_SID_Error : exception;

    -- @description
    --  Creates a new session. The calling process becomes the session
    --  leader and is detached from its controlling terminal.
    --  This should be called in a child process after a fork.
    -- @raises
    --  Set_SID_Error : Raised on failure (e.g., if already a group leader).
    procedure Set_SID;

    function getpid return Process_ID;

end POSIX.Process;
