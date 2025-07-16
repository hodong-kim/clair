-- posix-signal.ads
with System;
with Interfaces.C;
with POSIX.Process; -- For Process_ID

-- @summary
--  Provides features for POSIX signal handling. Includes an Ada interface
--  for sigaction(2) and related utility functions.
package POSIX.Signal is

    -- Corresponds to C's sigset_t.
    type Sigset_T is private;
    Signal_Error : exception;

    -- Signal numbers (full list omitted)
    SIGHUP    : constant Interfaces.C.int := 1;
    SIGINT    : constant Interfaces.C.int := 2;
    SIGQUIT   : constant Interfaces.C.int := 3;
    SIGILL    : constant Interfaces.C.int := 4;
    SIGTRAP   : constant Interfaces.C.int := 5;
    SIGABRT   : constant Interfaces.C.int := 6;
    SIGFPE    : constant Interfaces.C.int := 8;
    SIGKILL   : constant Interfaces.C.int := 9;
    SIGBUS    : constant Interfaces.C.int := 10;
    SIGSEGV   : constant Interfaces.C.int := 11;
    SIGSYS    : constant Interfaces.C.int := 12;
    SIGPIPE   : constant Interfaces.C.int := 13;
    SIGALRM   : constant Interfaces.C.int := 14;
    SIGTERM   : constant Interfaces.C.int := 15;
    SIGURG    : constant Interfaces.C.int := 16;
    SIGSTOP   : constant Interfaces.C.int := 17;
    SIGTSTP   : constant Interfaces.C.int := 18;
    SIGCONT   : constant Interfaces.C.int := 19;
    SIGCHLD   : constant Interfaces.C.int := 20;
    SIGUSR1   : constant Interfaces.C.int := 30;
    SIGUSR2   : constant Interfaces.C.int := 31;

    -- sigaction flags (sa_flags)
    SA_ONSTACK    : constant Interfaces.C.int := 16#0001#;
    SA_RESTART    : constant Interfaces.C.int := 16#0002#;
    SA_RESETHAND  : constant Interfaces.C.int := 16#0004#;
    SA_NOCLDSTOP  : constant Interfaces.C.int := 16#0008#;
    SA_NODEFER    : constant Interfaces.C.int := 16#0010#;
    SA_NOCLDWAIT  : constant Interfaces.C.int := 16#0020#;
    SA_SIGINFO    : constant Interfaces.C.int := 16#0040#;

    -- Signal handler types
    -- Default handler: void handler(int);
    type Signal_Handler is access procedure (sig : Interfaces.C.int);
    pragma Convention (C, Signal_Handler);

    -- SA_SIGINFO handler: void handler(int, siginfo_t *, void *);
    -- siginfo_t and ucontext_t are treated as opaque types here.
    type Siginfo_T is limited private;
    type Ucontext_T is limited private;
    type Siginfo_Handler is access procedure
      (sig   : Interfaces.C.int;
       info  : access Siginfo_T;
       uap   : access Ucontext_T);
    pragma Convention (C, Siginfo_Handler);

    -- Special handler values (SIG_DFL, SIG_IGN)
    SIG_DFL : constant Signal_Handler := null; -- Actual address is handled in the implementation
    SIG_IGN : constant Signal_Handler := null; -- Actual address is handled in the implementation

    -- Record matching the memory layout of C's struct sigaction
    -- The union part is simplified to System.Address to resolve layout issues
    type Sigaction_Record is record
       -- Typical C struct order: union, mask, flags
       sa_handler_address : System.Address := System.Null_Address;
       sa_mask            : Sigset_T;
       sa_flags           : Interfaces.C.int;
    end record;
    pragma Convention (C, Sigaction_Record);

    -- sigaction(2) bindings
    procedure sigaction
      (sig  : in Interfaces.C.int;
       act  : in Sigaction_Record;
       oact : out Sigaction_Record);

    procedure sigaction
      (sig : in Interfaces.C.int;
       act : in Sigaction_Record);

    -- sigsetops(3) functions
    procedure sigemptyset (set : in out Sigset_T);
    procedure sigfillset (set : in out Sigset_T);
    procedure sigaddset (set : in out Sigset_T; signo : Interfaces.C.int);
    procedure sigdelset (set : in out Sigset_T; signo : Interfaces.C.int);
    function sigismember (set : in Sigset_T; signo : Interfaces.C.int) return Boolean;

    -- raise(3) function
    procedure raise_signal (sig : in Interfaces.C.int);

private
    -- Defined in the private part to match the actual C type definition
    type Sigset_T is array (1 .. 4) of aliased Interfaces.C.unsigned_long;
    pragma Convention (C, Sigset_T);

    type Siginfo_T is null record; -- Placeholder
    pragma Convention (C, Siginfo_T);

    type Ucontext_T is null record; -- Placeholder
    pragma Convention (C, Ucontext_T);

end POSIX.Signal;
