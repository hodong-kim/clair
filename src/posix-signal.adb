-- posix-signal.adb
with Interfaces.C;

package body POSIX.Signal is

    use Interfaces.C;

    -- C function imports
    function c_sigaction
      (sig  : int;
       act  : access Sigaction_Record;
       oact : access Sigaction_Record) return int;
    pragma Import (C, c_sigaction, "sigaction");

    procedure c_sigemptyset (set: access Sigset_T);
    pragma Import (C, c_sigemptyset, "sigemptyset");

    procedure c_sigfillset (set: access Sigset_T);
    pragma Import (C, c_sigfillset, "sigfillset");

    procedure c_sigaddset (set: access Sigset_T; signo: int);
    pragma Import (C, c_sigaddset, "sigaddset");

    procedure c_sigdelset (set: access Sigset_T; signo: int);
    pragma Import (C, c_sigdelset, "sigdelset");

    function c_sigismember (set: access constant Sigset_T; signo: int) return int;
    pragma Import (C, c_sigismember, "sigismember");

    procedure c_raise (sig: int);
    pragma Import (C, c_raise, "raise");


    -- sigaction implementation
    procedure sigaction
      (sig  : in Interfaces.C.int;
       act  : in Sigaction_Record;
       oact : out Sigaction_Record)
    is
       act_ptr  : aliased Sigaction_Record := act;
       oact_ptr : aliased Sigaction_Record;
       result   : int;
    begin
       result := c_sigaction (sig, act_ptr'Access, oact_ptr'Access);
       if result /= 0 then
          raise Signal_Error with "sigaction failed";
       end if;
       oact := oact_ptr;
    end sigaction;

    procedure sigaction
      (sig : in Interfaces.C.int;
       act : in Sigaction_Record)
    is
       act_ptr : aliased Sigaction_Record := act;
       result  : int;
    begin
       result := c_sigaction (sig, act_ptr'Access, null);
       if result /= 0 then
          raise Signal_Error with "sigaction failed";
       end if;
    end sigaction;


    -- sigsetops procedure modification: use aliased local variable
    procedure sigemptyset (set : in out Sigset_T) is
       local_set : aliased Sigset_T := set;
    begin
       c_sigemptyset(local_set'Access);
       set := local_set; -- Copy the changed value back to the parameter
    end sigemptyset;

    procedure sigfillset (set : in out Sigset_T) is
       local_set : aliased Sigset_T := set;
    begin
       c_sigfillset(local_set'Access);
       set := local_set;
    end sigfillset;

    procedure sigaddset (set : in out Sigset_T; signo : Interfaces.C.int) is
       local_set : aliased Sigset_T := set;
    begin
       c_sigaddset(local_set'Access, signo);
       set := local_set;
    end sigaddset;

    procedure sigdelset (set : in out Sigset_T; signo : Interfaces.C.int) is
       local_set : aliased Sigset_T := set;
    begin
       c_sigdelset(local_set'Access, signo);
       set := local_set;
    end sigdelset;

    -- sigismember implementation
    function sigismember (set : in Sigset_T; signo : Interfaces.C.int) return Boolean is
       set_ptr : aliased constant Sigset_T := set;
    begin
       return c_sigismember (set_ptr'Access, signo) = 1;
    end sigismember;

    -- raise_signal implementation
    procedure raise_signal (sig : in Interfaces.C.int) is
    begin
       c_raise(sig);
    end raise_signal;

end POSIX.Signal;
