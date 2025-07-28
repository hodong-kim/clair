-- clair-signal.ads
-- Copyright (c) 2025 Hodong Kim <hodong@nimfsoft.art>
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

with Interfaces.C;
with Clair.Types;
with System.Storage_Elements;
with sys_signal_h;

package Clair.Signal is

  -- A distinct type for signal numbers
  subtype Number is Interfaces.C.int;
  subtype Action is sys_signal_h.sigaction;
  subtype Info   is sys_signal_h.siginfo_t;
  subtype Set    is sys_signal_h.sigset_t;

  -- Access type for user-registered handlers.
  type Handler1_Access is access procedure (signo   : Number);
  type Handler3_Access is access procedure (signo   : Number;
                                            siginfo : access Info;
                                            context : System.Address);
  pragma convention (c, Handler1_Access);
  pragma convention (c, Handler3_Access);

  -- signals
  SIGHUP    : constant := sys_signal_h.SIGHUP;
  SIGINT    : constant := sys_signal_h.SIGINT;
  SIGQUIT   : constant := sys_signal_h.SIGQUIT;
  SIGILL    : constant := sys_signal_h.SIGILL;
  SIGABRT   : constant := sys_signal_h.SIGABRT;
  SIGFPE    : constant := sys_signal_h.SIGFPE;
  SIGKILL   : constant := sys_signal_h.SIGKILL;
  SIGBUS    : constant := sys_signal_h.SIGBUS;
  SIGSEGV   : constant := sys_signal_h.SIGSEGV;
  SIGSYS    : constant := sys_signal_h.SIGSYS;
  SIGPIPE   : constant := sys_signal_h.SIGPIPE;
  SIGALRM   : constant := sys_signal_h.SIGALRM;
  SIGTERM   : constant := sys_signal_h.SIGTERM;
  SIGURG    : constant := sys_signal_h.SIGURG;
  SIGSTOP   : constant := sys_signal_h.SIGSTOP;
  SIGTSTP   : constant := sys_signal_h.SIGTSTP;
  SIGCONT   : constant := sys_signal_h.SIGCONT;
  SIGCHLD   : constant := sys_signal_h.SIGCHLD;
  SIGTTIN   : constant := sys_signal_h.SIGTTIN;
  SIGTTOU   : constant := sys_signal_h.SIGTTOU;
  SIGUSR1   : constant := sys_signal_h.SIGUSR1;
  SIGUSR2   : constant := sys_signal_h.SIGUSR2;

  -- Options may be specified by setting sa_flags.
  SA_NOCLDSTOP : constant := sys_signal_h.SA_NOCLDSTOP;
  SA_ONSTACK   : constant := sys_signal_h.SA_ONSTACK;
  SA_RESTART   : constant := sys_signal_h.SA_RESTART;
  SA_RESETHAND : constant := sys_signal_h.SA_RESETHAND;
  SA_NODEFER   : constant := sys_signal_h.SA_NODEFER;
  SA_NOCLDWAIT : constant := sys_signal_h.SA_NOCLDWAIT;
  SA_SIGINFO   : constant := sys_signal_h.SA_SIGINFO;

  function to_action_handler is new Ada.Unchecked_Conversion
    (source => System.Address,
     target => Handler1_Access);

  SIG_DFL : constant Handler1_Access :=
    to_action_handler (System.Storage_Elements.to_address (0)); -- <<<<<

  SIG_IGN : constant Handler1_Access :=
    to_action_handler (System.Storage_Elements.to_address (1)); -- <<<<<

  procedure empty_set (sig_set : aliased out Set);

  -- wrapper for raise(3): Sends a signal to the current process
  procedure send_signal (signo : Number);

  -- wrapper for sigaction(2) function
  procedure set_action (signo      : in          Number;
                        new_action : aliased in  Action;
                        old_action : aliased out Action);

  -- Atomically changes the signal mask and waits for a signal.
  -- This procedure will only return after a signal handler has been executed.
  procedure suspend (mask : aliased in Set);
  -- Atomically waits for any signal to be delivered.
  -- This is a safe implementation of pause(2) using sigsuspend(2).
  procedure pause;

end Clair.Signal;
