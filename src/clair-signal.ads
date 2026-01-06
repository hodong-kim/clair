-- clair-signal.ads
-- Copyright (c) 2025,2026 Hodong Kim <hodong@nimfsoft.art>
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
with Interfaces.C;
with System.Storage_Elements;
with Clair.Platform;

package Clair.Signal is

  -- A distinct type for signal numbers
  type Number is new Interfaces.C.int;
  type Flags  is new Interfaces.C.int;
  function "or" (left, right : Flags) return Flags;

  type Set is array (1 .. Clair.Platform.SIZE_OF_SIGSET)
    of Interfaces.Unsigned_8;
  pragma convention (c, Set);

  type Action is record
    sa_handler : System.Address;
    sa_flags   : Interfaces.C.int;
    sa_mask    : aliased Clair.Signal.Set;
  end record
    with convention => c;

  for Action use record
    sa_handler at Clair.Platform.SIGACTION_HANDLER_OFFSET
               range 0 .. Standard'address_size - 1;

    sa_flags   at Clair.Platform.SIGACTION_FLAGS_OFFSET
               range 0 .. Interfaces.C.int'size - 1;

    sa_mask    at Clair.Platform.SIGACTION_MASK_OFFSET
               range 0 .. (Clair.Platform.SIZE_OF_SIGSET *
                           System.Storage_Unit) - 1;
  end record;

  for Action'size use Clair.Platform.SIGACTION_SIZE * System.Storage_Unit;
  for Action'alignment use Clair.Platform.SIGACTION_ALIGNMENT;
  -- Access type for user-registered handlers.
  type Handler is access procedure (signo   : Number);

  pragma convention (c, Handler);

  INTERRUPT     : constant := Clair.Platform.SIGINT;
  TERMINATION   : constant := Clair.Platform.SIGTERM;
  TERMINAL_STOP : constant := Clair.Platform.SIGTSTP;
  CHILD_STATUS  : constant := Clair.Platform.SIGCHLD;

  No_Child_Wait : constant := Clair.Platform.SA_NOCLDWAIT;

  Default_Handler : constant Handler;
  Ignore_Handler  : constant Handler;

  procedure set_empty (sig_set : aliased out Set);

  procedure set_add   (sig_set : aliased in out Set;
                       signo   : Number);

  function is_member (Sig_Set : aliased in Set;
                      signum  : Number) return Boolean;

  procedure block (sig_set : aliased in Set;
                   oset    : out Set);

  procedure unblock (sig_set : aliased in Set;
                     oset    : out Set);

  procedure unblock (sig_set : aliased in Set);

  procedure set_mask (new_set : aliased in Set);

  procedure set_wait (sig_set : aliased in Set;
                      signo   : out Number);

  -- wrapper for raise(3): Sends a signal to the current process
  procedure send_signal (signo : Number);

  procedure set_action (signo   : Number;
                        handler : Signal.Handler;
                        flags   : Signal.Flags := 0);

  -- Atomically changes the signal mask and waits for a signal.
  -- This procedure will only return after a signal handler has been executed.
  procedure suspend (mask : aliased in Set);
  -- Atomically waits for any signal to be delivered.
  -- This is a safe implementation of pause(2) using sigsuspend(2).
  procedure pause;

private
  function to_handler is new Ada.Unchecked_Conversion
    (source => System.Address,
     target => Handler);

  Default_Handler : constant Handler :=
    to_handler (System.Storage_Elements.To_Address (Clair.Platform.SIG_DFL));

  Ignore_Handler : constant Handler :=
    to_handler (System.Storage_Elements.To_Address (Clair.Platform.SIG_IGN));
end Clair.Signal;
