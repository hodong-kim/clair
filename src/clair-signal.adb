-- clair-signal.adb
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
--
with System;
with Clair.Errno;

package body Clair.Signal is
  use type Interfaces.C.int;
  use type Interfaces.C.unsigned;

  function to_address is new Ada.Unchecked_Conversion (
    source => Handler,
    target => System.Address
  );

  function c_sigaction (
    Sig  : Interfaces.C.int;
    Act  : access Action;
    Oact : access Action) return Interfaces.C.int
  with import, convention => c, external_name => "sigaction";

  function c_sigemptyset (sig_set : access Set)
  return Interfaces.C.int;
  pragma import (c, c_sigemptyset, "sigemptyset");

  function c_sigaddset (sig_set : access Set; signo : Interfaces.C.int)
  return Interfaces.C.int;
  pragma import (c, c_sigaddset, "sigaddset");

  function c_sigismember (set    : System.Address;
                          signum : Interfaces.C.int) return Interfaces.C.int
  with import, convention => c, external_name => "sigismember";

  function c_pthread_sigmask (how     : Interfaces.C.int;
                            sig_set : System.Address;
                            oldset  : System.Address)
  return Interfaces.C.int;
  pragma import (c, c_pthread_sigmask, "sigprocmask");

  function c_sigwait (sig_set : access constant Set;
                      sig     : access Number) return Interfaces.C.int;
  pragma import (c, c_sigwait, "sigwait");

  function c_raise (signo : Interfaces.C.int) return Interfaces.C.int;
  pragma import (c, c_raise, "raise");

  function c_sigsuspend (mask : access constant Set)
    return Interfaces.C.int;
  pragma import (c, c_sigsuspend, "sigsuspend");

  procedure set_empty (sig_set : aliased out Set) is
    retval : constant Interfaces.C.int := c_sigemptyset (sig_set'access);
  begin
    if retval /= 0 then
      declare
        errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
        error_msg  : constant String :=
          Clair.Errno.format_posix_error_message
            (errno_code    => errno_code,
             function_name => "Clair.Signal.set_empty (sigemptyset)",
             context_info  => "Failed to initialize signal set");
      begin
        Clair.Errno.raise_from_errno (errno_code, error_msg);
      end;
    end if;
  end set_empty;

  procedure set_add (sig_set : aliased in out Set;
                     signo   : Number) is
    retval : constant Interfaces.C.int
           := c_sigaddset (sig_set'access, Interfaces.C.int(signo));
  begin
    if retval /= 0 then
      declare
        errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
        error_msg  : constant String :=
          Clair.Errno.format_posix_error_message
            (errno_code    => errno_code,
             function_name => "Clair.Signal.set_add (sigaddset)",
             context_info  => "for signal " & signo'image);
      begin
        Clair.Errno.raise_from_errno (errno_code, error_msg);
      end;
    end if;
  end set_add;

  function is_member (sig_set : aliased in Set;
                      signum  : Number) return Boolean is
    retval : Interfaces.C.int;
  begin
    retval := c_sigismember (sig_set'address, Interfaces.C.int(signum));

    if retval = 1 then
      return True;

    elsif retval = 0 then
      return False;

    else
        declare
          errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
          error_msg  : constant String :=
            Clair.Errno.format_posix_error_message
              (errno_code    => errno_code,
                Function_Name => "Clair.Signal.is_member (sigismember)",
                Context_Info  => "signum=" & signum'image);
        begin
          Clair.Errno.raise_from_errno (errno_code, error_msg);
          return False; -- Unreachable
        end;
    end if;
  end is_member;

  procedure block (sig_set : aliased in Set;
                   oset    : out Set) is
    local_oset : aliased Set;
    retval     : Interfaces.C.int;
  begin
    retval := c_pthread_sigmask (Clair.Platform.SIG_BLOCK,
                                 sig_set'address,
                                 local_oset'address);

    -- Pthread functions return non-zero on error (usually a positive error
    -- code).
    if retval /= 0 then
      declare
        -- Use retval directly as the error code; do not use get_errno.
        errno_code : constant Interfaces.C.int := retval;

        error_msg  : constant String :=
          Clair.Errno.format_posix_error_message
            (errno_code    => errno_code,
             Function_Name => "Clair.Signal.block (pthread_sigmask)",
             Context_Info  => "how=SIG_BLOCK");
      begin
        Clair.Errno.raise_from_errno (errno_code, error_msg);
      end;
    end if;

    oset := local_oset;
  end block;

  procedure unblock (sig_set : aliased in Set;
                     oset    : out Set) is
    local_oset : aliased Set;
    retval     : Interfaces.C.int;
  begin
    retval := c_pthread_sigmask (Clair.Platform.SIG_UNBLOCK,
                                sig_set'address,
                                local_oset'address);

    -- Pthread functions return non-zero on error (usually a positive error
    -- code).
    if retval /= 0 then
      declare
          -- Use retval directly as the error code; do not use get_errno.
          errno_code : constant Interfaces.C.int := retval;
          error_msg  : constant String :=
            Clair.Errno.format_posix_error_message
              (errno_code    => errno_code,
              Function_Name => "Clair.Signal.unblock (pthread_sigmask)",
              Context_Info  => "how=SIG_UNBLOCK");
      begin
          Clair.Errno.raise_from_errno (errno_code, error_msg);
      end;
    end if;

    oset := local_oset;
  end unblock;

  procedure unblock (sig_set : aliased in Set) is
    retval : Interfaces.C.int;
  begin
    retval := c_pthread_sigmask (Clair.Platform.SIG_UNBLOCK,
                                 sig_set'address,
                                 System.NULL_ADDRESS);

    -- Pthread functions return non-zero on error (usually a positive error
    -- code).
    if retval /= 0 then
      declare
          -- Use retval directly as the error code; do not use get_errno.
          errno_code : constant Interfaces.C.int := retval;

          error_msg  : constant String :=
            Clair.Errno.format_posix_error_message
              (errno_code    => errno_code,
              Function_Name => "Clair.Signal.unblock (pthread_sigmask)",
              Context_Info  => "how=SIG_UNBLOCK");
      begin
          Clair.Errno.raise_from_errno (errno_code, error_msg);
      end;
    end if;

  end unblock;

  procedure set_mask (New_Set : aliased in Set) is
    retval : Interfaces.C.int;
  begin
    retval := c_pthread_sigmask (Clair.Platform.SIG_SETMASK,
                                New_Set'address,
                                System.NULL_ADDRESS);
    if retval /= 0 then
      declare
          errno_code : constant Interfaces.C.int := retval;
          error_msg  : constant String :=
            Clair.Errno.format_posix_error_message
              (errno_code    => errno_code,
              Function_Name => "Clair.Signal.set_mask (pthread_sigmask)",
              Context_Info  => "how=SIG_SETMASK");
      begin
          Clair.Errno.raise_from_errno (errno_code, error_msg);
      end;
    end if;
  end set_mask;

  procedure set_wait (sig_set : aliased in Set;
                      signo   : out Number) is
    local_sig : aliased Number;
    -- sigwait returns 0 on success;
    -- otherwise, it returns the error number directly.
    retval : constant Interfaces.C.int
           := c_sigwait (sig_set'access, local_sig'access);
  begin
    if retval = 0 then
      signo := local_sig;
    else
      declare
        error_msg  : constant String :=
          Clair.Errno.format_posix_error_message
            (errno_code    => retval,
             function_name => "Clair.Signal.set_wait (sigwait)",
             context_info  => "Failed to wait for signal set");
      begin
        -- Note: We pass 'retval' directly
        -- because sigwait does not set the global errno.
        Clair.Errno.raise_from_errno (retval, error_msg);
      end;
    end if;
  end set_wait;

  -- Implementation of the raise(3) wrapper
  procedure send_signal (signo : Number) is
    retval : constant Interfaces.C.int
           := c_raise (Interfaces.C.int(signo));
  begin
    if retval /= 0 then
      declare
        errno_code : constant Interfaces.C.int := Clair.Errno.get_errno;
        error_msg  : constant String :=
          Clair.Errno.format_posix_error_message
            (errno_code    => errno_code,
             function_name => "Clair.Signal.send_signal",
             context_info  => "for signal " & signo'image);
      begin
        Clair.Errno.raise_from_errno (errno_code, error_msg);
      end;
    end if;
  end send_signal;

  procedure raise_sigaction_error (
    errno_code    : Interfaces.C.int;
    function_name : String;
    context_info  : String
  ) is
    error_msg : constant String :=
      Clair.Errno.format_posix_error_message
        (errno_code    => errno_code,
         function_name => function_name,
         context_info  => context_info);
  begin
    Clair.Errno.raise_from_errno (errno_code, error_msg);
  end raise_sigaction_error;

  procedure set_action (signo   : Number;
                        handler : Signal.Handler;
                        flags   : Signal.Flags := 0)
  is
    Act    : aliased Action;
    retval : Interfaces.C.int;
  begin
    -- Performs the equivalent operation to sigemptyset(&act.sa_mask) in C.
    set_empty (act.sa_mask);
    act.sa_handler := to_address (handler);
    act.sa_flags := Interfaces.C.int(flags);

    retval := c_sigaction (
      Sig  => Interfaces.C.int(signo),
      Act  => Act'Access,
      Oact => null
    );

    if retval = -1 then
      raise_sigaction_error (
        errno_code    => Clair.Errno.get_errno,
        function_name => "Clair.Signal.set_action",
        context_info  => "for signal " & signo'image
      );
    end if;
  end set_action;

  procedure suspend (mask : aliased in Set) is
    retval      : Interfaces.C.int;
    saved_errno : Interfaces.C.int;
  begin
    retval := c_sigsuspend (mask'access);

    saved_errno := Clair.Errno.get_errno;

    if retval /= -1 then
      raise Program_Error with
            "sigsuspend unexpectedly returned success (" & retval'image & ")";
    end if;

    if saved_errno /= Clair.Errno.EINTR then
      declare
        error_msg  : constant String :=
          Clair.Errno.format_posix_error_message
            (errno_code    => saved_errno,
             function_name => "Clair.Signal.suspend (sigsuspend)",
             context_info  => "Unexpected failure");
      begin
        raise Program_Error with error_msg;
      end;
    end if;
  end suspend;

  procedure pause is
    empty_mask : aliased Clair.Signal.Set;
  begin
    set_empty (empty_mask);
    suspend (empty_mask);
  end pause;

  function flags_to_uint is new Ada.Unchecked_Conversion
    (source => Flags, target => Interfaces.C.unsigned);

  function uint_to_flags is new Ada.Unchecked_Conversion
    (source => Interfaces.C.unsigned, target => Flags);

  function "or" (left, right : Flags) return Flags is
  begin
    return uint_to_flags (flags_to_uint(left) or flags_to_uint(right));
  end "or";

end Clair.Signal;
