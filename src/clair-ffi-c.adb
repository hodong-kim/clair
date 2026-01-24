-- clair-ffi-c.adb
-- Copyright (c) 2025,2026 Hodong Kim <hodong@nimfsoft.com>
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
with Ada.Unchecked_Conversion;
with Clair.Errors;
with Clair.Signal;
with Clair.Event_Loop;
with Clair.File;

package body Clair.FFI.C is

  use type System.Address;
  use type Interfaces.C.int;

  -----------------------------------------------------------------------------
  -- [1] Type Conversion Utilities and Generic Instantiation
  -----------------------------------------------------------------------------

  type Event_Loop_Access is access all Clair.Event_Loop.Context;
  pragma convention (c, Event_Loop_Access);

  -- [Marshal] System.Address -> Event_Loop_Access
  function to_context_ptr is new Ada.Unchecked_Conversion
    (source => System.Address,
     target => Event_Loop_Access);

  function to_address is new Ada.Unchecked_Conversion
    (source => Event_Loop_Access, target => System.Address);

  -- [Handle] Conversions
  function to_address_from_handle is new Ada.Unchecked_Conversion
    (source => Clair.Event_Loop.Handle, target => System.Address);

  -- [Handle] System.Address -> Clair.Event_Loop.Handle (Input)
  function to_handle is new Ada.Unchecked_Conversion
    (source => System.Address,
     target => Clair.Event_Loop.Handle);

  -- [Binding] Generic Package Instantiation
  package Loop_Binding is new Clair.FFI.Bindings
    (Context_Type   => Clair.Event_Loop.Context,
     Context_Access => Event_Loop_Access,
     to_context     => to_context_ptr,
     Handle_Type    => Clair.Event_Loop.Handle,
     to_address     => to_address_from_handle);

  -----------------------------------------------------------------------------
  -- [2] Manual Implementation: create / destroy
  -----------------------------------------------------------------------------

  function clair_event_loop_create return System.Address
    with export, convention => c, external_Name => "clair_event_loop_create";

  function clair_event_loop_create return System.Address is
    ptr : Clair.Event_Loop.Context_Access;
  begin
    ptr := Clair.Event_Loop.create;
    return to_address (Event_Loop_Access(ptr));
  exception
    when others =>
      return System.NULL_ADDRESS;
  end clair_event_loop_create;

  function destroy_action (ptr : Event_Loop_Access) return Interfaces.C.int is
    temp_ptr : Clair.Event_Loop.Context_Access
             := Clair.Event_Loop.Context_Access(ptr);
  begin
    Clair.Event_Loop.destroy (temp_ptr);

    return Clair.Errors.No_Error;
  end destroy_action;
  pragma inline (destroy_action);

  function clair_event_loop_destroy is new Loop_Binding.bind_destructor
    (action => destroy_action)
  with export, convention => c, external_name => "clair_event_loop_destroy";

  -----------------------------------------------------------------------------
  -- [3] initialize and finalize
  -----------------------------------------------------------------------------

  function initialize_action (ctx : in out Clair.Event_Loop.Context)
  return Interfaces.C.int is
  begin
    Clair.Event_Loop.initialize (ctx);
    return Clair.Errors.No_Error;
  end initialize_action;
  pragma inline (initialize_action);

  function clair_event_loop_initialize is new Loop_Binding.bind_int_1
    (action => initialize_action)
  with export, convention => c, external_Name => "clair_event_loop_initialize";

  function finalize_action (ctx : in out Clair.Event_Loop.Context)
  return Interfaces.C.int is
  begin
    Clair.Event_Loop.finalize (ctx);
    return Clair.Errors.No_Error;
  end finalize_action;
  pragma inline (finalize_action);

  function clair_event_loop_finalize is new Loop_Binding.bind_int_1
    (action => finalize_action)
  with export, convention => c, external_Name => "clair_event_loop_finalize";

  -----------------------------------------------------------------------------
  -- [4] Basic Operations (run / quit / iterate / depth)
  -----------------------------------------------------------------------------

  function run_action (ctx : in out Clair.Event_Loop.Context)
  return Interfaces.C.int is
  begin
    Clair.Event_Loop.run (ctx);
    return Clair.Errors.No_Error;
  end run_action;
  pragma inline (run_action);

  function clair_event_loop_run is new Loop_Binding.bind_int_1
    (action => run_action)
  with export, convention => c, external_Name => "clair_event_loop_run";

  function quit_action (ctx : in out Clair.Event_Loop.Context)
  return Interfaces.C.int is
  begin
    Clair.Event_Loop.quit (ctx);
    return Clair.Errors.No_Error;
  end quit_action;
  pragma inline (quit_action);

  function clair_event_loop_quit is new Loop_Binding.bind_int_1
    (action => quit_action)
  with export, convention => c, external_Name => "clair_event_loop_quit";

  function iterate_action (ctx     : in out Clair.Event_Loop.Context;
                           timeout : Clair.Event_Loop.Milliseconds)
  return Interfaces.C.int is
  begin
    return Boolean'pos(Clair.Event_Loop.iterate (ctx, timeout));
  end iterate_action;
  pragma inline (iterate_action);

  function clair_event_loop_iterate is new Loop_Binding.bind_int_2
    (Arg1_Type => Clair.Event_Loop.Milliseconds,
     action    => iterate_action)
  with export, convention => c, external_Name => "clair_event_loop_iterate";

  function get_depth_action (ctx : in out Clair.Event_Loop.Context)
  return Interfaces.C.int is
  begin
    return Interfaces.C.int(Clair.Event_Loop.get_depth (ctx));
  end get_depth_action;
  pragma inline (get_depth_action);

  function clair_event_loop_get_depth is new Loop_Binding.bind_int_1
    (action => get_depth_action)
  with export, convention => c, external_Name => "clair_event_loop_get_depth";

  -----------------------------------------------------------------------------
  -- [5] Resource Management (remove / modify)
  -----------------------------------------------------------------------------

  function remove_action (ctx  : in out Clair.Event_Loop.Context;
                          item : System.Address) return Interfaces.C.int is
  begin
    if item = System.Null_Address then
      return Clair.Errors.No_Error;
    end if;

    Clair.Event_Loop.remove (ctx, to_handle (item));
    return Clair.Errors.No_Error;
  end remove_action;
  pragma inline (remove_action);

  function clair_event_loop_remove is new Loop_Binding.bind_int_2
    (Arg1_Type => System.Address,
     action    => remove_action)
  with export, convention => c, external_Name => "clair_event_loop_remove";

  function modify_action (ctx    : in out Clair.Event_Loop.Context;
                          watch  : System.Address;
                          events : Interfaces.C.unsigned)
  return Interfaces.C.int is
  begin
    Clair.Event_Loop.modify_watch (ctx,
                                   to_handle (watch),
                                   Clair.Event_Loop.Event_Mask(events));
    return Clair.Errors.No_Error;
  end modify_action;
  pragma inline (modify_action);

  function clair_event_loop_modify_watch is new Loop_Binding.bind_int_3
    (Arg1_Type => System.Address,
     Arg2_Type => Interfaces.C.unsigned,
     action    => modify_action)
  with export,
       convention => c,
       external_name => "clair_event_loop_modify_watch";

  function add_watch_action (ctx       : in out Clair.Event_Loop.Context;
                             fd        : Clair.File.Descriptor;
                             events    : Clair.Event_Loop.Event_Mask;
                             callback  : Clair.Event_Loop.IO_Callback;
                             user_data : System.Address)
  return Clair.Event_Loop.Handle is
  begin
    return Clair.Event_Loop.add_watch (ctx, fd, events, callback, user_data);
  end add_watch_action;
  pragma inline (add_watch_action);

  function clair_event_loop_add_watch is new Loop_Binding.bind_handle_5
    (Arg1_Type => Clair.File.Descriptor,
     Arg2_Type => Clair.Event_Loop.Event_Mask,
     Arg3_Type => Clair.Event_Loop.IO_Callback,
     Arg4_Type => System.Address,
     action    => add_watch_action)
  with export, convention => c, external_Name => "clair_event_loop_add_watch";

  function add_timer_action (ctx       : in out Clair.Event_Loop.Context;
                             interval  : Clair.Event_Loop.Milliseconds;
                             callback  : Clair.Event_Loop.Timer_Callback;
                             one_shot  : Interfaces.C.C_bool;
                             user_data : System.Address)
  return Clair.Event_Loop.Handle is
  begin
    return Clair.Event_Loop.add_timer (ctx,
                                       interval,
                                       callback,
                                       Boolean(one_shot),
                                       user_data);
  end add_timer_action;
  pragma inline (add_timer_action);

  function clair_event_loop_add_timer is new Loop_Binding.bind_handle_5
    (Arg1_Type => Clair.Event_Loop.Milliseconds,
     Arg2_Type => Clair.Event_Loop.Timer_Callback,
     Arg3_Type => Interfaces.C.C_bool,
     Arg4_Type => System.Address,
     action    => add_timer_action)
  with export, convention => c, external_Name => "clair_event_loop_add_timer";

  function add_signal_action (ctx       : in out Clair.Event_Loop.Context;
                              signal_no : Interfaces.C.int;
                              callback  : Clair.Event_Loop.Signal_Callback;
                              user_data : System.Address)
  return Clair.Event_Loop.Handle is
  begin
    return Clair.Event_Loop.add_unix_signal (ctx,
                                             Clair.Signal.Number(signal_no),
                                             callback,
                                             user_data);
  end add_signal_action;
  pragma inline (add_signal_action);

  function clair_event_loop_add_unix_signal is new Loop_Binding.bind_handle_4
    (Arg1_Type => Interfaces.C.int,
     Arg2_Type => Clair.Event_Loop.Signal_Callback,
     Arg3_Type => System.Address,
     action    => add_signal_action)
  with export,
       convention => c,
       external_name => "clair_event_loop_add_unix_signal";

  function add_idle_action (ctx       : in out Clair.Event_Loop.Context;
                            callback  : Clair.Event_Loop.Idle_Callback;
                            user_data : System.Address)
  return Clair.Event_Loop.Handle is
  begin
    return Clair.Event_Loop.add_idle (ctx, callback, user_data);
  end add_idle_action;
  pragma inline (add_idle_action);

  function clair_event_loop_add_idle is new Loop_Binding.bind_handle_3
    (Arg1_Type => Clair.Event_Loop.Idle_Callback,
     Arg2_Type => System.Address,
     action    => add_idle_action)
  with export, convention => c, external_Name => "clair_event_loop_add_idle";

end Clair.FFI.C;
