-- libc.adb
with Interfaces.C;

use Interfaces.C;

package body Libc is

    function c_raise (sig : in int) return int;
    pragma Import (C, c_raise, "raise");

    procedure raise_signal (sig : in int) is
       result : constant int := c_raise (sig);
    begin
       if result /= 0 then
          raise Libc_Error with "raise(3) failed";
       end if;
    end raise_signal;

end Libc;
