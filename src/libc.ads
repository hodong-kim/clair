-- libc.ads
with Interfaces.C;

package Libc is
    Libc_Error : exception;

    procedure raise_signal (sig : in Interfaces.C.int);

end Libc;
