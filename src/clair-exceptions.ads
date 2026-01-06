-- clair-exceptions.ads
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
package Clair.Exceptions is
  pragma preelaborate;
  --
  -- System Error to Ada Exception Mapping
  --
  -- Provides exceptions for common errno values found on major operating
  -- systems. This includes standardized POSIX errors and various
  -- platform-specific extensions (e.g., from FreeBSD, Linux).
  --
  -- The exception names are derived from the descriptions in the
  -- strerror(3) and errno(2) man pages.
  --
  -- NOTE: This list is not exhaustive. The presence of a specific error
  --       is not guaranteed on all systems. POSIX-defined errors are
  --       generally portable, but platform-specific ones are not.
  --
  Operation_Not_Permitted                   : exception; --  1 EPERM
  No_Such_File_Or_Directory                 : exception; --  2 ENOENT
  No_Such_Process                           : exception; --  3 ESRCH
  Interrupted_System_Call                   : exception; --  4 EINTR
  Input_Output_Error                        : exception; --  5 EIO
  Device_Not_Configured                     : exception; --  6 ENXIO
  Argument_List_Too_Long                    : exception; --  7 E2BIG
  Exec_Format_Error                         : exception; --  8 ENOEXEC
  Bad_File_Descriptor                       : exception; --  9 EBADF
  No_Child_Processes                        : exception; -- 10 ECHILD
  Resource_Deadlock_Avoided                 : exception; -- 11 EDEADLK
  Cannot_Allocate_Memory                    : exception; -- 12 ENOMEM
  Permission_Denied                         : exception; -- 13 EACCES
  Bad_Address                               : exception; -- 14 EFAULT
  Block_Device_Required                     : exception; -- 15 ENOTBLK
  Device_Busy                               : exception; -- 16 EBUSY
  File_Exists                               : exception; -- 17 EEXIST
  Cross_Device_Link                         : exception; -- 18 EXDEV
  Operation_Not_Supported_By_Device         : exception; -- 19 ENODEV
  Not_A_Directory                           : exception; -- 20 ENOTDIR
  Is_A_Directory                            : exception; -- 21 EISDIR
  Invalid_Argument                          : exception; -- 22 EINVAL
  Too_Many_Open_Files_In_System             : exception; -- 23 ENFILE
  Too_Many_Open_Files                       : exception; -- 24 EMFILE
  Inappropriate_Ioctl_For_Device            : exception; -- 25 ENOTTY
  Text_File_Busy                            : exception; -- 26 ETXTBSY
  File_Too_Large                            : exception; -- 27 EFBIG
  No_Space_Left_On_Device                   : exception; -- 28 ENOSPC
  Illegal_Seek                              : exception; -- 29 ESPIPE
  Read_Only_File_System                     : exception; -- 30 EROFS
  Too_Many_Links                            : exception; -- 31 EMLINK
  Broken_Pipe                               : exception; -- 32 EPIPE
  Numerical_Argument_Out_Of_Domain          : exception; -- 33 EDOM
  Result_Too_Large                          : exception; -- 34 ERANGE
  Resource_Temporarily_Unavailable          : exception; -- 35 EAGAIN
  Operation_Would_Block                     : exception; -- 35 EWOULDBLOCK
  Operation_Now_In_Progress                 : exception; -- 36 EINPROGRESS
  Operation_Already_In_Progress             : exception; -- 37 EALREADY
  Socket_Operation_On_Non_Socket            : exception; -- 38 ENOTSOCK
  Destination_Address_Required              : exception; -- 39 EDESTADDRREQ
  Message_Too_Long                          : exception; -- 40 EMSGSIZE
  Protocol_Wrong_Type_For_Socket            : exception; -- 41 EPROTOTYPE
  Protocol_Not_Available                    : exception; -- 42 ENOPROTOOPT
  Protocol_Not_Supported                    : exception; -- 43 EPROTONOSUPPORT
  Socket_Type_Not_Supported                 : exception; -- 44 ESOCKTNOSUPPORT
  Operation_Not_Supported                   : exception; -- 45 ENOTSUP
  Protocol_Family_Not_Supported             : exception; -- 46 EPFNOSUPPORT
  Address_Family_Not_Supported              : exception; -- 47 EAFNOSUPPORT
  Address_Already_In_Use                    : exception; -- 48 EADDRINUSE
  Cannot_Assign_Requested_Address           : exception; -- 49 EADDRNOTAVAIL
  Network_Is_Down                           : exception; -- 50 ENETDOWN
  Network_Is_Unreachable                    : exception; -- 51 ENETUNREACH
  Network_Dropped_Connection_On_Reset       : exception; -- 52 ENETRESET
  Software_Caused_Connection_Abort          : exception; -- 53 ECONNABORTED
  Connection_Reset_By_Peer                  : exception; -- 54 ECONNRESET
  No_Buffer_Space_Available                 : exception; -- 55 ENOBUFS
  Socket_Is_Already_Connected               : exception; -- 56 EISCONN
  Socket_Is_Not_Connected                   : exception; -- 57 ENOTCONN
  Cannot_Send_After_Socket_Shutdown         : exception; -- 58 ESHUTDOWN
  Too_Many_References                       : exception; -- 59 ETOOMANYREFS
  Operation_Timed_Out                       : exception; -- 60 ETIMEDOUT
  Connection_Refused                        : exception; -- 61 ECONNREFUSED
  Too_Many_Levels_Of_Symbolic_Links         : exception; -- 62 ELOOP
  File_Name_Too_Long                        : exception; -- 63 ENAMETOOLONG
  Host_Is_Down                              : exception; -- 64 EHOSTDOWN
  No_Route_To_Host                          : exception; -- 65 EHOSTUNREACH
  Directory_Not_Empty                       : exception; -- 66 ENOTEMPTY
  Too_Many_Processes                        : exception; -- 67 EPROCLIM
  Too_Many_Users                            : exception; -- 68 EUSERS
  Disc_Quota_Exceeded                       : exception; -- 69 EDQUOT
  Stale_Nfs_File_Handle                     : exception; -- 70 ESTALE
  Too_Many_Levels_Of_Remote_In_Path         : exception; -- 71 EREMOTE
  RPC_Struct_Is_Bad                         : exception; -- 72 EBADRPC
  RPC_Version_Wrong                         : exception; -- 73 ERPCMISMATCH
  RPC_Prog_Not_Avail                        : exception; -- 74 EPROGUNAVAIL
  Program_Version_Wrong                     : exception; -- 75 EPROGMISMATCH
  Bad_Procedure_For_Program                 : exception; -- 76 EPROCUNAVAIL
  No_Locks_Available                        : exception; -- 77 ENOLCK
  Function_Not_Implemented                  : exception; -- 78 ENOSYS
  Inappropriate_File_Type_Or_Format         : exception; -- 79 EFTYPE
  Authentication_Error                      : exception; -- 80 EAUTH
  Need_Authenticator                        : exception; -- 81 ENEEDAUTH
  Identifier_Removed                        : exception; -- 82 EIDRM
  No_Message_Of_Desired_Type                : exception; -- 83 ENOMSG
  Value_Too_Large_To_Be_Stored_In_Data_Type : exception; -- 84 EOVERFLOW
  Operation_Canceled                        : exception; -- 85 ECANCELED
  Illegal_Byte_Sequence                     : exception; -- 86 EILSEQ
  Attribute_Not_Found                       : exception; -- 87 ENOATTR
  Programming_Error                         : exception; -- 88 EDOOFUS
  Bad_Message                               : exception; -- 89 EBADMSG
  Multihop_Attempted                        : exception; -- 90 EMULTIHOP
  Link_Has_Been_Severed                     : exception; -- 91 ENOLINK
  Protocol_Error                            : exception; -- 92 EPROTO
  Capabilities_Insufficient                 : exception; -- 93 ENOTCAPABLE
  Not_Permitted_In_Capability_Mode          : exception; -- 94 ECAPMODE
  State_Not_Recoverable                     : exception; -- 95 ENOTRECOVERABLE
  Previous_Owner_Died                       : exception; -- 96 EOWNERDEAD
  Integrity_Check_Failed                    : exception; -- 97 EINTEGRITY
  -- Dynamic library related exceptions
  Library_Load_Error  : exception;
  Library_Close_Error : exception;
  Symbol_Lookup_Error : exception;
  -- For errno values not explicitly mapped to a specific exception.
  Unmapped_Error : exception;

end Clair.Exceptions;
