-- clair-errors.ads
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
package Clair.Errors is
  pragma preelaborate;

  -- Error codes corresponding to errno
  No_Error                                  : constant :=  0;
  Operation_Not_Permitted                   : constant := -1;
  No_Such_File_Or_Directory                 : constant := -2;
  No_Such_Process                           : constant := -3;
  Interrupted_System_Call                   : constant := -4;
  Input_Output_Error                        : constant := -5;
  Device_Not_Configured                     : constant := -6;
  Argument_List_Too_Long                    : constant := -7;
  Exec_Format_Error                         : constant := -8;
  Bad_File_Descriptor                       : constant := -9;
  No_Child_Processes                        : constant := -10;
  Resource_Deadlock_Avoided                 : constant := -11;
  Cannot_Allocate_Memory                    : constant := -12;
  Permission_Denied                         : constant := -13;
  Bad_Address                               : constant := -14;
  Block_Device_Required                     : constant := -15;
  Device_Busy                               : constant := -16;
  File_Exists                               : constant := -17;
  Cross_Device_Link                         : constant := -18;
  Operation_Not_Supported_By_Device         : constant := -19;
  Not_A_Directory                           : constant := -20;
  Is_A_Directory                            : constant := -21;
  Invalid_Argument                          : constant := -22;
  Too_Many_Open_Files_In_System             : constant := -23;
  Too_Many_Open_Files                       : constant := -24;
  Inappropriate_Ioctl_For_Device            : constant := -25;
  Text_File_Busy                            : constant := -26;
  File_Too_Large                            : constant := -27;
  No_Space_Left_On_Device                   : constant := -28;
  Illegal_Seek                              : constant := -29;
  Read_Only_File_System                     : constant := -30;
  Too_Many_Links                            : constant := -31;
  Broken_Pipe                               : constant := -32;
  Numerical_Argument_Out_Of_Domain          : constant := -33;
  Result_Too_Large                          : constant := -34;
  Resource_Temporarily_Unavailable          : constant := -35;
  Operation_Would_Block                     : constant := -36;
  Operation_Now_In_Progress                 : constant := -37;
  Operation_Already_In_Progress             : constant := -38;
  Socket_Operation_On_Non_Socket            : constant := -39;
  Destination_Address_Required              : constant := -40;
  Message_Too_Long                          : constant := -41;
  Protocol_Wrong_Type_For_Socket            : constant := -42;
  Protocol_Not_Available                    : constant := -43;
  Protocol_Not_Supported                    : constant := -44;
  Socket_Type_Not_Supported                 : constant := -45;
  Operation_Not_Supported                   : constant := -46;
  Protocol_Family_Not_Supported             : constant := -47;
  Address_Family_Not_Supported              : constant := -48;
  Address_Already_In_Use                    : constant := -49;
  Cannot_Assign_Requested_Address           : constant := -50;
  Network_Is_Down                           : constant := -51;
  Network_Is_Unreachable                    : constant := -52;
  Network_Dropped_Connection_On_Reset       : constant := -53;
  Software_Caused_Connection_Abort          : constant := -54;
  Connection_Reset_By_Peer                  : constant := -55;
  No_Buffer_Space_Available                 : constant := -56;
  Socket_Is_Already_Connected               : constant := -57;
  Socket_Is_Not_Connected                   : constant := -58;
  Cannot_Send_After_Socket_Shutdown         : constant := -59;
  Too_Many_References                       : constant := -60;
  Operation_Timed_Out                       : constant := -61;
  Connection_Refused                        : constant := -62;
  Too_Many_Levels_Of_Symbolic_Links         : constant := -63;
  File_Name_Too_Long                        : constant := -64;
  Host_Is_Down                              : constant := -65;
  No_Route_To_Host                          : constant := -66;
  Directory_Not_Empty                       : constant := -67;
  Too_Many_Processes                        : constant := -68;
  Too_Many_Users                            : constant := -69;
  Disc_Quota_Exceeded                       : constant := -70;
  Stale_Nfs_File_Handle                     : constant := -71;
  Too_Many_Levels_Of_Remote_In_Path         : constant := -72;
  RPC_Struct_Is_Bad                         : constant := -73;
  RPC_Version_Wrong                         : constant := -74;
  RPC_Prog_Not_Avail                        : constant := -75;
  Program_Version_Wrong                     : constant := -76;
  Bad_Procedure_For_Program                 : constant := -77;
  No_Locks_Available                        : constant := -78;
  Function_Not_Implemented                  : constant := -79;
  Inappropriate_File_Type_Or_Format         : constant := -80;
  Authentication_Error                      : constant := -81;
  Need_Authenticator                        : constant := -82;
  Identifier_Removed                        : constant := -83;
  No_Message_Of_Desired_Type                : constant := -84;
  Value_Too_Large_To_Be_Stored_In_Data_Type : constant := -85;
  Operation_Canceled                        : constant := -86;
  Illegal_Byte_Sequence                     : constant := -87;
  Attribute_Not_Found                       : constant := -88;
  Programming_Error                         : constant := -89;
  Bad_Message                               : constant := -90;
  Multihop_Attempted                        : constant := -91;
  Link_Has_Been_Severed                     : constant := -92;
  Protocol_Error                            : constant := -93;
  Capabilities_Insufficient                 : constant := -94;
  Not_Permitted_In_Capability_Mode          : constant := -95;
  State_Not_Recoverable                     : constant := -96;
  Previous_Owner_Died                       : constant := -97;
  Integrity_Check_Failed                    : constant := -98;
  -- Mapping of Clair.DL exceptions
  Library_Load_Error                        : constant := -201;
  Library_Close_Error                       : constant := -202;
  Symbol_Lookup_Error                       : constant := -203;
  -- Mapping of Ada predefined exceptions to error codes
  Constraint_Error                          : constant := -301;
  Program_Error                             : constant := -302;
  Storage_Error                             : constant := -303;
  Tasking_Error                             : constant := -304;
  -- Unmapped Error
  Unmapped_Error                            : constant := -999;

end Clair.Errors;