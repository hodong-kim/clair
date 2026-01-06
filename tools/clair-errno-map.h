/*
 * clair-errno-map.h
 * Copyright (C) 2025 Hodong Kim <hodong@nimfsoft.art>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#ifndef CLAIR_ERRNO_MAP
#define CLAIR_ERRNO_MAP

#include <errno.h>

struct _Errno_Map {
  int   value;
  char* name;
  char* exp;
};

typedef struct _Errno_Map Errno_Map;

Errno_Map errno_map[] = {
  { EPERM, "EPERM", "Operation_Not_Permitted" },
  { ENOENT, "ENOENT", "No_Such_File_Or_Directory" },
  { ESRCH, "ESRCH", "No_Such_Process" },
  { EINTR, "EINTR", "Interrupted_System_Call" },
  { EIO, "EIO", "Input_Output_Error" },
  { ENXIO, "ENXIO", "Device_Not_Configured" },
  { E2BIG, "E2BIG", "Argument_List_Too_Long" },
  { ENOEXEC, "ENOEXEC", "Exec_Format_Error" },
  { EBADF, "EBADF", "Bad_File_Descriptor" },
  { ECHILD, "ECHILD", "No_Child_Processes" },
  { EDEADLK, "EDEADLK", "Resource_Deadlock_Avoided" },
  { ENOMEM, "ENOMEM", "Cannot_Allocate_Memory" },
  { EACCES, "EACCES", "Permission_Denied" },
  { EFAULT, "EFAULT", "Bad_Address" },
  { ENOTBLK, "ENOTBLK", "Block_Device_Required" },
  { EBUSY, "EBUSY", "Device_Busy" },
  { EEXIST, "EEXIST", "File_Exists" },
  { EXDEV, "EXDEV", "Cross_Device_Link" },
  { ENODEV, "ENODEV", "Operation_Not_Supported_By_Device" },
  { ENOTDIR, "ENOTDIR", "Not_A_Directory" },
  { EISDIR, "EISDIR", "Is_A_Directory" },
  { EINVAL, "EINVAL", "Invalid_Argument" },
  { ENFILE, "ENFILE", "Too_Many_Open_Files_In_System" },
  { EMFILE, "EMFILE", "Too_Many_Open_Files" },
  { ENOTTY, "ENOTTY", "Inappropriate_Ioctl_For_Device" },
  { ETXTBSY, "ETXTBSY", "Text_File_Busy" },
  { EFBIG, "EFBIG", "File_Too_Large" },
  { ENOSPC, "ENOSPC", "No_Space_Left_On_Device" },
  { ESPIPE, "ESPIPE", "Illegal_Seek" },
  { EROFS, "EROFS", "Read_Only_File_System" },
  { EMLINK, "EMLINK", "Too_Many_Links" },
  { EPIPE, "EPIPE", "Broken_Pipe" },
  { EDOM, "EDOM", "Numerical_Argument_Out_Of_Domain" },
  { ERANGE, "ERANGE", "Result_Too_Large" },
  { EAGAIN, "EAGAIN", "Resource_Temporarily_Unavailable" },
  { EWOULDBLOCK, "EWOULDBLOCK", "Operation_Would_Block" }, // EAGAIN
  { EINPROGRESS, "EINPROGRESS", "Operation_Now_In_Progress" },
  { EALREADY, "EALREADY", "Operation_Already_In_Progress" },
  { ENOTSOCK, "ENOTSOCK", "Socket_Operation_On_Non_Socket" },
  { EDESTADDRREQ, "EDESTADDRREQ", "Destination_Address_Required" },
  { EMSGSIZE, "EMSGSIZE", "Message_Too_Long" },
  { EPROTOTYPE, "EPROTOTYPE", "Protocol_Wrong_Type_For_Socket" },
  { ENOPROTOOPT, "ENOPROTOOPT", "Protocol_Not_Available" },
  { EPROTONOSUPPORT, "EPROTONOSUPPORT", "Protocol_Not_Supported" },
  { ESOCKTNOSUPPORT, "ESOCKTNOSUPPORT", "Socket_Type_Not_Supported" },
  { EOPNOTSUPP, "EOPNOTSUPP", "Operation_Not_Supported" },
  { ENOTSUP, "ENOTSUP", "Operation_Not_Supported" }, // EOPNOTSUPP
  { EPFNOSUPPORT, "EPFNOSUPPORT", "Protocol_Family_Not_Supported" },
  { EAFNOSUPPORT, "EAFNOSUPPORT", "Address_Family_Not_Supported" },
  { EADDRINUSE, "EADDRINUSE", "Address_Already_In_Use" },
  { EADDRNOTAVAIL, "EADDRNOTAVAIL", "Cannot_Assign_Requested_Address" },
  { ENETDOWN, "ENETDOWN", "Network_Is_Down" },
  { ENETUNREACH, "ENETUNREACH", "Network_Is_Unreachable" },
  { ENETRESET, "ENETRESET", "Network_Dropped_Connection_On_Reset" },
  { ECONNABORTED, "ECONNABORTED", "Software_Caused_Connection_Abort" },
  { ECONNRESET, "ECONNRESET", "Connection_Reset_By_Peer" },
  { ENOBUFS, "ENOBUFS", "No_Buffer_Space_Available" },
  { EISCONN, "EISCONN", "Socket_Is_Already_Connected" },
  { ENOTCONN, "ENOTCONN", "Socket_Is_Not_Connected" },
  { ESHUTDOWN, "ESHUTDOWN", "Cannot_Send_After_Socket_Shutdown" },
  { ETOOMANYREFS, "ETOOMANYREFS", "Too_Many_References" },
  { ETIMEDOUT, "ETIMEDOUT", "Operation_Timed_Out" },
  { ECONNREFUSED, "ECONNREFUSED", "Connection_Refused" },
  { ELOOP, "ELOOP", "Too_Many_Levels_Of_Symbolic_Links" },
  { ENAMETOOLONG, "ENAMETOOLONG", "File_Name_Too_Long" },
  { EHOSTDOWN, "EHOSTDOWN", "Host_Is_Down" },
  { EHOSTUNREACH, "EHOSTUNREACH", "No_Route_To_Host" },
  { ENOTEMPTY, "ENOTEMPTY", "Directory_Not_Empty" },
  { EPROCLIM, "EPROCLIM", "Too_Many_Processes" },
  { EUSERS, "EUSERS", "Too_Many_Users" },
  { EDQUOT, "EDQUOT", "Disc_Quota_Exceeded" },
  { ESTALE, "ESTALE", "Stale_Nfs_File_Handle" },
  { EREMOTE, "EREMOTE", "Too_Many_Levels_Of_Remote_In_Path" },
  { EBADRPC, "EBADRPC", "RPC_Struct_Is_Bad" },
  { ERPCMISMATCH, "ERPCMISMATCH", "RPC_Version_Wrong" },
  { EPROGUNAVAIL, "EPROGUNAVAIL", "RPC_Prog_Not_Avail" },
  { EPROGMISMATCH, "EPROGMISMATCH", "Program_Version_Wrong" },
  { EPROCUNAVAIL, "EPROCUNAVAIL", "Bad_Procedure_For_Program" },
  { ENOLCK, "ENOLCK", "No_Locks_Available" },
  { ENOSYS, "ENOSYS", "Function_Not_Implemented" },
  { EFTYPE, "EFTYPE", "Inappropriate_File_Type_Or_Format" },
  { EAUTH, "EAUTH", "Authentication_Error" },
  { ENEEDAUTH, "ENEEDAUTH", "Need_Authenticator" },
  { EIDRM, "EIDRM", "Identifier_Removed" },
  { ENOMSG, "ENOMSG", "No_Message_Of_Desired_Type" },
  { EOVERFLOW, "EOVERFLOW", "Value_Too_Large_To_Be_Stored_In_Data_Type" },
  { ECANCELED, "ECANCELED", "Operation_Canceled" },
  { EILSEQ, "EILSEQ", "Illegal_Byte_Sequence" },
  { ENOATTR, "ENOATTR", "Attribute_Not_Found" },
  { EDOOFUS, "EDOOFUS", "Programming_Error" },
  { EBADMSG, "EBADMSG", "Bad_Message" },
  { EMULTIHOP, "EMULTIHOP", "Multihop_Attempted" },
  { ENOLINK, "ENOLINK", "Link_Has_Been_Severed" },
  { EPROTO, "EPROTO", "Protocol_Error" },
  { ENOTCAPABLE, "ENOTCAPABLE", "Capabilities_Insufficient" },
  { ECAPMODE, "ECAPMODE", "Not_Permitted_In_Capability_Mode" },
  { ENOTRECOVERABLE, "ENOTRECOVERABLE", "State_Not_Recoverable" },
  { EOWNERDEAD, "EOWNERDEAD", "Previous_Owner_Died" },
  { EINTEGRITY, "EINTEGRITY", "Integrity_Check_Failed" }
};

#endif // CLAIR_ERRNO_MAP