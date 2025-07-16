## Cada API Reference Manual

This document provides an API reference for the modules within the `cada` library. The library offers Ada bindings for common C and POSIX APIs, aiming to wrap low-level system calls in a type-safe, idiomatic Ada interface with proper error handling.

### Table of Contents
1.  **Package `Dl`**: Dynamic Library Loading
2.  **Package `Libc`**: Standard C Library Functions
3.  **Package `POSIX`**: POSIX APIs
    * `POSIX.Process`
    * `POSIX.File_Control`
    * `POSIX.Signal`

---

### 1. Package `Dl`

Provides functionality for dynamically loading and interacting with shared libraries (e.g., `.so`, `.dll`) at runtime. It includes wrappers for the `dlopen`, `dlsym`, and `dlclose` functions.

#### 1.1 Exceptions

* `Library_Load_Error`: Raised when a library fails to load (`dlopen`).
* `Library_Close_Error`: Raised when a library fails to unload (`dlclose`).
* `Symbol_Lookup_Error`: Raised when looking up a symbol's address (`dlsym`) fails.

#### 1.2 Constants

These are flags used for the `mode` parameter in the `open` function.

* `RTLD_LAZY : constant Integer := 1;`
* `RTLD_NOW : constant Integer := 2;`
* `RTLD_LOCAL : constant Integer := 4;`
* `RTLD_GLOBAL : constant Integer := 8;`

#### 1.3 Functions and Procedures

* **`function open (path : in String; mode : in Integer) return System.Address;`**
    * **Description**: Loads the shared library specified by `path`.
    * **Parameters**:
        * `path`: The file path to the library.
        * `mode`: Flags that specify the loading behavior (e.g., `RTLD_LAZY`).
    * **Returns**: A handle to the library (`System.Address`) on success.
    * **Raises**: `Library_Load_Error` on failure.

* **`procedure close (handle : in System.Address);`**
    * **Description**: Unloads a previously loaded shared library.
    * **Parameters**:
        * `handle`: The library handle obtained from the `open` function.
    * **Raises**: `Library_Close_Error` on failure.

* **`function get_symbol (handle : in System.Address; sym_name : in String) return System.Address;`**
    * **Description**: Finds the address of a symbol (function or variable) within a library.
    * **Parameters**:
        * `handle`: The library handle.
        * `sym_name`: The name of the symbol to look up.
    * **Returns**: The memory address of the symbol.
    * **Raises**: `Symbol_Lookup_Error` if the symbol cannot be found.

---

### 2. Package `Libc`

Provides an interface to select functions from the standard C library.

#### 2.1 Exceptions

* `Libc_Error`: Raised on failure of a C library function call.

#### 2.2 Procedures

* **`procedure raise_signal (sig : in Interfaces.C.int);`**
    * **Description**: Calls the C library's `raise(3)` function to send a signal to the current process.
    * **Parameters**:
        * `sig`: The signal number to send (e.g., `POSIX.Signal.SIGUSR1`).
    * **Raises**: `Libc_Error` if the `raise(3)` call returns a non-zero value.

---

### 3. Package `POSIX`

This is a parent package that provides interfaces to the POSIX standard API.

### 3.1. `POSIX.Process`

Provides functions for process creation and control (`fork`, `setsid`, `getpid`).

#### 3.1.1 Types

* `Process_ID is new Interfaces.C.int;`: A process ID type corresponding to C's `pid_t`.

#### 3.1.2 Exceptions

* `Fork_Error`: Raised when a `fork(2)` or `_Fork(2)` call fails.
* `Set_SID_Error`: Raised when a `setsid(2)` call fails.

#### 3.1.3 Functions and Procedures

* **`function Fork return Process_ID;`**
    * **Description**: Creates a new child process by duplicating the current one. On success, returns the child's PID to the parent and 0 to the child.
    * **Raises**: `Fork_Error` if process creation fails.

* **`function Underscore_Fork return Process_ID;`**
    * **Description**: Similar to `fork`, but is an async-signal-safe version that can be called safely from within a signal handler.
    * **Raises**: `Fork_Error` if process creation fails.

* **`procedure Set_SID;`**
    * **Description**: Creates a new session. The calling process becomes the session leader and is detached from its controlling terminal. This should typically be called in a child process after a `fork`.
    * **Raises**: `Set_SID_Error` on failure (e.g., if the process is already a process group leader).

* **`function getpid return Process_ID;`**
    * **Description**: Returns the ID of the current process.

### 3.2. `POSIX.File_Control`

Provides an interface to POSIX functions for file descriptor control, such as `open`, `close`, `write`, `flock`, and `fcntl`.

#### 3.2.1 Types

* `File_Descriptor is new Interfaces.C.int;`
* `Mode is new Interfaces.C.unsigned;` (For the mode in `open`)
* `Flags is new Interfaces.C.int;` (For file status flags)
* `C_Offset is new Interfaces.C.long;` (Corresponds to C's `off_t`)
* `Flock_Record`: A record corresponding to `struct flock` in C, used for `fcntl` commands like `F_GETLK` and `F_SETLK`.
    * `L_Start : C_Offset;`
    * `L_Len : C_Offset;`
    * `L_PID : POSIX.Process.Process_ID;`
    * `L_Type : Interfaces.C.short;`
    * `L_Whence : Interfaces.C.short;`

#### 3.2.2 Exceptions

* `File_Control_Error`: Raised on failure of a file control-related system call.

#### 3.2.3 Functions and Procedures

* **`function Open (Path : String; Flags : Interfaces.C.int) return File_Descriptor;`**
    * **Description**: Opens a file. Suitable for use without the `O_CREAT` flag.

* **`function Open (Path : String; Flags : Interfaces.C.int; File_Mode : Mode) return File_Descriptor;`**
    * **Description**: Opens or creates a file. Required when using the `O_CREAT` flag to specify file permissions.

* **`procedure Close (FD : in File_Descriptor);`**
    * **Description**: Closes a file descriptor.
    * **Raises**: `File_Control_Error` on failure.

* **`function Write (FD : in File_Descriptor; Buffer : in String) return Interfaces.C.size_t;`**
    * **Description**: Writes data to a file descriptor. Note that this may result in a "partial write," where fewer bytes are written than are in the buffer.
    * **Returns**: The number of bytes actually written.
    * **Raises**: `File_Control_Error` on a system call error.

* **`procedure Flock (FD : in File_Descriptor; Operation : in Interfaces.C.int);`**
    * **Description**: Applies or removes an advisory lock on an open file. This is a wrapper for `flock(2)`.
    * **Raises**: `File_Control_Error` on failure (e.g., if the lock is already held and `LOCK_NB` is specified).

* **`function Dup (Old_FD : in File_Descriptor) return File_Descriptor;`**
    * **Description**: Duplicates an existing file descriptor to the lowest-numbered unused descriptor. This is a wrapper for `dup(2)`.
    * **Returns**: The new file descriptor.
    * **Raises**: `File_Control_Error` on failure.

* **`procedure Dup2 (Old_FD : in File_Descriptor; New_FD : in File_Descriptor);`**
    * **Description**: Duplicates a file descriptor to a specific descriptor number. This is a wrapper for `dup2(2)`.
    * **Raises**: `File_Control_Error` on failure.

* **`function Get_FL_Flags (FD : in File_Descriptor) return Flags;`**
    * **Description**: Gets the file status flags using `fcntl(F_GETFL)`.
    * **Raises**: `File_Control_Error` on failure.

* **`procedure Set_FL_Flags (FD : in File_Descriptor; Fl_Flags : in Flags);`**
    * **Description**: Sets the file status flags using `fcntl(F_SETFL)`.
    * **Raises**: `File_Control_Error` on failure.

* **`procedure Get_Lock (FD : in File_Descriptor; Lock : in out Flock_Record);`**
    * **Description**: Retrieves lock information using `fcntl(F_GETLK)`.
    * **Raises**: `File_Control_Error` on failure.

* **`procedure Set_Lock (FD : in File_Descriptor; Lock : in Flock_Record);`**
    * **Description**: Sets a lock using `fcntl(F_SETLK)`.
    * **Raises**: `File_Control_Error` on failure.

* **`procedure Set_Lock_Wait (FD : in File_Descriptor; Lock : in Flock_Record);`**
    * **Description**: Sets a lock and waits if necessary, using `fcntl(F_SETLKW)`.
    * **Raises**: `File_Control_Error` on failure.

### 3.3. `POSIX.Signal`

Provides an Ada interface for POSIX signal handling, including `sigaction(2)` and related utility functions.

#### 3.3.1 Types

* `Sigset_T is private;`: Corresponds to C's `sigset_t`.
* `Signal_Handler is access procedure (sig : Interfaces.C.int);`: An access type for a standard signal handler function.
* `Siginfo_Handler is access procedure (...)`: An access type for a signal handler that uses the `SA_SIGINFO` flag.
* `Sigaction_Record`: A record that matches the memory layout of C's `struct sigaction`.
    * `sa_handler_address : System.Address;`
    * `sa_mask : Sigset_T;`
    * `sa_flags : Interfaces.C.int;`

#### 3.3.2 Exceptions

* `Signal_Error`: Raised when a signal-related operation fails.

#### 3.3.3 Functions and Procedures

* **`procedure sigaction (sig : in Interfaces.C.int; act : in Sigaction_Record; oact : out Sigaction_Record);`**
    * **Description**: Changes the action taken by a process on receipt of a specific signal.
    * **Raises**: `Signal_Error` on failure.

* **`procedure sigaction (sig : in Interfaces.C.int; act : in Sigaction_Record);`**
    * **Description**: An overloaded version of `sigaction` that does not retrieve the old action.
    * **Raises**: `Signal_Error` on failure.

* **`procedure sigemptyset (set : in out Sigset_T);`**
    * **Description**: Initializes a signal set to be empty.

* **`procedure sigfillset (set : in out Sigset_T);`**
    * **Description**: Initializes a signal set to be full, including all signals.

* **`procedure sigaddset (set : in out Sigset_T; signo : Interfaces.C.int);`**
    * **Description**: Adds a signal to a signal set.

* **`procedure sigdelset (set : in out Sigset_T; signo : Interfaces.C.int);`**
    * **Description**: Removes a signal from a signal set.

* **`function sigismember (set : in Sigset_T; signo : Interfaces.C.int) return Boolean;`**
    * **Description**: Tests whether a signal is a member of a signal set.

* **`procedure raise_signal (sig : in Interfaces.C.int);`**
    * **Description**: Sends a signal to the executing program, a wrapper for `raise(3)`.
