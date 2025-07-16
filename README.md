# Cada

[![Project](https://img.shields.io/badge/Project-cada-blue.svg)](https://github.com/hodong-kim/cada)
[![License: 0BSD](https://img.shields.io/badge/License-0BSD-lightgrey.svg)](https://opensource.org/licenses/0BSD)

A practical Ada library providing bindings for common C and POSIX APIs.

---

## About The Project

This project aims to bridge Ada's high-level features with low-level POSIX/C system calls.

The goal is to provide an idiomatic Ada interface for common system-level programming tasks. By wrapping low-level calls in a type-safe API with error handling (through exceptions), `cada` is designed to assist in writing reliable and maintainable systems-level applications in Ada.

---

## Features

* **Dynamic Loading (`Dl`)**: Wrappers for `dlopen`, `dlsym`, and `dlclose` to interact with shared libraries at runtime.
* **POSIX Process Control (`POSIX.Process`)**: Interfaces for process management functions like `fork`, `setsid`, and `getpid`.
* **POSIX File Control (`POSIX.File_Control`)**: Bindings for file descriptor operations including `open`, `close`, `write`, `dup`, `dup2`, `flock`, and `fcntl`.
* **POSIX Signal Handling (`POSIX.Signal`)**: An interface for `sigaction` and related signal management utilities.

---

## Getting Started

Follow these steps to get the library built and tested on your local machine.

### Prerequisites

You will need an Ada compiler (GNAT) and the Rake build tool.

* **GNAT (Ada Compiler)**
* **Rake**

#### On Debian / Ubuntu

```sh
sudo apt-get update
sudo apt-get install gnat rake
```

#### On FreeBSD

```sh
sudo pkg install gnat13 rubygem-rake
```

> **Note for advanced users:** It is also possible to compile the latest version of GCC from the FreeBSD Ports tree (e.g., `lang/gcc14`). This may require manually editing the `Makefile` to enable Ada support before building.

### Building the Project

1.  **Clone the repo:**
    ```sh
    git clone [https://github.com/hodong-kim/cada.git](https://github.com/hodong-kim/cada.git)
    cd cada
    ```
2.  **Build the library:**
    Running `rake` in the root directory will build both the static (`libcada.a`) and shared (`libcada.so`) libraries in the `src/` directory.
    ```sh
    rake
    ```
3.  **Build and run the tests:**
    This command compiles all test programs and runs them to verify the library's functionality.
    ```sh
    rake test
    ```

-----

## Usage Example

The following is an example of using `cada` to acquire an exclusive, non-blocking file lock.

```ada
with Ada.Text_IO;
with POSIX.File_Control;
use POSIX.File_Control;

procedure Test_Lock is
   Lock_FD : File_Descriptor := -1;
begin
   -- Open a lock file
   Lock_FD := Open (Path => "my.lock",
                    Flags => O_WRONLY + O_CREAT,
                    File_Mode => 8#644#);

   -- Try to acquire an exclusive, non-blocking lock
   Flock (Lock_FD, LOCK_EX + LOCK_NB);

   Ada.Text_IO.Put_Line ("Lock acquired successfully!");
   -- ... critical section code ...

   Close (Lock_FD);

exception
   when File_Control_Error =>
      -- This will be raised if the lock is already held by another process
      Ada.Text_IO.Put_Line ("Could not acquire lock.");
end Test_Lock;
```

-----

## Bug Reports

To report a bug, please follow this procedure:

1.  **Check Existing Issues**: Before creating a new issue, check if the bug has already been reported by visiting the [Issues](https://github.com/hodong-kim/cada/issues) page.
2.  **Create a New Issue**: If it's a new bug, provide a clear and descriptive title.
3.  **Describe the Bug**: In the body of the issue, include the following:
     * A description of the bug.
     * Steps to reproduce the behavior.
     * The expected behavior.
     * The actual behavior, including any error messages or logs.
     * Your system environment (e.g., OS, GNAT compiler version).

A new bug report can be created [here](https://github.com/hodong-kim/cada/issues/new).

-----

## Contributing

Contributions to this project are welcome. To contribute, please follow these steps:

1.  Fork the Project
2.  Create your Feature Branch (`git checkout -b feature/NewFeature`)
3.  Commit your Changes (`git commit -m 'Add some NewFeature'`)
4.  Push to the Branch (`git push origin feature/NewFeature`)
5.  Open a Pull Request

-----

## License

Distributed under the 0BSD License. See `LICENSE` file for more information.

-----

## Project Home

The source code is available at:
https://github.com/hodong-kim/cada