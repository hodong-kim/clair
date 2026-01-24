-- clair-time.ads
-- Copyright (c) 2025 Hodong Kim <hodong@nimfsoft.com>
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
with System;
with Clair.Platform;

package Clair.Time is
  use type Clair.Platform.time_t;

  NSEC_PER_SEC : constant := 1_000_000_000;

  subtype time_t is Clair.Platform.time_t;

  type Timespec is record
    tv_sec  : time_t;
    tv_nsec : Interfaces.C.long;
  end record;

  for Timespec use record
    tv_sec  at Clair.Platform.TIMESPEC_SEC_OFFSET
            range 0 .. time_t'size - 1;
    tv_nsec at Clair.Platform.TIMESPEC_NSEC_OFFSET
            range 0 .. Interfaces.C.long'size - 1;
  end record;

  for Timespec'size use Clair.Platform.TIMESPEC_SIZE * System.Storage_Unit;
  for Timespec'alignment use Clair.Platform.TIMESPEC_ALIGNMENT;

  pragma convention (c, Timespec);

  function get_time (clock_id : Interfaces.C.int;
                     tp       : System.Address) return Interfaces.C.int;
  pragma import (c, get_time, "clock_gettime");
  pragma inline (get_time);

  function "-" (left, right : Timespec) return Timespec
    with post =>
      -- "-"'result.tv_nsec >= 0 and then
      Interfaces.C.">=" ("-"'result.tv_nsec, 0) and then
      -- "-"'result.tv_nsec < NSEC_PER_SEC;
      Interfaces.C."<"  ("-"'result.tv_nsec, NSEC_PER_SEC);

  function "+" (left, right : Timespec) return Timespec
    with post =>
      -- "-"'result.tv_nsec >= 0 and then
      Interfaces.C.">=" ("+"'result.tv_nsec, 0) and then
      -- "-"'result.tv_nsec < NSEC_PER_SEC;
      Interfaces.C."<"  ("+"'result.tv_nsec, NSEC_PER_SEC);

  procedure Normalize (Ts : in out Timespec)
    with post =>
      -- ts.tv_nsec >= 0 and then
      Interfaces.C.">=" (ts.tv_nsec, 0) and then
      -- ts.tv_nsec < NSEC_PER_SEC;
      Interfaces.C."<"  (ts.tv_nsec, NSEC_PER_SEC);

end Clair.Time;
