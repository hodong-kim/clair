-- clair-time.adb
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
package body Clair.Time is
  use type Interfaces.C.long;

  procedure normalize (ts : in out Timespec) is
  begin
    -------------------------------------------------------------------------
    -- 1. Overflow handling loop
    -------------------------------------------------------------------------
    while ts.tv_nsec >= NSEC_PER_SEC loop
      -- If tv_sec is already at its maximum, adding 1 results in a wrap-around
      -- (becoming negative). An explicit check is added to catch this safety
      -- even in release mode where checks are suppressed
      -- (e.g., GNAT's -gnatp switch).
      if ts.tv_sec = time_t'last then
        raise Constraint_Error with "Clair.Time: tv_sec overflow in normalize";
      end if;

      ts.tv_sec  := ts.tv_sec + 1;
      ts.tv_nsec := ts.tv_nsec - NSEC_PER_SEC;
    end loop;

    -------------------------------------------------------------------------
    -- 2. Underflow handling loop
    -------------------------------------------------------------------------
    while ts.tv_nsec < 0 loop
      -- If tv_sec is already at its minimum, subtracting 1 results
      -- in a wrap-around (becoming positive).
      if ts.tv_sec = time_t'first then
        raise Constraint_Error with "Clair.Time: tv_sec underflow in normalize";
      end if;

      ts.tv_sec  := ts.tv_sec - 1;
      ts.tv_nsec := ts.tv_nsec + NSEC_PER_SEC;
    end loop;
  end normalize;

  function "-" (left, right : Timespec) return Timespec is
    result : timespec;
  begin
    result.tv_sec  := left.tv_sec  - right.tv_sec;
    result.tv_nsec := left.tv_nsec - right.tv_nsec;
    normalize (result);
    return result;
  end "-";

  function "+" (left, right : Timespec) return Timespec is
    result : timespec;
  begin
    result.tv_sec  := left.tv_sec  + right.tv_sec;
    result.tv_nsec := left.tv_nsec + right.tv_nsec;
    normalize (result);
    return result;
  end "+";

end Clair.Time;
