-- clair-i18n.adb
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
with Interfaces.C.Strings;
with Clair.Errno;
with Clair.Exceptions;

package body Clair.I18N is
  use type Interfaces.C.Strings.chars_ptr;

  -- char *setlocale(int category, const char* locale)
  function c_setlocale (category : Interfaces.C.int;
                        locale   : Interfaces.C.char_array)
  return Interfaces.C.Strings.chars_ptr
  with import, convention => c, external_name => "setlocale";

  function c_bindtextdomain (domain : Interfaces.C.char_array;
                             dir    : Interfaces.C.char_array)
  return Interfaces.C.Strings.chars_ptr
  with import, convention => c, external_name => "bindtextdomain";

    -- char * bind_textdomain_codeset
  function c_bindcodeset (domain  : Interfaces.C.char_array;
                          codeset : Interfaces.C.char_array)
  return Interfaces.C.Strings.chars_ptr
  with import, convention => c, external_name => "bind_textdomain_codeset";

  -- char * textdomain (const char * domainname)
  function c_textdomain (domain : Interfaces.C.char_array)
  return Interfaces.C.Strings.chars_ptr
  with import, convention => c, external_name => "textdomain";

  procedure set_locale (category : Integer; locale : String) is
    result_ptr : Interfaces.C.Strings.chars_ptr;
  begin
    result_ptr := c_setlocale (Interfaces.C.int(category),
                              Interfaces.C.to_c (locale));
    -- Manual Specification: "returns NULL and fails to change the locale if..."
    if result_ptr = Interfaces.C.Strings.NULL_PTR then
      raise Clair.Exceptions.Invalid_Argument with
        "Locale setting failed: " & locale;
    end if;
  end set_locale;

  procedure bind_text_domain (domain : String; dir : String) is
    result_ptr : Interfaces.C.Strings.chars_ptr;
  begin
    result_ptr := c_bindtextdomain (Interfaces.C.to_c (domain),
                                    Interfaces.C.to_c (dir));

    if result_ptr = Interfaces.C.Strings.NULL_PTR then
      Clair.Errno.raise_from_errno (
        Clair.Errno.get_errno,
        "bind_text_domain (domain => """ &
        domain & """, dir => """ & dir & """)");
    end if;
  end bind_text_domain;

  procedure bind_text_domain_codeset (domain : String; codeset : String) is
    result_ptr : Interfaces.C.Strings.chars_ptr;
  begin
    -- Use Interfaces.C.to_c to create and pass temporary, stack-based C
    -- strings.
    -- Manual Specification: "The function makes copies of the argument strings
    -- as needed. Therefore, it is safe even after the temporary Ada memory is
    -- deallocated following the function call.
    result_ptr := c_bindcodeset (Interfaces.C.to_c (domain),
                                 Interfaces.C.to_c (codeset));

    if result_ptr = Interfaces.C.Strings.NULL_PTR then
      Clair.Errno.raise_from_errno
        (Clair.Errno.get_errno,
         "bind_text_domain_codeset (domain => """ & domain &
         """, codeset => """ & codeset & """)");
    end if;
  end bind_text_domain_codeset;

  procedure set_text_domain (domain : String) is
    result_ptr : Interfaces.C.Strings.chars_ptr;
  begin
    result_ptr := c_textdomain (Interfaces.C.to_c (domain));

    if result_ptr = Interfaces.C.Strings.NULL_PTR then
      Clair.Errno.raise_from_errno
        (Clair.Errno.get_errno,
         "set_text_domain (domain => """ & domain & """)");
    end if;
  end set_text_domain;

end Clair.I18N;
