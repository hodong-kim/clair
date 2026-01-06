-- clair-i18n.ads
-- Copyright (c) 2025 Hodong Kim <hodong@nimfsoft.art>
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
with Clair.Platform;

package Clair.I18N is

  LC_ALL      : constant := Clair.Platform.LC_ALL;
  LC_CTYPE    : constant := Clair.Platform.LC_CTYPE;
  LC_NUMERIC  : constant := Clair.Platform.LC_NUMERIC;
  LC_TIME     : constant := Clair.Platform.LC_TIME;
  LC_COLLATE  : constant := Clair.Platform.LC_COLLATE;
  LC_MONETARY : constant := Clair.Platform.LC_MONETARY;
  LC_MESSAGES : constant := Clair.Platform.LC_MESSAGES;

  procedure set_locale (category : Integer;
                        locale   : String);
  procedure bind_text_domain (domain : String;
                              dir    : String);
  procedure bind_text_domain_codeset (domain  : String;
                                      codeset : String);
  procedure set_text_domain (domain : String);

end Clair.I18N;