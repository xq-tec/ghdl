-- Two homographs of a resolution function are both candidates (LRM08 4.5.1):
-- listing the candidates must not crash.
package res_ovl_a is
  function res (v : bit_vector) return bit;
end package;

package body res_ovl_a is
  function res (v : bit_vector) return bit is
  begin
    return '0';
  end function;
end package body;

package res_ovl_b is
  function res (v : bit_vector) return bit;
end package;

package body res_ovl_b is
  function res (v : bit_vector) return bit is
  begin
    return '1';
  end function;
end package body;

use work.res_ovl_a.all;
use work.res_ovl_b.all;

entity err_resolution_overload is
end entity;

architecture bad of err_resolution_overload is
  subtype amb is res bit;
begin
end architecture;
