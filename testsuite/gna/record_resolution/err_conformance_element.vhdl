-- A subprogram body whose array element resolution differs from the one of
-- its declaration does not conform, also when the array subtype has an
-- element constraint (LRM08 4.10).
package err_conformance_element is
  type bv is array (natural range <>) of bit;
  type bva is array (natural range <>) of bv;
  function res_or (v : bv) return bit;
  function res_and (v : bv) return bit;

  procedure proc (x : ((res_or)) bva (0 to 1)(0 to 2));
end package;

package body err_conformance_element is
  function res_or (v : bv) return bit is
  begin
    return v (v'left);
  end function;

  function res_and (v : bv) return bit is
  begin
    return v (v'left);
  end function;

  procedure proc (x : ((res_and)) bva (0 to 1)(0 to 2)) is
  begin
  end procedure;
end package body;
