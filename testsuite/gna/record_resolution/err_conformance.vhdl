-- A subprogram body whose record resolution differs from the one of its
-- declaration does not conform (LRM08 4.10).
package err_conformance is
  type bv is array (natural range <>) of bit;
  function res_or (v : bv) return bit;
  function res_and (v : bv) return bit;

  type rec is record
    a : bit;
    b : integer;
  end record;

  procedure proc (x : (a res_or) rec);
end package;

package body err_conformance is
  function res_or (v : bv) return bit is
  begin
    return v (v'left);
  end function;

  function res_and (v : bv) return bit is
  begin
    return v (v'left);
  end function;

  procedure proc (x : (a res_and) rec) is
  begin
  end procedure;
end package body;
