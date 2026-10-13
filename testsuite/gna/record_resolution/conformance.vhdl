-- Subprogram bodies with record and array element resolutions conform to
-- their declarations (LRM08 4.10).
package conformance is
  type bv is array (natural range <>) of bit;
  type bva is array (natural range <>) of bv;
  function res_or (v : bv) return bit;

  type rec is record
    a : bit;
    b : integer;
    v : bv;
  end record;

  procedure proc_rec (x : (a res_or) rec);
  procedure proc_arr (x : (res_or) bv);
  --  With constraints.
  procedure proc_rec_c (x : (a res_or, v (res_or)) rec (v (0 to 2)));
  procedure proc_arr_c (x : (res_or) bv (0 to 2));
  procedure proc_arr_el (x : ((res_or)) bva (0 to 1)(0 to 2));
end package;

package body conformance is
  function res_or (v : bv) return bit is
  begin
    return v (v'left);
  end function;

  procedure proc_rec (x : (a res_or) rec) is
  begin
  end procedure;

  procedure proc_arr (x : (res_or) bv) is
  begin
  end procedure;

  procedure proc_rec_c (x : (a res_or, v (res_or)) rec (v (0 to 2))) is
  begin
  end procedure;

  procedure proc_arr_c (x : (res_or) bv (0 to 2)) is
  begin
  end procedure;

  procedure proc_arr_el (x : ((res_or)) bva (0 to 1)(0 to 2)) is
  begin
  end procedure;
end package body;
