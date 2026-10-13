-- Record and array element resolutions in a generic package (LRM08 6.3),
-- whose record type is not static.  The instance copies the element
-- subtypes, which have no type mark.
package sim_generic_res is
  type bv is array (natural range <>) of bit;

  function res_or (v : bv) return bit;
end package;

package body sim_generic_res is
  function res_or (v : bv) return bit is
    variable r : bit := '0';
  begin
    for i in v'range loop
      r := r or v (i);
    end loop;
    return r;
  end function;
end package body;

use work.sim_generic_res.all;

package sim_generic_pkg is
  generic (n : natural);

  type rec is record
    a : bit;
    v : bv (0 to n - 1);
  end record;

  type bva is array (natural range <>) of bv (0 to n - 1);

  subtype r_a is (a res_or) rec;
  subtype r_v is (v (res_or)) rec;
  subtype bva_r is ((res_or)) bva;

  procedure check (x : (a res_or) rec; y : (res_or) bv);
end package;

package body sim_generic_pkg is
  procedure check (x : (a res_or) rec; y : (res_or) bv) is
  begin
    assert x.a = '1' severity failure;
    assert y = "01" severity failure;
  end procedure;
end package body;

use work.sim_generic_res.all;

entity sim_generic is
end entity;

architecture behav of sim_generic is
  package inst is new work.sim_generic_pkg generic map (n => 2);

  signal s : inst.r_a := ('0', "00");
begin
  p1 : process
  begin
    s <= ('1', "10");
    wait;
  end process;

  p2 : process
  begin
    s.a <= '0';
    wait;
  end process;

  check : process
    variable vr : inst.rec := ('1', "01");
    variable vb : bv (0 to 1) := "01";
  begin
    inst.check (vr, vb);
    wait for 1 ns;
    assert s.a = '1' severity failure;
    assert s.v = "10" severity failure;
    report "PASS";
    wait;
  end process;
end architecture;
