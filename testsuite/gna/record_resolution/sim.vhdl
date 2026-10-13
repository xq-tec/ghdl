-- Record element resolutions (LRM08 6.3), simulated: the named elements are
-- resolved, the others are not.  GHDL's simulator handles scalar elements,
-- and array elements that are unbounded in the record type.
package sim_pkg is
  type bv is array (natural range <>) of bit;
  type int_vec is array (natural range <>) of integer;

  function res_or (v : bv) return bit;
  function res_and (v : bv) return bit;
  function res_sum (v : int_vec) return integer;

  type rec is record
    a : bit;
    b : integer;
    c : bit_vector (0 to 1);
  end record;

  subtype r_ab is (a res_or, b res_sum) rec;

  type urec is record
    a : bit;
    v : bv;
  end record;

  --  A record element resolution of a record subtype that is partially
  --  constrained.
  subtype r_ua is (a res_or) urec;
end package;

package body sim_pkg is
  function res_or (v : bv) return bit is
    variable r : bit := '0';
  begin
    for i in v'range loop
      r := r or v (i);
    end loop;
    return r;
  end function;

  function res_and (v : bv) return bit is
    variable r : bit := '1';
  begin
    for i in v'range loop
      r := r and v (i);
    end loop;
    return r;
  end function;

  function res_sum (v : int_vec) return integer is
    variable r : integer := 0;
  begin
    for i in v'range loop
      r := r + v (i);
    end loop;
    return r;
  end function;
end package body;

use work.sim_pkg.all;

--  A port whose subtype has a record resolution and is only partially
--  constrained, individually associated.
entity sim_port is
  port (p : in r_ua);
end entity;

architecture behav of sim_port is
begin
end architecture;

use work.sim_pkg.all;

entity sim is
end entity;

architecture behav of sim is
  type bva is array (natural range <>) of bv;

  --  A new resolution of an element that the type mark resolves already.
  subtype r_and is (a res_and) r_ab;
  --  An array element resolution together with an index constraint, also
  --  of a record element, and together with an element constraint.
  subtype bv3 is (res_or) bv (0 to 2);
  subtype r_v is (a res_or, v (res_or)) urec (v (0 to 2));
  subtype bva2 is ((res_or)) bva (0 to 1)(0 to 2);

  signal s : r_ab := ('0', 0, "00");
  signal s_and : r_and := ('0', 0, "00");
  signal s_bv3 : bv3 := "000";
  signal s_v : r_v := ('0', "000");
  signal s_bva2 : bva2 := ("000", "000");
  signal x : bit;
  signal y : bv (0 to 1);
begin
  p1 : process
  begin
    s.a <= '1';
    s.b <= 3;
    s_and.a <= '1';
    s_bv3 <= "100";
    s_v <= ('1', "100");
    s_bva2 <= ("100", "010");
    wait;
  end process;

  p2 : process
  begin
    s.a <= '0';
    s.b <= 4;
    s_and.a <= '0';
    s_bv3 <= "001";
    s_v <= ('0', "001");
    s_bva2 <= ("001", "000");
    wait;
  end process;

  --  The only driver of the unresolved element c.
  p3 : process
  begin
    s <= ('0', 5, "01");
    wait;
  end process;

  inst : entity work.sim_port
    port map (p.a => x, p.v => y);

  check : process
  begin
    wait for 1 ns;
    assert s.a = '1' severity failure;
    assert s.b = 12 severity failure;
    assert s.c = "01" severity failure;
    assert s_and.a = '0' severity failure;
    assert s_bv3 = "101" severity failure;
    assert s_v.a = '1' severity failure;
    assert s_v.v = "101" severity failure;
    assert s_bva2 (0) = "101" severity failure;
    assert s_bva2 (1) = "010" severity failure;
    report "PASS";
    wait;
  end process;
end architecture;
