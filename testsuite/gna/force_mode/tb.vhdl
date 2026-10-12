-- Explicit and default force modes, and legal force/release targets.
entity sub is
  port (o : out bit);
end entity;

architecture behav of sub is
begin
  o <= '0';

  process
  begin
    wait for 100 ns;

    -- A driving value force on an out port reaches the actual.
    o <= force out '1';
    wait for 2 ns;
    o <= release out;
    wait for 2 ns;

    -- An effective value force on an out port.  It doesn't reach the
    -- actual (LRM08 14.7.3.3), but GHDL's runtime collapses the port with
    -- its actual, so only the port is checked.
    o <= force in '1';
    wait for 1 ns;
    assert o = '1' report "force in o" severity failure;
    wait for 1 ns;
    o <= release in;
    wait for 2 ns;

    -- The default force mode of an out port is OUT.
    o <= force '1';
    wait for 2 ns;
    o <= release;
    wait;
  end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use work.pkg.all;

entity tb is
end entity;

architecture behav of tb is
  type rec_t is record
    a : bit;
    b : bit_vector(0 to 1);
  end record;

  signal s   : bit;
  signal bv  : bit_vector(0 to 3);
  signal r   : rec_t;
  signal slv : std_logic_vector(0 to 1);
  signal e   : erv(0 to 1);
  signal rs  : rv(0 to 1);
  signal t   : bit;
  signal ub  : bit_vector(0 to 1);

  -- A resolution indication has no effect on an alias (LRM08 6.3).
  alias ua : rv is ub;
begin
  s <= '0';
  bv <= "0000";
  r <= ('0', "00");
  slv <= "00";
  e <= (others => "00");
  rs <= "10";
  rs <= "01";
  ub <= "00";

  inst : entity work.sub port map (o => t);

  process
  begin
    -- Signal.
    s <= force in '1';
    wait for 1 ns;
    assert s = '1' report "force in s" severity failure;
    s <= release in;
    wait for 1 ns;
    assert s = '0' report "release in s" severity failure;

    s <= force out '1';
    wait for 1 ns;
    assert s = '1' report "force out s" severity failure;
    s <= release out;
    wait for 1 ns;
    assert s = '0' report "release out s" severity failure;

    s <= force '1';
    wait for 1 ns;
    assert s = '1' report "force s" severity failure;
    s <= release;
    wait for 1 ns;
    assert s = '0' report "release s" severity failure;

    -- Subelements of unresolved composites.
    bv (1) <= force in '1';
    bv (2 to 3) <= force out "11";
    r.b (0) <= force '1';
    wait for 1 ns;
    assert bv = "0111" report "force bv" severity failure;
    assert r = ('0', "10") report "force r" severity failure;
    bv (1) <= release in;
    bv (2 to 3) <= release out;
    r.b (0) <= release;
    wait for 1 ns;
    assert bv = "0000" report "release bv" severity failure;
    assert r = ('0', "00") report "release r" severity failure;

    -- An element of a std_logic_vector (element resolution).
    slv (0) <= force '1';
    wait for 1 ns;
    assert slv = "10" report "force slv(0)" severity failure;
    slv (0) <= release;
    wait for 1 ns;
    assert slv = "00" report "release slv(0)" severity failure;

    -- A whole array whose elements are resolved composites.
    e <= force (others => "11");
    wait for 1 ns;
    assert e = ("11", "11") report "force e" severity failure;
    e <= release;
    wait for 1 ns;
    assert e = ("00", "00") report "release e" severity failure;

    -- A member of an unresolved signal through an alias of a resolved
    -- subtype.
    ua (0) <= force '1';
    wait for 1 ns;
    assert ub = "10" report "force ua(0)" severity failure;
    ua (0) <= release;
    wait for 1 ns;
    assert ub = "00" report "release ua(0)" severity failure;

    -- A constrained subtype of a resolved composite subtype inherits the
    -- resolution function.
    assert rs = "11" report "resolved rs" severity failure;

    -- Out port of a sub-entity, see sub.
    wait for 100 ns - now + 1 ns;
    assert t = '1' report "force out o" severity failure;
    wait for 2 ns;
    assert t = '0' report "release out o" severity failure;
    wait for 4 ns;
    assert t = '0' report "release in o" severity failure;
    wait for 2 ns;
    assert t = '1' report "force o" severity failure;
    wait for 2 ns;
    assert t = '0' report "release o" severity failure;

    report "OK";
    wait;
  end process;
end architecture;
