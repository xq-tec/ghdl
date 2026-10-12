-- Expanded names whose prefix denotes the entity of the enclosing
-- architecture and whose suffix is declared in the architecture (LRM08 8.3).
package tb_pkg is
  -- Also made visible in the architecture by a use clause.
  constant C : natural := 100;
end package;

entity tb is
  generic (g : natural := 5);
  port (p : in bit := '1');

  function f (x : integer) return integer is
  begin
    return x + 1;
  end function;
end entity;

use work.tb_pkg.all;
architecture behav of tb is
  constant C : natural := 7;
  signal s : natural := 3;

  -- Overloads the entity's f.
  function f (x : bit) return integer is
  begin
    return bit'pos (x) + 10;
  end function;

  -- Used in a declaration of the architecture itself.
  constant D : natural := tb.C + tb.g;
begin
  blk : block
    signal x : natural := 11;
  begin
    inner : block
    begin
      process
      begin
        assert tb.C = 7 report "tb.C in block" severity failure;
        assert tb.s = 3 report "tb.s in block" severity failure;
        assert tb.f (4) = 5 report "tb.f (integer) in block"
          severity failure;
        assert tb.f ('1') = 11 report "tb.f (bit) in block"
          severity failure;
        assert tb.blk.x = 11 report "tb.blk.x" severity failure;
        wait;
      end process;
    end block;
  end block;

  process
    variable v : natural;
  begin
    v := tb.C + tb.s;
    assert v = 10 report "tb.C + tb.s" severity failure;
    assert tb.D = 12 report "tb.D" severity failure;
    assert work.tb.C = 7 report "work.tb.C" severity failure;
    assert work.tb_pkg.C = 100 report "work.tb_pkg.C" severity failure;
    assert tb.f (1) = 2 and tb.f ('0') = 10 report "tb.f" severity failure;
    assert tb.p = '1' report "tb.p" severity failure;
    assert tb.g = 5 report "tb.g" severity failure;
    tb.s <= 4;
    wait for 1 ns;
    assert tb.s = 4 report "tb.s <= 4" severity failure;
    wait;
  end process;
end architecture;
