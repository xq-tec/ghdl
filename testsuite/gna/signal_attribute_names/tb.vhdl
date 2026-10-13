-- Names whose prefix is a signal attribute or 'IMAGE:
-- * indexed and slice names of 'LAST_VALUE and 'DRIVING_VALUE of an array
--   signal (LRM08 8.4, 8.5, 16.2.4);
-- * selected names of 'LAST_VALUE and 'DELAYED of a record signal (LRM08 8.3);
-- * a slice of 'DELAYED without a parameter: a discrete range cannot be the
--   TIME parameter (LRM08 8.5, 16.2.4);
-- * indexed and slice names of 'IMAGE (LRM08 8.4, 8.5, 16.2.2).
entity tb is
end entity;

architecture behav of tb is
  type rec is record
    a : bit;
    b : integer;
  end record;

  signal v : bit_vector(0 to 3) := "0001";
  signal w : bit_vector(0 to 3) := "0000";
  signal s : rec := ('0', 1);
  signal d1 : bit;
  signal d2 : bit;
  signal d3 : bit_vector(0 to 1);
  signal d4 : bit;
  signal wakes : natural := 0;
begin
  v <= "1000" after 1 ns;
  s <= ('1', 2) after 1 ns;

  d1 <= v'delayed(1 ns)(3);
  d2 <= s'delayed(1 ns).a;
  d3 <= v'delayed(2 to 3);
  d4 <= s'delayed.a;

  --  Static signal names in a sensitivity list.
  sens : process (v'delayed(2 to 3), s'delayed(1 ns).a)
  begin
    wakes <= wakes + 1;
  end process;

  check : process
  begin
    wait for 1 ns;
    assert v'last_value(0) = '0' severity failure;
    assert v'last_value(3) = '1' severity failure;
    assert v'last_value(1 to 3) = "001" severity failure;
    assert s'last_value.a = '0' severity failure;
    assert s'last_value.b = 1 severity failure;
    wait for 0 ns;
    --  'DELAYED without a parameter follows one delta later.
    assert d3 = "01" severity failure;
    assert d2 = '0' severity failure;
    wait for 2 ns;
    assert d1 = '0' severity failure;
    assert d2 = '1' severity failure;
    assert d3 = "00" severity failure;
    assert d4 = '1' severity failure;
    assert v'delayed(2 to 3) = "00" severity failure;
    assert v'delayed(0 ns)(0) = '1' severity failure;
    assert integer'image(42)(1) = '4' severity failure;
    assert integer'image(42)(2) = '2' severity failure;
    assert integer'image(-7)(1 to 1) = "-" severity failure;
    assert integer'image(-7)(2 to 2) = "7" severity failure;
    assert wakes = 3 severity failure;
    report "PASS";
    wait;
  end process;

  drv : process
  begin
    w <= "0101";
    wait for 0 ns;
    assert w'driving_value(1) = '1' severity failure;
    assert w'driving_value(2 to 3) = "01" severity failure;
    wait;
  end process;
end architecture;
