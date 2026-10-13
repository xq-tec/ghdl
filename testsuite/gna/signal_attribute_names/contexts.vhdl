-- Names whose prefix is a signal attribute, in other contexts:
-- * as port actuals (LRM08 6.5.6.3) and in a wait statement (LRM08 10.2);
-- * as prefixes of attributes (LRM08 16.2.4);
-- * in concurrent assignments, whose sensitivity holds the prefix signal
--   (LRM08 11.6);
-- * 'LAST_VALUE of arrays of records, of records of arrays, and of
--   multidimensional arrays, and through an alias (LRM08 8.3, 8.4, 8.5).
entity contexts_sub is
  port (i : in bit; iv : in bit_vector(0 to 1); o : out boolean);
end entity;

architecture behav of contexts_sub is
begin
  o <= i = '1' and iv(0) = iv(1);
end architecture;

entity contexts is
end entity;

architecture behav of contexts is
  type rec is record
    a : bit;
    v : bit_vector(0 to 3);
  end record;
  type rarr is array (0 to 2) of rec;
  type m2 is array (0 to 1, 0 to 1) of bit;

  signal v : bit_vector(0 to 3) := "0001";
  signal s : rec := ('0', "0000");
  signal ra : rarr := (others => ('0', "0000"));
  signal m : m2 := (others => "01");
  signal o1, o2 : boolean;
  signal ev : boolean;
  signal d1 : bit;
  signal d2 : bit_vector(0 to 1);
  signal d3, d4 : bit;
  alias al : rec is s;
begin
  v <= "1000" after 1 ns, "0000" after 2 ns;
  s <= ('1', "1111") after 1 ns;
  ra <= (others => ('1', "1010")) after 1 ns;
  m <= (others => "10") after 1 ns;

  u1 : entity work.contexts_sub
    port map (i => s'delayed(1 ns).a, iv => v'delayed(2 to 3), o => o1);
  u2 : entity work.contexts_sub
    port map (i => s'delayed.v(1), iv => s'delayed(1 ns).v(0 to 1), o => o2);

  ev <= s'delayed(1 ns).a'event;
  d1 <= s'delayed(1 ns).a'delayed(1 ns);
  d2 <= v'delayed(2 to 3)'delayed;
  d3 <= v'last_value(0);
  d4 <= s'last_value.a;

  wake : process
  begin
    wait on s'delayed.a, v'delayed(0 ns)(0 to 1);
    assert now = 1 ns severity failure;
    wait;
  end process;

  check : process
  begin
    wait for 1 ns;
    assert ra'last_value(1).a = '0' severity failure;
    assert ra'last_value(0 to 1)(1).v(2) = '0' severity failure;
    assert s'last_value.v(3) = '0' severity failure;
    assert s'last_value.v(1 to 2) = "00" severity failure;
    assert al'last_value.a = '0' severity failure;
    assert al'delayed(1 ns).a = '0' severity failure;
    assert m'last_value(1, 1) = '1' severity failure;
    assert m'delayed(1 ns)(1, 0) = '0' severity failure;
    wait for 1 ns;
    wait for 0 ns;
    assert d3 = '1' severity failure;
    assert d4 = '0' severity failure;
    wait for 2 ns;
    assert o1 severity failure;
    assert o2 severity failure;
    assert d1 = '1' severity failure;
    assert d2 = "00" severity failure;
    --  Set when the delayed element changed, at 2 ns; not reevaluated since.
    assert ev severity failure;
    report "PASS";
    wait;
  end process;
end architecture;
