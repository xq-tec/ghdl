-- Component configurations of bound instances at two levels of hierarchy,
-- with generate statements: each block configuration applies to instances
-- bound by two configuration specifications, so it is copied for one of them,
-- and the copies of the outer block configuration contain the inner one.
--
-- O of LEAF_N is 100 * G + V, where V is the generic of INNER_N bound by the
-- innermost block configuration.

entity inner_n is
  generic (v : integer);
  port (o : out integer);
end entity;

architecture rtl of inner_n is
begin
  o <= v;
end architecture;

entity leaf_n is
  generic (g : integer := 0);
  port (o : out integer := 0);
end entity;

architecture a of leaf_n is
  component ic is
    generic (v : integer := -1);
    port (o : out integer := 0);
  end component;
  signal s : integer;
begin
  U : ic port map (o => s);
  o <= g * 100 + s;
end architecture;

entity mid_n is
  generic (k : integer := 0);
  port (o1, o2, o3, o4 : out integer := 0);
end entity;

architecture a of mid_n is
  component leaf_n is
    generic (g : integer := 0);
    port (o : out integer := 0);
  end component;
begin
  fg : for i in 0 to 1 generate
    for N1 : leaf_n use entity work.leaf_n(a) generic map (g => 1 + i);
    for N2 : leaf_n use entity work.leaf_n(a) generic map (g => 3 + i);
    signal t1, t2 : integer;
  begin
    N1 : leaf_n port map (o => t1);
    N2 : leaf_n port map (o => t2);
    g0 : if i = 0 generate
      o1 <= k * 10000 + t1;
      o2 <= k * 10000 + t2;
    end generate;
    g1 : if i = 1 generate
      o3 <= k * 10000 + t1;
      o4 <= k * 10000 + t2;
    end generate;
  end generate;
end architecture;

entity nested_top is
end entity;

architecture a of nested_top is
  component mid_n is
    generic (k : integer := 0);
    port (o1, o2, o3, o4 : out integer := 0);
  end component;
  for M1 : mid_n use entity work.mid_n(a) generic map (k => 1);
  for M2 : mid_n use entity work.mid_n(a) generic map (k => 2);
  signal a1, a2, a3, a4, b1, b2, b3, b4 : integer;
begin
  M1 : mid_n port map (a1, a2, a3, a4);
  M2 : mid_n port map (b1, b2, b3, b4);

  process
  begin
    wait for 1 ns;
    assert a1 = 10107 report "M1.N1(0): " & integer'image (a1)
      severity failure;
    assert a2 = 10307 report "M1.N2(0): " & integer'image (a2)
      severity failure;
    assert a3 = 10208 report "M1.N1(1): " & integer'image (a3)
      severity failure;
    assert a4 = 10409 report "M1.N2(1): " & integer'image (a4)
      severity failure;
    assert b1 = 20107 report "M2.N1(0): " & integer'image (b1)
      severity failure;
    assert b2 = 20307 report "M2.N2(0): " & integer'image (b2)
      severity failure;
    assert b3 = 20208 report "M2.N1(1): " & integer'image (b3)
      severity failure;
    assert b4 = 20409 report "M2.N2(1): " & integer'image (b4)
      severity failure;
    wait;
  end process;
end architecture;

configuration nested of nested_top is
  for a
    for all : mid_n
      for a
        for fg(0)
          for all : leaf_n
            for a
              for U : ic
                use entity work.inner_n(rtl) generic map (v => 7);
              end for;
            end for;
          end for;
        end for;
        for fg(1)
          for N1 : leaf_n
            for a
              for U : ic
                use entity work.inner_n(rtl) generic map (v => 8);
              end for;
            end for;
          end for;
          for others : leaf_n
            for a
              for U : ic
                use entity work.inner_n(rtl) generic map (v => 9);
              end for;
            end for;
          end for;
        end for;
      end for;
    end for;
  end for;
end configuration;
