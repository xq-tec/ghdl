-- Primary binding indications with an open entity aspect, a configuration,
-- and an entity without architecture; a component configuration for
-- instances bound by a configuration specification and an unbound instance
-- (which gets the default binding indication).
--
-- O of LEAF_A is 100 * G + V, where V is the generic of INNER_A bound by the
-- block configuration (0 if unbound).

entity inner_a is
  generic (v : integer);
  port (o : out integer);
end entity;

architecture rtl of inner_a is
begin
  o <= v;
end architecture;

entity leaf_a is
  generic (g : integer := 0);
  port (o : out integer := 0);
end entity;

architecture a of leaf_a is
  component ic is
    generic (v : integer := -1);
    port (o : out integer := 0);
  end component;
  signal s : integer;
begin
  U : ic port map (o => s);
  o <= g * 100 + s;
end architecture;

configuration leaf_a_cfg of leaf_a is
  for a
  end for;
end configuration;

entity aspects_top is
end entity;

architecture a of aspects_top is
  component leaf_a is
    generic (g : integer := 0);
    port (o : out integer := -1);
  end component;
  for L1 : leaf_a use open;
  for L2 : leaf_a use configuration work.leaf_a_cfg generic map (g => 2);
  for L3, L4 : leaf_a use entity work.leaf_a generic map (g => 3);
  signal o1, o2, o3, o4, o5, o6 : integer;
begin
  L1 : leaf_a port map (o => o1);
  L2 : leaf_a port map (o => o2);
  L3 : leaf_a port map (o => o3);
  L4 : leaf_a port map (o => o4);
  L5 : leaf_a port map (o => o5);
  L6 : leaf_a port map (o => o6);

  process
  begin
    wait for 1 ns;
    assert o1 = -1 report "L1: " & integer'image (o1) severity failure;
    assert o2 = 400 report "L2: " & integer'image (o2) severity failure;
    assert o3 = 507 report "L3: " & integer'image (o3) severity failure;
    assert o4 = 307 report "L4: " & integer'image (o4) severity failure;
    assert o5 = 7 report "L5: " & integer'image (o5) severity failure;
    assert o6 = 0 report "L6: " & integer'image (o6) severity failure;
    wait;
  end process;
end architecture;

configuration aspects of aspects_top is
  for a
    for L1 : leaf_a
    end for;
    for L2 : leaf_a
      generic map (g => 4);
    end for;
    for L3 : leaf_a
      generic map (g => 5);
      for a
        for U : ic
          use entity work.inner_a(rtl) generic map (v => 7);
        end for;
      end for;
    end for;
    for L4, L5 : leaf_a
      for a
        for U : ic
          use entity work.inner_a(rtl) generic map (v => 7);
        end for;
      end for;
    end for;
  end for;
end configuration;
