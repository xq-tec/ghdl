-- Component configurations of instances bound by configuration
-- specifications (LRM08 3.4.3, 7.3.2.1): the configuration specification
-- gives the primary binding indication, the component configuration may
-- give an incremental binding indication and a block configuration.  The
-- instantiation list may be ALL, OTHERS, or a list of labels; ALL and OTHERS
-- exclude only the instances named in a previous component configuration.
--
-- Each configuration X of TOP is simulated through the configuration X_TB of
-- WRAP, which gives the expected outputs as generics of TOP:
-- O = 100 * G + V, where V is 7 if the block configuration binds the inner
-- instance U of LEAF, and 0 if U is unbound.

entity inner is
  generic (v : integer);
  port (o : out integer);
end entity;

architecture rtl of inner is
begin
  o <= v;
end architecture;

entity leaf is
  generic (g : integer := 0);
  port (o : out integer := 0);
end entity;

architecture a of leaf is
  component ic is
    generic (v : integer := -1);
    port (o : out integer := 0);
  end component;
  signal s : integer;
begin
  U : ic port map (o => s);
  o <= g * 100 + s;
end architecture;

entity top is
  generic (e1 : integer; e2 : integer; e3 : integer);
end entity;

-- L1 and L2 are bound by one configuration specification, L3 is unbound.
architecture spec_list of top is
  component leaf is
    generic (g : integer := 0);
    port (o : out integer);
  end component;
  for L1, L2 : leaf use entity work.leaf(a) generic map (g => 1);
  signal o1, o2, o3 : integer;
begin
  L1 : leaf port map (o => o1);
  L2 : leaf port map (o => o2);
  L3 : leaf port map (o => o3);

  process
  begin
    wait for 1 ns;
    assert o1 = e1 report "L1: " & integer'image (o1) severity failure;
    assert o2 = e2 report "L2: " & integer'image (o2) severity failure;
    assert o3 = e3 report "L3: " & integer'image (o3) severity failure;
    wait;
  end process;
end architecture;

-- L1 and L2 are bound by two configuration specifications, L3 is unbound.
architecture spec_two of top is
  component leaf is
    generic (g : integer := 0);
    port (o : out integer);
  end component;
  for L1 : leaf use entity work.leaf(a) generic map (g => 1);
  for L2 : leaf use entity work.leaf(a) generic map (g => 2);
  signal o1, o2, o3 : integer;
begin
  L1 : leaf port map (o => o1);
  L2 : leaf port map (o => o2);
  L3 : leaf port map (o => o3);

  process
  begin
    wait for 1 ns;
    assert o1 = e1 report "L1: " & integer'image (o1) severity failure;
    assert o2 = e2 report "L2: " & integer'image (o2) severity failure;
    assert o3 = e3 report "L3: " & integer'image (o3) severity failure;
    wait;
  end process;
end architecture;

-- All instances are bound by a configuration specification with ALL.
architecture spec_all of top is
  component leaf is
    generic (g : integer := 0);
    port (o : out integer);
  end component;
  for all : leaf use entity work.leaf(a) generic map (g => 1);
  signal o1, o2, o3 : integer;
begin
  L1 : leaf port map (o => o1);
  L2 : leaf port map (o => o2);
  L3 : leaf port map (o => o3);

  process
  begin
    wait for 1 ns;
    assert o1 = e1 report "L1: " & integer'image (o1) severity failure;
    assert o2 = e2 report "L2: " & integer'image (o2) severity failure;
    assert o3 = e3 report "L3: " & integer'image (o3) severity failure;
    wait;
  end process;
end architecture;

-- 100, 100, 0
configuration all_noblk of top is
  for spec_list
    for all : leaf
    end for;
  end for;
end configuration;

-- 107, 107, 7
configuration all_blk of top is
  for spec_list
    for all : leaf
      for a
        for U : ic
          use entity work.inner(rtl) generic map (v => 7);
        end for;
      end for;
    end for;
  end for;
end configuration;

-- 100, 100, 0
configuration others_noblk of top is
  for spec_list
    for others : leaf
    end for;
  end for;
end configuration;

-- 107, 107, 0
configuration others_blk of top is
  for spec_list
    for L3 : leaf
    end for;
    for others : leaf
      for a
        for U : ic
          use entity work.inner(rtl) generic map (v => 7);
        end for;
      end for;
    end for;
  end for;
end configuration;

-- 100, 100, 0
configuration list_noblk of top is
  for spec_list
    for L1, L2 : leaf
    end for;
  end for;
end configuration;

-- 107, 107, 0
configuration list_blk of top is
  for spec_list
    for L1, L2 : leaf
      for a
        for U : ic
          use entity work.inner(rtl) generic map (v => 7);
        end for;
      end for;
    end for;
  end for;
end configuration;

-- 507, 507, 0
configuration list_map of top is
  for spec_list
    for L1, L2 : leaf
      generic map (g => 5);
      for a
        for U : ic
          use entity work.inner(rtl) generic map (v => 7);
        end for;
      end for;
    end for;
  end for;
end configuration;

-- 107, 207, 7
configuration two_blk of top is
  for spec_two
    for all : leaf
      for a
        for U : ic
          use entity work.inner(rtl) generic map (v => 7);
        end for;
      end for;
    end for;
  end for;
end configuration;

-- 300, 200, 0
configuration two_list_map of top is
  for spec_two
    for L1 : leaf
      generic map (g => 3);
    end for;
  end for;
end configuration;

-- 107, 107, 107
configuration all_spec_blk of top is
  for spec_all
    for all : leaf
      for a
        for U : ic
          use entity work.inner(rtl) generic map (v => 7);
        end for;
      end for;
    end for;
  end for;
end configuration;

-- 300, 400, 400
configuration all_spec_map of top is
  for spec_all
    for L1 : leaf
      generic map (g => 3);
    end for;
    for others : leaf
      generic map (g => 4);
    end for;
  end for;
end configuration;

entity wrap is
end entity;

architecture a of wrap is
  component top is
    generic (e1 : integer := -1; e2 : integer := -1; e3 : integer := -1);
  end component;
begin
  T : top;
end architecture;

configuration all_noblk_tb of wrap is
  for a
    for T : top
      use configuration work.all_noblk generic map (100, 100, 0);
    end for;
  end for;
end configuration;

configuration all_blk_tb of wrap is
  for a
    for T : top
      use configuration work.all_blk generic map (107, 107, 7);
    end for;
  end for;
end configuration;

configuration others_noblk_tb of wrap is
  for a
    for T : top
      use configuration work.others_noblk generic map (100, 100, 0);
    end for;
  end for;
end configuration;

configuration others_blk_tb of wrap is
  for a
    for T : top
      use configuration work.others_blk generic map (107, 107, 0);
    end for;
  end for;
end configuration;

configuration list_noblk_tb of wrap is
  for a
    for T : top
      use configuration work.list_noblk generic map (100, 100, 0);
    end for;
  end for;
end configuration;

configuration list_blk_tb of wrap is
  for a
    for T : top
      use configuration work.list_blk generic map (107, 107, 0);
    end for;
  end for;
end configuration;

configuration list_map_tb of wrap is
  for a
    for T : top
      use configuration work.list_map generic map (507, 507, 0);
    end for;
  end for;
end configuration;

configuration two_blk_tb of wrap is
  for a
    for T : top
      use configuration work.two_blk generic map (107, 207, 7);
    end for;
  end for;
end configuration;

configuration two_list_map_tb of wrap is
  for a
    for T : top
      use configuration work.two_list_map generic map (300, 200, 0);
    end for;
  end for;
end configuration;

configuration all_spec_blk_tb of wrap is
  for a
    for T : top
      use configuration work.all_spec_blk generic map (107, 107, 107);
    end for;
  end for;
end configuration;

configuration all_spec_map_tb of wrap is
  for a
    for T : top
      use configuration work.all_spec_map generic map (300, 400, 400);
    end for;
  end for;
end configuration;
