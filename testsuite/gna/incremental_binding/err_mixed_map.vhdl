-- A binding indication without entity aspect in a component configuration
-- for an instance bound by a configuration specification and an unbound
-- instance (LRM08 7.3.2.1).
entity leaf_mm is
  generic (g : integer := 0);
end entity;

architecture a of leaf_mm is
begin
end architecture;

entity top_mm is
end entity;

architecture a of top_mm is
  component leaf_mm is
    generic (g : integer := 0);
  end component;
  for L1 : leaf_mm use entity work.leaf_mm(a);
begin
  L1 : leaf_mm;
  L2 : leaf_mm;
end architecture;

configuration err_mixed_map of top_mm is
  for a
    for all : leaf_mm
      generic map (g => 1);
    end for;
  end for;
end configuration;
