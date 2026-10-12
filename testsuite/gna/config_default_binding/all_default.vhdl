-- A component configuration for several unbound instances, in a generate
-- statement: they all have the same default binding, so the block
-- configuration applies to all of them (LRM08 3.4.3).
entity leaf_ad is
end entity;

architecture old of leaf_ad is
begin
  assert false report "wrong architecture of leaf_ad" severity failure;
end architecture;

architecture x of leaf_ad is
begin
end architecture;

entity top_ad is
end entity;

architecture a of top_ad is
  component leaf_ad is
  end component;
begin
  g : for i in 0 to 1 generate
    L1 : leaf_ad;
    L2 : leaf_ad;
  end generate;
end architecture;

configuration all_default of top_ad is
  for a
    for g
      for all : leaf_ad
        for x
        end for;
      end for;
    end for;
  end for;
end configuration;
