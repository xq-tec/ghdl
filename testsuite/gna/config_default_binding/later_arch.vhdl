-- The default binding indication of a component configuration with a block
-- configuration is determined when the configuration is analyzed (LRM08
-- 7.3.3): an architecture analyzed later doesn't change it, even when the
-- configuration is analyzed again for elaboration.
entity leaf_la is
end entity;

architecture old of leaf_la is
begin
end architecture;

entity top_la is
end entity;

architecture a of top_la is
  component leaf_la is
  end component;
begin
  L1 : leaf_la;
end architecture;

configuration later_arch of top_la is
  for a
    for L1 : leaf_la
      for old
      end for;
    end for;
  end for;
end configuration;

architecture newer of leaf_la is
begin
  assert false report "wrong architecture of leaf_la" severity failure;
end architecture;
