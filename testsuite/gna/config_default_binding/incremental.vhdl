-- A component configuration with an incremental binding indication and a
-- block configuration: the instance is bound by its primary binding
-- indication.
entity leaf_inc is
  generic (g : integer := 0);
end entity;

architecture x of leaf_inc is
begin
  assert g = 5 report "incremental binding not applied" severity failure;
end architecture;

entity top_inc is
end entity;

architecture a of top_inc is
  component c is
    generic (g : integer := 0);
  end component;
  for L1 : c use entity work.leaf_inc(x);
begin
  L1 : c;
end architecture;

configuration incremental of top_inc is
  for a
    for L1 : c
      generic map (g => 5);
      for x
      end for;
    end for;
  end for;
end configuration;
