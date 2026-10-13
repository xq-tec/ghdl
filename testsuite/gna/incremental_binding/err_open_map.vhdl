-- An incremental binding indication with a map aspect for an instance whose
-- primary binding indication is open (LRM08 7.3.2.1).
entity top_om is
end entity;

architecture a of top_om is
  component leaf_om is
    generic (g : integer := 0);
  end component;
  for L1 : leaf_om use open;
begin
  L1 : leaf_om;
end architecture;

configuration err_open_map of top_om is
  for a
    for L1 : leaf_om
      generic map (g => 1);
    end for;
  end for;
end configuration;
