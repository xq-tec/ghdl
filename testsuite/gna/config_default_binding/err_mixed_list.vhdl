-- The NOTE example of LRM08 3.4.3, with "for L1, L2": a component configuration
-- with a block configuration must bind all its instances to the same design
-- entity.
entity e1_mlist is
end entity;

architecture x of e1_mlist is
begin
end architecture;

entity e2_mlist is
end entity;

architecture x of e2_mlist is
begin
end architecture;

entity top_mlist is
end entity;

architecture a of top_mlist is
  component c is
  end component;
  for L1 : c use entity work.e1_mlist(x);
  for L2 : c use entity work.e2_mlist(x);
begin
  L1 : c;
  L2 : c;
end architecture;

configuration err_mixed_list of top_mlist is
  for a
    for L1, L2 : c
      for x  -- error: L1 and L2 are bound to different design entities
      end for;
    end for;
  end for;
end configuration;
