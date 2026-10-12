-- The NOTE example of LRM08 3.4.3, with "for all": a component configuration
-- with a block configuration must bind all its instances to the same design
-- entity.
entity e1_mall is
end entity;

architecture x of e1_mall is
begin
end architecture;

entity e2_mall is
end entity;

architecture x of e2_mall is
begin
end architecture;

entity top_mall is
end entity;

architecture a of top_mall is
  component c is
  end component;
  for L1 : c use entity work.e1_mall(x);
  for L2 : c use entity work.e2_mall(x);
begin
  L1 : c;
  L2 : c;
end architecture;

configuration err_mixed_all of top_mall is
  for a
    for all : c
      for x  -- error: L1 and L2 are bound to different design entities
      end for;
    end for;
  end for;
end configuration;
