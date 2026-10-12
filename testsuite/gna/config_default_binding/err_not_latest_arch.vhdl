-- The block specification of a block configuration in a component
-- configuration with an implicit default binding indication must name the
-- most recently analyzed architecture of the entity (LRM08 3.4.2, 7.3.3).
entity e_nl is
end entity;

architecture x of e_nl is
begin
end architecture;

architecture y of e_nl is
begin
end architecture;

entity top_nl is
end entity;

architecture a of top_nl is
  component e_nl is
  end component;
begin
  L1 : e_nl;
end architecture;

configuration err_not_latest_arch of top_nl is
  for a
    for L1 : e_nl
      for x  -- error: the default binding is e_nl(y)
      end for;
    end for;
  end for;
end configuration;
