-- A component configuration with a block configuration for an instance bound
-- by a configuration specification and for an unbound instance, whose default
-- binding is another design entity (LRM08 3.4.3).
entity c_md is
end entity;

architecture x of c_md is
begin
end architecture;

entity e1_md is
end entity;

architecture x of e1_md is
begin
end architecture;

entity top_md is
end entity;

architecture a of top_md is
  component c_md is
  end component;
  for L1 : c_md use entity work.e1_md(x);
begin
  L1 : c_md;
  L2 : c_md;
end architecture;

configuration err_mixed_default of top_md is
  for a
    for all : c_md
      for x  -- error: L1 is bound to e1_md(x), L2 to c_md(x)
      end for;
    end for;
  end for;
end configuration;
