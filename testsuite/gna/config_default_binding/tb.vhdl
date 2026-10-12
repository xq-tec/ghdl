-- A component configuration with a block configuration but without a binding
-- indication, for an unbound instance, contains an implicit default binding
-- indication (LRM08 3.4.3).  As there is an explicit inner block
-- configuration, the architecture is the most recently analyzed one, chosen
-- at analysis (LRM08 7.3.3), so the instance is fully bound.
entity inner_nb is
end entity;

architecture a of inner_nb is
begin
end architecture;

entity mid_nb is
end entity;

architecture old of mid_nb is
begin
  assert false report "wrong architecture of mid_nb" severity failure;
end architecture;

architecture struct of mid_nb is
  component inner_nb is
  end component;
begin
  I1 : inner_nb;
end architecture;

entity top_nb is
end entity;

architecture struct of top_nb is
  component mid_nb is
  end component;
begin
  M1 : mid_nb;
end architecture;

configuration tb of top_nb is
  for struct
    for M1 : mid_nb
      for struct
        for I1 : inner_nb
          use entity work.inner_nb(a);
        end for;
      end for;
    end for;
  end for;
end configuration;
