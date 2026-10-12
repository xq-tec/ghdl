-- An expanded name with an entity prefix doesn't see an architecture
-- declaration that comes after it.
entity err_before_decl is
end entity;

architecture behav of err_before_decl is
  constant D : natural := err_before_decl.C;
  constant C : natural := 1;
begin
end architecture;
