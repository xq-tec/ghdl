-- An expanded name with an entity prefix doesn't see the declarations of
-- another architecture of the entity.
entity err_other_arch is
end entity;

architecture a1 of err_other_arch is
  constant C : natural := 1;
begin
end architecture;

architecture a2 of err_other_arch is
  constant D : natural := err_other_arch.C;
begin
end architecture;
