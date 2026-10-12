-- An expanded name with an entity prefix in the entity statement part
-- doesn't see the declarations of an architecture.
entity err_entity_stmt is
begin
  process
  begin
    assert err_entity_stmt.C = 1;
    wait;
  end process;
end entity;

architecture behav of err_entity_stmt is
  constant C : natural := 1;
begin
end architecture;
