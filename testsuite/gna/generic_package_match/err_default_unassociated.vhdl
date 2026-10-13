-- A formal generic package with GENERIC MAP (DEFAULT) must be associated
-- (LRM08 6.5.6.2).
entity err_default_unassociated is
end entity;

architecture behav of err_default_unassociated is
begin
  b : block
    generic (package fc is new work.p_const generic map (default));
  begin
  end block;
end architecture;
