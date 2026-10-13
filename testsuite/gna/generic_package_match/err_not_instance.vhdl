-- The actual of a formal generic package of an entity is the uninstantiated
-- package, not an instance of it (LRM08 6.5.7.2).
entity err_not_instance_e is
  generic (package fc is new work.p_const generic map (g => 1));
end entity;

architecture behav of err_not_instance_e is
begin
end architecture;

entity err_not_instance is
end entity;

architecture behav of err_not_instance is
begin
  inst : entity work.err_not_instance_e generic map (fc => work.p_const);
end architecture;
