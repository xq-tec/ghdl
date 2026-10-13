-- GENERIC MAP (DEFAULT) requires every generic of the uninstantiated package
-- to be a constant or a subprogram with a default (LRM08 6.5.5).
entity err_default_type is
  generic (package ft is new work.p_type generic map (default));
end entity;

architecture behav of err_default_type is
begin
end architecture;
