-- The actual instance does not match the implicit generic map aspect of
-- GENERIC MAP (DEFAULT) (LRM08 6.5.7.2 c).
entity err_default_value is
end entity;

architecture behav of err_default_value is
  function inc (x : integer) return integer is
  begin
    return x + 1;
  end function;

  package d4 is new work.p_def generic map (g => 4);
begin
  b : block
    generic (package fd is new work.p_def generic map (default));
    generic map (fd => d4);
  begin
  end block;
end architecture;
