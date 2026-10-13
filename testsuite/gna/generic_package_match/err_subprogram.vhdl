-- The actual instance has another subprogram for a generic subprogram
-- (LRM08 6.5.7.2).
entity err_subprogram is
end entity;

architecture behav of err_subprogram is
  function inc (x : integer) return integer is
  begin
    return x + 1;
  end function;

  function dbl (x : integer) return integer is
  begin
    return 2 * x;
  end function;

  package s1 is new work.p_sub generic map (f => inc);
begin
  b : block
    generic (package fs is new work.p_sub generic map (f => dbl));
    generic map (fs => s1);
  begin
  end block;
end architecture;
