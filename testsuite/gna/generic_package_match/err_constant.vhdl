-- The actual instance has another value for a generic constant
-- (LRM08 6.5.7.2).
entity err_constant is
end entity;

architecture behav of err_constant is
  package c1 is new work.p_const generic map (g => 2);
begin
  b : block
    generic (package fc is new work.p_const generic map (g => 3));
    generic map (fc => c1);
  begin
  end block;
end architecture;
