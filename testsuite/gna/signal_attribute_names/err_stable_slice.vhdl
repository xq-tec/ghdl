-- A discrete range in the parenthesis of 'STABLE is a slice of the attribute
-- without a parameter, whose value is not an array (LRM08 8.5, 16.2.4).
entity err_stable_slice is
end entity;

architecture bad of err_stable_slice is
  signal v : bit_vector(0 to 3);
  signal b : boolean;
begin
  b <= v'stable(1 to 2);
end architecture;
