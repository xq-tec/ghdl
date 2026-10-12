-- The bounds of a floating type definition shall be locally static
-- (LRM08 5.2.5.1).
entity err_float_bounds is
end entity;

architecture behav of err_float_bounds is
  function f return real is
  begin
    return 1.0;
  end function;

  type t is range 0.0 to f;
begin
end architecture;
