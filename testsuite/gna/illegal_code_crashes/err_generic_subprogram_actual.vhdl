-- A literal as the actual of a generic subprogram (LRM08 6.5.7.2).
entity err_generic_subprogram_actual_sub is
  generic (function f (x : integer) return integer);
end entity;

architecture rtl of err_generic_subprogram_actual_sub is
begin
end architecture;

entity err_generic_subprogram_actual is
end entity;

architecture bad of err_generic_subprogram_actual is
begin
  u : entity work.err_generic_subprogram_actual_sub
    generic map (f => 1);
end architecture;
