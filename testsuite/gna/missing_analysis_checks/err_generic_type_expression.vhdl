-- The actual of a generic type must be a subtype indication
-- (LRM08 6.5.7.2).
entity child_generic is
  generic (type t);
end entity;

architecture rtl of child_generic is
begin
end architecture;

entity err_generic_type_expression is
end entity;

architecture behav of err_generic_type_expression is
begin
  u : entity work.child_generic generic map (t => 1);
end architecture;
