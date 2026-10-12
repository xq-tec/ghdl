-- 'BASE used as a type mark (LRM08 16.2.2).
entity err_base_type_mark is
end entity;

architecture bad of err_base_type_mark is
  subtype t is integer range 1 to 10;
  signal s : t'base;
begin
  s <= 0;
end architecture;
