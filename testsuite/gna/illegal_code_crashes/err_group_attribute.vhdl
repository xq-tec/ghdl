-- An attribute name as a group constituent (LRM08 6.10).
entity err_group_attribute is
end entity;

architecture bad of err_group_attribute is
  group g_t is (signal);
  signal a : bit := '0';
  group g : g_t (a'event);
begin
end architecture;
