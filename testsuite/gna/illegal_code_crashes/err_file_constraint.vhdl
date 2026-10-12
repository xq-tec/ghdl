-- A constraint on a file subtype (LRM08 6.3).
entity err_file_constraint is
end entity;

architecture bad of err_file_constraint is
  type ft is file of integer;
  subtype bad_t is ft (0 to 3);
begin
end architecture;
