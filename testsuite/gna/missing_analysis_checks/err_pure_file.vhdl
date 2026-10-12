-- A pure function shall not reference an explicitly declared file
-- (LRM08 4.3).
entity err_pure_file is
end entity;

architecture behav of err_pure_file is
  type char_file is file of character;
  file f : char_file;

  pure function pure_f return boolean is
  begin
    return endfile (f);
  end function;
begin
end architecture;
