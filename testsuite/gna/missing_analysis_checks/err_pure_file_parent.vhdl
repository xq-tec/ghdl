-- A procedure whose parent is a pure function shall not reference an
-- explicitly declared file (LRM08 4.3).
entity err_pure_file_parent is
end entity;

architecture behav of err_pure_file_parent is
  type char_file is file of character;
  file f : char_file;

  pure function pure_f return integer is
    procedure inner is
    begin
      write (f, 'X');
    end procedure;
  begin
    inner;
    return 0;
  end function;
begin
end architecture;
