-- Force of a member of a resolved composite record field.
use work.pkg.all;

entity err_record is
end entity;

architecture behav of err_record is
  type rec_t is record
    a : rv(0 to 1);
  end record;
  signal r : rec_t;
begin
  process
  begin
    r.a(0) <= force '1';
    wait;
  end process;
end architecture;
