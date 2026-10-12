-- Force with an explicit mode of a member of a resolved composite.
use work.pkg.all;

entity err_member_explicit is
end entity;

architecture behav of err_member_explicit is
  signal s : rv(0 to 1);
begin
  process
  begin
    s(0) <= force out '1';
    wait;
  end process;
end architecture;
