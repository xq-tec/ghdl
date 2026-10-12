-- Force with the default mode of a member of a resolved composite.
use work.pkg.all;

entity err_member_default is
end entity;

architecture behav of err_member_default is
  signal s : rv(0 to 1);
begin
  process
  begin
    s(0) <= force '1';
    wait;
  end process;
end architecture;
