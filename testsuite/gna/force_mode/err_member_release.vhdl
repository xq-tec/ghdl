-- Release of a member of a resolved composite.
use work.pkg.all;

entity err_member_release is
end entity;

architecture behav of err_member_release is
  signal s : rv(0 to 1);
begin
  process
  begin
    s(0) <= release;
    wait;
  end process;
end architecture;
