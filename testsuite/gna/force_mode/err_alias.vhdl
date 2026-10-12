-- Force through an alias of a member of a resolved composite.
use work.pkg.all;

entity err_alias is
end entity;

architecture behav of err_alias is
  signal s : rv(0 to 1);
  alias s0 : bit is s(0);
begin
  process
  begin
    s0 <= force '1';
    wait;
  end process;
end architecture;
