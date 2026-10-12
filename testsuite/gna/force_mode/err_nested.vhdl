-- Force of a member of a resolved composite element.
use work.pkg.all;

entity err_nested is
end entity;

architecture behav of err_nested is
  type rv_arr is array (0 to 1) of rv(0 to 1);
  signal v : rv_arr;
begin
  process
  begin
    v(0)(1) <= force '1';
    wait;
  end process;
end architecture;
