-- Force of a member of an element-resolved composite element.
use work.pkg.all;

entity err_elem_resolved is
end entity;

architecture behav of err_elem_resolved is
  signal e : erv(0 to 1);
begin
  process
  begin
    e(0)(1) <= force '1';
    wait;
  end process;
end architecture;
