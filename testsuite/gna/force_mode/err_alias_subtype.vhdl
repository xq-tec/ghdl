-- Force through an alias of an unresolved subtype of a member of an
-- element-resolved composite.
use work.pkg.all;

entity err_alias_subtype is
end entity;

architecture behav of err_alias_subtype is
  signal e : erv(0 to 1);
  alias a : bv_vec(0 to 1) is e;
begin
  process
  begin
    a(0)(1) <= force '1';
    wait;
  end process;
end architecture;
