-- Force of a whole resolved composite.
use work.pkg.all;

entity err_whole is
end entity;

architecture behav of err_whole is
  signal s : rv(0 to 1);
begin
  process
  begin
    s <= force "11";
    wait;
  end process;
end architecture;
