-- Discrete range choices in assignment targets (VHDL-2008 slice association).
library ieee;
use ieee.std_logic_1164.all;

entity tb is
end entity;

architecture behav of tb is
begin
  process
    variable tmp1 : std_logic_vector(0 to 7);
    variable tmp2 : std_logic_vector(0 to 7);
    variable a : std_logic_vector(0 to 3);
    variable b : std_logic_vector(0 to 3);
  begin
    tmp1 := "10UXZWHL";
    (0 to 7 => tmp2) := tmp1;
    assert tmp2 = "10UXZWHL" severity failure;

    tmp1 := "10UXZWHL";
    (0 to 3 => a, 4 to 7 => b) := tmp1;
    assert a = "10UX" severity failure;
    assert b = "ZWHL" severity failure;

    -- Named associations need not be in order.
    tmp1 := "HLWZXU01";
    (4 to 7 => b, 0 to 3 => a) := tmp1;
    assert a = "HLWZ" severity failure;
    assert b = "XU01" severity failure;

    report "OK";
    wait;
  end process;
end architecture;
