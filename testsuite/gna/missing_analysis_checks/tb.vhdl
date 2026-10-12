-- Legal counterparts of the err_*.vhdl tests.
library ieee;
use ieee.std_logic_1164.all;

entity child is
  port (a : in bit_vector);
end entity;

architecture rtl of child is
begin
  process
  begin
    wait for 1 ns;
    assert a = "10" severity failure;
    wait;
  end process;
end architecture;

entity tb is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb is
  type int_file is file of integer;

  --  A pure function may read a file parameter (LRM08 4.3).
  pure function sum (file f : int_file) return integer is
    variable v, res : integer := 0;
  begin
    while not endfile (f) loop
      read (f, v);
      res := res + v;
    end loop;
    return res;
  end function;

  subtype bv2 is bit_vector (0 to 1);
  function cat (x : bit; y : bit) return bv2 is
  begin
    return x & y;
  end function;

  signal s : bit := '0';
  signal sel : std_ulogic_vector (1 downto 0) := "10";
  signal o : bit;
begin
  s <= '1';

  --  A matching selected signal assignment on std_ulogic_vector
  --  (LRM08 10.5.4).
  with sel select?
    o <= '1' when "1-",
         '0' when others;

  --  Non globally static actuals allowed for an unconstrained port
  --  (LRM08 6.5.6.3).
  u2 : entity work.child port map (a => cat (s, not s));
  u3 : entity work.child port map (a => (s, not s));

  process
    file f : int_file;
    variable r : integer;
    variable u : std_ulogic_vector (1 downto 0) := "10";
    type bv_ptr is access bit_vector;
    variable p : bv_ptr;
  begin
    file_open (f, "missing_analysis_checks.dat", write_mode);
    write (f, 3);
    write (f, 4);
    file_close (f);
    file_open (f, "missing_analysis_checks.dat", read_mode);
    r := sum (f);
    file_close (f);
    assert r = 7 severity failure;

    --  A matching case statement on std_ulogic_vector (LRM08 10.9).
    case? u is
      when "1-" => r := 1;
      when others => r := 2;
    end case?;
    assert r = 1 severity failure;

    --  An allocator with an index constraint (LRM08 9.3.7).
    p := new bit_vector (0 to 3);
    assert p'length = 4 severity failure;
    deallocate (p);

    wait for 1 ns;
    assert o = '1' severity failure;

    wait;
  end process;
end architecture;
