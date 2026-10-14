-- A range is not a valid prefix of a scalar type attribute (LRM08 16.2.2),
-- but -frelaxed (implied by --std=93c) accepts it with a warning.
entity tb is
end tb;

architecture behav of tb is
  type arr is array (2 to 5) of bit;
  signal x : bit_vector (7 downto 4);
begin
  process
  begin
    assert arr'range'left = 2 report "arr'range'left" severity failure;
    assert arr'range'right = 5 report "arr'range'right" severity failure;
    assert arr'range'high = 5 report "arr'range'high" severity failure;
    assert arr'range'low = 2 report "arr'range'low" severity failure;
    assert arr'range'ascending report "arr'range'ascending"
      severity failure;
    assert arr'reverse_range'left = 5 report "arr'reverse_range'left"
      severity failure;
    assert x'range'left = 7 report "x'range'left" severity failure;
    assert not x'range'ascending report "x'range'ascending"
      severity failure;
    wait;
  end process;
end behav;
