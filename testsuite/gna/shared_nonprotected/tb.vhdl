entity tb is
end tb;

architecture behav of tb is
  type ram_type is array (0 to 3) of bit_vector (7 downto 0);
  shared variable ram : ram_type := (others => (others => '0'));
begin
  wr : process
  begin
    ram (1) := x"5a";
    wait;
  end process;

  rd : process
  begin
    wait for 1 ns;
    assert ram (1) = x"5a" severity failure;
    wait;
  end process;
end behav;
