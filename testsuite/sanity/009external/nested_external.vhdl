entity nested_external is
end entity;

architecture a of nested_external is
  signal s : bit := '1';
  alias s_ext is << signal .nested_external.s : bit >>;
  signal t : bit;
begin
  t <= s_ext;
  check : process
  begin
    wait for 1 ns;
    assert << signal .nested_external.s : bit >> = '1';
    assert t = '1';
    wait;
  end process;
end architecture;
