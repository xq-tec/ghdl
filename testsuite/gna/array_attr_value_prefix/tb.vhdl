-- Array attributes whose prefix is a value of an array type but not an
-- object name (LRM08 8.1, 16.2.3).
entity tb is
end entity;

architecture behav of tb is
  type int_vector is array (natural range <>) of integer;

  function g (n : natural) return bit_vector is
    variable res : bit_vector (1 to n + 2);
  begin
    return res;
  end function;

  function fa (n : natural) return int_vector is
  begin
    return (1 to n => n);
  end function;

  signal s : bit;
  signal ss : bit_vector (7 downto 4) := "1010";
  signal sn : natural := 1;
  signal l1, l2 : natural;

  attribute mark : string;
  attribute mark of s : signal is "abcd";

  -- Globally static, so allowed in a declaration.
  constant C : natural := g (1)'length + s'path_name'length;
begin
  -- The bounds of a value prefix depend on the signals it reads, so they
  -- are in the sensitivity set (LRM08 10.2).
  l1 <= g (sn)'length;

  process (all)
  begin
    l2 <= to_string (sn)'length;
  end process;

  process
    variable x : integer := 1234;
    variable n : natural;
    variable ch : to_string (x)'element;
  begin
    assert s'simple_name'length = 1 report "s'simple_name" severity failure;
    assert s'path_name'length = 5 report "s'path_name" severity failure;
    assert s'instance_name'high = 12
      report "s'instance_name" severity failure;
    assert C = 8 report "C" severity failure;

    assert g (1)'length = 3 report "g (1)'length" severity failure;
    assert g (2)'left = 1 and g (2)'right = 4 report "g (2)'left/right"
      severity failure;
    assert fa (3)'high = 3 report "fa (3)'high" severity failure;
    assert fa (3)(1 to 2)'length = 2 report "fa (3)(1 to 2)'length"
      severity failure;

    n := 0;
    for i in to_string (x)'range loop
      n := n + 1;
    end loop;
    assert n = 4 report "to_string (x)'range" severity failure;
    ch := to_string (x)(2);
    assert ch = '2' report "to_string (x)'element" severity failure;
    assert to_string (x)'element'('3') = '3'
      report "to_string (x)'element'" severity failure;

    assert integer'image (123)'length = 3 report "integer'image"
      severity failure;
    assert s'mark'length = 4 report "s'mark" severity failure;
    wait;
  end process;

  process
  begin
    ss <= "0101";
    assert ss'driving_value'left = 7 report "ss'driving_value'left"
      severity failure;
    wait for 2 ns;
    assert ss'delayed (1 ns)'length = 4 report "ss'delayed'length"
      severity failure;
    assert ss'last_value'length = 4 report "ss'last_value'length"
      severity failure;
    assert ss'last_value'low = 4 report "ss'last_value'low"
      severity failure;

    assert l1 = 3 and l2 = 1 report "l1/l2" severity failure;
    sn <= 12;
    wait for 1 ns;
    assert l1 = 14 report "l1 not updated" severity failure;
    assert l2 = 2 report "l2 not updated" severity failure;
    report "done";
    wait;
  end process;
end architecture;
