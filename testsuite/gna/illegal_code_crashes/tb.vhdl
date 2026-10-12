-- Legal counterparts of the err_*.vhdl files.
entity tb is
end entity;

architecture behav of tb is
  type rec is record
    a, b : integer;
  end record;

  signal v : bit_vector (0 to 3) := "1010";
  constant pn : string := v'instance_name (1 to 3);

  group g_t is (signal, signal);
  group g : g_t (v, v);
begin
  g1 : for i in 0 to 1 generate
    signal s : bit := '1';
  begin
  end generate;

  process
    alias ext_s is <<signal .tb.g1(1).s : bit>>;
    variable r : rec;
    variable c : character;
  begin
    r := (a => 1, others => 2);
    assert r.b = 2 report "bad record aggregate" severity failure;

    c := v'path_name (1);
    assert c = ':' report "bad 'path_name index" severity failure;
    assert v'path_name (1 to 3) = ":tb" report "bad 'path_name slice"
      severity failure;
    assert pn = ":tb" report "bad 'instance_name slice" severity failure;

    assert ext_s = '1' report "bad external name" severity failure;
    wait;
  end process;
end architecture;
