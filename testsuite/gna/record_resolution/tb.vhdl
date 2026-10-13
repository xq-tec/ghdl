-- Record element resolutions (LRM08 6.3): each resolution indication of a
-- record resolution applies to the subtype of the element with that name,
-- recursively for nested record and array element resolutions.
entity tb is
end entity;

architecture behav of tb is
  type bv is array (natural range <>) of bit;
  type int_vec is array (natural range <>) of integer;

  function res_or (v : bv) return bit is
    variable r : bit := '0';
  begin
    for i in v'range loop
      r := r or v (i);
    end loop;
    return r;
  end function;

  function res_sum (v : int_vec) return integer is
    variable r : integer := 0;
  begin
    for i in v'range loop
      r := r + v (i);
    end loop;
    return r;
  end function;

  type inner is record
    x : bit;
    y : integer;
  end record;

  type rec is record
    a : bit;
    b : integer;
    c : inner;
    d : bv (0 to 1);
    e : bv;
  end record;

  --  One element.
  subtype r_a is (a res_or) rec (e (0 to 1));
  --  Several elements, a nested record resolution, an array element
  --  resolution inside, and a record constraint on an element without
  --  resolution.
  subtype r_all is
    (a res_or, b res_sum, c (x res_or, y res_sum), d (res_or))
    rec (e (0 to 2));
  --  A record resolution of a subtype that has one already: the elements
  --  it does not name keep their resolution.
  subtype r_ab is (b res_sum) r_a;

  signal s_a : r_a := ('0', 1, ('0', 2), "00", "00");
  signal s_all : r_all := ('0', 0, ('0', 0), "00", "000");
  signal s_ab : r_ab := ('0', 0, ('0', 3), "00", "00");
begin
  --  Two drivers of the resolved element a, one of the others.
  p_a1 : process
  begin
    s_a.a <= '1';
    s_a.b <= 5;
    wait;
  end process;
  p_a2 : process
  begin
    s_a.a <= '0';
    wait;
  end process;

  --  Two drivers of each resolved element, one of the unresolved one.
  p_all1 : process
  begin
    s_all.a <= '1';
    s_all.b <= 3;
    s_all.c <= ('0', 10);
    s_all.d <= "10";
    s_all.e <= "101";
    wait;
  end process;
  p_all2 : process
  begin
    s_all.a <= '0';
    s_all.b <= 4;
    s_all.c <= ('1', 20);
    s_all.d <= "01";
    wait;
  end process;

  p_ab1 : process
  begin
    s_ab.a <= '1';
    s_ab.b <= 7;
    wait;
  end process;
  p_ab2 : process
  begin
    s_ab.a <= '0';
    s_ab.b <= 8;
    wait;
  end process;

  check : process
  begin
    wait for 1 ns;
    assert s_a.a = '1' severity failure;
    assert s_a.b = 5 severity failure;
    assert s_a.c = ('0', 2) severity failure;
    assert s_all.a = '1' severity failure;
    assert s_all.b = 7 severity failure;
    assert s_all.c.x = '1' severity failure;
    assert s_all.c.y = 30 severity failure;
    assert s_all.d = "11" severity failure;
    assert s_all.e = "101" severity failure;
    assert s_ab.a = '1' severity failure;
    assert s_ab.b = 15 severity failure;
    report "PASS";
    wait;
  end process;
end architecture;
