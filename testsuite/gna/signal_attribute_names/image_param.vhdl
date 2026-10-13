-- Folding 'IMAGE, 'VALUE and an index keeps the parameter or index as
-- written: the value of an enumeration literal or of a constant is a node of
-- another tree (LRM08 9.4.2, 16.2.2).
entity image_param is
end entity;

architecture behav of image_param is
  constant c : integer := 2;
  constant cb : boolean := true;
  type barr is array (boolean) of integer;
  constant ba : barr := (1, 2);
  constant k1 : string := bit'image('1');
  constant k2 : string := boolean'image(cb);
  constant k3 : string := integer'image(c);
  constant k4 : string := boolean'image((true));
  constant k5 : string := boolean'image(boolean'(false));
  constant k6 : character := boolean'image(cb)(1);
  constant k7 : integer := integer'value(integer'image(c));
  constant k8 : integer := ba(true);
begin
  process
  begin
    assert k1 = "'1'" severity failure;
    assert k2 = "true" severity failure;
    assert k3 = "2" severity failure;
    assert k4 = "true" severity failure;
    assert k5 = "false" severity failure;
    assert k6 = 't' severity failure;
    assert k7 = 2 severity failure;
    assert k8 = 2 severity failure;
    assert character'image('a') = "'a'" severity failure;
    assert ba(cb) = 2 severity failure;
    report "PASS";
    wait;
  end process;
end architecture;
