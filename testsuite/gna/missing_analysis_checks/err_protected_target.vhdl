-- The target of a variable assignment shall not be of a protected type
-- (LRM08 10.6.2.1).
entity err_protected_target is
end entity;

architecture behav of err_protected_target is
  type pt is protected
    procedure set (n : integer);
  end protected;

  type pt is protected body
    variable v : integer := 0;
    procedure set (n : integer) is
    begin
      v := n;
    end procedure;
  end protected body;
begin
  process
    variable x, y : pt;
  begin
    x := y;
    wait;
  end process;
end architecture;
