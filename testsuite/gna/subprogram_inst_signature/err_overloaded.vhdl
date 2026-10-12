-- An overloaded uninstantiated subprogram name requires a signature (LRM08 4.4).
entity err_overloaded is end entity;
architecture a of err_overloaded is
  procedure gen_p generic (type t) (x : t) is begin null; end procedure;
  procedure gen_p (x : integer; y : integer) is begin null; end procedure;
  procedure bad is new gen_p generic map (t => integer);
begin end architecture;
