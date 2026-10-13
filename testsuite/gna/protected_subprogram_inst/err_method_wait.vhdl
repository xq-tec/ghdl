-- A method must not call a procedure instantiation whose uninstantiated
-- procedure contains a wait statement (LRM08 10.2, 4.3: an instantiated
-- subprogram is a parent of what its uninstantiated subprogram calls).
entity err_method_wait is
end entity;

architecture behav of err_method_wait is
  procedure gen generic (constant d : time) is
  begin
    wait for d;
  end procedure;

  type pt is protected
    procedure pause is new gen generic map (d => 1 ns);
    procedure m;
  end protected;

  type pt is protected body
    procedure m is
    begin
      pause;
    end procedure;
  end protected body;
begin
end architecture;
