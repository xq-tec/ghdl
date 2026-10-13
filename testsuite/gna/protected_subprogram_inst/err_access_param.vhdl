-- The parameters of a method must not be of an access type (LRM08 5.6.2),
-- also for a method that is a subprogram instantiation.
entity err_access_param is
end entity;

architecture behav of err_access_param is
  type int_acc is access integer;

  procedure gen generic (type t) (v : t) is
  begin
  end procedure;

  type pt is protected
    procedure acc_gen is new gen generic map (t => int_acc);
  end protected;

  type pt is protected body
  end protected body;
begin
end architecture;
