-- The result of a method must not be of an access type (LRM08 5.6.2), also
-- for a method that is a function instantiation.
entity err_access_return is
end entity;

architecture behav of err_access_return is
  type int_acc is access integer;

  function gen generic (type t) (x : integer) return t is
    variable r : t;
  begin
    return r;
  end function;

  type pt is protected
    function acc_gen is new gen generic map (t => int_acc);
  end protected;

  type pt is protected body
  end protected body;
begin
end architecture;
