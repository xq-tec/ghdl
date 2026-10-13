-- A record element resolution of an element of an access type (LRM08 6.3).
entity err_access_element is
end entity;

architecture bad of err_access_element is
  type bv is array (natural range <>) of bit;

  function res_or (v : bv) return bit is
  begin
    return v (v'left);
  end function;

  type ptr is access integer;

  type rec is record
    p : ptr;
    a : bit;
  end record;

  subtype bad_t is (p res_or) rec;
begin
end architecture;
