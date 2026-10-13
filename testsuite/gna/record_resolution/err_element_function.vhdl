-- The resolution function of a record element resolution must resolve the
-- element subtype (LRM08 4.6, 6.3).
entity err_element_function is
end entity;

architecture bad of err_element_function is
  type bv is array (natural range <>) of bit;

  function res_or (v : bv) return bit is
  begin
    return v (v'left);
  end function;

  type rec is record
    a : bit;
    b : integer;
  end record;

  subtype bad_t is (b res_or) rec;
begin
end architecture;
