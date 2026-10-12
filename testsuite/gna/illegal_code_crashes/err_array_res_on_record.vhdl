-- An array element resolution on a record subtype (LRM08 6.3).
entity err_array_res_on_record is
end entity;

architecture bad of err_array_res_on_record is
  function resolved (s : bit_vector) return bit is
  begin
    return s (s'left);
  end function;

  type rec is record
    a : bit;
  end record;

  subtype bad_t is (resolved) rec;
begin
end architecture;
