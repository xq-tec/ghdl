-- A selected name of 'LAST_VALUE of a record signal must name an element of
-- the record (LRM08 8.3).
entity err_last_value_element is
end entity;

architecture bad of err_last_value_element is
  type rec is record
    a : bit;
  end record;
  signal s : rec;
begin
  process
  begin
    assert s'last_value.c = '0';
    wait;
  end process;
end architecture;
