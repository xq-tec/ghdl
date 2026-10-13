-- 'DELAYED of an array signal is not a record (LRM08 8.3).
entity err_delayed_not_record is
end entity;

architecture bad of err_delayed_not_record is
  signal v : bit_vector(0 to 3);
  signal d : bit;
begin
  d <= v'delayed(1 ns).a;
end architecture;
