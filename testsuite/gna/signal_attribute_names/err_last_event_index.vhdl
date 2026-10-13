-- An index on 'LAST_EVENT, whose value is not an array (LRM08 8.4, 16.2.4).
entity err_last_event_index is
end entity;

architecture bad of err_last_event_index is
  signal v : bit_vector(0 to 3);
begin
  process
  begin
    assert v'last_event(0) = 0 ns;
    wait;
  end process;
end architecture;
