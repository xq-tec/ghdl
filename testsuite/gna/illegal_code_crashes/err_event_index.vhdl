-- An index on 'EVENT, whose value is not an array (LRM08 8.4).
entity err_event_index is
end entity;

architecture bad of err_event_index is
  signal s : bit;
  constant x : boolean := s'event (0);
begin
end architecture;
