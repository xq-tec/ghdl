-- The pathname expression '0' is BIT or CHARACTER: the complete context is
-- ambiguous (LRM08 8.7, 12.5).
entity err_pathname_ambiguous is
end entity;

architecture bad of err_pathname_ambiguous is
begin
  g1 : for i in 0 to 1 generate
    alias ext_s is <<signal .err_pathname_ambiguous.g1('0').s : bit>>;
    signal s : bit;
  begin
  end generate;
end architecture;
