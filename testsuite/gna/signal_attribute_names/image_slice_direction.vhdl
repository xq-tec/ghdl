-- A slice of 'IMAGE whose direction differs from that of the index range of
-- STRING is an error (LRM08 8.5), also when it is folded.
entity image_slice_direction is
end entity;

architecture behav of image_slice_direction is
begin
  process
  begin
    assert integer'image(1234)(2 downto 1) = "23";
    wait;
  end process;
end architecture;
