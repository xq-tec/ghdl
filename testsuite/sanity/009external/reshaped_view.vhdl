--  External names whose subtype reshapes the object: (0 to 7) over
--  (7 downto 0).  The design export must print the alias types as vectors.
entity reshaped_child is
end entity;

architecture a of reshaped_child is
  signal inner : bit_vector(7 downto 0) := x"A5";
begin
end architecture;

entity reshaped_view is
end entity;

architecture a of reshaped_view is
begin
  u : entity work.reshaped_child;

  --  Alias in a block: elaborated with the design.
  b : block
    alias bv is << signal .reshaped_view.u.inner : bit_vector(0 to 7) >>;
  begin
    assert bv(0) = '1' and bv(7) = '1' severity failure;
  end block;

  p : process
    --  Alias in a process.
    alias pv is << signal .reshaped_view.u.inner : bit_vector(0 to 7) >>;
  begin
    --  External name without alias: implicit declaration of the process.
    assert << signal .reshaped_view.u.inner : bit_vector(0 to 7) >> = x"A5"
      severity failure;
    assert pv(0) = '1' and pv(1) = '0' severity failure;
    wait;
  end process;
end architecture;
