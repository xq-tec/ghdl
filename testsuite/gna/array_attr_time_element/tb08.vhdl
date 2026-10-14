-- VHDL-2008 element constraints: the attribute is locally static when the
-- index constraint and the element constraints are, whatever the scalar
-- element subtypes (here TIME, which is globally static).
entity tb08 is
end tb08;

architecture behav of tb08 is
  type time_vector is array (natural range <>) of time;
  type time_matrix is array (natural range <>) of time_vector;
  type rec is record
    t : time;
    v : time_vector;
  end record;
  type rec_vector is array (natural range <>) of rec;
  type time_2d is array (natural range <>, natural range <>) of time;

  subtype m is time_matrix (1 to 2) (0 to 3);
  subtype r is rec_vector (4 downto 3) (v (0 to 1));
  subtype t2 is time_2d (1 to 2, 5 to 7);

  function sel (i : natural) return natural is
  begin
    case i is
      when m'left => return 1;
      when r'right => return 2;
      when t2'high (2) => return 3;
      when others => return 0;
    end case;
  end sel;

  constant cm : m := (m'right => (others => 1 ns), others => (others => 2 ns));
begin
  process
  begin
    assert sel (1) = 1 and sel (3) = 2 and sel (7) = 3 and sel (0) = 0
      report "sel" severity failure;
    assert cm (1) (0) = 2 ns and cm (2) (3) = 1 ns
      report "cm" severity failure;
    wait;
  end process;
end behav;
