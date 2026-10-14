-- Array attributes of a type prefix are locally static when the index
-- constraint is, whatever the element subtype (LRM08 9.4.2, LRM93 7.4.1).
-- TIME is globally static since VHDL-93, so are arrays of TIME.
package pkg is
  type distance is range 0 to 1000000
    units
      um;
      mm = 1000 um;
    end units;

  type rec is record
    t : time;
    n : integer;
  end record;

  subtype idx is integer range 3 to 6;

  type time_vector is array (idx range <>) of time;
  type distance_vector is array (natural range <>) of distance;
  type rec_vector is array (integer range <>) of rec;

  subtype time_4 is time_vector (idx);
  subtype distance_3 is distance_vector (2 downto 0);
  subtype rec_2 is rec_vector (-1 to 0);

  --  Aggregates with choices that are attributes of the subtypes.
  constant c_time : time_4 :=
    (time_4'left | time_4'right => 1 ns, others => 2 ns);
  constant c_dist : distance_3 :=
    (distance_3'high => 1 mm, distance_3'low => 2 um, others => 3 um);
  constant c_rec : rec_2 :=
    (rec_2'left => (1 ns, 1), others => (2 ns, 2));

  function sel_t (i : idx) return natural;
  function sel_d (i : natural) return natural;
  function sel_r (i : integer) return natural;
end pkg;

package body pkg is
  function sel_t (i : idx) return natural is
  begin
    case i is
      when time_4'left => return 1;
      when time_4'right => return 2;
      when others => return 0;
    end case;
  end sel_t;

  function sel_d (i : natural) return natural is
  begin
    case i is
      when distance_3'left => return 1;
      when distance_3'right => return 2;
      when others => return 0;
    end case;
  end sel_d;

  function sel_r (i : integer) return natural is
  begin
    case i is
      when rec_2'low => return 1;
      when rec_2'high => return 2;
      when others => return 0;
    end case;
  end sel_r;
end pkg;

use work.pkg.all;

entity tb is
end tb;

architecture behav of tb is
begin
  process
    variable vt : time_4 := (time_4'left => 5 ns, others => 6 ns);
  begin
    assert c_time = (1 ns, 2 ns, 2 ns, 1 ns)
      report "c_time" severity failure;
    assert c_dist = (1 mm, 3 um, 2 um)
      report "c_dist" severity failure;
    assert c_rec (-1) = (1 ns, 1) and c_rec (0) = (2 ns, 2)
      report "c_rec" severity failure;
    assert vt (3) = 5 ns and vt (6) = 6 ns report "vt" severity failure;

    assert sel_t (3) = 1 and sel_t (6) = 2 and sel_t (4) = 0
      report "sel_t" severity failure;
    assert sel_d (2) = 1 and sel_d (0) = 2 and sel_d (1) = 0
      report "sel_d" severity failure;
    assert sel_r (-1) = 1 and sel_r (0) = 2 and sel_r (5) = 0
      report "sel_r" severity failure;
    wait;
  end process;
end behav;
