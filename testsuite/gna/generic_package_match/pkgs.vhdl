-- Uninstantiated packages for the generic package matching tests.
package p_const is
  generic (
    g : integer := 1;
    r : real := 1.0;
    s : string := "ab";
    b : bit_vector := "01";
    e : character := 'a';
    tm : time := 1 ns
  );
  constant c : integer := g;
end package;

package p_type is
  generic (type t);
end package;

package p_sub is
  generic (function f (x : integer) return integer);
end package;

package p_pkg is
  generic (package ip is new work.p_const generic map (<>));
end package;

package p_def is
  generic (
    g : integer := 3;
    function inc (x : integer) return integer is <>
  );
  constant c : integer := inc (g);
end package;
