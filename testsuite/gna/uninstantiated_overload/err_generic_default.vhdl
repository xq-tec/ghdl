-- The defaults of generic subprograms must conform (LRM08 4.10).
package err_generic_default is
  procedure p generic (function f return integer is <>) (x : integer);
end package;

package body err_generic_default is
  procedure p generic (function f return integer) (x : integer) is
  begin
  end procedure;
end package body;
