-- A body with the same profile as an uninstantiated subprogram declaration
-- must conform to it (LRM08 4.10).
package err_nonconforming is
  procedure p generic (type t) (x : integer);
end package;

package body err_nonconforming is
  procedure p generic (type t) (constant x : in integer) is
  begin
  end procedure;
end package body;
