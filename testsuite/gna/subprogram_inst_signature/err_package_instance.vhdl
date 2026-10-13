-- A subprogram declared in a package instance cannot be instantiated (not
-- supported).  The signature still matches it: its result type is the copy
-- of its generic type (LRM08 12.3 r).
package err_package_instance_gp is
  generic (n : integer);
  function f generic (type t) (x : t) return t;
end package;

package body err_package_instance_gp is
  function f generic (type t) (x : t) return t is
  begin
    return x;
  end function;
end package body;

entity err_package_instance is end entity;
architecture a of err_package_instance is
  package p is new work.err_package_instance_gp generic map (n => 1);
  function fi is new p.f [t return t] generic map (t => integer);
begin end architecture;
