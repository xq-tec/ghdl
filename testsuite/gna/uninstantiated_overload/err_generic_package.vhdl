-- The generic map aspects of generic packages must conform (LRM08 4.10).
package err_generic_package_gp is
  generic (n : integer);
end package;

package err_generic_package is
  procedure p generic (package q is new work.err_generic_package_gp
                         generic map (n => 1)) (x : integer);
end package;

package body err_generic_package is
  procedure p generic (package q is new work.err_generic_package_gp
                         generic map (n => 2)) (x : integer) is
  begin
  end procedure;
end package body;
