-- Two uninstantiated subprograms with the same parameter and result type
-- profile are homographs (LRM08 4.5.1), even if their generic lists differ.
package err_homograph is
  procedure p generic (type t) (x : integer);
  procedure p generic (type u) (x : integer);
end package;
