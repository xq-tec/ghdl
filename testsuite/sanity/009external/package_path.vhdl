package gpkg is
  generic (const : natural);
end package;

package ipkg is new work.gpkg generic map (const => 42);

entity package_path is
end entity;

architecture a of package_path is
begin
  check : process
  begin
    assert << constant @work.ipkg.const : natural >> = 42;
    wait;
  end process;
end architecture;
