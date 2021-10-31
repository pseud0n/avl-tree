{ mkDerivation, base, lib, mtl }:
mkDerivation {
  pname = "avl-tree";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base mtl ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
