{ mkDerivation, base, bytestring, conduit, containers, errors
, filepath, lens, libarchive-conduit, mtl, optparse-applicative
, process, regex-tdfa, resourcet, stdenv, transformers, xml
}:
mkDerivation {
  pname = "autonix-deps";
  version = "0.1.0.1";
  src = ./.;
  buildDepends = [
    base bytestring conduit containers errors filepath lens
    libarchive-conduit mtl optparse-applicative process regex-tdfa
    resourcet transformers xml
  ];
  description = "Library for Nix expression dependency generation";
  license = stdenv.lib.licenses.bsd3;
}
