{ mkDerivation, aeson, aeson-pretty, base, bytestring, conduit
, containers, errors, filepath, lens, libarchive-conduit, mtl
, optparse-applicative, process, regex-tdfa, resourcet, semigroups
, stdenv, text, transformers, xml
}:
mkDerivation {
  pname = "autonix-deps";
  version = "0.2.0.0";
  src = ./.;
  buildDepends = [
    aeson aeson-pretty base bytestring conduit containers errors
    filepath lens libarchive-conduit mtl optparse-applicative process
    regex-tdfa resourcet semigroups text transformers xml
  ];
  description = "Library for Nix expression dependency generation";
  license = stdenv.lib.licenses.bsd3;
}
