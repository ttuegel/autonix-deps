{ nixpkgs ? import <nixpkgs> {}
, haskellPackages ? nixpkgs.haskellPackages
}:

haskellPackages.callPackage ./default.nix {
  libarchiveConduit = haskellPackages.callPackage ../libarchive-conduit {};
}
