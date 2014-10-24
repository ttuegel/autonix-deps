{ nixpkgs ? import ../../../.. {}
, haskellPackages ? nixpkgs.haskellPackages
}:

haskellPackages.callPackage ./default.nix {}
