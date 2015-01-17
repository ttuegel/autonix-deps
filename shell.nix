with (import <nixpkgs> {});
with pkgs;
(haskellngPackages.callPackage ./. {
  libarchive-conduit = haskellngPackages.callPackage ./libarchive-conduit {
    archive = pkgs.libarchive;
  };
}).env
