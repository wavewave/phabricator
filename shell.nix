{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  hsenv = haskellPackages.ghcWithPackages (p: with p; [
    aeson
    aeson-pretty
    lens
    wreq
  ]);

in

stdenv.mkDerivation {
  name = "phab-conduit-test";
  buildInputs = [ hsenv ];
}