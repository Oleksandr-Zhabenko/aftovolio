{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.mkDerivation {
  pname = "aftovolio";
  version = "0.6.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = with pkgs.haskellPackages; [
    async base cli-arguments containers deepseq directory
    intermediate-structures lists-flines minmax mmsyn2-array
    monoid-insertleft quantizer rev-scientific rhythmic-sequences
    uniqueness-periods-vector-stats
  ];
  executableHaskellDepends = with pkgs.haskellPackages; [
    async base cli-arguments containers deepseq directory
    intermediate-structures lists-flines minmax mmsyn2-array
    monoid-insertleft quantizer rev-scientific rhythmic-sequences
    uniqueness-periods-vector-stats
  ];
  description = "An AFTOVolio implementation for creating texts with special phonetic / prosodic properties";
  license = pkgs.lib.licenses.mit;
}
