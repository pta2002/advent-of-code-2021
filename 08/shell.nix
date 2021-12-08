let
  pkgs = import <nixpkgs> { };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (p: [p.split]))
  ];
}
