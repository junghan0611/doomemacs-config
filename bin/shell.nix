{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    python3
    emacs
  ];

  shellHook = ''
    echo "Denote Export Environment"
    echo "Python: $(python3 --version)"
    echo "Emacs: $(emacs --version | head -1)"
  '';
}
