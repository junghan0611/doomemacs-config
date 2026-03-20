{
  description = "Doom Emacs config — Emacs 31 IGC (MPS GC) for development";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, emacs-overlay }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ emacs-overlay.overlays.default ];
      };

      # Emacs 31 IGC (X11, --with-mps=yes)
      # emacs-overlay가 빌드한 바이너리를 그대로 가져옴
      emacs-igc-with-packages = (pkgs.emacsPackagesFor pkgs.emacs-igc).emacsWithPackages (epkgs: [
        epkgs.vterm
      ]);
    in
    {
      packages.${system} = {
        emacs-igc = emacs-igc-with-packages;
        default = emacs-igc-with-packages;
      };

      # nix develop로 emacs-igc를 PATH에 넣기
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [ emacs-igc-with-packages ];
        shellHook = ''
          echo "Emacs IGC (MPS GC) available:"
          emacs --version | head -1
          echo "  IGC: $(emacs --batch --eval '(princ (featurep (quote igc)))' 2>/dev/null)"
          echo ""
          echo "Usage:"
          echo "  emacs                    # 직접 실행"
          echo "  emacs --daemon=doom-igc  # daemon 모드"
          echo "  emacsclient -s doom-igc  # 접속"
        '';
      };
    };
}
