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

      # nix develop로 emacs-igc + export/verify 도구를 PATH에 넣기
      # garden export 파이프라인은 여기 응집 — notes 리포는 가든 도구가 바뀔 수 있으나
      # 작성/내보내기/검증은 doomemacs-config 측에서 안정적으로 운영한다.
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          emacs-igc-with-packages
          # bin/verify-*.py + 가든 link/content 검증
          (pkgs.python3.withPackages (ps: with ps; [ pyyaml ]))
          # 외부 link rot / redirect chain / deprecated host 추적
          pkgs.lychee
        ];
        shellHook = ''
          echo "Emacs IGC (MPS GC) available:"
          emacs --version | head -1
          echo "  IGC: $(emacs --batch --eval '(princ (featurep (quote igc)))' 2>/dev/null)"
          echo "  lychee: $(lychee --version 2>/dev/null | head -1)"
          echo ""
          echo "Usage:"
          echo "  emacs                    # 직접 실행"
          echo "  emacs --daemon=doom-igc  # daemon 모드"
          echo "  emacsclient -s doom-igc  # 접속"
          echo "  ./run.sh verify          # 가든 검증 (relref + content + lychee)"
          echo "  ./run.sh fix             # 자동 정정 (단계별 y/N)"
        '';
      };
    };
}
