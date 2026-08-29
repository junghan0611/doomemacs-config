{
  description = "Doom Emacs config — Emacs 31 channel";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      # Nixpkgs ships the released Emacs 31.  This machine runs i3/X11 and uses
      # GTK input-method integration, so choose GTK3 explicitly rather than
      # relying on the generic `emacs31` alias.
      # Output name `emacs-unstable` remains for launcher/alias compatibility.
      emacs-31 = pkgs.emacs31-gtk3;

      emacs-preview-with-packages = (pkgs.emacsPackagesFor emacs-31).emacsWithPackages (epkgs: [
        epkgs.vterm
      ]);
    in
    {
      packages.${system} = {
        emacs-unstable = emacs-preview-with-packages;
        emacs-31 = emacs-preview-with-packages;
        default = emacs-preview-with-packages;
      };

      # nix develop로 emacs-unstable + export/verify 도구를 PATH에 넣기.
      # garden export 파이프라인은 여기 응집 — notes 리포는 가든 도구가 바뀔 수
      # 있으나 작성/내보내기/검증은 doomemacs-config 측에서 안정적으로 운영한다.
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          emacs-preview-with-packages
          # bin/verify-*.py + 가든 link/content 검증
          (pkgs.python3.withPackages (ps: with ps; [ pyyaml ]))
          # 외부 link rot / redirect chain / deprecated host 추적
          pkgs.lychee
        ];
        shellHook = ''
          echo "Emacs preview available:"
          emacs --version | head -1
          echo "  lychee: $(lychee --version 2>/dev/null | head -1)"
          echo ""
          echo "Usage:"
          echo "  emacs                         # 직접 실행"
          echo "  emacs --daemon=doom-unstable  # daemon 모드"
          echo "  emacsclient -s doom-unstable  # 접속"
          echo "  ./run.sh verify               # 가든 검증 (relref + content + lychee)"
          echo "  ./run.sh fix                  # 자동 정정 (단계별 y/N)"
        '';
      };
    };
}
