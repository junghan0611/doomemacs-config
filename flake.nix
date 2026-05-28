{
  description = "Doom Emacs config — Emacs unstable channel (tracks next stable release)";

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

      # Emacs unstable channel — emacs-overlay의 `emacs-unstable` attribute는
      # Savannah의 latest release tag를 따라간다. 오늘은 30.2 (= system stable과
      # 동일), 31.1 정식 태그가 박히는 날 자동으로 31.1로 점프. "다음 안정판을
      # release day-1에 받는 채널"이라는 의도.
      emacs-unstable-with-packages = (pkgs.emacsPackagesFor pkgs.emacs-unstable).emacsWithPackages (epkgs: [
        epkgs.vterm
      ]);
    in
    {
      packages.${system} = {
        emacs-unstable = emacs-unstable-with-packages;
        default = emacs-unstable-with-packages;
      };

      # nix develop로 emacs-unstable + export/verify 도구를 PATH에 넣기.
      # garden export 파이프라인은 여기 응집 — notes 리포는 가든 도구가 바뀔 수
      # 있으나 작성/내보내기/검증은 doomemacs-config 측에서 안정적으로 운영한다.
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          emacs-unstable-with-packages
          # bin/verify-*.py + 가든 link/content 검증
          (pkgs.python3.withPackages (ps: with ps; [ pyyaml ]))
          # 외부 link rot / redirect chain / deprecated host 추적
          pkgs.lychee
        ];
        shellHook = ''
          echo "Emacs unstable available:"
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
