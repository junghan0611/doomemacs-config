{
  description = "Doom Emacs config — Emacs 31 preview channel";

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

      # Emacs 31 preview channel — emacs-overlay의 `emacs-unstable` attribute는
      # latest stable release tag를 따라가므로 현재 30.2에 머문다. `emacs-git`은
      # upstream master snapshot이라 이미 32.0.50일 수 있으므로, 31 pre-release를
      # 원하면 Savannah `emacs-31` release branch를 명시적으로 고정한다.
      # Output name `emacs-unstable`은 기존 launcher/alias 호환을 위해 유지.
      #
      # 핀은 브랜치 HEAD가 아니라 **pretest tag 커밋**에 박는다. HEAD는 태그 이후
      # 커밋이 계속 쌓여 재현 자리가 흔들린다. 다음 pretest로 올릴 때:
      #   git ls-remote https://git.savannah.gnu.org/git/emacs.git 'refs/tags/emacs-31.0.*'
      # 에서 `^{}` 붙은 (peeled) 커밋을 rev 로 쓰고, hash 는 더미로 바꿔 `nix build`
      # 실패 메시지의 `got:` 값을 옮긴다.
      emacs-31 = pkgs.emacs-git.overrideAttrs (_old: {
        pname = "emacs-31";
        name = "emacs-31-31.0.91";
        version = "31.0.91";
        src = pkgs.fetchgit {
          url = "https://git.savannah.gnu.org/git/emacs.git";
          # refs/tags/emacs-31.0.91^{} — pretest 2 (2026-07-24)
          rev = "57581b8bc2f73229d1f03dd5655aabb4a6de6183";
          hash = "sha256-3nvCiLiEtII1C57CLfDIbVqhiwadYViF9Nv32yDtLIQ=";
        };
      });

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
