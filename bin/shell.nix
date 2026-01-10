{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    python3
    python3Packages.pip
    emacs
  ];

  shellHook = ''
    echo "Denote Export Environment"
    echo "Python: $(python3 --version)"
    echo "Emacs: $(emacs --version | head -1)"

    # edge-tts 설치 확인 (없으면 안내)
    if ! command -v edge-tts &> /dev/null; then
      echo "edge-tts 미설치. 설치: pip install --user edge-tts"
    else
      echo "edge-tts: $(edge-tts --version 2>/dev/null || echo 'installed')"
    fi
  '';
}
