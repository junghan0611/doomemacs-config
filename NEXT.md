# NEXT.md — doomemacs-config

> 시작할 때 무엇을 할지 몰라서 발생하는 진행 정체를 막는다.
> 일정은 의미 없다. 적은 만큼 할 수 있는 만큼만 — 진행은 진행된다.

운영 baseline은 [AGENTS.md](AGENTS.md). 후속 작업 / 미완 검증은 여기에.

---

## pi daemon에서 doom :lang org localleader 누락 (2026-05-13)

`./run.sh pi tty` (alias `ep`)로 띄운 client에서 org 버퍼의 `,` (localleader)
키바인딩이 사용자 정의 10개만 보이고 doom standard 32개(`,e` org-export-dispatch
등)가 통째로 빠진다. `emacs -nw`(daemon 없이 직접 실행, alias `et`)는 정상.

진단 (확정):
- pi(`emacs --daemon=pi`): `,` localleader = 10 entries, `, e` → nil
- user(GUI, `doom run`):  `,` localleader = 42 entries, `, e` → org-export-dispatch
- doom-loaded=t, modulep! :lang org=t — 모듈 자체는 enabled

가설:
1. doom :lang org의 `(after! org ... :localleader ...)`가 first-frame /
   tty-setup hook 의존 평가라 frame 없는 daemon init에서 누락
2. 우리 `keybindings-config.el`이 sparse keymap을 새로 박아 doom 정의가 들어갈
   자리를 잃음 — but GUI에서는 같은 코드인데 정상이므로 (1)이 더 유력

후속:
- `cmd_pi_start` 끝에 `emacsclient -s pi -e '(run-hooks ...)'` 시험
  (after-make-frame-functions / server-after-make-frame-hook 등)
- doom modules/lang/org/config.el에서 `(after! org)` 블록의 평가 trigger 확인
- fix 후 user/pi 양쪽에서 `(key-binding (kbd \", e\"))` 동일 → org-export-dispatch
  나오는지 회귀 테스트

우회 (현재): `alias et='emacs -nw'`. daemon 없이 직접 실행 — init 비용은 매번
들지만 키바인딩 완전.
