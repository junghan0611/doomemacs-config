# NOW - 진행 중인 작업

## 2025-11-22: Doom Emacs Config 리팩터링

### ✅ 완료한 작업

#### 1. 한글 입력 버그 수정
- **문제**: agent-shell에서 한글 유니코드 깨짐 (NFD 분해형)
- **원인**: `korean/enable-nfc-mode-if-needed`가 Termux 전용으로 제한됨
- **해결**: 모든 터미널 환경에서 NFC 정규화 활성화
- **추가**: agent-shell 버퍼를 실제 버퍼 목록에 표시
- **커밋**: `8f00e4a`

#### 2. hlissner 스타일 구조 재구성
**목표**: config.el을 간결하게 (1802줄 → 523줄, 71% 감소)

**디렉터리 구조**:
```
doomemacs-config/
├── autoload/
│   └── junghan.el              # 자동로드 함수 (;;;###autoload)
├── lisp/
│   ├── ai-agent-shell.el       # Agent Shell + claude-code-ide
│   ├── ai-gptel.el             # GPTel
│   ├── completion-config.el    # Corfu, Vertico, Consult
│   ├── denote-config.el        # Denote + citar-denote
│   ├── denote-export.el        # Denote export (기존)
│   ├── denote-silo.el          # Denote silo (기존)
│   ├── evil-config.el          # Evil 모드
│   ├── korean-input.el         # 한글 입력 전체 시스템
│   ├── org-config.el           # Org-mode 전체
│   └── ui-config.el            # UI (dashboard, modeline, themes, outli)
├── config.el (523줄)           # 간결한 메인 설정
├── init.el
├── packages.el
└── +keybindings.el
```

**파일 이동**:
- `+korean-input-fix.el` → `lisp/korean-input.el`
- `+gptel.el` → `lisp/ai-gptel.el`
- `+denote-silo-dynamic.el` → `lisp/denote-silo.el`
- `+denote-export.el` → `lisp/denote-export.el`
- `+functions.el` → `autoload/junghan.el`

**추출한 섹션**:
- Org-mode 전체 (agenda, capture, journal, citar)
- Denote 설정 (citar-denote 포함)
- Completion (corfu, vertico, consult)
- AI 도구 (agent-shell, claude-code-ide, gptel)
- Evil 설정 (evil, evil-escape, smartparens)
- UI 설정 (dashboard, modeline, themes, outli)
- Korean input 전체 (input method, 폰트, evil 연동)

**커밋**:
- `f80ca4e` - autoload/, lisp/ 기본 구조
- `39127f9` - org-config.el
- `8903550` - denote, completion, ai-agent-shell
- `592717c` - evil-config.el
- `ec5970f` - ui-config.el
- `8560d29` - evil+hangul, claude-code-ide 통합
- `6712294` - korean-input 전체 통합

**브랜치**: `refactor/doom-native`

### 🔲 다음 작업 (TODO)

#### 1. 남은 작은 섹션 정리
- [ ] `better default` → `lisp/defaults.el`
- [ ] `overide doomemacs` (dired, popup-rule, dabbrev) → `lisp/doom-overrides.el`
- [ ] 개발 도구 (flymake, eglot, elfeed, tempel, imenu-list) → `lisp/dev-tools.el`
- [ ] 플랫폼별 (TERMUX, term-keys, Terminal Mode, termux-fixes) → `lisp/platform-termux.el`
- [ ] 통합/유틸리티 (git/magit, tramp, MU4e, pass, notification, py3status) → `lisp/integrations.el`

#### 2. 키바인딩 통합
- [ ] `+keybindings.el` → `config.el`로 통합 (hlissner 스타일)
- [ ] 각 lisp 파일에 키바인딩 섹션 추가 검토

#### 3. 테스트 및 검증
- [ ] Emacs 재시작 후 모든 기능 동작 확인
- [ ] 누락된 설정 확인
- [ ] 로딩 순서 이슈 확인

#### 4. 문서화
- [ ] 각 lisp 파일 Commentary 섹션 보강
- [ ] README 업데이트 (새 구조 설명)
- [ ] glg-config 관련 파일 정리 (feature/glg-config-refactor 브랜치)

#### 5. NixOS 통합 준비
- hlissner-dotfiles-nix 스타일로 `config.local.el` 생성 방식 검토
- NixOS 모듈에서 머신별 설정 주입 방법 연구

### 💡 설계 원칙 (hlissner에서 배운 것)

1. **config.el은 간결하게**: 로딩 로직 + 필수 설정만
2. **autoload/ 활용**: `;;;###autoload`로 lazy loading
3. **lisp/ 분리**: 독립적인 라이브러리
4. **outline 구조**: `;;;` (level 1), `;;;;` (level 2)
5. **provide/require**: 모든 파일에 명시
6. **한 기능 = 한 파일**: 설정 분산 방지

### 📝 참고

- hlissner doom.d: `/home/goqual/sync/man/dotsamples/doom/hlissner-dot-doom/`
- hlissner nixos: `/home/goqual/sync/man/dotsamples/nixos/hlissner-dotfiles-nix/`
- fulllab config: `/home/goqual/sync/emacs/emacs-fulllab-config/`
