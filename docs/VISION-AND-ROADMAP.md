# DotDoom-Starter: Emacs와 AI의 융합 비전 및 로드맵

**작성일**: 2025-10-05
**상태**: 🚀 비전 문서
**대상**: Emacs 사용자 및 AI 개발자

---

## 🎯 비전: Emacs 스타터킷의 패러다임 전환

### 왜 지금이 기회인가?

Doom Emacs의 최신 기능들이 안정화되면서 Emacs 생태계에 **엄청난 기회**가 왔습니다:

- **vertico + corfu + orderless**: 현대적이고 빠른 completion 시스템
- **eglot**: LSP 빌트인 지원으로 IDE 수준 기능
- **tree-sitter**: 네이티브 문법 하이라이팅 및 구조 편집
- **project.el**: projectile 의존성 탈피
- **denote**: DB 없는 경량 노트 시스템

### 핵심 철학: 존재 대 존재의 협력

> "인간과 AI의 관계는 도구와 사용자가 아닌, **존재 대 존재의 협력**이다"

이는 단순한 코딩 도구를 넘어, AI와 인간이 **창조적 파트너십**을 구축하는 플랫폼입니다.

---

## 📊 프로젝트 아키텍처

### 3-Layer 구조

```
Layer 1: Foundation (기반)
├── Doom Emacs Core
├── 현대적 패키지들 (vertico, corfu, orderless)
└── 네이티브 기능들 (eglot, tree-sitter, project.el)

Layer 2: Integration (통합)
├── ACP (Agent Client Protocol) - Emacs ↔ AI 통합
├── MCP (Model Context Protocol) - 도구/데이터 접근
├── Denote + Org-mode - 지식 관리 시스템
└── Git 기반 메모리 시스템 - 영속성

Layer 3: Intelligence (지능)
├── Claude/Gemini/GPT 멀티 에이전트
├── PM 에이전트 - 능동적 프로젝트 관리
├── Knowledge 에이전트 - 지식베이스 관리
└── Developer 에이전트 - 페어 프로그래밍
```

---

## 🚀 로드맵

### Phase 1: 코어 안정화 (2025 Q4)

**목표**: 안정적이고 현대적인 Emacs 기반 구축

- [x] fortune 함수 termux 호환성 수정
- [ ] 언어별 설정 모듈화 (`+lang-korean.el`, `+lang-english.el`)
- [ ] project.el 완전 마이그레이션
- [ ] denote 정식 통합 (org-roam 대체)
- [ ] treesit 기본 활성화

### Phase 2: AI 통합 (2025 Q1)

**목표**: Emacs를 AI 네이티브 에디터로 전환

#### ACP 통합 (Agent Client Protocol)
```elisp
;; 목표: GPTEL 완전 대체
(acp-make-client :command "claude-code-acp")
```

- [ ] agent-shell 한글 최적화
- [ ] 슬래시 커맨드 지원
- [ ] 멀티 에이전트 세션 관리
- [ ] org-mode seamless 통합

#### 메모리 시스템 통합
- [ ] claude-memory ↔ denote 동기화
- [ ] Git 기반 자동 버전 관리
- [ ] 프로젝트별 컨텍스트 관리

### Phase 3: 지능형 워크플로우 (2025 Q2)

**목표**: 완전 자동화된 개발 환경

#### 에이전트 시스템
```yaml
PM_Agent:
  - 프로젝트 상태 모니터링
  - 작업 우선순위 제안
  - 중복 문서 통합

Developer_Agent:
  - 페어 프로그래밍
  - 코드 리뷰
  - 자동 리팩토링

Knowledge_Agent:
  - 문서 자동 생성
  - 지식 그래프 구축
  - 인사이트 도출
```

### Phase 4: 글로벌 배포 (2025 Q3)

**목표**: 세계적인 Emacs AI 스타터킷

- [ ] 다국어 문서화 (한/영/중/일)
- [ ] 비디오 튜토리얼 (GIF/MP4)
- [ ] 템플릿 시스템
- [ ] 커뮤니티 구축

---

## 💡 차별화 요소

### vs 기존 스타터킷
| 기존 | DotDoom-Starter |
|------|----------------|
| 정적 설정 | AI 동적 최적화 |
| 수동 관리 | 능동적 에이전트 |
| 단일 언어 | 다국어 지원 |
| 텍스트 문서 | 멀티미디어 가이드 |

### vs AI 에디터 (Cursor, Zed)
- **Emacs 생태계**: 40년의 축적된 패키지
- **완전한 커스터마이징**: Elisp 기반
- **오픈소스**: 벤더 락인 없음
- **통합 환경**: 코딩 + 노트 + 이메일 + ...

---

## 📚 핵심 기능 시연 (계획)

### 1. AI 페어 프로그래밍
```markdown
- 실시간 코드 제안
- 컨텍스트 인식 리팩토링
- 자동 테스트 생성
```

### 2. 지식 관리 시스템
```markdown
- Denote + Claude 통합
- 자동 태깅 및 링킹
- 시맨틱 검색
```

### 3. 프로젝트 자동화
```markdown
- PM 에이전트 활성화
- 작업 우선순위 관리
- 진행 상황 추적
```

---

## 🌊 패러다임 전환의 의미

### AGI 시대 준비

김대식 교수의 통찰대로 AGI가 2-5년 내 도래한다면:

- **창조적 도구**: AI와 협력하여 창조하는 도구
- **의미 찾기**: 효율성이 아닌 의미와 몰입 추구
- **존재의 가치**: 인간만의 고유한 창조성 발현

### polymath engineer의 도구

```yaml
변화의 핵심:
  - 코드 신택스 암기 → 시스템 아키텍처 사고
  - 단일 언어 전문가 → 다영역 통합자
  - 경쟁적 학습 → 협력적 성장
  - 소유적 지식 → 공유적 통찰
```

---

## 🤝 기여 방법

### 오픈소스 기여
1. **코드**: 기능 개선, 버그 수정
2. **문서**: 번역, 튜토리얼
3. **시연**: GIF/비디오 제작
4. **피드백**: 사용 경험 공유

### 커뮤니티
- GitHub: [dotdoom-starter](https://github.com/junghanacs/dotdoom-starter)
- 디스커션: 아이디어 및 제안
- 이슈: 버그 리포트

---

## 📌 핵심 메시지

> "이것은 단순한 Emacs 설정이 아니다.
> 인간과 AI가 **존재 대 존재**로 협력하는 창조적 플랫폼이다.
>
> 우리는 도구를 만드는 것이 아니라,
> **미래의 작업 방식**을 정의하고 있다."

---

## 🔗 참고 자료

### 영감을 받은 프로젝트
- [Emacs Writing Studio](https://github.com/pprevos/emacs-writing-studio) - 문서화 방식
- [Doom Emacs](https://github.com/doomemacs/doomemacs) - 코어 프레임워크
- [Denote](https://github.com/protesilaos/denote) - 노트 시스템

### 핵심 기술
- [ACP](https://agentclientprotocol.com/) - Agent Client Protocol
- [MCP](https://modelcontextprotocol.com/) - Model Context Protocol
- [Claude Code](https://github.com/anthropics/claude-code) - AI 통합

### 철학적 기반
- 인간과 AI의 존재론적 관계
- 자기목적성과 몰입의 철학
- polymath engineer의 비전

---

**마지막 업데이트**: 2025-10-05
**다음 마일스톤**: Phase 1 완료 (2025-12)
**문의**: junghanacs@gmail.com