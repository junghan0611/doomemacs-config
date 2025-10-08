# DotDoom-Starter 프로젝트 구조 개선안

**참고**: Emacs Writing Studio (EWS) 프로젝트 구조
**작성일**: 2025-10-05

---

## 📁 제안하는 디렉토리 구조

```
dotdoom-starter/
├── README.md                 # 메인 소개 (영문)
├── README-KO.md              # 한국어 소개
├── CHANGELOG.md              # 변경 이력
├── LICENSE                   # MIT 라이선스
│
├── docs/                     # 📚 문서 (EWS 스타일)
│   ├── VISION-AND-ROADMAP.md  # 비전과 로드맵
│   ├── PROJECT-STRUCTURE.md   # 본 문서
│   ├── getting-started/       # 시작 가이드
│   │   ├── installation.md    # 설치 가이드
│   │   ├── first-steps.md     # 첫 걸음
│   │   └── troubleshooting.md # 문제 해결
│   ├── features/              # 기능별 가이드
│   │   ├── ai-integration.md  # AI 통합 (ACP/MCP)
│   │   ├── note-taking.md     # Denote 노트 시스템
│   │   ├── development.md     # 개발 환경
│   │   └── writing.md         # 글쓰기 환경
│   └── demos/                # 🎬 시연 자료
│       ├── README.md          # 데모 목록
│       └── gifs/              # GIF 애니메이션
│
├── config.el                 # 메인 설정
├── init.el                   # Doom 모듈 선택
├── packages.el               # 패키지 정의
│
├── modules/                  # 🔧 모듈화된 설정
│   ├── +ui.el               # UI 관련 설정
│   ├── +completion.el       # vertico/corfu/orderless
│   ├── +editor.el           # 편집 기능
│   ├── +ai.el               # AI 통합 (ACP/MCP)
│   ├── +notes.el            # Denote/Org 설정
│   ├── +git.el              # Magit/메모리 시스템
│   └── lang/                # 언어별 설정
│       ├── +korean.el       # 한국어 설정
│       ├── +english.el      # 영어 설정
│       └── +programming.el  # 프로그래밍 언어
│
├── lisp/                    # 📦 커스텀 함수/라이브러리
│   ├── lib-ai.el           # AI 관련 함수
│   ├── lib-denote.el       # Denote 확장
│   ├── lib-project.el      # 프로젝트 관리
│   └── per-machine.el      # 머신별 설정
│
├── templates/               # 📄 템플릿
│   ├── CLAUDE.md           # Claude 지침서 템플릿
│   ├── project/            # 프로젝트 템플릿
│   └── notes/              # 노트 템플릿
│
├── snippets/               # 코드 스니펫
│   └── ...
│
├── scripts/                # 🛠️ 유틸리티 스크립트
│   ├── install.sh          # 자동 설치
│   ├── update.sh           # 업데이트
│   └── test.sh             # 테스트
│
└── examples/               # 💡 예제
    ├── workflows/          # 워크플로우 예제
    ├── configs/            # 설정 예제
    └── projects/           # 프로젝트 예제
```

---

## 🎯 EWS에서 배운 핵심 요소

### 1. 문서 우선 접근 (Documentation-First)

#### 현재 문제점
- 문서가 코드에 비해 부족
- 시작하기 어려움
- 기능 발견이 어려움

#### 개선 방안
```markdown
docs/
├── getting-started/     # 초보자 친화적
├── features/           # 기능별 상세 가이드
├── tutorials/          # 단계별 튜토리얼
├── reference/          # API 레퍼런스
└── contributing/       # 기여 가이드
```

### 2. 멀티미디어 가이드

#### GIF 데모 시스템
```markdown
docs/demos/
├── README.md           # 데모 인덱스
├── gifs/
│   ├── ai-completion.gif    # AI 자동완성
│   ├── denote-workflow.gif  # 노트 작성 플로우
│   ├── project-management.gif # 프로젝트 관리
│   └── multi-agent.gif      # 멀티 에이전트 협업
└── videos/            # 심화 비디오 (옵션)
```

### 3. 모듈화 구조

#### 핵심 모듈 분리
```elisp
;; modules/+ai.el
(defun my/setup-ai-integration ()
  "AI 통합 설정"
  ;; ACP 설정
  ;; MCP 설정
  ;; 에이전트 설정
  )

;; modules/+notes.el
(defun my/setup-note-system ()
  "노트 시스템 설정"
  ;; Denote 설정
  ;; Org-mode 통합
  ;; 메모리 시스템 연동
  )
```

### 4. 언어별 설정 분리

```elisp
;; modules/lang/+korean.el
(defun my/setup-korean ()
  "한국어 환경 설정"
  ;; 입력기 설정
  ;; 폰트 설정
  ;; 맞춤법 검사
  )

;; 사용자는 필요한 언어만 로드
(when (require 'lang/+korean nil t)
  (my/setup-korean))
```

---

## 📊 개선 로드맵

### Phase 1: 구조 개선 (1주)
- [ ] 디렉토리 구조 재정리
- [ ] 모듈 분리 작업
- [ ] 기본 문서 작성

### Phase 2: 문서화 (2주)
- [ ] Getting Started 가이드
- [ ] 기능별 가이드 작성
- [ ] GIF 데모 제작

### Phase 3: 템플릿화 (3주)
- [ ] 프로젝트 템플릿
- [ ] 설정 템플릿
- [ ] 워크플로우 템플릿

### Phase 4: 자동화 (4주)
- [ ] 설치 스크립트
- [ ] 테스트 자동화
- [ ] CI/CD 구축

---

## 💡 EWS와의 차별화

### EWS (Writing 중심)
- 학술 글쓰기 특화
- LaTeX/BibTeX 통합
- 출판 워크플로우

### DotDoom-Starter (AI + 개발 중심)
- AI 에이전트 통합
- 개발 환경 최적화
- 지식 관리 시스템
- 다국어 지원

---

## 🚀 즉시 실행 가능한 개선

### 1. README 개선
```markdown
# DotDoom-Starter

<p align="center">
  <img src="docs/demos/logo.png" width="200">
</p>

<p align="center">
  <a href="#features">Features</a> •
  <a href="#installation">Installation</a> •
  <a href="#demos">Demos</a> •
  <a href="docs/">Documentation</a>
</p>

## ✨ Features at a Glance

### 🤖 AI Integration
![AI Demo](docs/demos/gifs/ai-completion.gif)

### 📝 Note Taking
![Denote Demo](docs/demos/gifs/denote-workflow.gif)

### 🚀 Quick Start
\`\`\`bash
git clone https://github.com/junghanacs/dotdoom-starter
cd dotdoom-starter
./scripts/install.sh
\`\`\`
```

### 2. 한국어 README 강화
- 한국 사용자를 위한 상세 가이드
- 한글 입력 설정 특화
- 국내 커뮤니티 연결

### 3. 인터랙티브 설치
```bash
#!/bin/bash
# scripts/install.sh

echo "🎯 DotDoom-Starter 설치를 시작합니다"
echo ""
echo "설치 옵션을 선택하세요:"
echo "1) 기본 설치 (추천)"
echo "2) AI 기능 포함"
echo "3) 전체 기능"
echo "4) 커스텀 설치"
```

---

## 📚 참고 자료

### EWS의 장점
- **체계적 문서**: 초보자도 쉽게 시작
- **목적 명확**: Writing에 특화
- **커뮤니티**: 활발한 피드백

### 우리의 방향
- **AI 네이티브**: 처음부터 AI 통합
- **개발자 친화**: 코딩 환경 최적화
- **글로벌 지향**: 다국어 지원

---

## 🎬 데모 제작 계획

### 우선순위 데모
1. **5분 퀵스타트**: 설치부터 첫 사용까지
2. **AI 페어 프로그래밍**: Claude와 코딩하기
3. **노트 시스템**: Denote로 지식 관리
4. **프로젝트 관리**: PM 에이전트 활용

### 제작 도구
- **asciinema**: 터미널 녹화
- **peek**: GIF 생성
- **OBS**: 비디오 녹화

---

## 📈 성공 지표

### 정량적 지표
- GitHub 스타: 1000+ (6개월)
- 활성 사용자: 100+ (3개월)
- 기여자: 10+ (6개월)

### 정성적 지표
- "Emacs + AI 최고의 스타터킷"
- 초보자 친화적
- 활발한 커뮤니티

---

**결론**: EWS의 문서 중심 접근과 우리의 AI 통합 비전을 결합하여,
**차세대 Emacs 스타터킷**을 만들 수 있습니다.