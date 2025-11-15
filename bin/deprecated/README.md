# Deprecated Scripts

이 폴더에는 더 이상 사용하지 않는 Bash 기반 export 스크립트들이 있습니다.

## ❌ Deprecated 이유

### 1. NBSP (U+00A0) 인코딩 문제

Denote 파일명에 NBSP (non-breaking space, U+00A0)가 포함된 경우:

```bash
# Bash 변수 전달 시 깨짐
file="20231015T120000--title with NBSP.org"  # ❌ 인코딩 깨짐
emacsclient --eval "(denote-export-file \"$file\")"  # 실패
```

Python은 완벽 처리:
```python
org_files = directory.glob("*.org")  # ✅ NBSP 안전
```

### 2. 성능 문제

**denote-export-parallel.sh (Bash N개 서버)**:
- 서버 초기화 타이밍 문제
- Ready 플래그 없음
- 결과: 빈 파일 (42 bytes) 대량 생성

**denote-export-safe.sh (순차 처리)**:
- 각 파일마다 emacs 프로세스 시작
- 속도: 0.1 files/sec
- 2008 파일 처리: ~5시간

Python multi-daemon:
- 속도: 1.8 files/sec
- 2008 파일 처리: ~18분
- **18배 빠름**

### 3. 복잡한 상태 관리

Bash로 여러 daemon 관리:
- 각 daemon ready 확인 어려움
- 파일 분산 로직 복잡
- 에러 처리 취약

Python ProcessPoolExecutor:
- 자동 병렬 관리
- Round-robin 분산
- 강력한 에러 처리

## 📂 파일 목록

| 파일 | 목적 | 대체 방법 |
|------|------|----------|
| `denote-export-parallel.sh` | Bash 병렬 처리 | `../denote-export-parallel.py` |
| `denote-export-safe.sh` | Bash 순차 처리 | `../denote-export-parallel.py` (1 daemon) |
| `denote-export-folder.sh` | Bash 폴더별 | `../denote-export-parallel.py` |
| `README-SAFE-EXPORT.md` | 안전 실행 가이드 | `../README.md` |

## 🔄 마이그레이션

### Before (Bash)

```bash
./bin/denote-export-safe.sh
# 문제: 5시간 소요, NBSP 파일 실패
```

### After (Python)

```bash
python3 bin/denote-export-parallel.py ~/org 4
# 개선: 18분 소요, 100% 성공
```

## 📚 참고 자료

개발 과정 상세: `../EXPORT-PARALLEL.org`

주요 커밋:
- `7bb726c`: Bash multi-daemon 구현 (실패)
- `51af58e`: Python 전환 (성공)
- `ff7c602`: Production 버그 해결
- `db84438`: Date field 중복 방지 (최신)

---

**보관 이유**: 개발 과정 및 실패 사례 참고용
**상태**: ⛔ Deprecated, 사용 금지
**마지막 업데이트**: 2025-11-15
