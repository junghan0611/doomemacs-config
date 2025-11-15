# 안전한 Denote Export 가이드

## 개요

폴더별로 순차 실행하는 안전한 export 스크립트입니다.

## 특징

- ✅ **폴더별 순차 처리**: meta → bib → notes (한 번에 1개 폴더)
- ✅ **단일 daemon**: 프로세스 1개로 안전하게 실행
- ✅ **상세한 로그**: 성공/실패 기록
- ✅ **자동 cleanup**: Ctrl+C 시 daemon 자동 종료

## 사용 방법

### 1. 스크립트 실행

```bash
cd ~/repos/gh/doomemacs-config
./bin/denote-export-safe.sh
```

### 2. 백그라운드 실행 (잠자기 전)

```bash
cd ~/repos/gh/doomemacs-config
nohup ./bin/denote-export-safe.sh > /tmp/export-$(date +%Y%m%d-%H%M%S).out 2>&1 &

# 프로세스 ID 확인
echo $!

# 나중에 로그 확인
tail -f /tmp/export-*.out
```

### 3. 결과 확인

```bash
# 로그 파일 확인
cat /tmp/denote-export-safe-*.log | grep -E "ERROR|✗"

# Export된 파일 개수
ls ~/repos/gh/notes/content/meta/*.md | wc -l   # 530개 예상
ls ~/repos/gh/notes/content/bib/*.md | wc -l    # 649개 예상
ls ~/repos/gh/notes/content/notes/*.md | wc -l  # 782개 예상

# Git 변경사항 확인
cd ~/repos/gh/notes && git status --short | wc -l
```

## 실행 시간 예상

- Meta (530개): 약 15-20분
- Bib (649개): 약 18-24분
- Notes (782개): 약 22-28분

**총 예상 시간: 55-72분 (약 1-1.2시간)**

## 로그 파일 위치

```
/tmp/denote-export-safe-YYYYMMDD-HHMMSS.log
```

## 문제 발생 시

### Daemon이 응답하지 않을 때

```bash
# Daemon 강제 종료
pkill -f "denote-export-safe"

# 다시 시작
./bin/denote-export-safe.sh
```

### 특정 폴더만 재실행

스크립트 내에서 FOLDERS 배열을 수정:

```bash
# meta만
FOLDERS=("meta")

# bib, notes만
FOLDERS=("bib" "notes")
```

## 성공 확인

모든 파일이 정상적으로 export되면:

1. **파일 개수 확인**: 1961개 (530 + 649 + 782)
2. **빈 파일 없음**: 모두 정상 크기
3. **Git 변경사항**: git diff로 내용 확인
4. **CI/CD 준비 완료**: 자동화 가능

---

**잠자기 전 실행 추천:**

```bash
cd ~/repos/gh/doomemacs-config
nohup ./bin/denote-export-safe.sh > /tmp/export-night-$(date +%Y%m%d).out 2>&1 &
echo "프로세스 ID: $!"
echo "일어나서 확인: tail -f /tmp/export-night-*.out"
```
