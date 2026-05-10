#!/usr/bin/env python3
"""verify-figures.py — Hugo figure src 경로 검증기 + 선택적 수정기.

doomemacs-config/run.sh의 Verify 섹션에서 호출. notes 리포의 content/ 안
{{< figure src="..." >}} 패턴을 점검해서:
  - /images/foo.png   또는 https://... → ALIVE  (정상)
  - /home/junghan/... 또는 ~/... 등    → broken
    - 소스 파일 1건  찾음              → REWRITE     (정정 가능)
    - 소스 파일 N건  찾음              → AMBIGUOUS   (수동 확인)
    - 소스 파일 0건                    → DEAD        (원본 유실)

REWRITE 적용 시:
  1. 소스 파일을 ~/repos/gh/notes/static/images/{basename} 으로 복사
  2. content/**/*.md 의 figure src 를 /images/{basename} 으로 치환

회귀 맥락:
  2025-10-27 7a2175b 으로 헤드리스 export 데몬이 도입되면서 Doom org 모듈의
  org-attach-id-dir 설정이 누락 → attachment: 링크가 깨진 절대경로로 박힘.
  이 도구는 누적된 broken 패턴을 청소한다. Root cause 자체는
  workflow-shared.el의 my/org-attach-id-dir SSOT 가 막는다.

Usage:
    verify-figures.py CONTENT_DIR              # 검증 리포트
    verify-figures.py CONTENT_DIR --fix        # dry-run 수정
    verify-figures.py CONTENT_DIR --fix --apply  # 실제 수정
"""

import argparse
import hashlib
import re
import shutil
import sys
from collections import defaultdict
from pathlib import Path

# ━━━ Configuration ━━━

HOME = Path.home()

# 깨진 figure src의 basename을 찾을 디렉토리
SEARCH_DIRS = [
    HOME / "screenshot",          # org-download-image-dir
    HOME / "org" / ".attach",     # org-attach-id-dir
]

# REWRITE 시 파일 복사 대상 (Quartz static/)
STATIC_IMAGES = HOME / "repos" / "gh" / "notes" / "static" / "images"

FIGURE_RE = re.compile(r'\{\{<\s*figure\s+src="([^"]+)"[^>]*>\}\}')


# ━━━ Categorization ━━━

def is_alive(src: str) -> bool:
    """Hugo가 정상적으로 풀 수 있는 src."""
    return (
        src.startswith("/images/")
        or src.startswith("https://")
        or src.startswith("http://")
    )


def is_broken(src: str) -> bool:
    """배포되면 깨질 src 패턴."""
    return (
        src.startswith("/home/")
        or src.startswith("~")
        or src.startswith("file://")
        or src.startswith("../")
    )


def build_basename_index(search_dirs):
    """basename → list of full paths."""
    idx = defaultdict(list)
    for d in search_dirs:
        if not d.exists():
            continue
        for p in d.rglob("*"):
            if p.is_file():
                idx[p.name].append(p)
    return idx


def categorize(src: str, idx: dict):
    """(category, info) — info는 REWRITE면 Path, AMBIGUOUS면 list[Path]."""
    if is_alive(src):
        return ("ALIVE", None)
    if not is_broken(src):
        return ("UNKNOWN", None)
    basename = Path(src).name
    matches = idx.get(basename, [])
    if not matches:
        return ("DEAD", None)
    if len(matches) == 1:
        return ("REWRITE", matches[0])
    # 여러 매치라도 sha256가 모두 동일하면 (마이그레이션 잔재) 첫 매치로 정정.
    # 다르면 AMBIGUOUS — 사용자 수동 확인 필요.
    if all(_same_content(matches[0], m) for m in matches[1:]):
        return ("REWRITE", matches[0])
    return ("AMBIGUOUS", matches)


# ━━━ Scanning ━━━

def scan(content_dir: Path, idx: dict):
    """list of (md_path, line_num, src, category, info)."""
    items = []
    for md in sorted(content_dir.rglob("*.md")):
        try:
            text = md.read_text(encoding="utf-8")
        except Exception as e:
            print(f"  [READ-ERROR] {md}: {e}", file=sys.stderr)
            continue
        for m in FIGURE_RE.finditer(text):
            src = m.group(1)
            category, info = categorize(src, idx)
            line_num = text.count("\n", 0, m.start()) + 1
            items.append((md, line_num, src, category, info))
    return items


# ━━━ Reporting ━━━

def rel_to_content(md: Path, content_dir: Path) -> str:
    try:
        return str(md.relative_to(content_dir))
    except ValueError:
        return str(md)


def report_summary(items, content_dir: Path):
    counts = defaultdict(int)
    for _, _, _, cat, _ in items:
        counts[cat] += 1
    total = sum(counts.values())

    print("=== figure src verify ===")
    print(f"  total      : {total}")
    for cat in ["ALIVE", "REWRITE", "AMBIGUOUS", "DEAD", "UNKNOWN"]:
        n = counts.get(cat, 0)
        marker = "✓" if cat == "ALIVE" else ("⚠" if cat == "REWRITE" else "✗")
        if n > 0:
            print(f"  {marker} {cat:10s}: {n}")

    problems = [it for it in items if it[3] not in ("ALIVE",)]
    if not problems:
        print()
        print("✅ 모든 figure src 정상")
        return 0

    print()
    print(f"--- 상세 ({len(problems)}건) ---")
    for md, line, src, cat, info in problems:
        rel = rel_to_content(md, content_dir)
        if cat == "REWRITE":
            print(f"  [{cat:10s}] {rel}:{line}")
            print(f"               src      = {src}")
            print(f"               source   = {info}")
        elif cat == "AMBIGUOUS":
            print(f"  [{cat:10s}] {rel}:{line}  src={src}  ({len(info)} matches)")
            for m in info:
                print(f"               - {m}")
        else:
            print(f"  [{cat:10s}] {rel}:{line}  src={src}")

    # exit code: REWRITE 만 있으면 0 (--fix 로 정리 가능), DEAD/AMBIGUOUS 있으면 1
    return 0 if (counts.get("DEAD", 0) + counts.get("AMBIGUOUS", 0) + counts.get("UNKNOWN", 0)) == 0 else 1


def report_fix(items, content_dir: Path, apply: bool) -> int:
    rewrites = [it for it in items if it[3] == "REWRITE"]
    if not rewrites:
        print("REWRITE 대상 없음")
        return 0

    print(f"=== figure REWRITE: {len(rewrites)}건 ===")
    if apply:
        STATIC_IMAGES.mkdir(parents=True, exist_ok=True)

    # md 파일별로 묶어서 한 번에 치환
    by_md = defaultdict(list)
    for md, line, src, cat, source_path in rewrites:
        by_md[md].append((line, src, source_path))

    conflicts = 0
    copied = 0
    skipped = 0
    rewritten_files = 0

    for md, entries in by_md.items():
        rel = rel_to_content(md, content_dir)
        text = md.read_text(encoding="utf-8") if apply else None
        any_replaced = False
        for line, src, source_path in entries:
            basename = source_path.name
            target = STATIC_IMAGES / basename
            new_src = f"/images/{basename}"

            action = "COPY"
            if target.exists():
                if _same_content(target, source_path):
                    action = "SKIP-IDENT"
                else:
                    action = "CONFLICT"

            print(f"  [{action:11s}] {rel}:{line}")
            print(f"                src   = {src}")
            print(f"                src→  = {new_src}")
            if action == "CONFLICT":
                print(f"                ! {target} 이미 존재 (다른 내용) — 수동 처리")
                conflicts += 1
                continue

            if apply:
                if action == "COPY":
                    shutil.copy2(source_path, target)
                    copied += 1
                else:
                    skipped += 1
                # 같은 src 가 여러 번 나올 수 있어 replace 그대로
                if src in text:
                    text = text.replace(src, new_src)
                    any_replaced = True

        if apply and any_replaced:
            md.write_text(text, encoding="utf-8")
            rewritten_files += 1

    print()
    if apply:
        print(f"적용 완료: 복사 {copied}, 동일파일 skip {skipped}, "
              f"md 수정 {rewritten_files} 파일, 충돌 {conflicts}")
    else:
        print(f"--apply 추가 시 적용. 충돌 예상 {conflicts}건.")

    # exit code: 충돌이 있으면 1, 아니면 0
    return 1 if conflicts > 0 else 0


def _same_content(a: Path, b: Path) -> bool:
    try:
        return (
            hashlib.sha256(a.read_bytes()).digest()
            == hashlib.sha256(b.read_bytes()).digest()
        )
    except Exception:
        return False


# ━━━ Main ━━━

def main():
    p = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    p.add_argument("content_dir", type=Path, help="notes/content 디렉토리")
    p.add_argument("--fix", action="store_true",
                   help="REWRITE 가능한 항목을 정정 (dry-run)")
    p.add_argument("--apply", action="store_true",
                   help="--fix 와 함께 사용. 실제 파일 복사 + 치환")
    args = p.parse_args()

    if not args.content_dir.is_dir():
        print(f"content 디렉토리 없음: {args.content_dir}", file=sys.stderr)
        sys.exit(2)

    if args.apply and not args.fix:
        print("--apply 는 --fix 와 함께 사용", file=sys.stderr)
        sys.exit(2)

    print(f"검색 경로: {[str(d) for d in SEARCH_DIRS]}")
    print(f"정적 대상: {STATIC_IMAGES}")
    print()

    idx = build_basename_index(SEARCH_DIRS)
    items = scan(args.content_dir, idx)

    summary_rc = report_summary(items, args.content_dir)

    if args.fix:
        print()
        fix_rc = report_fix(items, args.content_dir, apply=args.apply)
        return fix_rc
    return summary_rc


if __name__ == "__main__":
    sys.exit(main())
