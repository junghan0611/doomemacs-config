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

# REWRITE 시 파일 복사 대상 (Quartz static/)
STATIC_IMAGES = HOME / "repos" / "gh" / "notes" / "static" / "images"

# 깨진 figure src의 basename을 찾을 디렉토리.
# 우선순위: 이미 가든에 복사된 파일 → 원본 source. STATIC_IMAGES가 먼저면
# "이전 export에서 이미 옮겨진 파일"이 잡혀서 file copy 없이 src 치환만 한다.
SEARCH_DIRS = [
    STATIC_IMAGES,                # 이미 가든 측에 복사된 파일 (가장 흔한 경우)
    HOME / "screenshot",          # org-download-image-dir
    HOME / "org" / ".attach",     # org-attach-id-dir
]

# Hugo figure shortcode: {{< figure src="..." ... >}}
FIGURE_RE = re.compile(r'\{\{<\s*figure\s+src="([^"]+)"[^>]*>\}\}')

# Markdown image: ![alt](url "title")
# - 코드블럭/인라인 코드 안에 있는 건 못 가린다 (대부분의 경우 문제되지 않음).
# - alt text는 [], url은 () 안. URL에는 공백 + " title" 같은 게 올 수 있어 첫 token만.
MD_IMG_RE = re.compile(r'!\[[^\]]*\]\(\s*([^\s)]+)')


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
    """(category, info) — info는 REWRITE면 Path, AMBIGUOUS면 list[Path].

    is_broken / UNKNOWN 둘 다 basename lookup을 시도한다. UNKNOWN 패턴(`assets/`,
    `img/`, 상대경로 등)도 SEARCH_DIRS 어딘가에 같은 basename 파일이 있으면
    REWRITE로 승격 — Hugo가 풀 수 있는 /images/ 경로로 정정 가능."""
    if is_alive(src):
        return ("ALIVE", None)
    basename = Path(src).name
    matches = idx.get(basename, [])
    broken = is_broken(src)
    if not matches:
        # 알려진 broken 패턴이면 DEAD, 그 외는 UNKNOWN으로 분리
        return ("DEAD" if broken else "UNKNOWN", None)
    if len(matches) == 1:
        return ("REWRITE", matches[0])
    # 여러 매치라도 sha256가 모두 동일하면 (마이그레이션 잔재) 첫 매치로 정정.
    # 다르면 AMBIGUOUS — 사용자 수동 확인 필요.
    if all(_same_content(matches[0], m) for m in matches[1:]):
        return ("REWRITE", matches[0])
    return ("AMBIGUOUS", matches)


# ━━━ Scanning ━━━

def scan(content_dir: Path, idx: dict):
    """list of (md_path, line_num, src, category, info, syntax).

    syntax: "figure" (Hugo shortcode) | "md-img" (markdown ![](...)).
    Tracking the syntax matters for REWRITE — fix는 두 형태를 별도로 치환한다.
    """
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
            items.append((md, line_num, src, category, info, "figure"))
        for m in MD_IMG_RE.finditer(text):
            src = m.group(1)
            category, info = categorize(src, idx)
            line_num = text.count("\n", 0, m.start()) + 1
            items.append((md, line_num, src, category, info, "md-img"))
    return items


# ━━━ Reporting ━━━

def rel_to_content(md: Path, content_dir: Path) -> str:
    try:
        return str(md.relative_to(content_dir))
    except ValueError:
        return str(md)


def report_summary(items, content_dir: Path):
    counts = defaultdict(int)
    by_syntax = defaultdict(lambda: defaultdict(int))
    for _, _, _, cat, _, syntax in items:
        counts[cat] += 1
        by_syntax[syntax][cat] += 1
    total = sum(counts.values())

    print("=== figure / image src verify ===")
    print(f"  total      : {total}")
    for cat in ["ALIVE", "REWRITE", "AMBIGUOUS", "DEAD", "UNKNOWN"]:
        n = counts.get(cat, 0)
        marker = "✓" if cat == "ALIVE" else ("⚠" if cat == "REWRITE" else "✗")
        if n > 0:
            print(f"  {marker} {cat:10s}: {n}")
    # syntax 분포 (디버깅 도움)
    print(f"  ┕ figure shortcode : {sum(by_syntax['figure'].values())}")
    print(f"  ┕ markdown ![]()   : {sum(by_syntax['md-img'].values())}")

    problems = [it for it in items if it[3] != "ALIVE"]
    if not problems:
        print()
        print("✅ 모든 image/figure src 정상")
        return 0

    print()
    print(f"--- 상세 ({len(problems)}건) ---")
    for md, line, src, cat, info, syntax in problems:
        rel = rel_to_content(md, content_dir)
        tag = "fig" if syntax == "figure" else "md "
        if cat == "REWRITE":
            print(f"  [{cat:10s}] [{tag}] {rel}:{line}")
            print(f"               src      = {src}")
            print(f"               source   = {info}")
        elif cat == "AMBIGUOUS":
            print(f"  [{cat:10s}] [{tag}] {rel}:{line}  src={src}  ({len(info)} matches)")
            for m in info:
                print(f"               - {m}")
        else:
            print(f"  [{cat:10s}] [{tag}] {rel}:{line}  src={src}")

    return 0 if (counts.get("DEAD", 0) + counts.get("AMBIGUOUS", 0) + counts.get("UNKNOWN", 0)) == 0 else 1


def report_fix(items, content_dir: Path, apply: bool) -> int:
    rewrites = [it for it in items if it[3] == "REWRITE"]
    if not rewrites:
        print("REWRITE 대상 없음")
        return 0

    print(f"=== REWRITE: {len(rewrites)}건 ===")
    if apply:
        STATIC_IMAGES.mkdir(parents=True, exist_ok=True)

    # md 파일별로 묶어서 한 번에 치환
    by_md = defaultdict(list)
    for md, line, src, cat, source_path, syntax in rewrites:
        by_md[md].append((line, src, source_path, syntax))

    conflicts = 0
    copied = 0
    skipped = 0
    rewritten_files = 0

    for md, entries in by_md.items():
        rel = rel_to_content(md, content_dir)
        text = md.read_text(encoding="utf-8") if apply else None
        any_replaced = False
        for line, src, source_path, syntax in entries:
            basename = source_path.name
            target = STATIC_IMAGES / basename
            new_src = f"/images/{basename}"
            tag = "fig" if syntax == "figure" else "md "

            action = "COPY"
            if target.exists():
                if _same_content(target, source_path):
                    action = "SKIP-IDENT"
                else:
                    action = "CONFLICT"

            print(f"  [{action:11s}] [{tag}] {rel}:{line}")
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
                # src 문자열이 같으면 figure shortcode와 markdown 양쪽 다 한 번에 정정됨.
                # 같은 src 가 여러 위치에 나와도 모두 치환된다.
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
