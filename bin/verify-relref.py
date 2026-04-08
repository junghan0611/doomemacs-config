#!/usr/bin/env python3
"""verify-relref.py — Hugo relref 링크 검증기 + 선택적 수정기

검증기로서 export 후 notes 리포의 content/ 상태를 점검한다.
doomemacs-config/run.sh에서 export 완료 후 호출.

Usage:
    verify-relref.py CONTENT_DIR              # 검증 리포트
    verify-relref.py CONTENT_DIR --json       # JSON 리포트
    verify-relref.py CONTENT_DIR --fix        # dry-run 수정
    verify-relref.py CONTENT_DIR --fix --apply  # 실제 수정

분류:
    ALIVE       실제 파일 존재
    VIRTUAL     Quartz가 생성하는 route (/tags, /today 등)
    REWRITE     basename으로 유일 매핑 가능 → 경로 정정
    DEAD        실제 대상 없음 → plain text 치환
    AMBIGUOUS   basename 중복 → 수동 확인 필요
    MALFORMED   파싱 불가 패턴 (heading anchor 누출 등)
"""

import argparse
import json
import os
import re
import sys
from collections import defaultdict
from pathlib import Path

# ━━━ Configuration ━━━

# Quartz virtual routes — 파일 없이 생성되는 페이지
VIRTUAL_ROUTES = {
    "/tags", "/botlog", "/meta", "/bib", "/notes", "/journal",
    "/today", "/categories",
}
VIRTUAL_PREFIXES = ("/tags/",)  # /tags/* 모두 virtual

# ━━━ Relref 추출 ━━━

# 표준 패턴: [desc]({{< relref "target" >}})
RE_RELREF = re.compile(
    r'\[([^\]]*(?:\[[^\]]*\][^\]]*)*)\]'   # [desc] — nested bracket 허용
    r'\(\{\{<\s*relref\s+"([^"]+)"\s*>\}\}\)'
)

# Bare relref: [{{< relref "target" >}}] (desc 없음)
RE_BARE_RELREF = re.compile(
    r'\[\{\{<\s*relref\s+"([^"]+)"\s*>\}\}\]'
)

# Heading anchor에 relref가 누출된 패턴 (ox-hugo artifact)
RE_HEADING_RELREF = re.compile(
    r'\{#[^}]*relref[^}]*\}'
)

# Anchor 내 relref noise 제거용 — section 있는 패턴 + section 없는 패턴
RE_ANCHOR_RELREF = re.compile(
    r'(\{#[^}]*?)--relref-(?:[a-z]*-)?([0-9t]+-dot-md)([^}]*\})'
)


def classify_target(target: str, content_dir: Path, file_index: dict) -> tuple[str, str]:
    """relref target을 분류하고 (category, detail)을 반환."""
    # Split fragment (#h-UUID) from path before classification
    if "#" in target:
        target_path, _fragment = target.split("#", 1)
    else:
        target_path = target
    clean = target_path.lstrip("/")

    # 1. ALIVE — 파일 존재
    if (content_dir / clean).is_file():
        return ("ALIVE", "")

    # 2. VIRTUAL — Quartz route
    normalized = "/" + clean.rstrip("/")
    if normalized in VIRTUAL_ROUTES:
        return ("VIRTUAL", "quartz route")
    for prefix in VIRTUAL_PREFIXES:
        if normalized.startswith(prefix):
            return ("VIRTUAL", "quartz tag route")

    # 3. basename 검색 — REWRITE 또는 AMBIGUOUS
    bn = os.path.basename(target_path)
    if bn in file_index:
        matches = file_index[bn]
        if len(matches) == 1:
            correct = "/" + matches[0]
            return ("REWRITE", correct)
        else:
            return ("AMBIGUOUS", f"{len(matches)} matches: {matches[:3]}")

    # 4. denote 풀네임 (YYYYMMDDTHHMMSS--title__tags, .md 없음)
    m = re.match(r'(\d{8}T\d{6})', bn)
    if m:
        denote_id = m.group(1)
        md_name = denote_id + ".md"
        if md_name in file_index:
            matches = file_index[md_name]
            if len(matches) == 1:
                return ("REWRITE", "/" + matches[0])
            else:
                return ("AMBIGUOUS", f"denote id {denote_id}: {len(matches)} matches")
        return ("DEAD", "denote id not exported")

    # 5. .md 확장자 없는 비-denote 패턴
    if not bn.endswith(".md"):
        return ("VIRTUAL", "non-file route (assumed)")

    # 6. 진짜 dead
    return ("DEAD", "file not found")


def build_file_index(content_dir: Path) -> dict[str, list[str]]:
    """content/ 아래 모든 .md 파일의 basename → relative path 매핑."""
    index = defaultdict(list)
    for md in content_dir.rglob("*.md"):
        rel = str(md.relative_to(content_dir))
        index[md.name].append(rel)
    return dict(index)


def scan_file(filepath: Path, content_dir: Path, file_index: dict) -> list[dict]:
    """파일에서 relref를 추출하고 분류."""
    results = []
    try:
        text = filepath.read_text(encoding="utf-8")
    except Exception as e:
        return [{"type": "ERROR", "file": str(filepath), "msg": str(e)}]

    relpath = str(filepath.relative_to(content_dir))

    # 표준 relref
    for m in RE_RELREF.finditer(text):
        desc, target = m.group(1), m.group(2)
        cat, detail = classify_target(target, content_dir, file_index)
        results.append({
            "file": relpath,
            "target": target,
            "desc": desc[:80],
            "category": cat,
            "detail": detail,
            "line": text[:m.start()].count("\n") + 1,
            "pattern": "standard",
        })

    # Bare relref
    for m in RE_BARE_RELREF.finditer(text):
        target = m.group(1)
        cat, detail = classify_target(target, content_dir, file_index)
        results.append({
            "file": relpath,
            "target": target,
            "desc": "",
            "category": cat,
            "detail": detail,
            "line": text[:m.start()].count("\n") + 1,
            "pattern": "bare",
        })

    # Heading anchor 누출
    for m in RE_HEADING_RELREF.finditer(text):
        results.append({
            "file": relpath,
            "target": m.group(0)[:60],
            "desc": "",
            "category": "MALFORMED",
            "detail": "relref leaked into heading anchor",
            "line": text[:m.start()].count("\n") + 1,
            "pattern": "heading",
        })

    return results


# ━━━ Fix logic ━━━

def fix_heading_anchors(filepath: Path, apply: bool) -> int:
    """heading anchor에서 --relref-section-id-dot-md noise 제거. Returns count."""
    text = filepath.read_text(encoding="utf-8")
    new_text, count = RE_ANCHOR_RELREF.subn(r'\1\3', text)
    if count > 0 and apply:
        filepath.write_text(new_text, encoding="utf-8")
    return count


def fix_file(filepath: Path, items: list[dict], apply: bool) -> tuple[int, int]:
    """파일의 DEAD/REWRITE 항목 수정. Returns (dead_count, rewrite_count)."""
    text = filepath.read_text(encoding="utf-8")
    original = text

    dead_targets = {i["target"] for i in items if i["category"] == "DEAD"}
    rewrite_map = {i["target"]: i["detail"] for i in items if i["category"] == "REWRITE"}

    dead_count = 0
    rewrite_count = 0

    def replacer(m):
        nonlocal dead_count, rewrite_count
        desc, target = m.group(1), m.group(2)
        if target in dead_targets:
            dead_count += 1
            return desc  # plain text, 대괄호 제거
        elif target in rewrite_map:
            rewrite_count += 1
            new_target = rewrite_map[target]
            return f'[{desc}]({{{{< relref "{new_target}" >}}}})'
        return m.group(0)

    text = RE_RELREF.sub(replacer, text)

    if text != original and apply:
        filepath.write_text(text, encoding="utf-8")

    return dead_count, rewrite_count


# ━━━ Main ━━━

def main():
    parser = argparse.ArgumentParser(description="Hugo relref 검증기")
    parser.add_argument("content_dir", help="Hugo content 디렉토리 경로")
    parser.add_argument("--json", action="store_true", help="JSON 리포트 출력")
    parser.add_argument("--fix", action="store_true", help="수정 모드 (DEAD/REWRITE만)")
    parser.add_argument("--fix-anchors", action="store_true", help="heading anchor에서 relref noise 제거")
    parser.add_argument("--apply", action="store_true", help="실제 파일 수정 (--fix/--fix-anchors 필요)")
    parser.add_argument("--category", help="특정 카테고리만 표시 (DEAD,REWRITE,...)")
    parser.add_argument("--summary", action="store_true", help="요약만 출력 (상세 생략)")
    args = parser.parse_args()

    if args.apply and not (args.fix or args.fix_anchors):
        print("❌ --apply는 --fix와 함께 사용하세요", file=sys.stderr)
        sys.exit(1)

    content_dir = Path(args.content_dir).resolve()
    if not content_dir.is_dir():
        print(f"❌ content 디렉토리 없음: {content_dir}", file=sys.stderr)
        sys.exit(1)

    print(f"📂 Scanning: {content_dir}", file=sys.stderr)

    # 파일 인덱스 구축
    file_index = build_file_index(content_dir)
    print(f"   파일 인덱스: {sum(len(v) for v in file_index.values())} files", file=sys.stderr)

    # 전체 스캔
    all_results = []
    md_files = sorted(content_dir.rglob("*.md"))
    for f in md_files:
        all_results.extend(scan_file(f, content_dir, file_index))

    # 통계
    stats = defaultdict(int)
    for r in all_results:
        stats[r["category"]] += 1

    # ━━━ JSON 출력 ━━━
    if args.json:
        output = {
            "stats": dict(stats),
            "total": len(all_results),
            "items": [r for r in all_results if r["category"] != "ALIVE"],
        }
        print(json.dumps(output, ensure_ascii=False, indent=2))
        return

    # ━━━ Fix Anchors 모드 ━━━
    if args.fix_anchors:
        total_anchors = 0
        files_modified = 0
        for f in md_files:
            count = fix_heading_anchors(f, args.apply)
            if count > 0:
                files_modified += 1
                total_anchors += count
                marker = "✓" if args.apply else "📄"
                relpath = str(f.relative_to(content_dir))
                print(f"  {marker} {relpath} (🔧{count})")
        print()
        if args.apply:
            print(f"✅ anchor 정리 완료: {files_modified} 파일, {total_anchors} anchors")
        else:
            print(f"🔍 Dry-run: {files_modified} 파일, {total_anchors} anchors")
            print(f"   실행: verify-relref.py {args.content_dir} --fix-anchors --apply")
        return

    # ━━━ Fix 모드 ━━━
    if args.fix:
        by_file = defaultdict(list)
        for r in all_results:
            if r["category"] in ("DEAD", "REWRITE"):
                by_file[r["file"]].append(r)

        total_dead = 0
        total_rewrite = 0
        files_modified = 0

        for relpath, items in sorted(by_file.items()):
            filepath = content_dir / relpath
            dc, rc = fix_file(filepath, items, args.apply)
            if dc + rc > 0:
                files_modified += 1
                marker = "✓" if args.apply else "📄"
                print(f"  {marker} {relpath} (🔴{dc} 🟡{rc})")
                if not args.apply:
                    for i in items:
                        if i["category"] == "DEAD":
                            print(f"    🔴 DEL: {i['desc'][:60]} ← \"{i['target']}\"")
                        elif i["category"] == "REWRITE":
                            print(f"    🟡 FIX: \"{i['target']}\" → \"{i['detail']}\"")
                total_dead += dc
                total_rewrite += rc

        print()
        if args.apply:
            print(f"✅ 수정 완료: {files_modified} 파일")
        else:
            print(f"🔍 Dry-run: {files_modified} 파일")
        print(f"   🔴 dead→text:  {total_dead}")
        print(f"   🟡 rewrite:    {total_rewrite}")
        if not args.apply:
            print(f"   실행: verify-relref.py {args.content_dir} --fix --apply")
        return

    # ━━━ 검증 리포트 ━━━
    print()
    print("═══ Relref 검증 리포트 ═══")
    print()
    print(f"총 relref: {len(all_results)}")
    for cat in ["ALIVE", "VIRTUAL", "REWRITE", "DEAD", "AMBIGUOUS", "MALFORMED"]:
        icon = {"ALIVE": "✅", "VIRTUAL": "🔵", "REWRITE": "🟡",
                "DEAD": "🔴", "AMBIGUOUS": "🟠", "MALFORMED": "❌"}.get(cat, "?")
        count = stats.get(cat, 0)
        if count > 0:
            print(f"  {icon} {cat:12s} {count:5d}")

    if args.summary:
        return

    # 카테고리 필터
    filter_cats = set()
    if args.category:
        filter_cats = {c.strip().upper() for c in args.category.split(",")}

    # ALIVE 제외한 상세 출력
    print()
    for cat in ["VIRTUAL", "REWRITE", "DEAD", "AMBIGUOUS", "MALFORMED"]:
        items = [r for r in all_results if r["category"] == cat]
        if not items:
            continue
        if filter_cats and cat not in filter_cats:
            continue

        icon = {"VIRTUAL": "🔵", "REWRITE": "🟡", "DEAD": "🔴",
                "AMBIGUOUS": "🟠", "MALFORMED": "❌"}[cat]
        print(f"─── {icon} {cat} ({len(items)}) ───")

        by_target = defaultdict(list)
        for r in items:
            by_target[r["target"]].append(r)

        for target, refs in sorted(by_target.items(), key=lambda x: -len(x[1])):
            detail = refs[0]["detail"]
            ref_count = len(refs)
            files = sorted(set(r["file"] for r in refs))
            if cat == "REWRITE":
                print(f"  {target}")
                print(f"    → {detail}  ({ref_count} refs in {len(files)} files)")
            elif cat in ("DEAD", "AMBIGUOUS", "MALFORMED"):
                print(f"  {target}  ({ref_count} refs)")
                if detail:
                    print(f"    {detail}")
                if ref_count <= 3:
                    for f in files:
                        print(f"    in: {f}")
            else:
                if ref_count > 1:
                    print(f"  {target}  ({ref_count} refs)")
                else:
                    print(f"  {target}")
        print()


if __name__ == "__main__":
    main()
