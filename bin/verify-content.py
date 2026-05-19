#!/usr/bin/env python3
"""verify-content.py — 가든 md 콘텐츠 위생 검증/정정 (Stage 3).

site-policy.el SSOT를 emacs batch로 읽어 정책 적용.
verify-relref.py / verify-figures.py 옆에 같은 패턴.

카테고리:
    HOST_ALIAS       site-policy host-aliases. 자동 정정 (host 부분만 치환).
    INTERNAL_PATH    ~/repos/, /home/junghan/, file:// URL/링크. plain text.
    PRIVATE_ENDPOINT localhost, 127.0.0.1, 사설망. 보고만.
    URL_CRED         basic auth URL. 보안 alert (보고만).
    ORPHAN           [desc] 끝에 target 없음. plain text 치환.

Usage:
    verify-content.py CONTENT_DIR              # 검증 리포트
    verify-content.py CONTENT_DIR --summary    # 카운트만
    verify-content.py CONTENT_DIR --fix        # dry-run
    verify-content.py CONTENT_DIR --fix --apply  # 실제 수정
    verify-content.py CONTENT_DIR --json       # JSON 리포트

SSOT: bin/site-policy.el — host-aliases 한 줄 추가하면 즉시 반영.
"""

import argparse
import json
import os
import re
import shutil
import subprocess
import sys
from collections import defaultdict
from pathlib import Path

# ━━━ SSOT loader ━━━

POLICY_DUMP_EL = "(progn (require (quote json)) (princ (json-encode my/site-policy)))"


def load_policy(policy_file: Path) -> dict:
    """Load site-policy.el via emacs batch json dump."""
    out = subprocess.check_output(
        [
            "emacs", "--batch", "-Q",
            "--load", str(policy_file),
            "--eval", POLICY_DUMP_EL,
        ],
        stderr=subprocess.DEVNULL,
    ).decode()
    return json.loads(out)


def elisp_re_to_python(pat: str) -> str:
    """Convert elisp regex artifacts to Python.
    Currently only `\\<` and `\\>` (elisp word boundary) → `\\b`."""
    return pat.replace(r"\<", r"\b").replace(r"\>", r"\b")


# ━━━ Detection patterns ━━━

# Standard markdown link [desc](target). Preceded by `!` = image, handled separately.
RE_MD_LINK = re.compile(r"(?<!!)\[([^\]\n]+)\]\(([^)\n]+)\)")

# Image link ![alt](src) — categorized but fix is delicate (drop image entirely)
RE_MD_IMAGE = re.compile(r"!\[([^\]\n]*)\]\(([^)\n]+)\)")

# Orphan: [desc] NOT followed by `(` `{` `[` `!` `:` — dangling bracket
RE_ORPHAN = re.compile(r"(?<!!)\[([^\]\n]+)\](?![(\{\[!:])")


def is_relref_target(target: str) -> bool:
    """Skip targets handled by verify-relref.py."""
    return "{{< relref " in target


def categorize(target: str, policy: dict) -> tuple[str, str] | tuple[None, str]:
    """Classify a URL/target. Returns (category, detail) or (None, '')."""
    # URL_CRED first — security wins
    cred_pat = policy.get("credential-in-url")
    if cred_pat and re.search(cred_pat, target):
        return "URL_CRED", "basic auth in URL"

    # HOST_ALIAS — fixable
    for old_host, new_host in (policy.get("host-aliases") or {}).items():
        if old_host in target:
            return "HOST_ALIAS", new_host

    # INTERNAL_PATH — fixable (plain text)
    for pat in policy.get("internal-paths") or []:
        if re.search(pat, target):
            return "INTERNAL_PATH", pat

    # PRIVATE_ENDPOINT — report only
    for pat in policy.get("private-endpoints") or []:
        if re.search(elisp_re_to_python(pat), target):
            return "PRIVATE_ENDPOINT", pat

    return None, ""


# ━━━ Scanner ━━━

def scan_file(filepath: Path, content_dir: Path, policy: dict) -> list[dict]:
    results = []
    try:
        text = filepath.read_text(encoding="utf-8")
    except Exception as e:
        return [{"category": "ERROR", "file": str(filepath), "detail": str(e)}]

    relpath = str(filepath.relative_to(content_dir))

    def append(m, cat, detail, target, desc):
        results.append({
            "file": relpath,
            "category": cat,
            "detail": detail,
            "target": target,
            "desc": desc,
            "line": text[:m.start()].count("\n") + 1,
            "match_start": m.start(),
            "match_end": m.end(),
            "match_text": m.group(0),
            "pattern": "link",
        })

    # 1. Standard md links
    for m in RE_MD_LINK.finditer(text):
        desc, target = m.group(1), m.group(2)
        if is_relref_target(target):
            continue
        cat, detail = categorize(target, policy)
        if cat:
            append(m, cat, detail, target, desc[:80])

    # 2. Image links — same categorization but fix policy is different (skip auto-fix)
    for m in RE_MD_IMAGE.finditer(text):
        alt, src = m.group(1), m.group(2)
        if is_relref_target(src):
            continue
        cat, detail = categorize(src, policy)
        if cat:
            entry = {
                "file": relpath,
                "category": cat,
                "detail": detail + " (image — fix=manual)",
                "target": src,
                "desc": alt[:80],
                "line": text[:m.start()].count("\n") + 1,
                "match_start": m.start(),
                "match_end": m.end(),
                "match_text": m.group(0),
                "pattern": "image",
            }
            results.append(entry)

    # 3. Orphan brackets
    if policy.get("orphan-bracket"):
        # Build a set of brackets that are part of a real link (start position)
        link_starts = set()
        for m in RE_MD_LINK.finditer(text):
            link_starts.add(m.start())
        for m in RE_MD_IMAGE.finditer(text):
            link_starts.add(m.start())

        for m in RE_ORPHAN.finditer(text):
            # Skip if this bracket is actually part of a [text](url) link
            if m.start() in link_starts:
                continue
            inside = m.group(1)
            # Skip footnote refs [^1]
            if inside.startswith("^"):
                continue
            # Skip callout markers [!abstract]
            if inside.startswith("!"):
                continue
            # Skip reference-style link defs [ref]: url (next char is `:`) — already excluded by regex
            # Skip task list `[ ]`, `[x]`
            if inside.strip() in ("", "x", "X", " "):
                continue
            results.append({
                "file": relpath,
                "category": "ORPHAN",
                "detail": "dangling bracket — no target",
                "target": "",
                "desc": inside[:80],
                "line": text[:m.start()].count("\n") + 1,
                "match_start": m.start(),
                "match_end": m.end(),
                "match_text": m.group(0),
                "pattern": "orphan",
            })

    return results


# ━━━ Fixer ━━━

# Categories that can be auto-fixed
FIXABLE = {"HOST_ALIAS", "INTERNAL_PATH", "ORPHAN", "GITHUB_404"}


# ━━━ lychee integration ━━━

def find_lychee() -> list[str] | None:
    """Return the command prefix to invoke lychee, or None if unavailable.
    Prefers lychee on PATH; falls back to `nix shell nixpkgs#lychee --command lychee`."""
    if shutil.which("lychee"):
        return ["lychee"]
    if shutil.which("nix"):
        return ["nix", "shell", "nixpkgs#lychee", "--command", "lychee"]
    return None


def collect_github_urls(content_dir: Path, github_user: str) -> dict[str, list[str]]:
    """Walk content_dir, collect unique github.com/USER URLs → list of file refs."""
    pat = re.compile(rf"https?://github\.com/{re.escape(github_user)}/[^\s)\]\"']+")
    out: dict = defaultdict(list)
    for md in content_dir.rglob("*.md"):
        try:
            text = md.read_text(encoding="utf-8")
        except Exception:
            continue
        for m in pat.finditer(text):
            url = m.group(0).rstrip(".,;)")
            # Drop fragment for lychee — GitHub returns same status w/wo
            base = url.split("#", 1)[0]
            out[base].append(str(md.relative_to(content_dir)))
    return out


def check_urls_with_lychee(urls: list[str], policy: dict) -> set[str] | None:
    """Pipe URLs to lychee via stdin, return 404/410 set or None on failure.
    Honors GITHUB_TOKEN (or GITHUB_PERSONAL_ACCESS_TOKEN) for rate-limit relief."""
    bin_cmd = find_lychee()
    if bin_cmd is None:
        print("⚠ lychee not found (need: nix develop OR install lychee)", file=sys.stderr)
        return None
    if not urls:
        return set()
    lyc = policy.get("lychee") or {}
    cmd = bin_cmd + ["--no-progress", "--format", "json"]
    cmd += ["--max-redirects", str(lyc.get("max-redirects", 5))]
    if lyc.get("max-concurrency"):
        cmd += ["--max-concurrency", str(lyc["max-concurrency"])]
    if lyc.get("max-retries"):
        cmd += ["--max-retries", str(lyc["max-retries"])]
    if lyc.get("retry-wait-time"):
        cmd += ["--retry-wait-time", str(lyc["retry-wait-time"])]
    if lyc.get("cache"):
        cmd += ["--cache"]
    cmd += ["-"]
    # Pass-through GitHub token to lychee env (lychee auto-reads GITHUB_TOKEN).
    # Accept both GITHUB_TOKEN and GITHUB_PERSONAL_ACCESS_TOKEN names.
    env = os.environ.copy()
    token = env.get("GITHUB_TOKEN") or env.get("GITHUB_PERSONAL_ACCESS_TOKEN")
    token_note = ""
    if token:
        env["GITHUB_TOKEN"] = token
        token_note = " (GitHub token: ON)"
    print(f"   lychee 검증: {len(urls)} URLs...{token_note}", file=sys.stderr)
    try:
        result = subprocess.run(
            cmd, input="\n".join(urls),
            capture_output=True, text=True, timeout=600,
            env=env,
        )
    except subprocess.TimeoutExpired:
        print("⚠ lychee timeout (600s)", file=sys.stderr)
        return None
    try:
        data = json.loads(result.stdout)
    except json.JSONDecodeError:
        print(f"⚠ lychee output parse failed: {result.stderr[:200]}", file=sys.stderr)
        return None
    failed: set = set()
    for _src, items in (data.get("error_map") or {}).items():
        for item in items:
            url = item.get("url") or ""
            status = item.get("status") or {}
            code = status.get("code") if isinstance(status, dict) else None
            if code in (404, 410):
                failed.add(url)
    print(f"   lychee 결과: {data.get('successful', 0)} OK, "
          f"{len(failed)} 404/410, {data.get('errors', 0)} errors total",
          file=sys.stderr)
    return failed


def scan_file_for_404(filepath: Path, content_dir: Path, failed_urls: set) -> list[dict]:
    """Find positions of known-404 URLs in filepath."""
    results = []
    try:
        text = filepath.read_text(encoding="utf-8")
    except Exception:
        return results
    relpath = str(filepath.relative_to(content_dir))
    for m in RE_MD_LINK.finditer(text):
        desc, target = m.group(1), m.group(2)
        target_no_frag = target.split("#", 1)[0]
        if target_no_frag in failed_urls:
            results.append({
                "file": relpath,
                "category": "GITHUB_404",
                "detail": "github URL returns 404/410",
                "target": target,
                "desc": desc[:80],
                "line": text[:m.start()].count("\n") + 1,
                "match_start": m.start(),
                "match_end": m.end(),
                "match_text": m.group(0),
                "pattern": "link",
            })
    return results


def fix_file(filepath: Path, items: list[dict], policy: dict, apply: bool) -> dict:
    """Apply fixes. Returns count by category."""
    text = filepath.read_text(encoding="utf-8")
    original = text
    counts: dict = defaultdict(int)

    # Only fixable items, sorted by match_start descending so offsets stay valid
    fixable_items = [i for i in items
                     if i["category"] in FIXABLE and i.get("pattern") != "image"]
    fixable_items.sort(key=lambda x: -x["match_start"])

    aliases = policy.get("host-aliases") or {}

    for item in fixable_items:
        start, end = item["match_start"], item["match_end"]
        cat = item["category"]
        if cat == "HOST_ALIAS":
            target = item["target"]
            new_target = target
            for old_host, new_host in aliases.items():
                if old_host in new_target:
                    new_target = new_target.replace(old_host, new_host)
                    break
            new_chunk = f"[{item['desc']}]({new_target})"
        elif cat == "INTERNAL_PATH":
            # Drop the link, keep the description as plain text
            new_chunk = item["desc"]
        elif cat == "ORPHAN":
            # Strip brackets, keep inside as plain text
            new_chunk = item["desc"]
        elif cat == "GITHUB_404":
            # 404/410 GitHub URL — keep description, drop the link
            new_chunk = item["desc"]
        else:
            continue
        text = text[:start] + new_chunk + text[end:]
        counts[cat] += 1

    if text != original and apply:
        filepath.write_text(text, encoding="utf-8")

    return dict(counts)


# ━━━ Main ━━━

# ━━━ ~/org origin mapping ━━━
# Map garden detection → source .org file via denote ID, so the user can
# jump straight to ~/org for manual edit (e.g. private-repo URLs, .gitignored
# files, rename cases — anything the automatic fixer can't decide).

ORG_ROOT = Path(os.environ.get("ORG_ROOT", os.path.expanduser("~/org"))).resolve()

_ORG_SOURCE_CACHE: dict[str, str | None] = {}


def find_org_source(garden_relpath: str) -> str | None:
    """Map garden md path → ~/org source file path via denote ID."""
    if garden_relpath in _ORG_SOURCE_CACHE:
        return _ORG_SOURCE_CACHE[garden_relpath]
    result: str | None = None
    parts = Path(garden_relpath).parts
    if parts:
        filename = parts[-1]
        m = re.match(r"^(\d{8}T\d{6})\.md$", filename)
        if m and ORG_ROOT.is_dir():
            denote_id = m.group(1)
            # Try same subdir first (most common: botlog→botlog, notes→notes, ...)
            candidates = []
            if len(parts) > 1:
                same = ORG_ROOT.joinpath(*parts[:-1])
                if same.is_dir():
                    candidates.extend(same.glob(f"{denote_id}--*.org"))
            # Fallback: scan top-level ~/org subdirs for the denote-id
            if not candidates:
                for f in ORG_ROOT.rglob(f"{denote_id}--*.org"):
                    candidates.append(f)
                    break
            if candidates:
                # Display as "<subdir>/<filename>" relative to ORG_ROOT
                try:
                    result = str(candidates[0].relative_to(ORG_ROOT))
                except ValueError:
                    result = str(candidates[0])
    _ORG_SOURCE_CACHE[garden_relpath] = result
    return result


ICONS = {
    "HOST_ALIAS": "🔵",
    "INTERNAL_PATH": "🟡",
    "PRIVATE_ENDPOINT": "🟠",
    "URL_CRED": "🔐",
    "ORPHAN": "🔴",
    "GITHUB_404": "💀",
    "ERROR": "❌",
}


def main():
    parser = argparse.ArgumentParser(description="Garden md content verifier (Stage 3)")
    parser.add_argument("content_dir", help="Hugo content 디렉토리 경로")
    parser.add_argument("--policy", default=None,
                        help="site-policy.el 경로 (기본: 스크립트 옆)")
    parser.add_argument("--summary", action="store_true", help="카운트만")
    parser.add_argument("--json", action="store_true", help="JSON 리포트")
    parser.add_argument("--fix", action="store_true", help="수정 모드 (FIXABLE만)")
    parser.add_argument("--apply", action="store_true", help="실제 파일 수정 (--fix 필요)")
    parser.add_argument("--category", help="특정 카테고리만 표시 (콤마 구분)")
    parser.add_argument("--lychee", action="store_true",
                        help="lychee로 GitHub URL 200 OK 검증 (네트워크 호출, 수십 초~분 소요)")
    args = parser.parse_args()

    if args.apply and not args.fix:
        print("❌ --apply는 --fix와 함께", file=sys.stderr)
        sys.exit(1)

    content_dir = Path(args.content_dir).resolve()
    if not content_dir.is_dir():
        print(f"❌ content 디렉토리 없음: {content_dir}", file=sys.stderr)
        sys.exit(1)

    policy_file = Path(args.policy) if args.policy else Path(__file__).parent / "site-policy.el"
    if not policy_file.exists():
        print(f"❌ site-policy 파일 없음: {policy_file}", file=sys.stderr)
        sys.exit(1)

    print(f"📂 Scanning: {content_dir}", file=sys.stderr)
    print(f"📜 Policy:   {policy_file}", file=sys.stderr)
    policy = load_policy(policy_file)

    md_files = sorted(content_dir.rglob("*.md"))
    print(f"   파일 인덱스: {len(md_files)} files", file=sys.stderr)

    all_results: list[dict] = []
    for f in md_files:
        all_results.extend(scan_file(f, content_dir, policy))

    # Optional second pass: lychee check on github.com/USER URLs
    if args.lychee:
        url_refs = collect_github_urls(content_dir, policy.get("github-user", ""))
        failed_set = check_urls_with_lychee(list(url_refs.keys()), policy)
        if failed_set:
            for f in md_files:
                all_results.extend(scan_file_for_404(f, content_dir, failed_set))

    stats: dict = defaultdict(int)
    for r in all_results:
        stats[r["category"]] += 1

    # ━━━ JSON ━━━
    if args.json:
        print(json.dumps({"stats": dict(stats),
                          "total": len(all_results),
                          "items": all_results},
                         ensure_ascii=False, indent=2))
        return

    # ━━━ Fix mode ━━━
    if args.fix:
        by_file: dict = defaultdict(list)
        for r in all_results:
            if r["category"] in FIXABLE:
                by_file[r["file"]].append(r)

        total = defaultdict(int)
        files_touched = 0
        for relpath, items in sorted(by_file.items()):
            filepath = content_dir / relpath
            counts = fix_file(filepath, items, policy, args.apply)
            if counts:
                files_touched += 1
                marker = "✓" if args.apply else "📄"
                pretty = " ".join(f"{ICONS.get(c, '?')}{counts[c]}" for c in counts)
                print(f"  {marker} {relpath}  ({pretty})")
                if not args.apply:
                    org_src = find_org_source(relpath)
                    if org_src:
                        print(f"    → ~/org: {org_src}")
                    for i in items:
                        if i["category"] in FIXABLE:
                            print(f"    {ICONS.get(i['category'], '?')} "
                                  f"L{i['line']}: {i['match_text'][:100]}")
                for c, n in counts.items():
                    total[c] += n

        # Report unfixable categories (security alerts etc.)
        non_fixable = [r for r in all_results
                       if r["category"] not in FIXABLE]
        if non_fixable:
            print()
            print("─── 자동 정정 안 됨 (보고만) ───")
            by_cat: dict = defaultdict(list)
            for r in non_fixable:
                by_cat[r["category"]].append(r)
            for cat in sorted(by_cat):
                items = by_cat[cat]
                icon = ICONS.get(cat, "?")
                print(f"  {icon} {cat}  ({len(items)})")
                for i in items[:5]:
                    print(f"    {i['file']}:{i['line']}  {i['target'][:80]}")
                if len(items) > 5:
                    print(f"    ... +{len(items) - 5} more")

        print()
        if args.apply:
            print(f"✅ 수정 완료: {files_touched} 파일")
        else:
            print(f"🔍 Dry-run: {files_touched} 파일")
        for c, n in sorted(total.items()):
            print(f"   {ICONS.get(c, '?')} {c:18s} {n}")
        if not args.apply and any(total.values()):
            print()
            print("   실제 적용: verify-content.py --fix --apply")
        return

    # ━━━ Report ━━━
    print()
    print("═══ Content 검증 리포트 (Stage 3) ═══")
    print()
    print(f"총 검출: {len(all_results)}")
    for cat in ["HOST_ALIAS", "INTERNAL_PATH", "PRIVATE_ENDPOINT", "URL_CRED", "ORPHAN", "GITHUB_404", "ERROR"]:
        n = stats.get(cat, 0)
        if n:
            icon = ICONS.get(cat, "?")
            tag = " (자동정정)" if cat in FIXABLE else " (보고만)"
            print(f"  {icon} {cat:18s} {n:5d}{tag}")

    if args.summary:
        return

    filter_cats = set()
    if args.category:
        filter_cats = {c.strip().upper() for c in args.category.split(",")}

    # Detail per category
    print()
    for cat in ["HOST_ALIAS", "INTERNAL_PATH", "PRIVATE_ENDPOINT", "URL_CRED", "ORPHAN", "GITHUB_404"]:
        items = [r for r in all_results if r["category"] == cat]
        if not items:
            continue
        if filter_cats and cat not in filter_cats:
            continue
        icon = ICONS.get(cat, "?")
        print(f"─── {icon} {cat} ({len(items)}) ───")
        for item in items[:30]:
            print(f"  {item['file']}:{item['line']}")
            print(f"    {item['match_text'][:120]}")
            org_src = find_org_source(item['file'])
            if org_src:
                print(f"    → ~/org: {org_src}")
        if len(items) > 30:
            print(f"  ... +{len(items) - 30} more (use --category={cat} for full)")
        print()


if __name__ == "__main__":
    main()
