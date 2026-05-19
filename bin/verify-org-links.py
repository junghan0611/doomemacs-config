#!/usr/bin/env python3
"""verify-org-links.py — ~/org 안의 [[https://github.com/USER/...]] URL을 lychee로 검증.

fix-org-links.el이 변환한 GitHub URL이 실제 GitHub에 존재하는지 확인하는
read-only 도구. 자동 정정 안 함 — broken URL을 file:line으로 보고만 한다.
사용자가 ~/org에서 직접 수정 (private 의도 표기 / plain text / rename 등).

verify-content.py의 lychee 헬퍼를 그대로 재사용. site-policy.el SSOT 공유.

⚠ 신뢰 조건 — GITHUB_TOKEN:
    token 없으면 결과는 **참고용 (advisory)** 이다. GitHub secondary abuse
    rate-limit으로 broken 다수가 false positive (~/org 1420 URL 기준
    token 없음 84 broken vs token+튜닝 81 broken — 23 → 81 사이 차이가
    rate-limit으로 잘못 분류된 자리). 실제 ~/org 분류 작업은 token 있는
    conclusive run 기준으로 한다. run.sh `fix-org --check` 흐름은
    ~/.env.local 자동 source.

Usage:
    verify-org-links.py [ORG_DIR]
    ./run.sh fix-org --check
"""

import re
import sys
from collections import defaultdict
from importlib.util import module_from_spec, spec_from_file_location
from pathlib import Path

# Reuse verify-content.py helpers (load via importlib because filename has '-')
_VC_PATH = Path(__file__).parent / "verify-content.py"
_spec = spec_from_file_location("vc", str(_VC_PATH))
vc = module_from_spec(_spec)
_spec.loader.exec_module(vc)

POLICY_FILE = Path(__file__).parent / "site-policy.el"


def collect_org_github_urls(org_dir: Path, user: str) -> dict[str, list[tuple[str, int]]]:
    """Walk org_dir; return {url_no_fragment: [(rel_path, line), ...]}."""
    # Org link: [[URL]] or [[URL][desc]]
    pat = re.compile(
        rf"\[\[(https?://github\.com/{re.escape(user)}/[^\]\s]+)(?:\]\[[^\]\n]+)?\]\]"
    )
    refs: dict = defaultdict(list)
    for orgf in org_dir.rglob("*.org"):
        try:
            text = orgf.read_text(encoding="utf-8")
        except Exception:
            continue
        for m in pat.finditer(text):
            url = m.group(1).rstrip(".,;")
            base = url.split("#", 1)[0]
            line = text[: m.start()].count("\n") + 1
            try:
                rel = str(orgf.relative_to(org_dir))
            except ValueError:
                rel = str(orgf)
            refs[base].append((rel, line))
    return refs


def main():
    target = Path(sys.argv[1]) if len(sys.argv) > 1 else Path.home() / "org"
    target = target.resolve()
    if not target.is_dir():
        print(f"❌ {target} 디렉토리 없음", file=sys.stderr)
        sys.exit(1)

    print(f"📂 Scan: {target}", file=sys.stderr)
    policy = vc.load_policy(POLICY_FILE)
    user = policy.get("github-user", "")
    if not user:
        print("❌ github-user not in site-policy.el", file=sys.stderr)
        sys.exit(1)

    refs = collect_org_github_urls(target, user)
    total_refs = sum(len(v) for v in refs.values())
    print(f"   github.com/{user} URLs: {len(refs)} unique ({total_refs} refs)",
          file=sys.stderr)

    if not refs:
        print("✅ no github URLs")
        return

    failed = vc.check_urls_with_lychee(list(refs.keys()), policy)
    if failed is None:
        print("⚠ lychee 호출 실패", file=sys.stderr)
        sys.exit(1)

    if not failed:
        print(f"✅ ~/org github URL 깨끗 ({len(refs)} URLs, broken 0)")
        return

    # Group failures by file for compact output
    by_file: dict = defaultdict(list)
    for url in failed:
        for f, ln in refs.get(url, []):
            by_file[f].append((ln, url))

    print()
    print(f"💀 broken URLs: {len(failed)} unique → {total_refs}건 중 {sum(len(refs[u]) for u in failed)}개 참조")
    print()
    for f in sorted(by_file):
        items = sorted(by_file[f])
        print(f"📄 {f}  ({len(items)})")
        for ln, url in items:
            print(f"    L{ln}: {url}")
        print()

    print("→ ~/org에서 위 위치를 직접 수정 (private 의도 표기 / plain text / rename 등).")


if __name__ == "__main__":
    main()
