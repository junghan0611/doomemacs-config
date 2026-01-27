#!/usr/bin/env bash
# gh-starred-to-bib.sh — GitHub starred repos를 BibTeX 형식으로 변환
#
# Usage: ./gh-starred-to-bib.sh [output.bib]
# Default output: ~/org/resources/github-starred.bib
#
# Citar 템플릿 호환 필드:
#   ${dateadded:10}       <- dateadded   (starred_at: star한 시점)
#   ${datemodified:10}    <- datemodified (pushed_at: 마지막 코드 푸시)
#   ${date year issued:4} <- date        (updated_at: 리포 업데이트)
#   ${keywords:*}         <- keywords    (topics)
#   ${url:19}             <- url         (html_url)
#   ${abstract}           <- abstract    (description)
#   ${author editor:19}   <- author      (owner.login)
#   ${title:49}           <- title       (full_name: owner/repo)
#
# GitHub API 참고:
#   Accept: application/vnd.github.star+json 헤더로 starred_at 필드 포함
#   응답 구조: {repo: {...}, starred_at: "..."}

set -euo pipefail

OUTPUT="${1:-$HOME/org/resources/github-starred.bib}"
SCRIPT_PATH="$(cd "$(dirname "$0")" && pwd)/$(basename "$0")"
TEMP_FILE=$(mktemp)

echo "Fetching starred repos from GitHub..." >&2

# GitHub 계정 정보 가져오기
GH_USER=$(gh api user --jq '.login')

# 임시 파일에 entries 먼저 저장 (개수 계산용)
# Accept 헤더로 starred_at 필드 포함
gh api --paginate user/starred -H "Accept: application/vnd.github.star+json" | jq -r '
.[] |
# starred_at과 repo 분리
.starred_at as $starred |
.repo |
# bib key 생성: owner-repo (특수문자 제거)
(.full_name | gsub("[^a-zA-Z0-9]"; "")) as $key |
# topics를 comma로 연결
(.topics | if length > 0 then join(", ") else "" end) as $keywords |
# description 이스케이프 (bib 특수문자)
(.description // "" | gsub("[{}]"; "") | gsub("\""; "\\\"") | gsub("\n"; " ")) as $desc |
# license 이름
(.license.name // "") as $license |
# urldate (starred_at의 날짜 부분만)
($starred | split("T")[0]) as $urldate |

"@software{\($key),
  title = {\(.full_name)},
  author = {\(.owner.login)},
  date = {\(.updated_at)},
  origdate = {\(.created_at)},
  url = {\(.html_url)},
  urldate = {\($urldate)},
  abstract = {\($desc)},
  keywords = {\($keywords)},
  note = {stars: \(.stargazers_count), language: \(.language // "unknown"), license: \($license)},
  datemodified = {\(.pushed_at)},
  dateadded = {\($starred)}
}
"
' > "$TEMP_FILE"

COUNT=$(grep -c '^@software{' "$TEMP_FILE")
TIMESTAMP=$(date -Iseconds)

# BibTeX 파일 헤더 작성
cat > "$OUTPUT" << EOF
% -*- bibtex -*-
% GitHub Starred Repositories
%
% Account:  $GH_USER
% Entries:  $COUNT
% Updated:  $TIMESTAMP
%
% Script:   $SCRIPT_PATH
% Usage:    $SCRIPT_PATH [output.bib]
% Default:  ~/org/resources/github-starred.bib
%
% Regenerate: Run the script to update this file.
% Citar will auto-detect this file if placed in org/resources/.

EOF

# entries 추가
cat "$TEMP_FILE" >> "$OUTPUT"
rm -f "$TEMP_FILE"

echo "Done! $COUNT entries written to $OUTPUT" >&2
