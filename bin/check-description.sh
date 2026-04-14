#!/usr/bin/env bash
# check-description.sh — 공개 org 문서의 description/abstract 누락 검사
#
# Copyright (C) 2026 Junghan Kim
#
# 대상: ~/org/{botlog,notes,bib,meta} 아래 org 파일 중
#       #+export_file_name: 헤더가 있는 파일만 검사
# 검사:
#   1) #+description: 헤더가 있고 값이 비어있지 않은지
#   2) 본문에 [!abstract] callout 이 있는지

set -euo pipefail

ORG_ROOT="${HOME}/org"
DATE_STR="$(date +%F)"
MODE="summary"
BOTLOG_ONLY=0

TARGET_DIRS=(botlog notes bib meta)

declare -A TOTALS
declare -A DESC_MISSING_COUNTS
declare -A ABSTRACT_MISSING_COUNTS

declare -a BOTLOG_DESC_MISSING=()
declare -a BOTLOG_ABSTRACT_MISSING=()
declare -a NOTES_DESC_MISSING=()
declare -a NOTES_ABSTRACT_MISSING=()
declare -a BIB_DESC_MISSING=()
declare -a BIB_ABSTRACT_MISSING=()
declare -a META_DESC_MISSING=()
declare -a META_ABSTRACT_MISSING=()

usage() {
  cat <<EOF
Usage: $(basename "$0") [--summary|--list|--json] [--botlog-only]

Options:
  --summary      요약만 출력 (기본)
  --list         누락 파일 전체 리스트 출력
  --json         JSON 출력 (에이전트 소비용)
  --botlog-only  botlog만 검사
  -h, --help     도움말
EOF
}

for arg in "$@"; do
  case "$arg" in
    --summary) MODE="summary" ;;
    --list)    MODE="list" ;;
    --json)    MODE="json" ;;
    --botlog-only) BOTLOG_ONLY=1 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $arg" >&2; usage >&2; exit 2 ;;
  esac
done

if [[ $BOTLOG_ONLY -eq 1 ]]; then
  TARGET_DIRS=(botlog)
fi

path_pretty() {
  local path="$1"
  printf '%s' "${path/#$HOME/~}"
}

json_escape() {
  local s="$1"
  s=${s//\\/\\\\}
  s=${s//\"/\\\"}
  s=${s//$'\n'/\\n}
  s=${s//$'\r'/\\r}
  s=${s//$'\t'/\\t}
  printf '%s' "$s"
}

array_name() {
  local dir="$1"
  local kind="$2"
  local upper
  upper=$(echo "$dir" | tr '[:lower:]' '[:upper:]')
  if [[ "$kind" == "description" ]]; then
    echo "${upper}_DESC_MISSING"
  else
    echo "${upper}_ABSTRACT_MISSING"
  fi
}

append_missing() {
  local dir="$1"
  local kind="$2"
  local file="$3"
  local name
  name=$(array_name "$dir" "$kind")
  declare -n ref="$name"
  ref+=("$file")
}

check_file() {
  local file="$1"
  local dir="$2"
  local missing=0

  if ! grep -Eq '^#\+description:[[:space:]]*[^[:space:]].*$' "$file"; then
    DESC_MISSING_COUNTS[$dir]=$(( ${DESC_MISSING_COUNTS[$dir]:-0} + 1 ))
    append_missing "$dir" description "$file"
    missing=1
  fi

  if ! grep -Fq '[!abstract]' "$file"; then
    ABSTRACT_MISSING_COUNTS[$dir]=$(( ${ABSTRACT_MISSING_COUNTS[$dir]:-0} + 1 ))
    append_missing "$dir" abstract "$file"
    missing=1
  fi

  return $missing
}

scan_dir() {
  local dir="$1"
  local base="$ORG_ROOT/$dir"
  local file

  TOTALS[$dir]=0
  DESC_MISSING_COUNTS[$dir]=0
  ABSTRACT_MISSING_COUNTS[$dir]=0

  [[ -d "$base" ]] || return 0

  while IFS= read -r -d '' file; do
    TOTALS[$dir]=$(( ${TOTALS[$dir]:-0} + 1 ))
    check_file "$file" "$dir" || true
  done < <(
    find "$base" \( -type d -name agenda -prune \) -o \( -type f -name '*.org' -print0 \) \
      | while IFS= read -r -d '' candidate; do
          if grep -Eq '^#\+export_file_name:' "$candidate"; then
            printf '%s\0' "$candidate"
          fi
        done
  )
}

print_section_summary() {
  local dir="$1"
  local total=${TOTALS[$dir]:-0}
  local desc=${DESC_MISSING_COUNTS[$dir]:-0}
  local abstract=${ABSTRACT_MISSING_COUNTS[$dir]:-0}

  echo "[PASS] ${total}/${total} ${dir} 파일 검사 완료"
  echo "  ⚠ description 누락: ${desc}개"
  echo "  ⚠ abstract 누락: ${abstract}개"
  if [[ "$dir" == "notes" ]]; then
    echo "  (notes는 참고 표시만, botlog 우선)"
  fi
  echo ""
}

print_missing_list() {
  local dir="$1"
  local kind="$2"
  local title="$3"
  local name
  local file

  name=$(array_name "$dir" "$kind")
  declare -n ref="$name"

  [[ ${#ref[@]} -gt 0 ]] || return 0

  echo "--- ${title} ---"
  for file in "${ref[@]}"; do
    echo "  $(path_pretty "$file")"
  done
  echo ""
}

print_text_output() {
  local dir

  echo "=== Description/Abstract 검사 (${DATE_STR}) ==="
  echo ""

  for dir in "${TARGET_DIRS[@]}"; do
    print_section_summary "$dir"
    if [[ "$MODE" == "list" ]]; then
      print_missing_list "$dir" description "description 누락"
      print_missing_list "$dir" abstract "abstract 누락"
    fi
  done
}

print_json_array() {
  local name="$1"
  local idx
  declare -n ref="$name"

  echo -n '['
  for idx in "${!ref[@]}"; do
    [[ "$idx" -gt 0 ]] && echo -n ','
    echo -n '"'
    json_escape "$(path_pretty "${ref[$idx]}")"
    echo -n '"'
  done
  echo -n ']'
}

print_json_output() {
  local dir
  local first=1
  local total_missing=0

  for dir in "${TARGET_DIRS[@]}"; do
    total_missing=$(( total_missing + ${DESC_MISSING_COUNTS[$dir]:-0} + ${ABSTRACT_MISSING_COUNTS[$dir]:-0} ))
  done

  echo '{'
  echo "  \"date\": \"${DATE_STR}\"," 
  echo "  \"ok\": $([[ $total_missing -eq 0 ]] && echo true || echo false),"
  echo '  "targets": {'

  for dir in "${TARGET_DIRS[@]}"; do
    [[ $first -eq 0 ]] && echo ','
    first=0
    local desc_name abstract_name
    desc_name=$(array_name "$dir" description)
    abstract_name=$(array_name "$dir" abstract)

    echo "    \"${dir}\": {"
    echo "      \"total\": ${TOTALS[$dir]:-0},"
    echo "      \"missing_description\": ${DESC_MISSING_COUNTS[$dir]:-0},"
    echo "      \"missing_abstract\": ${ABSTRACT_MISSING_COUNTS[$dir]:-0},"
    echo -n '      "description_files": '
    print_json_array "$desc_name"
    echo ','
    echo -n '      "abstract_files": '
    print_json_array "$abstract_name"
    echo -n $'\n    }'
  done

  echo ''
  echo '  }'
  echo '}'
}

main() {
  local dir
  local total_missing=0

  for dir in "${TARGET_DIRS[@]}"; do
    scan_dir "$dir"
    total_missing=$(( total_missing + ${DESC_MISSING_COUNTS[$dir]:-0} + ${ABSTRACT_MISSING_COUNTS[$dir]:-0} ))
  done

  case "$MODE" in
    summary|list) print_text_output ;;
    json)         print_json_output ;;
  esac

  if [[ $total_missing -gt 0 ]]; then
    exit 1
  fi
}

main "$@"
