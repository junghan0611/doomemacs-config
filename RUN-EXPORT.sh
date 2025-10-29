#!/usr/bin/env bash
# 잠자기 전 실행 스크립트

cd ~/repos/gh/doomemacs-config

echo "======================================"
echo "Denote Export 백그라운드 실행"
echo "======================================"
echo ""

# 백그라운드 실행
nohup ./bin/denote-export-safe.sh > /tmp/export-night-$(date +%Y%m%d).out 2>&1 &
PID=$!

echo "✓ 프로세스 시작됨"
echo "  PID: $PID"
echo "  로그: /tmp/export-night-$(date +%Y%m%d).out"
echo ""
echo "확인 명령어:"
echo "  tail -f /tmp/export-night-$(date +%Y%m%d).out"
echo "  ps -p $PID"
echo ""
echo "완료 후 확인:"
echo "  cat /tmp/denote-export-safe-*.log | grep -E 'ERROR|✗|completed'"
echo "  ls ~/repos/gh/notes/content/{meta,bib,notes}/*.md | wc -l"
echo ""
echo "잘 자세요! 내일 아침 결과를 확인하세요."
echo "======================================"
