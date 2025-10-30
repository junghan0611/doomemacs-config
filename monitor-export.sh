#!/usr/bin/env bash
# 내보내기 진행 상황 모니터링

LOG_FILE="/tmp/denote-export-meta-multi-20251030.log"

echo "=== Denote Export 진행 상황 ==="
echo ""

# 프로세스 확인
if ps aux | grep -q "[d]enote-export-parallel"; then
    echo "✅ 실행 중"
else
    echo "⏹️  완료 또는 중단됨"
fi

echo ""
echo "=== 통계 ==="
SUCCESS=$(grep -c "^✓" "$LOG_FILE" 2>/dev/null || echo 0)
FAIL=$(grep -c "^✗" "$LOG_FILE" 2>/dev/null || echo 0)
TOTAL=$((SUCCESS + FAIL))

echo "성공: $SUCCESS"
echo "실패: $FAIL"
echo "처리: $TOTAL / 530"
echo "진행률: $((TOTAL * 100 / 530))%"

echo ""
echo "=== 최근 5개 ==="
tail -5 "$LOG_FILE" 2>/dev/null | grep -E "^[✓✗]"
