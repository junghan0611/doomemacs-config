#!/usr/bin/env python3
"""
edge-tts-convert.py - TTS 텍스트를 MP3로 변환 (진행 상황 표시)

Usage:
    edge-tts-convert.py INPUT_TXT [OUTPUT_MP3]
    edge-tts-convert.py --voice ko-KR-SunHiNeural --rate +50% input.txt

Options:
    --voice VOICE   음성 선택 (기본: ko-KR-HyunsuMultilingualNeural)
    --rate RATE     속도 조정 (기본: +25%)
    --volume VOL    음량 조정 (기본: +0%)
    --pitch PITCH   음높이 조정 (기본: +0Hz)
"""

import argparse
import os
import subprocess
import sys
import time
from pathlib import Path

DEFAULT_VOICE = "ko-KR-HyunsuMultilingualNeural"
DEFAULT_RATE = "+25%"
DEFAULT_VOLUME = "+0%"
DEFAULT_PITCH = "+0Hz"


def convert_text_to_mp3(
    input_file: str,
    output_file: str,
    voice: str = DEFAULT_VOICE,
    rate: str = DEFAULT_RATE,
    volume: str = DEFAULT_VOLUME,
    pitch: str = DEFAULT_PITCH,
) -> bool:
    """텍스트 파일을 MP3로 변환 (edge-tts CLI 사용)"""

    # 텍스트 파일 읽기
    text = Path(input_file).read_text(encoding="utf-8").strip()
    text_len = len(text)

    # 예상 시간 계산 (1글자당 약 0.04초, 실측: 23001자→918초)
    estimated_seconds = text_len * 0.04
    estimated_minutes = estimated_seconds / 60

    # 예상 파일 크기 (1글자당 약 700바이트, 실측: 23001자→15.3MB)
    estimated_size_mb = text_len * 700 / 1024 / 1024

    print(f"변환 시작: {text_len:,}자", file=sys.stderr)
    print(f"음성: {voice}, 속도: {rate}", file=sys.stderr)
    print(f"예상 시간: {estimated_minutes:.0f}분, 예상 크기: {estimated_size_mb:.1f}MB", file=sys.stderr)
    print(f"출력 파일: {output_file}", file=sys.stderr)
    print("-" * 50, file=sys.stderr)

    start_time = time.time()

    # edge-tts CLI 명령어 구성
    cmd = [
        "edge-tts",
        "--voice", voice,
        "--rate", rate,
        "--volume", volume,
        "--pitch", pitch,
        "--file", input_file,
        "--write-media", output_file,
    ]

    # 프로세스 실행 (진행 상황은 edge-tts가 직접 출력)
    try:
        # stderr를 실시간으로 표시
        process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )

        # 진행 상황 표시 (파일 크기 모니터링)
        output_path = Path(output_file)
        last_size = 0
        dots = 0

        while process.poll() is None:
            time.sleep(2)
            if output_path.exists():
                current_size = output_path.stat().st_size
                if current_size > last_size:
                    dots += 1
                    elapsed = time.time() - start_time
                    progress_mb = current_size / 1024 / 1024
                    print(f"\r진행 중: {progress_mb:.1f}MB / {estimated_size_mb:.1f}MB ({elapsed:.0f}초 경과)",
                          end="", file=sys.stderr)
                    last_size = current_size

        stdout, stderr = process.communicate()

        if process.returncode != 0:
            print(f"\n오류: {stderr}", file=sys.stderr)
            return False

    except FileNotFoundError:
        print("edge-tts를 찾을 수 없습니다. 설치하세요: pip install edge-tts", file=sys.stderr)
        return False
    except Exception as e:
        print(f"\n변환 실패: {e}", file=sys.stderr)
        return False

    elapsed = time.time() - start_time

    if output_path.exists():
        file_size = output_path.stat().st_size / 1024 / 1024
        print(f"\n\n완료: {file_size:.1f}MB, {elapsed:.0f}초 소요", file=sys.stderr)
        return True
    else:
        print("\n변환 실패: 파일이 생성되지 않음", file=sys.stderr)
        return False


def main():
    parser = argparse.ArgumentParser(
        description="TTS 텍스트를 MP3로 변환 (진행 상황 표시)"
    )
    parser.add_argument("input_txt", help="입력 텍스트 파일")
    parser.add_argument("output_mp3", nargs="?", help="출력 MP3 파일 (생략 시 자동 생성)")
    parser.add_argument("--voice", default=DEFAULT_VOICE, help="음성")
    parser.add_argument("--rate", default=DEFAULT_RATE, help="속도 (예: +25%%)")
    parser.add_argument("--volume", default=DEFAULT_VOLUME, help="음량 (예: +0%%)")
    parser.add_argument("--pitch", default=DEFAULT_PITCH, help="음높이 (예: +0Hz)")

    args = parser.parse_args()

    # 입력 파일 확인
    input_path = Path(args.input_txt)
    if not input_path.exists():
        print(f"파일을 찾을 수 없습니다: {args.input_txt}", file=sys.stderr)
        sys.exit(1)

    # 텍스트 확인
    text = input_path.read_text(encoding="utf-8").strip()
    if not text:
        print("텍스트가 비어있습니다.", file=sys.stderr)
        sys.exit(1)

    # 출력 파일명 결정
    if args.output_mp3:
        output_path = Path(args.output_mp3)
    else:
        # 입력 파일명에서 .txt를 .mp3로 변경
        output_path = input_path.with_suffix(".mp3")

    # 변환 실행
    success = convert_text_to_mp3(
        str(input_path),
        str(output_path),
        voice=args.voice,
        rate=args.rate,
        volume=args.volume,
        pitch=args.pitch,
    )

    if success:
        print(str(output_path))  # 성공 시 출력 경로 반환
    else:
        sys.exit(1)


if __name__ == "__main__":
    main()
