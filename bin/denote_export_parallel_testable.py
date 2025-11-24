#!/usr/bin/env python3
"""
Testable wrapper for denote-export-parallel.py

이 모듈은 테스트를 위해 상태를 초기화하고 접근할 수 있게 합니다.
"""

import subprocess
import signal
import sys

# Global state for cleanup
_num_daemons = 0
_cleanup_done = False
_executor = None
_interrupted = False


def reset_cleanup_state(num_daemons=0):
    """테스트를 위한 상태 초기화"""
    global _num_daemons, _cleanup_done, _executor, _interrupted
    _num_daemons = num_daemons
    _cleanup_done = False
    _executor = None
    _interrupted = False


def set_executor(executor):
    """테스트를 위한 executor 설정"""
    global _executor
    _executor = executor


def get_cleanup_state():
    """현재 상태 반환"""
    return {
        'num_daemons': _num_daemons,
        'cleanup_done': _cleanup_done
    }


def stop_daemons(num_daemons):
    """Stop all daemons."""
    for i in range(1, num_daemons + 1):
        daemon_name = f"denote-export-daemon-{i}"
        try:
            subprocess.run(
                ['emacsclient', '-s', daemon_name, '--eval', '(kill-emacs)'],
                capture_output=True,
                timeout=5
            )
            print(f"[INFO]   Daemon {i} stopped", flush=True)
        except Exception as e:
            print(f"[WARN]   Failed to stop daemon {i}: {e}", flush=True)


def cleanup_daemons():
    """Cleanup function called on exit/signal."""
    global _cleanup_done, _num_daemons
    if _cleanup_done or _num_daemons == 0:
        return
    _cleanup_done = True

    print("\n[INFO] ====== Emergency Cleanup ======", flush=True)

    # First, try graceful shutdown
    for i in range(1, _num_daemons + 1):
        daemon_name = f"denote-export-daemon-{i}"
        try:
            subprocess.run(
                ['emacsclient', '-s', daemon_name, '--eval', '(kill-emacs)'],
                capture_output=True,
                timeout=3
            )
            print(f"[INFO]   Daemon {i} stopped gracefully", flush=True)
        except:
            pass

    # Then, force kill any remaining processes
    try:
        result = subprocess.run(
            ['pkill', '-f', 'denote-export-daemon'],
            capture_output=True
        )
        if result.returncode == 0:
            print("[INFO]   Killed remaining daemons via pkill", flush=True)
    except:
        pass

    print("[INFO] ====== Cleanup completed ======", flush=True)


def signal_handler(signum, frame):
    """Handle SIGINT (Ctrl+C) and SIGTERM."""
    global _executor, _interrupted

    sig_name = signal.Signals(signum).name
    print(f"\n[WARN] Received {sig_name}, stopping workers and cleaning up...", flush=True)

    # Set interrupt flag
    _interrupted = True

    # Shutdown executor first
    if _executor is not None:
        print("[INFO] Shutting down executor...", flush=True)
        _executor.shutdown(wait=False, cancel_futures=True)

    cleanup_daemons()
    sys.exit(128 + signum)


def register_signal_handlers():
    """Signal handlers 등록"""
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)


if __name__ == '__main__':
    # 직접 실행 시 테스트
    print("Testing cleanup module...")
    reset_cleanup_state(num_daemons=2)
    print(f"State: {get_cleanup_state()}")
    print("Done!")
