#!/usr/bin/env python3
"""
Tests for denote-export daemon cleanup on Ctrl+C / SIGINT / SIGTERM

이 테스트는 실제 Emacs daemon을 사용하지 않고,
cleanup 로직의 정확성을 검증합니다.

실행 방법:
    cd tests
    python3 -m pytest test_daemon_cleanup.py -v

    # pytest 없이 실행
    python3 test_daemon_cleanup.py
"""

import os
import sys
import time
import signal
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch, MagicMock, call

# Add bin directory to path for importing
BIN_DIR = Path(__file__).parent.parent / "bin"
sys.path.insert(0, str(BIN_DIR))


class TestDaemonCleanupBasic(unittest.TestCase):
    """기본 cleanup 함수 테스트 (mock 사용)"""

    def test_stop_daemons_calls_emacsclient(self):
        """stop_daemons가 모든 daemon에 대해 emacsclient를 호출하는지 확인"""
        # Import after path setup
        from denote_export_parallel_testable import stop_daemons

        with patch('subprocess.run') as mock_run:
            mock_run.return_value = MagicMock(returncode=0)

            stop_daemons(3)

            # 3개 daemon에 대해 호출되어야 함
            self.assertEqual(mock_run.call_count, 3)

            # 각 daemon 이름이 올바른지 확인
            calls = mock_run.call_args_list
            for i, c in enumerate(calls, 1):
                args = c[0][0]  # positional args
                self.assertIn(f'denote-export-daemon-{i}', args)
                self.assertIn('(kill-emacs)', args)

    def test_cleanup_daemons_sets_flag(self):
        """cleanup_daemons가 _cleanup_done 플래그를 설정하는지 확인"""
        from denote_export_parallel_testable import (
            cleanup_daemons, reset_cleanup_state, get_cleanup_state
        )

        with patch('subprocess.run'):
            reset_cleanup_state(num_daemons=2)
            self.assertFalse(get_cleanup_state()['cleanup_done'])

            cleanup_daemons()

            self.assertTrue(get_cleanup_state()['cleanup_done'])

    def test_cleanup_daemons_only_runs_once(self):
        """cleanup_daemons가 한 번만 실행되는지 확인 (중복 호출 방지)"""
        from denote_export_parallel_testable import (
            cleanup_daemons, reset_cleanup_state
        )

        with patch('subprocess.run') as mock_run:
            mock_run.return_value = MagicMock(returncode=0)

            reset_cleanup_state(num_daemons=2)

            # 첫 번째 호출
            cleanup_daemons()
            first_call_count = mock_run.call_count

            # 두 번째 호출 (무시되어야 함)
            cleanup_daemons()
            second_call_count = mock_run.call_count

            self.assertEqual(first_call_count, second_call_count)

    def test_cleanup_skipped_when_no_daemons(self):
        """daemon 수가 0이면 cleanup을 건너뛰는지 확인"""
        from denote_export_parallel_testable import (
            cleanup_daemons, reset_cleanup_state
        )

        with patch('subprocess.run') as mock_run:
            reset_cleanup_state(num_daemons=0)

            cleanup_daemons()

            mock_run.assert_not_called()


class TestSignalHandling(unittest.TestCase):
    """Signal handler 테스트"""

    def test_signal_handler_calls_cleanup(self):
        """signal_handler가 cleanup_daemons를 호출하는지 확인"""
        from denote_export_parallel_testable import (
            signal_handler, reset_cleanup_state, get_cleanup_state
        )

        with patch('subprocess.run'):
            with patch('sys.exit') as mock_exit:
                reset_cleanup_state(num_daemons=2)

                # SIGINT 시뮬레이션
                signal_handler(signal.SIGINT, None)

                self.assertTrue(get_cleanup_state()['cleanup_done'])
                mock_exit.assert_called_once_with(128 + signal.SIGINT)

    def test_signal_handler_exit_code(self):
        """signal_handler가 올바른 exit code로 종료하는지 확인"""
        from denote_export_parallel_testable import (
            signal_handler, reset_cleanup_state
        )

        with patch('subprocess.run'):
            with patch('sys.exit') as mock_exit:
                reset_cleanup_state(num_daemons=1)

                # SIGTERM
                signal_handler(signal.SIGTERM, None)

                mock_exit.assert_called_with(128 + signal.SIGTERM)


class TestPkillFallback(unittest.TestCase):
    """pkill fallback 테스트"""

    def test_pkill_called_after_graceful_shutdown(self):
        """graceful shutdown 후 pkill이 호출되는지 확인"""
        from denote_export_parallel_testable import (
            cleanup_daemons, reset_cleanup_state
        )

        call_order = []

        def track_calls(*args, **kwargs):
            cmd = args[0] if args else kwargs.get('args', [])
            if 'emacsclient' in cmd:
                call_order.append('emacsclient')
            elif 'pkill' in cmd:
                call_order.append('pkill')
            return MagicMock(returncode=0)

        with patch('subprocess.run', side_effect=track_calls):
            reset_cleanup_state(num_daemons=2)
            cleanup_daemons()

            # emacsclient가 먼저, 그 다음 pkill
            emacs_count = call_order.count('emacsclient')
            pkill_idx = call_order.index('pkill') if 'pkill' in call_order else -1

            self.assertEqual(emacs_count, 2)  # 2개 daemon
            self.assertGreater(pkill_idx, 0)  # pkill은 emacsclient 후에


class TestIntegration(unittest.TestCase):
    """실제 프로세스를 사용한 통합 테스트 (가벼운 버전)"""

    def test_subprocess_timeout_handling(self):
        """subprocess timeout이 제대로 처리되는지 확인"""
        from denote_export_parallel_testable import (
            cleanup_daemons, reset_cleanup_state
        )

        def timeout_side_effect(*args, **kwargs):
            raise subprocess.TimeoutExpired(cmd=args[0], timeout=3)

        with patch('subprocess.run', side_effect=timeout_side_effect):
            reset_cleanup_state(num_daemons=1)

            # timeout이 발생해도 예외 없이 완료되어야 함
            try:
                cleanup_daemons()
                completed = True
            except Exception as e:
                completed = False

            self.assertTrue(completed, "cleanup should handle timeout gracefully")


class TestRealDaemonCleanup(unittest.TestCase):
    """실제 Emacs daemon을 사용한 테스트 (선택적)

    이 테스트는 실제 Emacs가 설치되어 있어야 합니다.
    CI 환경에서는 건너뜁니다.
    """

    @unittest.skipUnless(
        subprocess.run(['which', 'emacs'], capture_output=True).returncode == 0,
        "Emacs not installed"
    )
    def test_real_daemon_start_and_cleanup(self):
        """실제 Emacs daemon 시작 및 정리 테스트"""
        daemon_name = "test-cleanup-daemon-1"

        # 기존 daemon 정리
        subprocess.run(
            ['emacsclient', '-s', daemon_name, '--eval', '(kill-emacs)'],
            capture_output=True
        )
        time.sleep(0.5)

        # Daemon 시작 (--quick으로 빠르게)
        proc = subprocess.Popen(
            ['emacs', '--quick', f'--daemon={daemon_name}'],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL
        )

        # Daemon이 시작될 때까지 대기
        time.sleep(3)

        # Daemon이 실행 중인지 확인
        result = subprocess.run(
            ['emacsclient', '-s', daemon_name, '--eval', 't'],
            capture_output=True,
            timeout=5
        )
        daemon_running = result.returncode == 0

        # 정리
        subprocess.run(
            ['emacsclient', '-s', daemon_name, '--eval', '(kill-emacs)'],
            capture_output=True,
            timeout=5
        )
        time.sleep(0.5)

        # Daemon이 종료되었는지 확인
        result = subprocess.run(
            ['emacsclient', '-s', daemon_name, '--eval', 't'],
            capture_output=True,
            timeout=2
        )
        daemon_stopped = result.returncode != 0

        self.assertTrue(daemon_running, "Daemon should have started")
        self.assertTrue(daemon_stopped, "Daemon should have stopped after cleanup")


class TestExecutorShutdown(unittest.TestCase):
    """Executor shutdown 테스트"""

    def test_executor_shutdown_on_signal(self):
        """signal handler가 executor를 shutdown하는지 확인"""
        from denote_export_parallel_testable import (
            signal_handler, reset_cleanup_state, set_executor
        )
        from concurrent.futures import ProcessPoolExecutor

        with patch('subprocess.run'):
            with patch('sys.exit'):
                reset_cleanup_state(num_daemons=1)

                # Mock executor
                mock_executor = MagicMock()
                set_executor(mock_executor)

                signal_handler(signal.SIGINT, None)

                # Executor should be shutdown
                mock_executor.shutdown.assert_called_once_with(
                    wait=False, cancel_futures=True
                )


class TestCtrlCSimulation(unittest.TestCase):
    """Ctrl+C (SIGINT) 시뮬레이션 테스트

    실제로 subprocess를 fork하고 SIGINT를 보내서 테스트합니다.
    """

    def test_sigint_triggers_cleanup(self):
        """SIGINT가 cleanup을 트리거하는지 확인"""
        # 테스트용 스크립트 생성
        test_script = '''
import sys
import time
import signal
sys.path.insert(0, "{bin_dir}")

# Testable 모듈 사용
from denote_export_parallel_testable import (
    reset_cleanup_state, register_signal_handlers
)

# 상태 초기화 및 signal handler 등록
reset_cleanup_state(num_daemons=2)
register_signal_handlers()

# SIGINT 대기 (실제로는 parent가 보냄)
print("READY", flush=True)
time.sleep(10)
'''.format(bin_dir=str(BIN_DIR))

        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
            f.write(test_script)
            script_path = f.name

        try:
            proc = subprocess.Popen(
                [sys.executable, script_path],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )

            # "READY" 출력 대기
            try:
                line = proc.stdout.readline()
                if "READY" in line:
                    # SIGINT 전송
                    proc.send_signal(signal.SIGINT)

                    # 프로세스 종료 대기
                    proc.wait(timeout=5)

                    # Exit code 확인 (128 + SIGINT = 130)
                    # Note: signal handler에서 sys.exit 호출 시
                    self.assertIn(proc.returncode, [130, -2, 1])
            except subprocess.TimeoutExpired:
                proc.kill()
                self.fail("Process did not exit after SIGINT")

        finally:
            os.unlink(script_path)


def create_testable_module():
    """테스트 가능한 모듈 파일 생성

    원본 denote-export-parallel.py를 테스트 가능하게 래핑합니다.
    """
    testable_path = BIN_DIR / "denote_export_parallel_testable.py"

    content = '''#!/usr/bin/env python3
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

    print("\\n[INFO] ====== Emergency Cleanup ======", flush=True)

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
    print(f"\\n[WARN] Received {sig_name}, stopping workers and cleaning up...", flush=True)

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
'''

    with open(testable_path, 'w') as f:
        f.write(content)

    return testable_path


# 테스트 실행 전 testable 모듈 생성
if __name__ == '__main__':
    print("=" * 60)
    print("Denote Export Daemon Cleanup Tests")
    print("=" * 60)
    print()

    # Testable 모듈 생성
    testable_path = create_testable_module()
    print(f"Created testable module: {testable_path}")
    print()

    # 테스트 실행
    unittest.main(verbosity=2)
