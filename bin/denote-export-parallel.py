#!/usr/bin/env python3
"""
Denote Export Parallel - Python multiprocessing for NBSP-safe parallel export

This script handles Unicode filenames (including NBSP U+00A0) correctly
and manages multiple Emacs daemons for parallel processing.

Features:
- Automatic daemon management (start/stop)
- Round-robin file distribution
- True parallel processing
- Unicode-safe (NBSP U+00A0)
"""

import os
import sys
import subprocess
import time
import signal
import atexit
from pathlib import Path
from concurrent.futures import ProcessPoolExecutor, as_completed

# Configuration
HOME = Path.home()
SERVER_SCRIPT = HOME / "repos/gh/doomemacs-config/bin/denote-export-server.el"

# Global state for cleanup
_num_daemons = 0
_cleanup_done = False
_executor = None  # Global executor reference for signal handler
_interrupted = False  # Flag to stop processing


def cleanup_daemons():
    """Cleanup function called on exit/signal."""
    global _cleanup_done, _num_daemons, _is_main_process
    # Only cleanup from main process
    if not _is_main_process or _cleanup_done or _num_daemons == 0:
        return
    _cleanup_done = True

    print("\n[INFO] ====================================== Emergency Cleanup ======================================", flush=True)

    # First, try graceful shutdown
    for i in range(1, _num_daemons + 1):
        daemon_name = f"denote-export-daemon-{i}"
        try:
            subprocess.run(
                ['emacsclient', '-s', daemon_name, '--eval', '(kill-emacs)'],
                capture_output=True,
                timeout=3
            )
            print(f"[INFO]   ✓ Daemon {i} stopped gracefully", flush=True)
        except:
            pass

    # Then, force kill any remaining processes
    try:
        result = subprocess.run(
            ['pkill', '-f', 'denote-export-daemon'],
            capture_output=True
        )
        if result.returncode == 0:
            print("[INFO]   ✓ Killed remaining daemon processes via pkill", flush=True)
    except:
        pass

    print("[INFO] ✓ Emergency cleanup completed!", flush=True)
    print("[INFO] =============================================================================================", flush=True)


def signal_handler(signum, frame):
    """Handle SIGINT (Ctrl+C) and SIGTERM."""
    global _executor, _interrupted, _is_main_process

    # Only handle in main process
    if not _is_main_process:
        return

    sig_name = signal.Signals(signum).name
    print(f"\n[WARN] Received {sig_name}, stopping workers and cleaning up...", flush=True)

    # Set interrupt flag to stop submitting new tasks
    _interrupted = True

    # Shutdown executor first (cancel pending futures, don't wait for running ones)
    if _executor is not None:
        print("[INFO] Shutting down executor...", flush=True)
        _executor.shutdown(wait=False, cancel_futures=True)

    # Then cleanup daemons
    cleanup_daemons()
    sys.exit(128 + signum)


# Register cleanup handlers (only in main process)
# Worker processes will inherit these but we check _is_main_process
_is_main_process = True

def _register_handlers():
    """Register signal handlers - call only from main process."""
    atexit.register(cleanup_daemons)
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)

def is_daemon_running(daemon_name):
    """Check if daemon is running."""
    try:
        result = subprocess.run(
            ['emacsclient', '-s', daemon_name, '--eval', 't'],
            capture_output=True,
            timeout=2
        )
        return result.returncode == 0
    except:
        return False

def start_daemons(num_daemons):
    """Start daemons and wait until they're ready."""
    for i in range(1, num_daemons + 1):
        daemon_name = f"denote-export-daemon-{i}"

        # Stop if already running
        if is_daemon_running(daemon_name):
            print(f"[INFO] Daemon {i} already running, stopping...", flush=True)
            subprocess.run(
                ['emacsclient', '-s', daemon_name, '--eval', '(kill-emacs)'],
                capture_output=True
            )
            time.sleep(1)

        # Start daemon
        print(f"[INFO] Creating daemon {i}: {daemon_name}", flush=True)
        subprocess.Popen(
            ['emacs', '--quick', f'--daemon={daemon_name}', '--load', str(SERVER_SCRIPT)],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL
        )

        # Wait for daemon to start
        time.sleep(3)

        # Wait for ready flag
        timeout = 60
        elapsed = 0
        while elapsed < timeout:
            try:
                result = subprocess.run(
                    ['emacsclient', '-s', daemon_name, '--eval', "(boundp 'denote-export-server-ready)"],
                    capture_output=True,
                    text=True,
                    timeout=2
                )
                if result.returncode == 0 and 't' in result.stdout:
                    print(f"[INFO]   ✓ Daemon {i} initialized and ready!", flush=True)
                    break
            except:
                pass

            time.sleep(1)
            elapsed += 1

        if elapsed >= timeout:
            print(f"[ERROR] Daemon {i} initialization timeout", flush=True)
            sys.exit(1)

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
            print(f"[INFO]   ✓ Daemon {i} stopped successfully", flush=True)
        except Exception as e:
            print(f"[WARN]   ✗ Failed to stop daemon {i}: {e}", flush=True)

def export_file_via_daemon(args):
    """Export a single file via emacsclient to a specific daemon."""
    global _is_main_process
    # Mark as worker process to prevent signal handler execution
    _is_main_process = False

    file_path, daemon_id = args
    daemon_name = f"denote-export-daemon-{daemon_id}"

    # Escape quotes in file path for Elisp string
    file_path_escaped = str(file_path).replace('"', '\\"')
    elisp_cmd = f'(denote-export-file "{file_path_escaped}")'

    try:
        result = subprocess.run(
            ['emacsclient', '-s', daemon_name, '--eval', elisp_cmd],
            capture_output=True,
            text=True,
            timeout=360
        )

        basename = file_path.name
        if result.returncode == 0 and 'SUCCESS:' in result.stdout:
            print(f"✓ [D{daemon_id}] {basename}", flush=True)
            return True, basename
        else:
            print(f"✗ [D{daemon_id}] {basename}", flush=True)
            if result.returncode != 0:
                print(f"  Return code: {result.returncode}", flush=True)
            if result.stderr:
                print(f"  Stderr: {result.stderr[:200]}", flush=True)
            if result.stdout and 'SUCCESS:' not in result.stdout:
                print(f"  Stdout: {result.stdout[:200]}", flush=True)
            return False, basename
    except Exception as e:
        print(f"✗ [D{daemon_id}] {file_path.name} - Error: {e}")
        return False, file_path.name

def main():
    global _num_daemons

    # Register signal handlers in main process
    _register_handlers()

    if len(sys.argv) < 3:
        print("Usage: denote-export-parallel.py <directory> <num_daemons>")
        sys.exit(1)

    directory = Path(sys.argv[1])
    num_daemons = int(sys.argv[2])

    # Set global for cleanup handler
    _num_daemons = num_daemons

    if not directory.exists():
        print(f"Error: Directory not found: {directory}")
        sys.exit(1)

    # Get all .org files (Python handles Unicode perfectly)
    org_files = sorted(directory.glob("*.org"))
    total_files = len(org_files)

    print(f"[INFO] Found {total_files} files in {directory}", flush=True)
    print(f"[INFO] Using {num_daemons} parallel daemons", flush=True)
    print(f"[INFO] NBSP(U+00A0) safe: Python handles Unicode correctly", flush=True)
    print(flush=True)

    # Start daemons
    print(f"[INFO] ====================================== Daemon Lifecycle ======================================", flush=True)
    start_daemons(num_daemons)
    print(f"[INFO] ✓ All {num_daemons} daemons created and ready!", flush=True)
    print(f"[INFO] =============================================================================================", flush=True)
    print(flush=True)

    try:
        # Assign files to daemons (round-robin)
        file_daemon_pairs = []
        for idx, file_path in enumerate(org_files):
            daemon_id = (idx % num_daemons) + 1
            file_daemon_pairs.append((file_path, daemon_id))

        # Process in parallel
        global _executor, _interrupted
        start_time = time.time()
        success_count = 0
        error_count = 0
        cancelled_count = 0

        _executor = ProcessPoolExecutor(max_workers=num_daemons)
        try:
            futures = {_executor.submit(export_file_via_daemon, pair): pair
                      for pair in file_daemon_pairs}

            for future in as_completed(futures):
                if _interrupted:
                    # Cancel remaining futures and break
                    for f in futures:
                        f.cancel()
                    break

                try:
                    success, basename = future.result(timeout=1)
                    if success:
                        success_count += 1
                    else:
                        error_count += 1
                except Exception as e:
                    if not _interrupted:
                        print(f"[WARN] Future error: {e}", flush=True)
                    cancelled_count += 1
        finally:
            _executor.shutdown(wait=False, cancel_futures=True)
            _executor = None

        duration = time.time() - start_time
        processed = success_count + error_count
        speed = processed / duration if duration > 0 else 0

        print()
        print(f"[INFO] ========================================")
        if _interrupted:
            print(f"[INFO] Export interrupted!")
            print(f"[INFO] Processed: {processed}/{total_files}, Success: {success_count}, Errors: {error_count}")
        else:
            print(f"[INFO] Export completed!")
            print(f"[INFO] Success: {success_count}, Errors: {error_count}, Total: {total_files}")
        print(f"[INFO] Duration: {duration:.1f}s, Speed: {speed:.2f} files/sec")
        print(f"[INFO] ========================================")

        return 0 if error_count == 0 and not _interrupted else 1

    finally:
        # ALWAYS cleanup daemons (even on error/interrupt)
        print()
        print(f"[INFO] ====================================== Daemon Cleanup ======================================", flush=True)
        stop_daemons(num_daemons)
        print(f"[INFO] ✓ All {num_daemons} daemons stopped and cleaned up!", flush=True)
        print(f"[INFO] =============================================================================================", flush=True)

if __name__ == '__main__':
    sys.exit(main())
