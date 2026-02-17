#!/usr/bin/env python3
"""
Denote Export/Dblock Parallel - Python multiprocessing for NBSP-safe parallel processing

This script handles Unicode filenames (including NBSP U+00A0) correctly
and manages multiple Emacs daemons for parallel processing.

Modes:
- export: Org → Hugo MD conversion
- dblock: Dynamic block update

Features:
- Automatic daemon management (start/stop/reuse)
- Round-robin file distribution
- True parallel processing
- Unicode-safe (NBSP U+00A0)
- Incremental export (mtime-based)
- Profiling stats (GC, memory, speed)
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
SCRIPT_DIR = Path(__file__).parent.resolve()
SERVER_SCRIPT = SCRIPT_DIR / "denote-export.el"
TIMESTAMP_DIR = SCRIPT_DIR / ".last-export"

# Global state for cleanup
_num_daemons = 0
_cleanup_done = False
_executor = None  # Global executor reference for signal handler
_interrupted = False  # Flag to stop processing
_mode = "export"  # Current mode: export or dblock
_no_cleanup = False  # Skip daemon cleanup on exit


def cleanup_daemons():
    """Cleanup function called on exit/signal."""
    global _cleanup_done, _num_daemons, _is_main_process, _no_cleanup
    # Only cleanup from main process
    if not _is_main_process or _cleanup_done or _num_daemons == 0:
        return
    if _no_cleanup:
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
    global _executor, _interrupted, _is_main_process, _no_cleanup

    # Only handle in main process
    if not _is_main_process:
        return

    sig_name = signal.Signals(signum).name
    print(f"\n[WARN] Received {sig_name}, stopping workers and cleaning up...", flush=True)

    # Set interrupt flag to stop submitting new tasks
    _interrupted = True

    # Force cleanup on signal even if --no-cleanup was set
    _no_cleanup = False

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
    """Check if daemon is running and ready."""
    try:
        result = subprocess.run(
            ['emacsclient', '-s', daemon_name, '--eval',
             "(boundp 'denote-export-server-ready)"],
            capture_output=True,
            text=True,
            timeout=3
        )
        return result.returncode == 0 and 't' in result.stdout
    except:
        return False

def start_daemons(num_daemons):
    """Start daemons or reuse existing ones."""
    reused = 0
    started = 0

    for i in range(1, num_daemons + 1):
        daemon_name = f"denote-export-daemon-{i}"

        # Reuse if already running and ready
        if is_daemon_running(daemon_name):
            # Reset file counter for fresh profiling
            subprocess.run(
                ['emacsclient', '-s', daemon_name, '--eval',
                 '(setq denote-export-file-counter 0)'],
                capture_output=True, timeout=3
            )
            print(f"[INFO]   ♻ Daemon {i} reused (already running)", flush=True)
            reused += 1
            continue

        # Start new daemon
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

        started += 1

    if reused > 0:
        print(f"[INFO] Daemons: {reused} reused, {started} new (total {num_daemons})", flush=True)

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

def get_daemon_profile(daemon_id):
    """Query profiling stats from a daemon."""
    daemon_name = f"denote-export-daemon-{daemon_id}"
    try:
        result = subprocess.run(
            ['emacsclient', '-s', daemon_name, '--eval',
             '(format "%d %d %.3f %d" (emacs-pid) gcs-done gc-elapsed denote-export-file-counter)'],
            capture_output=True, text=True, timeout=5
        )
        if result.returncode == 0:
            # Parse: "PID GC_COUNT GC_ELAPSED FILE_COUNT"
            raw = result.stdout.strip().strip('"')
            parts = raw.split()
            if len(parts) == 4:
                pid = int(parts[0])
                gc_count = int(parts[1])
                gc_elapsed = float(parts[2])
                file_count = int(parts[3])
                # Get RSS from /proc
                rss_mb = 0
                try:
                    status = Path(f"/proc/{pid}/status").read_text()
                    for line in status.split('\n'):
                        if line.startswith('VmRSS:'):
                            rss_kb = int(line.split()[1])
                            rss_mb = rss_kb / 1024
                            break
                except:
                    pass
                return {
                    'daemon': daemon_id, 'pid': pid,
                    'gc_count': gc_count, 'gc_elapsed': gc_elapsed,
                    'file_count': file_count, 'rss_mb': rss_mb
                }
    except:
        pass
    return None

def print_profile_summary(num_daemons, duration, success_count, error_count, total_files):
    """Print profiling summary with per-daemon stats."""
    print(flush=True)
    print(f"[PROFILE] ====================================== Performance Summary ======================================", flush=True)

    # Per-daemon stats
    total_gc = 0
    total_gc_time = 0
    total_rss = 0
    max_rss = 0
    profiles = []

    for i in range(1, num_daemons + 1):
        p = get_daemon_profile(i)
        if p:
            profiles.append(p)
            total_gc += p['gc_count']
            total_gc_time += p['gc_elapsed']
            total_rss += p['rss_mb']
            if p['rss_mb'] > max_rss:
                max_rss = p['rss_mb']

    # Table header
    print(f"[PROFILE]  Daemon | Files | GC Count | GC Time  | RSS (MB)", flush=True)
    print(f"[PROFILE]  -------+-------+----------+----------+--------", flush=True)
    for p in profiles:
        print(f"[PROFILE]  D{p['daemon']:>4}  | {p['file_count']:>5} | {p['gc_count']:>8} | {p['gc_elapsed']:>6.1f}s  | {p['rss_mb']:>6.0f}", flush=True)

    # Summary
    processed = success_count + error_count
    speed = processed / duration if duration > 0 else 0

    print(f"[PROFILE]  -------+-------+----------+----------+--------", flush=True)
    print(f"[PROFILE]  Total  | {processed:>5} | {total_gc:>8} | {total_gc_time:>6.1f}s  | {total_rss:>6.0f}", flush=True)
    print(f"[PROFILE]", flush=True)
    print(f"[PROFILE]  Files:    {success_count} success, {error_count} errors / {total_files} total", flush=True)
    print(f"[PROFILE]  Duration: {duration:.1f}s ({duration/60:.1f}min)", flush=True)
    print(f"[PROFILE]  Speed:    {speed:.2f} files/sec", flush=True)
    print(f"[PROFILE]  Daemons:  {num_daemons} (max RSS: {max_rss:.0f}MB, total: {total_rss:.0f}MB)", flush=True)
    if total_gc > 0:
        print(f"[PROFILE]  GC:       {total_gc} runs, {total_gc_time:.1f}s ({total_gc_time/duration*100:.1f}% of wall time)", flush=True)
    print(f"[PROFILE] =============================================================================================", flush=True)

def process_file_via_daemon(args):
    """Process a single file via emacsclient to a specific daemon."""
    global _is_main_process, _mode
    # Mark as worker process to prevent signal handler execution
    _is_main_process = False

    file_path, daemon_id, mode = args
    daemon_name = f"denote-export-daemon-{daemon_id}"

    # Escape quotes in file path for Elisp string
    file_path_escaped = str(file_path).replace('"', '\\"')

    # Choose function based on mode
    if mode == "dblock":
        elisp_cmd = f'(denote-dblock-update-file "{file_path_escaped}")'
        success_marker = None  # dblock doesn't return SUCCESS:
    else:
        elisp_cmd = f'(denote-export-file "{file_path_escaped}")'
        success_marker = "SUCCESS:"

    try:
        result = subprocess.run(
            ['emacsclient', '-s', daemon_name, '--eval', elisp_cmd],
            capture_output=True,
            text=True,
            timeout=360
        )

        basename = file_path.name

        # Check success
        if mode == "dblock":
            # dblock: success if no error
            if result.returncode == 0:
                print(f"✓ [D{daemon_id}] {basename}", flush=True)
                return True, basename
            else:
                print(f"✗ [D{daemon_id}] {basename}", flush=True)
                if result.stderr:
                    print(f"  Stderr: {result.stderr[:200]}", flush=True)
                return False, basename
        else:
            # export: success if SUCCESS: in output
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

def get_last_export_time(source_dir):
    """디렉토리의 마지막 export 타임스탬프 조회."""
    ts_file = TIMESTAMP_DIR / f"last-export-{source_dir.name}"
    if ts_file.exists():
        return float(ts_file.read_text().strip())
    return 0

def save_export_time(source_dir, timestamp):
    """지정된 시각을 마지막 export 타임스탬프로 저장."""
    TIMESTAMP_DIR.mkdir(parents=True, exist_ok=True)
    ts_file = TIMESTAMP_DIR / f"last-export-{source_dir.name}"
    ts_file.write_text(str(timestamp))

def filter_changed_files(org_files, source_dir, force=False):
    """마지막 export 이후 변경된 파일만 필터링."""
    if force:
        print(f"[FILTER] Force mode: 전체 {len(org_files)}개 내보내기", flush=True)
        return org_files

    last_export = get_last_export_time(source_dir)
    if last_export == 0:
        print(f"[FILTER] 첫 실행: 전체 {len(org_files)}개 내보내기", flush=True)
        return org_files

    from datetime import datetime
    last_dt = datetime.fromtimestamp(last_export)
    print(f"[FILTER] 마지막 내보내기: {last_dt.strftime('%Y-%m-%d %H:%M:%S')}", flush=True)

    changed = []
    skipped = 0
    for f in org_files:
        if f.stat().st_mtime > last_export:
            changed.append(f)
        else:
            skipped += 1

    print(f"[FILTER] 전체: {len(org_files)}, 변경: {len(changed)}, 건너뛰기: {skipped}", flush=True)
    return changed

def get_org_files(directory, mode):
    """Get list of .org files based on mode."""
    if mode == "dblock":
        # Recursive search for dblock mode
        all_files = []
        for org_file in directory.rglob("*.org"):
            # Skip lock files (.#filename)
            if not org_file.name.startswith(".#"):
                all_files.append(org_file)
        return sorted(all_files)
    else:
        # Non-recursive for export mode
        return sorted(directory.glob("*.org"))

def main():
    global _num_daemons, _mode, _no_cleanup, _cleanup_done

    # Register signal handlers in main process
    _register_handlers()

    if len(sys.argv) < 4:
        print("Usage: denote-export-parallel.py <mode> <directory> <num_daemons> [--force] [--no-cleanup]")
        print("  mode: export | dblock")
        print("  directory: target directory")
        print("  num_daemons: number of parallel daemons")
        print("  --force: skip incremental check, export all files")
        print("  --no-cleanup: keep daemons alive after completion (for reuse)")
        sys.exit(1)

    mode = sys.argv[1]
    directory = Path(sys.argv[2])
    num_daemons = int(sys.argv[3])
    extra_args = sys.argv[4:]
    force = "--force" in extra_args
    _no_cleanup = "--no-cleanup" in extra_args

    if mode not in ("export", "dblock"):
        print(f"Error: Invalid mode '{mode}'. Use 'export' or 'dblock'")
        sys.exit(1)

    # Set globals
    _num_daemons = num_daemons
    _mode = mode

    if not directory.exists():
        print(f"Error: Directory not found: {directory}")
        sys.exit(1)

    # Get all .org files (Python handles Unicode perfectly)
    all_org_files = get_org_files(directory, mode)
    total_all = len(all_org_files)

    mode_desc = "Hugo Export" if mode == "export" else "Dblock Update"
    search_type = "non-recursive" if mode == "export" else "recursive"

    print(f"[INFO] Mode: {mode_desc}", flush=True)
    print(f"[INFO] Found {total_all} files in {directory} ({search_type})", flush=True)

    # Incremental filtering (export mode only)
    if mode == "export":
        org_files = filter_changed_files(all_org_files, directory, force=force)
    else:
        org_files = all_org_files

    total_files = len(org_files)

    print(f"[INFO] Processing {total_files} files", flush=True)
    print(f"[INFO] Using {num_daemons} parallel daemons", flush=True)
    print(flush=True)

    if total_files == 0:
        print("[INFO] No files to process. Exiting.")
        if mode == "export":
            save_export_time(directory, time.time())
        return 0

    # Record start time BEFORE processing (not end time)
    # Files modified during export will be caught in the next run
    export_start_time = time.time()

    # Start or reuse daemons
    print(f"[INFO] ====================================== Daemon Lifecycle ======================================", flush=True)
    start_daemons(num_daemons)
    print(f"[INFO] ✓ All {num_daemons} daemons ready!", flush=True)
    print(f"[INFO] =============================================================================================", flush=True)
    print(flush=True)

    try:
        # Assign files to daemons (round-robin)
        file_daemon_pairs = []
        for idx, file_path in enumerate(org_files):
            daemon_id = (idx % num_daemons) + 1
            file_daemon_pairs.append((file_path, daemon_id, mode))

        # Process in parallel
        global _executor, _interrupted
        start_time = time.time()
        success_count = 0
        error_count = 0
        cancelled_count = 0

        _executor = ProcessPoolExecutor(max_workers=num_daemons)
        try:
            futures = {_executor.submit(process_file_via_daemon, pair): pair
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

        # Print profiling summary
        print_profile_summary(num_daemons, duration, success_count, error_count, total_files)

        # Save start timestamp on success (export mode only)
        # Using start time ensures files modified during export are caught next run
        if mode == "export" and error_count == 0 and not _interrupted:
            save_export_time(directory, export_start_time)
            print(f"[INFO] ✓ 타임스탬프 저장 완료 (시작 시각 기준)", flush=True)

        return 0 if error_count == 0 and not _interrupted else 1

    finally:
        if _no_cleanup:
            print(f"\n[INFO] --no-cleanup: daemons kept alive for reuse", flush=True)
        else:
            # ALWAYS cleanup daemons (even on error/interrupt)
            print()
            print(f"[INFO] ====================================== Daemon Cleanup ======================================", flush=True)
            stop_daemons(num_daemons)
            _cleanup_done = True  # Prevent atexit double cleanup
            print(f"[INFO] ✓ All {num_daemons} daemons stopped and cleaned up!", flush=True)
            print(f"[INFO] =============================================================================================", flush=True)

if __name__ == '__main__':
    sys.exit(main())
