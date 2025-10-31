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
from pathlib import Path
from concurrent.futures import ProcessPoolExecutor, as_completed

# Configuration
HOME = Path.home()
SERVER_SCRIPT = HOME / "repos/gh/doomemacs-config/bin/denote-export-server.el"

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
    print(f"[INFO] Starting {num_daemons} daemons...")

    for i in range(1, num_daemons + 1):
        daemon_name = f"denote-export-daemon-{i}"

        # Stop if already running
        if is_daemon_running(daemon_name):
            print(f"[INFO] Daemon {i} already running, stopping...")
            subprocess.run(
                ['emacsclient', '-s', daemon_name, '--eval', '(kill-emacs)'],
                capture_output=True
            )
            time.sleep(1)

        # Start daemon
        print(f"[INFO] Starting daemon {i}: {daemon_name}")
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
                    print(f"[INFO] ✓ Daemon {i} ready!")
                    break
            except:
                pass

            time.sleep(1)
            elapsed += 1

        if elapsed >= timeout:
            print(f"[ERROR] Daemon {i} initialization timeout")
            sys.exit(1)

    print(f"[INFO] All {num_daemons} daemons ready!\n")

def stop_daemons(num_daemons):
    """Stop all daemons."""
    print("\n[INFO] Stopping daemons...")
    for i in range(1, num_daemons + 1):
        daemon_name = f"denote-export-daemon-{i}"
        try:
            subprocess.run(
                ['emacsclient', '-s', daemon_name, '--eval', '(kill-emacs)'],
                capture_output=True,
                timeout=5
            )
            print(f"[INFO] ✓ Daemon {i} stopped")
        except:
            pass

def export_file_via_daemon(args):
    """Export a single file via emacsclient to a specific daemon."""
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
            timeout=180
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
    if len(sys.argv) < 3:
        print("Usage: denote-export-parallel.py <directory> <num_daemons>")
        sys.exit(1)

    directory = Path(sys.argv[1])
    num_daemons = int(sys.argv[2])

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
    start_daemons(num_daemons)

    # Assign files to daemons (round-robin)
    file_daemon_pairs = []
    for idx, file_path in enumerate(org_files):
        daemon_id = (idx % num_daemons) + 1
        file_daemon_pairs.append((file_path, daemon_id))

    # Process in parallel
    start_time = time.time()
    success_count = 0
    error_count = 0

    with ProcessPoolExecutor(max_workers=num_daemons) as executor:
        futures = {executor.submit(export_file_via_daemon, pair): pair
                  for pair in file_daemon_pairs}

        for future in as_completed(futures):
            success, basename = future.result()
            if success:
                success_count += 1
            else:
                error_count += 1

    duration = time.time() - start_time
    speed = total_files / duration if duration > 0 else 0

    print()
    print(f"[INFO] ========================================")
    print(f"[INFO] Export completed!")
    print(f"[INFO] Success: {success_count}, Errors: {error_count}, Total: {total_files}")
    print(f"[INFO] Duration: {duration:.1f}s, Speed: {speed:.2f} files/sec")
    print(f"[INFO] ========================================")

    # Cleanup daemons
    stop_daemons(num_daemons)

    return 0 if error_count == 0 else 1

if __name__ == '__main__':
    sys.exit(main())
