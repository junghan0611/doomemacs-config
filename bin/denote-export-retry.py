#!/usr/bin/env python3
"""
Denote Export Retry - Retry failed files with increased timeout

Usage:
  1. Extract failed files from log:
     grep "^✗" /tmp/meta-export.log | \
       sed 's/.*"\(\/home.*\.org\)".*/\1/' > /tmp/failed-files-paths.txt

  2. Run retry:
     python3 bin/denote-export-retry.py /tmp/failed-files-paths.txt
"""

import subprocess
import sys
from pathlib import Path

def main():
    if len(sys.argv) < 2:
        print("Usage: denote-export-retry.py <failed_files_list>")
        print("\nExample:")
        print("  grep '^✗' /tmp/meta-export.log | \\")
        print("    sed 's/.*\"\\(\\/.*.org\\)\".*/\\1/' > /tmp/failed.txt")
        print("  python3 bin/denote-export-retry.py /tmp/failed.txt")
        sys.exit(1)

    failed_list_file = sys.argv[1]

    # Read failed files
    with open(failed_list_file, 'r') as f:
        failed_files = [line.strip() for line in f if line.strip()]

    print(f"[Retry] Found {len(failed_files)} failed files")
    print(f"[Retry] Using daemon: denote-export-daemon-1")
    print(f"[Retry] Timeout: 180 seconds\n")

    success = 0
    errors = 0

    for idx, file_path in enumerate(failed_files, 1):
        file_path_obj = Path(file_path)
        basename = file_path_obj.name

        print(f"[{idx}/{len(failed_files)}] {basename}", flush=True)

        # Escape quotes for Elisp
        file_path_escaped = file_path.replace('"', '\\"')
        elisp_cmd = f'(denote-export-file "{file_path_escaped}")'

        try:
            result = subprocess.run(
                ['emacsclient', '-s', 'denote-export-daemon-1', '--eval', elisp_cmd],
                capture_output=True,
                text=True,
                timeout=180  # 180 seconds timeout
            )

            if result.returncode == 0 and 'SUCCESS:' in result.stdout:
                print(f"  ✓ Success", flush=True)
                success += 1
            else:
                print(f"  ✗ Failed", flush=True)
                if result.stderr:
                    print(f"    Error: {result.stderr[:200]}", flush=True)
                errors += 1
        except subprocess.TimeoutExpired:
            print(f"  ✗ Timeout (180s)", flush=True)
            errors += 1
        except Exception as e:
            print(f"  ✗ Exception: {e}", flush=True)
            errors += 1

    print(f"\n[Retry] Results: {success} success, {errors} errors")
    sys.exit(0 if errors == 0 else 1)

if __name__ == '__main__':
    main()
