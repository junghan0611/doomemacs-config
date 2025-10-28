#!/usr/bin/make -f
# denote-export-multi.mk - Multiple Emacs daemons for true parallel export
#
# Copyright (C) 2025 Junghan Kim
# Author: Junghan Kim <junghanacs@gmail.com>
#
# Usage:
#   make -f denote-export-multi.mk test     # Test with test folder
#   make -f denote-export-multi.mk all      # Export all folders
#   make -f denote-export-multi.mk clean    # Stop all daemons

SHELL := /run/current-system/sw/bin/bash
.SHELLFLAGS := -euo pipefail -c

# Configuration
ORG_DIR := $(HOME)/org
HUGO_DIR := $(HOME)/repos/gh/notes
DOOM_DIR := $(HOME)/repos/gh/doomemacs-config
SERVER_SCRIPT := $(DOOM_DIR)/bin/denote-export-server.el
DAEMON_NAME := denote-export-daemon

# Parallel configuration
NUM_DAEMONS := 4        # Number of Emacs daemons
JOBS_PER_DAEMON := 2    # Concurrent jobs per daemon
TOTAL_JOBS := $(shell echo $$(($(NUM_DAEMONS) * $(JOBS_PER_DAEMON))))

# Find all org files
META_FILES := $(shell find $(ORG_DIR)/meta -name "*.org" -type f 2>/dev/null)
BIB_FILES := $(shell find $(ORG_DIR)/bib -name "*.org" -type f 2>/dev/null)
NOTES_FILES := $(shell find $(ORG_DIR)/notes -name "*.org" -type f 2>/dev/null)
TEST_FILES := $(shell find $(ORG_DIR)/test -name "*.org" -type f 2>/dev/null)

ALL_FILES := $(META_FILES) $(BIB_FILES) $(NOTES_FILES) $(TEST_FILES)

# Colors
GREEN := \033[0;32m
YELLOW := \033[1;33m
RED := \033[0;31m
NC := \033[0m

.PHONY: all test clean daemon-start daemon-stop status help
.PHONY: export-meta export-bib export-notes export-test

# Help
help:
	@echo "Denote Multi-Daemon Export System"
	@echo ""
	@echo "Configuration:"
	@echo "  Daemons: $(NUM_DAEMONS)"
	@echo "  Jobs per daemon: $(JOBS_PER_DAEMON)"
	@echo "  Total parallel jobs: $(TOTAL_JOBS)"
	@echo ""
	@echo "Targets:"
	@echo "  make -f denote-export-multi.mk test   - Export test folder"
	@echo "  make -f denote-export-multi.mk all    - Export all folders"
	@echo "  make -f denote-export-multi.mk clean  - Stop all daemons"
	@echo "  make -f denote-export-multi.mk status - Check daemon status"

# Start multiple daemons
daemon-start:
	@echo "$(GREEN)[INFO]$(NC) Starting $(NUM_DAEMONS) Emacs daemons..."
	@for i in $$(seq 1 $(NUM_DAEMONS)); do \
		DAEMON="$(DAEMON_NAME)-$$i"; \
		if emacsclient -s "$$DAEMON" --eval 't' &>/dev/null 2>&1; then \
			echo "$(GREEN)[INFO]$(NC) Daemon $$i already running"; \
			continue; \
		fi; \
		echo "$(YELLOW)[INFO]$(NC) Starting daemon $$i..."; \
		emacs --quick --daemon="$$DAEMON" --load $(SERVER_SCRIPT) &>/dev/null & \
	done
	@echo "$(YELLOW)[INFO]$(NC) Waiting for all daemons to initialize..."
	@for i in $$(seq 1 $(NUM_DAEMONS)); do \
		DAEMON="$(DAEMON_NAME)-$$i"; \
		ELAPSED=0; \
		TIMEOUT=30; \
		while ! emacsclient -s "$$DAEMON" --eval 't' &>/dev/null 2>&1; do \
			if [ $$ELAPSED -ge $$TIMEOUT ]; then \
				echo "$(RED)[ERROR]$(NC) Daemon $$i failed to start"; \
				exit 1; \
			fi; \
			sleep 1; \
			ELAPSED=$$((ELAPSED + 1)); \
		done; \
		ELAPSED=0; \
		TIMEOUT=60; \
		while ! emacsclient -s "$$DAEMON" --eval '(boundp '"'"'denote-export-server-ready)' 2>/dev/null | grep -q 't'; do \
			if [ $$ELAPSED -ge $$TIMEOUT ]; then \
				echo "$(RED)[ERROR]$(NC) Daemon $$i initialization timeout"; \
				exit 1; \
			fi; \
			sleep 1; \
			ELAPSED=$$((ELAPSED + 1)); \
		done; \
		echo "$(GREEN)[INFO]$(NC) ✓ Daemon $$i ready"; \
	done
	@echo "$(GREEN)[INFO]$(NC) All $(NUM_DAEMONS) daemons ready!"

# Stop all daemons
daemon-stop:
	@echo "$(YELLOW)[INFO]$(NC) Stopping all Emacs daemons..."
	@for i in $$(seq 1 $(NUM_DAEMONS)); do \
		DAEMON="$(DAEMON_NAME)-$$i"; \
		if emacsclient -s "$$DAEMON" --eval '(kill-emacs)' &>/dev/null 2>&1; then \
			echo "$(GREEN)[INFO]$(NC) Daemon $$i stopped"; \
		fi; \
	done
	@echo "$(GREEN)[INFO]$(NC) All daemons stopped"

# Check status
status:
	@echo "$(GREEN)[INFO]$(NC) Checking daemon status..."
	@for i in $$(seq 1 $(NUM_DAEMONS)); do \
		DAEMON="$(DAEMON_NAME)-$$i"; \
		if emacsclient -s "$$DAEMON" --eval 't' &>/dev/null 2>&1; then \
			READY=$$(emacsclient -s "$$DAEMON" --eval '(boundp '"'"'denote-export-server-ready)' 2>/dev/null); \
			if echo "$$READY" | grep -q 't'; then \
				echo "$(GREEN)[OK]$(NC) Daemon $$i: running and ready"; \
			else \
				echo "$(YELLOW)[WARN]$(NC) Daemon $$i: starting..."; \
			fi; \
		else \
			echo "$(RED)[ERROR]$(NC) Daemon $$i: not running"; \
		fi; \
	done

# Export test folder
export-test: daemon-start
	@echo "$(GREEN)[INFO]$(NC) Exporting test folder"
	@echo "$(GREEN)[INFO]$(NC) Files: $(words $(TEST_FILES)), Daemons: $(NUM_DAEMONS), Total jobs: $(TOTAL_JOBS)"
	@START=$$(date +%s); \
	i=1; \
	printf '%s\n' $(TEST_FILES) | while read file; do \
		DAEMON_ID=$$((i % $(NUM_DAEMONS) + 1)); \
		echo "$$DAEMON_ID $$file"; \
		i=$$((i + 1)); \
	done | xargs -P $(TOTAL_JOBS) -n 2 sh -c 'DAEMON_ID=$$1; FILE=$$2; RESULT=$$(emacsclient -s $(DAEMON_NAME)-$$DAEMON_ID --eval "(denote-export-file \"$$FILE\")" 2>&1); if echo "$$RESULT" | grep -q "^\"SUCCESS:"; then echo "✓ [D$$DAEMON_ID] $$(basename $$FILE)"; else echo "✗ [D$$DAEMON_ID] $$(basename $$FILE)"; fi' sh; \
	END=$$(date +%s); \
	DURATION=$$((END - START)); \
	echo "$(GREEN)[INFO]$(NC) Completed in $${DURATION}s ($(TOTAL_JOBS) parallel jobs)"

# Export all
test: export-test

# Cleanup
clean: daemon-stop
