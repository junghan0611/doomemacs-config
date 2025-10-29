#!/usr/bin/make -f
# denote-export.mk - Makefile for parallel Denote to Hugo export
#
# Copyright (C) 2025 Junghan Kim
# Author: Junghan Kim <junghanacs@gmail.com>
#
# Usage:
#   make -f denote-export.mk test     # Test with test folder only
#   make -f denote-export.mk all      # Export all folders
#   make -f denote-export.mk clean    # Stop daemon and cleanup

# Support both Ubuntu and NixOS
SHELL := $(shell if [ -f /run/current-system/sw/bin/bash ]; then echo /run/current-system/sw/bin/bash; else echo /usr/bin/bash; fi)
.SHELLFLAGS := -euo pipefail -c

# Configuration
ORG_DIR := $(HOME)/org
HUGO_DIR := $(HOME)/repos/gh/notes
DOOM_DIR := $(HOME)/repos/gh/doomemacs-config
SERVER_SCRIPT := $(DOOM_DIR)/bin/denote-export-server.el
DAEMON_NAME := denote-export-daemon

# Number of parallel jobs per directory
JOBS_PER_DIR := 2

# Directories to export
DIRS := meta bib notes test

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

.PHONY: all test clean daemon status help
.PHONY: export-meta export-bib export-notes export-test

# Default target
help:
	@echo "Denote Export System (Makefile)"
	@echo ""
	@echo "Targets:"
	@echo "  make -f denote-export.mk test       - Export test folder only (47 files)"
	@echo "  make -f denote-export.mk export-meta - Export meta folder (530 files)"
	@echo "  make -f denote-export.mk export-bib  - Export bib folder (649 files)"
	@echo "  make -f denote-export.mk export-notes - Export notes folder (782 files)"
	@echo "  make -f denote-export.mk all        - Export all folders (2008 files)"
	@echo "  make -f denote-export.mk clean      - Stop daemon and cleanup"
	@echo "  make -f denote-export.mk status     - Check daemon status"
	@echo ""
	@echo "Configuration:"
	@echo "  ORG_DIR:  $(ORG_DIR)"
	@echo "  HUGO_DIR: $(HUGO_DIR)"
	@echo "  JOBS:     $(JOBS_PER_DIR) per directory"

# Check if daemon is running
daemon-check:
	@if ! emacsclient -s $(DAEMON_NAME) --eval 't' &>/dev/null; then \
		echo "$(YELLOW)[INFO]$(NC) Starting Emacs daemon..."; \
		emacs --quick --daemon=$(DAEMON_NAME) --load $(SERVER_SCRIPT) &>/dev/null; \
		sleep 5; \
		echo "$(YELLOW)[INFO]$(NC) Waiting for daemon initialization..."; \
		for i in {1..30}; do \
			if emacsclient -s $(DAEMON_NAME) --eval '(boundp '"'"'denote-export-server-ready)' 2>/dev/null | grep -q 't'; then \
				echo "$(GREEN)[INFO]$(NC) Daemon ready after $$i seconds"; \
				break; \
			fi; \
			sleep 1; \
		done; \
	else \
		echo "$(GREEN)[INFO]$(NC) Daemon already running"; \
	fi

# Status check
status:
	@echo "$(GREEN)[INFO]$(NC) Checking daemon status..."
	@if emacsclient -s $(DAEMON_NAME) --eval 't' &>/dev/null; then \
		echo "$(GREEN)[OK]$(NC) Daemon is running"; \
		READY=$$(emacsclient -s $(DAEMON_NAME) --eval '(boundp '"'"'denote-export-server-ready)' 2>/dev/null); \
		if echo "$$READY" | grep -q 't'; then \
			echo "$(GREEN)[OK]$(NC) Daemon is fully initialized"; \
		else \
			echo "$(YELLOW)[WARN]$(NC) Daemon is starting..."; \
		fi; \
	else \
		echo "$(RED)[ERROR]$(NC) Daemon is not running"; \
		exit 1; \
	fi

# Export single file helper script
define export-one-file
	RESULT=$$(emacsclient -s $(DAEMON_NAME) --eval "(denote-export-file \"$(1)\")" 2>&1); \
	if echo "$$RESULT" | grep -q "^\"SUCCESS:"; then \
		echo "$(GREEN)✓$(NC) $$(basename $(1))"; \
	else \
		echo "$(RED)✗$(NC) $$(basename $(1))"; \
	fi
endef

# Export directories
export-test: daemon-check
	@echo "$(GREEN)[INFO]$(NC) Exporting test folder ($(words $(TEST_FILES)) files, $(JOBS_PER_DIR) jobs)..."
	@START=$$(date +%s); \
	printf '%s\n' $(TEST_FILES) | xargs -P $(JOBS_PER_DIR) -I {} sh -c 'RESULT=$$(emacsclient -s $(DAEMON_NAME) --eval "(denote-export-file \"{}\")" 2>&1); if echo "$$RESULT" | grep -q "^\"SUCCESS:"; then echo "✓ $$(basename {})"; else echo "✗ $$(basename {})"; fi'; \
	END=$$(date +%s); \
	DURATION=$$((END - START)); \
	echo "$(GREEN)[INFO]$(NC) Completed in $${DURATION}s"

export-meta: daemon-check
	@echo "$(GREEN)[INFO]$(NC) Exporting meta folder ($(words $(META_FILES)) files, $(JOBS_PER_DIR) jobs)..."
	@START=$$(date +%s); \
	printf '%s\n' $(META_FILES) | xargs -P $(JOBS_PER_DIR) -I {} sh -c 'RESULT=$$(emacsclient -s $(DAEMON_NAME) --eval "(denote-export-file \"{}\")" 2>&1); if echo "$$RESULT" | grep -q "^\"SUCCESS:"; then echo "✓ $$(basename {})"; else echo "✗ $$(basename {})"; fi'; \
	END=$$(date +%s); \
	DURATION=$$((END - START)); \
	echo "$(GREEN)[INFO]$(NC) Completed in $${DURATION}s"

export-bib: daemon-check
	@echo "$(GREEN)[INFO]$(NC) Exporting bib folder ($(words $(BIB_FILES)) files, $(JOBS_PER_DIR) jobs)..."
	@START=$$(date +%s); \
	printf '%s\n' $(BIB_FILES) | xargs -P $(JOBS_PER_DIR) -I {} sh -c 'RESULT=$$(emacsclient -s $(DAEMON_NAME) --eval "(denote-export-file \"{}\")" 2>&1); if echo "$$RESULT" | grep -q "^\"SUCCESS:"; then echo "✓ $$(basename {})"; else echo "✗ $$(basename {})"; fi'; \
	END=$$(date +%s); \
	DURATION=$$((END - START)); \
	echo "$(GREEN)[INFO]$(NC) Completed in $${DURATION}s"

export-notes: daemon-check
	@echo "$(GREEN)[INFO]$(NC) Exporting notes folder ($(words $(NOTES_FILES)) files, $(JOBS_PER_DIR) jobs)..."
	@START=$$(date +%s); \
	printf '%s\n' $(NOTES_FILES) | xargs -P $(JOBS_PER_DIR) -I {} sh -c 'RESULT=$$(emacsclient -s $(DAEMON_NAME) --eval "(denote-export-file \"{}\")" 2>&1); if echo "$$RESULT" | grep -q "^\"SUCCESS:"; then echo "✓ $$(basename {})"; else echo "✗ $$(basename {})"; fi'; \
	END=$$(date +%s); \
	DURATION=$$((END - START)); \
	echo "$(GREEN)[INFO]$(NC) Completed in $${DURATION}s"

# Test target (test folder only)
test: export-test

# Export all directories (sequential by directory, parallel within directory)
all: daemon-check
	@echo "$(GREEN)[INFO]$(NC) ======================================"
	@echo "$(GREEN)[INFO]$(NC) Starting export of all directories"
	@echo "$(GREEN)[INFO]$(NC) Total files: $(words $(ALL_FILES))"
	@echo "$(GREEN)[INFO]$(NC) Jobs per directory: $(JOBS_PER_DIR)"
	@echo "$(GREEN)[INFO]$(NC) ======================================"
	@TOTAL_START=$$(date +%s); \
	$(MAKE) --no-print-directory -f $(firstword $(MAKEFILE_LIST)) export-meta; \
	$(MAKE) --no-print-directory -f $(firstword $(MAKEFILE_LIST)) export-bib; \
	$(MAKE) --no-print-directory -f $(firstword $(MAKEFILE_LIST)) export-notes; \
	TOTAL_END=$$(date +%s); \
	TOTAL_DURATION=$$((TOTAL_END - TOTAL_START)); \
	echo "$(GREEN)[INFO]$(NC) ======================================"; \
	echo "$(GREEN)[INFO]$(NC) All directories completed"; \
	echo "$(GREEN)[INFO]$(NC) Total time: $${TOTAL_DURATION}s"; \
	echo "$(GREEN)[INFO]$(NC) Speed: $$(echo "scale=1; $(words $(ALL_FILES)) / $$TOTAL_DURATION" | bc) files/sec"; \
	echo "$(GREEN)[INFO]$(NC) ======================================"

# Cleanup
clean:
	@echo "$(YELLOW)[INFO]$(NC) Stopping Emacs daemon..."
	@if emacsclient -s $(DAEMON_NAME) --eval '(kill-emacs)' &>/dev/null 2>&1; then \
		echo "$(GREEN)[INFO]$(NC) Daemon stopped"; \
	else \
		echo "$(YELLOW)[WARN]$(NC) Daemon was not running"; \
	fi
