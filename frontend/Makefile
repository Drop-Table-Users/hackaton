
.PHONY: help
help:
	@echo "Usage: make <target>"
	@echo
	@echo "Targets:"
	@echo "  help   -- show this help"
	@echo "  shell  -- start a nix development shell"
	@echo "  build  -- build the project"

.PHONY: shell
shell:
	nix develop --no-write-lock-file

.PHONY: build
build:
	pnpm build
