# ═══════════════════════════════════════════════════════════════════════════
# Φ (Phi) - Algebraic Metaprogramming Framework
# ═══════════════════════════════════════════════════════════════════════════

.PHONY: all build clean test help scala rust rvm phi haskell install install-rvm full c cuda

# Enable parallel execution
MAKEFLAGS += -j3

# Default target
all: build test

# Full pipeline: clean, build, test, install
full: clean build test install

# ─────────────────────────────────────────────────────────────────────────────
# Build targets (run in parallel with make -j)
# ─────────────────────────────────────────────────────────────────────────────

build: scala rust haskell c

# Scala phi interpreter
scala phi:
	cd ports/scala/tools/phi && sbt compile

# Rust RVM implementation
rust rvm:
	cd ports/rust/tools/rvm && RUSTFLAGS="-D warnings" cargo build --release

# Haskell phi interpreter
haskell:
	cd ports/haskell/tools/phi && cabal build

# C HVM examples
c:
	@mkdir -p build
	gcc -O3 -Wall -Wextra -o build/sum examples/c/sum.c

# CUDA HVM examples (optional, requires nvcc)
cuda:
	@mkdir -p build
	@if command -v nvcc >/dev/null 2>&1; then \
		echo "Building CUDA targets..."; \
		nvcc -O3 -o build/sum_gpu examples/cuda/sum.cu; \
	else \
		echo "nvcc not found, skipping CUDA build"; \
	fi

# ─────────────────────────────────────────────────────────────────────────────
# Install targets
# ─────────────────────────────────────────────────────────────────────────────

install: install-rvm

install-rvm: rust
	@mkdir -p ~/bin
	cp ports/rust/tools/rvm/target/release/rosettavm ~/bin/rvm
	@echo "Installed rvm to ~/bin/rvm"
	@echo "Make sure ~/bin is in your PATH"

# ─────────────────────────────────────────────────────────────────────────────
# Test targets (run in parallel with make -j)
# ─────────────────────────────────────────────────────────────────────────────

test: test-scala test-rust test-haskell

test-scala: scala
	cd ports/scala/tools/phi && sbt test

test-rust: rust
	cd ports/rust/tools/rvm && RUSTFLAGS="-D warnings" cargo test

test-haskell: haskell
	cd ports/haskell/tools/phi && cabal test

# ─────────────────────────────────────────────────────────────────────────────
# Run targets
# ─────────────────────────────────────────────────────────────────────────────

run-phi:
	cd ports/scala/tools/phi && sbt run

run-rvm:
	cd ports/rust/tools/rvm && cargo run --release

run-sum:
	./build/sum 1000000

run-sum-gpu:
	@if [ -f build/sum_gpu ]; then \
		./build/sum_gpu 1000000; \
	else \
		echo "CUDA binary not found. Run 'make cuda' first."; \
	fi

# ─────────────────────────────────────────────────────────────────────────────
# Clean targets
# ─────────────────────────────────────────────────────────────────────────────

clean: clean-scala clean-rust clean-haskell clean-c

clean-scala:
	cd ports/scala/tools/phi && sbt clean
	rm -rf ports/scala/tools/phi/target
	rm -rf ports/scala/tools/phi/project/target

clean-rust:
	cd ports/rust/tools/rvm && cargo clean

clean-haskell:
	cd ports/haskell/tools/phi && cabal clean

clean-c:
	rm -rf build

# ─────────────────────────────────────────────────────────────────────────────
# Documentation
# ─────────────────────────────────────────────────────────────────────────────

docs:
	@echo "Documentation in docs/"
	@ls -la docs/

# ─────────────────────────────────────────────────────────────────────────────
# Help
# ─────────────────────────────────────────────────────────────────────────────

help:
	@echo "Φ (Phi) Build System"
	@echo ""
	@echo "Usage: make [target]"
	@echo ""
	@echo "Main targets:"
	@echo "  all          Build and test all ports (default)"
	@echo "  full         Clean, build, test, and install"
	@echo ""
	@echo "Build targets:"
	@echo "  build        Build all ports"
	@echo "  scala/phi    Build Scala phi interpreter"
	@echo "  rust/rvm     Build Rust RVM"
	@echo "  haskell      Build Haskell phi interpreter"
	@echo ""
	@echo "Test targets:"
	@echo "  test         Run all tests"
	@echo "  test-scala   Run Scala tests"
	@echo "  test-rust    Run Rust tests"
	@echo "  test-haskell Run Haskell tests"
	@echo ""
	@echo "Run targets:"
	@echo "  run-phi      Run Scala phi interpreter"
	@echo "  run-rvm      Run Rust RVM"
	@echo ""
	@echo "Install targets:"
	@echo "  install      Install rvm to ~/bin"
	@echo "  install-rvm  Install rvm to ~/bin"
	@echo ""
	@echo "Clean targets:"
	@echo "  clean          Clean all build artifacts"
	@echo "  clean-scala    Clean Scala build artifacts"
	@echo "  clean-rust     Clean Rust build artifacts"
	@echo "  clean-haskell  Clean Haskell build artifacts"
	@echo ""
	@echo "Directory structure:"
	@echo "  specs/       Phi language specifications"
	@echo "  specs/xforms Transformations (x2y.phi)"
	@echo "  examples/    Example phi programs"
	@echo "  docs/        Documentation"
	@echo "  ports/       Language implementations"
	@echo "    scala/phi  Scala phi interpreter"
	@echo "    rust/rvm   Rust RosettaVM"
