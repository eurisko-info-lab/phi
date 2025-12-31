# ═══════════════════════════════════════════════════════════════════════════
# Φ (Phi) - Algebraic Metaprogramming Framework
# ═══════════════════════════════════════════════════════════════════════════

.PHONY: all build clean test help scala rust rvm phi

# Default target
all: build

# ─────────────────────────────────────────────────────────────────────────────
# Build targets
# ─────────────────────────────────────────────────────────────────────────────

build: scala rust

# Scala phi interpreter
scala phi:
	cd ports/scala/phi && sbt compile

# Rust RVM implementation
rust rvm:
	cd ports/rust/rvm && cargo build --release

# ─────────────────────────────────────────────────────────────────────────────
# Test targets
# ─────────────────────────────────────────────────────────────────────────────

test: test-scala test-rust

test-scala:
	cd ports/scala/phi && sbt test

test-rust:
	cd ports/rust/rvm && cargo test

# ─────────────────────────────────────────────────────────────────────────────
# Run targets
# ─────────────────────────────────────────────────────────────────────────────

run-phi:
	cd ports/scala/phi && sbt run

run-rvm:
	cd ports/rust/rvm && cargo run --release

# ─────────────────────────────────────────────────────────────────────────────
# Clean targets
# ─────────────────────────────────────────────────────────────────────────────

clean: clean-scala clean-rust

clean-scala:
	cd ports/scala/phi && sbt clean
	rm -rf ports/scala/phi/target
	rm -rf ports/scala/phi/project/target

clean-rust:
	cd ports/rust/rvm && cargo clean

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
	@echo "Build targets:"
	@echo "  all          Build all ports (default)"
	@echo "  build        Build all ports"
	@echo "  scala/phi    Build Scala phi interpreter"
	@echo "  rust/rvm     Build Rust RVM"
	@echo ""
	@echo "Test targets:"
	@echo "  test         Run all tests"
	@echo "  test-scala   Run Scala tests"
	@echo "  test-rust    Run Rust tests"
	@echo ""
	@echo "Run targets:"
	@echo "  run-phi      Run Scala phi interpreter"
	@echo "  run-rvm      Run Rust RVM"
	@echo ""
	@echo "Clean targets:"
	@echo "  clean        Clean all build artifacts"
	@echo "  clean-scala  Clean Scala build artifacts"
	@echo "  clean-rust   Clean Rust build artifacts"
	@echo ""
	@echo "Directory structure:"
	@echo "  specs/       Phi language specifications"
	@echo "  specs/xforms Transformations (x2y.phi)"
	@echo "  examples/    Example phi programs"
	@echo "  docs/        Documentation"
	@echo "  ports/       Language implementations"
	@echo "    scala/phi  Scala phi interpreter"
	@echo "    rust/rvm   Rust RosettaVM"
