.PHONY: \
	fmt \
	clippy

fmt:
	cargo +nightly fmt --all --check

clippy:
	cargo clippy --all-features --all-targets -- -D warnings
