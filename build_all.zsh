#!/bin/zsh

cargo fmt && cargo build && cargo clippy -- -D clippy::pedantic && cargo test && cargo doc --document-private-items