#!/bin/zsh

# Fail on simple command failure
set -e

handle_error() {
    echo "An error occurred, halting build_all.zsh"
    exit 1
}

trap handle_error ERR

cargo fmt 
cargo build
cargo clippy -- -D clippy::pedantic
cargo test --no-fail-fast
cargo doc --document-private-items
