# Repository Guidelines

## Project Structure & Module Organization
- Workspace root holds `Cargo.toml` with shared metadata and dependencies; crates live under `core/*`.
- `core/json` is the current crate (`fiber_json`), with source in `core/json/src` and tests colocated in the same modules.
- Build artifacts land in `target/`; keep it out of commits. Add new crates under `core/<name>` and register them in the workspace `Cargo.toml`.

## Build, Test, and Development Commands
- `cargo fmt` — format all workspace crates.
- `cargo clippy --all-targets --all-features` — lint with warnings treated as guidance; keep output clean before pushing.
- `cargo test --workspace` — run all tests across crates; use `-- --nocapture` when debugging.
- `cargo check --workspace` — quick type/lint feedback during inner-loop edits.
- `cargo doc --workspace --open` — generate API docs locally to validate public surface clarity.

## Coding Style & Naming Conventions
- Rust 2024 edition; prefer idiomatic Rust patterns and standard library traits.
- Follow `rustfmt` defaults (4-space indent, max width per formatter); do not hand-tune formatting.
- Use `snake_case` for functions/modules, `CamelCase` for types/traits, and `SCREAMING_SNAKE_CASE` for consts/statics.
- Keep modules small and focused; expose minimal public APIs. Add module-level docs when behavior is non-obvious.

## Testing Guidelines
- Use built-in `cargo test`; keep unit tests near implementations and name them after behavior (`adds_two_numbers`, `rejects_invalid_payload`).
- Prefer fast, deterministic tests; mock external effects rather than hitting networks or disks.
- Add regression tests alongside bug fixes and include minimal repro inputs in assertions.
- Aim for meaningful coverage on public APIs; avoid untested `unsafe` or complex logic.

## Commit & Pull Request Guidelines
- No history yet; adopt Conventional Commits (`feat:`, `fix:`, `chore:`, etc.) for clarity and changelog friendliness.
- Keep commits focused and buildable; include rationale in the body when behavior changes or edge cases are covered.
- PRs should describe intent, scope, and testing performed; link issues when applicable and note any follow-up TODOs.
- Include screenshots or sample output only when the change affects behavior that benefits from visual/IO confirmation.

## Security & Configuration Tips
- Do not commit secrets or personal tokens; use environment variables for local testing.
- Keep dependencies minimal; prefer workspace crates before adding new external crates. Run `cargo deny` or `cargo audit` if introduced.
