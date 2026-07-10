# Documentation Index

## Overview
Documentation for the `table_one()` refactor: the manual `numeric_desp()`/
`logical_desp()`/`factor_desp()` implementation was replaced with a
`gtsummary`-based rewrite.

**Status:** The refactor is merged to `master` (commit `1eaae1c`, 2026-07-09).
This is no longer a forward-looking migration plan — the documents below
describe (and, where fixed during this audit, verify) the table_one() that
exists today. For the authoritative, diff-verified list of what changed
versus the pre-refactor code, see **REPRODUCING_LEGACY_RESULTS.md** at the
repo root. For repo history and open work, see **HANDOFF.md**.

---

## Quick Navigation

### For Users (want to use `table_one()`)
1. Start with: **QUICKREF.md** (5 min read)
2. See examples: **EXAMPLES.md**
3. Reference: **API_REFERENCE.md** (as needed)

### For Developers (working on the codebase)
1. Start with: **REFACTORING_SUMMARY.md** (executive overview)
2. What actually changed vs. the pre-refactor code: **REPRODUCING_LEGACY_RESULTS.md**
3. See code differences: **CODE_COMPARISON.md**

### For Decision Makers
1. Read: **BEFORE_AFTER_EXAMPLE.md**
2. Review: **REFACTORING_SUMMARY.md**

---

## File Guide

### Core Documentation

#### **REFACTORING_SUMMARY.md**
- **Purpose:** Executive overview of the refactoring — what changed, why, and
  what it cost/gained (verified line counts, not benchmark numbers).
- **Audience:** All stakeholders.

#### **QUICKREF.md**
- **Purpose:** Quick reference for common tasks — usage patterns, common
  customizations, troubleshooting.
- **Audience:** All users.

---

### Learning & Understanding

#### **EXAMPLES.md**
- **Purpose:** Learn by example — basic tables, grouped comparisons, data
  dictionaries, custom formatting, export, missing-data handling, stratified
  analysis. All code blocks in this file have been run against the current
  `table_one()` as part of this audit; a couple of examples that referenced
  functions no longer in gtsummary were fixed.
- **Audience:** End users.

#### **CODE_COMPARISON.md**
- **Purpose:** Side-by-side old-code/new-code snippets illustrating what the
  refactor replaced. Line counts for the removed files are cited from
  `git show 7ab169b:R/<file> | wc -l`, not estimated.
- **Audience:** Developers.

#### **BEFORE_AFTER_EXAMPLE.md**
- **Purpose:** A single worked example (old workflow vs. new workflow) plus
  export options and a few advanced use cases.
- **Audience:** All users.

---

### Reference Material

#### **API_REFERENCE.md**
- **Purpose:** Full parameter documentation for `table_one()`, matched
  against the actual function signature in `R/desp_table_gtsummary.R`.
- **Audience:** Developers.

---

### Implementation Files

#### **R/desp_table_gtsummary.R**
- `table_one()`, `format_pvalue()`, and the helpers `mean_sd()`, `med_iqr()`,
  `n_avail()` (all exported — see `NAMESPACE`).
- Current size: 397 lines (`wc -l R/desp_table_gtsummary.R`).

---

## Verified Facts (from this audit, 2026-07-09)

- `table_one()`'s current signature, defaults, and behavior are documented
  accurately in **API_REFERENCE.md** as of this audit — it was previously out
  of sync with the actual code (wrong defaults for `missing`/`missing_text`,
  missing parameters, an incorrect claim that the `group` argument had to be
  named).
- The pre-refactor files this replaced totaled 825 lines across 4 files
  (`desp_table.R` 127, `numeric_desp.R` 271, `logical_desp.R` 150,
  `factor_desp.R` 277 — all at commit `7ab169b`, all now removed from the
  repo). The replacement, `R/desp_table_gtsummary.R`, is 397 lines.
- No performance/memory benchmark exists anywhere in this repo for
  `table_one()` (only `construct_surv_var()`/`construct_cmprisk_var()` are
  benchmarked, in `dev-tests/test_construct_equiv.R`). Earlier drafts of
  these documents contained specific ms/MB/speedup numbers for `table_one()`
  that could not be reproduced or sourced from anything in the repo; they
  have been removed rather than left in place.
- `gtsummary` 2.1.0 (the version this package uses) has no `as_latex()` and
  no `all_logical()` selector, and no `modify_rows_print()` /
  `modify_table_styling(..., bold = TRUE)` — earlier drafts referenced all
  four; fixed or removed where found.

---

## Common Questions

### Q: Where do I start?
**A:** Read QUICKREF.md, then check EXAMPLES.md if needed.

### Q: What changed from the pre-refactor version?
**A:** See REPRODUCING_LEGACY_RESULTS.md — it's the diff-verified source of
truth for breaking changes (positional `group` still works; the return type
changed from a dataframe to a `tbl_summary`/`gtsummary` object; some
low-level functions were removed; two edge-case bugs in
`construct_surv_var()`/`construct_cmprisk_var()` were fixed).

### Q: What if my analysis breaks?
**A:** Most likely cause is the new return type. Use
`gtsummary::as_tibble()` to get a dataframe back. See
REPRODUCING_LEGACY_RESULTS.md for the full list of behavior changes,
including two that can silently change *numeric results*, not just types.

---

## Document Maintenance

**Last updated:** 2026-07-09, during an audit that checked every
quantitative/factual claim in this file set against the current repo state
(function signatures, exported names, `git show`-verified historical line
counts, and actually running the code examples in R). Claims that didn't hold
up were fixed or removed; `MIGRATION_GUIDE.md` was deleted as redundant with
REPRODUCING_LEGACY_RESULTS.md (see that deletion's rationale in the commit
that removed it).
