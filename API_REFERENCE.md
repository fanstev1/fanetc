# API Reference: gtsummary-based table_one()

## Function Signature

Verified against `R/desp_table_gtsummary.R` on `master` (2026-07-09):

```r
table_one(
  df,
  group,
  datadic = NULL,
  var_name,
  var_desp,
  include,
  missing = "ifany",
  missing_text = "(Missing)",
  missing_group_exclude = TRUE,
  add_p = NULL,
  add_overall = NULL,
  sort_by_p = FALSE,
  continuous_stat = c("meansd", "mediqr"),
  pvalue_fun = format_pvalue
)
```

`group` and `include` are NSE (bare column name) arguments with no default —
omit them for "no grouping" / "include everything" (passing `group = NULL`
also works). `var_name`/`var_desp` let `datadic` use differently-named
columns (default to `var_name`/`var_desp`, matching the pre-refactor API).
`missing_group_exclude = TRUE` (default) drops rows with missing `group`; set
`FALSE` to keep them as their own `missing_text`-labeled level instead.
`continuous_stat` chooses `"meansd"` (mean/SD, t-test or ANOVA — the default)
or `"mediqr"` (median/IQR, Wilcoxon or Kruskal-Wallis) for the whole table.

## Parameters

### `df`
- **Type:** dataframe
- **Description:** Input dataframe containing numeric, logical, and factor variables
- **Requirements:**
  - Should NOT contain character or Date variables (will be automatically removed)
  - All variables except group variable will be included in summary
- **Example:**
  ```r
  df <- data.frame(
    age = rnorm(100, 50, 15),
    gender = factor(c(rep("M", 50), rep("F", 50))),
    disease = sample(c(TRUE, FALSE), 100, replace = TRUE)
  )
  ```

---

### `group`
- **Type:** symbol (unquoted column name)
- **Default:** `NULL` (no grouping)
- **Description:** Optional grouping/stratification variable
- **Requirements:**
  - Must be a factor or logical variable
  - Must be quoted as a bare name (e.g., `group = sex`, not `group = "sex"`)
  - Missing values are automatically removed from analysis
- **Behavior:**
  - When specified, creates separate columns for each group level
  - Automatically adds p-values if `add_p = TRUE`
  - Automatically adds overall column if `add_overall = TRUE`
- **Examples:**
  ```r
  # With grouping
  table_one(df, group = gender)
  
  # Without grouping (NULL)
  table_one(df)
  ```

---

### `include`
- **Type:** character vector
- **Default:** `NULL` (include all variables except group)
- **Description:** Specific variables to include in summary
- **Requirements:**
  - Vector of variable names as strings
  - Useful for selecting subset of variables
- **Examples:**
  ```r
  # Include only specific variables
  table_one(df, include = c("age", "weight", "height"))
  
  # Include all (default)
  table_one(df)  # equivalent to include = NULL
  ```

---

### `datadic`
- **Type:** dataframe with two columns
- **Default:** `NULL` (use original variable names as labels)
- **Description:** Data dictionary providing human-readable labels for variables
- **Requirements:**
  - Must have columns: `var_name` and `var_desp` (or custom names via ..._name and ..._desp)
  - `var_name`: Original column names from df
  - `var_desp`: Display labels for output table
  - All df variables don't need to be in datadic (unmapped variables use original names)
- **Example:**
  ```r
  datadic <- data.frame(
    var_name = c("age", "wt", "ht"),
    var_desp = c("Age (years)", "Weight (kg)", "Height (cm)")
  )
  table_one(df, datadic = datadic)
  ```

---

### `missing`
- **Type:** character
- **Default:** `"ifany"`
- **Options:**
  - `"no"` - Never show missing count
  - `"ifany"` - Show missing count only if any missing values present (default)
  - `"always"` - Always show missing count
- **Description:** Controls display of missing data in the table
- **Examples:**
  ```r
  table_one(df, missing = "ifany")   # (default) Show if any present
  table_one(df, missing = "no")      # Hide missing
  table_one(df, missing = "always")  # Always show
  ```

---

### `missing_text`
- **Type:** character
- **Default:** `"(Missing)"`
- **Description:** Label for missing value rows
- **Example:**
  ```r
  table_one(df, missing = "ifany", missing_text = "Missing")
  ```

---

### `add_p`
- **Type:** logical
- **Default:** `NULL` (auto-determined)
  - `TRUE` if `group` is specified
  - `FALSE` if no `group`
- **Description:** Whether to add p-values for group comparisons
- **Requirements:**
  - Only meaningful when `group` is specified
  - Automatically selects appropriate tests:
    - **Continuous:** t-test (2 groups) or one-way ANOVA (>2 groups)
    - **Categorical:** Fisher's exact test
- **Examples:**
  ```r
  # Automatic (adds p if grouping)
  table_one(df, group = sex)     # add_p = TRUE by default
  
  # Explicit control
  table_one(df, group = sex, add_p = TRUE)   # Include p-values
  table_one(df, group = sex, add_p = FALSE)  # No p-values
  ```

---

### `add_overall`
- **Type:** logical
- **Default:** `NULL` (auto-determined)
  - `TRUE` if `group` is specified
  - `FALSE` if no `group`
- **Description:** Whether to add overall/total column
- **Behavior:**
  - First column shows statistics for all data combined
  - Other columns show group-specific statistics
  - Useful for seeing both overall and group-stratified summaries
- **Examples:**
  ```r
  # With grouping - shows Overall + Group columns
  table_one(df, group = sex, add_overall = TRUE)
  
  # Without overall column
  table_one(df, group = sex, add_overall = FALSE)
  ```

---

### `sort_by_p`
- **Type:** logical
- **Default:** `FALSE`
- **Description:** Sort variables by p-value (ascending)
- **Requirements:**
  - Only used when `add_p = TRUE`
  - Variables with smallest p-values appear first
  - Useful for identifying most significant differences
- **Examples:**
  ```r
  # Show most significant differences first
  table_one(df, group = sex, add_p = TRUE, sort_by_p = TRUE)
  ```

---

### `pvalue_fun`
- **Type:** function
- **Default:** `format_pvalue` (custom formatting function)
- **Description:** Function to format p-values in output
- **Requirements:**
  - Must accept numeric vector and return character vector
  - Must handle NA values
- **Default behavior:**
  - p ≥ 0.1995: formatted to 2 decimals
  - p < 0.1995: formatted to 3 decimals
  - p < 0.001: displayed as "<0.001"
- **Examples:**
  ```r
  # Use default formatting (Annals of Medicine style)
  table_one(df, group = sex, add_p = TRUE)
  
  # Use standard R formatting
  table_one(df, group = sex, add_p = TRUE, 
            pvalue_fun = function(x) format.pval(x, digits = 3))
  
  # Custom formatting function
  my_pval_format <- function(x) {
    ifelse(x < 0.001, "<0.001",
           ifelse(x < 0.05, sprintf("%.03f", x),
                  sprintf("%.02f", x)))
  }
  table_one(df, group = sex, add_p = TRUE, pvalue_fun = my_pval_format)
  ```

---

## Return Value

### Type
An object with class `c("tbl_summary", "gtsummary")` (verified via `class()`)

### Properties
- Can be printed directly with `print()`
- Can be converted to other formats
- Can be further customized with gtsummary functions
- Can be exported to multiple formats

### Example
```r
tbl <- table_one(df, group = sex)

# Different output methods
print(tbl)                          # Console
gtsummary::as_kable(tbl)            # Markdown
gtsummary::as_gt(tbl)               # HTML
gtsummary::as_flex_table(tbl)       # Word
gtsummary::as_tibble(tbl)           # Dataframe
```

---

## Statistics Calculated

### Continuous Variables
- **One statistic line per variable, chosen by `continuous_stat`:**
  - `continuous_stat = "meansd"` (default): `mean ± SD`
  - `continuous_stat = "mediqr"`: `median (Q1 – Q3)`
  - Verified: `table_one()` does **not** show both mean/SD and median/IQR
    rows for the same variable at once — earlier drafts of this doc (and of
    EXAMPLES.md/BEFORE_AFTER_EXAMPLE.md) showed both, which does not match
    the current implementation.
- **Decimal places:** Auto-determined per variable via `decimalplaces()`

### Categorical Variables (including logical/binary)
- **Statistic:** `n (%)` where n is count and % is percentage
- **Decimal places:** fixed regardless of sample size — n uses 0 decimals,
  % uses 1 decimal (verified: a 300-row and a 50-row table both render
  percentages like `150 (50.0%)` / `24 (48.0%)`)

### Statistical Tests (when `add_p = TRUE`)
- **Continuous:** depends on `continuous_stat`:
  - `"meansd"`: Student's t-test, unequal variance (2 groups) or one-way
    ANOVA (>2 groups)
  - `"mediqr"`: Wilcoxon rank-sum test (2 groups) or Kruskal-Wallis (>2 groups)
- **Categorical:** Fisher's exact test, always called with
  `hybrid = TRUE, simulate.p.value = TRUE`

---

## Common Operations After table_one()

### Modify Headers
```r
tbl %>%
  gtsummary::modify_header(
    stat_1 ~ "**Female**",
    stat_2 ~ "**Male**",
    p.value ~ "**P**"
  )
```

### Bold Labels and P-values
```r
tbl %>%
  gtsummary::bold_labels() %>%
  gtsummary::bold_p(t = 0.05)  # Bold p-values < 0.05
```

### Add Footnote
```r
tbl %>%
  gtsummary::modify_footnote(
    all_stat_cols() ~ "Mean ± SD (continuous), n (%) (categorical)"
  )
```

### Export Formats
```r
# HTML
gtsummary::as_gt(tbl) %>% gt::gtsave("table.html")

# Word  
gtsummary::as_flex_table(tbl)  # For officer package

# Markdown
gtsummary::as_kable(tbl)

# Dataframe
gtsummary::as_tibble(tbl)
```

(Verified against gtsummary 2.1.0's exports: `as_flex_table`, `as_gt`,
`as_gtsummary`, `as_hux_table`, `as_hux_xlsx`, `as_kable`, `as_kable_extra`,
`as_tibble`. There is no `as_latex()` — for LaTeX output, go through
`as_kable_extra()` or `as_gt()` and consult the current gtsummary/gt/kableExtra
docs.)

### Extract Inline Statistics
```r
gtsummary::inline_text(tbl, variable = age, column = stat_1)
# Returns: "50 ± 15"
```

---

## Error Handling

### Invalid variable types
- **Character variables:** Automatically removed
- **Date/DateTime variables:** Automatically removed
- **Unknown types:** Converted or skipped

### Missing group values
- By default (`missing_group_exclude = TRUE`), rows with a missing `group`
  value are dropped, no error thrown
- Set `missing_group_exclude = FALSE` to keep them instead, as their own
  `missing_text`-labeled group level

### Non-matching data dictionary
- Variables in df not in datadic: Use original names
- Variables in datadic not in df: Safely ignored

### Statistical test failures
- Failed tests return NA p-value
- Does not stop execution
- Table is still produced with missing test results

---

## Backward Compatibility Notes

No benchmark suite exists for `table_one()` in this repo (`dev-tests/` only
benchmarks `construct_surv_var()`/`construct_cmprisk_var()`), so this document
no longer states specific timing/memory numbers for it — none could be
verified.

### From the Pre-Refactor Implementation
- **Group parameter:** `group` is still accepted positionally —
  `table_one(df, sex)` works exactly as before, verified against the current
  code and by `dev-tests/test_backward_compat.R` / `test_api_compat.R`. Using
  `group = sex` is recommended for clarity but is not required.
- **BREAKING: return type.** `table_one()` now returns a `tbl_summary`/
  `gtsummary` object instead of a plain dataframe. Use
  `gtsummary::as_tibble()` to get a dataframe back.
- **COMPATIBLE:** `datadic` parameter works the same.

See `REPRODUCING_LEGACY_RESULTS.md` at the repo root for the full, diff-verified
list of behavior changes versus the pre-refactor `fanetc_legacy` branch.

### Workaround for the Return-Type Change
```r
# Convert object to dataframe
tbl <- table_one(df, group = sex)
df_result <- gtsummary::as_tibble(tbl)
```

---

## Helper Functions

### format_pvalue()
Consistent p-value formatting across the package

### mean_sd()
Format mean and standard deviation

### med_iqr()
Format median and interquartile range

### n_avail()
Count non-missing observations

All these are exported and can be used independently.

---

## Related Functions

From the `gtsummary` package:
- `tbl_summary()` - Core function used internally
- `add_p()` - Adds statistical tests
- `add_overall()` - Adds overall column
- `modify_header()` - Customize column headers
- `bold_labels()` - Bold variable names
- `bold_p()` - Bold significant p-values
- `as_gt()`, `as_kable()`, `as_flex_table()` - Format conversion
- `as_tibble()` - Convert to dataframe

See gtsummary documentation at https://www.danieldsjoberg.com/gtsummary/)
