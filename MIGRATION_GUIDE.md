# Migration Guide: From Manual Implementation to gtsummary

## Overview
The original `table_one()` implementation has been refactored to use the `gtsummary` package. This provides significant advantages in terms of code maintainability, feature richness, and statistical rigor.

## Key Changes

### 1. **Simplified Function Structure**

**Before:** 5 separate functions
- `table_one()` - Main orchestrator
- `numeric_desp()` - Numeric variable handling
- `logical_desp()` - Logical variable handling
- `factor_desp()` - Factor variable handling  
- Supporting statistical test functions (`two_sample_test`, `k_sample_test`, `fisher_test`)

**After:** 1 main function with optional helper functions
- `table_one()` - Single entry point using `gtsummary::tbl_summary()`
- Helper functions for custom extraction/formatting

### 2. **Dependencies Update**

**Previous dependencies:**
```
Hmisc, magrittr, tidyverse, rlang, forcats, lubridate, 
reshape2, survival, cmprisk, broom, grid, gridExtra, viridis, cowplot
```

**New dependencies:**
```
gtsummary, tidyverse, rlang, forcats
```

**Add to DESCRIPTION:**
```
gtsummary
```

### 3. **Code Reduction**

- **Numeric summary:** `numeric_desp()` (~120 lines) → built into `tbl_summary()` statistic argument
- **Factor summary:** `factor_desp()` (~200 lines) → handled automatically
- **Logical summary:** `logical_desp()` (~100 lines) → converted to categorical automatically
- **Statistical tests:** Custom test functions → `gtsummary::add_p()` with automatic test selection

**Result:** ~800 lines → ~200 lines (75% reduction)

## Function Usage Comparison

### Basic Usage

**Original:**
```r
table_one(df, datadic = datadic)
```

**New:**
```r
table_one(df, datadic = datadic)
```
Same interface, cleaner implementation.

### With Grouping Variable

**Original:**
```r
table_one(df, sex, datadic = datadic)
```

**New:**
```r
table_one(df, group = sex, datadic = datadic)
```
Note: `group` parameter is now explicit (named argument).

### Advanced Options

**New capability - Control statistics displayed:**
```r
# Mean/SD and median/IQR for continuous variables
# Frequency (n) and percentage for categorical
tbl <- table_one(df, group = sex)

# Convert to other formats
gtsummary::as_kable(tbl)              # Markdown table
gtsummary::as_flex_table(tbl)         # Word-compatible
tbl %>% gtsummary::as_gt()            # HTML table
```

## Automatic Type Detection

`gtsummary` automatically detects and handles:

| Variable Type | Previous | New |
|---|---|---|
| Numeric (continuous) | `numeric_desp()` | `{mean} ± {sd}` and `{median} ({p25} - {p75})` |
| Logical (binary) | `logical_desp()` with binary conversion | Auto-converted to categorical, `{n} ({p}%)` |
| Factor (categorical) | `factor_desp()` | Auto-detected, `{n} ({p}%)` |

## Statistical Testing

### Automatic Test Selection

**Before:** Manual function selection based on:
- Variable type (continuous vs. categorical)
- Number of groups (2 vs. >2)
- Special handling for Fisher's exact (logical)

**After:** `gtsummary::add_p()` automatically selects:
- **Continuous:** t-test (2 groups) or one-way ANOVA (>2 groups)
- **Categorical:** Fisher's exact test (with simulation if necessary)

### Manual Control

```r
tbl <- table_one(df, group = sex, add_p = TRUE)  # Automatic tests
tbl <- table_one(df, group = sex, add_p = FALSE) # No p-values
```

## Data Dictionary Support

The data dictionary functionality is preserved:

```r
datadic <- data.frame(
  var_name = c("sex", "income", "dm"),
  var_desp = c("Sex", "Household income", "Diabetes mellitus")
)

table_one(df, group = sex, datadic = datadic)
```

## Enhanced Features (Available with gtsummary)

Once you have a `tbl_summary` object, you can:

### 1. **Modify Headers**
```r
table_one(df, group = sex) %>%
  gtsummary::modify_header(
    stat_1 ~ "**Female**, n = {N}",
    stat_2 ~ "**Male**, n = {N}"
  )
```

### 2. **Merge/Combine P-values and Overall**
```r
table_one(df, group = sex, add_overall = TRUE) %>%
  gtsummary::add_p()
```

### 3. **Format Specific Columns**
```r
table_one(df, group = sex) %>%
  gtsummary::modify_fmt_fun(
    stat_0 ~ function(x) gsub(",", ".", x)
  )
```

### 4. **Export to Multiple Formats**
```r
tbl <- table_one(df, group = sex)

# To Excel (via flextable)
doc <- officer::read_docx() %>%
  flextable::body_add_flextable(gtsummary::as_flex_table(tbl))
officer::print(doc, "output.docx")

# To HTML
gtsummary::as_gt(tbl) %>% gt::gtsave("output.html")
```

## Migration Steps

### Step 1: Update DESCRIPTION
```r
# Remove old dependencies (if not used elsewhere)
# Add new dependency
usethis::use_package("gtsummary")
```

### Step 2: Replace Function File
Keep the old files as backup and use `desp_table_gtsummary.R` as the new implementation.

### Step 3: Update Function Calls
- Change positional `group` argument to named: `group = var_name`
- Remove dependency on `datadic` parameter structure (or keep as-is)
- Use `gtsummary::` functions for post-processing

### Step 4: Testing
```r
# Test with basic data
df <- data.frame(
  group = factor(c(rep("A", 50), rep("B", 50))),
  cont = rnorm(100),
  cat = factor(sample(c("X", "Y", "Z"), 100, replace = TRUE)),
  binary = sample(c(TRUE, FALSE), 100, replace = TRUE)
)

tbl <- table_one(df, group = group)
print(tbl)
```

## Breaking Changes

1. **Group parameter naming:** Must use `group = var_name` instead of `table_one(df, var_name)`
2. **Return type:** Returns `gtsummary_tbl_summary` object instead of raw dataframe
   - Use `as_tibble()` to convert back to dataframe if needed
3. **Removed functions:** `numeric_desp()`, `logical_desp()`, `factor_desp()` are no longer needed
4. **Statistical test functions:** `two_sample_test()`, `k_sample_test()`, `fisher_test()` are no longer needed

## Handling the Breaking Changes

### If you need to export the table as a dataframe:
```r
tbl <- table_one(df, group = sex)
df_out <- gtsummary::as_tibble(tbl)
```

### If you need custom formatting:
```r
tbl <- table_one(df, group = sex) %>%
  gtsummary::modify_table_styling(
    columns = starts_with("stat_"),
    rows = !is.na(label),
    text_format = "bold"
  )
```

## Benefits Summary

| Aspect | Before | After |
|--------|--------|-------|
| **Code lines** | ~800 | ~200 |
| **Functions** | 8 | 1 main + helpers |
| **Maintenance burden** | High (many edge cases) | Low (leverages gtsummary) |
| **Output formats** | Dataframe only | Dataframe, HTML, Word, LaTeX, PDF |
| **Statistical tests** | Manual selection | Automatic |
| **Publication-ready** | Requires post-processing | Direct output |
| **Customization** | Limited | Extensive (gtsummary ecosystem) |

## Additional Resources

- [gtsummary website](https://www.danieldsjoberg.com/gtsummary/)
- [gtsummary API](https://www.danieldsjoberg.com/gtsummary/reference/index.html)
- [gtsummary vignettes](https://www.danieldsjoberg.com/gtsummary/articles/index.html)

## Questions and Troubleshooting

### Q: How do I add more statistics (e.g., min/max)?
```r
table_one(df) %>%
  gtsummary::add_stat(fns = list(
    all_continuous() ~ function(x) list(
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
  ))
```

### Q: How do I exclude certain variables?
```r
table_one(df, include = c("sex", "income", "dm"))
```

### Q: How do I change the p-value formatting function?
```r
table_one(df, group = sex, pvalue_fun = function(x) {
  format.pval(x, digits = 3)
})
```
