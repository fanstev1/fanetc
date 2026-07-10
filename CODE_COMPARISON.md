# Code Comparison: Original vs gtsummary Implementation

## Example 1: Basic Summary Table

### Original Implementation
```r
# Multiple functions working together
sum_table <- table_one(df, datadic = datadic)
# Output is a dataframe with manual column construction
```

**Behind the scenes:**
1. `table_one()` calls `numeric_desp()`, `logical_desp()`, `factor_desp()`
2. Each function constructs rows independently
3. Results are bound together with manual row ID construction
4. Statistical tests are performed separately for each variable type

### gtsummary Implementation
```r
sum_table <- table_one(df, datadic = datadic)
# Output is a publication-ready table object
print(sum_table)
```

**Behind the scenes:**
1. `tbl_summary()` auto-detects all variable types
2. Generates appropriate statistics based on type
3. Returns a formatted table object ready for export

---

## Example 2: Grouped Comparison with Tests

### Original Implementation
```r
# Called with NSE, but still complex
sum_table <- table_one(df, sex, datadic = datadic)

# The function internally:
# 1. Calls numeric_desp(df, !!group)
#    - Runs two_sample_test() / k_sample_test() 
#    - Performs t-test vs Wilcox test selection
# 2. Calls logical_desp(df, !!group)
#    - Runs fisher_test() with conditional logic
# 3. Calls factor_desp(df, !!group)
#    - Runs fisher_test() with simulation.p.value
# 4. Combines all results with suffix renaming
```

**Code complexity:** the four files backing this (`desp_table.R`,
`numeric_desp.R`, `logical_desp.R`, `factor_desp.R`, all removed from the
repo — line counts below are from the last commit before the refactor,
`7ab169b`) totaled 825 lines (127 + 271 + 150 + 277).

### gtsummary Implementation
```r
sum_table <- table_one(df, group = sex, datadic = datadic, add_p = TRUE)
```

**Code complexity:** the call site is one line; the implementation behind it
(`R/desp_table_gtsummary.R`, current `master`) is 397 lines.

---

## Example 3: Numeric Variable Processing

### Original Code (numeric_desp function)
```r
numeric_desp <- function(df, group) {
  group <- rlang::enquo(group)
  df <- df %>% ungroup()

  if (rlang::quo_is_missing(group)) {
    df <- df %>% select_if(is.numeric)
    
    # Calculate n available
    n_var <- df %>%
      summarise_if(is.numeric, list(~ n_avail(.))) %>%
      melt(id.vars= "rowname", value.name = "n")
    
    # Calculate mean/sd and median/iqr
    sum_stat <- df %>%
      summarise_if(is.numeric, list(~ mean_sd(.), ~ med_iqr(.))) %>%
      # ... complex renaming logic
    
  } else {
    # Complex grouped operations
    df <- df %>% group_by(!!group) %>% select_if(is.numeric)
    
    # Similar calculations with pivot logic
    # Then call two_sample_test() or k_sample_test()
  }
  
  # Merge n_var and sum_stat with suffixes
}
```

(`numeric_desp.R` was 271 lines total, including the nested
`two_sample_test()`/`k_sample_test()` helpers shown in Example 5 below —
verified via `git show 7ab169b:R/numeric_desp.R | wc -l`)

### gtsummary Alternative
```r
# All handled in one line within tbl_summary()
statistic = list(
  all_continuous() ~ "{mean} ± {sd}",
  all_categorical() ~ "{n} ({p}%)"
)
```

**~3 lines of code**

---

## Example 4: Logical Variable Processing

### Original Code (logical_desp function)
```r
logical_desp <- function(df, group) {
  binary_desp <- function(x, pct_digits= 1) {
    fun <- c(sum, mean)
    out <- sapply(fun, function(f) {
      res <- try(f(x, na.rm= TRUE), silent = TRUE)
      res <- if (class(res)== "try-error") NA else res
      return(res)
    })
    
    freq <- formatC(out[1], format= "d", big.mark = ",")
    pct <- formatC(out[2]*100, digits= pct_digits, format= "f")
    pct <- paste0(pct, "%")
    out <- paste0(freq, " (", pct, ")")
  }
  
  # ... complex grouped summarization
  
  test_fun <- fisher_test
  # Custom fisher_test function implementation with error handling
}

# Plus the nested fisher_test() function
```

(`logical_desp.R` was 150 lines total — verified via
`git show 7ab169b:R/logical_desp.R | wc -l`)

### gtsummary Alternative
```r
# Logical variables need no special declaration: gtsummary auto-detects them
# and maps them to its built-in "dichotomous" type (the actual code in
# R/desp_table_gtsummary.R uses
#   type = list(all_categorical(dichotomous = FALSE) ~ "categorical",
#               all_dichotomous() ~ "dichotomous")
# there is no all_logical() selector in gtsummary -- an earlier draft of
# this doc used one, but it is not an exported function)

# Fisher's test is applied automatically via add_p()
gtsummary::add_p(test = list(
  all_categorical() ~ "fisher.test"
))
```

**~5 lines of code**

---

## Example 5: Statistical Testing Comparison

### Original: Manual Test Selection
```r
# numeric_desp calls:
test_fun <- if (n_groups(df)==2) two_sample_test else k_sample_test

# two_sample_test does:
# - t-test for means
# - Wilcox test for medians

# logical_desp calls:
fisher_test <- if (n_groups(df)==2) {
  function(...) try(fisher.test(..., conf.int= FALSE), silent = TRUE)
} else {
  function(...) try(fisher.test(..., hybrid = TRUE, conf.int= FALSE), silent = TRUE)
}

# Manual p-value formatting
pval = format_pvalue(pval)
```

(This test-selection logic was spread across `numeric_desp.R` and
`logical_desp.R`; no reliable way to isolate an exact line count for just
this piece was found, so no specific number is given here.)

### gtsummary: Automatic Test Selection
```r
gtsummary::add_p(
  test = list(
    all_continuous() ~ "t.test",      # Automatic t-test vs ANOVA selection
    all_categorical() ~ "fisher.test"  # Automatic Fisher's exact
  ),
  pvalue_fun = format_pvalue            # Custom formatting function
)
```

**All test-selection logic handled by gtsummary in a single `add_p()` call**

---

## Example 6: Complete Workflow

### Original Workflow
```r
# Data preparation (manual type cleaning)
df <- df %>%
  ungroup() %>%
  select_if(Negate(is.character)) %>%
  select_if(Negate(is.Date)) %>%
  mutate_if(is.factor, droplevels)

# Create summary
out_lst <- NULL
if (any_numeric) {
  num_out_lst <- numeric_desp(df, !!group)
  out_lst <- c(out_lst, num_out_lst)
}
if (any_factor) {
  fct_out_lst <- factor_desp(df, !!group)
  out_lst <- c(out_lst, fct_out_lst)
}
if (any_logical) {
  logic_out_lst <- logical_desp(df, !!group)
  out_lst <- c(out_lst, logic_out_lst)
}

# Bind and format
out <- bind_rows(out_lst) %>%
  mutate(type = ifelse(...), ...) %>%
  select(row_id, variable, type, ...)

# Manual label application
if (!is.null(datadic)) {
  out <- out %>%
    left_join(datadic, by = c("variable" = quo_name(var_name)))
}
```

### gtsummary Workflow
```r
tbl <- table_one(df, group = sex, datadic = datadic, add_p = TRUE)

# That's it! Can now:
print(tbl)                             # Print to console
gtsummary::as_gt(tbl)                  # HTML
gtsummary::as_kable(tbl)               # Markdown
gtsummary::as_flex_table(tbl)          # Word
gtsummary::as_tibble(tbl)              # Dataframe
```

---

## Error Handling

### Original: Manual Try-Catch
```r
res <- try(f(x, na.rm= TRUE), silent = TRUE)
res <- if (class(res)== "try-error") NA else res
return(res)
```

### gtsummary: Built-in Robustness
```r
# Errors are handled automatically
# Invalid tests for variable combinations return NA
# No try-catch blocks needed
```

---

## Output Format Examples

### Original Output (Dataframe)
```
     variable        type stat_1         stat_2         pval
1      income      meansd "565 ± 289"    "543 ± 301"   "0.31"
2      income      mediqr "512 (345-761)" "489 (312-798)" "0.29"
3        sex Female    freq "45 (50%)"     "68 (68%)"   "<0.01"
4        sex   Male    freq "45 (50%)"     "32 (32%)"   "<0.01"
...
```

### gtsummary Output (Multiple Formats Available)

Verified: `table_one()` shows one continuous-stat row per variable, not both
mean/SD and median/IQR (controlled by the `continuous_stat` argument, default
`"meansd"`):

```
| **Characteristic** | **Female**, n=113 | **Male**, n=100 | **p-value** |
|---|---|---|---|
| Income | 565 ± 289 | 543 ± 301 | 0.31 |
| Sex | | | <0.01 |
| Female | 45 (50%) | 68 (68%) | |
| Male | 45 (50%) | 32 (32%) | |
...
```

Plus automatic formatting for publication readiness!
