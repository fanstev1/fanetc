# Quick Reference Guide

## Installation

```r
# Install gtsummary
install.packages("gtsummary")

# Or use the development version
devtools::install_github("ddsjoberg/gtsummary")

# Update DESCRIPTION file
usethis::use_package("gtsummary")
```

## Basic Usage

```r
# No grouping
table_one(df)

# With grouping
table_one(df, group = sex)

# With p-values and overall column (default when grouped)
table_one(df, group = sex, add_p = TRUE, add_overall = TRUE)

# Without p-values
table_one(df, group = sex, add_p = FALSE)
```

## Data Dictionary

```r
# Define labels
datadic <- data.frame(
  var_name = c("age", "wt"),
  var_desp = c("Age (years)", "Weight (kg)")
)

# Use in table
table_one(df, group = treatment, datadic = datadic)
```

## Common Customizations

### Print to Console
```r
tbl <- table_one(df, group = sex)
print(tbl)
```

### Convert to HTML
```r
gtsummary::as_gt(tbl)
```

### Convert to Markdown
```r
gtsummary::as_kable(tbl)
```

### Convert to Word
```r
gtsummary::as_flex_table(tbl)
```

### Convert to Dataframe
```r
gtsummary::as_tibble(tbl)
```

### Bold Labels
```r
tbl %>%
  gtsummary::bold_labels()
```

### Bold Significant P-values
```r
tbl %>%
  gtsummary::bold_p(t = 0.05)
```

### Custom Headers
```r
tbl %>%
  gtsummary::modify_header(
    stat_1 ~ "**Treatment A**",
    stat_2 ~ "**Treatment B**"
  )
```

### Sort by P-value
```r
table_one(df, group = sex, add_p = TRUE, sort_by_p = TRUE)
```

### Show Missing Data
```r
table_one(df, missing = "ifany")
```

## Extract Statistics

### From Table
```r
# Get specific statistic as text
gtsummary::inline_text(tbl, variable = age, column = stat_1)
```

### As Dataframe
```r
gtsummary::as_tibble(tbl)
```

## Common Issues & Solutions

| Problem | Solution |
|---------|----------|
| Getting error about `group` | Use named parameter: `group = sex` (not positional) |
| Character variables included | They're auto-removed; if needed, convert to factor first |
| Date variables included | They're auto-removed; summarize separately if needed |
| Too many decimal places | Use `gtsummary::modify_fmt_fun()` |
| Want different p-value format | Pass custom function to `pvalue_fun` argument |
| Want to save to Word | Use `gtsummary::as_flex_table()` and officer package |
| Want to export to Excel | Convert to dataframe first with `as_tibble()` |

## Chaining Operations

```r
table_one(df, group = sex, add_p = TRUE) %>%
  gtsummary::bold_labels() %>%
  gtsummary::bold_p(t = 0.05) %>%
  gtsummary::modify_header(
    all_stat_cols() ~ "**{level}**"
  ) %>%
  gtsummary::as_gt()
```

## Performance Tips

1. **Large dataframes (>10k rows):** Performance is still good; no special handling needed
2. **Many variables (>50):** Consider using `include` parameter to select subset
3. **Multiple tables:** Save intermediate results to avoid recomputation
4. **Memory:** gtsummary is memory-efficient; default output is optimized

## File Locations

- **New function:** `/R/desp_table_gtsummary.R`
- **Migration guide:** `/MIGRATION_GUIDE.md`
- **Code comparison:** `/CODE_COMPARISON.md`
- **Examples:** `/EXAMPLES.md`
- **API reference:** `/API_REFERENCE.md`
- **This guide:** `/QUICKREF.md`

## Migration Checklist

- [ ] Install `gtsummary` package
- [ ] Update `DESCRIPTION` file with gtsummary dependency
- [ ] Replace `desp_table.R` with `desp_table_gtsummary.R`
- [ ] Update function calls: `table_one(df, sex)` → `table_one(df, group = sex)`
- [ ] Test with existing data
- [ ] Update downstream code that depends on input dataframes
- [ ] Consider removing old R files (keep as backup initially)

## Quick Examples

### Example 1: Baseline Table
```r
df <- data.frame(age = rnorm(100, 50, 15),
                 sex = factor(c(rep("M", 50), rep("F", 50))),
                 disease = sample(c(TRUE, FALSE), 100, replace = TRUE))
table_one(df)
```

### Example 2: Randomized Trial
```r
tbl <- table_one(df, group = treatment, add_p = TRUE) %>%
  gtsummary::bold_labels() %>%
  gtsummary::bold_p(t = 0.05)
```

### Example 3: With Labels
```r
dict <- data.frame(var_name = c("age", "bmi"),
                    var_desp = c("Age (years)", "BMI (kg/m²)"))
table_one(df, group = sex, datadic = dict)
```

### Example 4: Save to Word
```r
library(officer)
tbl <- table_one(df, group = sex)
doc <- read_docx() %>%
  body_add_flextable(gtsummary::as_flex_table(tbl))
print(doc, "table.docx")
```

## Key Differences from Original

| Aspect | Old | New |
|--------|-----|-----|
| Group parameter | Positional | Named: `group = ` |
| Return type | Dataframe | `tbl_summary` object |
| Variable type functions | Separate functions | Built into `tbl_summary()` |
| Statistical tests | Manual | Automatic via `add_p()` |
| Export formats | Limited | Multiple (HTML, Word, LaTeX, etc.) |
| Code lines | ~800 | ~200 |

## Resources

- **gtsummary website:** https://www.danieldsjoberg.com/gtsummary/
- **gtsummary GitHub:** https://github.com/ddsjoberg/gtsummary
- **gtsummary articles:** https://www.danieldsjoberg.com/gtsummary/articles/
- **gtsummary API:** https://www.danieldsjoberg.com/gtsummary/reference/

## Need Help?

1. Check **EXAMPLES.md** for usage patterns
2. See **API_REFERENCE.md** for function details
3. Review **CODE_COMPARISON.md** for implementation differences
4. Check gtsummary documentation for advanced customization
