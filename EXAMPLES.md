# Practical Examples: Using gtsummary-based table_one()

## Example 1: Basic Descriptive Table (No Groups)

```r
library(gtsummary)
library(tidyverse)

# Create sample data
set.seed(42)
df <- data.frame(
  age = rnorm(100, mean = 50, sd = 15),
  weight = rnorm(100, mean = 75, sd = 20),
  height = rnorm(100, mean = 170, sd = 10),
  gender = factor(sample(c("Male", "Female"), 100, replace = TRUE)),
  smoker = sample(c(TRUE, FALSE), 100, replace = TRUE),
  diabetes = sample(c(TRUE, FALSE), 100, replace = TRUE)
)

# Create basic table
tbl1 <- table_one(df)
print(tbl1)

# Output will show one row per continuous variable (mean +/- SD, since the
# default continuous_stat = "meansd"; pass continuous_stat = "mediqr" for
# median (Q1-Q3) instead -- table_one() shows one or the other, not both):
# Characteristic           N = 100
# age                       50 +/- 16
# weight                    73 +/- 18
# height                    170 +/- 10
# gender
#   Female                  51 (51.0%)
#   Male                    49 (49.0%)
# smoker                    49 (49.0%)   <- logical vars render as ONE
# diabetes                  50 (50.0%)      dichotomous row (count of TRUE),
#                                           not separate TRUE/FALSE rows
```

---

## Example 2: Comparison by Group with Tests

```r
# Create grouped data
set.seed(42)
df <- data.frame(
  treatment = factor(rep(c("Control", "Treatment"), each = 50)),
  age = c(rnorm(50, 45, 12), rnorm(50, 52, 14)),
  systolic = c(rnorm(50, 120, 12), rnorm(50, 125, 15)),
  diastolic = c(rnorm(50, 80, 8), rnorm(50, 82, 10)),
  hypertension = c(
    sample(c(TRUE, FALSE), 50, replace = TRUE, prob = c(0.3, 0.7)),
    sample(c(TRUE, FALSE), 50, replace = TRUE, prob = c(0.45, 0.55))
  ),
  compliance = factor(
    c(
      sample(c("Low", "Medium", "High"), 50, replace = TRUE),
      sample(c("Low", "Medium", "High"), 50, replace = TRUE, prob = c(0.1, 0.3, 0.6))
    )
  )
)

# Create comparison table with statistical tests
tbl2 <- table_one(df, group = treatment, add_p = TRUE, add_overall = TRUE)
print(tbl2)

# Can also use:
tbl2 %>% gtsummary::as_kable()  # For markdown documents
tbl2 %>% gtsummary::as_gt()     # For HTML/RMarkdown
tbl2 %>% gtsummary::as_flex_table()  # For Word documents
```

---

## Example 3: Using Data Dictionary

```r
# Create data with original variable names
df <- data.frame(
  trt = factor(rep(c("Control", "Treatment"), each = 75)),
  wt = rnorm(150, 70, 15),
  ht = rnorm(150, 172, 8),
  sbp = rnorm(150, 122, 14),
  dbp = rnorm(150, 81, 10),
  hiv_status = sample(c(TRUE, FALSE), 150, replace = TRUE, prob = c(0.1, 0.9)),
  hiv_therapy = factor(
    sample(c("None", "ART", "PrEP"), 150, replace = TRUE, prob = c(0.7, 0.2, 0.1))
  )
)

# Create data dictionary for better labels
datadic <- data.frame(
  var_name = c("trt", "wt", "ht", "sbp", "dbp", "hiv_status", "hiv_therapy"),
  var_desp = c(
    "Treatment Group",
    "Weight (kg)",
    "Height (cm)",
    "Systolic BP (mmHg)",
    "Diastolic BP (mmHg)",
    "HIV Positive",
    "HIV Therapy Type"
  ),
  stringsAsFactors = FALSE
)

# Create table with labels
tbl3 <- table_one(
  df,
  group = trt,
  datadic = datadic,
  add_p = TRUE,
  add_overall = TRUE
)

print(tbl3)
# Now shows nice labels instead of "wt", "ht", etc.
```

---

## Example 4: Customizing Output Format

```r
# Start with basic table
tbl4 <- table_one(df, group = treatment, add_p = TRUE)

# Modify headers
tbl4 <- tbl4 %>%
  gtsummary::modify_header(
    stat_1 ~ "**Control** (n = {N})",
    stat_2 ~ "**Treatment** (n = {N})",
    p.value ~ "**P-value**"
  )

# Modify footnote
tbl4 <- tbl4 %>%
  gtsummary::modify_footnote(
    all_stat_cols() ~ "Mean ± SD (continuous), n (%) (categorical)"
  )

# Bold the label column (modify_table_styling() has no `bold` argument --
# verified; the correct argument is `text_format = "bold"`)
tbl4 <- tbl4 %>%
  gtsummary::modify_table_styling(
    columns = label,
    text_format = "bold"
  )

print(tbl4)
```

---

## Example 5: Combining Tables

```r
# Create separate tables for different subgroups
tbl5a <- table_one(df %>% filter(age >= 18, age < 40), datadic = datadic)
tbl5b <- table_one(df %>% filter(age >= 40, age < 60), datadic = datadic)
tbl5c <- table_one(df %>% filter(age >= 60), datadic = datadic)

# Combine them
tbl5_combined <- gtsummary::tbl_merge(
  list(tbl5a, tbl5b, tbl5c),
  tab_spanner = c("**Age 18-40**", "**Age 40-60**", "**Age 60+**")
)

print(tbl5_combined)
```

---

## Example 6: Exporting to Word

```r
library(officer)

# Create the table
tbl6 <- table_one(df, group = treatment, add_p = TRUE)

# Convert to flextable
ft <- gtsummary::as_flex_table(tbl6)

# Create Word document
doc <- officer::read_docx() %>%
  officer::body_add_heading("Patient Characteristics", level = 1) %>%
  flextable::body_add_flextable(ft) %>%
  officer::body_add_par(
    "Data are presented as mean ± SD for continuous variables and n (%) for categorical variables.
     P-values were calculated using t-tests and Fisher's exact tests."
  )

# Save
print(doc, "table_one.docx")
```

---

## Example 7: Exporting to HTML

```r
# Create the table
tbl7 <- table_one(df, group = treatment, add_p = TRUE) %>%
  gtsummary::modify_header(
    stat_1 ~ "**Control**",
    stat_2 ~ "**Treatment**"
  )

# Convert to gt (gt package provides HTML output)
gt_tbl <- gtsummary::as_gt(tbl7)

# Save HTML
gt::gtsave(gt_tbl, "table_one.html")

# Or use knitr for RMarkdown
gtsummary::as_kable(tbl7)
```

---

## Example 8: Inline Statistics in Text

```r
# Create table
tbl8 <- table_one(df, group = treatment)

# Extract specific statistics for use in text
age_control <- gtsummary::inline_text(tbl8, variable = age, column = stat_1)
age_treatment <- gtsummary::inline_text(tbl8, variable = age, column = stat_2)

# Use in RMarkdown
cat(sprintf(
  "Mean age was %s in the control group and %s in the treatment group.",
  age_control,
  age_treatment
))

# Output: "Mean age was 45 ± 12 in the control group and 52 ± 14 in the treatment group."
```

---

## Example 9: Custom Missing Data Display

```r
# Data with missing values
df_missing <- df %>%
  mutate(
    age = ifelse(runif(n()) < 0.1, NA, age),
    weight = ifelse(runif(n()) < 0.05, NA, weight),
    # must be a factor: table_one() silently drops character columns
    # (verified -- a character treatment_response column does not appear
    # in the output table at all), so wrap it in factor() to summarize it
    treatment_response = factor(sample(c("Yes", "No", NA), n(), replace = TRUE))
  )

# Show missing data
tbl9 <- table_one(
  df_missing,
  group = treatment,
  missing = "ifany",  # Only show if any missing
  missing_text = "Missing"
)

print(tbl9)
# Will show rows for missing values when present
```

---

## Example 10: Numeric Precision Control

```r
# Create table with custom digit specification
tbl10 <- table_one(df, group = treatment) %>%
  gtsummary::modify_fmt_fun(
    all_continuous() ~ function(x) formatC(x, digits = 3, format = "f")
  )

print(tbl10)
# Now shows 3 decimal places for all continuous variables
```

---

## Example 11: Sorted by P-value

```r
# table_one() has a dedicated sort_by_p argument (verified working; it calls
# gtsummary::sort_p() internally). There is no modify_rows_print() in
# gtsummary 2.1.0 -- an earlier draft of this example referenced it, but it
# is not an exported function.
tbl11 <- table_one(df, group = treatment, add_p = TRUE, sort_by_p = TRUE)

print(tbl11)
# Variables with smallest p-values appear first
```

---

## Example 12: Stratified Analysis

```r
# Three-way comparison
set.seed(42)
df_strat <- data.frame(
  treatment = factor(rep(c("Control", "Active"), each = 100)),
  gender = factor(rep(rep(c("M", "F"), each = 50), 2)),
  age = rnorm(200, 50, 15),
  response = rnorm(200, 100, 20)
)

# Overall table
tbl12a <- table_one(df_strat, group = treatment, add_p = TRUE)

# Stratified by gender
tbl12_male <- table_one(
  df_strat %>% filter(gender == "M"),
  group = treatment,
  add_p = TRUE
)

tbl12_female <- table_one(
  df_strat %>% filter(gender == "F"),
  group = treatment,
  add_p = TRUE
)

# Combine with stratification
tbl12_combined <- gtsummary::tbl_stack(
  list(tbl12a, tbl12_male, tbl12_female),
  group_header = c("Overall", "Males", "Females")
)

print(tbl12_combined)
```

---

## Tips and Tricks

### Save code with custom settings
```r
my_table_one <- function(df, group = NULL) {
  table_one(
    df,
    group = !!enquo(group),
    add_p = TRUE,
    add_overall = TRUE,
    missing = "ifany"
  ) %>%
    gtsummary::modify_header(
      all_stat_cols() ~ "**{level}** (n={N})"
    ) %>%
    gtsummary::bold_labels() %>%
    gtsummary::bold_p(t = 0.05)
}

# Use your custom function
my_table <- my_table_one(df, group = treatment)
```

### Create publication-ready table
```r
table_one(df, group = treatment, add_p = TRUE) %>%
  gtsummary::bold_labels() %>%
  gtsummary::bold_p(t = 0.05) %>%
  gtsummary::modify_footnote(all_stat_cols() ~ NA) %>%
  gtsummary::as_gt()
```

### Extract summary statistics table as dataframe
```r
tbl <- table_one(df, group = treatment, add_p = TRUE)
df_results <- gtsummary::as_tibble(tbl)
```
