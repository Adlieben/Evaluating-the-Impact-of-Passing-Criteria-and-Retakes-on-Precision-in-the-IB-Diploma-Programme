# 1. Load library and datasets ----
library(tidyverse)

may_component_data <- read.csv("A. Data/A. May data/M23 component data.csv", stringsAsFactors = FALSE)
may_component_grade_boundaries <- read.csv("A. Data/A. May data/M23 component grade boundaries.csv", stringsAsFactors = FALSE)
may_item_data <- read.csv("A. Data/A. May data/M23 item data.csv", stringsAsFactors = FALSE)

nov_component_grade_boundaries <- read.csv("A. Data/C. November data/N23 component grade boundaries.csv", stringsAsFactors = FALSE)

nl_key <- read.csv("Keys/no_lang_key.csv", stringsAsFactors = FALSE)

# 2. Define a function to reverse anonymisation ----
reverse_nl <- function(df, col_name, key_df) {
  df[[col_name]] <- key_df$Original[match(df[[col_name]], key_df$Anonymized)]
  return(df)
}

# 3. Recover original names from key ----
may_component_data <- reverse_nl(may_component_data, "NO_LANG_CODE", nl_key)
may_component_grade_boundaries <- reverse_nl(may_component_grade_boundaries, "NO_LANG_CODE", nl_key)
may_item_data <- reverse_nl(may_item_data, "NO_LANG_CODE", nl_key)
nov_component_grade_boundaries <- reverse_nl(nov_component_grade_boundaries, "NO_LANG_CODE", nl_key)

# 4. Optionally save the de-anonymised datasets ----
write.csv(may_component_data, "A. Data/A. May data/M23 component data (deanon).csv", row.names = FALSE)
write.csv(may_component_grade_boundaries, "A. Data/A. May data/M23 component grade boundaries (deanon).csv", row.names = FALSE)
write.csv(may_item_data, "A. Data/A. May data/M23 item data (deanon).csv", row.names = FALSE)
write.csv(nov_component_grade_boundaries, "C. November data/N23 component grade boundaries (deanon).csv", row.names = FALSE)
