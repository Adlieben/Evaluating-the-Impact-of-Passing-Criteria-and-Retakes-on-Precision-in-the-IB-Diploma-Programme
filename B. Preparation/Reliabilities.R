# Reliability is calculated here then added to the preparation file
# 1.1 LOAD and TIDY the data ----
item_data <- read.csv('A. Data/A. May data/M23 Item Data.csv', header = TRUE, sep=",")

library(tidyverse)
library(psych) # For alpha and omega
library(mice) # To impute missing grades
library(mirt) # For marginal reliability
library(MBESS) # For the GLB
library(Rcsdp) # Additional requirement for the GLB
library(purrr)
library(openxlsx)

# Create wide data
item_wide <- item_data %>%
  pivot_wider(names_from = ITEM, values_from = MARK)

# Split data by component
components <- split(item_wide, item_wide$NO_LANG_CODE)
components_back <- components # Save as backup

# 1.2 LOAD and TIDY MCQ answers ----
# Define a function to process each dataset
process_dataset <- function(data, dataset_name) {
  # Keep columns starting with "N"
  data <- data %>% select(starts_with("N"))
  
  # Remove rows with only 0s
  data <- data[rowSums(data != 0) > 0, ]
  
  # Add 'NO_LANG_CODE' column
  data <- data %>% mutate(NO_LANG_CODE = dataset_name) %>%
    select(NO_LANG_CODE, everything())
  
  # Add 'cand_anon' column with random candidate numbers
  data <- data %>% mutate(cand_anon = sample(1:nrow(data), nrow(data), replace = FALSE))
  
  # Convert to tibble
  data <- as_tibble(data)
  
  return(data)
}

# Define datasets and their names
datasets <- list(
  nl79 = read.xlsx('A. Data/B. May MCQ/nl79.xlsx'),
  nl350 = read.xlsx('A. Data/B. May MCQ/nl350.xlsx'),
  nl663 = read.xlsx('A. Data/B. May MCQ/nl663.xlsx'),
  nl914 = read.xlsx('A. Data/B. May MCQ/nl914.xlsx'),
  nl373 = read.xlsx('A. Data/B. May MCQ/nl1.xlsx', sheet = "nl373"),
  nl908 = read.xlsx('A. Data/B. May MCQ/nl1.xlsx', sheet = "nl908"),
  nl860 = read.xlsx('A. Data/B. May MCQ/nl1.xlsx', sheet = "nl860"),
  nl221 = read.xlsx('A. Data/B. May MCQ/nl1.xlsx', sheet = "nl221"),
  nl1007 = read.xlsx('A. Data/B. May MCQ/nl2.xlsx', sheet = "nl1007"),
  nl527 = read.xlsx('A. Data/B. May MCQ/nl2.xlsx', sheet = "nl527"),
  nl441 = read.xlsx('A. Data/B. May MCQ/nl2.xlsx', sheet = "nl441"),
  nl584 = read.xlsx('A. Data/B. May MCQ/nl2.xlsx', sheet = "nl584"),
  nl983 = read.xlsx('A. Data/B. May MCQ/nl3.xlsx', sheet = "nl983"),
  nl177 = read.xlsx('A. Data/B. May MCQ/nl3.xlsx', sheet = "nl177"),
  nl637 = read.xlsx('A. Data/B. May MCQ/nl3.xlsx', sheet = "nl637"),
  nl830 = read.xlsx('A. Data/B. May MCQ/nl3.xlsx', sheet = "nl830"),
  nl208 = read.xlsx('A. Data/B. May MCQ/nl4.xlsx', sheet = "nl208"),
  nl942 = read.xlsx('A. Data/B. May MCQ/nl4.xlsx', sheet = "nl942")
)

# Add MCQ items
for (name in names(datasets)) {
  components[[name]] <- process_dataset(datasets[[name]], name)
}

# 2. IDENTIFY components with optional items ----
# Find components with optionality
components_with_optionality <- list()

# Loop through each component in components list
for (component_name in names(components)) {
  # Extract the current component dataframe
  component <- component %>%
    dplyr::select(-cand_anon, -NO_LANG_CODE) %>%
    select_if(~ !all(is.na(.)))
  
  # Calculate the theoretical maximum score for this component
  max_scores <- apply(component, 2, max, na.rm = TRUE)  # Maximum recorded score per item
  theoretical_max_score <- sum(max_scores)              # Sum of maximum scores
  
  # Retrieve the actual maximum score from Compbounds for the component
  actual_max_score <- Compbounds$GRADETOMARK[Compbounds$NO_LANG_CODE == component_name & 
                                               Compbounds$COMPONENT_GRADE %in% c("A", "7")][1]
  
  # Check if theoretical maximum is greater than actual maximum (indicating optionality)
  if (length(actual_max_score) > 0 && theoretical_max_score > actual_max_score) {
    components_with_optionality[[component_name]] <- list(
      theoretical_max_score = theoretical_max_score,
      actual_max_score = actual_max_score
    )
  }
}

# Manually go through each component
for (component_name in names(components_with_optionality)) {
  # Display the current component name
  print(paste("Component:", component_name))
  
  # Access the current component's data
  subset_data <- components[["abdutSLITZ0XXXX"]]
  
  # Remove columns that are entirely NA
  subset_data <- subset_data[, colSums(!is.na(subset_data)) > 0, drop = FALSE]
  
  # View the cleaned data
  View(subset_data)
  
  # Prompt user to proceed
  repeat {
    user_input <- readline(prompt = "Type 'Yes' to proceed to the next component: ")
    if (tolower(user_input) == "yes") break
  }
}

# 3. CALCULATE reliability ----
# Store original component names
original_names <- names(components)

# Convert all component names to lowercase
names(components) <- tolower(names(components))

# Define Helper Functions

# Function to remove observations based on a condition
remove_observations <- function(df, condition_col, condition_val) {
  df %>% filter(!(!!sym(condition_col) == condition_val))
}

# Function to handle components with no optionality (do nothing)
handle_no_optionality <- function(df) {
  return(df)
}

# Function to handle optionality based on question numbers
handle_optional_questions <- function(df) {
  # Exclude 'cand_anon' and 'no_lang_code' from processing
  item_cols <- setdiff(names(df), c("cand_anon", "no_lang_code"))
  
  # Extract question numbers from item names (find the first number in the item name)
  question_numbers <- str_extract(item_cols, "\\d+")
  question_numbers <- as.numeric(question_numbers)
  
  # Create a data frame mapping column names to question numbers
  question_map <- data.frame(
    column = item_cols,
    question_number = question_numbers,
    stringsAsFactors = FALSE
  )
  
  # Exclude columns where question_number is NA
  question_map <- question_map[!is.na(question_map$question_number), ]
  
  if (nrow(question_map) == 0) {
    message("No question numbers extracted. Please check the item names.")
    return(df)
  }
  
  # Subset the dataframe to only item columns
  df_items <- df[, question_map$column, drop = FALSE]
  
  # Create a logical matrix indicating if the item is non-zero and not NA
  item_non_zero_matrix <- df_items != 0 & !is.na(df_items)
  
  # For each student, determine which questions were taken
  # Group columns by question_number
  question_groups <- split(question_map$column, question_map$question_number)
  
  n_students <- nrow(df)
  
  # Initialize a matrix to keep track of whether each student took each question
  questions_taken_matrix <- matrix(FALSE, nrow = n_students, ncol = length(question_groups))
  colnames(questions_taken_matrix) <- names(question_groups)
  
  # Determine if each student took each question
  for (i in seq_along(question_groups)) {
    cols_for_q <- question_groups[[i]]
    item_non_zero_subset <- item_non_zero_matrix[, cols_for_q, drop = FALSE]
    student_took_q <- rowSums(item_non_zero_subset) > 0
    questions_taken_matrix[, i] <- student_took_q
  }
  
  # For students who did not take a question, set items to NA
  for (i in seq_along(question_groups)) {
    cols_for_q <- question_groups[[i]]
    students_not_took_q <- which(!questions_taken_matrix[, i])
    
    if (length(students_not_took_q) > 0) {
      df[students_not_took_q, cols_for_q] <- NA
    }
  }
  
  return(df)
}

# Function to handle special optionality cases
handle_nl832 <- function(df) {
  df %>%
    rowwise() %>%
    mutate(
      Q5_nonzero = !is.na(Q5) && Q5 != 0,
      Q6_nonzero = !is.na(Q6) && Q6 != 0
    ) %>%
    mutate(
      Q5 = if_else(Q5_nonzero, Q5, as.numeric(NA)),
      Q6 = if_else(Q6_nonzero, Q6, as.numeric(NA))
    ) %>%
    ungroup() %>%
    select(-Q5_nonzero, -Q6_nonzero)
}

# Start processing components

# 1. Components with observations to remove and no optionality (indicative of a badly recorded grade)
components_to_remove <- list(
  "nl1013" = list(condition_col = "Q1", condition_val = 8),
  "nl816" = list(condition_col = "Criterion A", condition_val = 15),
  "nl2" = list(condition_col = "Q6", condition_val = 7),
  "nl1049" = list(condition_col = "Criterion A", condition_val = 15),
  "nl623" = list(condition_col = "Criterion A", condition_val = 13),
  "nl775" = list(condition_col = "Criterion B", condition_val = 12),
  "nl805" = list(condition_col = "C", condition_val = 6),
  "nl790" = list(condition_col = "cand_anon", condition_val = 49471),
  "nl528" = list(condition_col = "Criterion B", condition_val = 6),
  "nl424" = list(condition_col = "A", condition_val = 9),
  "nl5" = list(condition_col = "C", condition_val = 6),
  "nl720" = list(condition_col = "Criterion C", condition_val = 10),
  "nl813" = list(condition_col = "Criterion E", condition_val = 7)
)

for (comp in names(components_to_remove)) {
  if (comp %in% names(components)) {
    condition_col <- components_to_remove[[comp]]$condition_col
    condition_val <- components_to_remove[[comp]]$condition_val
    components[[comp]] <- remove_observations(components[[comp]], condition_col, condition_val)
    message(paste("Removed observations from", comp, "where", condition_col, "=", condition_val))
  }
}

# 2. Components with no optionality
no_optionality_components <- c(
  "nl687", "nl624", "nl726", "nl962", "nl443", "nl582", "nl1056",
  "nl1002", "nl986", "nl581", "nl813" # Already processed
)

for (comp in no_optionality_components) {
  if (comp %in% names(components)) {
    components[[comp]] <- handle_no_optionality(components[[comp]])
    message(paste("No optionality handling needed for", comp))
  }
}

# 3. Components with optionality based on question numbers
optional_components <- c(
  "nl387", "nl500", "nl457", "nl62", "nl509", "nl907", "nl306", "nl543",
  "nl748", "nl876", "nl76", "nl427", "nl750", "nl931", "nl925", "nl444",
  "nl218", "nl281", "nl933", "nl124", "nl538", "nl963", "nl913", "nl191",
  "nl932", "nl785", "nl601", "nl473", "nl982", "nl717", "nl893", "nl376",
  "nl248", "nl981", "nl749", "nl858", "nl764", "nl554", "nl419", "nl232",
  "nl52", "nl38", "nl196", "nl736", "nl422", "nl1039", "nl900", "nl413",
  "nl941", "nl490", "nl354", "nl669", "nl690", "nl365", "nl857", "nl872",
  "nl747", "nl704", "nl801", "nl77", "nl1003", "nl412", "nl612", "nl121",
  "nl698", "nl849", "nl822", "nl8", "nl16", "nl43", "nl680", "nl1000",
  "nl1020", "nl794", "nl1012", "nl864", "nl511", "nl1025", "nl776", "nl612",
  "nl689", "nl37", "nl1042", "nl864", "nl621", "nl593", "nl736", "nl490",
  "nl511", "nl520", "nl952", "nl703", "nl795", "nl985", "nl135", "nl998",
  "nl384", "nl855", "nl590", "nl771", "nl463", "nl506", "nl595", "nl86",
  "nl41", "nl472", "nl559", "nl723", "nl738", "nl282", "nl112", "nl494",
  "nl222", "nl203", "nl118", "nl316", "nl501", "nl462", "nl1", "nl416",
  "nl165", "nl899", "nl944", "nl1060", "nl300", "nl458", "nl438", "nl33",
  "nl711", "nl512", "nl752", "nl239", "nl911", "nl173", "nl549", "nl735",
  "nl600", "nl905", "nl290", "nl163", "nl710", "nl879", "nl262", "nl951",
  "nl800", "nl935", "nl334", "nl997", "nl633", "nl347", "nl965", "nl228",
  "nl423", "nl450", "nl27", "nl44", "nl580", "nl902", "nl183", "nl482"
)

for (comp in optional_components) {
  if (comp %in% names(components)) {
    components[[comp]] <- handle_optional_questions(components[[comp]])
    message(paste("Handled optionality for", comp))
  }
}

# 4. Handle nl832 special case
if ("nl832" %in% names(components)) {
  components[["nl832"]] <- handle_nl832(components[["nl832"]])
  message("Handled special optionality for nl832")
}

# 5. Restore Original Component Names
names(components) <- original_names

# 6. Save the updated components
saveRDS(components, file="Data/Components with missing.RData")

# Initialize a list to store reliability results
reliabilities_marginal <- vector("list", length(components))
names(reliabilities_marginal) <- sapply(components, function(x) as.character(x[1, 1]))

# Loop through each component
for (i in seq_along(components)) {
  
  component <- components[[i]]
  component_name <- names(reliabilities_marginal)[i]
  
  # Increment counter and print progress every 20 components
  if (i %% 20 == 0) {
    message(sprintf("Processing component %d: %s", i, component_name))
  }
  
  # Step 1: Clean the data
  items <- component %>%
    select(-cand_anon, -NO_LANG_CODE) %>%          # Remove specified columns
    select_if(~ !all(is.na(.)))                   # Remove columns that are entirely NA
  
  # Remove duplicated columns
  if (ncol(items) > 1) {
    duplicated_cols <- duplicated(as.list(items))
    if (any(duplicated_cols)) {
      items <- items[, !duplicated_cols]
    }
  }
  
  # Remove columns with zero variance
  if (ncol(items) > 0) {
    item_variances <- sapply(items, function(x) var(x, na.rm = TRUE))
    items <- items[, item_variances != 0 & !is.na(item_variances), drop = FALSE]
    # Remove people that have now taken no items
    items <- items[rowSums(!is.na(items) & items != 0) > 0, , drop = FALSE]
  }
  
  # Proceed only if there are enough items and people
  if (ncol(items) < 2 | ncol(items) >= nrow(items)) {
    reliabilities_marginal[[i]] <- list(
      reliability = NA,
      method = "insufficient data"
    )
    next
  }
  
  # Step 2: Check for missing data
  has_missing <- any(is.na(items))
  
  if (!has_missing) {
    # No missing data: Calculate omega using psych::omega
    omega_result <- tryCatch({
      omega_value <- omega(items, nfactors = 1, fm = "ml", plot = FALSE)$omega_h
      if(length(omega_value) == 0) NA else omega_value
    }, error = function(e) {
      warning(sprintf("Omega calculation failed for component '%s': %s", component_name, e$message))
      NA
    })
    
    reliabilities_marginal[[i]] <- list(
      reliability = omega_result,
      method = "omega"
    )
    
  } else {
    # Missing data: Fit mirt polytomous model and compute marginal reliability
    mirt_result <- tryCatch({
      model_fit <- mirt(items, 1, itemtype = 'gpcm', verbose = FALSE)
      reliability_value <- marginal_rxx(model_fit)
      if(length(reliability_value) == 0) NA else reliability_value
    }, error = function(e) {
      warning(sprintf("MIRT reliability calculation failed for component '%s': %s", component_name, e$message))
      NA
    })
    
    reliabilities_marginal[[i]] <- list(
      reliability = mirt_result,
      method = "marginal_rxx"
    )
  }
}

# Convert the reliabilities list to a data frame
reliability_df <- do.call(rbind, lapply(names(reliabilities_marginal), function(name) {
  data.frame(
    Component = name,
    Reliability = reliabilities_marginal[[name]]$reliability,
    Method = reliabilities_marginal[[name]]$method,
    stringsAsFactors = FALSE
  )
}))

reliabilities <- reliability_df$Reliability # recode for future use

# 4. INVESTIGATE reliabilities ----
# Descriptives
mrel <- mean(reliabilities, na.rm = T)
varrel <- var(reliabilities, na.rm = T) # Very low

# Plot
reliability_plot <- as.data.frame(reliabilities)
ggplot(reliability_plot, aes(x = reliabilities)) + geom_density()
# We notice some issues with negative values, let's inspect

# Missing reliabilities
NA_rel <- reliability_df %>% filter(is.na(reliabilities)) # Not too many
table(reliability_df$Method)
sum(is.na(reliability_df$Reliability))
sum(reliability_df$Reliability > 0.8, na.rm = T)
sum(reliability_df$Reliability > 0.98, na.rm = T)

# 5. IMPUTE missing reliabilities ----
reliabilities_imp <- ifelse(is.na(reliabilities), mrel, reliabilities)
reliability_df_imp <- reliability_df
reliability_df_imp$Reliability <- reliabilities_imp

# 6. SAVE reliabilities ----
write.csv(reliability_df_imp, "D. Results/A. Intermediate/reliability_imp.csv")