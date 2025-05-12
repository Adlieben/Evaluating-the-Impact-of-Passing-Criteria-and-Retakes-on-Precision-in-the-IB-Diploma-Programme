# This file is loaded in the simulation but can be tested alone.
# 1. Libraries and data ----
library(MASS) # For multivariate function
library(tidyverse)
library(data.table)

# Data
data <- read.csv('A. Data/A. May Data/M23 component data.csv', header = TRUE, sep=",")
Compbounds <- read.csv('A. Data/A. May Data/M23 component grade boundaries.csv')
Subbounds <- read.csv('A. Data/A. May Data/M23 subject boundaries.csv')
Novcompbounds <- fread('A. Data/C. November Data/N23 component grade boundaries.csv')
Novsubbounds <- read.csv('A. Data/C. November Data/N23 subject boundaries.csv')

###############
# 2. Data cleaning ----
# Find associated codes for each subject and add them to the bounds file
dp_data <- data %>% filter(CATEGORY %in% c('DIPLOMA', 'COURSE', 'ANTICIPATED', 'RETAKE'))
codes <- dp_data %>% dplyr::select(PAPER_CODE, SUBJECT_OPTION) %>% unique() %>%
  mutate(PAPER_CODE = strtrim(PAPER_CODE, 5)) %>% unique()
Subbounds <- Subbounds %>%
  left_join(codes %>% dplyr::select(SUBJECT_OPTION, PAPER_CODE), by = "SUBJECT_OPTION")

# Add the TK bounds that are not present
tk_boundaries <- data.frame(
  SUBJECT_OPTION = rep("THEORY KNOWL.", 15),
  LVL = rep("TK", 15),
  SUBJECT_TIMEZONE = rep(0:2, each = 5),
  SUBJECT_GRADE = rep(c("E", "D", "C", "B", "A"), 3),
  OLD_SUBJECT_UPPER_BOUNDARY = rep(c(4, 10, 16, 22, 30), 3),
  NEW_SUBJECT_UPPER_BOUNDARY = rep(c(4, 10, 16, 22, 30), 3),
  PAPER_CODE = rep("tokxx", 15)
)

Subbounds <- rbind(Subbounds, tk_boundaries)

candidate_subject_lvl_counts <- dp_data %>%
  mutate(subj_lvl = paste(SUBJECT_OPTION, LVL, sep = "_")) %>%
  group_by(CANDIDATE) %>%
  summarise(n_subject_lvl = n_distinct(subj_lvl)) %>%
  ungroup()

filtered_candidates <- candidate_subject_lvl_counts %>%
  # Keep only those with at least 8 unique combos
  filter(n_subject_lvl >= 8, CANDIDATE != '278601') %>%
  select(CANDIDATE)

sim_data <- dp_data %>%
  semi_join(filtered_candidates, by = "CANDIDATE") %>%
  dplyr::select(CANDIDATE, PAPER_CODE, MODERATED_MARK, SUBJECT_OPTION, LVL, SUBJECT_TIMEZONE, RESULT_CODE, TOTAL_POINTS) %>% unique() %>%
  filter(!is.na(MODERATED_MARK))

decision <- sim_data %>% select(CANDIDATE, RESULT_CODE, TOTAL_POINTS) %>% unique()

# Separate level for later
sim_data <- sim_data %>%
  mutate(test = paste0(PAPER_CODE, "_",
                       SUBJECT_OPTION, "_", LVL, "_", SUBJECT_TIMEZONE)) %>%
  select(-c(PAPER_CODE, SUBJECT_OPTION, LVL, SUBJECT_TIMEZONE, RESULT_CODE, TOTAL_POINTS))

###############
# 3. Create wide format ----
sim_wide_0 <- sim_data %>% pivot_wider(names_from = c(test), values_from = MODERATED_MARK)

# Do this in two steps to retain candidate number ordering
sim_wide <- sim_wide_0 %>% dplyr::select(-CANDIDATE)
# Could rename row names here but it's a tibble

###############
# 4. Calculate means, variance and reliability of component scores ----
# MEANS
M <- colMeans(sim_wide, na.rm = TRUE)
mean_df <- as.data.frame(M)

# Modifications to be able to match reliability
base_names <- sub("_.*", "", rownames(mean_df))
lookup <- setNames(Compbounds$NO_LANG_CODE, Compbounds$PAPER_CODE)
mean_df$Component <- lookup[base_names]
comp_names <- mean_df$Component
mean_df_order <- data.frame(OriginalRow = rownames(mean_df), mean_df, stringsAsFactors = FALSE)

# COVARIANCE
# S <- cov(sim_wide, use = "pairwise.complete.obs")
S <- as.matrix(read.csv("D. Results/A. Intermediate/covmat.csv"))[,-1] # load to save time, run command above to re-obtain
S <- apply(S, 2, as.numeric)
diag(S) <- ifelse(diag(S) == 0, mean(diag(S)[diag(S) != 0], na.rm = TRUE), diag(S))

# RELIABILITY
# The reliability dataframe has one component more than observed in dp_data
# This is the personal project exclusive to the Career-Programme (CP)
rel <- read.csv("D. Results/A. Intermediate/reliability_imp.csv") # Obtained in other script
merged_df <- mean_df_order %>%
  left_join(rel, by = "Component") %>% dplyr::select(-c(M, OriginalRow, Method))
reliability_df <- merged_df %>% dplyr::select(Reliability)
R <- unlist(reliability_df)

# For the moment impute values for components not in the dataset
R[is.na(R)] <- mean(R, na.rm = TRUE)

# Compute Sigma.E
s_diag <- as.numeric(diag(S))
me_var <- as.numeric((1 - R) * s_diag)
me_var[is.na(me_var)] <- mean(me_var, na.rm = TRUE)
Sigma.E <- diag(me_var) # This is the variance, not standard deviation
Sigma.T <- S - Sigma.E
Sigma.E[is.na(Sigma.E)] <- 0
Sigma.E[is.infinite(Sigma.E)] <- 0

# Inspect large values
colnames(Sigma.E) <- comp_names
cols_over_30 <- colnames(Sigma.E)[apply(Sigma.E, 2, function(col) any(50 < col))]
cols_over_30

# Check whether variance or reliability is the issue
colnames(S) <- comp_names
diag_S <- diag(S)
names(diag_S) <- colnames(S)
data.frame(
  Component = cols_over_30,
  Diag_S = diag_S[cols_over_30],
  Reliability = merged_df$Reliability[match(cols_over_30, merged_df$Component)]
)

# Plot for some
ggplot(dp_data[dp_data$NO_LANG_CODE == "comscHP1TZ2XXXX", ], aes(x = MODERATED_MARK)) +
  geom_density(fill = "blue", alpha = 0.4) +
  labs(title = "Density of MODERATED_MARK for comscHP1TZ2XXXX",
       x = "MODERATED_MARK",
       y = "Density")

# 4.1 Use Kelley's formula to shrink components ----
# Create a copy of the wide matrix
sim_wide_shrunk <- sim_wide

# Loop over each component (column)
for(j in seq_len(ncol(sim_wide_shrunk))) {
  # Get the mean for component j from the vector M
  mu_j <- M[j]
  # Get the reliability for component j from the vector R
  r_j <- R[j]
  # Apply Kelley’s formula to each non-missing score:
  sim_wide_shrunk[, j] <-  mu_j + r_j * (sim_wide_shrunk[, j] - mu_j)
}

# (Optional) Inspect before/after for a sample column
head(sim_wide[, 1:5])
head(sim_wide_shrunk[, 1:5])
sum(sim_wide != sim_wide_shrunk)

###############
# 5. Prepare arguments for baseline fortran call ----
# 5.1 Prepare component data ----
data_matrix <- as.matrix(sim_wide_shrunk)
data_matrix_noNA <- data_matrix
data_matrix_noNA[is.na(data_matrix_noNA)] <- -999  # Replace NA with dummy value

mask_matrix <- ifelse(is.na(sim_wide_shrunk), 0L, 1L)

SigmaE_matrix <- as.matrix(Sigma.E)
diag_fortran <- as.double(diag(SigmaE_matrix))

# Get scaling factors from boundaries (column 8 of Compbounds):
bounds_list <- lapply(colnames(sim_wide_shrunk), function(colname) {
  paper_code <- sub("_.*", "", colname)
  lvl <- sub(".*_([A-Z]{2})_\\d+$", "\\1", colname)
  
  Compbounds %>% filter(PAPER_CODE == paper_code, LVL == lvl)
})
scaleFactor_vector <- sapply(bounds_list, function(x) x[[8]][1])

# 5.2 Create subject group vector & flatten grade boundaries ----
# Create subject group vector from column names
col_info <- tidyr::separate(tibble(col_name = colnames(sim_wide_shrunk)),
                            col_name,
                            into = c("Code", "Subject", "Lvl", "Timezone"),
                            sep = "_")
subject_group <- paste(col_info$Subject, col_info$Lvl, col_info$Timezone, sep = "_")
group_vector <- as.integer(factor(subject_group, levels = unique(subject_group)))

# Create a truncated col_info (one row per distinct subject group)
col_info_trunc <- col_info %>% distinct(across(c(Subject, Lvl, Timezone)), .keep_all = TRUE)
unique_subjects <- paste(col_info_trunc$Subject, col_info_trunc$Lvl, col_info_trunc$Timezone, sep = "_")

# Define a helper function to convert grade letters to numeric.
convertGrade <- function(x) {
  if (is.na(as.numeric(x))) {
    grade_map <- c("A" = 5, "B" = 4, "C" = 3, "D" = 2, "E" = 1)
    return(grade_map[x])
  } else {
    return(x)
  }
}

# Create a list of grade boundaries (one element per distinct subject)
grade_bounds_list <- lapply(1:nrow(col_info_trunc), function(i) {
  lvl <- as.character(col_info_trunc[i, "Lvl"])
  subject <- as.character(col_info_trunc[i, "Subject"])
  time_zone <- as.character(col_info_trunc[i, "Timezone"])
  code <- as.character(col_info_trunc[i, "Code"])
  if (lvl != "EE") {
    bounds <- Subbounds %>% filter(LVL == lvl,
                                   SUBJECT_OPTION == subject,
                                   SUBJECT_TIMEZONE == time_zone)
  } else {
    bounds <- Compbounds %>% filter(PAPER_CODE == code) %>% unique()
    bounds <- bounds[nrow(bounds):1, ]  # Reverse order for EE
    colnames(bounds)[c(4,6)] <- c("SUBJECT_GRADE", "NEW_SUBJECT_UPPER_BOUNDARY")
  }
  return(bounds)
})
names(grade_bounds_list) <- unique_subjects

# For each subject group, determine the number of boundary rows and create matrices.
nBound_vector <- sapply(grade_bounds_list, nrow) - 1 # -1 to ensure logic with checking within Fortran
nBoundMax <- max(nBound_vector+1)
nGroups <- length(unique_subjects)

bound_ub_matrix <- matrix(-999, nrow = nBoundMax, ncol = nGroups)
grade_val_matrix <- matrix(-999, nrow = nBoundMax, ncol = nGroups)
for (i in seq_along(unique_subjects)) {
  bounds <- grade_bounds_list[[unique_subjects[i]]]
  nB <- nrow(bounds)
  bound_ub_matrix[1:nB, i] <- as.numeric(bounds$NEW_SUBJECT_UPPER_BOUNDARY)
  grade_val_matrix[1:nB, i] <- sapply(bounds$SUBJECT_GRADE, convertGrade)
}

bound_ub_vector <- as.double(bound_ub_matrix)
grade_val_vector <- as.double(grade_val_matrix)
nBound_vector_int <- as.integer(nBound_vector)
nBoundMax_int <- as.integer(nBoundMax)

# 5.3 Define bonus mapping for TK/EE subjects ----
# Create the bonus mapping for TK/EE subjects.
# Here, we assume that TK/EE final grades (A-E) are converted to numeric 1-5.
# We then define the bonus mapping matrix where rows are TK grades and columns are EE grades.
tokee_matrix <- matrix(c(3, 3, 2, 2, 0,
                         3, 2, 2, 1, 0,
                         2, 2, 1, 0, 0,
                         2, 1, 0, 0, 0,
                         0, 0, 0, 0, 0),
                       nrow = 5, byrow = TRUE)
grade_nums <- 5:1
bonus_codes_matrix <- outer(grade_nums, grade_nums, FUN = function(tk, ee) { 10 * tk + ee })
bonus_points_matrix <- tokee_matrix
bonus_codes_vector <- as.integer(bonus_codes_matrix)
bonus_points_vector <- as.integer(bonus_points_matrix)
bonus_length <- as.integer(length(bonus_codes_vector))
bonus_length <- as.integer(bonus_length)

# Define a flag for each subject group: 1 if TK, EE, HL, SL and 0 otherwise.
tk_ee_flag <- ifelse(col_info_trunc$Lvl %in% c("TK", "EE"), 1L, 0L)
tk_ee_flag <- as.integer(tk_ee_flag)
hl_flag <- ifelse(col_info_trunc$Lvl %in% c("HL"), 1L, 0L)
sl_flag <- ifelse(col_info_trunc$Lvl %in% c("SL"), 1L, 0L)
hl_flag <- as.integer(hl_flag)
sl_flag <- as.integer(sl_flag)
# 5.4 Prepare other parameters for Fortran call ----
# Inputs
nrows <- nrow(data_matrix)
ncols <- ncol(data_matrix)
data_fortran    <- as.double(data_matrix_noNA)
mask_fortran    <- as.integer(mask_matrix)
scale_fortran   <- as.double(scaleFactor_vector)
group_fortran   <- as.integer(group_vector)
nSim <- 1000

# Outputs (need to create beforehand for storage)
totals_fortran  <- double(nrows * nGroups)
finalGrades_fortran <- double(nrows * nGroups)
passFlag_fortran <- integer(nrows)
student_totals_fortran <- double(nrows)
ierr_fortran    <- integer(1)
tk_ee_count_fortran <- integer(nrows)
failReasonsReal_vec <- integer(nrows * 7)
componentScoresReal_vec <- double(nrows * ncols)
groupTotalsReal_vec      <- double(nrows * nGroups)
subjectGradesReal_vec    <- double(nrows * nGroups)
failReasonsReal_vec <- integer(nrows * 7)
failReasonsObs_all <- array(0L, c(nrows, 7, nSim))
results_mat <- matrix(NA_real_, nrow=nSim, ncol=5)
colnames(results_mat) <- c("sens", "spec", "ppv", "npv", "acc")
mismatchStorage <- matrix(0L, nrow=nSim, ncol=7,
                          dimnames=list(NULL, paste0("Cond",1:7)))
reasonMatrix_list <- vector("list", nSim)

###############
# 6. Prepare arguments for retake analysis ----
# 6.1 Set up retake probability ----
model_data <- dp_data %>% # Extract only necessary variables
  semi_join(filtered_candidates, by = "CANDIDATE") %>%
  dplyr::select(CANDIDATE, GENDER, REGIONAL_OFFICE) %>% unique()

# Load the data from the retake file
combined_data <- read.csv('D. Results/A. Intermediate/retake_combined.csv')[,-1]

# Run logistic model
logit_model <- glm(Retake ~ GENDER + REGIONAL_OFFICE + TOTAL_POINTS + Fails,
                   data = combined_data, family = binomial)
summary(logit_model)

# Create baseline probabilities of retake
model_data_full <- model_data
model_data_full$TOTAL_POINTS <- 0
model_data_full$Fails <- 0

# Compute the baseline linear predictor for model_data using the model.
baseline <- predict(logit_model, newdata = model_data_full, type = "link")
baseline[is.na(baseline)] <- 0

# (This baseline is: intercept + effect(GENDER) + effect(REGIONAL_OFFICE))

# Extract the coefficients for TOTAL_POINTS and Fails (to be applied later in Fortran)
coef_total_points <- coef(logit_model)["TOTAL_POINTS"]
coef_fails        <- coef(logit_model)["Fails"]
coeffs <- c(coef_total_points, coef_fails)

sim_total_points <- model_data_full$TOTAL_POINTS  # simulated total points (computed later in Fortran)
sim_fails        <- model_data_full$Fails        # simulated number of fails

# Set the decision threshold (0.2 to allign with previous findings)
threshold <- 0.2

# 6.2 Define distribution of differences ----
Diffs <- read.csv('D. Results/A. Intermediate/100differences.csv')[-1]
mean100 <- mean(Diffs[,1], na.rm = T)
sd100 <- sd(Diffs[,1], na.rm = T)
# 6.3 Create November subjects boundary object ----
Nov_grade_bounds_list <- lapply(1:nrow(col_info_trunc), function(i) {
  lvl <- as.character(col_info_trunc[i, "Lvl"])
  subject <- as.character(col_info_trunc[i, "Subject"])
  time_zone <- as.character(col_info_trunc[i, "Timezone"])
  code <- as.character(col_info_trunc[i, "Code"])
  if (lvl != "EE") {
    bounds <- Novsubbounds %>% filter(LVL == lvl,
                                      SUBJECT_OPTION == subject,
                                      SUBJECT_TIMEZONE == time_zone)
  } else {
    bounds <- Novcompbounds %>% filter(PAPER_CODE == code) %>% unique()
    colnames(bounds)[c(4,6)] <- c("SUBJECT_GRADE", "NEW_SUBJECT_UPPER_BOUNDARY")
  }
  if (nrow(bounds) == 0){ # Added here to account for some bounds not existing in November
    bounds <- grade_bounds_list[[i]]
  } 
  return(bounds)
})
names(Nov_grade_bounds_list) <- unique_subjects

nBound_vector_nov <- sapply(Nov_grade_bounds_list, nrow)
nBoundMax_nov <- max(nBound_vector_nov)

bound_ub_matrix_nov  <- matrix(-999, nrow = nBoundMax_nov, ncol = nGroups)
grade_val_matrix_nov <- matrix(-999, nrow = nBoundMax_nov, ncol = nGroups)

for (i in seq_along(unique_subjects)) {
  bounds <- Nov_grade_bounds_list[[unique_subjects[i]]]
  nB <- nrow(bounds)
  
  bound_ub_matrix_nov[1:nB, i]  <- as.numeric(bounds$NEW_SUBJECT_UPPER_BOUNDARY)
  grade_val_matrix_nov[1:nB, i] <- sapply(bounds$SUBJECT_GRADE, convertGrade)
}

# Correct previous behaviour of function
grade_val_matrix_nov[1:5,] <- 1:5

# 6.4 Create May and November component boundary object ----
comp_info <- col_info %>% select(Code, Lvl)

unique_comps <- as.character(as.matrix(comp_info))
nComponents <- ncol(sim_wide_shrunk)
comp_tkee <- ifelse(col_info$Lvl %in% c("EE", "TK"), 1L, 0L)

Comp_list <- lapply(1:nrow(comp_info), function(i) {
  code <- as.character(comp_info[i, "Code"])
  if (!(col_info[i, "Lvl"] %in% c("EE", "TK"))) {
    bounds <- Compbounds %>% filter(PAPER_CODE == code)
    bounds <- bounds[1:7,]
  } else {
    bounds <- Compbounds %>% filter(PAPER_CODE == code) %>% unique()
    bounds <- bounds[5:1, ]
  }
  return(bounds)
})

# November inputs to fortran
maxNovBound <- 7
nov_boundaries <- matrix(NA_real_, nrow = maxNovBound, ncol = nComponents)
nBound_Nov <- integer(nComponents)

# The following function is necessary to account for some of the components
# Not being technically retakable in November. To account for this, the
# boundaries of a different time zone are used instead (TZ0 instead of TZ2).
# In case even this fails, we take the component boundaries from May out of lack
# of a better option
try_nov_bounds <- function(code, lvl, Novcompbounds) {
  # 1) Attempt a direct exact match
  bounds <- Novcompbounds %>% 
    filter(PAPER_CODE == code, LVL == lvl) %>%
    unique()
  
  # 2) If that yields no rows, try partial matching: 
  #    "same first 8 characters" + "same last 4 characters."
  if (nrow(bounds) == 0) {
    code_8first <- substr(code, 1, 8)
    n_code <- nchar(code)
    if (n_code >= 4) {
      code_4last <- substr(code, n_code - 3, n_code)
    } else {
      code_4last <- code  # if code is too short, fallback
    }
    
    fallback <- Novcompbounds %>%
      filter(substr(NO_LANG_CODE, 1, 8) == code_8first,
             substr(NO_LANG_CODE, nchar(NO_LANG_CODE)-3, nchar(NO_LANG_CODE)) == code_4last) %>%
      unique()
    
    if (nrow(fallback) > 0) {
      bounds <- fallback
    } else { # If all else fails, take boundary from May
      fallback <- Compbounds %>% 
        filter(PAPER_CODE == code, LVL == lvl) %>%
        unique()
      bounds <- fallback
    }
  }
  return(bounds)
}

Nov_comp_list <- lapply(1:nrow(comp_info), function(i) {
  code <- as.character(comp_info[i, "Code"])
  lvl <- as.character(comp_info[i, "Lvl"])
  
  if (!(lvl %in% c("EE", "TK"))) {
    bounds <- try_nov_bounds(code, lvl, Novcompbounds)
    # Now do any trimming to the first 7 rows if needed:
    if (nrow(bounds) >= 7) {
      bounds <- bounds[1:7, ]
    } else if (nrow(bounds) < 7 && nrow(bounds) > 0) {
      # fill up or handle partial if you want
    } else if (nrow(bounds) == 0) {
      # fallback to something else (like the M23 or your original logic)
    }
    
  } else {
    bounds <- try_nov_bounds(code, lvl, Novcompbounds) %>% unique()
    if (nrow(bounds) >= 5) {
      bounds <- bounds[1:5, ]
    } else if (nrow(bounds) == 0) {
      # fallback logic
    }
  }
  
  return(bounds)
})

for(i in seq_len(nComponents)) {
  # For each component, extract its boundaries from Nov_comp_list.
  bounds <- Nov_comp_list[[i]]
  bound_vals <- as.numeric(bounds$GRADETOMARK)  # The raw boundary cutpoints
  
  if(comp_tkee[i] == 1) {
    # TK/EE => 5 boundaries
    nBound_Nov[i] <- 5
    if(length(bound_vals) >= 5) {
      nov_boundaries[1:5, i] <- bound_vals[1:5]
    } else if(length(bound_vals) > 0) {
      nov_boundaries[1:length(bound_vals), i] <- bound_vals
      nov_boundaries[(length(bound_vals)+1):5, i] <- rep(bound_vals[length(bound_vals)], 5 - length(bound_vals))
    } else {
      nov_boundaries[1:5, i] <- rep(100.0, 5)
    }
    # For consistency, set rows 6 & 7 to -999 if you want
    nov_boundaries[6:7, i] <- -999
    
  } else {
    # Non-TK/EE => 7 boundaries
    nBound_Nov[i] <- 7
    if(length(bound_vals) >= 7) {
      nov_boundaries[1:7, i] <- bound_vals[1:7]
    } else if(length(bound_vals) > 0) {
      nov_boundaries[1:length(bound_vals), i] <- bound_vals
      nov_boundaries[(length(bound_vals)+1):7, i] <- rep(bound_vals[length(bound_vals)], 7 - length(bound_vals))
    } else {
      nov_boundaries[1:7, i] <- rep(100.0, 7)
    }
  }
}

# Get rid of NA values
nov_boundaries[is.na(nov_boundaries)] <- -999

grade_bands_TKEE_nov  <- seq(0, 100, length.out = 6)  # same as May
grade_bands_other_nov <- seq(0, 100, length.out = 8)

# Get the scaling for November
scaleFactor_vector_nov <- sapply(Nov_comp_list, function(x) {
  if (nrow(x) > 0) {
    return(x[[8]][1])  
  } else {
    return(NA_real_)
  }
})

# 6.5 Prepare arguments for fortran ----
# We will create a matrix of May boundaries with 7 rows (for non-TK/EE)
# and a vector nBound_May of length nComponents.
maxMayBound <- 7
may_boundaries <- matrix(NA_real_, nrow = maxMayBound, ncol = nComponents)
nBound_May <- integer(nComponents)

# Build an indicator vector: 1 if the component's level is "EE" or "TK", 0 otherwise.
# (We use the Lvl field from col_info; ensure that col_info has the correct order corresponding to columns.)

for(i in seq_len(nComponents)) {
  # For each component, extract its boundaries from Comp_list.
  bounds <- Comp_list[[i]]

  # Check that the expected column exists.
  if(!("GRADETOMARK" %in% colnames(bounds))) {
    warning(paste("Component", i, "has no GRADETOMARK; filling with 100"))
    bound_vals <- numeric(0)
  } else {
    bound_vals <- as.numeric(bounds$GRADETOMARK)
  }

  if(comp_tkee[i] == 1) {
    # For TK/EE components, we expect 5 boundaries.
    nBound_May[i] <- 5
    if(length(bound_vals) >= 5) {
      may_boundaries[1:5, i] <- bound_vals[1:5]
    } else if(length(bound_vals) > 0) {
      may_boundaries[1:length(bound_vals), i] <- bound_vals
      may_boundaries[(length(bound_vals)+1):5, i] <- rep(bound_vals[length(bound_vals)], 5 - length(bound_vals))
    } else {
      may_boundaries[1:5, i] <- rep(100.0, 5)
    }
    # For consistency, set rows 6 and 7 to -999
    may_boundaries[6:7, i] <- -999
  } else {
    # For non-TK/EE components, we expect 7 boundaries.
    nBound_May[i] <- 7
    if(length(bound_vals) >= 7) {
      may_boundaries[1:7, i] <- bound_vals[1:7]
    } else if(length(bound_vals) > 0) {
      may_boundaries[1:length(bound_vals), i] <- bound_vals
      may_boundaries[(length(bound_vals)+1):7, i] <- rep(bound_vals[length(bound_vals)], 7 - length(bound_vals))
    } else {
      may_boundaries[1:7, i] <- rep(100.0, 7)
    }
  }
}

# Define grade bands for conversion:
grade_bands_TKEE <- seq(0, 100, length.out = 6)    # for TK/EE subjects (5 intervals)
grade_bands_other <- seq(0, 100, length.out = 8)     # for other subjects (7 intervals)

# Add flag of whether component was an exam or continuous assessment
col_info <- col_info %>%
  left_join(dp_data %>% select(PAPER_CODE, ASSESSMENT_METHOD),
            by = c("Code" = "PAPER_CODE")) %>%
  rename(Type = ASSESSMENT_METHOD) %>% unique() %>%
  mutate(IsExam = Type == "EXAM")
exam_vector <- as.integer(col_info$IsExam)
# 6.6 Prepare gender and IB office outputs ----
# Demographic set
Dem_data <- dp_data %>%
  semi_join(filtered_candidates, by = "CANDIDATE") %>%
  dplyr::select(CANDIDATE, GENDER, REGIONAL_OFFICE) %>% unique()

# Flags for gender and office to pass to fortran
Dem_data_flags <- Dem_data %>%
  mutate(
    gender_flag = case_when(
      GENDER == "X"      ~ 0L,
      GENDER == "MALE"   ~ 1L,
      GENDER == "FEMALE" ~ 2L,
      TRUE ~ 0L  # fallback or handle unknown
    ),
    office_flag = case_when(
      REGIONAL_OFFICE == "IBAEM" ~ 0L,
      REGIONAL_OFFICE == "IBAP"  ~ 1L,
      REGIONAL_OFFICE == "IBLA"  ~ 2L,
      REGIONAL_OFFICE == "IBNA"  ~ 3L,
      TRUE ~ 0L  # fallback or handle unknown
    )
  )
gender_fortran <- as.integer(Dem_data_flags$gender_flag)
office_fortran <- as.integer(Dem_data_flags$office_flag)

################
# 7. Prepare arguments without Kelley estimates ----
# These are used in the last step of the simulation
# 7.1  Prepare component data ----------------------------------------
data_matrix_orig       <- as.matrix(sim_wide)
data_matrix_orig_noNA  <- data_matrix_orig
data_matrix_orig_noNA[is.na(data_matrix_orig_noNA)] <- -999          # dummy for NA

mask_matrix_orig       <- ifelse(is.na(sim_wide), 0L, 1L)

SigmaE_matrix_orig     <- as.matrix(Sigma.E)                          # reuse ΣE
diag_fortran_orig      <- as.double(diag(SigmaE_matrix_orig))

# Scaling factors from Compbounds (column 9)
bounds_list_orig <- lapply(colnames(sim_wide), function(colname) {
  paper_code <- sub("_.*", "", colname)
  lvl        <- sub(".*_([A-Z]{2})_\\d+$", "\\1", colname)
  Compbounds %>% filter(PAPER_CODE == paper_code, LVL == lvl)
})
scaleFactor_vector_orig <- sapply(bounds_list_orig, \(x) x[[9]][1])


# 7.2  Create subject-group vector & flatten grade doundaries ---------

col_info_orig <- tidyr::separate(
  tibble(col_name = colnames(sim_wide)),
  col_name,
  into = c("Code", "Subject", "Lvl", "Timezone"),
  sep  = "_"
)

subject_group_orig <- paste(col_info_orig$Subject,
                            col_info_orig$Lvl,
                            col_info_orig$Timezone,
                            sep = "_")
group_vector_orig  <- as.integer(factor(subject_group_orig,
                                        levels = unique(subject_group_orig)))

# One row per distinct subject group
col_info_trunc_orig  <- col_info_orig %>%
  distinct(across(c(Subject, Lvl, Timezone)), .keep_all = TRUE)
unique_subjects_orig <- paste(col_info_trunc_orig$Subject,
                              col_info_trunc_orig$Lvl,
                              col_info_trunc_orig$Timezone,
                              sep = "_")

# Helper: convert A–E → 5–1, else numeric passthrough
convertGrade <- function(x) {
  if (is.na(as.numeric(x))) {
    grade_map <- c(A = 5, B = 4, C = 3, D = 2, E = 1)
    grade_map[x]
  } else as.numeric(x)
}

grade_bounds_list_orig <- lapply(seq_len(nrow(col_info_trunc_orig)), function(i) {
  lvl  <- col_info_trunc_orig$Lvl[i]
  subj <- col_info_trunc_orig$Subject[i]
  tz   <- col_info_trunc_orig$Timezone[i]
  code <- col_info_trunc_orig$Code[i]
  
  if (lvl != "EE") {
    Subbounds %>% filter(LVL == lvl,
                         SUBJECT_OPTION == subj,
                         SUBJECT_TIMEZONE == tz)
  } else {
    bounds <- Compbounds %>% filter(PAPER_CODE == code) %>% unique()
    bounds <- bounds[nrow(bounds):1, ]                              # reverse for EE
    colnames(bounds)[c(4, 6)] <- c("SUBJECT_GRADE",
                                   "NEW_SUBJECT_UPPER_BOUNDARY")
    bounds
  }
})

names(grade_bounds_list_orig) <- unique_subjects_orig

nBound_vector_orig <- sapply(grade_bounds_list_orig, nrow) - 1       # minus 1 → Fortran logic
nBoundMax_orig     <- max(nBound_vector_orig + 1)
nGroups_orig       <- length(unique_subjects_orig)

bound_ub_matrix_orig  <- matrix(-999, nrow = nBoundMax_orig, ncol = nGroups_orig)
grade_val_matrix_orig <- matrix(-999, nrow = nBoundMax_orig, ncol = nGroups_orig)

for (i in seq_along(unique_subjects_orig)) {
  bounds <- grade_bounds_list_orig[[i]]
  nB     <- nrow(bounds)
  bound_ub_matrix_orig [1:nB, i] <- as.numeric(bounds$NEW_SUBJECT_UPPER_BOUNDARY)
  grade_val_matrix_orig[1:nB, i] <- sapply(bounds$SUBJECT_GRADE, convertGrade)
}

bound_ub_vector_orig   <- as.double(bound_ub_matrix_orig)
grade_val_vector_orig  <- as.double(grade_val_matrix_orig)
nBound_vector_int_orig <- as.integer(nBound_vector_orig)
nBoundMax_int_orig     <- as.integer(nBoundMax_orig)


# 7.3  Bonus mapping, HL/SL/TK/EE flags ------------------------------

# (The bonus-code and tokee matrices you built in Step 5 can be reused.)
tk_ee_flag_orig <- as.integer(col_info_trunc_orig$Lvl %in% c("TK", "EE"))
hl_flag_orig    <- as.integer(col_info_trunc_orig$Lvl == "HL")
sl_flag_orig    <- as.integer(col_info_trunc_orig$Lvl == "SL")


# 7.4  Wrap everything for the Fortran call --------------------------

nrows_orig <- nrow(data_matrix_orig)
ncols_orig <- ncol(data_matrix_orig)

data_fortran_orig      <- as.double(data_matrix_orig_noNA)
mask_fortran_orig      <- as.integer(mask_matrix_orig)
scale_fortran_orig     <- as.double(scaleFactor_vector_orig)
group_fortran_orig     <- as.integer(group_vector_orig)
nSim                   <- 1000      # (same as before)

# Output placeholders (mirrors of Step 5)
totals_fortran_orig          <- double(nrows_orig * nGroups_orig)
finalGrades_fortran_orig     <- double(nrows_orig * nGroups_orig)
passFlag_fortran_orig        <- integer(nrows_orig)
student_totals_fortran_orig  <- double(nrows_orig)
ierr_fortran_orig            <- integer(1)
tk_ee_count_fortran_orig     <- integer(nrows_orig)
failReasonsReal_vec_orig     <- integer(nrows_orig * 7)
componentScoresReal_vec_orig <- double(nrows_orig * ncols_orig)
groupTotalsReal_vec_orig     <- double(nrows_orig * nGroups_orig)
subjectGradesReal_vec_orig   <- double(nrows_orig * nGroups_orig)
mismatchStorage_orig         <- matrix(0L, nrow = nSim, ncol = 7,
                                       dimnames = list(NULL, paste0("Cond", 1:7)))
reasonMatrix_list_orig       <- vector("list", nSim)
scratch_totals        <- double(nrows_orig * nGroups_orig)
scratch_finalGrades   <- double(nrows_orig * nGroups_orig)
scratch_compScores    <- double(nrows_orig * ncols_orig)
scratch_groupTotals   <- double(nrows_orig * nGroups_orig)
scratch_subjectGrades <- double(nrows_orig * nGroups_orig)
scratch_failReasons   <- integer(nrows_orig * 7)