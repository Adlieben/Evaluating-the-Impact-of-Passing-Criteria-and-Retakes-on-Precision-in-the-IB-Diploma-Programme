# Increment distribution is determined here then added to the preparation file
# 1 LOAD the data and libraries----
library(tidyverse)
library(psych) # For alpha and omega
library(mice) # To impute missing grades
library(mirt) # For marginal reliability
library(MBESS) # For the GLB
library(Rcsdp) # Additional requirement for the GLB
library(purrr)
library(openxlsx)
library(caret)
library(data.table)
library(moments) # Skewness and kurtosis

# Read the data
data <- read.csv('A. Data/A. May Data/M23 component data.csv', header = TRUE, sep=",")
data_n <- read.csv('A. Data/C. November Data/N23 component Data.csv')
Compbounds <- fread('A. Data/A. May Data/M23 component grade boundaries.csv')
Subbounds <- read.csv('A. Data/A. May Data/M23 subject boundaries.csv')
Novcompbounds <- fread('A. Data/C. November Data/N23 component grade boundaries.csv')
Novsubbounds <- read.csv('A. Data/C. November Data/N23 subject boundaries.csv')
Realfailreasons <- fread('D. Results/A. Intermediate/Realfailreasons.csv')[,-1]
colnames(Realfailreasons)[1] <- 'CANDIDATE'

# 1.1 Data CLEANUP May 2023 ----
# Find associated codes for each subject and add them to the bounds file
dp_data <- data %>% filter(CATEGORY %in% c('DIPLOMA', 'COURSE', 'ANTICIPATED', 'RETAKE'))
codes <- dp_data %>% dplyr::select(PAPER_CODE, SUBJECT_OPTION) %>% unique() %>%
  mutate(PAPER_CODE = strtrim(PAPER_CODE, 5)) %>% unique()
Subbounds <- Subbounds %>%
  left_join(codes %>% dplyr::select(SUBJECT_OPTION, PAPER_CODE), by = "SUBJECT_OPTION")

# Add the TK bounds that are not present for some reason
tk_boundaries <- data.frame(
  SUBJECT_OPTION = rep("THEORY KNOWL.", 15),
  LVL = rep("TK", 15),
  SUBJECT_TIMEZONE = rep(0:2, each = 5),
  SUBJECT_GRADE = rep(c("E", "D", "C", "B", "A"), 3),
  OLD_SUBJECT_UPPER_BOUNDARY = rep(c(0, 4, 10, 16, 22), 3),
  NEW_SUBJECT_UPPER_BOUNDARY = rep(c(0, 4, 10, 16, 22), 3),
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
  filter(n_subject_lvl >= 8) %>%
  select(CANDIDATE)

sim_data <- dp_data %>%
  semi_join(filtered_candidates, by = "CANDIDATE") %>%
  dplyr::select(CANDIDATE, PAPER_CODE, MODERATED_MARK, SUBJECT_OPTION, LVL, SUBJECT_TIMEZONE, RESULT_CODE, TOTAL_POINTS) %>% unique() %>% 
  filter(!is.na(MODERATED_MARK))

# Separate level for later
sim_data <- sim_data %>% 
  mutate(test = paste0(PAPER_CODE, "_", 
                       SUBJECT_OPTION, "_", LVL, "_", SUBJECT_TIMEZONE)) %>%
  select(-c(PAPER_CODE, SUBJECT_OPTION, LVL, SUBJECT_TIMEZONE))

sim_wide_0 <- sim_data %>% pivot_wider(names_from = c(test), values_from = MODERATED_MARK)

sim_wide <- sim_wide_0 %>% dplyr::select(-CANDIDATE)

# 1.2 Data CLEANUP November 2023 ----
# Remove useless column
data_nov <- data_n
data_nov <- data_nov[data_nov$CANDIDATE %in% sim_data$CANDIDATE, ] # to maintain complete records

sim_nov <- data_nov %>%
  dplyr::select(CANDIDATE, PAPER_CODE, MODERATED_MARK, SUBJECT_OPTION, LVL, SUBJECT_TIMEZONE) %>% unique() %>% 
  filter(!is.na(MODERATED_MARK))

# Separate level for later
sim_nov <- sim_nov %>% 
  mutate(test = paste0(PAPER_CODE, "_", 
                       SUBJECT_OPTION, "_", LVL, "_", SUBJECT_TIMEZONE)) %>%
  select(-c(PAPER_CODE, SUBJECT_OPTION, LVL, SUBJECT_TIMEZONE))

sim_nov_0 <- sim_nov %>% pivot_wider(names_from = c(test), values_from = MODERATED_MARK)

sim_nov <- sim_nov_0 %>% dplyr::select(-CANDIDATE)

# 2. DESCRIPTIVES ----
# First very naive estimate of people who passed on retake
canddec_n <- data_nov %>% select(CANDIDATE, RESULT_CODE) %>% unique()
table(canddec_n$RESULT_CODE) # so still about half the retakers did not pass
# IMPORTANT: 2554 students retook in November from the original dataset

# Now proportion that retook a subject after failing
canddec <- dp_data %>%
  semi_join(filtered_candidates, by = "CANDIDATE") %>%
  dplyr::select(CANDIDATE, RESULT_CODE) %>% unique()
negdec <- canddec %>% filter(RESULT_CODE == 'F')
posdec <- canddec %>% filter(RESULT_CODE != 'F')

# Get the desired proportions
prop <- sum(canddec_n$CANDIDATE %in% negdec$CANDIDATE)/nrow(canddec_n) # Proportion of students who retook that failed originally
prop
prop <- sum(canddec_n$CANDIDATE %in% posdec$CANDIDATE)/nrow(canddec_n) # Proportion of students who retook that succeeded originally
prop # so most people who retook did fail originally

# Investigate original scores of people who retook
original_scores <- dp_data %>% filter(CANDIDATE %in% canddec_n$CANDIDATE)
original_scores_score <- original_scores %>% select(CANDIDATE, TOTAL_POINTS, RESULT_CODE) %>% unique()
original_scores_score <- original_scores_score %>% 
  left_join(data_nov %>% 
              select(CANDIDATE, TOTAL_POINTS, RESULT_CODE) %>% 
              rename(NEW_POINTS = TOTAL_POINTS, NEW_CODE = RESULT_CODE),
            by = "CANDIDATE") %>% unique()
original_scores_score$DIFF <- original_scores_score$NEW_POINTS - original_scores_score$TOTAL_POINTS # Change in points
# I've investigated some of the records here to see how people chose to take retakes

# 275337 decides to retake an exam but doesn't turn up so gets docked the points they would have gotten?
# Also why would they retake Spanish when Chemistry is clearly what let them down?
# 100222 fails due to HL condition only, retakes 2 HL subjects, improves in 1 and that is sufficient
# 270404 didn't take most of their subjects the first time, so their improvement is due to this
# A lot of candidates have pending results originally, which is misleading, could ask Antony how that is resolved
# e.g. are students' records then added to the next session? Where are they recorded?
# 147693 didn't take their SL subjects the first time around but corrected this thereafter
# 101004's total does not correspond at all to the sum of subject grades

# How many candidates who failed and took a retake still failed?
t <- table(Original = original_scores_score$RESULT_CODE, New = original_scores_score$NEW_CODE)
sum(t[3,1:2])/sum(t[3,1:3]) # Desired proportion from table

# How many students passed then, including retake?
decision_retake <- decision
decision_retake <- merge(decision_retake, original_scores_score[, c("CANDIDATE", "NEW_CODE")], 
                         by = "CANDIDATE", all.x = TRUE)
decision_retake$RESULT_CODE <- ifelse(!is.na(decision_retake$NEW_CODE), 
                                      decision_retake$NEW_CODE, 
                                      decision_retake$RESULT_CODE)
decision_retake$NEW_CODE <- NULL
sum(decision_retake$RESULT_CODE %in% c('B', 'D'))/nrow(decision_retake)

# How many subjects did student retake on average?
retake_subs <- data_nov %>%
  select(CANDIDATE, SUBJECT_OPTION) %>%
  distinct() %>%
  count(CANDIDATE, name = "num_subjects")
mean(retake_subs$num_subjects)
sd(retake_subs$num_subjects)

# Which subjects were generally retaken?
retake_subs <- data_nov %>%
  select(CANDIDATE, SUBJECT_OPTION, LVL, GROUP_NO) %>%
  distinct()
table(retake_subs$LVL)
table(retake_subs$GROUP_NO)
length(retake_subs$LVL)

# What was the mean total for retakers?
mean(original_scores_score$NEW_POINTS, na.rm = T)
sd(original_scores_score$NEW_POINTS, na.rm = T)

# What was the mean per subject for retakers?
sub_groups <- data_nov %>% distinct(CANDIDATE, SUBJECT_GRADE, SUBJECT_OPTION)
mean(as.numeric(sub_groups$SUBJECT_GRADE), na.rm = T)
sd(as.numeric(sub_groups$SUBJECT_GRADE), na.rm = T)
grade_map <- c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5)
grade_groups <- data_nov %>%
  select(CANDIDATE, GROUP_NO, SUBJECT_OPTION, SUBJECT_GRADE) %>%
  distinct() %>%
  filter(SUBJECT_GRADE %in% names(grade_map)) %>%
  mutate(SUBJECT_GRADE_NUM = grade_map[SUBJECT_GRADE])
summary(grade_groups$SUBJECT_GRADE_NUM)

# How many components, subjects and languages were retaken?
nrow(data_nov %>% distinct(PAPER_CODE))
nrow(data_nov %>% distinct(SUBJECT_OPTION))
table(data_nov %>% select(LANGUAGE))

# Proportion of students who failed who took a retake
prop <- sum(canddec_n$CANDIDATE %in% negdec$CANDIDATE)/nrow(negdec)
prop # Very low compared to expectations, let's see the people who failed but didn't retake

# Check the previous candidates
candidates_in_data_nov <- dp_data %>% filter(RESULT_CODE == 'F') %>%
  semi_join(filtered_candidates, by = "CANDIDATE") %>% # For complete records
  semi_join(data_nov, by = "CANDIDATE") %>% 
  select(CANDIDATE, SUBJECT, LVL, SUBJECT_GRADE, TOTAL_POINTS, RESULT_CODE, GENDER, LANGUAGE1, REGIONAL_OFFICE, GROUP_NO) %>% unique() #Better to investigate

# Candidates who are NOT in data_nov (non-matching candidates)
candidates_not_in_data_nov <- dp_data %>% filter(RESULT_CODE == 'F') %>%
  semi_join(filtered_candidates, by = "CANDIDATE") %>% # For complete records
  anti_join(data_nov, by = "CANDIDATE") %>% 
  select(CANDIDATE, SUBJECT, LVL, SUBJECT_GRADE, TOTAL_POINTS, RESULT_CODE, GENDER, LANGUAGE1, REGIONAL_OFFICE, GROUP_NO) %>% unique()
# Looks like a lot had not done enough subjects in May, data is a mess
# If the candidate did take the required number of subjects, it should be 8 rows
# 100117 may have thought they were too far away from a pass with 17

# Does the original total affect odds of retaking? Let's see:
# Combine both datasets
candidates_in_data_nov <- candidates_in_data_nov %>% mutate(Group = "Retook Exam")
candidates_not_in_data_nov <- candidates_not_in_data_nov %>% mutate(Group = "Did Not Retake")
combined_data <- bind_rows(candidates_in_data_nov, candidates_not_in_data_nov) %>% unique()
# So few decided to retake

# Plot
ggplot(combined_data, aes(x = TOTAL_POINTS, fill = Group)) + # Noticeable difference here
  geom_density(alpha = 0.3) + # Density plot with transparency
  labs(title = "Distribution of Total Points",
       x = "Total Points",
       y = "Density",
       fill = "Candidate Group") +
  theme_minimal()



# 3. DEVISE a distribution of score improvements for components ----
# 3.1 Add previous score to data_nov ----
data_old <- data %>%
  group_by(CANDIDATE, SUBJECT_OPTION, COMPONENT) %>%
  slice(1) %>%    # keep the first row per group
  ungroup() %>%
  select(CANDIDATE, SUBJECT_OPTION, COMPONENT, MODERATED_MARK) %>%
  rename(old_component_score = MODERATED_MARK)

data_nov <- data_nov %>%
  left_join(data_old, by = c("CANDIDATE", "SUBJECT_OPTION", "COMPONENT"))

# 3.2 Turning scores into proportions of 100 to measure score increase ----
grade_bands_TKEE <- seq(0, 100, length.out = 6)
grade_bands_0_100 <- seq(0, 100, length.out = 8)
make_comp_list <- function(compbounds_dt) {
  split(compbounds_dt, by = "PAPER_CODE", keep.by = FALSE)
}
comp_list_May <- make_comp_list(Compbounds)
comp_list_Nov <- make_comp_list(Novcompbounds)
score_to_100piecewise <- function(raw_score, paper_code, comp_list) {
  if (!paper_code %in% names(comp_list)) {
    return(NA_real_)
  }
  
  dt_bounds <- comp_list[[paper_code]]
  
  if (!("November" %in% names(dt_bounds))){ # Account for the N23 bounds not having LVL
  if (unique(dt_bounds$LVL)[1] %in% c("EE", "TK")) {
    # For EE, sort boundaries in descending order
    dt_bounds <- dt_bounds[nrow(dt_bounds):1,]
  } else {
    # For other levels, sort in ascending order
    dt_bounds <- dt_bounds
  }
  }
  
  # Remove duplicate bounds
  if (nrow(dt_bounds) == 10){
    dt_bounds <- dt_bounds[1:5,]
  }
  
  if (nrow(dt_bounds) %in% c(14,21,28,35,42)){
    dt_bounds <- dt_bounds[1:7,]
  }
  
  dt_bounds$GRADETOMARK <- dt_bounds$GRADETOMARK + 0.000001 # to conserve logic of equality direction
  
  # If raw_score is NA, return NA
  if (is.na(raw_score)) return(NA_real_)

  g_index <- findInterval(raw_score, dt_bounds$GRADETOMARK, left.open = FALSE) + 1
  if (g_index < 1) g_index <- 1
  if (g_index > 7) g_index <- 7
  
  upper_bound <- dt_bounds$GRADETOMARK[g_index]
  lower_bound <- if (g_index == 1) 0 else dt_bounds$GRADETOMARK[g_index - 1]
  
  denom <- (upper_bound - lower_bound)
  if (!is.na(denom)){
  if (denom < 1e-9) denom <- 1e-9  # avoid zero denominators
  frac <- (raw_score - lower_bound) / denom
  if (frac < 0) frac <- 0
  if (frac > 1) frac <- 1
  } else {
    frac <- NA
  }
  if (nrow(dt_bounds) == 7){
  band_low <- grade_bands_0_100[g_index]     # e.g. if g_index=3 => ~28.57
  band_high <- grade_bands_0_100[g_index+1]  # e.g. if g_index=3 => ~42.86
  sub_width <- band_high - band_low  # ~14.2857
  } else { # When it's TK or EE
  band_low <- grade_bands_TKEE[g_index]
  band_high <- grade_bands_TKEE[g_index+1]
  sub_width <- band_high - band_low
  }
  
  # 5e) final 0-100 score
  final_100 <- band_low + sub_width * frac
  final_100
}

data_nov <- as.data.table(data_nov)
dp_data <- as.data.table(dp_data)

dp_data[, score_100 := mapply(
  FUN = score_to_100piecewise,
  raw_score = MODERATED_MARK,
  paper_code = PAPER_CODE,
  MoreArgs = list(comp_list = comp_list_May)
)]

data_nov[, score_100_new := mapply(
  FUN = score_to_100piecewise,
  raw_score = MODERATED_MARK,
  paper_code = PAPER_CODE,
  MoreArgs = list(comp_list = comp_list_Nov)
)]

data_nov[, score_100_old := mapply(
  FUN = score_to_100piecewise,
  raw_score = old_component_score,
  paper_code = PAPER_CODE,
  MoreArgs = list(comp_list = comp_list_May)
)]

# Summary of component scores out of 100
mean(data_nov$score_100_new, na.rm = T)
sd(dp_nov$score_100_new, na.rm = T)
mean(dp_data$score_100, na.rm = T)
sd(dp_data$score_100, na.rm = T)

# 3.3 Getting the distribution of changes ----
data_nov[, score_diff := score_100_new - score_100_old]
data_nov_exam <- data_nov %>% filter(ASSESSMENT_METHOD == 'EXAM')

# Get summary statistics for the difference
summary(data_nov$score_diff) # A lot of NAs, normal since marks are missing everywhere
sd(data_nov$score_diff, na.rm = T)

data_nov_sub <- data_nov[score_diff >= -20 & score_diff <= 20]

# Plot the histogram and density for the subset
ggplot(data_nov_sub, aes(x = score_diff)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Score Differences (New - Old) [Subset -20 to 20]",
       x = "Score Difference",
       y = "Density")

# Kolmogorov–Smirnov test
ks.test(scale(data_nov_sub$score_diff), "pnorm")

# QQ-plot
qqnorm(data_nov_sub$score_diff)

# Skewness and curtosis
skewness(data_nov_sub$score_diff, na.rm = TRUE)
kurtosis(data_nov_sub$score_diff, na.rm = TRUE)

# 4. SAVE score differences ----
write.csv(data_nov$score_diff, 'D. Results/A. Intermediate/100differences.csv')
