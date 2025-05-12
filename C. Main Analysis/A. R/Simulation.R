# 1. Load preparation data ----
# The main preparation steps are kept in a separate document
# Please run it before running this one with the following function
source("C. Main Analysis/A. R/Preparation.R")

# 2. Test rules ----
############################
# 2.1 Current rules ----
## 2.1.1 Initial ----
try(dyn.load("C. Main Analysis/B. Fortran/simulation_module_base.dll"), silent = T)
true_out <- .Fortran("compute_true_passFlag",
                 data         = as.double(data_matrix_noNA),
                 diagVar      = as.double(diag(SigmaE_matrix)),
                 scaleFactor  = as.double(scaleFactor_vector),
                 mask         = as.integer(mask_fortran),
                 group        = as.integer(group_fortran),
                 nrows        = as.integer(nrow(data_matrix_noNA)),
                 ncols        = as.integer(ncols),
                 nGroups      = as.integer(nGroups),
                 nBound       = as.integer(nBound_vector),
                 nBoundMax    = as.integer(nBoundMax_int),
                 bound_ub     = as.double(bound_ub_vector),
                 grade_val    = as.double(grade_val_vector),
                 totals       = double(nrow(data_matrix_noNA) * nGroups),
                 finalGrades  = double(nrow(data_matrix_noNA) * nGroups),
                 tk_ee_flag   = as.integer(tk_ee_flag),
                 hl_flag      = as.integer(hl_flag),
                 sl_flag      = as.integer(sl_flag),
                 bonus_length = as.integer(bonus_length),
                 bonus_codes  = as.integer(bonus_codes_vector),
                 bonus_points = as.integer(bonus_points_vector),
                 observed_student_totals = double(nrow(data_matrix_noNA)),
                 observed_hl_totals     = double(nrow(data_matrix_noNA)),
                 observed_sl_totals     = double(nrow(data_matrix_noNA)),
                 observed_passFlag      = integer(nrow(data_matrix_noNA)),
                 failReasonsReal        = as.integer(failReasonsReal_vec),
                 componentScoresReal    = as.double(componentScoresReal_vec),
                 groupTotalsReal        = as.double(groupTotalsReal_vec),
                 subjectGradesReal      = as.double(subjectGradesReal_vec),
                 ierr                   = integer(1))

if (true_out$ierr != 0) stop("Error in compute_true_passFlag")

# Store output
observed_passFlag <- true_out$observed_passFlag
write.csv(observed_passFlag, 'D. Results/A. Intermediate/observed_passFlag.csv')
observed_passFlag_df <- as.data.frame(observed_passFlag)
failReasonsReal_mat <- matrix(true_out$failReasonsReal, nrow=nrow(data_matrix_noNA), ncol=7, byrow=FALSE)
failReasonReal_df <- as.data.frame(failReasonsReal_mat)
componentScoresReal_vec <- matrix(true_out$componentScoresReal, nrow=nrow(data_matrix_noNA), byrow=FALSE)
componentScoresReal_df <- as.data.frame(componentScoresReal_vec)
subjectGradesReal_mat <- matrix(true_out$subjectGradesReal,   nrow=nrow(data_matrix_noNA), ncol=nGroups, byrow=FALSE)
write.csv(as.data.frame(cbind(decision$CANDIDATE, failReasonsReal_mat)), 'D. Results/A. Intermediate/Realfailreasons.csv')

# Check the passflag accuracy and fail per condition
colSums(failReasonsReal_mat)
table(observed_passFlag_df$observed_passFlag, decision$RESULT_CODE)
decision <- cbind(decision, Observed = observed_passFlag_df$observed_passFlag)

# Create an empty 7x7 matrix to hold the intersection counts:
M <- matrix(0, nrow = 7, ncol = 7)

# Loop over each condition (columns 1 to 7)
for(i in 1:7) {
  for(j in 1:7) {
    if(i == j) {
      # Diagonal: count only students who failed ONLY condition i.
      M[i, i] <- sum(failReasonsReal_mat[, i] == 1 & rowSums(failReasonsReal_mat) == 1)
    } else {
      if (i < j){
      # Off-diagonals: count students who failed both condition i and condition j
      M[i, j] <- sum(failReasonsReal_mat[, i] == 1 & failReasonsReal_mat[, j] == 1)
      }
    }
  }
}
dimnames(M) <- list(paste0("Cond", 1:7), paste0("Cond", 1:7))

# Compute pass rate and accuracy
sum(observed_passFlag)/length(observed_passFlag) # Kelley's estimates
sum(decision$RESULT_CODE %in% c("B", "D"))/length(decision$RESULT_CODE)

## 2.1.2 With error ----
# Set Seed
out <- .Fortran("init_fortran_seed",
                ierr = as.integer(0))

for (r in seq_len(nSim)) {
  # Prepare space for failReasonsObs in Fortran call
  failReasonsObs_vec <- integer(nrow(data_matrix_noNA) * 7)
  reasonDiff_vec     <- integer(7)
  
  out <- .Fortran("add_error_scale_total_and_assign",
                  data        = as.double(data_matrix_noNA),
                  diagVar     = as.double(diag(SigmaE_matrix)),
                  scaleFactor = as.double(scaleFactor_vector),
                  mask        = as.integer(mask_fortran),
                  group       = as.integer(group_fortran),
                  nrows       = as.integer(nrow(data_matrix_noNA)),
                  ncols       = as.integer(ncols),
                  nGroups     = as.integer(nGroups),
                  nBound      = as.integer(nBound_vector),
                  nBoundMax   = as.integer(nBoundMax_int),
                  bound_ub    = as.double(bound_ub_vector),
                  grade_val   = as.double(grade_val_vector),
                  tk_ee_flag  = as.integer(tk_ee_flag),
                  hl_flag     = as.integer(hl_flag),
                  sl_flag     = as.integer(sl_flag),
                  bonus_length= as.integer(bonus_length),
                  bonus_codes = as.integer(bonus_codes_vector),
                  bonus_points= as.integer(bonus_points_vector),
                  observed_passFlag = as.integer(true_out$observed_passFlag),
                  failReasonsReal   = as.integer(failReasonsReal_mat),
                  failReasonsObs    = as.integer(failReasonsObs_vec),
                  reasonDiff        = as.integer(reasonDiff_vec),
                  sensitivity       = double(1),
                  specificity       = double(1),
                  ppv               = double(1),
                  npv               = double(1),
                  accuracy          = double(1),
                  reasonMatrix      = integer(7 * 7),
                  ierr              = integer(1))
  
  if (out$ierr != 0) stop("Error in add_error_scale_total_and_assign, ierr=", out$ierr)
  
  # Extract the 5 metrics:
  results_mat[r, ] <- c(out$sensitivity, out$specificity, out$ppv, out$npv, out$accuracy)
  
  mismatchStorage[r, ] <- out$reasonDiff
  
  reasonMatrix_list[[r]] <- matrix(out$reasonMatrix, nrow = 7, ncol = 7, byrow = FALSE)
}

## 2.1.3 Investigate ----
# View the mismatch means
colMeans(mismatchStorage)

# Look at matrix of overlapping flag changes, done only once
reasonMatrix_list

# Compute average metrics:
colMeans(results_mat)

# Put in a data frame for convenience:
results_df <- as.data.frame(results_mat)
names(results_df) <- c("sens","spec","ppv","npv","acc")

# Save results
write.csv(results_df, "D. Results/B. Main/A. Base/results_base.csv", row.names=FALSE)

# Inspect distribution of accuracy:
ggplot(results_df, aes(x=acc)) +
  geom_histogram(bins=25, fill="lightblue", color="black") +
  labs(title="Distribution of Accuracy Across 1000 Repli# cates",
       x="Accuracy", y="Count")

summary(results_df$acc)

############################
# 2.2 Rule 1 ----
## 2.2.1 Initial ----
try(dyn.unload("C. Main Analysis/B. Fortran/simulation_module_base.dll"), silent = T)
dyn.load("C. Main Analysis/B. Fortran/simulation_module_rule1.dll")

# We'll define new vectors to hold the outputs specific to Rule 1
componentScoresReal_rule1_vec <- double(nrow(data_matrix_noNA) * ncols)
groupTotalsReal_rule1_vec     <- double(nrow(data_matrix_noNA) * nGroups)
subjectGradesReal_rule1_vec   <- double(nrow(data_matrix_noNA) * nGroups)
failReasonsReal_rule1_vec     <- integer(nrow(data_matrix_noNA) * 7)

true_out_rule1 <- .Fortran("compute_true_passFlag1",
                     data         = as.double(data_matrix_noNA),
                     diagVar      = as.double(diag(SigmaE_matrix)),
                     scaleFactor  = as.double(scaleFactor_vector),
                     mask         = as.integer(mask_fortran),
                     group        = as.integer(group_fortran),
                     nrows        = as.integer(nrow(data_matrix_noNA)),
                     ncols        = as.integer(ncols),
                     nGroups      = as.integer(nGroups),
                     nBound       = as.integer(nBound_vector),
                     nBoundMax    = as.integer(nBoundMax_int),
                     bound_ub     = as.double(bound_ub_vector),
                     grade_val    = as.double(grade_val_vector),
                     totals       = double(nrow(data_matrix_noNA) * nGroups),
                     finalGrades  = double(nrow(data_matrix_noNA) * nGroups),
                     tk_ee_flag   = as.integer(tk_ee_flag),
                     hl_flag      = as.integer(hl_flag),
                     sl_flag      = as.integer(sl_flag),
                     bonus_length = as.integer(bonus_length),
                     bonus_codes  = as.integer(bonus_codes_vector),
                     bonus_points = as.integer(bonus_points_vector),
                     observed_student_totals = double(nrow(data_matrix_noNA)),
                     observed_hl_totals     = double(nrow(data_matrix_noNA)),
                     observed_sl_totals     = double(nrow(data_matrix_noNA)),
                     observed_passFlag      = integer(nrow(data_matrix_noNA)),
                     failReasonsReal        = as.integer(failReasonsReal_vec),
                     componentScoresReal    = as.double(componentScoresReal_vec),
                     groupTotalsReal        = as.double(groupTotalsReal_vec),
                     subjectGradesReal      = as.double(subjectGradesReal_vec),
                     ierr                   = integer(1))

if (true_out_rule1$ierr != 0) stop("Error in compute_true_passFlag (Rule 1)")

# Reshape or store the real fail reasons for Rule 1
failReasonsReal_rule1_mat <- matrix(true_out_rule1$failReasonsReal, nrow=nrow(data_matrix_noNA), ncol=7, byrow=FALSE)
observed_passFlag_rule1   <- true_out_rule1$observed_passFlag
write.csv(observed_passFlag_rule1, 'D. Results/A. Intermediate/observed_passFlag_rule1.csv')

# cat("Fail reasons (Rule 1) sum:\n")
print(colSums(failReasonsReal_rule1_mat))

# cat("Table of pass flags vs. decision codes (Rule 1):\n")
table(observed_passFlag_rule1, decision$RESULT_CODE)

## 2.2.2 With error ----
# We'll store repli# cate results in unique objects for Rule 1
results_mat_rule1       <- matrix(NA_real_, nrow=nSim, ncol=5)
mismatchStorage_rule1   <- matrix(0L,        nrow=nSim, ncol=7)
colnames(results_mat_rule1) <- c("sens","spec","ppv","npv","acc")

# Seed
init_out <- .Fortran("init_fortran_seed1", ierr=integer(1))
if (init_out$ierr != 0) stop("Error seeding RNG (Rule 1)")

for (r in seq_len(nSim)) {
  failReasonsObs_rule1_vec <- integer(nrow(data_matrix_noNA) * 7)
  reasonDiff_rule1_vec     <- integer(7)
  
  out_rule1 <- .Fortran("add_error_scale_total_and_assign1",
                        data            = as.double(data_matrix_noNA),
                        diagVar         = as.double(diag(SigmaE_matrix)),
                        scaleFactor     = as.double(scaleFactor_vector),
                        mask            = as.integer(mask_fortran),
                        group           = as.integer(group_fortran),
                        nrows           = as.integer(nrow(data_matrix_noNA)),
                        ncols           = as.integer(ncols),
                        nGroups         = as.integer(nGroups),
                        nBound          = as.integer(nBound_vector),
                        nBoundMax       = as.integer(nBoundMax_int),
                        bound_ub        = as.double(bound_ub_vector),
                        grade_val       = as.double(grade_val_vector),
                        tk_ee_flag      = as.integer(tk_ee_flag),
                        hl_flag         = as.integer(hl_flag),
                        sl_flag         = as.integer(sl_flag),
                        bonus_length    = as.integer(bonus_length),
                        bonus_codes     = as.integer(bonus_codes_vector),
                        bonus_points    = as.integer(bonus_points_vector),
                        observed_passFlag = as.integer(observed_passFlag_rule1),
                        failReasonsReal   = as.integer(failReasonsReal_rule1_mat),
                        failReasonsObs    = as.integer(failReasonsObs_rule1_vec),
                        reasonDiff        = as.integer(reasonDiff_rule1_vec),
                        sensitivity       = double(1),
                        specificity       = double(1),
                        ppv               = double(1),
                        npv               = double(1),
                        accuracy          = double(1),
                        ierr              = integer(1))
  
  if (out_rule1$ierr != 0) stop("Error in add_error_scale_total_and_assign, ierr=", out_rule1$ierr)
  
  # Store classifi# cation metrics
  results_mat_rule1[r, ] <- c(out_rule1$sensitivity, out_rule1$specificity, 
                              out_rule1$ppv,        out_rule1$npv, 
                              out_rule1$accuracy)
  
  # Store mismatch
  mismatchStorage_rule1[r, ] <- out_rule1$reasonDiff
}

## 2.2.3 Investigate ----
# cat("Mismatch means (Rule 1):\n")
print(colMeans(mismatchStorage_rule1))

# cat("Average metrics (Rule 1):\n")
print(colMeans(results_mat_rule1))

results_df_rule1 <- as.data.frame(results_mat_rule1)
names(results_df_rule1) <- c("sens","spec","ppv","npv","acc")

write.csv(results_df_rule1, "D. Results/B. Main/A. Base/results_rule1.csv", row.names=FALSE)

ggplot(results_df_rule1, aes(x=acc)) +
  geom_histogram(bins=25, fill="lightblue", color="black") +
  labs(title="Distribution of Accuracy Across 1000 Repli# cates (Rule 1)",
       x="Accuracy", y="Count")

summary(results_df_rule1$acc)


############################
# 2.3 Rule 2 ----
## 2.3.1 Initial ----
# dyn.unload("C. Main Analysis/B. Fortran/simulation_module_rule2.dll")
try(dyn.unload("C. Main Analysis/B. Fortran/simulation_module_rule1.dll"), silent = T)
dyn.load("C. Main Analysis/B. Fortran/simulation_module_rule2.dll")

componentScoresReal_rule2_vec <- double(nrow(data_matrix_noNA) * ncols)
groupTotalsReal_rule2_vec     <- double(nrow(data_matrix_noNA) * nGroups)
subjectGradesReal_rule2_vec   <- double(nrow(data_matrix_noNA) * nGroups)
failReasonsReal_rule2_vec     <- integer(nrow(data_matrix_noNA) * 7)

true_out_rule2 <- .Fortran("compute_true_passFlag2",
                           data         = as.double(data_matrix_noNA),
                           diagVar      = as.double(diag(SigmaE_matrix)),
                           scaleFactor  = as.double(scaleFactor_vector),
                           mask         = as.integer(mask_fortran),
                           group        = as.integer(group_fortran),
                           nrows        = as.integer(nrow(data_matrix_noNA)),
                           ncols        = as.integer(ncols),
                           nGroups      = as.integer(nGroups),
                           nBound       = as.integer(nBound_vector),
                           nBoundMax    = as.integer(nBoundMax_int),
                           bound_ub     = as.double(bound_ub_vector),
                           grade_val    = as.double(grade_val_vector),
                           totals       = double(nrow(data_matrix_noNA) * nGroups),
                           finalGrades  = double(nrow(data_matrix_noNA) * nGroups),
                           tk_ee_flag   = as.integer(tk_ee_flag),
                           hl_flag      = as.integer(hl_flag),
                           sl_flag      = as.integer(sl_flag),
                           bonus_length = as.integer(bonus_length),
                           bonus_codes  = as.integer(bonus_codes_vector),
                           bonus_points = as.integer(bonus_points_vector),
                           observed_student_totals = double(nrow(data_matrix_noNA)),
                           observed_hl_totals     = double(nrow(data_matrix_noNA)),
                           observed_sl_totals     = double(nrow(data_matrix_noNA)),
                           observed_passFlag      = integer(nrow(data_matrix_noNA)),
                           failReasonsReal        = as.integer(failReasonsReal_rule2_vec),
                           componentScoresReal    = as.double(componentScoresReal_rule2_vec),
                           groupTotalsReal        = as.double(groupTotalsReal_rule2_vec),
                           subjectGradesReal      = as.double(subjectGradesReal_rule2_vec),
                           ierr                   = integer(1))

if (true_out_rule2$ierr != 0) stop("Error in compute_true_passFlag (Rule 2)")

failReasonsReal_rule2_mat <- matrix(true_out_rule2$failReasonsReal, nrow=nrow(data_matrix_noNA), ncol=7, byrow=FALSE)
observed_passFlag_rule2   <- true_out_rule2$observed_passFlag
write.csv(observed_passFlag_rule2, 'D. Results/A. Intermediate/observed_passFlag_rule2.csv')

# cat("Fail reasons (Rule 2) sum:\n")
print(colSums(failReasonsReal_rule2_mat))

## 2.3.2 With error ----
results_mat_rule2       <- matrix(NA_real_, nrow=nSim, ncol=5)
mismatchStorage_rule2   <- matrix(0L,        nrow=nSim, ncol=7)
colnames(results_mat_rule2) <- c("sens","spec","ppv","npv","acc")

init_out_rule2 <- .Fortran("init_fortran_seed2", ierr=integer(1))
if (init_out_rule2$ierr != 0) stop("Error seeding RNG (Rule 2)")

for (r in seq_len(nSim)) {
  failReasonsObs_rule2_vec <- integer(nrow(data_matrix_noNA) * 7)
  reasonDiff_rule2_vec     <- integer(7)
  
  out_rule2 <- .Fortran("add_error_scale_total_and_assign2",
                        data        = as.double(data_matrix_noNA),
                        diagVar     = as.double(diag(SigmaE_matrix)),
                        scaleFactor = as.double(scaleFactor_vector),
                        mask        = as.integer(mask_fortran),
                        group       = as.integer(group_fortran),
                        nrows       = as.integer(nrow(data_matrix_noNA)),
                        ncols       = as.integer(ncols),
                        nGroups     = as.integer(nGroups),
                        nBound      = as.integer(nBound_vector),
                        nBoundMax   = as.integer(nBoundMax_int),
                        bound_ub    = as.double(bound_ub_vector),
                        grade_val   = as.double(grade_val_vector),
                        tk_ee_flag  = as.integer(tk_ee_flag),
                        hl_flag     = as.integer(hl_flag),
                        sl_flag     = as.integer(sl_flag),
                        bonus_length= as.integer(bonus_length),
                        bonus_codes = as.integer(bonus_codes_vector),
                        bonus_points= as.integer(bonus_points_vector),
                        observed_passFlag = as.integer(observed_passFlag_rule2),
                        failReasonsReal   = as.integer(failReasonsReal_rule2_mat),
                        failReasonsObs    = as.integer(failReasonsObs_rule2_vec),
                        reasonDiff        = as.integer(reasonDiff_rule2_vec),
                        sensitivity       = double(1),
                        specificity       = double(1),
                        ppv               = double(1),
                        npv               = double(1),
                        accuracy          = double(1),
                        ierr              = integer(1))
  
  if (out_rule2$ierr != 0) stop("Error in add_error_scale_total_and_assign (Rule 2), ierr=", out_rule2$ierr)
  
  results_mat_rule2[r, ] <- c(out_rule2$sensitivity, out_rule2$specificity, 
                              out_rule2$ppv,        out_rule2$npv, 
                              out_rule2$accuracy)
  
  mismatchStorage_rule2[r, ] <- out_rule2$reasonDiff
}

## 2.3.3 Investigate ----
# cat("Mismatch means (Rule 2):\n")
print(colMeans(mismatchStorage_rule2))

# cat("Average metrics (Rule 2):\n")
print(colMeans(results_mat_rule2))

results_df_rule2 <- as.data.frame(results_mat_rule2)
names(results_df_rule2) <- c("sens","spec","ppv","npv","acc")

write.csv(results_df_rule2, "D. Results/B. Main/A. Base/results_rule2.csv", row.names=FALSE)

ggplot(results_df_rule2, aes(x=acc)) +
  geom_histogram(bins=25, fill="lightblue", color="black") +
  labs(title="Distribution of Accuracy Across 1000 Repli# cates (Rule 2)",
       x="Accuracy", y="Count")

summary(results_df_rule2$acc)


############################
# 2.4 Rule 3 ----
## 2.4.1 Initial ----
# dyn.unload("simulation_module_rule3.dll")
try(dyn.unload("C. Main Analysis/B. Fortran/simulation_module_rule2.dll"), silent = T)
dyn.load("C. Main Analysis/B. Fortran/simulation_module_rule3.dll")

componentScoresReal_rule3_vec <- double(nrow(data_matrix_noNA) * ncols)
groupTotalsReal_rule3_vec     <- double(nrow(data_matrix_noNA) * nGroups)
subjectGradesReal_rule3_vec   <- double(nrow(data_matrix_noNA) * nGroups)
failReasonsReal_rule3_vec     <- integer(nrow(data_matrix_noNA) * 7)

true_out_rule3 <- .Fortran("compute_true_passFlag3",
                           data         = as.double(data_matrix_noNA),
                           diagVar      = as.double(diag(SigmaE_matrix)),
                           scaleFactor  = as.double(scaleFactor_vector),
                           mask         = as.integer(mask_fortran),
                           group        = as.integer(group_fortran),
                           nrows        = as.integer(nrow(data_matrix_noNA)),
                           ncols        = as.integer(ncols),
                           nGroups      = as.integer(nGroups),
                           nBound       = as.integer(nBound_vector),
                           nBoundMax    = as.integer(nBoundMax_int),
                           bound_ub     = as.double(bound_ub_vector),
                           grade_val    = as.double(grade_val_vector),
                           totals       = double(nrow(data_matrix_noNA) * nGroups),
                           finalGrades  = double(nrow(data_matrix_noNA) * nGroups),
                           tk_ee_flag   = as.integer(tk_ee_flag),
                           hl_flag      = as.integer(hl_flag),
                           sl_flag      = as.integer(sl_flag),
                           bonus_length = as.integer(bonus_length),
                           bonus_codes  = as.integer(bonus_codes_vector),
                           bonus_points = as.integer(bonus_points_vector),
                           observed_student_totals = double(nrow(data_matrix_noNA)),
                           observed_hl_totals     = double(nrow(data_matrix_noNA)),
                           observed_sl_totals     = double(nrow(data_matrix_noNA)),
                           observed_passFlag      = integer(nrow(data_matrix_noNA)),
                           failReasonsReal        = as.integer(failReasonsReal_rule3_vec),
                           componentScoresReal    = as.double(componentScoresReal_rule3_vec),
                           groupTotalsReal        = as.double(groupTotalsReal_rule3_vec),
                           subjectGradesReal      = as.double(subjectGradesReal_rule3_vec),
                           ierr                   = integer(1))

if (true_out_rule3$ierr != 0) stop("Error in compute_true_passFlag (Rule 3)")

failReasonsReal_rule3_mat <- matrix(true_out_rule3$failReasonsReal, nrow=nrow(data_matrix_noNA), ncol=7, byrow=FALSE)
observed_passFlag_rule3   <- true_out_rule3$observed_passFlag
write.csv(observed_passFlag_rule3, 'D. Results/A. Intermediate/observed_passFlag_rule3.csv')

# cat("Fail reasons (Rule 3) sum:\n")
print(colSums(failReasonsReal_rule3_mat))

## 2.4.2 With error ----
results_mat_rule3       <- matrix(NA_real_, nrow=nSim, ncol=5)
mismatchStorage_rule3   <- matrix(0L,        nrow=nSim, ncol=7)
colnames(results_mat_rule3) <- c("sens","spec","ppv","npv","acc")

init_out_rule3 <- .Fortran("init_fortran_seed3", ierr=integer(1))
if (init_out_rule3$ierr != 0) stop("Error seeding RNG (Rule 3)")

for (r in seq_len(nSim)) {
  failReasonsObs_rule3_vec <- integer(nrow(data_matrix_noNA) * 7)
  reasonDiff_rule3_vec     <- integer(7)
  
  out_rule3 <- .Fortran("add_error_scale_total_and_assign3",
                        data        = as.double(data_matrix_noNA),
                        diagVar     = as.double(diag(SigmaE_matrix)),
                        scaleFactor = as.double(scaleFactor_vector),
                        mask        = as.integer(mask_fortran),
                        group       = as.integer(group_fortran),
                        nrows       = as.integer(nrow(data_matrix_noNA)),
                        ncols       = as.integer(ncols),
                        nGroups     = as.integer(nGroups),
                        nBound      = as.integer(nBound_vector),
                        nBoundMax   = as.integer(nBoundMax_int),
                        bound_ub    = as.double(bound_ub_vector),
                        grade_val   = as.double(grade_val_vector),
                        tk_ee_flag  = as.integer(tk_ee_flag),
                        hl_flag     = as.integer(hl_flag),
                        sl_flag     = as.integer(sl_flag),
                        bonus_length= as.integer(bonus_length),
                        bonus_codes = as.integer(bonus_codes_vector),
                        bonus_points= as.integer(bonus_points_vector),
                        observed_passFlag = as.integer(observed_passFlag_rule3),
                        failReasonsReal   = as.integer(failReasonsReal_rule3_mat),
                        failReasonsObs    = as.integer(failReasonsObs_rule3_vec),
                        reasonDiff        = as.integer(reasonDiff_rule3_vec),
                        sensitivity       = double(1),
                        specificity       = double(1),
                        ppv               = double(1),
                        npv               = double(1),
                        accuracy          = double(1),
                        ierr              = integer(1))
  
  if (out_rule3$ierr != 0) stop("Error in add_error_scale_total_and_assign (Rule 3), ierr=", out_rule3$ierr)
  
  results_mat_rule3[r, ] <- c(out_rule3$sensitivity, out_rule3$specificity, 
                              out_rule3$ppv,        out_rule3$npv, 
                              out_rule3$accuracy)
  
  mismatchStorage_rule3[r, ] <- out_rule3$reasonDiff
}

## 2.4.3 Investigate ----
# cat("Mismatch means (Rule 3):\n")
print(colMeans(mismatchStorage_rule3))

# cat("Average metrics (Rule 3):\n")
print(colMeans(results_mat_rule3))

results_df_rule3 <- as.data.frame(results_mat_rule3)
names(results_df_rule3) <- c("sens","spec","ppv","npv","acc")

write.csv(results_df_rule3, "D. Results/B. Main/A. Base/results_rule3.csv", row.names=FALSE)

ggplot(results_df_rule3, aes(x=acc)) +
  geom_histogram(bins=25, fill="lightblue", color="black") +
  labs(title="Distribution of Accuracy Across 1000 Repli# cates (Rule 3)",
       x="Accuracy", y="Count")

summary(results_df_rule3$acc)


############################
# 2.5 Rule 4 ----
## 2.5.1 Initial ----
try(dyn.unload("C. Main Analysis/B. Fortran/simulation_module_rule3.dll"), silent = T)
dyn.load("C. Main Analysis/B. Fortran/simulation_module_rule4.dll")

componentScoresReal_rule4_vec <- double(nrow(data_matrix_noNA) * ncols)
groupTotalsReal_rule4_vec     <- double(nrow(data_matrix_noNA) * nGroups)
subjectGradesReal_rule4_vec   <- double(nrow(data_matrix_noNA) * nGroups)
failReasonsReal_rule4_vec     <- integer(nrow(data_matrix_noNA) * 7)

true_out_rule4 <- .Fortran("compute_true_passFlag4",
                           data         = as.double(data_matrix_noNA),
                           diagVar      = as.double(diag(SigmaE_matrix)),
                           scaleFactor  = as.double(scaleFactor_vector),
                           mask         = as.integer(mask_fortran),
                           group        = as.integer(group_fortran),
                           nrows        = as.integer(nrow(data_matrix_noNA)),
                           ncols        = as.integer(ncols),
                           nGroups      = as.integer(nGroups),
                           nBound       = as.integer(nBound_vector),
                           nBoundMax    = as.integer(nBoundMax_int),
                           bound_ub     = as.double(bound_ub_vector),
                           grade_val    = as.double(grade_val_vector),
                           totals       = double(nrow(data_matrix_noNA) * nGroups),
                           finalGrades  = double(nrow(data_matrix_noNA) * nGroups),
                           tk_ee_flag   = as.integer(tk_ee_flag),
                           hl_flag      = as.integer(hl_flag),
                           sl_flag      = as.integer(sl_flag),
                           bonus_length = as.integer(bonus_length),
                           bonus_codes  = as.integer(bonus_codes_vector),
                           bonus_points = as.integer(bonus_points_vector),
                           observed_student_totals = double(nrow(data_matrix_noNA)),
                           observed_hl_totals     = double(nrow(data_matrix_noNA)),
                           observed_sl_totals     = double(nrow(data_matrix_noNA)),
                           observed_passFlag      = integer(nrow(data_matrix_noNA)),
                           failReasonsReal        = as.integer(failReasonsReal_rule4_vec),
                           componentScoresReal    = as.double(componentScoresReal_rule4_vec),
                           groupTotalsReal        = as.double(groupTotalsReal_rule4_vec),
                           subjectGradesReal      = as.double(subjectGradesReal_rule4_vec),
                           ierr                   = integer(1))

if (true_out_rule4$ierr != 0) stop("Error in compute_true_passFlag (Rule 4)")

failReasonsReal_rule4_mat <- matrix(true_out_rule4$failReasonsReal, nrow=nrow(data_matrix_noNA), ncol=7, byrow=FALSE)
observed_passFlag_rule4   <- true_out_rule4$observed_passFlag
write.csv(observed_passFlag_rule4, 'D. Results/A. Intermediate/observed_passFlag_rule4.csv')

# cat("Fail reasons (Rule 4) sum:\n")
print(colSums(failReasonsReal_rule4_mat))

## 2.5.2 With error ----
results_mat_rule4       <- matrix(NA_real_, nrow=nSim, ncol=5)
mismatchStorage_rule4   <- matrix(0L,        nrow=nSim, ncol=7)
colnames(results_mat_rule4) <- c("sens","spec","ppv","npv","acc")

init_out_rule4 <- .Fortran("init_fortran_seed4", ierr=integer(1))
if (init_out_rule4$ierr != 0) stop("Error seeding RNG (Rule 4)")

for (r in seq_len(nSim)) {
  failReasonsObs_rule4_vec <- integer(nrow(data_matrix_noNA) * 7)
  reasonDiff_rule4_vec     <- integer(7)
  
  out_rule4 <- .Fortran("add_error_scale_total_and_assign4",
                        data        = as.double(data_matrix_noNA),
                        diagVar     = as.double(diag(SigmaE_matrix)),
                        scaleFactor = as.double(scaleFactor_vector),
                        mask        = as.integer(mask_fortran),
                        group       = as.integer(group_fortran),
                        nrows       = as.integer(nrow(data_matrix_noNA)),
                        ncols       = as.integer(ncols),
                        nGroups     = as.integer(nGroups),
                        nBound      = as.integer(nBound_vector),
                        nBoundMax   = as.integer(nBoundMax_int),
                        bound_ub    = as.double(bound_ub_vector),
                        grade_val   = as.double(grade_val_vector),
                        tk_ee_flag  = as.integer(tk_ee_flag),
                        hl_flag     = as.integer(hl_flag),
                        sl_flag     = as.integer(sl_flag),
                        bonus_length= as.integer(bonus_length),
                        bonus_codes = as.integer(bonus_codes_vector),
                        bonus_points= as.integer(bonus_points_vector),
                        observed_passFlag = as.integer(observed_passFlag_rule4),
                        failReasonsReal   = as.integer(failReasonsReal_rule4_mat),
                        failReasonsObs    = as.integer(failReasonsObs_rule4_vec),
                        reasonDiff        = as.integer(reasonDiff_rule4_vec),
                        sensitivity       = double(1),
                        specificity       = double(1),
                        ppv               = double(1),
                        npv               = double(1),
                        accuracy          = double(1),
                        ierr              = integer(1))
  
  if (out_rule4$ierr != 0) stop("Error in add_error_scale_total_and_assign (Rule 4), ierr=", out_rule4$ierr)
  
  results_mat_rule4[r, ] <- c(out_rule4$sensitivity, out_rule4$specificity, 
                              out_rule4$ppv,        out_rule4$npv, 
                              out_rule4$accuracy)
  
  mismatchStorage_rule4[r, ] <- out_rule4$reasonDiff
}

## 2.5.3 Investigate ----
# cat("Mismatch means (Rule 4):\n")
print(colMeans(mismatchStorage_rule4))

# cat("Average metrics (Rule 4):\n")
print(colMeans(results_mat_rule4))

results_df_rule4 <- as.data.frame(results_mat_rule4)
names(results_df_rule4) <- c("sens","spec","ppv","npv","acc")

write.csv(results_df_rule4, "D. Results/B. Main/A. Base/results_rule4.csv", row.names=FALSE)

ggplot(results_df_rule4, aes(x=acc)) +
  geom_histogram(bins=25, fill="lightblue", color="black") +
  labs(title="Distribution of Accuracy Across 1000 Repli# cates (Rule 4)",
       x="Accuracy", y="Count")

summary(results_df_rule4$acc)
############################
# 2.6 Rule 5 ----
## 2.6.1 Initial ----
try(dyn.unload("C. Main Analysis/B. Fortran/simulation_module_rule4.dll"), silent = T)
dyn.load("C. Main Analysis/B. Fortran/simulation_module_rule5.dll")

componentScoresReal_rule5_vec <- double(nrow(data_matrix_noNA) * ncols)
groupTotalsReal_rule5_vec     <- double(nrow(data_matrix_noNA) * nGroups)
subjectGradesReal_rule5_vec   <- double(nrow(data_matrix_noNA) * nGroups)
failReasonsReal_rule5_vec     <- integer(nrow(data_matrix_noNA) * 7)

true_out_rule5 <- .Fortran("compute_true_passFlag5",
                           data         = as.double(data_matrix_noNA),
                           diagVar      = as.double(diag(SigmaE_matrix)),
                           scaleFactor  = as.double(scaleFactor_vector),
                           mask         = as.integer(mask_fortran),
                           group        = as.integer(group_fortran),
                           nrows        = as.integer(nrow(data_matrix_noNA)),
                           ncols        = as.integer(ncols),
                           nGroups      = as.integer(nGroups),
                           nBound       = as.integer(nBound_vector),
                           nBoundMax    = as.integer(nBoundMax_int),
                           bound_ub     = as.double(bound_ub_vector),
                           grade_val    = as.double(grade_val_vector),
                           totals       = double(nrow(data_matrix_noNA) * nGroups),
                           finalGrades  = double(nrow(data_matrix_noNA) * nGroups),
                           tk_ee_flag   = as.integer(tk_ee_flag),
                           hl_flag      = as.integer(hl_flag),
                           sl_flag      = as.integer(sl_flag),
                           bonus_length = as.integer(bonus_length),
                           bonus_codes  = as.integer(bonus_codes_vector),
                           bonus_points = as.integer(bonus_points_vector),
                           observed_student_totals = double(nrow(data_matrix_noNA)),
                           observed_hl_totals     = double(nrow(data_matrix_noNA)),
                           observed_sl_totals     = double(nrow(data_matrix_noNA)),
                           observed_passFlag      = integer(nrow(data_matrix_noNA)),
                           failReasonsReal        = as.integer(failReasonsReal_rule5_vec),
                           componentScoresReal    = as.double(componentScoresReal_rule5_vec),
                           groupTotalsReal        = as.double(groupTotalsReal_rule5_vec),
                           subjectGradesReal      = as.double(subjectGradesReal_rule5_vec),
                           ierr                   = integer(1))

if (true_out_rule5$ierr != 0) stop("Error in compute_true_passFlag (Rule 5)")

failReasonsReal_rule5_mat <- matrix(true_out_rule5$failReasonsReal, nrow=nrow(data_matrix_noNA), ncol=7, byrow=FALSE)
observed_passFlag_rule5   <- true_out_rule5$observed_passFlag
write.csv(observed_passFlag_rule5, 'D. Results/A. Intermediate/observed_passFlag_rule5.csv')

# cat("Fail reasons (Rule 5) sum:\n")
print(colSums(failReasonsReal_rule5_mat))

############################
# 3. Check results ----

# 0) Load results back in
results_mat <- read.csv("D. Results/B. Main/A. Base/results_base.csv")
results_mat_rule1 <- read.csv("D. Results/B. Main/A. Base/results_rule1.csv")
results_mat_rule2 <- read.csv("D. Results/B. Main/A. Base/results_rule2.csv")
results_mat_rule3 <- read.csv("D. Results/B. Main/A. Base/results_rule3.csv")
results_mat_rule4 <- read.csv("D. Results/B. Main/A. Base/results_rule4.csv")

# 1) Compute column means for each rule:
mean_base  <- colMeans(results_mat)
mean_r1    <- colMeans(results_mat_rule1)
mean_r2    <- colMeans(results_mat_rule2)
mean_r3    <- colMeans(results_mat_rule3)
mean_r4    <- colMeans(results_mat_rule4)

# 2) Combine those means row-wise into a single object:
comparison <- rbind(
  "Base"  = mean_base,
  "Rule1" = mean_r1,
  "Rule2" = mean_r2,
  "Rule3" = mean_r3,
  "Rule4" = mean_r4
)

# 3) Give nice column names:
colnames(comparison) <- c("Sensitivity", "Specificity", "PPV", "NPV", "Accuracy")

# 4) Inspect:
comparison

# 4. Test rules with retakes ----
# From here onwards we also look at gender and office differences
############################
# 4.1 Current rules ----
## 4.1.1 Arrays ----
# Initialising is unnecessary, we can just use the previous results
# Load the initial observed passes
observed_passFlag <- read.csv('D. Results/A. Intermediate/observed_passFlag.csv')$x
observed_passFlag_rule1 <- read.csv('D. Results/A. Intermediate/observed_passFlag_rule1.csv')$x
observed_passFlag_rule2 <- read.csv('D. Results/A. Intermediate/observed_passFlag_rule2.csv')$x
observed_passFlag_rule3 <- read.csv('D. Results/A. Intermediate/observed_passFlag_rule3.csv')$x
observed_passFlag_rule4 <- read.csv('D. Results/A. Intermediate/observed_passFlag_rule4.csv')$x
observed_passFlag_rule5 <- read.csv('D. Results/A. Intermediate/observed_passFlag_rule5.csv')$x

# dyn.unload("C. Main Analysis/B. Fortran/simulation_module_retake_base.dll")
# dyn.unload("C. Main Analysis/B. Fortran/simulation_module_rule4.dll")
dyn.load("C. Main Analysis/B. Fortran/simulation_module_retake_base.dll")

# Create storage matrices for overall and subgroup accuracies
results_overall  <- matrix(NA_real_, nrow = nSim, ncol = 5)
colnames(results_overall) <- c("sens", "spec", "ppv", "npv", "acc")

results_retake   <- matrix(NA_real_, nrow = nSim, ncol = 5)
colnames(results_retake) <- c("sens", "spec", "ppv", "npv", "acc")

demog_normal <- data.frame(
  acc_m = rep(NA_real_, nSim),
  acc_f = NA_real_,
  acc_ibaem = NA_real_,
  acc_ibap = NA_real_,
  acc_ibna = NA_real_
)

demog_retake <- data.frame(
  acc2_m = rep(NA_real_, nSim),
  acc2_f = NA_real_,
  acc2_ibaem = NA_real_,
  acc2_ibap = NA_real_,
  acc2_ibna = NA_real_
)

# Seed the RNG before the loop
init_out <- .Fortran("init_fortran_seed_retake", ierr = integer(1))
if (init_out$ierr != 0) stop("Error initializing RNG in Fortran.")

## 4.1.2 Analysis ----
for (r in 1:nSim) {
  # Call the Fortran subroutine (using the updated call as above)
  out <- .Fortran(
    "add_error_scale_total_and_assign_retake",
    data                = as.double(data_matrix_noNA),
    diagVar             = as.double(diag_fortran),
    scaleFactor         = as.double(scale_fortran),
    mask                = as.integer(mask_fortran),
    group               = as.integer(group_fortran),
    nrows               = as.integer(nrow(data_matrix_noNA)),
    ncols               = as.integer(ncol(data_matrix_noNA)),
    nGroups             = as.integer(nGroups),
    
    nBound              = as.integer(nBound_vector_int),
    nBoundMax           = as.integer(nBoundMax_int),
    bound_ub            = as.double(bound_ub_vector),
    grade_val           = as.double(grade_val_vector),
    
    tk_ee_flag          = as.integer(tk_ee_flag),
    comp_tkee           = as.integer(comp_tkee),
    hl_flag             = as.integer(hl_flag),
    sl_flag             = as.integer(sl_flag),
    bonus_length        = as.integer(bonus_length),
    bonus_codes         = as.integer(bonus_codes_vector),
    bonus_points        = as.integer(bonus_points_vector),
    observed_passFlag   = as.integer(observed_passFlag),
    
    baseline            = as.double(baseline),
    coeffs              = as.double(coeffs),
    threshold           = as.double(threshold),
    
    may_boundaries      = as.double(may_boundaries),
    nBound_May          = as.integer(nBound_May),
    grade_bands_TKEE    = as.double(grade_bands_TKEE),
    grade_bands_other   = as.double(grade_bands_other),
    
    mean_retake         = as.double(mean100),
    sd_retake           = as.double(sd100),
    exam_flag           = as.integer(exam_vector),
    
    nov_boundaries      = as.double(nov_boundaries),
    nBound_Nov          = as.integer(nBound_Nov),
    grade_bands_TKEE_nov= as.double(grade_bands_TKEE_nov),
    grade_bands_other_nov=as.double(grade_bands_other_nov),
    
    bound_ub_nov        = as.double(bound_ub_matrix_nov),
    grade_val_nov       = as.double(grade_val_matrix_nov),
    scaleFactor_nov     = as.double(scaleFactor_vector_nov),
    
    gender              = as.integer(gender_fortran),
    office              = as.integer(office_fortran),
    
    observed_passFlag_nov       = integer(nrow(data_matrix_noNA)),
    observed_passFlag_nov_error = integer(nrow(data_matrix_noNA)),
    
    sensitivity         = double(1),
    specificity         = double(1),
    ppv                 = double(1),
    npv                 = double(1),
    accuracy            = double(1),
    
    sensitivity_retake  = double(1),
    specificity_retake  = double(1),
    ppv_retake          = double(1),
    npv_retake          = double(1),
    accuracy_retake     = double(1),
    
    accuracy_m          = double(1),
    accuracy_f          = double(1),
    accuracy_ibaem      = double(1),
    accuracy_ibap       = double(1),
    accuracy_ibna       = double(1),
    
    accuracy2_m         = double(1),
    accuracy2_f         = double(1),
    accuracy2_ibaem     = double(1),
    accuracy2_ibap      = double(1),
    accuracy2_ibna      = double(1),
    
    ierr                = as.integer(1)
  )
  
  ## Store overall (normal) metrics:
  results_overall[r, ] <- c(out$sensitivity, out$specificity, out$ppv, out$npv, out$accuracy)
  
  ## Store retake metrics:
  results_retake[r, ]  <- c(out$sensitivity_retake, out$specificity_retake, out$ppv_retake,
                            out$npv_retake, out$accuracy_retake)
  
  ## Store subgroup (demographic) metrics:
  demog_normal$acc_m[r]      <- out$accuracy_m
  demog_normal$acc_f[r]      <- out$accuracy_f
  demog_normal$acc_ibaem[r]  <- out$accuracy_ibaem
  demog_normal$acc_ibap[r]   <- out$accuracy_ibap
  demog_normal$acc_ibna[r]   <- out$accuracy_ibna
  
  demog_retake$acc2_m[r]     <- out$accuracy2_m
  demog_retake$acc2_f[r]     <- out$accuracy2_f
  demog_retake$acc2_ibaem[r] <- out$accuracy2_ibaem
  demog_retake$acc2_ibap[r]  <- out$accuracy2_ibap
  demog_retake$acc2_ibna[r]  <- out$accuracy2_ibna
}

## 4.1.3 Investigate ----
# Now you can inspect the results:
print("Overall (normal) metrics over iterations:")
print(results_overall)

print("Retake metrics over iterations:")
print(results_retake)

print("Demographic subgroup (normal) accuracies over iterations:")
print(demog_normal)

print("Demographic subgroup retake accuracies over iterations:")
print(demog_retake)

# Save Base overall metrics
write.csv(results_overall, file = "D. Results/B. Main/B. Retake/Base_overall.csv", row.names = FALSE)
write.csv(results_retake, file = "D. Results/B. Main/B. Retake/Base_retake.csv", row.names = FALSE)
write.csv(demog_normal, file = "D. Results/B. Main/B. Retake/Base_demog_normal.csv", row.names = FALSE)
write.csv(demog_retake, file = "D. Results/B. Main/B. Retake/Base_demog_retake.csv", row.names = FALSE)
############################
# 4.2 Rule 1 ----
## 4.2.1 Arrays ----
# Initialising is unnecessary, we can just use the previous results

# dyn.unload("C. Main Analysis/B. Fortran/simulation_module_retake_rule1.dll")
dyn.unload("C. Main Analysis/B. Fortran/simulation_module_retake_base.dll")
dyn.load("C. Main Analysis/B. Fortran/simulation_module_retake_rule1.dll")

# Create storage matrices for overall and subgroup accuracies
results_overall1  <- matrix(NA_real_, nrow = nSim, ncol = 5)
colnames(results_overall1) <- c("sens", "spec", "ppv", "npv", "acc")

results_retake1   <- matrix(NA_real_, nrow = nSim, ncol = 5)
colnames(results_retake1) <- c("sens", "spec", "ppv", "npv", "acc")

demog_normal1 <- data.frame(
  acc_m = rep(NA_real_, nSim),
  acc_f = NA_real_,
  acc_ibaem = NA_real_,
  acc_ibap = NA_real_,
  acc_ibna = NA_real_
)

demog_retake1 <- data.frame(
  acc2_m = rep(NA_real_, nSim),
  acc2_f = NA_real_,
  acc2_ibaem = NA_real_,
  acc2_ibap = NA_real_,
  acc2_ibna = NA_real_
)

# Seed the RNG before the loop
init_out <- .Fortran("init_fortran_seed_retake1", ierr = integer(1))
if (init_out$ierr != 0) stop("Error initializing RNG in Fortran.")

## 4.2.2 Analysis ----
for (r in 1:nSim) {
  # Call the Fortran subroutine (using the updated call as above)
  out <- .Fortran(
    "add_error_scale_total_and_assign_retake1",
    data                = as.double(data_matrix_noNA),
    diagVar             = as.double(diag_fortran),
    scaleFactor         = as.double(scale_fortran),
    mask                = as.integer(mask_fortran),
    group               = as.integer(group_fortran),
    nrows               = as.integer(nrow(data_matrix_noNA)),
    ncols               = as.integer(ncol(data_matrix_noNA)),
    nGroups             = as.integer(nGroups),
    
    nBound              = as.integer(nBound_vector_int),
    nBoundMax           = as.integer(nBoundMax_int),
    bound_ub            = as.double(bound_ub_vector),
    grade_val           = as.double(grade_val_vector),
    
    tk_ee_flag          = as.integer(tk_ee_flag),
    comp_tkee           = as.integer(comp_tkee),
    hl_flag             = as.integer(hl_flag),
    sl_flag             = as.integer(sl_flag),
    bonus_length        = as.integer(bonus_length),
    bonus_codes         = as.integer(bonus_codes_vector),
    bonus_points        = as.integer(bonus_points_vector),
    observed_passFlag   = as.integer(observed_passFlag_rule1),
    
    baseline            = as.double(baseline),
    coeffs              = as.double(coeffs),
    threshold           = as.double(threshold),
    
    may_boundaries      = as.double(may_boundaries),
    nBound_May          = as.integer(nBound_May),
    grade_bands_TKEE    = as.double(grade_bands_TKEE),
    grade_bands_other   = as.double(grade_bands_other),
    
    mean_retake         = as.double(mean100),
    sd_retake           = as.double(sd100),
    exam_flag           = as.integer(exam_vector),
    
    nov_boundaries      = as.double(nov_boundaries),
    nBound_Nov          = as.integer(nBound_Nov),
    grade_bands_TKEE_nov= as.double(grade_bands_TKEE_nov),
    grade_bands_other_nov=as.double(grade_bands_other_nov),
    
    bound_ub_nov        = as.double(bound_ub_matrix_nov),
    grade_val_nov       = as.double(grade_val_matrix_nov),
    scaleFactor_nov     = as.double(scaleFactor_vector_nov),
    
    gender              = as.integer(gender_fortran),
    office              = as.integer(office_fortran),
    
    observed_passFlag_nov       = integer(nrow(data_matrix_noNA)),
    observed_passFlag_nov_error = integer(nrow(data_matrix_noNA)),
    
    sensitivity         = double(1),
    specificity         = double(1),
    ppv                 = double(1),
    npv                 = double(1),
    accuracy            = double(1),
    
    sensitivity_retake  = double(1),
    specificity_retake  = double(1),
    ppv_retake          = double(1),
    npv_retake          = double(1),
    accuracy_retake     = double(1),
    
    accuracy_m          = double(1),
    accuracy_f          = double(1),
    accuracy_ibaem      = double(1),
    accuracy_ibap       = double(1),
    accuracy_ibna       = double(1),
    
    accuracy2_m         = double(1),
    accuracy2_f         = double(1),
    accuracy2_ibaem     = double(1),
    accuracy2_ibap      = double(1),
    accuracy2_ibna      = double(1),
    
    ierr                = as.integer(1)
  )
  
  ## Store overall (normal) metrics:
  results_overall1[r, ] <- c(out$sensitivity, out$specificity, out$ppv, out$npv, out$accuracy)
  
  ## Store retake metrics:
  results_retake1[r, ]  <- c(out$sensitivity_retake, out$specificity_retake, out$ppv_retake,
                            out$npv_retake, out$accuracy_retake)
  
  ## Store subgroup (demographic) metrics:
  demog_normal1$acc_m[r]      <- out$accuracy_m
  demog_normal1$acc_f[r]      <- out$accuracy_f
  demog_normal1$acc_ibaem[r]  <- out$accuracy_ibaem
  demog_normal1$acc_ibap[r]   <- out$accuracy_ibap
  demog_normal1$acc_ibna[r]   <- out$accuracy_ibna
  
  demog_retake1$acc2_m[r]     <- out$accuracy2_m
  demog_retake1$acc2_f[r]     <- out$accuracy2_f
  demog_retake1$acc2_ibaem[r] <- out$accuracy2_ibaem
  demog_retake1$acc2_ibap[r]  <- out$accuracy2_ibap
  demog_retake1$acc2_ibna[r]  <- out$accuracy2_ibna
}

## 4.2.3 Investigate ----
# Now you can inspect the results:
print("Overall (normal) metrics over iterations:")
print(results_overall1)

print("Retake metrics over iterations:")
print(results_retake1)

print("Demographic subgroup (normal) accuracies over iterations:")
print(demog_normal1)

print("Demographic subgroup retake accuracies over iterations:")
print(demog_retake1)

# Save Rule 1 overall metrics
write.csv(results_overall1, file = "D. Results/B. Main/B. Retake/Rule1_overall.csv", row.names = FALSE)
write.csv(results_retake1, file = "D. Results/B. Main/B. Retake/Rule1_retake.csv", row.names = FALSE)
write.csv(demog_normal1, file = "D. Results/B. Main/B. Retake/Rule1_demog_normal.csv", row.names = FALSE)
write.csv(demog_retake1, file = "D. Results/B. Main/B. Retake/Rule1_demog_retake.csv", row.names = FALSE)
############################
# 4.3 Rule 2 ----
## 4.3.1 Arrays ----
# Unload previous rule’s DLL and load the new one
dyn.unload("C. Main Analysis/B. Fortran/simulation_module_retake_rule1.dll")
dyn.load("C. Main Analysis/B. Fortran/simulation_module_retake_rule2.dll")

# Create storage matrices and data frames for overall and subgroup accuracies for Rule 2:
results_overall2  <- matrix(NA_real_, nrow = nSim, ncol = 5)
colnames(results_overall2) <- c("sens","spec","ppv","npv","acc")

results_retake2   <- matrix(NA_real_, nrow = nSim, ncol = 5)
colnames(results_retake2) <- c("sens","spec","ppv","npv","acc")

demog_normal2 <- data.frame(
  acc_m     = rep(NA_real_, nSim),
  acc_f     = rep(NA_real_, nSim),
  acc_ibaem = rep(NA_real_, nSim),
  acc_ibap  = rep(NA_real_, nSim),
  acc_ibna  = rep(NA_real_, nSim)
)

demog_retake2 <- data.frame(
  acc2_m     = rep(NA_real_, nSim),
  acc2_f     = rep(NA_real_, nSim),
  acc2_ibaem = rep(NA_real_, nSim),
  acc2_ibap  = rep(NA_real_, nSim),
  acc2_ibna  = rep(NA_real_, nSim)
)

# Seed the RNG before the loop (for Rule 2)
init_out_rule2 <- .Fortran("init_fortran_seed_retake2", ierr = integer(1))
if (init_out_rule2$ierr != 0) stop("Error initializing RNG in Fortran (Rule 2).")

## 4.3.2 Analysis ----
for (r in seq_len(nSim)) {
  out_rule2 <- .Fortran(
    "add_error_scale_total_and_assign_retake2",
    data                = as.double(data_matrix_noNA),
    diagVar             = as.double(diag_fortran),
    scaleFactor         = as.double(scale_fortran),
    mask                = as.integer(mask_fortran),
    group               = as.integer(group_fortran),
    nrows               = as.integer(nrow(data_matrix_noNA)),
    ncols               = as.integer(ncol(data_matrix_noNA)),
    nGroups             = as.integer(nGroups),
    
    nBound              = as.integer(nBound_vector_int),
    nBoundMax           = as.integer(nBoundMax_int),
    bound_ub            = as.double(bound_ub_vector),
    grade_val           = as.double(grade_val_vector),
    
    tk_ee_flag          = as.integer(tk_ee_flag),
    comp_tkee           = as.integer(comp_tkee),
    hl_flag             = as.integer(hl_flag),
    sl_flag             = as.integer(sl_flag),
    bonus_length        = as.integer(bonus_length),
    bonus_codes         = as.integer(bonus_codes_vector),
    bonus_points        = as.integer(bonus_points_vector),
    observed_passFlag   = as.integer(observed_passFlag_rule2),
    
    baseline            = as.double(baseline),
    coeffs              = as.double(coeffs),
    threshold           = as.double(threshold),
    
    may_boundaries      = as.double(may_boundaries),
    nBound_May          = as.integer(nBound_May),
    grade_bands_TKEE    = as.double(grade_bands_TKEE),
    grade_bands_other   = as.double(grade_bands_other),
    
    mean_retake         = as.double(mean100),
    sd_retake           = as.double(sd100),
    exam_flag           = as.integer(exam_vector),
    
    nov_boundaries      = as.double(nov_boundaries),
    nBound_Nov          = as.integer(nBound_Nov),
    grade_bands_TKEE_nov= as.double(grade_bands_TKEE_nov),
    grade_bands_other_nov= as.double(grade_bands_other_nov),
    
    bound_ub_nov        = as.double(bound_ub_matrix_nov),
    grade_val_nov       = as.double(grade_val_matrix_nov),
    scaleFactor_nov     = as.double(scaleFactor_vector_nov),
    
    gender              = as.integer(gender_fortran),
    office              = as.integer(office_fortran),
    
    observed_passFlag_nov       = integer(nrow(data_matrix_noNA)),
    observed_passFlag_nov_error = integer(nrow(data_matrix_noNA)),
    
    sensitivity         = double(1),
    specificity         = double(1),
    ppv                 = double(1),
    npv                 = double(1),
    accuracy            = double(1),
    
    sensitivity_retake  = double(1),
    specificity_retake  = double(1),
    ppv_retake          = double(1),
    npv_retake          = double(1),
    accuracy_retake     = double(1),
    
    accuracy_m          = double(1),
    accuracy_f          = double(1),
    accuracy_ibaem      = double(1),
    accuracy_ibap       = double(1),
    accuracy_ibna       = double(1),
    
    accuracy2_m         = double(1),
    accuracy2_f         = double(1),
    accuracy2_ibaem     = double(1),
    accuracy2_ibap      = double(1),
    accuracy2_ibna      = double(1),
    
    ierr                = as.integer(1)
  )
  
  ## Store overall (normal) metrics:
  results_overall2[r, ] <- c(out_rule2$sensitivity, out_rule2$specificity, 
                             out_rule2$ppv, out_rule2$npv, out_rule2$accuracy)
  
  ## Store retake metrics:
  results_retake2[r, ] <- c(out_rule2$sensitivity_retake, out_rule2$specificity_retake, 
                            out_rule2$ppv_retake, out_rule2$npv_retake, out_rule2$accuracy_retake)
  
  ## Store subgroup (demographic) metrics from the normal branch:
  demog_normal2$acc_m[r]     <- out_rule2$accuracy_m
  demog_normal2$acc_f[r]     <- out_rule2$accuracy_f
  demog_normal2$acc_ibaem[r] <- out_rule2$accuracy_ibaem
  demog_normal2$acc_ibap[r]  <- out_rule2$accuracy_ibap
  demog_normal2$acc_ibna[r]  <- out_rule2$accuracy_ibna
  
  ## Store subgroup (demographic) metrics from the retake branch:
  demog_retake2$acc2_m[r]    <- out_rule2$accuracy2_m
  demog_retake2$acc2_f[r]    <- out_rule2$accuracy2_f
  demog_retake2$acc2_ibaem[r]<- out_rule2$accuracy2_ibaem
  demog_retake2$acc2_ibap[r] <- out_rule2$accuracy2_ibap
  demog_retake2$acc2_ibna[r] <- out_rule2$accuracy2_ibna
}

## 4.3.3 Investigate ----
# cat("Overall (normal) metrics for Rule 2:\n")
print(results_overall2)
# cat("Retake (with error) metrics for Rule 2:\n")
print(results_retake2)
# cat("Demographic subgroup (normal) accuracies for Rule 2:\n")
print(demog_normal2)
# cat("Demographic subgroup retake accuracies for Rule 2:\n")
print(demog_retake2)

# Save Rule 2 overall metrics
write.csv(results_overall2, file = "D. Results/B. Main/B. Retake/Rule2_overall.csv", row.names = FALSE)

# Save Rule 2 retake metrics
write.csv(results_retake2, file = "D. Results/B. Main/B. Retake/Rule2_retake.csv", row.names = FALSE)

# Save Rule 2 demographic subgroup normal accuracies
write.csv(demog_normal2, file = "D. Results/B. Main/B. Retake/Rule2_demog_normal.csv", row.names = FALSE)

# Save Rule 2 demographic subgroup retake accuracies
write.csv(demog_retake2, file = "D. Results/B. Main/B. Retake/Rule2_demog_retake.csv", row.names = FALSE)

############################
# 4.4 Rule 3 ----
## 4.4.1 Arrays ----
dyn.unload("C. Main Analysis/B. Fortran/simulation_module_retake_rule2.dll")
dyn.load("C. Main Analysis/B. Fortran/simulation_module_retake_rule3.dll")

results_overall3  <- matrix(NA_real_, nrow = nSim, ncol = 5)
colnames(results_overall3) <- c("sens","spec","ppv","npv","acc")

results_retake3   <- matrix(NA_real_, nrow = nSim, ncol = 5)
colnames(results_retake3) <- c("sens","spec","ppv","npv","acc")

demog_normal3 <- data.frame(
  acc_m     = rep(NA_real_, nSim),
  acc_f     = rep(NA_real_, nSim),
  acc_ibaem = rep(NA_real_, nSim),
  acc_ibap  = rep(NA_real_, nSim),
  acc_ibna  = rep(NA_real_, nSim)
)

demog_retake3 <- data.frame(
  acc2_m     = rep(NA_real_, nSim),
  acc2_f     = rep(NA_real_, nSim),
  acc2_ibaem = rep(NA_real_, nSim),
  acc2_ibap  = rep(NA_real_, nSim),
  acc2_ibna  = rep(NA_real_, nSim)
)

init_out_rule3 <- .Fortran("init_fortran_seed_retake3", ierr = integer(1))
if (init_out_rule3$ierr != 0) stop("Error initializing RNG in Fortran (Rule 3).")

## 4.4.2 Analysis ----
for (r in 1:nSim) {
  out_rule3 <- .Fortran(
    "add_error_scale_total_and_assign_retake3",
    data                = as.double(data_matrix_noNA),
    diagVar             = as.double(diag_fortran),
    scaleFactor         = as.double(scale_fortran),
    mask                = as.integer(mask_fortran),
    group               = as.integer(group_fortran),
    nrows               = as.integer(nrow(data_matrix_noNA)),
    ncols               = as.integer(ncol(data_matrix_noNA)),
    nGroups             = as.integer(nGroups),
    
    nBound              = as.integer(nBound_vector_int),
    nBoundMax           = as.integer(nBoundMax_int),
    bound_ub            = as.double(bound_ub_vector),
    grade_val           = as.double(grade_val_vector),
    
    tk_ee_flag          = as.integer(tk_ee_flag),
    comp_tkee           = as.integer(comp_tkee),
    hl_flag             = as.integer(hl_flag),
    sl_flag             = as.integer(sl_flag),
    bonus_length        = as.integer(bonus_length),
    bonus_codes         = as.integer(bonus_codes_vector),
    bonus_points        = as.integer(bonus_points_vector),
    observed_passFlag   = as.integer(observed_passFlag_rule3),
    
    baseline            = as.double(baseline),
    coeffs              = as.double(coeffs),
    threshold           = as.double(threshold),
    
    may_boundaries      = as.double(may_boundaries),
    nBound_May          = as.integer(nBound_May),
    grade_bands_TKEE    = as.double(grade_bands_TKEE),
    grade_bands_other   = as.double(grade_bands_other),
    
    mean_retake         = as.double(mean100),
    sd_retake           = as.double(sd100),
    exam_flag           = as.integer(exam_vector),
    
    nov_boundaries      = as.double(nov_boundaries),
    nBound_Nov          = as.integer(nBound_Nov),
    grade_bands_TKEE_nov= as.double(grade_bands_TKEE_nov),
    grade_bands_other_nov=as.double(grade_bands_other_nov),
    
    bound_ub_nov        = as.double(bound_ub_matrix_nov),
    grade_val_nov       = as.double(grade_val_matrix_nov),
    scaleFactor_nov     = as.double(scaleFactor_vector_nov),
    
    gender              = as.integer(gender_fortran),
    office              = as.integer(office_fortran),
    
    observed_passFlag_nov       = integer(nrow(data_matrix_noNA)),
    observed_passFlag_nov_error = integer(nrow(data_matrix_noNA)),
    
    sensitivity         = double(1),
    specificity         = double(1),
    ppv                 = double(1),
    npv                 = double(1),
    accuracy            = double(1),
    
    sensitivity_retake  = double(1),
    specificity_retake  = double(1),
    ppv_retake          = double(1),
    npv_retake          = double(1),
    accuracy_retake     = double(1),
    
    accuracy_m          = double(1),
    accuracy_f          = double(1),
    accuracy_ibaem      = double(1),
    accuracy_ibap       = double(1),
    accuracy_ibna       = double(1),
    
    accuracy2_m         = double(1),
    accuracy2_f         = double(1),
    accuracy2_ibaem     = double(1),
    accuracy2_ibap      = double(1),
    accuracy2_ibna      = double(1),
    
    ierr                = as.integer(1)
  )
  
  results_overall3[r, ] <- c(out_rule3$sensitivity, out_rule3$specificity, 
                             out_rule3$ppv, out_rule3$npv, out_rule3$accuracy)
  
  results_retake3[r, ]  <- c(out_rule3$sensitivity_retake, out_rule3$specificity_retake, 
                             out_rule3$ppv_retake, out_rule3$npv_retake, out_rule3$accuracy_retake)
  
  demog_normal3$acc_m[r]      <- out_rule3$accuracy_m
  demog_normal3$acc_f[r]      <- out_rule3$accuracy_f
  demog_normal3$acc_ibaem[r]  <- out_rule3$accuracy_ibaem
  demog_normal3$acc_ibap[r]   <- out_rule3$accuracy_ibap
  demog_normal3$acc_ibna[r]   <- out_rule3$accuracy_ibna
  
  demog_retake3$acc2_m[r]     <- out_rule3$accuracy2_m
  demog_retake3$acc2_f[r]     <- out_rule3$accuracy2_f
  demog_retake3$acc2_ibaem[r] <- out_rule3$accuracy2_ibaem
  demog_retake3$acc2_ibap[r]  <- out_rule3$accuracy2_ibap
  demog_retake3$acc2_ibna[r]  <- out_rule3$accuracy2_ibna
}

## 4.4.3 Investigate ----
# cat("Rule 3 Overall (normal) metrics:\n")
print(results_overall3)
# cat("Rule 3 Retake metrics:\n")
print(results_retake3)
# cat("Rule 3 Demographic subgroup (normal) accuracies:\n")
print(demog_normal3)
# cat("Rule 3 Demographic subgroup retake accuracies:\n")
print(demog_retake3)

# Save Rule 3 overall metrics
write.csv(results_overall3, file = "D. Results/B. Main/B. Retake/Rule3_overall.csv", row.names = FALSE)

# Save Rule 3 retake metrics
write.csv(results_retake3, file = "D. Results/B. Main/B. Retake/Rule3_retake.csv", row.names = FALSE)

# Save Rule 3 demographic subgroup normal accuracies
write.csv(demog_normal3, file = "D. Results/B. Main/B. Retake/Rule3_demog_normal.csv", row.names = FALSE)

# Save Rule 3 demographic subgroup retake accuracies
write.csv(demog_retake3, file = "D. Results/B. Main/B. Retake/Rule3_demog_retake.csv", row.names = FALSE)
############################
# 4.5 Rule 4 ----
## 4.5.1 Arrays ----
dyn.unload("C. Main Analysis/B. Fortran/simulation_module_retake_rule3.dll")
dyn.load("C. Main Analysis/B. Fortran/simulation_module_retake_rule4.dll")

results_overall4  <- matrix(NA_real_, nrow = nSim, ncol = 5)
colnames(results_overall4) <- c("sens","spec","ppv","npv","acc")

results_retake4   <- matrix(NA_real_, nrow = nSim, ncol = 5)
colnames(results_retake4) <- c("sens","spec","ppv","npv","acc")

demog_normal4 <- data.frame(
  acc_m     = rep(NA_real_, nSim),
  acc_f     = rep(NA_real_, nSim),
  acc_ibaem = rep(NA_real_, nSim),
  acc_ibap  = rep(NA_real_, nSim),
  acc_ibna  = rep(NA_real_, nSim)
)

demog_retake4 <- data.frame(
  acc2_m     = rep(NA_real_, nSim),
  acc2_f     = rep(NA_real_, nSim),
  acc2_ibaem = rep(NA_real_, nSim),
  acc2_ibap  = rep(NA_real_, nSim),
  acc2_ibna  = rep(NA_real_, nSim)
)

init_out_rule4 <- .Fortran("init_fortran_seed_retake4", ierr = integer(1))
if (init_out_rule4$ierr != 0) stop("Error initializing RNG in Fortran (Rule 4).")

## 4.5.2 Analysis ----
for (r in seq_len(nSim)) {
  out_rule4 <- .Fortran(
    "add_error_scale_total_and_assign_retake4",
    data                = as.double(data_matrix_noNA),
    diagVar             = as.double(diag_fortran),
    scaleFactor         = as.double(scale_fortran),
    mask                = as.integer(mask_fortran),
    group               = as.integer(group_fortran),
    nrows               = as.integer(nrow(data_matrix_noNA)),
    ncols               = as.integer(ncol(data_matrix_noNA)),
    nGroups             = as.integer(nGroups),
    
    nBound              = as.integer(nBound_vector_int),
    nBoundMax           = as.integer(nBoundMax_int),
    bound_ub            = as.double(bound_ub_vector),
    grade_val           = as.double(grade_val_vector),
    
    tk_ee_flag          = as.integer(tk_ee_flag),
    comp_tkee           = as.integer(comp_tkee),
    hl_flag             = as.integer(hl_flag),
    sl_flag             = as.integer(sl_flag),
    bonus_length        = as.integer(bonus_length),
    bonus_codes         = as.integer(bonus_codes_vector),
    bonus_points        = as.integer(bonus_points_vector),
    observed_passFlag   = as.integer(observed_passFlag_rule4),
    
    baseline            = as.double(baseline),
    coeffs              = as.double(coeffs),
    threshold           = as.double(threshold),
    
    may_boundaries      = as.double(may_boundaries),
    nBound_May          = as.integer(nBound_May),
    grade_bands_TKEE    = as.double(grade_bands_TKEE),
    grade_bands_other   = as.double(grade_bands_other),
    
    mean_retake         = as.double(mean100),
    sd_retake           = as.double(sd100),
    exam_flag           = as.integer(exam_vector),
    
    nov_boundaries      = as.double(nov_boundaries),
    nBound_Nov          = as.integer(nBound_Nov),
    grade_bands_TKEE_nov= as.double(grade_bands_TKEE_nov),
    grade_bands_other_nov=as.double(grade_bands_other_nov),
    
    bound_ub_nov        = as.double(bound_ub_matrix_nov),
    grade_val_nov       = as.double(grade_val_matrix_nov),
    scaleFactor_nov     = as.double(scaleFactor_vector_nov),
    
    gender              = as.integer(gender_fortran),
    office              = as.integer(office_fortran),
    
    observed_passFlag_nov       = integer(nrow(data_matrix_noNA)),
    observed_passFlag_nov_error = integer(nrow(data_matrix_noNA)),
    
    sensitivity         = double(1),
    specificity         = double(1),
    ppv                 = double(1),
    npv                 = double(1),
    accuracy            = double(1),
    
    sensitivity_retake  = double(1),
    specificity_retake  = double(1),
    ppv_retake          = double(1),
    npv_retake          = double(1),
    accuracy_retake     = double(1),
    
    accuracy_m          = double(1),
    accuracy_f          = double(1),
    accuracy_ibaem      = double(1),
    accuracy_ibap       = double(1),
    accuracy_ibna       = double(1),
    
    accuracy2_m         = double(1),
    accuracy2_f         = double(1),
    accuracy2_ibaem     = double(1),
    accuracy2_ibap      = double(1),
    accuracy2_ibna      = double(1),
    
    ierr                = as.integer(1)
  )
  
  results_overall4[r, ] <- c(out_rule4$sensitivity, out_rule4$specificity, 
                             out_rule4$ppv, out_rule4$npv, out_rule4$accuracy)
  
  results_retake4[r, ]  <- c(out_rule4$sensitivity_retake, out_rule4$specificity_retake, 
                             out_rule4$ppv_retake, out_rule4$npv_retake, out_rule4$accuracy_retake)
  
  demog_normal4$acc_m[r]      <- out_rule4$accuracy_m
  demog_normal4$acc_f[r]      <- out_rule4$accuracy_f
  demog_normal4$acc_ibaem[r]  <- out_rule4$accuracy_ibaem
  demog_normal4$acc_ibap[r]   <- out_rule4$accuracy_ibap
  demog_normal4$acc_ibna[r]   <- out_rule4$accuracy_ibna
  
  demog_retake4$acc2_m[r]     <- out_rule4$accuracy2_m
  demog_retake4$acc2_f[r]     <- out_rule4$accuracy2_f
  demog_retake4$acc2_ibaem[r] <- out_rule4$accuracy2_ibaem
  demog_retake4$acc2_ibap[r]  <- out_rule4$accuracy2_ibap
  demog_retake4$acc2_ibna[r]  <- out_rule4$accuracy2_ibna
}

## 4.5.3 Investigate ----
# cat("Rule 4 Overall (normal) metrics:\n")
print(results_overall4)
# cat("Rule 4 Retake metrics:\n")
print(results_retake4)
# cat("Rule 4 Demographic subgroup (normal) accuracies:\n")
print(demog_normal4)
# cat("Rule 4 Demographic subgroup retake accuracies:\n")
print(demog_retake4)
# Save Rule 4 overall metrics
write.csv(results_overall4, file = "D. Results/B. Main/B. Retake/Rule4_overall.csv", row.names = FALSE)

# Save Rule 4 retake metrics
write.csv(results_retake4, file = "D. Results/B. Main/B. Retake/Rule4_retake.csv", row.names = FALSE)

# Save Rule 4 demographic subgroup normal accuracies
write.csv(demog_normal4, file = "D. Results/B. Main/B. Retake/Rule4_demog_normal.csv", row.names = FALSE)

# Save Rule 4 demographic subgroup retake accuracies
write.csv(demog_retake4, file = "D. Results/B. Main/B. Retake/Rule4_demog_retake.csv", row.names = FALSE)
############################
# 4.6 Rule 5 ----
## 4.6.1 Arrays ----
dyn.unload("C. Main Analysis/B. Fortran/simulation_module_retake_rule4.dll")
dyn.load("C. Main Analysis/B. Fortran/simulation_module_retake_rule5.dll")

results_overall5  <- matrix(NA_real_, nrow = nSim, ncol = 5)
colnames(results_overall5) <- c("sens","spec","ppv","npv","acc")

results_retake5   <- matrix(NA_real_, nrow = nSim, ncol = 5)
colnames(results_retake5) <- c("sens","spec","ppv","npv","acc")

demog_normal5 <- data.frame(
  acc_m     = rep(NA_real_, nSim),
  acc_f     = rep(NA_real_, nSim),
  acc_ibaem = rep(NA_real_, nSim),
  acc_ibap  = rep(NA_real_, nSim),
  acc_ibna  = rep(NA_real_, nSim)
)

demog_retake5 <- data.frame(
  acc2_m     = rep(NA_real_, nSim),
  acc2_f     = rep(NA_real_, nSim),
  acc2_ibaem = rep(NA_real_, nSim),
  acc2_ibap  = rep(NA_real_, nSim),
  acc2_ibna  = rep(NA_real_, nSim)
)

init_out_rule5 <- .Fortran("init_fortran_seed_retake5", ierr = integer(1))
if (init_out_rule5$ierr != 0) stop("Error initializing RNG in Fortran (Rule 5).")

## 4.6.2 Analysis ----
for (r in seq_len(nSim)) {
  out_rule5 <- .Fortran(
    "add_error_scale_total_and_assign_retake5",
    data                = as.double(data_matrix_noNA),
    diagVar             = as.double(diag_fortran),
    scaleFactor         = as.double(scale_fortran),
    mask                = as.integer(mask_fortran),
    group               = as.integer(group_fortran),
    nrows               = as.integer(nrow(data_matrix_noNA)),
    ncols               = as.integer(ncol(data_matrix_noNA)),
    nGroups             = as.integer(nGroups),
    
    nBound              = as.integer(nBound_vector_int),
    nBoundMax           = as.integer(nBoundMax_int),
    bound_ub            = as.double(bound_ub_vector),
    grade_val           = as.double(grade_val_vector),
    
    tk_ee_flag          = as.integer(tk_ee_flag),
    comp_tkee           = as.integer(comp_tkee),
    hl_flag             = as.integer(hl_flag),
    sl_flag             = as.integer(sl_flag),
    bonus_length        = as.integer(bonus_length),
    bonus_codes         = as.integer(bonus_codes_vector),
    bonus_points        = as.integer(bonus_points_vector),
    observed_passFlag   = as.integer(observed_passFlag_rule5),
    
    baseline            = as.double(baseline),
    coeffs              = as.double(coeffs),
    threshold           = as.double(threshold),
    
    may_boundaries      = as.double(may_boundaries),
    nBound_May          = as.integer(nBound_May),
    grade_bands_TKEE    = as.double(grade_bands_TKEE),
    grade_bands_other   = as.double(grade_bands_other),
    
    mean_retake         = as.double(mean100),
    sd_retake           = as.double(sd100),
    exam_flag           = as.integer(exam_vector),
    
    nov_boundaries      = as.double(nov_boundaries),
    nBound_Nov          = as.integer(nBound_Nov),
    grade_bands_TKEE_nov= as.double(grade_bands_TKEE_nov),
    grade_bands_other_nov=as.double(grade_bands_other_nov),
    
    bound_ub_nov        = as.double(bound_ub_matrix_nov),
    grade_val_nov       = as.double(grade_val_matrix_nov),
    scaleFactor_nov     = as.double(scaleFactor_vector_nov),
    
    gender              = as.integer(gender_fortran),
    office              = as.integer(office_fortran),
    
    observed_passFlag_nov       = integer(nrow(data_matrix_noNA)),
    observed_passFlag_nov_error = integer(nrow(data_matrix_noNA)),
    
    sensitivity         = double(1),
    specificity         = double(1),
    ppv                 = double(1),
    npv                 = double(1),
    accuracy            = double(1),
    
    sensitivity_retake  = double(1),
    specificity_retake  = double(1),
    ppv_retake          = double(1),
    npv_retake          = double(1),
    accuracy_retake     = double(1),
    
    accuracy_m          = double(1),
    accuracy_f          = double(1),
    accuracy_ibaem      = double(1),
    accuracy_ibap       = double(1),
    accuracy_ibna       = double(1),
    
    accuracy2_m         = double(1),
    accuracy2_f         = double(1),
    accuracy2_ibaem     = double(1),
    accuracy2_ibap      = double(1),
    accuracy2_ibna      = double(1),
    
    ierr                = as.integer(1)
  )
  
  results_overall5[r, ] <- c(out_rule5$sensitivity, out_rule5$specificity, 
                             out_rule5$ppv, out_rule5$npv, out_rule5$accuracy)
  
  results_retake5[r, ]  <- c(out_rule5$sensitivity_retake, out_rule5$specificity_retake, 
                             out_rule5$ppv_retake, out_rule5$npv_retake, out_rule5$accuracy_retake)
  
  demog_normal5$acc_m[r]      <- out_rule5$accuracy_m
  demog_normal5$acc_f[r]      <- out_rule5$accuracy_f
  demog_normal5$acc_ibaem[r]  <- out_rule5$accuracy_ibaem
  demog_normal5$acc_ibap[r]   <- out_rule5$accuracy_ibap
  demog_normal5$acc_ibna[r]   <- out_rule5$accuracy_ibna
  
  demog_retake5$acc2_m[r]     <- out_rule5$accuracy2_m
  demog_retake5$acc2_f[r]     <- out_rule5$accuracy2_f
  demog_retake5$acc2_ibaem[r] <- out_rule5$accuracy2_ibaem
  demog_retake5$acc2_ibap[r]  <- out_rule5$accuracy2_ibap
  demog_retake5$acc2_ibna[r]  <- out_rule5$accuracy2_ibna
}

## 4.6.3 Investigate ----
# cat("Rule 5 Overall (normal) metrics:\n")
print(results_overall5)
# cat("Rule 5 Retake metrics:\n")
print(results_retake5)
# cat("Rule 5 Demographic subgroup (normal) accuracies:\n")
print(demog_normal5)
# cat("Rule 5 Demographic subgroup retake accuracies:\n")
print(demog_retake5)
# Save Rule 5 overall metrics
write.csv(results_overall5, file = "D. Results/B. Main/B. Retake/Rule5_overall.csv", row.names = FALSE)

# Save Rule 5 retake metrics
write.csv(results_retake5, file = "D. Results/B. Main/B. Retake/Rule5_retake.csv", row.names = FALSE)

# Save Rule 5 demographic subgroup normal accuracies
write.csv(demog_normal5, file = "D. Results/B. Main/B. Retake/Rule5_demog_normal.csv", row.names = FALSE)

# Save Rule 5 demographic subgroup retake accuracies
write.csv(demog_retake5, file = "D. Results/B. Main/B. Retake/Rule5_demog_retake.csv", row.names = FALSE)
############################
# 5. Check results ----
# 5.1 Base ----
base_overall <- read.csv("D. Results/B. Main/B. Retake/Base_overall.csv")
base_retake  <- read.csv("D. Results/B. Main/B. Retake/Base_retake.csv")
base_demog_normal <- read.csv("D. Results/B. Main/B. Retake/Base_demog_normal.csv")
base_demog_retake <- read.csv("D. Results/B. Main/B. Retake/Base_demog_retake.csv")

# cat("==== Base Rule Overall Metrics ====\n")
print(summary(base_overall))
# cat("\nMeans (Base Overall):\n")
print(colMeans(base_overall, na.rm = TRUE))
# cat("\nStandard Deviations (Base Overall):\n")
print(apply(base_overall, 2, sd, na.rm = TRUE))

# cat("\n==== Base Rule Retake Metrics ====\n")
print(summary(base_retake))
# cat("\nMeans (Base Retake):\n")
print(colMeans(base_retake, na.rm = TRUE))
# cat("\nStandard Deviations (Base Retake):\n")
print(apply(base_retake, 2, sd, na.rm = TRUE))

# cat("\n==== Base Rule Demographic (Normal) Metrics ====\n")
print(summary(base_demog_normal))
# cat("\nMeans (Base Demographic Normal):\n")
print(colMeans(base_demog_normal, na.rm = TRUE))

# cat("\n==== Base Rule Demographic (Retake) Metrics ====\n")
print(summary(base_demog_retake))
# cat("\nMeans (Base Demographic Retake):\n")
print(colMeans(base_demog_retake, na.rm = TRUE))

# 5.2 Rule 1 ----
rule1_overall <- read.csv("D. Results/B. Main/B. Retake/Rule1_overall.csv")
rule1_retake  <- read.csv("D. Results/B. Main/B. Retake/Rule1_retake.csv")
rule1_demog_normal <- read.csv("D. Results/B. Main/B. Retake/Rule1_demog_normal.csv")
rule1_demog_retake <- read.csv("D. Results/B. Main/B. Retake/Rule1_demog_retake.csv")

# cat("==== Rule 1 Overall Metrics ====\n")
print(summary(rule1_overall))
# cat("\nMeans (Rule 1 Overall):\n")
print(colMeans(rule1_overall, na.rm = TRUE))
# cat("\nStandard Deviations (Rule 1 Overall):\n")
print(apply(rule1_overall, 2, sd, na.rm = TRUE))

# cat("\n==== Rule 1 Retake Metrics ====\n")
print(summary(rule1_retake))
# cat("\nMeans (Rule 1 Retake):\n")
print(colMeans(rule1_retake, na.rm = TRUE))
# cat("\nStandard Deviations (Rule 1 Retake):\n")
print(apply(rule1_retake, 2, sd, na.rm = TRUE))

# cat("\n==== Rule 1 Demographic (Normal) Metrics ====\n")
print(summary(rule1_demog_normal))
# cat("\nMeans (Rule 1 Demographic Normal):\n")
print(colMeans(rule1_demog_normal, na.rm = TRUE))

# cat("\n==== Rule 1 Demographic (Retake) Metrics ====\n")
print(summary(rule1_demog_retake))
# cat("\nMeans (Rule 1 Demographic Retake):\n")
print(colMeans(rule1_demog_retake, na.rm = TRUE))

# 5.3 Rule 2 ----
rule2_overall <- read.csv("D. Results/B. Main/B. Retake/Rule2_overall.csv")
rule2_retake  <- read.csv("D. Results/B. Main/B. Retake/Rule2_retake.csv")
rule2_demog_normal <- read.csv("D. Results/B. Main/B. Retake/Rule2_demog_normal.csv")
rule2_demog_retake <- read.csv("D. Results/B. Main/B. Retake/Rule2_demog_retake.csv")

# cat("==== Rule 2 Overall Metrics ====\n")
print(summary(rule2_overall))
# cat("\nMeans (Rule 2 Overall):\n")
print(colMeans(rule2_overall, na.rm = TRUE))
# cat("\nStandard Deviations (Rule 2 Overall):\n")
print(apply(rule2_overall, 2, sd, na.rm = TRUE))

# cat("\n==== Rule 2 Retake Metrics ====\n")
print(summary(rule2_retake))
# cat("\nMeans (Rule 2 Retake):\n")
print(colMeans(rule2_retake, na.rm = TRUE))
# cat("\nStandard Deviations (Rule 2 Retake):\n")
print(apply(rule2_retake, 2, sd, na.rm = TRUE))

# cat("\n==== Rule 2 Demographic (Normal) Metrics ====\n")
print(summary(rule2_demog_normal))
# cat("\nMeans (Rule 2 Demographic Normal):\n")
print(colMeans(rule2_demog_normal, na.rm = TRUE))

# cat("\n==== Rule 2 Demographic (Retake) Metrics ====\n")
print(summary(rule2_demog_retake))
# cat("\nMeans (Rule 2 Demographic Retake):\n")
print(colMeans(rule2_demog_retake, na.rm = TRUE))
# 5.4 Rule 3 ----
rule3_overall <- read.csv("D. Results/B. Main/B. Retake/Rule3_overall.csv")
rule3_retake  <- read.csv("D. Results/B. Main/B. Retake/Rule3_retake.csv")
rule3_demog_normal <- read.csv("D. Results/B. Main/B. Retake/Rule3_demog_normal.csv")
rule3_demog_retake <- read.csv("D. Results/B. Main/B. Retake/Rule3_demog_retake.csv")

# cat("==== Rule 3 Overall Metrics ====\n")
print(summary(rule3_overall))
# cat("\nMeans (Rule 3 Overall):\n")
print(colMeans(rule3_overall, na.rm = TRUE))
# cat("\nStandard Deviations (Rule 3 Overall):\n")
print(apply(rule3_overall, 2, sd, na.rm = TRUE))

# cat("\n==== Rule 3 Retake Metrics ====\n")
print(summary(rule3_retake))
# cat("\nMeans (Rule 3 Retake):\n")
print(colMeans(rule3_retake, na.rm = TRUE))
# cat("\nStandard Deviations (Rule 3 Retake):\n")
print(apply(rule3_retake, 2, sd, na.rm = TRUE))

# cat("\n==== Rule 3 Demographic (Normal) Metrics ====\n")
print(summary(rule3_demog_normal))
# cat("\nMeans (Rule 3 Demographic Normal):\n")
print(colMeans(rule3_demog_normal, na.rm = TRUE))

# cat("\n==== Rule 3 Demographic (Retake) Metrics ====\n")
print(summary(rule3_demog_retake))
# cat("\nMeans (Rule 3 Demographic Retake):\n")
print(colMeans(rule3_demog_retake, na.rm = TRUE))


# 5.5 Rule 4 ----
rule4_overall <- read.csv("D. Results/B. Main/B. Retake/Rule4_overall.csv")
rule4_retake  <- read.csv("D. Results/B. Main/B. Retake/Rule4_retake.csv")
rule4_demog_normal <- read.csv("D. Results/B. Main/B. Retake/Rule4_demog_normal.csv")
rule4_demog_retake <- read.csv("D. Results/B. Main/B. Retake/Rule4_demog_retake.csv")

# cat("==== Rule 4 Overall Metrics ====\n")
print(summary(rule4_overall))
# cat("\nMeans (Rule 4 Overall):\n")
print(colMeans(rule4_overall, na.rm = TRUE))
# cat("\nStandard Deviations (Rule 4 Overall):\n")
print(apply(rule4_overall, 2, sd, na.rm = TRUE))

# cat("\n==== Rule 4 Retake Metrics ====\n")
print(summary(rule4_retake))
# cat("\nMeans (Rule 4 Retake):\n")
print(colMeans(rule4_retake, na.rm = TRUE))
# cat("\nStandard Deviations (Rule 4 Retake):\n")
print(apply(rule4_retake, 2, sd, na.rm = TRUE))

# cat("\n==== Rule 4 Demographic (Normal) Metrics ====\n")
print(summary(rule4_demog_normal))
# cat("\nMeans (Rule 4 Demographic Normal):\n")
print(colMeans(rule4_demog_normal, na.rm = TRUE))

# cat("\n==== Rule 4 Demographic (Retake) Metrics ====\n")
print(summary(rule4_demog_retake))
# cat("\nMeans (Rule 4 Demographic Retake):\n")
print(colMeans(rule4_demog_retake, na.rm = TRUE))

# 5.5 Rule 5 ----
rule5_overall <- read.csv("D. Results/B. Main/B. Retake/Rule5_overall.csv")
rule5_retake  <- read.csv("D. Results/B. Main/B. Retake/B. Retake/Rule5_retake.csv")
rule5_demog_normal <- read.csv("D. Results/B. Main/B. Retake/B. Retake/Rule5_demog_normal.csv")
rule5_demog_retake <- read.csv("D. Results/B. Main/B. Retake/B. Retake/Rule5_demog_retake.csv")

# cat("==== Rule 5 Overall Metrics ====\n")
print(summary(rule5_overall))
# cat("\nMeans (Rule 5 Overall):\n")
print(colMeans(rule5_overall, na.rm = TRUE))
# cat("\nStandard Deviations (Rule 5 Overall):\n")
print(apply(rule5_overall, 2, sd, na.rm = TRUE))

# cat("\n==== Rule 5 Retake Metrics ====\n")
print(summary(rule5_retake))
# cat("\nMeans (Rule 5 Retake):\n")
print(colMeans(rule5_retake, na.rm = TRUE))
# cat("\nStandard Deviations (Rule 5 Retake):\n")
print(apply(rule5_retake, 2, sd, na.rm = TRUE))

# cat("\n==== Rule 5 Demographic (Normal) Metrics ====\n")
print(summary(rule5_demog_normal))
# cat("\nMeans (Rule 5 Demographic Normal):\n")
print(colMeans(rule5_demog_normal, na.rm = TRUE))

# cat("\n==== Rule 5 Demographic (Retake) Metrics ====\n")
print(summary(rule5_demog_retake))
# cat("\nMeans (Rule 5 Demographic Retake):\n")
print(colMeans(rule5_demog_retake, na.rm = TRUE))

# 6. Figures in article ----
# Total means
summary(decision$TOTAL_POINTS)
sd(decision$TOTAL_POINTS, na.rm = T)

# Means by subject
grade_groups <- dp_data %>% semi_join(filtered_candidates, by = "CANDIDATE") %>%
  select(CANDIDATE, GROUP_NO, SUBJECT_OPTION, SUBJECT_GRADE) %>% distinct()
summary(as.numeric(grade_groups$SUBJECT_GRADE))
sd(as.numeric(grade_groups$SUBJECT_GRADE), na.rm = T)

# For core subjects
grade_map <- c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5)
grade_groups <- dp_data %>%
  semi_join(filtered_candidates, by = "CANDIDATE") %>%
  select(CANDIDATE, GROUP_NO, SUBJECT_OPTION, SUBJECT_GRADE) %>%
  distinct() %>%
  # Keep only letter grades A–E
  filter(SUBJECT_GRADE %in% names(grade_map)) %>%
  # Replace letter grades with numeric values
  mutate(SUBJECT_GRADE_NUM = grade_map[SUBJECT_GRADE])
summary(grade_groups$SUBJECT_GRADE_NUM)

# Check subject groups
sub_groups <- dp_data %>% semi_join(filtered_candidates, by = "CANDIDATE") %>%
  select(CANDIDATE, GROUP_NO, SUBJECT_OPTION) %>% distinct()
table(sub_groups$GROUP_NO)


# 6.1 Numbers ----
read_metrics <- function(rule, kind) {
  read_csv(file.path("D. Results/B. Retake/Results", sprintf("%s_%s.csv", rule, kind)),
           show_col_types = FALSE)
}

rules      <- c("Base", paste0("Rule", 1:5))
pretty_rule <- \(r) if (r == "Base") "Base" else str_replace(r, "Rule", "Rule ")

prec_long <- map_dfr(rules, \(r) {
  before_vals <- read_metrics(r, "overall")[[5]]      # column-5 = accuracy
  after_vals  <- read_metrics(r, "retake") [[5]]
  
  tibble(
    Rule     = pretty_rule(r),
    Stage    = rep(c("Test", "Retake"),
                   times = c(length(before_vals), length(after_vals))),
    Precision = c(before_vals, after_vals)
  )
}) |>
  mutate(
    Rule  = factor(Rule,  levels = c("Base", paste0("Rule ", 1:5))),
    Stage = factor(Stage, levels = c("Test", "Retake"))
  )

Dem_data <- cbind(Dem_data, Flag = observed_passFlag)[,1:4]

gender_pass <- Dem_data %>%                                     
  group_by(GENDER) %>%                                          
  summarise(
    n_students = n(),
    prop_pass  = mean(Flag == 1),        # “1” = pass
    .groups    = "drop"
  )

print(gender_pass)

office_pass <- Dem_data %>%                                     
  group_by(REGIONAL_OFFICE) %>%                                 
  summarise(
    n_students = n(),
    prop_pass  = mean(Flag == 1),
    .groups    = "drop"
  )

print(office_pass)

# 6.2 Boxplots ----
ggplot(prec_long, aes(Rule, Precision, fill = Stage)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  scale_fill_manual(values = c("Test" = "white",
                               "Retake"  = "grey40"), name = "") +
  labs(x = NULL, y = "Precision") +
  theme_bw(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor    = element_blank())

# 6.3 Tables ----
fmt_scientific <- \(x, digits = 1) formatC(x, format = "e", digits = digits)

summ_fun <- \(x) {
  m  <- mean(x, na.rm = TRUE)
  sd <- sd(x,   na.rm = TRUE)
  sprintf("%.3f (%s)", m, fmt_scientific(sd))
}

metrics_before <- map_dfr(rules, \(r) {
  read_metrics(r, "overall") |>
    summarise(across(everything(), summ_fun)) |>
    mutate(Rule = pretty_rule(r), .before = 1)
})

metrics_after  <- map_dfr(rules, \(r) {
  read_metrics(r, "retake") |>
    summarise(across(everything(), summ_fun)) |>
    mutate(Rule = pretty_rule(r), .before = 1)
})

print(metrics_before)   # → table: means (sd) BEFORE retake
print(metrics_after)    # → table: means (sd) AFTER  retake

read_demog <- function(rule, phase = c("normal", "retake")) {
  phase <- match.arg(phase)
  read_csv(
    file.path("D. Results/B. Main/B. Retake/Results", sprintf("%s_demog_%s.csv", rule, phase)),
    show_col_types = FALSE
  )
}
fmt_ms <- \(x) sprintf("%.3f (%.1e)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))

build_table <- function(phase) {
  map_dfr(rules, \(r) {
    read_demog(r, phase) |>
      summarise(across(starts_with("acc"), fmt_ms)) |>
      mutate(Rule = pretty_rule(r), .before = 1)
  })
}

table_before <- build_table("normal")   # before retake
table_after  <- build_table("retake")   # after  retake
print(table_before)
print(table_after)

# 7. Precision Kelley and observed ----
rules        <- c("Base", paste0("Rule", 1:5))
dlls         <- file.path("Fortran",
                          c("simulation_module_base.dll",
                            paste0("simulation_module_rule", 1:5, ".dll")))
fortran_sym  <- c("compute_true_passFlag",
                  paste0("compute_true_passFlag", 1:5))

## Kelley-shrunk flag vectors you already created:
kelley_flags <- list(
  Base  = observed_passFlag,
  Rule1 = observed_passFlag_rule1,
  Rule2 = observed_passFlag_rule2,
  Rule3 = observed_passFlag_rule3,
  Rule4 = observed_passFlag_rule4,
  Rule5 = observed_passFlag_rule5
)

## container for new (no-Kelley) flags
noK_flags <- vector("list", length(rules)); names(noK_flags) <- rules

for (i in seq_along(rules)) {
  
  try(dyn.unload(dlls[i]), silent = TRUE)   # in case it is still loaded
  dyn.load(dlls[i])
  
  out <- .Fortran(fortran_sym[i],
                  data         = as.double(data_matrix_orig_noNA),
                  diagVar      = as.double(diag(SigmaE_matrix_orig)),
                  scaleFactor  = as.double(scaleFactor_vector_orig),
                  mask         = as.integer(mask_fortran_orig),
                  group        = as.integer(group_fortran_orig),
                  nrows        = as.integer(nrows_orig),
                  ncols        = as.integer(ncols_orig),
                  nGroups      = as.integer(nGroups_orig),
                  nBound       = as.integer(nBound_vector_int_orig),
                  nBoundMax    = as.integer(nBoundMax_int_orig),
                  bound_ub     = as.double(bound_ub_vector_orig),
                  grade_val    = as.double(grade_val_vector_orig),
                  totals       = double(nrows_orig * nGroups_orig),            # throw-away
                  finalGrades  = double(nrows_orig * nGroups_orig),            #    ”     ”
                  tk_ee_flag   = as.integer(tk_ee_flag_orig),
                  hl_flag      = as.integer(hl_flag_orig),
                  sl_flag      = as.integer(sl_flag_orig),
                  bonus_length = as.integer(bonus_length),
                  bonus_codes  = as.integer(bonus_codes_vector),
                  bonus_points = as.integer(bonus_points_vector),
                  observed_student_totals = double(nrows_orig),                # dummy
                  observed_hl_totals     = double(nrows_orig),
                  observed_sl_totals     = double(nrows_orig),
                  observed_passFlag      = integer(nrows_orig),                # ← what we need
                  failReasonsReal        = integer(nrows_orig * 7),            # dummy
                  componentScoresReal    = double(nrows_orig * ncols_orig),    # dummy
                  groupTotalsReal        = double(nrows_orig * nGroups_orig),  # dummy
                  subjectGradesReal      = double(nrows_orig * nGroups_orig),  # dummy
                  ierr                   = integer(1))
  
  if (out$ierr != 0)
    stop("Fortran error for ", rules[i], "  (ierr = ", out$ierr, ")")
  
  noK_flags[[i]] <- out$observed_passFlag
  write.csv(out$observed_passFlag,
            sprintf("D. Results/A. Intermediate/observed_passFlag_%s_noKelley.csv",
                    tolower(rules[i])),
            row.names = FALSE)
  
  dyn.unload(dlls[i])                       # keep namespace clean
}

# Precision = agreement proportion (Kelley == no-Kelley)

precision_vec <- mapply(function(k, n) mean(k == n), kelley_flags, noK_flags)

precision_tbl <- data.frame(
  Rule      = rules,
  Precision = round(precision_vec, 5)
)

print(precision_tbl, row.names = FALSE)

write.csv(precision_tbl,
          "D. Results/B. Main/C. Other/precision_agreement_Kelley_vs_noKelley.csv",
          row.names = FALSE)