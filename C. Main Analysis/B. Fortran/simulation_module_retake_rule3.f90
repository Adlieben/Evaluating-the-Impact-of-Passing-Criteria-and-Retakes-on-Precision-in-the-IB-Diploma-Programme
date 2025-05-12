subroutine add_error_scale_total_and_assign_retake3( &
     data, diagVar, scaleFactor, mask, group, nrows, ncols, nGroups, &
     nBound, nBoundMax, bound_ub, grade_val, &
     tk_ee_flag, comp_tkee, hl_flag, sl_flag, bonus_length, bonus_codes, &
     bonus_points, observed_passFlag, &
     baseline, coeffs, threshold, &
     may_boundaries, nBound_May, grade_bands_TKEE, grade_bands_other, &
     mean_retake, sd_retake, exam_flag, &
     nov_boundaries, nBound_Nov, grade_bands_TKEE_nov, grade_bands_other_nov, &
     bound_ub_nov, grade_val_nov, scaleFactor_nov, gender, office, &
     observed_passFlag_nov, observed_passFlag_nov_error, &
     sensitivity, specificity, ppv, npv, accuracy, &
     sensitivity_retake, specificity_retake, ppv_retake, npv_retake, accuracy_retake, &
     accuracy_m, accuracy_f, accuracy_ibaem, accuracy_ibap, accuracy_ibna, &
     accuracy2_m, accuracy2_f, accuracy2_ibaem, accuracy2_ibap, accuracy2_ibna, ierr)

  implicit none

  !=======================================================================
  ! 1) INTENT(IN) Variables
  !=======================================================================
  real*8, intent(in)     :: data(nrows, ncols)
  real*8, intent(in)     :: diagVar(ncols)
  real*8, intent(in)     :: scaleFactor(ncols)
  integer, intent(in)    :: mask(nrows, ncols)
  integer, intent(in)    :: group(ncols), nrows, ncols, nGroups
  integer, intent(in)    :: nBound(nGroups), nBoundMax
  real*8, intent(in)     :: bound_ub(nBoundMax, nGroups)
  real*8, intent(in)     :: grade_val(nBoundMax, nGroups)

  integer, intent(in)    :: tk_ee_flag(nGroups), comp_tkee(ncols), hl_flag(nGroups), sl_flag(nGroups)
  integer, intent(in)    :: bonus_length, bonus_codes(bonus_length), bonus_points(bonus_length)
  integer, intent(in)    :: observed_passFlag(nrows)

  real*8, intent(in)     :: baseline(nrows)
  real*8, intent(in)     :: coeffs(2)
  real*8, intent(in)     :: threshold

  real*8, intent(in)     :: may_boundaries(7, ncols)
  integer, intent(in)    :: nBound_May(ncols)
  real*8, intent(in)     :: grade_bands_TKEE(6), grade_bands_other(8)
  real*8, intent(in)     :: mean_retake, sd_retake
  integer, intent(in)    :: exam_flag(nrows)
  real*8, intent(in)     :: nov_boundaries(7, ncols)
  integer, intent(in)    :: nBound_Nov(ncols)
  real*8, intent(in)     :: grade_bands_TKEE_nov(6), grade_bands_other_nov(8)
  real*8, intent(in)     :: bound_ub_nov(nBoundMax, nGroups)
  real*8, intent(in)     :: grade_val_nov(nBoundMax, nGroups)
  real*8, intent(in)     :: scaleFactor_nov(ncols)
  
  integer, intent(in)    :: gender(nrows)
  integer, intent(in)    :: office(nrows)

  !=======================================================================
  ! 2) INTENT(OUT) Variables
  !=======================================================================
  ! Keep only final pass/fail flags & final accuracy metrics
  integer, intent(out)   :: observed_passFlag_nov(nrows)
  integer, intent(out)   :: observed_passFlag_nov_error(nrows)

  real*8, intent(out)    :: sensitivity, specificity, ppv, npv, accuracy
  real*8, intent(out)    :: sensitivity_retake, specificity_retake, ppv_retake, npv_retake, accuracy_retake
  real*8, intent(out)    :: accuracy_m, accuracy_f
  real*8, intent(out)    :: accuracy_ibaem, accuracy_ibap, accuracy_ibna
  real*8, intent(out)    :: accuracy2_m, accuracy2_f
  real*8, intent(out)    :: accuracy2_ibaem, accuracy2_ibap, accuracy2_ibna

  integer, intent(out)   :: ierr

  !=======================================================================
  ! 3) LOCAL ARRAYS that used to be INTENT(OUT), but are now only internal
  !=======================================================================
  ! For example: failReasonsObs, reasonDiff, retake_probability, etc.
  ! Also your old score100, scoreNov, retake_decision if still needed internally.

  real*8, allocatable :: score100(:,:)
  real*8, allocatable :: scoreNov(:,:)
  integer, allocatable :: failReasonsObs(:,:)
  integer, allocatable :: retake_decision(:)
  real*8,  allocatable :: retake_probability(:)
  integer, allocatable :: reasonDiff(:)

  ! Keep everything else you used internally:
  real*8, allocatable :: temp(:,:), totals(:,:), finalGrades(:,:)
  real*8, allocatable :: student_totals(:), hl_totals(:), sl_totals(:)
  integer, allocatable :: passFlag(:), failCounter(:), tk_ee_count(:)
  integer, allocatable :: cnt(:,:), grade_23_count(:)

  real*8, allocatable :: temp_retake(:,:), totals_retake(:,:), finalGrades_retake(:,:)
  real*8, allocatable :: hl_totals_retake(:), sl_totals_retake(:), student_totals_retake(:)
  integer, allocatable :: failReasonsObsRetake(:,:)
  integer, allocatable :: cnt_retake(:,:), tk_ee_count_retake(:), grade_23_count_retake(:)
  real*8, allocatable :: finalGrades_retake_error(:,:)
  real*8, allocatable :: hl_totals_retake_error(:), sl_totals_retake_error(:)
  real*8, allocatable :: student_totals_retake_error(:)
  integer, allocatable :: tk_ee_count_retake_error(:)

  !=======================================================================
  ! 4) Other Local Variables
  !=======================================================================
  integer :: i, j, g, k
  real*8  :: u1, u2, r, theta, z, dtemp, inc
  integer :: totalCount, tk_grade, ee_grade, bonus, combined_grade
  integer :: TP, FP, FN, TN
  integer :: TP2, FP2, FN2, TN2
  integer :: grade2count, grade23count, failcount
  integer :: k_index
  real*8  :: lower_bound, upper_bound, fraction, denom
  integer :: TP_m, FP_m, FN_m, TN_m
  integer :: TP_f, FP_f, FN_f, TN_f
  integer :: TP_ibaem, FP_ibaem, FN_ibaem, TN_ibaem
  integer :: TP_ibap, FP_ibap, FN_ibap, TN_ibap
  integer :: TP_ibna, FP_ibna, FN_ibna, TN_ibna
  integer :: TP2_m, FP2_m, FN2_m, TN2_m
  integer :: TP2_f, FP2_f, FN2_f, TN2_f
  integer :: TP2_ibaem, FP2_ibaem, FN2_ibaem, TN2_ibaem
  integer :: TP2_ibap, FP2_ibap, FN2_ibap, TN2_ibap
  integer :: TP2_ibna, FP2_ibna, FN2_ibna, TN2_ibna

  integer, parameter :: debug_unit = 99

  !-----------------------------------------------------------------------
  ! Step 0: Initialization
  !-----------------------------------------------------------------------
  open(unit=debug_unit, file="debug_log.txt", status="replace", action="write", iostat=ierr)
  if (ierr /= 0) then
     write(*,*) "Error opening debug file, ierr=", ierr
  end if

  write(debug_unit,*) "=== Debug Start: Subroutine add_error_scale_total_and_assign ==="
  write(debug_unit,*) "nrows=", nrows, " ncols=", ncols, " nGroups=", nGroups
  write(debug_unit,*) "nBoundMax_int=", nBoundMax

  write(debug_unit,*) "DEBUG: Starting add_error_scale_total_and_assign"
  flush(debug_unit)

  ierr = 0
  totalCount = 0

  ! Initialize / allocate local arrays
  allocate(score100(nrows,ncols));         score100(:,:) = -999.0d0
  allocate(scoreNov(nrows,ncols));         scoreNov(:,:) = -999.0d0
  allocate(retake_probability(nrows));     retake_probability(:) = 0.0d0
  allocate(retake_decision(nrows));        retake_decision(:) = 0
  allocate(failReasonsObs(nrows,7));       failReasonsObs(:,:) = 0
  allocate(reasonDiff(7));                 reasonDiff = 0

  allocate(temp(nrows, ncols));                temp(:,:) = 0.0d0
  allocate(totals(nrows, nGroups));            totals(:,:) = -999.0d0
  allocate(finalGrades(nrows, nGroups));       finalGrades(:,:) = -999.0d0
  allocate(cnt(nrows, nGroups));               cnt(:,:) = 0
  allocate(student_totals(nrows));             student_totals(:) = 0.0d0
  allocate(hl_totals(nrows));                  hl_totals(:) = 0.0d0
  allocate(sl_totals(nrows));                  sl_totals(:) = 0.0d0
  allocate(passFlag(nrows));                   passFlag(:) = 1
  allocate(failCounter(nrows));                failCounter(:) = 0
  allocate(tk_ee_count(nrows));                tk_ee_count(:) = 0
  allocate(grade_23_count(nrows));             grade_23_count(:) = 0

  allocate(temp_retake(nrows, ncols));         temp_retake(:,:) = -999.0d0
  allocate(totals_retake(nrows, nGroups));     totals_retake(:,:) = -999.0d0
  allocate(finalGrades_retake(nrows, nGroups));finalGrades_retake(:,:) = -999.0d0
  allocate(hl_totals_retake(nrows));           hl_totals_retake(:) = 0.0d0
  allocate(sl_totals_retake(nrows));           sl_totals_retake(:) = 0.0d0
  allocate(student_totals_retake(nrows));      student_totals_retake(:) = 0.0d0
  allocate(failReasonsObsRetake(nrows,7));     failReasonsObsRetake(:,:) = 0
  allocate(cnt_retake(nrows, nGroups));        cnt_retake(:,:) = 0
  allocate(tk_ee_count_retake(nrows));         tk_ee_count_retake(:) = 0
  allocate(grade_23_count_retake(nrows));      grade_23_count_retake(:) = 0

  allocate(finalGrades_retake_error(nrows, nGroups))
  finalGrades_retake_error(:,:) = -999.0d0
  allocate(hl_totals_retake_error(nrows))
  hl_totals_retake_error(:) = 0.0d0
  allocate(sl_totals_retake_error(nrows))
  sl_totals_retake_error(:) = 0.0d0
  allocate(student_totals_retake_error(nrows))
  student_totals_retake_error(:) = 0.0d0
  allocate(tk_ee_count_retake_error(nrows))
  tk_ee_count_retake_error(:) = 0

  ! Copy input data into temp array
  do i = 1, nrows
    do j = 1, ncols
       temp(i,j) = data(i,j)
    end do
  end do

write(debug_unit,*) 'DEBUG: Allocated temp(', nrows, ',', ncols, ')'
write(debug_unit,*) 'DEBUG: Allocated totals(', nrows, ',', nGroups, ')'
flush(debug_unit)

! -------------------------------------------------------------------------
! Step 1: Add measurement error + scale
! -------------------------------------------------------------------------
write(debug_unit,*) 'DEBUG: Entering Step 1: Adding measurement error and scaling'
do j = 1, ncols
 do i = 1, nrows
    if (mask(i,j) == 1) then
       call random_number(u1)
       call random_number(u2)
       if (u1 < 1d-12) u1 = 1d-12
       r = dsqrt(-2d0 * dlog(u1))
       theta = 2d0 * 3.141592653589793d0 * u2
       z = r * dcos(theta)
       dtemp = temp(i,j) + dsqrt(diagVar(j)) * z
       temp(i,j) = dnint(dtemp)
       temp(i,j) = temp(i,j) * scaleFactor(j)
       totalCount = totalCount + 1
    else
       temp(i,j) = 0.0d0
    end if
 end do
end do

! -------------------------------------------------------------------------
! Step 2: Sum scaled values by subject group
! -------------------------------------------------------------------------
write(debug_unit,*) 'DEBUG: Entering Step 2: Summing scaled values by subject group'
do i = 1, nrows
 do g = 1, nGroups
    totals(i,g) = 0.0d0
 end do
end do

do j = 1, ncols
 g = group(j)
 if (g < 1 .or. g > nGroups) then
    ierr = 1
    deallocate(temp, totals, finalGrades, cnt, student_totals, hl_totals, sl_totals, &
               passFlag, failCounter, tk_ee_count, grade_23_count)
    return
 end if
 do i = 1, nrows
    if (mask(i,j) == 1) then
       totals(i,g) = totals(i,g) + temp(i,j)
       cnt(i,g)    = cnt(i,g) + 1
    end if
 end do
end do

! Round totals or set -999 if no valid cells
do i = 1, nrows
 do g = 1, nGroups
    if (cnt(i,g) == 0) then
       totals(i,g) = -999.0d0
    else
       totals(i,g) = dnint(totals(i,g))
    end if
 end do
end do

! -------------------------------------------------------------------------
! Step 3: Assign final grades
! -------------------------------------------------------------------------
write(debug_unit,*) 'DEBUG: Entering Step 3: Assigning final grades'
flush(debug_unit)
do i = 1, nrows
 do g = 1, nGroups
    if (totals(i,g) == -999.0d0) then
       finalGrades(i,g) = -999.0d0
    else
       finalGrades(i,g) = grade_val(1,g)
       do k = 1, nBound(g)
          if (totals(i,g) > bound_ub(k,g)) then
             finalGrades(i,g) = grade_val(k+1,g)
          else
             exit
          end if
       end do
    end if
 end do
end do

! -------------------------------------------------------------------------
! Step 4: Compute final student totals
! -------------------------------------------------------------------------
write(debug_unit,*) 'DEBUG: Entering Step 4: Compute final student totals'
flush(debug_unit)
do i = 1, nrows
 student_totals(i) = 0.0d0
 tk_grade = -999
 ee_grade = -999
 tk_ee_count(i) = 0

 do g = 1, nGroups
    if (totals(i,g) == -999.0d0) cycle
    if (tk_ee_flag(g) == 1) then
       if (tk_grade == -999) then
          tk_grade = int(finalGrades(i,g))
       else
          ee_grade = int(finalGrades(i,g))
       end if
       tk_ee_count(i) = tk_ee_count(i) + 1
    else
       student_totals(i) = student_totals(i) + finalGrades(i,g)
    end if
 end do

 ! Combine TK+EE if both present
 if ((tk_grade /= -999) .and. (ee_grade /= -999)) then
    combined_grade = tk_grade * 10 + ee_grade
    bonus = -999
    do k = 1, bonus_length
       if (combined_grade == bonus_codes(k)) then
          bonus = bonus_points(k)
          exit
       end if
    end do
    if (bonus /= -999) then
       student_totals(i) = student_totals(i) + bonus
    end if
 end if
end do

! -------------------------------------------------------------------------
! Step 4.1: Compute HL and SL totals
! -------------------------------------------------------------------------
do i = 1, nrows
 hl_totals(i) = 0.0d0
 sl_totals(i) = 0.0d0
 do g = 1, nGroups
    if (totals(i,g) == -999.0d0) cycle
    if (hl_flag(g) == 1) hl_totals(i) = hl_totals(i) + finalGrades(i,g)
    if (sl_flag(g) == 1) sl_totals(i) = sl_totals(i) + finalGrades(i,g)
 end do
end do

! -------------------------------------------------------------------------
! Step 5: Decide pass/fail, track fail reasons
! -------------------------------------------------------------------------
write(debug_unit,*) 'DEBUG: Entering Step 5: Decide pass/fail, track fail reasons'
flush(debug_unit)
do i = 1, nrows
 passFlag(i) = 1
 failCounter(i) = 0
 grade_23_count(i) = 0

 ! Initialize reasons array
 do k = 1, 7
    failReasonsObs(i,k) = 0
 end do

 ! Condition 1
 if (student_totals(i) < 20.0d0) then
    failReasonsObs(i,1) = 1
 end if

 ! Condition 4
 do g = 1, nGroups
    if (finalGrades(i,g) == 1) then
       failReasonsObs(i,4) = 1
       exit
    end if
 end do

 ! Condition 5
 if (tk_ee_count(i) /= 2) then
    failReasonsObs(i,5) = 1
 end if

 ! Condition 6 & 7
 grade2count  = 0
 grade23count = 0
 do g = 1, nGroups
  if (tk_ee_flag(g) /= 1) then
    if (finalGrades(i,g) == 2) then
       grade2count  = grade2count + 1
       grade_23_count(i) = grade_23_count(i) + 1
    else if (finalGrades(i,g) == 3) then
       grade_23_count(i) = grade_23_count(i) + 1
    end if
  end if
 end do

 if (grade2count > 2) then
    failReasonsObs(i,6) = 1
 end if
 if (grade_23_count(i) > 3) then
    failReasonsObs(i,7) = 1
 end if

 ! If any condition triggered => fail
 if (sum(failReasonsObs(i,:)) > 0) then
    passFlag(i) = 0
 end if
end do

! -------------------------------------------------------------------------
! Step 7: Compute confusion matrix & metrics
! -------------------------------------------------------------------------
write(debug_unit,*) 'DEBUG: Compute confusion matrix & metrics'
flush(debug_unit)
TP = 0
FP = 0
FN = 0
TN = 0

do i = 1, nrows
 if (passFlag(i) == 1 .and. observed_passFlag(i) == 1) then
    TP = TP + 1
 else if (passFlag(i) == 1 .and. observed_passFlag(i) == 0) then
    FP = FP + 1
 else if (passFlag(i) == 0 .and. observed_passFlag(i) == 1) then
    FN = FN + 1
 else if (passFlag(i) == 0 .and. observed_passFlag(i) == 0) then
    TN = TN + 1
 end if
end do

sensitivity = 0d0
specificity = 0d0
ppv         = 0d0
npv         = 0d0
accuracy    = 0d0

if (TP + FN > 0) sensitivity = dble(TP) / dble(TP + FN)
if (TN + FP > 0) specificity = dble(TN) / dble(TN + FP)
if (TP + FP > 0) ppv         = dble(TP) / dble(TP + FP)
if (TN + FN > 0) npv         = dble(TN) / dble(TN + FN)
if (TP + TN + FP + FN > 0) accuracy = dble(TP + TN) / dble(TP + TN + FP + FN)

! Gender accuracies
TP_m = 0; FP_m = 0; FN_m = 0; TN_m = 0
TP_f = 0; FP_f = 0; FN_f = 0; TN_f = 0

  do i = 1, nrows
     if (gender(i) == 1) then         ! MALE
        if (passFlag(i) == 1 .and. observed_passFlag(i) == 1) then
           TP_m = TP_m + 1
        else if (passFlag(i) == 1 .and. observed_passFlag(i) == 0) then
           FP_m = FP_m + 1
        else if (passFlag(i) == 0 .and. observed_passFlag(i) == 1) then
           FN_m = FN_m + 1
        else if (passFlag(i) == 0 .and. observed_passFlag(i) == 0) then
           TN_m = TN_m + 1
        end if
     else if (gender(i) == 2) then    ! FEMALE
        if (passFlag(i) == 1 .and. observed_passFlag(i) == 1) then
           TP_f = TP_f + 1
        else if (passFlag(i) == 1 .and. observed_passFlag(i) == 0) then
           FP_f = FP_f + 1
        else if (passFlag(i) == 0 .and. observed_passFlag(i) == 1) then
           FN_f = FN_f + 1
        else if (passFlag(i) == 0 .and. observed_passFlag(i) == 0) then
           TN_f = TN_f + 1
        end if
     end if
  end do

  if (TP_m + TN_m + FP_m + FN_m > 0) then
     accuracy_m = dble(TP_m + TN_m) / dble(TP_m + TN_m + FP_m + FN_m)
  else
     accuracy_m = 0d0
  endif

  if (TP_f + TN_f + FP_f + FN_f > 0) then
     accuracy_f = dble(TP_f + TN_f) / dble(TP_f + TN_f + FP_f + FN_f)
  else
     accuracy_f = 0d0
  endif

  write(debug_unit,*) "Overall Accuracy - MALE: ", accuracy_m
  write(debug_unit,*) "Overall Accuracy - FEMALE: ", accuracy_f

  ! Now, for office groups (only IBAEM, IBAP, and IBNA)
  TP_ibaem = 0; FP_ibaem = 0; FN_ibaem = 0; TN_ibaem = 0
  TP_ibap  = 0; FP_ibap  = 0; FN_ibap  = 0; TN_ibap  = 0
  TP_ibna  = 0; FP_ibna  = 0; FN_ibna  = 0; TN_ibna  = 0

  do i = 1, nrows
     select case (office(i))
        case (0)  ! IBAEM
           if (passFlag(i)==1 .and. observed_passFlag(i)==1) then
              TP_ibaem = TP_ibaem + 1
           else if (passFlag(i)==1 .and. observed_passFlag(i)==0) then
              FP_ibaem = FP_ibaem + 1
           else if (passFlag(i)==0 .and. observed_passFlag(i)==1) then
              FN_ibaem = FN_ibaem + 1
           else if (passFlag(i)==0 .and. observed_passFlag(i)==0) then
              TN_ibaem = TN_ibaem + 1
           end if
        case (1)  ! IBAP
           if (passFlag(i)==1 .and. observed_passFlag(i)==1) then
              TP_ibap = TP_ibap + 1
           else if (passFlag(i)==1 .and. observed_passFlag(i)==0) then
              FP_ibap = FP_ibap + 1
           else if (passFlag(i)==0 .and. observed_passFlag(i)==1) then
              FN_ibap = FN_ibap + 1
           else if (passFlag(i)==0 .and. observed_passFlag(i)==0) then
              TN_ibap = TN_ibap + 1
           end if
        case (3)  ! IBNA
           if (passFlag(i)==1 .and. observed_passFlag(i)==1) then
              TP_ibna = TP_ibna + 1
           else if (passFlag(i)==1 .and. observed_passFlag(i)==0) then
              FP_ibna = FP_ibna + 1
           else if (passFlag(i)==0 .and. observed_passFlag(i)==1) then
              FN_ibna = FN_ibna + 1
           else if (passFlag(i)==0 .and. observed_passFlag(i)==0) then
              TN_ibna = TN_ibna + 1
           end if
        case default
           ! do nothing for other offices
     end select
  end do

  if (TP_ibaem + TN_ibaem + FP_ibaem + FN_ibaem > 0) then
     accuracy_ibaem = dble(TP_ibaem + TN_ibaem) / dble(TP_ibaem + TN_ibaem + FP_ibaem + FN_ibaem)
  else
     accuracy_ibaem = 0d0
  endif

  if (TP_ibap + TN_ibap + FP_ibap + FN_ibap > 0) then
     accuracy_ibap = dble(TP_ibap + TN_ibap) / dble(TP_ibap + TN_ibap + FP_ibap + FN_ibap)
  else
     accuracy_ibap = 0d0
  endif

  if (TP_ibna + TN_ibna + FP_ibna + FN_ibna > 0) then
     accuracy_ibna = dble(TP_ibna + TN_ibna) / dble(TP_ibna + TN_ibna + FP_ibna + FN_ibna)
  else
     accuracy_ibna = 0d0
  endif

  write(debug_unit,*) "Overall Accuracy - IBAEM: ", accuracy_ibaem
  write(debug_unit,*) "Overall Accuracy - IBAP: ", accuracy_ibap
  write(debug_unit,*) "Overall Accuracy - IBNA: ", accuracy_ibna

! -------------------------------------------------------------------------
! Step 8: Compute retake probability and decision
!   Fails is computed as the sum of failReasonsObs for the candidate.
!   total_points is student_totals.
!   Logistic model: logit = baseline(i) + coeffs(1)*total_points + coeffs(2)*Fails.
!   probability = 1/(1+exp(-logit))
!   Decision = 1 if probability > threshold, else 0.
!   This probability is not used later but is presented as a possible extension here.
! -------------------------------------------------------------------------

write(debug_unit,*) 'DEBUG: Entering Step 8: Computing retake probability'
flush(debug_unit)
  do i = 1, nrows
     retake_decision(i) = 0
     failcount = 0
     do k = 1, 7
        failcount = failcount + failReasonsObs(i,k)
     end do
    dtemp = baseline(i) + coeffs(1) * student_totals(i) + coeffs(2) * dble(failcount)
     retake_probability(i) = 1.0d0 / (1.0d0 + exp(-dtemp))
     if (passFlag(i) == 0) then
       if (retake_probability(i) > threshold) then
          retake_decision(i) = 1
       else
          retake_decision(i) = 0
       end if
      end if

  end do
  
! -------------------------------------------------------------------------
! Step 9: Convert raw component scores (in data from Step 1) to a 0–100 scale
! for each component (column).
! -------------------------------------------------------------------------
write(debug_unit,*) 'DEBUG: Entering Step 9: Convert raw component scores'
flush(debug_unit)
do i = 1, nrows
  do j = 1, ncols
    if (data(i,j) == -999.0d0) then
       score100(i,j) = -999.0d0
    else
       if (comp_tkee(j) == 1) then
          ! TK/EE component: use nBound_May(j)=5 and grade_bands_TKEE(1:6)
          k_index = 0
          do k = 1, nBound_May(j)
             if (data(i,j) <= may_boundaries(k, j)) then
                k_index = k
                exit
             end if
          end do
          if (k_index == 0) then
             k_index = nBound_May(j)
          end if
          if (k_index < 1) k_index = 1
          if (k_index > 5) k_index = 5

          if (k_index == 1) then
             lower_bound = 0.0d0
          else
             lower_bound = may_boundaries(k_index - 1, j)
          end if
          upper_bound = may_boundaries(k_index, j)
          denom = upper_bound - lower_bound
          if (denom < 1d-9) denom = 1d-9
          fraction = (data(i,j) - lower_bound) / denom
          if (fraction < 0d0) fraction = 0d0
          if (fraction > 1d0) fraction = 1d0
          score100(i,j) = grade_bands_TKEE(k_index) + (grade_bands_TKEE(k_index+1) - grade_bands_TKEE(k_index)) * fraction
       else
          ! Non-TK/EE component: use nBound_May(j)=7 and grade_bands_other(1:8)
          k_index = 0
          do k = 1, nBound_May(j)
             if (data(i,j) <= may_boundaries(k, j)) then
                k_index = k
                exit
             end if
          end do
          if (k_index == 0) then
             k_index = nBound_May(j)
          end if
          if (k_index < 1) k_index = 1
          if (k_index > 7) k_index = 7

          if (k_index == 1) then
             lower_bound = 0.0d0
          else
             lower_bound = may_boundaries(k_index - 1, j)
          end if
          upper_bound = may_boundaries(k_index, j)
          denom = upper_bound - lower_bound
          if (denom < 1d-9) denom = 1d-9
          fraction = (data(i,j) - lower_bound) / denom
          if (fraction < 0d0) fraction = 0d0
          if (fraction > 1d0) fraction = 1d0
          score100(i,j) = grade_bands_other(k_index) + (grade_bands_other(k_index+1) - grade_bands_other(k_index)) * fraction
       end if
    end if
  end do
end do

! ---------------------------------------------------------------------
! Step 10: Add retake increment to score100 for those who failed
! ---------------------------------------------------------------------
  write(debug_unit,*) 'DEBUG: Entering Step 10: Add retake increment'
  flush(debug_unit)

  do i = 1, nrows
    if (passFlag(i) == 0) then
       do j = 1, ncols
        ! if (exam_flag(j) == 1) then ! No increment added for continuous assessment if desirable
         ! skip if we have a sentinel -999 or something
         if (score100(i,j) == -999.0d0) cycle

         ! generate standard normal z ~ N(0,1) just like step 1
         call random_number(u1)
         if (u1 < 1d-12) u1 = 1d-12
         call random_number(u2)
         r = dsqrt(-2d0 * dlog(u1))
         theta = 2d0 * 3.141592653589793d0 * u2
         z = r * dcos(theta)

         ! scale & shift to mean_retake + z*sd_retake
         inc = mean_retake + z * sd_retake
         
         ! add the increment
         score100(i,j) = score100(i,j) + inc
         score100(i,j) = max(0d0, min(100d0, score100(i,j)))
        ! end if ! Related to the exam condition commented out above
       end do
    end if
  end do
  
! ---------------------------------------------------------------------
! Step 11: Convert final 0–100 scores to raw November scale
! ---------------------------------------------------------------------
write(debug_unit,*) 'DEBUG: Entering Step 11: Convert final 100-scale to raw November scale'
flush(debug_unit)

do i = 1, nrows
  do j = 1, ncols
    if (score100(i,j) < -998.99d0) then
       ! sentinel for missing
       scoreNov(i,j) = -999.0d0

    else

       ! Distinguish between TK/EE vs. other components by comp_tkee(j)
       if (comp_tkee(j) == 1) then
          ! TK/EE => nBound_Nov(j)=5, grade_bands_TKEE_nov(1..6)
          ! find the "band" in 0–100 that the candidate's score100 falls into
          k_index = 0
          do k = 1, nBound_Nov(j)
             if (score100(i,j) <= grade_bands_TKEE_nov(k+1)) then
                k_index = k
                exit
             end if
          end do
          if (k_index == 0) then
             k_index = nBound_Nov(j)
          end if
          if (k_index < 1) k_index = 1
          if (k_index > 5) k_index = 5

          ! "band_low" and "band_high" in the 0–100 scale
          lower_bound = grade_bands_TKEE_nov(k_index)
          upper_bound = grade_bands_TKEE_nov(k_index+1)
          denom = upper_bound - lower_bound
          if (denom < 1d-9) denom = 1d-9

          ! fraction in [0..1] along that band
          fraction = (score100(i,j) - lower_bound) / denom
          if (fraction < 0d0) fraction = 0d0
          if (fraction > 1d0) fraction = 1d0

          ! Now map that fraction onto the raw Nov boundary range
          ! For band k_index, the "raw" boundaries are nov_boundaries(k_index) & nov_boundaries(k_index+1) ...
          ! But note the code structure is nearly identical to Step 9 in reverse.
          if (k_index == 1) then
             lower_bound = 0.0d0
          else
             lower_bound = nov_boundaries(k_index - 1, j)
          end if
          upper_bound = nov_boundaries(k_index, j)

          denom = upper_bound - lower_bound
          if (denom < 1d-9) denom = 1d-9

          scoreNov(i,j) = lower_bound + fraction * denom

        else
          ! Non-TK/EE => nBound_Nov(j)=7, grade_bands_other_nov(1..8)
          ! find the "band" in 0–100 that the candidate's score100 falls into
          k_index = 0
          do k = 1, nBound_Nov(j)
             if (score100(i,j) <= grade_bands_other_nov(k+1)) then
                k_index = k
                exit
             end if
          end do
          if (k_index == 0) then
             k_index = nBound_Nov(j)
          end if
          if (k_index < 1) k_index = 1
          if (k_index > 7) k_index = 7

          ! "band_low" and "band_high" in the 0–100 scale
          lower_bound = grade_bands_other_nov(k_index)
          upper_bound = grade_bands_other_nov(k_index+1)
          denom = upper_bound - lower_bound
          if (denom < 1d-9) denom = 1d-9

          fraction = (score100(i,j) - lower_bound) / denom
          if (fraction < 0d0) fraction = 0d0
          if (fraction > 1d0) fraction = 1d0

          ! Convert fraction back to raw November boundaries
          if (k_index == 1) then
             lower_bound = 0.0d0
          else
             lower_bound = nov_boundaries(k_index - 1, j)
          end if
          upper_bound = nov_boundaries(k_index, j)

          denom = upper_bound - lower_bound
          if (denom < 1d-9) denom = 1d-9

          scoreNov(i,j) = lower_bound + fraction * denom

        end if

        ! Optionally round or clamp
        scoreNov(i,j) = dnint(scoreNov(i,j))
        if (scoreNov(i,j) < 0d0) scoreNov(i,j) = 0d0

    end if
  end do
end do

! -------------------------------------------------------------------------
! Step 12: Scale final November raw scores (scoreNov) by scaleFactor_nov
!          Then sum them by subject group => totals_retake
! -------------------------------------------------------------------------
write(debug_unit,*) 'DEBUG: Entering Step 12: Retake scale & sum'

! 12.0 Initialize totals_retake and cnt_retake
do i = 1, nrows
  do g = 1, nGroups
    totals_retake(i,g) = 0.0d0
    cnt_retake(i,g)    = 0
  end do
end do

! 12.1 Multiply scoreNov(i,j) by scaleFactor_nov(j) and sum by group
do j = 1, ncols
   g = group(j)
   if (g < 1 .or. g > nGroups) then
      ierr = 2
      write(debug_unit,*) 'ERROR: group(j)=', g, ' is out of valid range.'
      ! Deallocate as needed, then return (adjust which arrays you deallocate)
      deallocate(temp, totals, finalGrades, cnt, student_totals, hl_totals, sl_totals, &
                 passFlag, failCounter, tk_ee_count, grade_23_count, &
                 temp_retake, totals_retake, finalGrades_retake, hl_totals_retake, &
                 sl_totals_retake, student_totals_retake, &
                 failReasonsObsRetake, cnt_retake, tk_ee_count_retake, grade_23_count_retake)
      return
   end if

   do i = 1, nrows
      if (scoreNov(i,j) > -999.0d0) then
         dtemp = dnint(scoreNov(i,j) * scaleFactor_nov(j))
         totals_retake(i,g) = totals_retake(i,g) + dtemp
         cnt_retake(i,g)    = cnt_retake(i,g) + 1
      end if
   end do
end do

! 12.2 Round or set -999 if no valid cells
do i = 1, nrows
  do g = 1, nGroups
    if (cnt_retake(i,g) == 0) then
      totals_retake(i,g) = -999.0d0
    else
      totals_retake(i,g) = dnint(totals_retake(i,g))
    end if
  end do
end do

write(debug_unit,*) 'DEBUG: Step 12 complete'
flush(debug_unit)

! -------------------------------------------------------------------------
! Step 13: Assign final retake grades => finalGrades_retake
!    Using the same nBound(g) as May, but referencing bound_ub_nov/grade_val_nov
! -------------------------------------------------------------------------
write(debug_unit,*) 'DEBUG: Entering Step 13: Assign final retake grades'

do i = 1, nrows
  do g = 1, nGroups
    if (totals_retake(i,g) == -999.0d0) then
      finalGrades_retake(i,g) = -999.0d0
    else
      finalGrades_retake(i,g) = grade_val_nov(1,g)

      ! Use the same nBound(g) used for May
      do k = 1, nBound(g)
         if (totals_retake(i,g) > bound_ub_nov(k,g)) then
            finalGrades_retake(i,g) = grade_val_nov(k+1,g)
         else
            exit
         end if
      end do
      ! Only change grade if it is better than the first try
      if (finalGrades_retake(i,g) < finalGrades(i,g)) then
      finalGrades_retake(i,g) = finalGrades(i,g)
      end if
    end if
  end do
end do

write(debug_unit,*) 'DEBUG: Step 13 complete'
flush(debug_unit)

! -------------------------------------------------------------------------
! Step 14: Compute final student totals for retake => student_totals_retake
! -------------------------------------------------------------------------
write(debug_unit,*) 'DEBUG: Entering Step 14: Compute retake student totals'

do i = 1, nrows
  tk_grade = -999
  ee_grade = -999

  do g = 1, nGroups
     if (totals_retake(i,g) == -999.0d0) cycle
     
     if (tk_ee_flag(g) == 1) then
        if (tk_grade == -999) then
           tk_grade = int(finalGrades_retake(i,g))
        else
           ee_grade = int(finalGrades_retake(i,g))
        end if
        tk_ee_count_retake(i) = tk_ee_count_retake(i) + 1
     else
        student_totals_retake(i) = student_totals_retake(i) + finalGrades_retake(i,g)
     end if
  end do

  ! Combine TK+EE if both present
  if ((tk_grade /= -999) .and. (ee_grade /= -999)) then
     combined_grade = tk_grade * 10 + ee_grade
     bonus = -999
     do k = 1, bonus_length
       if (combined_grade == bonus_codes(k)) then
         bonus = bonus_points(k)
         exit
       end if
     end do
     if (bonus /= -999) then
       student_totals_retake(i) = student_totals_retake(i) + bonus
     end if
  end if
end do

write(debug_unit,*) 'DEBUG: Step 14 complete'
flush(debug_unit)

! -------------------------------------------------------------------------
! Step 15: Decide retake pass/fail => observed_passFlag_nov
! Replicates the same 7 conditions as Step 5, but now referencing the
! retake arrays: student_totals_retake, hl_totals_retake, sl_totals_retake,
! finalGrades_retake, and tk_ee_count_retake.
! -------------------------------------------------------------------------
write(debug_unit,*) 'DEBUG: Entering Step 15: Decide retake pass/fail'

! 15.1 Compute HL and SL totals for retake
do i = 1, nrows
  hl_totals_retake(i) = 0.0d0
  sl_totals_retake(i) = 0.0d0
  do g = 1, nGroups
    if (finalGrades_retake(i,g) == -999.0d0) cycle
    if (hl_flag(g) == 1) hl_totals_retake(i) = hl_totals_retake(i) + finalGrades_retake(i,g)
    if (sl_flag(g) == 1) sl_totals_retake(i) = sl_totals_retake(i) + finalGrades_retake(i,g)
  end do
end do

! 15.2 Decide pass/fail for retake
do i = 1, nrows
  observed_passFlag_nov(i) = 1             ! start as pass

  ! --- Condition 1: total < 20 => fail
  if (student_totals_retake(i) < 20.0d0) then
    observed_passFlag_nov(i) = 0
  end if

  ! --- Condition 4: any subject = grade 1 => fail
  do g = 1, nGroups
    if (finalGrades_retake(i,g) == 1) then
      observed_passFlag_nov(i) = 0
      exit
    end if
  end do

  ! --- Condition 5: tk_ee_count_retake(i) != 2 => fail
  if (tk_ee_count_retake(i) /= 2) then
    observed_passFlag_nov(i) = 0
  end if

  ! --- Condition 6 & 7: count how many 2’s, how many 2/3’s
  grade2count  = 0
  grade23count = 0
  do g = 1, nGroups
    if (tk_ee_flag(g) /= 1) then
      if (finalGrades_retake(i,g) == 2) then
        grade2count  = grade2count + 1
        grade23count = grade23count + 1
      else if (finalGrades_retake(i,g) == 3) then
        grade23count = grade23count + 1
      end if
    end if
  end do

  ! Condition 6: more than 2 subject grades = 2 => fail
  if (grade2count > 2) then
    observed_passFlag_nov(i) = 0
  end if

  ! Condition 7: more than 3 subject grades in {2,3} => fail
  if (grade23count > 3) then
    observed_passFlag_nov(i) = 0
  end if

end do

write(debug_unit,*) 'DEBUG: Step 15 complete'
flush(debug_unit)

! -------------------------------------------------------------------------
! Step 16: Add measurement error to the "scoreNov" matrix (just like Step 1)
! -------------------------------------------------------------------------
write(debug_unit,*) 'DEBUG: Entering Step 16: Add measurement error to scoreNov'
flush(debug_unit)

do j = 1, ncols
  do i = 1, nrows
     if (scoreNov(i,j) > -999.0d0) then
        call random_number(u1)
        call random_number(u2)
        if (u1 < 1d-12) u1 = 1d-12
        r = dsqrt(-2d0 * dlog(u1))
        theta = 2d0 * 3.141592653589793d0 * u2
        z = r * dcos(theta)

        ! Add the same normal error as in Step 1 (optionally using diagVar(j))
        dtemp = scoreNov(i,j) + dsqrt(diagVar(j)) * z

        ! Round to integer
        scoreNov(i,j) = dnint(dtemp)
     end if
  end do
end do

! -------------------------------------------------------------------------
! Step 17: Scale these new "scoreNov" values again and sum them by group
!          (just like Step 12, using scaleFactor_nov).
! -------------------------------------------------------------------------
write(debug_unit,*) 'DEBUG: Entering Step 17: Scale and sum final retake scores with error'
flush(debug_unit)

! 17.0 Re-initialize totals_retake and cnt_retake
do i = 1, nrows
  do g = 1, nGroups
    totals_retake(i,g) = 0.0d0
    cnt_retake(i,g)    = 0
  end do
end do

! 17.1 Multiply by scaleFactor_nov(j) and sum by subject group
do j = 1, ncols
   g = group(j)
   if (g < 1 .or. g > nGroups) then
      ierr = 3
      write(debug_unit,*) 'ERROR: group(j)=', g, ' is out of valid range in Step 17'
      deallocate(temp, totals, finalGrades, cnt, student_totals, hl_totals, sl_totals, &
                 passFlag, failCounter, tk_ee_count, grade_23_count, &
                 temp_retake, totals_retake, finalGrades_retake, hl_totals_retake, &
                 sl_totals_retake, student_totals_retake, &
                 failReasonsObsRetake, cnt_retake, tk_ee_count_retake, grade_23_count_retake)
      return
   end if

   do i = 1, nrows
      if (scoreNov(i,j) > -999.0d0) then
         dtemp = dnint(scoreNov(i,j) * scaleFactor_nov(j))
         totals_retake(i,g) = totals_retake(i,g) + dtemp
         cnt_retake(i,g)    = cnt_retake(i,g) + 1
      end if
   end do
end do

! 17.2 Round or set -999 if no valid cells
do i = 1, nrows
  do g = 1, nGroups
    if (cnt_retake(i,g) == 0) then
      totals_retake(i,g) = -999.0d0
    else
      totals_retake(i,g) = dnint(totals_retake(i,g))
    end if
  end do
end do

write(debug_unit,*) 'DEBUG: Step 17 complete'
flush(debug_unit)

! -------------------------------------------------------------------------
! Step 18: Assign final retake grades (with error) => finalGrades_retake_error
! -------------------------------------------------------------------------
write(debug_unit,*) 'DEBUG: Entering Step 18: Assign final retake ERROR grades'

do i = 1, nrows
  do g = 1, nGroups
    if (totals_retake(i,g) == -999.0d0) then
      finalGrades_retake_error(i,g) = -999.0d0
    else
      finalGrades_retake_error(i,g) = grade_val_nov(1,g)

      ! Use the same nBound(g) used previously
      do k = 1, nBound(g)
         if (totals_retake(i,g) > bound_ub_nov(k,g)) then
            finalGrades_retake_error(i,g) = grade_val_nov(k+1,g)
         else
            exit
         end if
      end do
      ! Only change grade if it is better than the first try
      if (finalGrades_retake(i,g) < finalGrades(i,g)) then
      finalGrades_retake(i,g) = finalGrades(i,g)
      end if
    end if
  end do
end do


! -------------------------------------------------------------------------
! Step 19: Compute final student totals for retake ERROR => student_totals_retake_error
! -------------------------------------------------------------------------
write(debug_unit,*) 'DEBUG: Entering Step 19: Compute retake ERROR student totals'

! First zero them out in case they already had data
do i = 1, nrows
  student_totals_retake_error(i) = 0.0d0
  tk_ee_count_retake_error(i)    = 0
  hl_totals_retake_error(i)      = 0.0d0
  sl_totals_retake_error(i)      = 0.0d0
end do

do i = 1, nrows
  tk_grade = -999
  ee_grade = -999

  ! Sum subject grades
  do g = 1, nGroups
     if (totals_retake(i,g) == -999.0d0) cycle

     if (tk_ee_flag(g) == 1) then
        if (tk_grade == -999) then
           tk_grade = int(finalGrades_retake_error(i,g))
        else
           ee_grade = int(finalGrades_retake_error(i,g))
        end if
        tk_ee_count_retake_error(i) = tk_ee_count_retake_error(i) + 1
     else
        student_totals_retake_error(i) = student_totals_retake_error(i) + finalGrades_retake_error(i,g)
     end if

     ! Track HL/SL totals the same way as Step 15.1
     if (finalGrades_retake_error(i,g) /= -999.0d0) then
        if (hl_flag(g) == 1) hl_totals_retake_error(i) = hl_totals_retake_error(i) + finalGrades_retake_error(i,g)
        if (sl_flag(g) == 1) sl_totals_retake_error(i) = sl_totals_retake_error(i) + finalGrades_retake_error(i,g)
     end if
  end do

  ! Combine TK+EE if both present
  if ((tk_grade /= -999) .and. (ee_grade /= -999)) then
     combined_grade = tk_grade * 10 + ee_grade
     bonus = -999
     do k = 1, bonus_length
       if (combined_grade == bonus_codes(k)) then
         bonus = bonus_points(k)
         exit
       end if
     end do
     if (bonus /= -999) then
       student_totals_retake_error(i) = student_totals_retake_error(i) + bonus
     end if
  end if
end do


! -------------------------------------------------------------------------
! Step 20: Decide pass/fail => observed_passFlag_nov_error
! -------------------------------------------------------------------------
write(debug_unit,*) 'DEBUG: Entering Step 20: Decide retake pass/fail with ERROR'

do i = 1, nrows
  observed_passFlag_nov_error(i) = 1  ! start as pass

  ! Condition 1: total < 20 => fail
  if (student_totals_retake_error(i) < 20.0d0) then
    observed_passFlag_nov_error(i) = 0
  end if

  ! Condition 4: any subject = grade 1 => fail
  do g = 1, nGroups
    if (finalGrades_retake_error(i,g) == 1) then
      observed_passFlag_nov_error(i) = 0
      exit
    end if
  end do

  ! Condition 5: tk_ee_count_retake_error(i) != 2 => fail
  if (tk_ee_count_retake_error(i) /= 2) then
    observed_passFlag_nov_error(i) = 0
  end if

  ! Condition 6 & 7: count how many 2's, how many 2/3's
  grade2count  = 0
  grade23count = 0
  do g = 1, nGroups
    if (tk_ee_flag(g) /= 1) then
      if (finalGrades_retake_error(i,g) == 2) then
        grade2count  = grade2count + 1
        grade23count = grade23count + 1
      else if (finalGrades_retake_error(i,g) == 3) then
        grade23count = grade23count + 1
      end if
    end if
  end do

  if (grade2count > 2) then
    observed_passFlag_nov_error(i) = 0
  end if
  if (grade23count > 3) then
    observed_passFlag_nov_error(i) = 0
  end if
end do

write(debug_unit,*) 'DEBUG: Step 20 complete'
flush(debug_unit)

! -------------------------------------------------------------------------
! Step 21: Compute confusion matrix & metrics for retake with error
! -------------------------------------------------------------------------
write(debug_unit,*) 'DEBUG: Entering Step 21: Compute confusion matrix & metrics (ERROR)'

TP2 = 0
FP2 = 0
FN2 = 0
TN2 = 0

do i = 1, nrows
  if (observed_passFlag_nov(i) == 1 .and. observed_passFlag_nov_error(i) == 1) then
     TP2 = TP2 + 1
  else if (observed_passFlag_nov(i) == 1 .and. observed_passFlag_nov_error(i) == 0) then
     FP2 = FP2 + 1
  else if (observed_passFlag_nov(i) == 0 .and. observed_passFlag_nov_error(i) == 1) then
     FN2 = FN2 + 1
  else if (observed_passFlag_nov(i) == 0 .and. observed_passFlag_nov_error(i) == 0) then
     TN2 = TN2 + 1
  end if
end do

! Reset your 'retake' metrics to zero before computing
sensitivity_retake = 0d0
specificity_retake = 0d0
ppv_retake         = 0d0
npv_retake         = 0d0
accuracy_retake    = 0d0

if (TP2 + FN2 > 0) sensitivity_retake = dble(TP2) / dble(TP2 + FN2)
if (TN2 + FP2 > 0) specificity_retake = dble(TN2) / dble(TN2 + FP2)
if (TP2 + FP2 > 0) ppv_retake         = dble(TP2) / dble(TP2 + FP2)
if (TN2 + FN2 > 0) npv_retake         = dble(TN2) / dble(TN2 + FN2)
if (TP2 + TN2 + FP2 + FN2 > 0) accuracy_retake = dble(TP2 + TN2) / dble(TP2 + TN2 + FP2 + FN2)

write(debug_unit,*) 'DEBUG: Step 21 complete'
flush(debug_unit)

! Gender accuracies
  TP2_m = 0; FP2_m = 0; FN2_m = 0; TN2_m = 0
  TP2_f = 0; FP2_f = 0; FN2_f = 0; TN2_f = 0

  do i = 1, nrows
     if (gender(i) == 1) then         ! MALE
        if (observed_passFlag_nov(i) == 1 .and. observed_passFlag_nov_error(i) == 1) then
           TP2_m = TP2_m + 1
        else if (observed_passFlag_nov(i) == 1 .and. observed_passFlag_nov_error(i) == 0) then
           FP2_m = FP2_m + 1
        else if (observed_passFlag_nov(i) == 0 .and. observed_passFlag_nov_error(i) == 1) then
           FN2_m = FN2_m + 1
        else if (observed_passFlag_nov(i) == 0 .and. observed_passFlag_nov_error(i) == 0) then
           TN2_m = TN2_m + 1
        end if
     else if (gender(i) == 2) then    ! FEMALE
        if (observed_passFlag_nov(i) == 1 .and. observed_passFlag_nov_error(i) == 1) then
           TP2_f = TP2_f + 1
        else if (observed_passFlag_nov(i) == 1 .and. observed_passFlag_nov_error(i) == 0) then
           FP2_f = FP2_f + 1
        else if (observed_passFlag_nov(i) == 0 .and. observed_passFlag_nov_error(i) == 1) then
           FN2_f = FN2_f + 1
        else if (observed_passFlag_nov(i) == 0 .and. observed_passFlag_nov_error(i) == 0) then
           TN2_f = TN2_f + 1
        end if
     end if
  end do

  if (TP2_m + TN2_m + FP2_m + FN2_m > 0) then
     accuracy2_m = dble(TP2_m + TN2_m) / dble(TP2_m + TN2_m + FP2_m + FN2_m)
  else
     accuracy2_m = 0d0
  endif

  if (TP2_f + TN2_f + FP2_f + FN2_f > 0) then
     accuracy2_f = dble(TP2_f + TN2_f) / dble(TP2_f + TN2_f + FP2_f + FN2_f)
  else
     accuracy2_f = 0d0
  endif

  write(debug_unit,*) "Retake Accuracy - MALE: ", accuracy2_m
  write(debug_unit,*) "Retake Accuracy - FEMALE: ", accuracy2_f

  ! Office accuracies
  TP2_ibaem = 0; FP2_ibaem = 0; FN2_ibaem = 0; TN2_ibaem = 0
  TP2_ibap  = 0; FP2_ibap  = 0; FN2_ibap  = 0; TN2_ibap  = 0
  TP2_ibna  = 0; FP2_ibna  = 0; FN2_ibna  = 0; TN2_ibna  = 0

  do i = 1, nrows
     select case (office(i))
        case (0)  ! IBAEM
           if (observed_passFlag_nov(i)==1 .and. observed_passFlag_nov_error(i)==1) then
              TP2_ibaem = TP2_ibaem + 1
           else if (observed_passFlag_nov(i)==1 .and. observed_passFlag_nov_error(i)==0) then
              FP2_ibaem = FP2_ibaem + 1
           else if (observed_passFlag_nov(i)==0 .and. observed_passFlag_nov_error(i)==1) then
              FN2_ibaem = FN2_ibaem + 1
           else if (observed_passFlag_nov(i)==0 .and. observed_passFlag_nov_error(i)==0) then
              TN2_ibaem = TN2_ibaem + 1
           end if
        case (1)  ! IBAP
           if (observed_passFlag_nov(i)==1 .and. observed_passFlag_nov_error(i)==1) then
              TP2_ibap = TP2_ibap + 1
           else if (observed_passFlag_nov(i)==1 .and. observed_passFlag_nov_error(i)==0) then
              FP2_ibap = FP2_ibap + 1
           else if (observed_passFlag_nov(i)==0 .and. observed_passFlag_nov_error(i)==1) then
              FN2_ibap = FN2_ibap + 1
           else if (observed_passFlag_nov(i)==0 .and. observed_passFlag_nov_error(i)==0) then
              TN2_ibap = TN2_ibap + 1
           end if
        case (3)  ! IBNA
           if (observed_passFlag_nov(i)==1 .and. observed_passFlag_nov_error(i)==1) then
              TP2_ibna = TP2_ibna + 1
           else if (observed_passFlag_nov(i)==1 .and. observed_passFlag_nov_error(i)==0) then
              FP2_ibna = FP2_ibna + 1
           else if (observed_passFlag_nov(i)==0 .and. observed_passFlag_nov_error(i)==1) then
              FN2_ibna = FN2_ibna + 1
           else if (observed_passFlag_nov(i)==0 .and. observed_passFlag_nov_error(i)==0) then
              TN2_ibna = TN2_ibna + 1
           end if
        case default
           ! ignore other offices
     end select
  end do

  if (TP2_ibaem + TN2_ibaem + FP2_ibaem + FN2_ibaem > 0) then
     accuracy2_ibaem = dble(TP2_ibaem + TN2_ibaem) / dble(TP2_ibaem + TN2_ibaem + FP2_ibaem + FN2_ibaem)
  else
     accuracy2_ibaem = 0d0
  endif

  if (TP2_ibap + TN2_ibap + FP2_ibap + FN2_ibap > 0) then
     accuracy2_ibap = dble(TP2_ibap + TN2_ibap) / dble(TP2_ibap + TN2_ibap + FP2_ibap + FN2_ibap)
  else
     accuracy2_ibap = 0d0
  endif

  if (TP2_ibna + TN2_ibna + FP2_ibna + FN2_ibna > 0) then
     accuracy2_ibna = dble(TP2_ibna + TN2_ibna) / dble(TP2_ibna + TN2_ibna + FP2_ibna + FN2_ibna)
  else
     accuracy2_ibna = 0d0
  endif

  write(debug_unit,*) "Retake Accuracy - IBAEM: ", accuracy2_ibaem
  write(debug_unit,*) "Retake Accuracy - IBAP: ", accuracy2_ibap
  write(debug_unit,*) "Retake Accuracy - IBNA: ", accuracy2_ibna

  ! At the very end, before deallocating, write a final debug message:
  write(debug_unit,*) "=== Debug End: Subroutine completed successfully ==="
  flush(debug_unit)

  ! Optionally, close the debug file here if you want to release the unit:
  close(debug_unit)

deallocate(reasonDiff, score100, scoreNov, retake_probability, retake_decision, failReasonsObs,  &
              temp, totals, finalGrades, cnt, student_totals, hl_totals, sl_totals, passFlag,       &
              failCounter, tk_ee_count, grade_23_count, temp_retake, totals_retake,                 &
              finalGrades_retake, hl_totals_retake, sl_totals_retake, student_totals_retake,        &
              failReasonsObsRetake, cnt_retake, tk_ee_count_retake, grade_23_count_retake,          &
              finalGrades_retake_error, hl_totals_retake_error, sl_totals_retake_error,             &
              student_totals_retake_error, tk_ee_count_retake_error )
  return
  end subroutine add_error_scale_total_and_assign_retake3

  subroutine init_fortran_seed_retake3(ierr)
  implicit none
  integer, intent(out) :: ierr
  integer :: n, i
  integer, allocatable :: seed(:)
  ierr = 0
  call random_seed(size = n)
  allocate(seed(n))
  do i = 1, n
     seed(i) = i + 1234
  end do
  call random_seed(PUT = seed)
  deallocate(seed)
  return
  end subroutine init_fortran_seed_retake3
