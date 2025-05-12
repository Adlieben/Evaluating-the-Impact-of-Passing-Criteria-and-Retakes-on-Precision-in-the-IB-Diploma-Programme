! -------------------------------------------------------------------------------------
! simulation_module_rule5.f90
! Rule 5 = No HL/SL checks, no maximum 3s and total <18 => fail (instead of <24).
! Conditions 2,3,7 are removed. Condition 1 changed to 18.
! -------------------------------------------------------------------------------------

subroutine compute_true_passFlag5( &
    data, diagVar, scaleFactor, mask, group, nrows, ncols, nGroups, &
    nBound, nBoundMax, bound_ub, grade_val, &
    totals, finalGrades, tk_ee_flag, hl_flag, sl_flag, &
    bonus_length, bonus_codes, bonus_points, &
    observed_student_totals, observed_hl_totals, observed_sl_totals, &
    observed_passFlag, failReasonsReal, &
    componentScoresReal, groupTotalsReal, subjectGradesReal, &
    ierr)

  implicit none

  ! -------------------------------------------------------------------------
  ! Inputs
  ! -------------------------------------------------------------------------
  integer, intent(in)               :: nrows, ncols, nGroups, nBoundMax, bonus_length
  integer, intent(in)               :: mask(nrows, ncols)
  integer, intent(in)               :: group(ncols)
  integer, intent(in)               :: nBound(nGroups)
  integer, intent(in)               :: tk_ee_flag(nGroups)
  integer, intent(in)               :: hl_flag(nGroups)
  integer, intent(in)               :: sl_flag(nGroups)
  integer, intent(in)               :: bonus_codes(bonus_length)
  integer, intent(in)               :: bonus_points(bonus_length)
  real*8, intent(in)                :: data(nrows, ncols)
  real*8, intent(in)                :: diagVar(ncols)
  real*8, intent(in)                :: scaleFactor(ncols)
  real*8, intent(in)                :: bound_ub(nBoundMax, nGroups)
  real*8, intent(in)                :: grade_val(nBoundMax, nGroups)

  ! -------------------------------------------------------------------------
  ! Outputs
  ! -------------------------------------------------------------------------
  real*8, intent(out)               :: totals(nrows, nGroups)
  real*8, intent(out)               :: finalGrades(nrows, nGroups)
  real*8, intent(out)               :: observed_student_totals(nrows)
  real*8, intent(out)               :: observed_hl_totals(nrows)
  real*8, intent(out)               :: observed_sl_totals(nrows)
  integer, intent(out)              :: observed_passFlag(nrows)
  integer, intent(out)              :: failReasonsReal(nrows, 7)
  integer, intent(out)              :: ierr

  ! Additional debug arrays
  real*8, intent(out) :: componentScoresReal(nrows, ncols)
  real*8, intent(out) :: groupTotalsReal(nrows, nGroups)
  real*8, intent(out) :: subjectGradesReal(nrows, nGroups)

  ! -------------------------------------------------------------------------
  ! Local arrays
  ! -------------------------------------------------------------------------
  real*8, allocatable :: temp(:,:)
  integer, allocatable :: cnt(:,:)
  integer, allocatable :: tk_ee_count(:)

  ! Locals
  integer :: i, j, g, k
  integer :: tk_grade, ee_grade, bonus, combined_grade
  integer :: grade2count, grade23count
  integer :: ierr_local

  ! -------------------------------------------------------------------------
  ! Step 0: Initialization
  ! -------------------------------------------------------------------------
  ierr_local = 0
  allocate(temp(nrows, ncols))
  allocate(cnt(nrows, nGroups))
  allocate(tk_ee_count(nrows))

  ! Copy data
  do i = 1, nrows
     do j = 1, ncols
        temp(i,j) = data(i,j)
     end do
  end do

  ! -------------------------------------------------------------------------
  ! Step 1: Round + scale (no random error)
  ! -------------------------------------------------------------------------
  do j = 1, ncols
     do i = 1, nrows
        if (mask(i,j) == 1) then
           temp(i,j) = dnint(temp(i,j))
           temp(i,j) = temp(i,j) * scaleFactor(j)
        else
           temp(i,j) = 0.0d0
        end if
     end do
  end do

  ! -------------------------------------------------------------------------
  ! Step 2: sum scaled values by group
  ! -------------------------------------------------------------------------
  totals = 0.0d0
  cnt = 0
  do j = 1, ncols
     g = group(j)
     if (g < 1 .or. g > nGroups) then
        ierr_local = 1
        deallocate(temp, cnt, tk_ee_count)
        ierr = ierr_local
        return
     end if
     do i = 1, nrows
        if (mask(i,j) == 1) then
           totals(i,g) = totals(i,g) + temp(i,j)
           cnt(i,g)    = cnt(i,g) + 1
        end if
     end do
  end do

  do i = 1, nrows
     do g = 1, nGroups
        if (cnt(i,g) == 0) then
           totals(i,g) = -999.d0
        else
           totals(i,g) = dnint(totals(i,g))
        end if
     end do
  end do

  ! -------------------------------------------------------------------------
  ! Step 3: Assign final grades
  ! -------------------------------------------------------------------------
  do i = 1, nrows
     do g = 1, nGroups
        if (totals(i,g) == -999.d0) then
           finalGrades(i,g) = -999.d0
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
  ! Step 4: Summation for pass/fail logic
  ! -------------------------------------------------------------------------
  do i = 1, nrows
     observed_student_totals(i) = 0.0d0
     tk_grade = -999
     ee_grade = -999
     tk_ee_count(i) = 0

     do g = 1, nGroups
        if (totals(i,g) == -999.d0) cycle
        if (tk_ee_flag(g) == 1) then
           if (tk_grade == -999) then
              tk_grade = int(finalGrades(i,g))
           else
              ee_grade = int(finalGrades(i,g))
           end if
           tk_ee_count(i) = tk_ee_count(i) + 1
        else
           observed_student_totals(i) = observed_student_totals(i) + finalGrades(i,g)
        end if
     end do

     if ((tk_grade /= -999).and.(ee_grade /= -999)) then
        combined_grade = tk_grade*10 + ee_grade
        bonus = -999
        do k = 1, bonus_length
           if (combined_grade == bonus_codes(k)) then
              bonus = bonus_points(k)
              exit
           end if
        end do
        if (bonus /= -999) then
           observed_student_totals(i) = observed_student_totals(i) + bonus
        end if
     end if
  end do

  ! -------------------------------------------------------------------------
  ! Step 4.1: Compute HL, SL totals
  ! -------------------------------------------------------------------------
  do i = 1, nrows
     observed_hl_totals(i) = 0.d0
     observed_sl_totals(i) = 0.d0
     do g = 1, nGroups
        if (totals(i,g) == -999.d0) cycle
        if (hl_flag(g) == 1) observed_hl_totals(i) = observed_hl_totals(i) + finalGrades(i,g)
        if (sl_flag(g) == 1) observed_sl_totals(i) = observed_sl_totals(i) + finalGrades(i,g)
     end do
  end do

  ! -------------------------------------------------------------------------
  ! Step 5: Rule 4 pass/fail
  !   Condition 1 => total<18 => fail
  !   Condition 2,3 => removed (no HL or SL checks)
  !   Condition 4 => grade=1 => fail
  !   Condition 5 => incomplete TK/EE => fail
  !   Condition 6 => >=2 of '2'
  !   Condition 7 => >=3 of '2 or 3'
  ! -------------------------------------------------------------------------
  do i = 1, nrows
     observed_passFlag(i) = 1
     failReasonsReal(i,:) = 0

     ! Condition 1 => total<18
     if (observed_student_totals(i) < 18.d0) then
        failReasonsReal(i,1) = 1
     end if

     ! Condition 4 => any=1 => fail
     do g = 1, nGroups
        if (finalGrades(i,g) == 1) then
           failReasonsReal(i,4) = 1
           exit
        end if
     end do

     ! Condition 5 => incomplete TK/EE => fail
     if (tk_ee_count(i) /= 2) then
        failReasonsReal(i,5) = 1
     end if

     ! Condition 6
     grade2count = 0
     grade23count = 0
     do g = 1, nGroups
        if (finalGrades(i,g) == 2) then
           grade2count = grade2count + 1
           grade23count = grade23count + 1
        else if (finalGrades(i,g) == 3) then
           grade23count = grade23count + 1
        end if
     end do
     if (grade2count > 2) then
        failReasonsReal(i,6) = 1
     end if
     if (sum(failReasonsReal(i,:)) > 0) then
        observed_passFlag(i) = 0
     end if
  end do

  ! -------------------------------------------------------------------------
  ! Copy debug arrays
  ! -------------------------------------------------------------------------
  do i = 1, nrows
     do j = 1, ncols
        componentScoresReal(i,j) = temp(i,j)
     end do
     do g = 1, nGroups
        groupTotalsReal(i,g)   = totals(i,g)
        subjectGradesReal(i,g) = finalGrades(i,g)
     end do
  end do

  ierr = 0
  deallocate(temp, cnt, tk_ee_count)
  return
end subroutine compute_true_passFlag5


subroutine add_error_scale_total_and_assign5( &
     data, diagVar, scaleFactor, mask, group, nrows, ncols, nGroups, &
     nBound, nBoundMax, bound_ub, grade_val, &
     tk_ee_flag, hl_flag, sl_flag, bonus_length, bonus_codes, bonus_points, &
     observed_passFlag, &
     failReasonsReal,   &
     failReasonsObs,    &
     reasonDiff,        &
     sensitivity, specificity, ppv, npv, accuracy, ierr)

  implicit none

  ! -------------------------------------------------------------------------
  ! Inputs
  ! -------------------------------------------------------------------------
  integer, intent(in)               :: nrows, ncols, nGroups, nBoundMax, bonus_length
  integer, intent(in)               :: mask(nrows, ncols)
  integer, intent(in)               :: group(ncols)
  integer, intent(in)               :: nBound(nGroups)
  integer, intent(in)               :: tk_ee_flag(nGroups)
  integer, intent(in)               :: hl_flag(nGroups)
  integer, intent(in)               :: sl_flag(nGroups)
  integer, intent(in)               :: bonus_codes(bonus_length)
  integer, intent(in)               :: bonus_points(bonus_length)
  integer, intent(in)               :: observed_passFlag(nrows)
  integer, intent(in)               :: failReasonsReal(nrows, 7)
  real*8, intent(in)                :: data(nrows, ncols)
  real*8, intent(in)                :: diagVar(ncols)
  real*8, intent(in)                :: scaleFactor(ncols)
  real*8, intent(in)                :: bound_ub(nBoundMax, nGroups)
  real*8, intent(in)                :: grade_val(nBoundMax, nGroups)

  ! -------------------------------------------------------------------------
  ! Outputs
  ! -------------------------------------------------------------------------
  integer, intent(out)              :: failReasonsObs(nrows, 7)
  integer, intent(out)              :: reasonDiff(7)

  real*8, intent(out)               :: sensitivity
  real*8, intent(out)               :: specificity
  real*8, intent(out)               :: ppv
  real*8, intent(out)               :: npv
  real*8, intent(out)               :: accuracy
  integer, intent(out)              :: ierr

  ! -------------------------------------------------------------------------
  ! Local arrays
  ! -------------------------------------------------------------------------
  real*8, allocatable :: temp(:,:)
  real*8, allocatable :: totals(:,:)
  real*8, allocatable :: finalGrades(:,:)
  real*8, allocatable :: student_totals(:)
  real*8, allocatable :: hl_totals(:)
  real*8, allocatable :: sl_totals(:)
  integer, allocatable :: passFlag(:)
  integer, allocatable :: failCounter(:)
  integer, allocatable :: tk_ee_count(:)
  integer, allocatable :: cnt(:,:)
  integer, allocatable :: grade_23_count(:)

  ! Additional local
  integer :: i, j, g, k
  real*8 :: u1, u2, r, theta, z, dtemp
  integer :: totalCount, tk_grade, ee_grade, bonus, combined_grade
  integer :: TP, FP, FN, TN
  integer :: grade2count, grade23count
  integer :: ierr_local

  ! Step 0
  ierr_local=0
  reasonDiff=0
  totalCount=0

  allocate(temp(nrows, ncols))
  allocate(totals(nrows, nGroups))
  allocate(finalGrades(nrows, nGroups))
  allocate(cnt(nrows, nGroups))
  allocate(student_totals(nrows))
  allocate(hl_totals(nrows))
  allocate(sl_totals(nrows))
  allocate(passFlag(nrows))
  allocate(failCounter(nrows))
  allocate(tk_ee_count(nrows))
  allocate(grade_23_count(nrows))

  ! Copy data
  do i=1,nrows
     do j=1,ncols
        temp(i,j)=data(i,j)
     end do
  end do

  ! Step 1: add measurement error + scale
  do j=1,ncols
     do i=1,nrows
        if (mask(i,j)==1) then
           call random_number(u1)
           call random_number(u2)
           if (u1<1d-12) u1=1d-12
           r=dsqrt(-2d0*dlog(u1))
           theta=2d0*3.141592653589793d0*u2
           z=r*dcos(theta)
           dtemp=temp(i,j)+dsqrt(diagVar(j))*z
           temp(i,j)=dnint(dtemp)
           temp(i,j)=temp(i,j)*scaleFactor(j)
           totalCount=totalCount+1
        else
           temp(i,j)=0.d0
        end if
     end do
  end do

  ! Step 2: sum by group
  do i=1,nrows
     do g=1,nGroups
        totals(i,g)=0.d0
     end do
  end do

  do j=1,ncols
     g=group(j)
     if (g<1.or.g>nGroups) then
        ierr_local=1
        deallocate(temp, totals, finalGrades, cnt, student_totals, hl_totals, sl_totals, &
                   passFlag, failCounter, tk_ee_count, grade_23_count)
        ierr=ierr_local
        return
     end if
     do i=1,nrows
        if (mask(i,j)==1) then
           totals(i,g)=totals(i,g)+temp(i,j)
           cnt(i,g)=cnt(i,g)+1
        end if
     end do
  end do

  do i=1,nrows
     do g=1,nGroups
        if (cnt(i,g)==0) then
           totals(i,g)=-999.d0
        else
           totals(i,g)=dnint(totals(i,g))
        end if
     end do
  end do

  ! Step 3: final grades
  do i=1,nrows
     do g=1,nGroups
        if (totals(i,g)==-999.d0) then
           finalGrades(i,g)=-999.d0
        else
           finalGrades(i,g)=grade_val(1,g)
           do k=1,nBound(g)
              if (totals(i,g)>bound_ub(k,g)) then
                 finalGrades(i,g)=grade_val(k+1,g)
              else
                 exit
              end if
           end do
        end if
     end do
  end do

  ! Step 4: student totals
  do i=1,nrows
     student_totals(i)=0.d0
     tk_grade=-999
     ee_grade=-999
     tk_ee_count(i)=0

     do g=1,nGroups
        if (totals(i,g)==-999.d0) cycle
        if (tk_ee_flag(g)==1) then
           if (tk_grade==-999) then
              tk_grade=int(finalGrades(i,g))
           else
              ee_grade=int(finalGrades(i,g))
           end if
           tk_ee_count(i)=tk_ee_count(i)+1
        else
           student_totals(i)=student_totals(i)+finalGrades(i,g)
        end if
     end do

     if ((tk_grade/=-999).and.(ee_grade/=-999)) then
        combined_grade=tk_grade*10+ee_grade
        bonus=-999
        do k=1,bonus_length
           if (combined_grade==bonus_codes(k)) then
              bonus=bonus_points(k)
              exit
           end if
        end do
        if (bonus/=-999) then
           student_totals(i)=student_totals(i)+bonus
        end if
     end if
  end do

  ! Step 4.1: HL, SL totals
  do i=1,nrows
     hl_totals(i)=0.d0
     sl_totals(i)=0.d0
     do g=1,nGroups
        if (totals(i,g)==-999.d0) cycle
        if (hl_flag(g)==1) hl_totals(i)=hl_totals(i)+finalGrades(i,g)
        if (sl_flag(g)==1) sl_totals(i)=sl_totals(i)+finalGrades(i,g)
     end do
  end do

  ! Step 5: Rule 4 pass/fail
  !  Condition 1 => <18
  !  Condition 2,3 => removed
  !  Condition 4 => any=1
  !  Condition 5 => TK/EE incomplete
  !  Condition 6 => >=2 of '2'
  !  Condition 7 => >=3 of '2 or 3'
  do i=1,nrows
     passFlag(i)=1
     failCounter(i)=0
     grade_23_count(i)=0
     do k=1,7
        failReasonsObs(i,k)=0
     end do

     ! Condition 1 => total<18
     if (student_totals(i)<18.d0) then
        failReasonsObs(i,1)=1
     end if

     ! Condition 4 => any=1 => fail
     do g=1,nGroups
        if (finalGrades(i,g)==1) then
           failReasonsObs(i,4)=1
           exit
        end if
     end do

     ! Condition 5 => TK/EE incomplete
     if (tk_ee_count(i)/=2) then
        failReasonsObs(i,5)=1
     end if

     ! Condition 6
     grade2count=0
     grade23count=0
     do g=1,nGroups
        if (finalGrades(i,g)==2) then
           grade2count=grade2count+1
           grade_23_count(i)=grade_23_count(i)+1
        else if (finalGrades(i,g)==3) then
           grade_23_count(i)=grade_23_count(i)+1
        end if
     end do
     if (grade2count>2) then
        failReasonsObs(i,6)=1
     end if

     if (sum(failReasonsObs(i,:))>0) then
        passFlag(i)=0
     end if
  end do

  ! Step 6: compare reasons => reasonDiff
  do i=1,nrows
     do k=1,7
        if (failReasonsReal(i,k)/=failReasonsObs(i,k)) then
           reasonDiff(k)=reasonDiff(k)+1
        end if
     end do
  end do

  ! Step 7: confusion matrix
  TP=0; FP=0; FN=0; TN=0
  do i=1,nrows
     if (passFlag(i)==1 .and. observed_passFlag(i)==1) then
        TP=TP+1
     else if (passFlag(i)==1 .and. observed_passFlag(i)==0) then
        FP=FP+1
     else if (passFlag(i)==0 .and. observed_passFlag(i)==1) then
        FN=FN+1
     else if (passFlag(i)==0 .and. observed_passFlag(i)==0) then
        TN=TN+1
     end if
  end do

  sensitivity=0.d0
  specificity=0.d0
  ppv=0.d0
  npv=0.d0
  accuracy=0.d0

  if (TP+FN>0) sensitivity=dble(TP)/dble(TP+FN)
  if (TN+FP>0) specificity=dble(TN)/dble(TN+FP)
  if (TP+FP>0) ppv=dble(TP)/dble(TP+FP)
  if (TN+FN>0) npv=dble(TN)/dble(TN+FN)
  if ((TP+TN+FP+FN)>0) accuracy=dble(TP+TN)/dble(TP+TN+FP+FN)

  ierr=0
  deallocate(temp, totals, finalGrades, cnt, student_totals, hl_totals, sl_totals, &
             passFlag, failCounter, tk_ee_count, grade_23_count)
  return
end subroutine add_error_scale_total_and_assign5

subroutine check_seed_size5(n, ierr)
   ! Returns the required length for the Fortran random seed array.
   implicit none
   integer, intent(out) :: n      ! The required length
   integer, intent(out) :: ierr   ! Error flag

   ierr = 0
   call random_seed(size = n)
end subroutine check_seed_size5

subroutine init_fortran_seed5(ierr)
  ! A small subroutine that sets the Fortran random seed
  ! with no big arrays or dimension mess.
  implicit none
  integer, intent(out) :: ierr

  integer :: n, i
  integer, allocatable :: seed(:)

  ierr = 0
  ! 1) Ask runtime how many integers are required:
  call random_seed(size = n)

  ! 2) Allocate that many:
  allocate(seed(n))

  ! 3) Fill them with non-zero integers.
  do i = 1, n
    seed(i) = i + 1234
  end do

  ! 4) Finally, call random_seed(PUT=seed)
  call random_seed(PUT = seed)

  ! 5) Deallocate local array
  deallocate(seed)
end subroutine init_fortran_seed5