module dynFATESLandUseChangeMod

#include "shr_assert.h"

  !---------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Handle reading of the land use harmonization (LUH2) dataset

  ! !USES:
  use shr_kind_mod          , only : r8 => shr_kind_r8
  use shr_log_mod           , only : errMsg => shr_log_errMsg
  use decompMod             , only : bounds_type, BOUNDS_LEVEL_PROC
  use abortutils            , only : endrun
  use dynFileMod            , only : dyn_file_type
  use dynVarTimeUninterpMod , only : dyn_var_time_uninterp_type
  use elm_varcon            , only : grlnd

  implicit none

  private

  ! To Do: expand the full list of varnames
  ! TO DO: add state and management arrays
  real(r8), allocatable, public :: landuse_transitions(:,:)           ! land use areas
  integer, public, parameter    :: num_landuse_transition_vars = 2
  type(dyn_file_type), target   :: dynFatesLandUse_file
  !
  character(len=14), public, parameter :: landuse_transition_varnames(num_landuse_transition_vars) = &
       [character(len=14) :: 'PRIMF_TO_SECDN', 'SECDN_TO_SECDf']

  type(dyn_var_time_uninterp_type) :: landuse_transition_vars(num_landuse_transition_vars) ! value of each landuse variable

  public :: dynFatesLandUseInit
  public :: dynFatesLandUseInterp

contains

  !-----------------------------------------------------------------------
  subroutine dynFatesLandUseInit(bounds, landuse_filename)

    ! !DESCRIPTION:
    ! Initialize data structures for land use information.

    ! !USES:
    use elm_varctl            , only : use_cn
    use dynVarTimeUninterpMod , only : dyn_var_time_uninterp_type
    use dynTimeInfoMod        , only : YEAR_POSITION_START_OF_TIMESTEP
    use dynTimeInfoMod        , only : YEAR_POSITION_END_OF_TIMESTEP

    ! !ARGUMENTS:
    type(bounds_type), intent(in) :: bounds        ! proc-level bounds
    character(len=*) , intent(in) :: landuse_filename  ! name of file containing land use information

    ! !LOCAL VARIABLES
    integer :: varnum     ! counter for harvest variables
    integer :: landuse_shape(1)  ! land use shape
    integer :: num_points ! number of spatial points
    integer :: ier        ! error code
    !
    character(len=*), parameter :: subname = 'dynFatesLandUseInit'
    !-----------------------------------------------------------------------

    SHR_ASSERT_ALL(bounds%level == BOUNDS_LEVEL_PROC, subname // ': argument must be PROC-level bounds')

    if (use_cn) return ! Use this as a protection in lieu of build namelist check?

    ! Allocate and initialize the land use array
    allocate(landuse_transitions(num_landuse_transition_vars,bounds%begg:bounds%endg),stat=ier)
    landuse_transitions = 0._r8
    if (ier /= 0) then
       call endrun(msg=' allocation error for landuse_transitions'//errMsg(__FILE__, __LINE__))
    end if

    ! Generate the dyn_file_type object
    ! TO DO: check whether to initialize with start or end
    dynFatesLandUse_file = dyn_file_type(landuse_filename, YEAR_POSITION_START_OF_TIMESTEP)

    ! TO DO: replicate this for each of the landuse types
    ! Get initial land use data
    num_points = (bounds%endg - bounds%begg + 1)
    landuse_shape(1) = num_points
    do varnum = 1, num_landuse_transition_vars
       landuse_transition_vars(varnum) = dyn_var_time_uninterp_type( &
            dyn_file=dynFatesLandUse_file, varname=landuse_transition_varnames(varnum), &
            dim1name=grlnd, conversion_factor=1.0_r8, &
            do_check_sums_equal_1=.false., data_shape=landuse_shape)
    end do

  end subroutine dynFatesLandUseInit


  !-----------------------------------------------------------------------
  subroutine dynFatesLandUseInterp(bounds, do_landuse_update)

    use dynTimeInfoMod , only : time_info_type
    use elm_varctl     , only : use_cn

    ! !ARGUMENTS:
    type(bounds_type), intent(in)  :: bounds            ! proc-level bounds
    logical          , intent(out) :: do_landuse_update ! land use update flag

    ! !LOCAL VARIABLES:
    integer               :: varnum       ! counter for harvest variables
    real(r8), allocatable :: this_data(:) ! data for a single harvest variable
    character(len=*), parameter :: subname = 'dynFatesLandUseInterp'
    !-----------------------------------------------------------------------
    SHR_ASSERT_ALL(bounds%level == BOUNDS_LEVEL_PROC, subname // ': argument must be PROC-level bounds')

    ! This shouldn't be called by cn currently, but return if it is
    if (use_cn) return ! Use this as a protection in lieu of build namelist check?

    ! input land use data for current year are stored in year+1 in the file
    call dynFatesLandUse_file%time_info%set_current_year_get_year(1)

    if (dynFatesLandUse_file%time_info%is_before_time_series()) then
       ! Set the land use flag to false to avoid this update step in elmfates_interface call
       do_landuse_update = .false.

       ! Reset the land use transitions to zero for safety
       landuse_transitions(1:num_landuse_tranisition_vars,bounds%begg:bounds%endg) = 0._r8
    else
       do_landuse_update = .true.

       ! Right now we don't account for the topounits
       allocate(this_data(bounds%begg:bounds%endg))
       do varnum = 1, num_landuse_transitions_vars
          call landuse_transition_vars(varnum)%get_current_data(this_data)
          landuse_transitions(varnum,bounds%begg:bounds%endg) = this_data(bounds%begg:bounds%endg)
       end do
       deallocate(this_data)
    end if

  end subroutine dynFatesLandUseInterp

end module dynFATESLandUseChangeMod
