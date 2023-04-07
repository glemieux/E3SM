module dynLandUseMod

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
  real(r8), allocatable, public :: landuse_transitions(:,:) ! land use areas
  integer, public, parameter :: num_landuse_transition_vars = 2
  character(len=64), public, parameter :: landuse_transition_varnames(num_landuse_transition_vars) = &
       [character(len=64) :: 'PRIMF_TO_SECDN', 'SECDN_TO_SECDf']
  type(dyn_var_time_uninterp_type) :: landuse_transition_vars(num_landuse_transition_vars) ! value of each landuse variable

  public :: dynLandUseInit
  public :: dynLandUseInterp

contains

  !-----------------------------------------------------------------------
  subroutine dynLandUseInit(bounds, landuse_filename)

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
    character(len=*), parameter :: subname = 'dynLandUseInit'
    !-----------------------------------------------------------------------

    SHR_ASSERT_ALL(bounds%level == BOUNDS_LEVEL_PROC, subname // ': argument must be PROC-level bounds')

    if (use_cn) return ! Use this as a protection in lieu of build namelist check?

    ! Allocate and initialize the land use array
    allocate(landuse_transitions(num_landuse_transition_vars,bounds%begg:bounds%endg),stat=ierr)
    landuse_transitions = 0._r8
    if (ier /= 0) then
       call endrun(msg=' allocation error for landuse_transitions'//errMsg(__FILE__, __LINE__))
    end if

    ! Generate the dyn_file_type object
    ! TO DO: check whether to initialize with start or end
    dynLandUse_file = dyn_file_type(landuse_filename, YEAR_POSITION_START_OF_TIMESTEP)

    ! TO DO: replicate this for each of the landuse types
    ! Get initial land use data
    num_points = (bounds%endg - bounds%begg + 1)
    landuse_shape(1) = num_points
    do varnum = 1, num_landuse_transition_vars
       landuse_transition_vars(varnum) = dyn_var_time_uninterp_type( &
            dyn_file=dynLandUse_file, varname=landuse_transition_varnames(varnum), &
            dim1name=grlnd, conversion_factor=1.0_r8, &
            do_check_sums_equal_1=.false., data_shape=landuse_shape)
    end do

  end subroutine dynLandUseInit


  !-----------------------------------------------------------------------
  subroutine dynLandUseInterp
  end subroutine dynLandUseInterp

end module dynLandUseMod
