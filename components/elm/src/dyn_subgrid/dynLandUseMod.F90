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
  use ColumnType            , only : col_pp

  implicit none

  private

  integer, public, parameter :: num_landuse_vars = 2
  character(len=64), public, parameter :: landuse_varnames(num_landuse_vars) = &
       [character(len=64) :: 'PRIMF_TO_SECDN', 'SECDN_TO_SECDf']

  type(dyn_var_time_uninterp_type) :: landuse_vars(num_landuse_vars)   ! value of each harvest variable

  public :: dynLandUseInit
  public :: dynLandUseInterp

contains

  !-----------------------------------------------------------------------
  subroutine dynLandUseInit(bounds, landuse_filename)

    ! !DESCRIPTION:
    ! Initialize data structures for land use information.

    ! !USES:
    use elm_varctl            , only : use_cn, use_fates
    use dynVarTimeUninterpMod , only : dyn_var_time_uninterp_type
    use dynTimeInfoMod        , only : YEAR_POSITION_START_OF_TIMESTEP
    use dynTimeInfoMod        , only : YEAR_POSITION_END_OF_TIMESTEP

    ! !ARGUMENTS:
    type(bounds_type), intent(in) :: bounds        ! proc-level bounds
    character(len=*) , intent(in) :: landuse_filename  ! name of file containing land use information

    ! !LOCAL VARIABLES
    integer :: varnum     ! counter for harvest variables
    integer :: num_points ! number of spatial points
    integer :: ier        ! error code
    !
    character(len=*), parameter :: subname = 'dynHarvest_init'
    !-----------------------------------------------------------------------

    SHR_ASSERT_ALL(bounds%level == BOUNDS_LEVEL_PROC, subname // ': argument must be PROC-level bounds')

    if (use_cn) return ! Use this as a protection in lieu of build namelist check?

    ! Generate the dyn_file_type object
    dynLandUse_file = dyn_file_type(landuse_filename, YEAR_POSITION_END_OF_TIMESTEP)

    ! Get initial land use data
    num_points = (bounds%endg - bounds%begg + 1)
    do varnum = 1, num_landuse_vars
       landuse_vars(varnum) = dyn_var_time_uninterp_type( &
            dyn_file=dynLandUse_file, varname=landuse_varnames(varnum), &
            dim1name=grlnd, conversion_factor=1.0_r8, &
            do_check_sums_equal_1=.false., data_shape=landuse_shape)
    end do

  end subroutine dynLandUseInit


  !-----------------------------------------------------------------------
  subroutine dynLandUseInterp
  end subroutine dynLandUseInterp

end module dynLandUseMod
