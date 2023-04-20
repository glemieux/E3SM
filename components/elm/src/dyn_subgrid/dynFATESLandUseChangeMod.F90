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

  real(r8), allocatable, public :: landuse_transitions(:,:)
  real(r8), allocatable, public :: landuse_states(:,:)

  integer, public, parameter    :: num_landuse_transition_vars = 108
  integer, public, parameter    :: num_landuse_state_vars = 12

  type(dyn_file_type), target   :: dynFatesLandUse_file

  ! Land use name arrays
  character(len=5), public, parameter  :: landuse_state_varnames(num_landuse_state_vars) = &
                    [character(len=5)  :: 'PRIMF','PRIMN','SECDF','SECDN','PASTR','RANGE', &
                                          'URBAN','C3ANN','C4ANN','C3PER','C4PER','C3NFX']

  character(len=14), public, parameter :: landuse_transition_varnames(num_landuse_transition_vars) = &
                    [character(len=14) :: 'PRIMF_TO_SECDN','PRIMF_TO_PASTR','PRIMF_TO_RANGE','PRIMF_TO_URBAN', &
                                          'PRIMF_TO_C3ANN','PRIMF_TO_C4ANN','PRIMF_TO_C3PER','PRIMF_TO_C4PER','PRIMF_TO_C3NFX', &
                                          'PRIMN_TO_SECDF','PRIMN_TO_PASTR','PRIMN_TO_RANGE','PRIMN_TO_URBAN', &
                                          'PRIMN_TO_C3ANN','PRIMN_TO_C4ANN','PRIMN_TO_C3PER','PRIMN_TO_C4PER','PRIMN_TO_C3NFX', &
                                          'SECDF_TO_SECDN','SECDF_TO_PASTR','SECDF_TO_RANGE','SECDF_TO_URBAN', &
                                          'SECDF_TO_C3ANN','SECDF_TO_C4ANN','SECDF_TO_C3PER','SECDF_TO_C4PER','SECDF_TO_C3NFX', &
                                          'SECDN_TO_SECDF','SECDN_TO_PASTR','SECDN_TO_RANGE','SECDN_TO_URBAN', &
                                          'SECDN_TO_C3ANN','SECDN_TO_C4ANN','SECDN_TO_C3PER','SECDN_TO_C4PER','SECDN_TO_C3NFX', &
                                          'PASTR_TO_SECDF','PASTR_TO_SECDN','PASTR_TO_RANGE','PASTR_TO_URBAN', &
                                          'PASTR_TO_C3ANN','PASTR_TO_C4ANN','PASTR_TO_C3PER','PASTR_TO_C4PER','PASTR_TO_C3NFX', &
                                          'RANGE_TO_SECDF','RANGE_TO_SECDN','RANGE_TO_PASTR','RANGE_TO_URBAN', &
                                          'RANGE_TO_C3ANN','RANGE_TO_C4ANN','RANGE_TO_C3PER','RANGE_TO_C4PER','RANGE_TO_C3NFX', &
                                          'URBAN_TO_SECDF','URBAN_TO_SECDN','URBAN_TO_PASTR','URBAN_TO_RANGE', &
                                          'URBAN_TO_C3ANN','URBAN_TO_C4ANN','URBAN_TO_C3PER','URBAN_TO_C4PER','URBAN_TO_C3NFX', &
                                          'C3ANN_TO_C4ANN','C3ANN_TO_C3PER','C3ANN_TO_C4PER','C3ANN_TO_C3NFX', &
                                          'C3ANN_TO_SECDF','C3ANN_TO_SECDN','C3ANN_TO_PASTR','C3ANN_TO_RANGE','C3ANN_TO_URBAN', &
                                          'C4ANN_TO_C3ANN','C4ANN_TO_C3PER','C4ANN_TO_C4PER','C4ANN_TO_C3NFX', &
                                          'C4ANN_TO_SECDF','C4ANN_TO_SECDN','C4ANN_TO_PASTR','C4ANN_TO_RANGE','C4ANN_TO_URBAN', &
                                          'C3PER_TO_C3ANN','C3PER_TO_C4ANN','C3PER_TO_C4PER','C3PER_TO_C3NFX', &
                                          'C3PER_TO_SECDF','C3PER_TO_SECDN','C3PER_TO_PASTR','C3PER_TO_RANGE','C3PER_TO_URBAN', &
                                          'C4PER_TO_C3ANN','C4PER_TO_C4ANN','C4PER_TO_C3PER','C4PER_TO_C3NFX', &
                                          'C4PER_TO_SECDF','C4PER_TO_SECDN','C4PER_TO_PASTR','C4PER_TO_RANGE','C4PER_TO_URBAN', &
                                          'C3NFX_TO_C3ANN','C3NFX_TO_C4ANN','C3NFX_TO_C3PER','C3NFX_TO_C4PER', &
                                          'C3NFX_TO_SECDF','C3NFX_TO_SECDN','C3NFX_TO_PASTR','C3NFX_TO_RANGE','C3NFX_TO_URBAN']

  type(dyn_var_time_uninterp_type) :: landuse_transition_vars(num_landuse_transition_vars) ! value of each landuse variable
  type(dyn_var_time_uninterp_type) :: landuse_state_vars(num_landuse_state_vars)           ! value of each landuse variable

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

    ! Allocate and initialize the land use arrays
    allocate(landuse_states(num_landuse_state_vars,bounds%begg:bounds%endg),stat=ier)
    if (ier /= 0) then
       call endrun(msg=' allocation error for landuse_states'//errMsg(__FILE__, __LINE__))
    end if
    allocate(landuse_transitions(num_landuse_transition_vars,bounds%begg:bounds%endg),stat=ier)
    if (ier /= 0) then
       call endrun(msg=' allocation error for landuse_transitions'//errMsg(__FILE__, __LINE__))
    end if

    landuse_states= 0._r8
    landuse_transitions = 0._r8

    ! Generate the dyn_file_type object
    ! TO DO: check whether to initialize with start or end
    dynFatesLandUse_file = dyn_file_type(landuse_filename, YEAR_POSITION_START_OF_TIMESTEP)

    ! Get initial land use data
    num_points = (bounds%endg - bounds%begg + 1)
    landuse_shape(1) = num_points ! Does this need an explicit array shape to be passed to the constructor?
    do varnum = 1, num_landuse_transition_vars
       landuse_transition_vars(varnum) = dyn_var_time_uninterp_type( &
            dyn_file=dynFatesLandUse_file, varname=landuse_transition_varnames(varnum), &
            dim1name=grlnd, conversion_factor=1.0_r8, &
            do_check_sums_equal_1=.false., data_shape=landuse_shape)
    end do
    do varnum = 1, num_landuse_state_vars
       landuse_transition_vars(varnum) = dyn_var_time_uninterp_type( &
            dyn_file=dynFatesLandUse_file, varname=landuse_state_varnames(varnum), &
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
       landuse_transitions(1:num_landuse_transition_vars,bounds%begg:bounds%endg) = 0._r8
       landuse_states(1:num_landuse_state_vars,bounds%begg:bounds%endg) = 0._r8
    else
       do_landuse_update = .true.

       ! Right now we don't account for the topounits
       allocate(this_data(bounds%begg:bounds%endg))
       do varnum = 1, num_landuse_transition_vars
          call landuse_transition_vars(varnum)%get_current_data(this_data)
          landuse_transitions(varnum,bounds%begg:bounds%endg) = this_data(bounds%begg:bounds%endg)
       end do
       do varnum = 1, num_landuse_state_vars
          call landuse_state_vars(varnum)%get_current_data(this_data)
          landuse_states(varnum,bounds%begg:bounds%endg) = this_data(bounds%begg:bounds%endg)
       end do
       deallocate(this_data)
    end if

  end subroutine dynFatesLandUseInterp

end module dynFATESLandUseChangeMod
