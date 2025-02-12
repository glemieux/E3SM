module dynEDMod

  !-----------------------------------------------------------------------
  ! !USES:
  use shr_kind_mod   , only : r8 => shr_kind_r8
  use decompMod      , only : bounds_type
  use landunit_varcon, only : istsoil
  use VegetationType      , only : veg_pp
  use ColumnType     , only : col_pp
  use elm_varctl, only : use_fates_multicolumn
  use elm_varctl, only : iulog

  !
  ! !PUBLIC MEMBER FUNCTIONS:
  implicit none
  private
  save

  public :: dyn_ED     ! transfers weights calculated internally by ED into wtcol. 
  !------------------------------------------------------------------------
 
contains

  !------------------------------------------------------------------------
  subroutine dyn_ED( bounds )
    !
    ! !ARGUMENTS:
    type(bounds_type), intent(in) :: bounds  ! bounds
    
    ! !LOCAL VARIABLES:
    integer  ::  p,c           ! indices
    !------------------------------------------------------------------------
    
    do p = bounds%begp,bounds%endp
       c = veg_pp%column(p)
       if (col_pp%itype(c) == istsoil) then
          if ( veg_pp%is_veg(p) .or. veg_pp%is_bareground(p)) then
             ! If using singlesite multicolumn option, fates has only one patch
             ! per column and we leave the wtcol as one for all patches and
             ! directly manipulate the column weight relative to the landunit
             if (trim(use_fates_multicolumn) == "singlesite") then
                col_pp%wtlunit(c) = veg_pp%wt_fates(p)
             else
                veg_pp%wtcol(p) = veg_pp%wt_fates(p)
             end if
          else
             if (trim(use_fates_multicolumn) .ne. "singlesite") then
                veg_pp%wtcol(p) = 0.0_r8 
             end if
          end if
       end if
    end do

  end subroutine dyn_ED

end module dynEDMod
