module restart_dynamics
  use shr_kind_mod, only: r8 => shr_kind_r8

  use dyn_comp,    only : dyn_import_t, dyn_export_t
  use pio,         only : var_desc_t
  use spmd_utils,  only : iam
  use cam_logfile, only : iulog
  use control_mod, only : theta_hydrostatic_mode
  use perf_mod,    only : t_startf, t_stopf

  implicit none

  type(var_desc_t) :: udesc, vdesc, tdesc, psdesc, phisdesc, timedesc, &
                      Wdesc, PHINHdesc

  type(var_desc_t) :: FQpsdesc, omegadesc

  type(var_desc_t), pointer :: qdesc_dp(:)

  integer :: ncol_dimid,  nlev_dimid, nlevp_dimid
  public :: init_restart_dynamics

  logical :: initialized=.false.

CONTAINS

  subroutine init_restart_dynamics(File, dyn_out)
    use pio, only : pio_global, pio_unlimited, pio_double, pio_def_dim, &
         pio_put_att, pio_def_var,  &
         pio_setdebuglevel, pio_inq_dimid, file_desc_t
    use cam_pio_utils, only : pio_subsystem
    use dimensions_mod, only : np, ne, nlev, qsize_d, nlevp, nelem , nelemd
    use constituents, only : cnst_name
    use dyn_grid, only : get_horiz_grid_dim_d
    use hycoef, only: init_restart_hycoef

    type(file_desc_t),  intent(inout) :: file
    type(dyn_export_t), intent(in)    :: dyn_out

    integer :: vdimids(2)
    integer :: ierr, i, dummy, j, k, ie
    integer :: timelevels_dimid

    call init_restart_hycoef(File, vdimids)

    call PIO_Setdebuglevel(0)
    ierr = PIO_Def_Dim(File,'ncol_d',nelem*np*np,ncol_dimid)

    ierr = PIO_Def_Dim(File,'timelevels',PIO_UNLIMITED,timelevels_dimid)

    nlev_dimid = vdimids(1)
    nlevp_dimid = vdimids(2)


    ierr = PIO_Put_Att(File, PIO_GLOBAL, 'ne', ne)
    ierr = PIO_Put_Att(File, PIO_GLOBAL, 'np', np)

    ierr = PIO_Def_Var(File, 'time', pio_double, (/timelevels_dimid/), timedesc)

    ierr = PIO_Def_Var(File, 'U', pio_double, (/ncol_dimid, nlev_dimid, timelevels_dimid/), Udesc)
    ierr = PIO_Def_Var(File, 'V', pio_double, (/ncol_dimid, nlev_dimid, timelevels_dimid/), Vdesc)
#ifdef MODEL_THETA_L
    ierr = PIO_Def_Var(File, 'VTheta_dp', pio_double, (/ncol_dimid, nlev_dimid, timelevels_dimid/), Tdesc)
    if( .not. theta_hydrostatic_mode ) then
       ierr = PIO_Def_Var(File, 'W', pio_double, (/ncol_dimid, nlevp_dimid, timelevels_dimid/), Wdesc)
       ierr = PIO_Def_Var(File, 'PHINH', pio_double, (/ncol_dimid, nlevp_dimid, timelevels_dimid/), PHINHdesc)
    endif
#else
    ierr = PIO_Def_Var(File, 'T', pio_double, (/ncol_dimid, nlev_dimid, timelevels_dimid/), Tdesc)
#endif
    !
    ! Omega is not strictly needed, it could be rederived - but this is easier
    !

    ierr = PIO_Def_Var(File, 'OMEGA', pio_double, (/ncol_dimid, nlev_dimid/), omegadesc)
    ierr = PIO_Def_Var(File, 'PS', pio_double, (/ncol_dimid, timelevels_dimid/), PSdesc)
    ierr = PIO_Def_Var(File, 'PHIS', pio_double, (/ncol_dimid/), phisdesc)

    allocate(qdesc_dp(qsize_d))

    do i=1,qsize_d
       ierr = PIO_Def_Var(File,"dp"//cnst_name(i), pio_double, (/ncol_dimid, nlev_dimid, timelevels_dimid/), Qdesc_dp(i))
    end do

  end subroutine init_restart_dynamics


  subroutine write_restart_dynamics(File, dyn_out)
    use pio, only : pio_offset_kind, io_desc_t, pio_double, pio_write_darray, &
         pio_put_var, pio_initdecomp, pio_setframe, &
         pio_freedecomp, pio_enddef, file_desc_t
    use cam_pio_utils, only : pio_subsystem
    use dyn_comp, only : timelevel
    use control_mod, only: qsplit
    use time_mod, only : tstep, TimeLevel_Qdp
    use element_mod, only : element_t
    use dimensions_mod, only : nlev, qsize_d, nlevp, np, ne, nelemd, nelem
    use dof_mod, only : UniquePoints
    use time_manager, only: get_curr_time
    use parallel_mod, only: par
    use hycoef, only: write_restart_hycoef
    use cam_abortutils,   only: endrun

    implicit none
    type(file_desc_t) :: File
    type(dyn_export_t), intent(in)  :: dyn_out

    type(io_desc_t) :: iodesc2d, iodesc3d, iodesc3dp
    integer :: st, ie, k, en, tl, tlQdp, ierr, q
    integer(kind=pio_offset_kind), parameter :: t = 1

    real(kind=r8),pointer :: vartmp(:,:,:), var3d(:,:,:,:), var3dp(:,:,:,:), var2d(:,:,:)
    integer :: i, j

    type(element_t), pointer :: elem(:)
    real(kind=r8) :: time
    integer :: ndcur, nscur
    integer(kind=pio_offset_kind), pointer :: ldof(:)

    call write_restart_hycoef(File)

   if (par%dynproc) then
    elem => dyn_out%elem
   else
    allocate (elem(0))
   endif

    ldof => get_restart_decomp(elem, 1)
    call PIO_InitDecomp(pio_subsystem, pio_double, (/nelem*np*np/) , ldof , iodesc2d)
    deallocate(ldof)

    ldof => get_restart_decomp(elem, nlev)       
    call PIO_InitDecomp(pio_subsystem, pio_double, (/nelem*np*np,nlev/),ldof , iodesc3d)
    deallocate(ldof)

#ifdef MODEL_THETA_L
    if( .not. theta_hydrostatic_mode ) then
       ldof => get_restart_decomp(elem, nlevp)
       call PIO_InitDecomp(pio_subsystem, pio_double, (/nelem*np*np,nlevp/),ldof , iodesc3dp)
       deallocate(ldof)
    endif
#endif
    initialized = .true.
    

    call get_curr_time(ndcur, nscur)

    allocate(vartmp(np,np,nlev))
    en=0
    do ie=1,nelemd
       en=en+elem(ie)%idxp%NumUniquePts
    end do

    allocate(var2d(np,np,nelemd ))

    allocate(var3d(np,np,nelemd,nlev))

#ifdef MODEL_THETA_L
    if( .not. theta_hydrostatic_mode ) then
       allocate(var3dp(np,np,nelemd,nlevp))
    endif
#endif

!$omp parallel do private(ie, j, i)
    do ie=1,nelemd
       do j=1,np
          do i=1,np
             var2d(i,j,ie) = elem(ie)%state%phis(i,j)
          end do
       end do
    end do
    call PIO_Write_Darray(File,Phisdesc,iodesc2d, var2d,ierr)

!$omp parallel do private(ie, k, j, i)
    do ie=1,nelemd
       do k=1,nlev
          do j=1,np
             do i=1,np
                var3d(i,j,ie,k) = elem(ie)%derived%omega_p(i,j,k)
             end do
          end do
       end do
    end do
    call PIO_Write_Darray(File,omegadesc,iodesc3d, var3d,ierr)


    tl = timelevel%n0
    call TimeLevel_Qdp(timelevel, qsplit, tlQdp)
    time = ndcur+real(nscur,kind=r8)/86400._r8

    ierr = pio_put_var(File,timedesc%varid, (/int(t)/), time)

!$omp parallel do private(ie, j, i)
    do ie=1,nelemd
       do j=1,np
          do i=1,np
             var2d(i,j,ie) = elem(ie)%state%ps_v(i,j,tl)
          end do
       end do
    end do
    call PIO_Setframe(File,PSdesc, t)
    call PIO_Write_Darray(File,PSdesc,iodesc2d, var2d,ierr)

    ! Write the U component of Velocity
!$omp parallel do private(ie, k, j, i)
    do ie=1,nelemd
       do k=1,nlev
          do j=1,np
             do i=1,np
                var3d(i,j,ie,k) = elem(ie)%state%V(i,j,1,k,tl)
             end do
          end do
       end do
    end do


    call PIO_Setframe(File,Udesc, t)
    call PIO_Write_Darray(File,Udesc,iodesc3d,var3d,ierr)


       ! Write the V component of Velocity
!$omp parallel do private(ie, k, j, i)
    do ie=1,nelemd
       do k=1,nlev
          do j=1,np
             do i=1,np
                var3d(i,j,ie,k) = elem(ie)%state%V(i,j,2,k,tl)
             end do
          end do
       end do
    end do

    call PIO_Setframe(File,Vdesc, t)
    call PIO_Write_Darray(File,Vdesc,iodesc3d,var3d,ierr)

    ! Write T
!$omp parallel do private(ie, k, j, i)
    do ie=1,nelemd
       do k=1,nlev
          do j=1,np
             do i=1,np
#ifdef MODEL_THETA_L
                var3d(i,j,ie,k) = elem(ie)%state%VTheta_dp(i,j,k,tl)
#else
                var3d(i,j,ie,k) = elem(ie)%state%T(i,j,k,tl)
#endif
             end do
          end do
       end do
    end do

    call PIO_Setframe(File,Tdesc, t)
    call PIO_Write_Darray(File,Tdesc,iodesc3d,var3d,ierr)

#ifdef MODEL_THETA_L    
    if( .not. theta_hydrostatic_mode )then
    ! Write W
!$omp parallel do private(ie, k, j, i)
       do ie=1,nelemd
          do k=1,nlevp
             do j=1,np
                do i=1,np
                   var3dp(i,j,ie,k) = elem(ie)%state%w_i(i,j,k,tl)
                end do
             end do
          end do
       end do
       call PIO_Setframe(File,Wdesc, t)
       call PIO_Write_Darray(File,Wdesc,iodesc3dp,var3dp,ierr)

    ! Write Phi
!$omp parallel do private(ie, k, j, i)
       do ie=1,nelemd
          do k=1,nlevp
             do j=1,np
                do i=1,np
                   var3dp(i,j,ie,k) = elem(ie)%state%phinh_i(i,j,k,tl)
                end do
             end do
          end do
       end do
       call PIO_Setframe(File,PHINHdesc, t)
       call PIO_Write_Darray(File,PHINHdesc,iodesc3dp,var3dp,ierr)
    endif
#endif


    do q=1,qsize_d

       ! Write Q
!$omp parallel do private(ie, k, j, i)
       do ie=1,nelemd
          do k=1,nlev
             do j=1,np
                do i=1,np
                   var3d(i,j,ie,k) = elem(ie)%state%Qdp(i,j,k,q,tlQdp)
                end do
             end do
          end do
       end do
       call PIO_Setframe(File,Qdesc_dp(q), t)
       call PIO_Write_Darray(File,Qdesc_dp(q),iodesc3d,var3d,ierr)

    end do


    deallocate(var3d)
    deallocate(var2d)
    deallocate(qdesc_dp)
    call pio_freedecomp(File, iodesc2d)
    call pio_freedecomp(File, iodesc3d)
#ifdef MODEL_THETA_L
    if( .not. theta_hydrostatic_mode ) then
       deallocate(var3dp)
       call pio_freedecomp(File, iodesc3dp)
    endif
#endif


   if (par%dynproc) then
   else
    deallocate(elem)
   endif  !!  par%dynproc

  end subroutine write_restart_dynamics

  !
  ! Get the integer mapping of a variable in the dynamics decomp in memory.  
  ! The canonical ordering is as on the file. A 0 value indicates that the
  ! variable is not on the file (eg halo or boundary values)
  !

  function get_restart_decomp(elem, lev) result(ldof)
    use element_mod, only : element_t
    use dimensions_mod, only : np, nelemd, nelem
    use pio, only: pio_offset_kind
    type(element_t), intent(in) :: elem(:)
    integer(kind=pio_offset_kind), pointer :: ldof(:)
    integer, intent(in) :: lev

    integer ::  i, j, k, ie

    j=1

    allocate(ldof(nelemd*np*np*lev))
    do k=1,lev
       do ie=1,nelemd
          do i=1,np*np
             ldof(j) = int(elem(ie)%GlobalID-1, pio_offset_kind)*np*np &
                     + int(k-1, pio_offset_kind)*nelem*np*np + i
             j=j+1
          end do
       end do
    end do

  end function get_restart_decomp


  subroutine read_restart_dynamics (File, dyn_in, dyn_out, NLFileName)
    ! for restart and initial condition, timelevel == timelevel_dyn
    ! so we wont update this routine to use both  
    use dyn_comp, only : timelevel
    use parallel_mod, only : initmp, par
    use element_mod, only : element_t
    use pio, only : file_desc_t, pio_global, pio_double, pio_offset_kind, &
         pio_get_att, pio_inq_dimid, pio_inq_dimlen, pio_initdecomp, pio_inq_varid, &
         pio_read_darray, pio_setframe, file_desc_t, io_desc_t, pio_double
    use dyn_comp, only : dyn_init1, dyn_init2, frontgf_idx, frontga_idx
    use phys_grid, only: phys_grid_init
    use physpkg, only: phys_register
    use phys_control, only: use_gw_front
    use physics_buffer, only: pbuf_add_field, dtype_r8
    use ppgrid, only: pcols, pver
    use dimensions_mod, only : nlev, nlevp, np, ne, nelemd, qsize_d
    use cam_abortutils,   only: endrun
    use namelist_mod, only: readnl
    use constituents, only : cnst_name
    use cam_pio_utils, only : pio_subsystem
    use spmd_dyn, only: spmd_readnl
    use control_mod,            only: qsplit
    use time_mod,               only: TimeLevel_Qdp
    use phys_grid_ctem,   only: phys_grid_ctem_reg

    !
    ! Input arguments
    !
    type(File_desc_t), intent(inout) :: File
    type(dyn_import_t), intent(inout)  :: dyn_in
    type(dyn_export_t), intent(inout)  :: dyn_out
    character(len=*), intent(in) :: NLFileName

    type(io_desc_t) :: iodesc2d, iodesc3d, iodesc3dp
    real(r8), allocatable :: var3d(:), var3dp(:), var2d(:)
    integer :: ie, ierr, fne, fnp, fnlev
    integer :: ncols
    integer(kind=pio_offset_kind), pointer :: ldof(:)
    type(element_t), pointer :: elem(:)               ! pointer to dyn_in element array
    integer(kind=pio_offset_kind), parameter :: t = 1
    integer :: i, k, cnt, st, en, tl, tlQdp, ii, jj, s2d, q, j
    integer :: timelevel_dimid, timelevel_chk
    integer :: npes_se
!    type(file_desc_t) :: ncid
!    integer :: ncid


!how to place a check so that someone does not restart nh from hy file?
!is hy restart run from nh ok?

    call t_startf('dyn_init1')
    call dyn_init1(file, NLFileName, dyn_in, dyn_out)
    call t_stopf('dyn_init1')

    ! Define physics data structures
    if(par%masterproc  ) write(iulog,*) 'Running phys_grid_init()'
    call phys_grid_init()

    ! Register zonal average grid for phys TEM diagnostics
    call phys_grid_ctem_reg()

    ! Initialize index values for advected and non-advected tracers
    call phys_register()

    ! Frontogenesis indices
    if (use_gw_front) then
       call pbuf_add_field("FRONTGF", "global", dtype_r8, (/pcols,pver/), &
            frontgf_idx)
       call pbuf_add_field("FRONTGA", "global", dtype_r8, (/pcols,pver/), &
            frontga_idx)
    end if

   if (par%dynproc) then
    elem=>dyn_in%elem
   else
    allocate (elem(0))
   endif

    ierr = PIO_Get_Att(File, PIO_GLOBAL, 'ne', fne)
    ierr = PIO_Get_Att(File, PIO_GLOBAL, 'np', fnp)

    if(ne/=fne .or. np/=fnp) then
       write(iulog,*) 'Restart file np or ne does not match model. np (file, model):',fnp,np,&
            ' ne (file, model) ', fne, ne
       call endrun()
    end if

    ierr = PIO_Inq_DimID(File, 'lev', nlev_dimid)
    ierr = PIO_Inq_dimlen(File, nlev_dimid, fnlev)
    if(ne/=fne .or. np/=fnp) then
       write(iulog,*) 'Restart file nlev does not match model. nlev (file, namelist):',fnlev, nlev
       call endrun()
    end if

    ierr = PIO_Inq_DimID(File, 'timelevels', timelevel_dimid)
    ierr = PIO_Inq_dimlen(File, timelevel_dimid, timelevel_chk)


    ierr = PIO_Inq_DimID(File, 'ncol_d', ncol_dimid)

    ierr = PIO_Inq_dimlen(File, ncol_dimid, ncols)


    ldof => get_restart_decomp(elem, 1)
    s2d=size(ldof)
    allocate(var3d(s2d*nlev), var2d(s2d))

    var2d = 0._r8
    var3d = 0._r8
    call PIO_InitDecomp(pio_subsystem, pio_double, (/ncols/) , ldof , iodesc2d)
    deallocate(ldof)

    ldof => get_restart_decomp(elem, nlev)
    call PIO_InitDecomp(pio_subsystem, pio_double, (/ncols,nlev/),ldof , iodesc3d)
    deallocate(ldof)

#ifdef MODEL_THETA_L
    if ( .not. theta_hydrostatic_mode ) then
       allocate(var3dp(s2d*nlevp))
       var3dp = 0.0
    endif

    ldof => get_restart_decomp(elem, nlevp)
    call PIO_InitDecomp(pio_subsystem, pio_double, (/ncols,nlevp/),ldof , iodesc3dp)
    deallocate(ldof)
#endif


    initialized = .true.

    ierr = PIO_Inq_varid(File, 'U', udesc)

    ierr = PIO_Inq_varid(File, 'V', Vdesc)

#ifdef MODEL_THETA_L
    ierr = PIO_Inq_varid(File, 'VTheta_dp', tdesc)
#else
    ierr = PIO_Inq_varid(File, 'T', tdesc)
#endif

    ierr = PIO_Inq_varid(File, 'OMEGA', OMEGAdesc)

    ierr = PIO_Inq_varid(File, 'PS', psdesc)

    ierr = PIO_Inq_varid(File, 'PHIS', phisdesc)

#ifdef MODEL_THETA_L
    if ( .not. theta_hydrostatic_mode ) then
       ierr = PIO_Inq_varid(File, 'W', Wdesc)
       ierr = PIO_Inq_varid(File, 'PHINH', PHINHdesc)
    endif
#endif

    allocate(qdesc_dp(qsize_d))

    do q=1,qsize_d
       ierr = PIO_Inq_varid(File, "dp"//cnst_name(q) ,Qdesc_dp(q))
    end do

    call pio_setframe(File,phisdesc, int(1,kind=pio_offset_kind))

    call pio_read_darray(File, phisdesc, iodesc2d, var2d, ierr)

    cnt=0
    do ie=1,nelemd
       do j=1,np
          do i=1,np
             cnt=cnt+1
             elem(ie)%state%phis(i,j) = var2d(cnt)
          end do
       end do
    end do


    do ie=1,nelemd
       elem(ie)%derived%fM = 0._r8
       elem(ie)%derived%fT = 0._r8
       elem(ie)%derived%fQ = 0._r8
       elem(ie)%state%Q = 0._r8
    end do

    call pio_read_darray(File, omegadesc, iodesc3d, var3d, ierr)
    cnt=0

    do k=1,nlev
       do ie=1,nelemd
          do j=1,np
             do i=1,np
                cnt=cnt+1
                elem(ie)%derived%omega_p(i,j,k) = var3d(cnt)
             end do
          end do
       end do
    end do


    tl = timelevel%n0
    call TimeLevel_Qdp(timelevel, qsplit, tlQdp)

    call pio_setframe(File,psdesc, t)
    call pio_read_darray(File, psdesc, iodesc2d, var2d, ierr)

    cnt=0
    do ie=1,nelemd
       do j=1,np
          do i=1,np
             cnt=cnt+1
             elem(ie)%state%ps_v(i,j,tl) = var2d(cnt)
          end do
       end do
    end do

    call pio_setframe(File,udesc, t)
    call pio_read_darray(File, udesc, iodesc3d, var3d, ierr)

    cnt=0
    do k=1,nlev
       do ie=1,nelemd
          do j=1,np
             do i=1,np
                cnt=cnt+1
                elem(ie)%state%v(i,j,1,k,tl) = var3d(cnt)
             end do
          end do
       end do
    end do

    call pio_setframe(File,vdesc, t)
    call pio_read_darray(File, vdesc, iodesc3d, var3d, ierr)
    cnt=0
    do k=1,nlev
       do ie=1,nelemd
          do j=1,np
             do i=1,np
                cnt=cnt+1
                elem(ie)%state%v(i,j,2,k,tl) = var3d(cnt)
             end do
          end do
       end do
    end do

    call pio_setframe(File,tdesc, t)
    call pio_read_darray(File, tdesc, iodesc3d, var3d, ierr)
    cnt=0
    do k=1,nlev
       do ie=1,nelemd
          do j=1,np
             do i=1,np
                cnt=cnt+1
#ifdef MODEL_THETA_L
                elem(ie)%state%VTheta_dp(i,j,k,tl) = var3d(cnt)
#else
                elem(ie)%state%T(i,j,k,tl) = var3d(cnt)
#endif
             end do
          end do
       end do
    end do

#ifdef MODEL_THETA_L
    if ( theta_hydrostatic_mode ) then
       do ie=1,nelemd
          elem(ie)%state%w_i = 0
          elem(ie)%state%phinh_i = 0
       end do       
    else
       call pio_setframe(File,Wdesc, t)
       call pio_read_darray(File, Wdesc, iodesc3dp, var3dp, ierr)
       cnt=0
       do k=1,nlevp
          do ie=1,nelemd
             do j=1,np
                do i=1,np
                   cnt=cnt+1
                   elem(ie)%state%w_i(i,j,k,tl) = var3dp(cnt)
                end do
             end do
          end do
       end do

       call pio_setframe(File,PHINHdesc, t)
       call pio_read_darray(File, PHINHdesc, iodesc3dp, var3dp, ierr)
       cnt=0
       do k=1,nlevp
          do ie=1,nelemd
             do j=1,np
                do i=1,np
                   cnt=cnt+1
                   elem(ie)%state%phinh_i(i,j,k,tl) = var3dp(cnt)
                end do
             end do
          end do
       end do

    endif
#endif

    do ie = 1,nelemd
       elem(ie)%state%Qdp = 0
    end do
    do q=1,qsize_d  
       call pio_setframe(File,qdesc_dp(q), t)
       call pio_read_darray(File, qdesc_dp(q), iodesc3d, var3d, ierr)
       cnt=0
       do k=1,nlev
          do ie=1,nelemd
             do j=1,np
                do i=1,np
                   cnt=cnt+1
                   elem(ie)%state%Qdp(i,j,k,q,tlQdp) = var3d(cnt)
                end do
             end do
          end do
       end do

    end do

    deallocate(var3d,var2d)

#ifdef MODEL_THETA_L
    if ( .not. theta_hydrostatic_mode )then
       deallocate(var3dp)
    endif
#endif

    deallocate(qdesc_dp)

    call t_startf('dyn_init2')
    call dyn_init2(dyn_in)
    call t_stopf('dyn_init2')

   if (par%dynproc) then
   else
    deallocate(elem)
   endif  !!  par%dynproc

    return

  end subroutine read_restart_dynamics

end module restart_dynamics
