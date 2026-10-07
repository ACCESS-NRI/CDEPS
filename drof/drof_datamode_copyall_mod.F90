module drof_datamode_copyall_mod

  use ESMF             , only : ESMF_State, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_tinterp_mod , only : shr_tInterp_getFactors
  use shr_cal_mod      , only : shr_cal_date2ymd, shr_cal_ymd2date, shr_cal_numDaysInMonth
  use shr_const_mod    , only : SHR_CONST_SPVAL
  use shr_log_mod      , only : shr_log_error

  implicit none
  private

  public  :: drof_datamode_copyall_advertise
  public  :: drof_datamode_copyall_init_pointers
  public  :: drof_datamode_copyall_advance
  public  :: drof_datamode_copyall_rofi_scale
  public  :: drof_datamode_copyall_rofi_scale_annual_mean

  ! export state pointer arrays
  real(r8), pointer :: Forr_rofl(:) => null()
  real(r8), pointer :: Forr_rofi(:) => null()

  ! stream pointer arrays
  real(r8), pointer :: strm_Forr_rofl(:) => null() ! optional, but at least one provided in stream
  real(r8), pointer :: strm_Forr_rofi(:) => null() ! optional, but at least one provided in stream

  character(len=*) , parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine drof_datamode_copyall_advertise(exportState, fldsexport, flds_scalar_name, rc)

    ! input/output variables
    type(esmf_State)   , intent(inout) :: exportState
    type(fldlist_type) , pointer       :: fldsexport
    character(len=*)   , intent(in)    :: flds_scalar_name
    integer            , intent(out)   :: rc

    ! local variables
    type(fldlist_type), pointer :: fldList
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Advertise export fields
    call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
    call dshr_fldlist_add(fldsExport, "Forr_rofl")
    call dshr_fldlist_add(fldsExport, "Forr_rofi")

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(drof_comp_advertise): Fr_ocn'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine drof_datamode_copyall_advertise

  !===============================================================================
  subroutine drof_datamode_copyall_init_pointers(exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(drof_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Initialize module ponters
    call dshr_state_getfldptr(exportState, 'Forr_rofl' , fldptr1=Forr_rofl , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Forr_rofi' , fldptr1=Forr_rofi , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call shr_strdata_get_stream_pointer( sdat, 'Forr_rofl', strm_Forr_rofl, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (.not. associated(strm_Forr_rofl)) then
       Forr_rofl(:) = 0._r8
    end if

    call shr_strdata_get_stream_pointer( sdat, 'Forr_rofi', strm_Forr_rofi, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (.not. associated(strm_Forr_rofi)) then
       Forr_rofi(:) = 0._r8
    end if

    if (.not. associated(strm_Forr_rofl) .and. .not. associated(strm_Forr_rofi)) then
       call shr_log_error(subname//'ERROR: At least one of strm_Forr_rofl or strm_Forr_rofi must be associated for drof', rc=rc)
       return
    end if

  end subroutine drof_datamode_copyall_init_pointers

  !===============================================================================
  subroutine drof_datamode_copyall_advance(model_lat, rofi_scale_sh, rofi_scale_nh)

    ! input/output variables
    real(r8), intent(in) :: model_lat(:)
    real(r8), intent(in) :: rofi_scale_sh  ! Forr_rofi scale factor for lat < 0
    real(r8), intent(in) :: rofi_scale_nh  ! Forr_rofi scale factor for lat >= 0

    ! local variables
    integer :: ni
    !-------------------------------------------------------------------------------

    ! zero out "special values" of export fields
    if (associated(strm_Forr_rofl)) then
       do ni = 1, size(Forr_rofl)
          if (abs(strm_Forr_rofl(ni)) < 1.e28_r8) then
             Forr_rofl(ni) = strm_Forr_rofl(ni)
          else
             Forr_rofl(ni) = 0.0_r8
          end if
       enddo
    end if

    if (associated(strm_Forr_rofi)) then
       do ni = 1, size(Forr_rofi)
          if (abs(strm_Forr_rofi(ni)) < 1.e28_r8) then
             Forr_rofi(ni) = strm_Forr_rofi(ni)
          else
             Forr_rofi(ni) = 0.0_r8
          end if
       end do
       Forr_rofi(:) = Forr_rofi(:) * merge(rofi_scale_sh, rofi_scale_nh, model_lat(:) < 0.0_r8)
    end if

  end subroutine drof_datamode_copyall_advance

  !===============================================================================
  subroutine drof_datamode_copyall_rofi_scale(scale, ymd, tod, calendar, logunit, scale_now, rc)

    ! Calculate scale factor for model time, by interpolating between monthly values
    ! Forr_rofi scale factor at model time ymd, tod. scale(m) applies at the middle of
    ! month m, and is linearly interpolated in time between mid-month points (December
    ! wraps to January), using the same time interpolation as stream data.

    ! input/output variables
    real(r8)         , intent(in)  :: scale(12)
    integer          , intent(in)  :: ymd
    integer          , intent(in)  :: tod
    character(len=*) , intent(in)  :: calendar
    integer          , intent(in)  :: logunit
    real(r8)         , intent(out) :: scale_now
    integer          , intent(out) :: rc

    ! local variables
    integer  :: yy, mm, dd
    integer  :: yy2, mm2              ! month after yy, mm
    integer  :: ymd1, tod1, ymd2, tod2 ! middle of months yy, mm and yy2, mm2
    real(r8) :: f1, f2
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! find the mid-month points either side of ymd, tod
    call shr_cal_date2ymd(ymd, yy, mm, dd)
    call mid_month(yy, mm, ymd1, tod1)
    if (ymd < ymd1 .or. (ymd == ymd1 .and. tod < tod1)) then
       ! before the middle of this month, so start from last month
       if (mm == 1) yy = yy - 1
       mm = mod(mm + 10, 12) + 1
       call mid_month(yy, mm, ymd1, tod1)
    end if
    yy2 = merge(yy + 1, yy, mm == 12)
    mm2 = mod(mm, 12) + 1
    call mid_month(yy2, mm2, ymd2, tod2)

    call shr_tInterp_getFactors(ymd1, tod1, ymd2, tod2, ymd, tod, f1, f2, calendar, logunit, &
         algo='linear', rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    scale_now = f1 * scale(mm) + f2 * scale(mm2)

  contains

    subroutine mid_month(y, m, ymd_mid, tod_mid)
      ! date and seconds of the middle of month m in year y
      integer, intent(in)  :: y, m
      integer, intent(out) :: ymd_mid, tod_mid
      integer :: half_month ! seconds from the start of the month to its middle

      half_month = shr_cal_numDaysInMonth(y, m, calendar) * 43200
      call shr_cal_ymd2date(y, m, 1 + half_month / 86400, ymd_mid)
      tod_mid = mod(half_month, 86400)
    end subroutine mid_month

  end subroutine drof_datamode_copyall_rofi_scale

  !===============================================================================
  real(r8) function drof_datamode_copyall_rofi_scale_annual_mean(scale)

    ! Annual mean (365 day year) of the scale factor from drof_datamode_copyall_rofi_scale.
    ! This is the mean of the interpolated curve, which differs from the mean of the 12
    ! values because months have different lengths.

    ! input/output variables
    real(r8), intent(in) :: scale(12)

    ! local variables
    integer :: m, m_next
    integer, parameter :: days_in_month(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    !-------------------------------------------------------------------------------

    ! integrate the piecewise linear curve between successive mid-month points
    drof_datamode_copyall_rofi_scale_annual_mean = 0.0_r8
    do m = 1, 12
       m_next = mod(m, 12) + 1
       drof_datamode_copyall_rofi_scale_annual_mean = drof_datamode_copyall_rofi_scale_annual_mean + &
            0.25_r8 * real(days_in_month(m) + days_in_month(m_next), r8) * (scale(m) + scale(m_next))
    end do
    drof_datamode_copyall_rofi_scale_annual_mean = drof_datamode_copyall_rofi_scale_annual_mean / 365.0_r8

  end function drof_datamode_copyall_rofi_scale_annual_mean

end module drof_datamode_copyall_mod
