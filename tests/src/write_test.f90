module write_test

implicit none

  contains

subroutine write_test(p,z,t,td,spd,dir,u,v,rh,thick, &
  p_qc,z_qc,t_qc,td_qc,spd_qc,dir_qc,u_qc,v_qc,rh_qc,thick_qc, &
  slp , ter , lat , lon , outfile )
  ! put this in a subroutine or function
  do idx=1,size(time_littler)
    ! set input data, fall back to default values
    ! add: allow for multiple levels
    if (ANY(variable_mapping=="pressure" ) .AND. &
      (pressure(idx) /= fill_value)) then
      p = pressure(idx)
    else
      p = dpressure
    end if
    if (ANY(variable_mapping=="height" ) .AND. &
      (height(idx) /= fill_value)) then
      z = height(idx) ! either p or z must be defined
    else
      z = dheight
    endif
    if (ANY(variable_mapping=="temperature" ) .AND. &
      (temperature(idx) /= fill_value)) then
      t = temperature(idx) + 273.15 ! convert to K
    else
      t = dtemperature
    end if
    if (ANY(variable_mapping=="dew_point" ) .AND. &
      (dew_point(idx) /= fill_value)) then
      td = dew_point(idx)
    else
      td = ddew_point
    end if
    if (ANY(variable_mapping=="speed" ) .AND. &
      (spd(idx) /= fill_value)) then
      spd = speed(idx)
    else
      spd = dspeed
    end if
    if (ANY(variable_mapping=="direction") .AND. & 
      (direction(idx) /= fill_value)) then
      dir = direction(idx)
    else
      dir = ddirection
    end if
    if (ANY(variable_mapping=="uwind" ) .AND. &
      (uwind(idx) /= fill_value)) then
      u = uwind(idx)
    else
      u = du
    end if
    if (ANY(variable_mapping=="vwind" ) .AND. &
      (vwind(idx) /= fill_value)) then
      v = vwind(idx)
    else
      v = dv
    end if
    if (ANY(variable_mapping=="temperature" ) .AND. &
      (humidity(idx) /= fill_value)) then
      rh = humidity(idx)
    else
      rh = drh
    end if
    if (ANY(variable_mapping=="thickness") .AND. &
      (thickness(idx) /= fill_value)) then
      thick = thickness(idx)
    else
      thick = dthickness
    end if
    p_qc = dpressure_qc
    z_qc = dheight_qc
    t_qc = dtemperature_qc
    td_qc = ddew_point_qc
    spd_qc = dspeed_qc
    dir_qc = ddirection_qc
    u_qc = du_qc
    v_qc = dv_qc
    rh_qc = drh_qc
    thick_qc = dthickness_qc
    if ( kx == 1 ) then ! surface variables
      call write_obs(p,z,t,td,spd,dir,u,v,rh,thick, &
        p_qc,z_qc,t_qc,td_qc,spd_qc,dir_qc,u_qc,v_qc,rh_qc,thick_qc, &
        slp, ter, lat, lon, time_littler(idx), kx, &
        '99001  Maybe more site info             ', &
        'SURFACE DATA FROM ??????????? SOURCE    ', &
        'FM-12 SYNOP                             ', &
        '                                        ', &
        bogus , iseq_num , 2, outfile )
    else ! vertical profile
      call write_obs(p,z,t,td,spd,dir,u,v,rh,thick, &
        p_qc,z_qc,t_qc,td_qc,spd_qc,dir_qc,u_qc,v_qc,rh_qc,thick_qc, &
        slp, ter, lat, lon, time_littler(idx), kx, &
        '99001  Maybe more site info             ', &
        'SOUNDINGS FROM ????????? SOURCE         ', &
        'FM-35 TEMP                              ', &
        '                                        ', &
        bogus , iseq_num , 2, outfile )
    endif
  end do
