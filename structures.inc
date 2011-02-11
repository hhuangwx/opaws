!***************************************************
! code obtained from Wen-Chau Lee at the National  *
! Center for Atmospheric Research on 8/6/01        *
!             					   *
! modified to use derived types in fortran90       *
! by Lou Wicker 10/20/03                           *
!***************************************************

!***************************************************
! FORTRAN STRUCTURES CONTAINING DORADE DESCRIPTORS 
!***************************************************

      type vold_info
        integer(kind=2)  ver_num
        integer(kind=2)  vol_num
        integer(kind=4)  max_bytes
        character(len=20) proj_name
        integer(kind=2)  year
        integer(kind=2)  mon
        integer(kind=2)  day
        integer(kind=2)  hour
        integer(kind=2)  min
        integer(kind=2)  sec
        character(len=8) flight_num
        character(len=8) gen_fac_name
        integer(kind=2)  gen_year
        integer(kind=2)  gen_mon
        integer(kind=2)  gen_day
        integer(kind=2)  num_sensors
      end type vold_info
!**************************************************
      type radd_info
        character(len=8) rad_name
        real(kind=4)     rad_constant
        real(kind=4)     peak_pow
        real(kind=4)     noise_pos
        real(kind=4)     rec_gain
        real(kind=4)     ant_gain
        real(kind=4)     sys_gain
        real(kind=4)     horiz_beam_width
        real(kind=4)     vert_beam_width
        integer(kind=2)  rad_type
        integer(kind=2)  scan_mode
        real(kind=4)     scan_rate
        real(kind=4)     start_ang
        real(kind=4)     stop_ang
        integer(kind=2)  num_param_desc
        integer(kind=2)  num_desc
        integer(kind=2)  compress_flag
        integer(kind=2)  data_reduc_flag
        real(kind=4)     data_reduc_parm1
        real(kind=4)     data_reduc_parm2
        real(kind=4)     radar_lon
        real(kind=4)     radar_lat
        real(kind=4)     radar_alt
        real(kind=4)     unambig_vel
        real(kind=4)     unambig_range
        integer(kind=2)  num_freq
        integer(kind=2)  num_pulse
        real(kind=4)     freq1
        real(kind=4)     freq2
        real(kind=4)     freq3
        real(kind=4)     freq4
        real(kind=4)     freq5
        real(kind=4)     pulse_per1
        real(kind=4)     pulse_per2
        real(kind=4)     pulse_per3
        real(kind=4)     pulse_per4
        real(kind=4)     pulse_per5
      end type radd_info
!**************************************************
      type celv_info
        integer(kind=4) total_gates
        real(kind=4)    gate_spacing(3000)
      end type celv_info
!**************************************************
      type cfac_info
        real(kind=4) c_azimuth
        real(kind=4) c_elevation
        real(kind=4) c_range_delay
        real(kind=4) c_rad_lon
        real(kind=4) c_rad_lat
        real(kind=4) c_alt_msl
        real(kind=4) c_alt_agl
        real(kind=4) c_ew_grspeed
        real(kind=4) c_ns_grspeed
        real(kind=4) c_vert_vel
        real(kind=4) c_head
        real(kind=4) c_roll
        real(kind=4) c_pitch
        real(kind=4) c_drift
        real(kind=4) c_rotang
        real(kind=4) c_tiltang
      end type cfac_info
!**************************************************
      type parm_info
        character(len=8)  parm_name
        character(len=40) parm_desc
        character(len=8)  parm_unit
        integer(kind=2)   ipp
        integer(kind=2)   trans_freq
        real(kind=4)      rec_bandwidth
        integer(kind=2)   pulse_width
        integer(kind=2)   polarization
        integer(kind=2)   num_samples
        integer(kind=2)   parm_type
        character(len=8)  threshold_fld
        real(kind=4)      threshold_val
        real(kind=4)      scale_fac
        real(kind=4)      offset_fac
        integer(kind=4)   baddata_flag
      end type parm_info
!**************************************************
      type swib_info
        character(len=8) rad_name
        integer(kind=4)  sweep_num
        integer(kind=4)  num_rays
        real(kind=4)     start_ang
        real(kind=4)     stop_ang
        real(kind=4)     fixed_ang
        integer(kind=4)  filter_flag
      end type swib_info
!**************************************************
      type ryib_info
        integer(kind=4) sweep_num
        integer(kind=4) julian_day
        integer(kind=2) hour
        integer(kind=2) min
        integer(kind=2) sec
        integer(kind=2) msec
        real(kind=4)    azimuth
        real(kind=4)    elevation
        real(kind=4)    peak_power
        real(kind=4)    scan_rate
        integer(kind=4) ray_status
      end type ryib_info
!**************************************************
      type asib_info
        real(kind=4) lon
        real(kind=4) lat
        real(kind=4) alt_msl
        real(kind=4) alt_agl
        real(kind=4) ew_gspeed
        real(kind=4) ns_gspeed
        real(kind=4) vert_vel
        real(kind=4) head
        real(kind=4) roll
        real(kind=4) pitch
        real(kind=4) drift
        real(kind=4) rot_ang
        real(kind=4) tilt_ang
        real(kind=4) ew_horiz_wind
        real(kind=4) ns_horiz_wind
        real(kind=4) vert_wind
        real(kind=4) head_change
        real(kind=4) pitch_change
      end type asib_info 
!**************************************************
      type rdat_info
        character(len=8) parm_name
        real(kind=4)     data(3000)
      end type rdat_info
!**************************************************
!*      type parm
!*        integer(kind=2)  int(4) 
!*        character(len=8) name
!*      end type parm
