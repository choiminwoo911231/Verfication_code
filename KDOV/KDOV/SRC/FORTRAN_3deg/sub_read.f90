module sub_read
  use netcdf
  implicit none

  contains

!gpsro
 !-----------------------------------------------------
   subroutine read_gpsro(infile, lon_dump, lat_dump, satid_dump, prs_dump, var)
 !-----------------------------------------------------           
     integer:: status, status1
     character(len=*), intent(in) :: infile 
     integer :: ncid, nobs_varid, nobs, nsat_varid, nsat
     integer :: o_varid, b_varid, e_varid, lon_varid, lat_varid, satid_varid, prs_varid
     real, dimension(:),   allocatable, intent(out) :: lon_dump, lat_dump, satid_dump, prs_dump
     real, dimension(:,:), allocatable, intent(out) :: var
     real, dimension(:),   allocatable :: o_dump, b_dump, e_dump
            
     !Read File

      call check(nf90_open(trim(infile),nf90_nowrite,ncid))
      status = nf90_inq_dimid(ncid, "nobs", nobs_varid);    status = nf90_Inquire_Dimension(ncid, nobs_varid, len = nobs)
      status = nf90_inq_dimid(ncid, "satid", nsat_varid);   status = nf90_Inquire_Dimension(ncid, nsat_varid, len = nsat)

      !GET Variable
      ALLOCATE(lon_dump(nobs));  ALLOCATE(lat_dump(nobs));  ALLOCATE(satid_dump(nobs)); ALLOCATE(prs_dump(nobs))
      call check(nf90_inq_varid(ncid,"longitude",lon_varid));  call check(nf90_inq_varid(ncid,"latitude",lat_varid))
      call check(nf90_inq_varid(ncid,"satid",satid_varid));    call check(nf90_inq_varid(ncid,"pressure",prs_varid))
      call check(nf90_get_var(ncid,lon_varid,lon_dump));       call check(nf90_get_var(ncid,lat_varid,lat_dump))
      call check(nf90_get_var(ncid,satid_varid,satid_dump));   call check(nf90_get_var(ncid,prs_varid,prs_dump))

      ALLOCATE(o_dump(nobs));  ALLOCATE(b_dump(nobs));  ALLOCATE(e_dump(nobs))
      call check(nf90_inq_varid(ncid,"obsBA",o_varid));   call check(nf90_inq_varid(ncid,"bkgBA",b_varid))
      call check(nf90_inq_varid(ncid,"errBA",e_varid));   call check(nf90_get_var(ncid,o_varid,o_dump))
      call check(nf90_get_var(ncid,b_varid,b_dump));      call check(nf90_get_var(ncid,e_varid,e_dump))

      ALLOCATE(var(4,nobs))
      var(1,:) = o_dump*10000.;  var(2,:) = b_dump*10000.;  var(3,:) = -999.;  var(4,:) = e_dump
  
      call check(nf90_close(ncid))
    end subroutine read_gpsro
 !-----------------------------------------------------



!grndgnss
 !-----------------------------------------------------
   subroutine read_grndgnss(infile, lon_dump, lat_dump, var)
 !-----------------------------------------------------           
     integer:: status
     character(len=*), intent(in) :: infile
     integer   :: ichk
     integer :: ncid, nobs_varid, nobs
     integer :: o_varid, b_varid, e_varid, c_varid, lon_varid, lat_varid
     real, dimension(:),   allocatable, intent(out) :: lon_dump, lat_dump
     real, dimension(:,:), allocatable, intent(out) :: var
     real, dimension(:),   allocatable :: o_dump, b_dump, e_dump, c_dump

     !Read File

      call check(nf90_open(trim(infile),nf90_nowrite,ncid))
      status = nf90_inq_dimid(ncid, "nobs", nobs_varid);   status = nf90_Inquire_Dimension(ncid, nobs_varid, len = nobs)

      !GET Variable
      ALLOCATE(lon_dump(nobs));  ALLOCATE(lat_dump(nobs))
      call check(nf90_inq_varid(ncid,"longitude",lon_varid));  call check(nf90_inq_varid(ncid,"latitude",lat_varid))
      call check(nf90_get_var(ncid,lon_varid,lon_dump));       call check(nf90_get_var(ncid,lat_varid,lat_dump))

      ALLOCATE(o_dump(nobs));  ALLOCATE(b_dump(nobs));  ALLOCATE(e_dump(nobs));  ALLOCATE(c_dump(nobs))
      call check(nf90_inq_varid(ncid,"obsZTD",o_varid));  call check(nf90_inq_varid(ncid,"bkgZTD",b_varid))
      call check(nf90_inq_varid(ncid,"errZTD",e_varid));  call check(nf90_inq_varid(ncid,"corZTD",c_varid))
      call check(nf90_get_var(ncid,o_varid,o_dump));      call check(nf90_get_var(ncid,b_varid,b_dump))
      call check(nf90_get_var(ncid,e_varid,e_dump));      call check(nf90_get_var(ncid,c_varid,c_dump))

      ALLOCATE(var(4,nobs))
      var(1,:) = o_dump;  var(2,:) = b_dump;  var(3,:) = c_dump;  var(4,:) = e_dump
 

      call check(nf90_close(ncid))
    end subroutine read_grndgnss
 !-----------------------------------------------------



!aircraft
 !-----------------------------------------------------
 subroutine read_aircraft(infile, lon_dump, lat_dump, prs_dump, var_u, var_v, var_t)
 !-----------------------------------------------------
     integer:: status, status1, status2, status3
     character(len=*), intent(in) :: infile
     integer :: ncid, nobs_varid, nobs, nobsU_varid, nobsU, nobsV_varid, nobsV, nobsT_varid, nobsT
     integer :: lon_varid, lat_varid, prs_varid
     integer :: o_u_varid, o_v_varid, o_t_varid, b_u_varid, b_v_varid, b_t_varid, e_u_varid, e_v_varid, e_t_varid
     real, dimension(:),   allocatable, intent(out) :: lon_dump, lat_dump, prs_dump
     real, dimension(:,:), allocatable, intent(out) :: var_u, var_v, var_t
     real, dimension(:),   allocatable :: o_u_dump, o_v_dump, o_t_dump, b_u_dump, b_v_dump, b_t_dump, e_u_dump, e_v_dump, e_t_dump

     !Read File

      call check(nf90_open(trim(infile),nf90_nowrite,ncid))
      status = nf90_inq_dimid(ncid, "nobs", nobs_varid);    status  = nf90_Inquire_Dimension(ncid, nobs_varid, len = nobs)
      status = nf90_inq_dimid(ncid, "nobsU", nobsU_varid);  status = nf90_Inquire_Dimension(ncid, nobsU_varid, len = nobsU)
      status = nf90_inq_dimid(ncid, "nobsV", nobsV_varid);  status = nf90_Inquire_Dimension(ncid, nobsV_varid, len = nobsV)
      status = nf90_inq_dimid(ncid, "nobsT", nobsT_varid);  status = nf90_Inquire_Dimension(ncid, nobsT_varid, len = nobsT)

      ALLOCATE(lon_dump(nobs));  ALLOCATE(lat_dump(nobs));  ALLOCATE(prs_dump(nobs))
      call check(nf90_inq_varid(ncid,"longitude",lon_varid));   call check(nf90_inq_varid(ncid,"latitude",lat_varid))
      call check(nf90_get_var(ncid,lon_varid,lon_dump));        call check(nf90_get_var(ncid,lat_varid,lat_dump))
      call check(nf90_inq_varid(ncid,"pressure",prs_varid));    call check(nf90_get_var(ncid,prs_varid,prs_dump))

      ALLOCATE(o_u_dump(nobsU));  ALLOCATE(o_v_dump(nobsV));  ALLOCATE(o_t_dump(nobsT))
      ALLOCATE(b_u_dump(nobsU));  ALLOCATE(b_v_dump(nobsV));  ALLOCATE(b_t_dump(nobsT))
      ALLOCATE(e_u_dump(nobsU));  ALLOCATE(e_v_dump(nobsV));  ALLOCATE(e_t_dump(nobsT))
      call check(nf90_inq_varid(ncid,"obsU",o_u_varid));  call check(nf90_inq_varid(ncid,"obsV",o_v_varid));  call check(nf90_inq_varid(ncid,"obsT",o_t_varid))
      call check(nf90_inq_varid(ncid,"bkgU",b_u_varid));  call check(nf90_inq_varid(ncid,"bkgV",b_v_varid));  call check(nf90_inq_varid(ncid,"bkgT",b_t_varid))
      call check(nf90_inq_varid(ncid,"errU",e_u_varid));  call check(nf90_inq_varid(ncid,"errV",e_v_varid));  call check(nf90_inq_varid(ncid,"errT",e_t_varid))
      call check(nf90_get_var(ncid,o_u_varid,o_u_dump));  call check(nf90_get_var(ncid,o_v_varid,o_v_dump));  call check(nf90_get_var(ncid,o_t_varid,o_t_dump))
      call check(nf90_get_var(ncid,b_u_varid,b_u_dump));  call check(nf90_get_var(ncid,b_v_varid,b_v_dump));  call check(nf90_get_var(ncid,b_t_varid,b_t_dump))
      call check(nf90_get_var(ncid,e_u_varid,e_u_dump));  call check(nf90_get_var(ncid,e_v_varid,e_v_dump));  call check(nf90_get_var(ncid,e_t_varid,e_t_dump))

      ALLOCATE(var_u(4,nobsU));  ALLOCATE(var_v(4,nobsV));  ALLOCATE(var_t(4,nobsT))
      var_u(1,:) = o_u_dump;  var_u(2,:) = b_u_dump;  var_u(3,:) = -999.;  var_u(4,:) = e_u_dump
      var_v(1,:) = o_v_dump;  var_v(2,:) = b_v_dump;  var_v(3,:) = -999.;  var_v(4,:) = e_v_dump
      var_t(1,:) = o_t_dump;  var_t(2,:) = b_t_dump;  var_t(3,:) = -999.;  var_t(4,:) = e_t_dump

      call check(nf90_close(ncid))
  end subroutine read_aircraft
 !-----------------------------------------------------
           





!sonde
 !-----------------------------------------------------
 subroutine read_sonde(infile, lon_dump, lat_dump, prs_dump, var_u, var_v, var_t, var_q)
 !-----------------------------------------------------
     integer:: status, status1, status2, status3, status4
     character(len=*), intent(in) :: infile
     integer :: ncid, nobs_varid, nobs, nobsU_varid, nobsU, nobsV_varid, nobsV, nobsT_varid, nobsT, nobsQ_varid, nobsQ
     integer :: idxU_varid, idxV_varid, idxT_varid, idxQ_varid
     integer :: lon_varid, lat_varid, prs_varid
     integer :: o_u_varid, o_v_varid, o_t_varid, o_q_varid, b_u_varid, b_v_varid, b_t_varid, b_q_varid, e_u_varid, e_v_varid, e_t_varid, e_q_varid
     real, dimension(:),   allocatable, intent(out) :: lon_dump, lat_dump, prs_dump
     real, dimension(:,:), allocatable, intent(out) :: var_u, var_v, var_t, var_q
     real, dimension(:),   allocatable :: o_u_dump, o_v_dump, o_t_dump, o_q_dump, b_u_dump, b_v_dump, b_t_dump, b_q_dump,&
             e_u_dump, e_v_dump, e_t_dump, e_q_dump, idxu_dump, idxv_dump, idxt_dump, idxq_dump

     !Read File

      call check(nf90_open(trim(infile),nf90_nowrite,ncid))
      status = nf90_inq_dimid(ncid, "nobs", nobs_varid);   status  = nf90_Inquire_Dimension(ncid, nobs_varid, len = nobs)
      status = nf90_inq_dimid(ncid, "nobsU", nobsU_varid); status = nf90_Inquire_Dimension(ncid, nobsU_varid, len = nobsU)
      status = nf90_inq_dimid(ncid, "nobsV", nobsV_varid); status = nf90_Inquire_Dimension(ncid, nobsV_varid, len = nobsV)
      status = nf90_inq_dimid(ncid, "nobsT", nobsT_varid); status = nf90_Inquire_Dimension(ncid, nobsT_varid, len = nobsT)
      status = nf90_inq_dimid(ncid, "nobsQ", nobsQ_varid); status = nf90_Inquire_Dimension(ncid, nobsQ_varid, len = nobsQ)

      ALLOCATE(lon_dump(nobs));  ALLOCATE(lat_dump(nobs));  ALLOCATE(prs_dump(nobs))
      call check(nf90_inq_varid(ncid,"longitude",lon_varid));   call check(nf90_inq_varid(ncid,"latitude",lat_varid))
      call check(nf90_get_var(ncid,lon_varid,lon_dump));        call check(nf90_get_var(ncid,lat_varid,lat_dump))
      call check(nf90_inq_varid(ncid,"pressure",prs_varid));    call check(nf90_get_var(ncid,prs_varid,prs_dump))


      ALLOCATE(o_u_dump(nobsU));  ALLOCATE(o_v_dump(nobsV));  ALLOCATE(o_t_dump(nobsT));  ALLOCATE(o_q_dump(nobsQ))
      ALLOCATE(b_u_dump(nobsU));  ALLOCATE(b_v_dump(nobsV));  ALLOCATE(b_t_dump(nobsT));  ALLOCATE(b_q_dump(nobsQ))
      ALLOCATE(e_u_dump(nobsU));  ALLOCATE(e_v_dump(nobsV));  ALLOCATE(e_t_dump(nobsT));  ALLOCATE(e_q_dump(nobsQ))
      ALLOCATE(idxu_dump(nobsU)); ALLOCATE(idxv_dump(nobsV)); ALLOCATE(idxt_dump(nobsT)); ALLOCATE(idxq_dump(nobsQ))
      call check(nf90_inq_varid(ncid,"obsU",o_u_varid));  call check(nf90_inq_varid(ncid,"obsV",o_v_varid))
      call check(nf90_inq_varid(ncid,"obsT",o_t_varid));  call check(nf90_inq_varid(ncid,"obsQ",o_q_varid))
      call check(nf90_inq_varid(ncid,"bkgU",b_u_varid));  call check(nf90_inq_varid(ncid,"bkgV",b_v_varid))
      call check(nf90_inq_varid(ncid,"bkgT",b_t_varid));  call check(nf90_inq_varid(ncid,"bkgQ",b_q_varid))
      call check(nf90_inq_varid(ncid,"errU",e_u_varid));  call check(nf90_inq_varid(ncid,"errV",e_v_varid))
      call check(nf90_inq_varid(ncid,"errT",e_t_varid));  call check(nf90_inq_varid(ncid,"errQ",e_q_varid))
      call check(nf90_inq_varid(ncid,"idxU",idxU_varid)); call check(nf90_inq_varid(ncid,"idxV",idxV_varid))
      call check(nf90_inq_varid(ncid,"idxT",idxT_varid)); call check(nf90_inq_varid(ncid,"idxQ",idxQ_varid))
      call check(nf90_get_var(ncid,o_u_varid,o_u_dump));  call check(nf90_get_var(ncid,o_v_varid,o_v_dump))
      call check(nf90_get_var(ncid,o_t_varid,o_t_dump));  call check(nf90_get_var(ncid,o_q_varid,o_q_dump))
      call check(nf90_get_var(ncid,b_u_varid,b_u_dump));  call check(nf90_get_var(ncid,b_v_varid,b_v_dump))
      call check(nf90_get_var(ncid,b_t_varid,b_t_dump));  call check(nf90_get_var(ncid,b_q_varid,b_q_dump))
      call check(nf90_get_var(ncid,e_u_varid,e_u_dump));  call check(nf90_get_var(ncid,e_v_varid,e_v_dump))
      call check(nf90_get_var(ncid,e_t_varid,e_t_dump));  call check(nf90_get_var(ncid,e_q_varid,e_q_dump))
      call check(nf90_get_var(ncid,idxU_varid,idxu_dump)); call check(nf90_get_var(ncid,idxV_varid,idxv_dump))
      call check(nf90_get_var(ncid,idxT_varid,idxt_dump)); call check(nf90_get_var(ncid,idxQ_varid,idxq_dump))

      ALLOCATE(var_u(5,nobsU));  ALLOCATE(var_v(5,nobsV));  ALLOCATE(var_t(5,nobsT));  ALLOCATE(var_q(5,nobsQ))
      var_u(1,:) = o_u_dump;  var_u(2,:) = b_u_dump;  var_u(3,:) = -999.;  var_u(4,:) = e_u_dump;  var_u(5,:) = idxu_dump
      var_v(1,:) = o_v_dump;  var_v(2,:) = b_v_dump;  var_v(3,:) = -999.;  var_v(4,:) = e_v_dump;  var_v(5,:) = idxv_dump
      var_t(1,:) = o_t_dump;  var_t(2,:) = b_t_dump;  var_t(3,:) = -999.;  var_t(4,:) = e_t_dump;  var_t(5,:) = idxt_dump
      var_q(1,:) = o_q_dump*1000.;  var_q(2,:) = b_q_dump*1000.;  var_q(3,:) = -999.;  var_q(4,:) = e_q_dump*1000.;  var_q(5,:) = idxq_dump


      call check(nf90_close(ncid))
  end subroutine read_sonde
 !-----------------------------------------------------




!scatwind
 !-----------------------------------------------------
 subroutine read_scatwind(infile, lon_dump, lat_dump, satid_dump, var_u, var_v)
 !-----------------------------------------------------
     integer:: status
     character(len=*), intent(in) :: infile
     integer :: ncid, nobs_varid, nobs
     integer :: lon_varid, lat_varid, satid_varid
     integer :: o_u_varid, o_v_varid, b_u_varid, b_v_varid, e_u_varid, e_v_varid, c_u_varid, c_v_varid
     real, dimension(:),   allocatable, intent(out) :: lon_dump, lat_dump, satid_dump
     real, dimension(:,:), allocatable, intent(out) :: var_u, var_v
     real, dimension(:),   allocatable :: o_u_dump, o_v_dump, b_u_dump, b_v_dump, e_u_dump, e_v_dump, c_u_dump, c_v_dump
 
     !Read File

      call check(nf90_open(trim(infile),nf90_nowrite,ncid))
      status = nf90_inq_dimid(ncid, "nobs", nobs_varid);  status = nf90_Inquire_Dimension(ncid, nobs_varid, len = nobs)

      ALLOCATE(lon_dump(nobs));  ALLOCATE(lat_dump(nobs));  ALLOCATE(satid_dump(nobs))
      call check(nf90_inq_varid(ncid,"longitude",lon_varid)); call check(nf90_inq_varid(ncid,"latitude",lat_varid)); call check(nf90_inq_varid(ncid,"satid",satid_varid))
      call check(nf90_get_var(ncid,lon_varid,lon_dump));      call check(nf90_get_var(ncid,lat_varid,lat_dump));     call check(nf90_get_var(ncid,satid_varid,satid_dump))

      ALLOCATE(o_u_dump(nobs));  ALLOCATE(o_v_dump(nobs));  ALLOCATE(b_u_dump(nobs))
      ALLOCATE(b_v_dump(nobs));  ALLOCATE(e_u_dump(nobs));  ALLOCATE(e_v_dump(nobs))
      ALLOCATE(c_u_dump(nobs));  ALLOCATE(c_v_dump(nobs))
      call check(nf90_inq_varid(ncid,"obsU10",o_u_varid));  call check(nf90_inq_varid(ncid,"obsV10",o_v_varid))
      call check(nf90_inq_varid(ncid,"bkgU10",b_u_varid));  call check(nf90_inq_varid(ncid,"bkgV10",b_v_varid))
      call check(nf90_inq_varid(ncid,"errU10",e_u_varid));  call check(nf90_inq_varid(ncid,"errV10",e_v_varid))
      call check(nf90_inq_varid(ncid,"corU10",c_u_varid));  call check(nf90_inq_varid(ncid,"corV10",c_v_varid))
      call check(nf90_get_var(ncid,o_u_varid,o_u_dump));    call check(nf90_get_var(ncid,o_v_varid,o_v_dump))
      call check(nf90_get_var(ncid,b_u_varid,b_u_dump));    call check(nf90_get_var(ncid,b_v_varid,b_v_dump))
      call check(nf90_get_var(ncid,e_u_varid,e_u_dump));    call check(nf90_get_var(ncid,e_v_varid,e_v_dump))
      call check(nf90_get_var(ncid,c_u_varid,c_u_dump));    call check(nf90_get_var(ncid,c_v_varid,c_v_dump))

      ALLOCATE(var_u(4,nobs)); ALLOCATE(var_v(4,nobs))
      var_u(1,:) = o_u_dump;  var_u(2,:) = b_u_dump;  var_u(3,:) = c_u_dump;  var_u(4,:) = e_u_dump
      var_v(1,:) = o_v_dump;  var_v(2,:) = b_v_dump;  var_v(3,:) = c_v_dump;  var_v(4,:) = e_v_dump
      
      call check(nf90_close(ncid))
 end subroutine read_scatwind
 !-----------------------------------------------------
           


!surface
 !-----------------------------------------------------
 subroutine read_surface(infile, lon_dump, lat_dump, var_u, var_v, var_t, var_q, var_rh, var_ps)
 !-----------------------------------------------------
     integer:: status, status1, status2, status3, status4, status5, status6
     character(len=*), intent(in) :: infile
     integer :: ncid, nobs_varid, nobs, nobsps_varid, nobsps, nobst2_varid, nobst2, nobsq2_varid, nobsq2, nobsrh2_varid, nobsrh2,&
             nobsu10_varid, nobsu10, nobsv10_varid, nobsv10
     integer :: lon_varid, lat_varid
     integer :: o_u_varid, o_v_varid, o_ps_varid, o_t2_varid, o_q2_varid, o_rh2_varid
     integer :: b_u_varid, b_v_varid, b_ps_varid, b_t2_varid, b_q2_varid, b_rh2_varid
     integer :: c_u_varid, c_v_varid, c_ps_varid, c_t2_varid, c_q2_varid, c_rh2_varid
     integer :: e_u_varid, e_v_varid, e_ps_varid, e_t2_varid, e_q2_varid, e_rh2_varid
     integer :: idxu_varid, idxv_varid, idxt2_varid, idxq2_varid, idxps_varid, idxrh2_varid
     real, dimension(:),   allocatable, intent(out) :: lon_dump, lat_dump
     real, dimension(:,:), allocatable, intent(out) :: var_u, var_v, var_t, var_q, var_rh, var_ps
     real, dimension(:),   allocatable :: o_u_dump, o_v_dump, o_t2_dump, o_rh2_dump, o_ps_dump, o_q2_dump
     real, dimension(:),   allocatable :: b_u_dump, b_v_dump, b_t2_dump, b_rh2_dump, b_ps_dump, b_q2_dump
     real, dimension(:),   allocatable :: c_u_dump, c_v_dump, c_t2_dump, c_rh2_dump, c_ps_dump, c_q2_dump
     real, dimension(:),   allocatable :: e_u_dump, e_v_dump, e_t2_dump, e_rh2_dump, e_ps_dump, e_q2_dump
     real, dimension(:),   allocatable :: idxu_dump, idxv_dump, idxt2_dump, idxrh2_dump, idxps_dump, idxq2_dump

     !Read File
      call check(nf90_open(trim(infile),nf90_nowrite,ncid))
      status  = nf90_inq_dimid(ncid,"nobs", nobs_varid);       status = nf90_Inquire_Dimension(ncid, nobs_varid, len = nobs)
      status = nf90_inq_dimid(ncid, "nobsPs", nobsps_varid);   status = nf90_Inquire_Dimension(ncid, nobsps_varid, len = nobsps)
      status = nf90_inq_dimid(ncid, "nobsT2", nobst2_varid);   status = nf90_Inquire_Dimension(ncid, nobst2_varid, len = nobst2)
      status = nf90_inq_dimid(ncid, "nobsQ2", nobsq2_varid);   status = nf90_Inquire_Dimension(ncid, nobsq2_varid, len = nobsq2)
      status = nf90_inq_dimid(ncid, "nobsRH2", nobsrh2_varid); status = nf90_Inquire_Dimension(ncid, nobsrh2_varid, len = nobsrh2)
      status = nf90_inq_dimid(ncid, "nobsU10", nobsu10_varid); status = nf90_Inquire_Dimension(ncid, nobsu10_varid, len = nobsu10)
      status = nf90_inq_dimid(ncid, "nobsV10", nobsv10_varid); status = nf90_Inquire_Dimension(ncid, nobsv10_varid, len = nobsv10)

      ALLOCATE(lon_dump(nobs));  ALLOCATE(lat_dump(nobs))
      call check(nf90_inq_varid(ncid,"longitude",lon_varid));  call check(nf90_inq_varid(ncid,"latitude",lat_varid))
      call check(nf90_get_var(ncid,lon_varid,lon_dump));       call check(nf90_get_var(ncid,lat_varid,lat_dump))

      call check(nf90_inq_varid(ncid,"obsU10",o_u_varid));    call check(nf90_inq_varid(ncid,"bkgU10",b_u_varid))
      call check(nf90_inq_varid(ncid,"corU10",c_u_varid));    call check(nf90_inq_varid(ncid,"errU10",e_u_varid))
      call check(nf90_inq_varid(ncid,"obsV10",o_v_varid));    call check(nf90_inq_varid(ncid,"bkgV10",b_v_varid))
      call check(nf90_inq_varid(ncid,"corV10",c_v_varid));    call check(nf90_inq_varid(ncid,"errV10",e_v_varid))
      call check(nf90_inq_varid(ncid,"obsPs", o_ps_varid));   call check(nf90_inq_varid(ncid,"bkgPs", b_ps_varid))
      call check(nf90_inq_varid(ncid,"corPs", c_ps_varid));   call check(nf90_inq_varid(ncid,"errPs", e_ps_varid))
      call check(nf90_inq_varid(ncid,"obsT2", o_t2_varid));   call check(nf90_inq_varid(ncid,"bkgT2", b_t2_varid))
      call check(nf90_inq_varid(ncid,"corT2", c_t2_varid));   call check(nf90_inq_varid(ncid,"errT2", e_t2_varid))
      call check(nf90_inq_varid(ncid,"obsQ2", o_q2_varid));   call check(nf90_inq_varid(ncid,"bkgQ2", b_q2_varid))
      call check(nf90_inq_varid(ncid,"corQ2", c_q2_varid));   call check(nf90_inq_varid(ncid,"errQ2", e_q2_varid))
      call check(nf90_inq_varid(ncid,"obsRH2",o_rh2_varid));  call check(nf90_inq_varid(ncid,"bkgRH2",b_rh2_varid))
      call check(nf90_inq_varid(ncid,"corRH2",c_rh2_varid));  call check(nf90_inq_varid(ncid,"errRH2",e_rh2_varid))
      call check(nf90_inq_varid(ncid,"idxU10",idxu_varid));   call check(nf90_inq_varid(ncid,"idxV10",idxv_varid))
      call check(nf90_inq_varid(ncid,"idxPs", idxps_varid));  call check(nf90_inq_varid(ncid,"idxT2",idxt2_varid))
      call check(nf90_inq_varid(ncid,"idxRH2",idxrh2_varid)); call check(nf90_inq_varid(ncid,"idxQ2",idxq2_varid))

      ALLOCATE(o_u_dump(nobsu10));   ALLOCATE(o_v_dump(nobsv10));    ALLOCATE(o_t2_dump(nobst2))
      ALLOCATE(b_u_dump(nobsu10));   ALLOCATE(b_v_dump(nobsv10));    ALLOCATE(b_t2_dump(nobst2))
      ALLOCATE(e_u_dump(nobsu10));   ALLOCATE(e_v_dump(nobsv10));    ALLOCATE(e_t2_dump(nobst2))
      ALLOCATE(c_u_dump(nobsu10));   ALLOCATE(c_v_dump(nobsv10));    ALLOCATE(c_t2_dump(nobst2))

      ALLOCATE(o_q2_dump(nobsq2));   ALLOCATE(o_rh2_dump(nobsrh2));  ALLOCATE(o_ps_dump(nobsps))
      ALLOCATE(b_q2_dump(nobsq2));   ALLOCATE(b_rh2_dump(nobsrh2));  ALLOCATE(b_ps_dump(nobsps))
      ALLOCATE(e_q2_dump(nobsq2));   ALLOCATE(e_rh2_dump(nobsrh2));  ALLOCATE(e_ps_dump(nobsps))
      ALLOCATE(c_q2_dump(nobsq2));   ALLOCATE(c_rh2_dump(nobsrh2));  ALLOCATE(c_ps_dump(nobsps))

      ALLOCATE(idxu_dump(nobsu10));  ALLOCATE(idxv_dump(nobsv10));   ALLOCATE(idxt2_dump(nobst2))
      ALLOCATE(idxq2_dump(nobsq2));  ALLOCATE(idxrh2_dump(nobsrh2)); ALLOCATE(idxps_dump(nobsps))

      call check(nf90_get_var(ncid,o_u_varid,   o_u_dump));    call check(nf90_get_var(ncid,b_u_varid,  b_u_dump)) 
      call check(nf90_get_var(ncid,c_u_varid,   c_u_dump));    call check(nf90_get_var(ncid,e_u_varid,  e_u_dump)) 
      call check(nf90_get_var(ncid,o_v_varid,   o_v_dump));    call check(nf90_get_var(ncid,b_v_varid,  b_v_dump)) 
      call check(nf90_get_var(ncid,c_v_varid,   c_v_dump));    call check(nf90_get_var(ncid,e_v_varid,  e_v_dump)) 
      call check(nf90_get_var(ncid,o_t2_varid,  o_t2_dump));   call check(nf90_get_var(ncid,b_t2_varid, b_t2_dump)) 
      call check(nf90_get_var(ncid,c_t2_varid,  c_t2_dump));   call check(nf90_get_var(ncid,e_t2_varid, e_t2_dump)) 
      call check(nf90_get_var(ncid,o_q2_varid,  o_q2_dump));   call check(nf90_get_var(ncid,b_q2_varid, b_q2_dump)) 
      call check(nf90_get_var(ncid,c_q2_varid,  c_q2_dump));   call check(nf90_get_var(ncid,e_q2_varid, e_q2_dump)) 
      call check(nf90_get_var(ncid,o_rh2_varid, o_rh2_dump));  call check(nf90_get_var(ncid,b_rh2_varid,b_rh2_dump))
      call check(nf90_get_var(ncid,c_rh2_varid, c_rh2_dump));  call check(nf90_get_var(ncid,e_rh2_varid,e_rh2_dump))
      call check(nf90_get_var(ncid,o_ps_varid,  o_ps_dump));   call check(nf90_get_var(ncid,b_ps_varid, b_ps_dump))
      call check(nf90_get_var(ncid,c_ps_varid,  c_ps_dump));   call check(nf90_get_var(ncid,e_ps_varid, e_ps_dump))
      call check(nf90_get_var(ncid,idxu_varid,  idxu_dump));   call check(nf90_get_var(ncid,idxv_varid, idxv_dump))
      call check(nf90_get_var(ncid,idxt2_varid, idxt2_dump));  call check(nf90_get_var(ncid,idxq2_varid, idxq2_dump))
      call check(nf90_get_var(ncid,idxrh2_varid,idxrh2_dump)); call check(nf90_get_var(ncid,idxps_varid, idxps_dump))

      ALLOCATE(var_u(5,nobsu10));  ALLOCATE(var_v(5,nobsv10));  ALLOCATE(var_t(5,nobst2));  ALLOCATE(var_q(5,nobsq2))
      ALLOCATE(var_rh(5,nobsrh2)); ALLOCATE(var_ps(5,nobsps))

      var_u(1,:) = o_u_dump;  var_u(2,:) = b_u_dump;  var_u(3,:) = c_u_dump;  var_u(4,:) = e_u_dump;  var_u(5,:) = idxu_dump
      DEALLOCATE(o_u_dump); DEALLOCATE(b_u_dump); DEALLOCATE(c_u_dump); DEALLOCATE(e_u_dump); DEALLOCATE(idxu_dump)

      var_v(1,:) = o_v_dump;  var_v(2,:) = b_v_dump;  var_v(3,:) = c_v_dump;  var_v(4,:) = e_v_dump; var_v(5,:) = idxv_dump
      DEALLOCATE(o_v_dump); DEALLOCATE(b_v_dump); DEALLOCATE(c_v_dump); DEALLOCATE(e_v_dump); DEALLOCATE(idxv_dump)

      var_t(1,:) = o_t2_dump;  var_t(2,:) = b_t2_dump;  var_t(3,:) = c_t2_dump;  var_t(4,:) = e_t2_dump; var_t(5,:) = idxt2_dump
      DEALLOCATE(o_t2_dump); DEALLOCATE(b_t2_dump); DEALLOCATE(c_t2_dump); DEALLOCATE(e_t2_dump); DEALLOCATE(idxt2_dump)

      var_q(1,:) = o_q2_dump*1000.;  var_q(2,:) = b_q2_dump*1000.;  var_q(3,:) = c_q2_dump*1000.;  var_q(4,:) = e_q2_dump*1000.; var_q(5,:) = idxq2_dump
      DEALLOCATE(o_q2_dump); DEALLOCATE(b_q2_dump); DEALLOCATE(c_q2_dump); DEALLOCATE(e_q2_dump); DEALLOCATE(idxq2_dump)

      var_rh(1,:) = o_rh2_dump; var_rh(2,:) = b_rh2_dump; var_rh(3,:) = c_rh2_dump; var_rh(4,:) = e_rh2_dump; var_rh(5,:) = idxrh2_dump
      DEALLOCATE(o_rh2_dump); DEALLOCATE(b_rh2_dump); DEALLOCATE(c_rh2_dump); DEALLOCATE(e_rh2_dump); DEALLOCATE(idxrh2_dump)

      var_ps(1,:) = o_ps_dump; var_ps(2,:) = b_ps_dump; var_ps(3,:) = c_ps_dump; var_ps(4,:) = e_ps_dump; var_ps(5,:) = idxps_dump
      DEALLOCATE(o_ps_dump); DEALLOCATE(b_ps_dump); DEALLOCATE(c_ps_dump); DEALLOCATE(e_ps_dump); DEALLOCATE(idxps_dump)

      call check(nf90_close(ncid))
 end subroutine read_surface
 !-----------------------------------------------------





!amv
 !-----------------------------------------------------
 subroutine read_amv(infile, lon_dump, lat_dump, satid_dump, prs_dump, var_u, var_v)
 !-----------------------------------------------------
     integer:: status
     character(len=*), intent(in) :: infile
     integer :: ncid, nobs_varid, nobs, nchan_varid, nchan, nsat_varid, nsat
     integer :: lon_varid, lat_varid, satid_varid, ch_varid, prs_varid
     integer :: o_u_varid, o_v_varid, b_u_varid, b_v_varid, e_u_varid, e_v_varid, qc_varid
     real, dimension(:),   allocatable, intent(out) :: lon_dump, lat_dump, satid_dump, prs_dump
     real, dimension(:,:), allocatable, intent(out) :: var_u, var_v
     real, dimension(:),   allocatable :: o_u_dump, o_v_dump, b_u_dump, b_v_dump, e_u_dump, e_v_dump

     !Read File

      call check(nf90_open(trim(infile),nf90_nowrite,ncid))
      status = nf90_inq_dimid(ncid, "nobs", nobs_varid)
      status = nf90_Inquire_Dimension(ncid, nobs_varid, len = nobs)

      ALLOCATE(lon_dump(nobs));   ALLOCATE(lat_dump(nobs))
      ALLOCATE(satid_dump(nobs)); ALLOCATE(prs_dump(nobs))
      call check(nf90_inq_varid(ncid,"longitude",lon_varid));  call check(nf90_inq_varid(ncid,"latitude",lat_varid))
      call check(nf90_inq_varid(ncid,"satid",satid_varid));    call check(nf90_inq_varid(ncid,"pressure",prs_varid))
      call check(nf90_get_var(ncid,lon_varid,lon_dump));       call check(nf90_get_var(ncid,lat_varid,lat_dump))
      call check(nf90_get_var(ncid,satid_varid,satid_dump));   call check(nf90_get_var(ncid,prs_varid,prs_dump))

      ALLOCATE(o_u_dump(nobs));  ALLOCATE(o_v_dump(nobs))
      ALLOCATE(b_u_dump(nobs));  ALLOCATE(b_v_dump(nobs));
      ALLOCATE(e_u_dump(nobs));  ALLOCATE(e_v_dump(nobs))
      call check(nf90_inq_varid(ncid,"obsU",o_u_varid));  call check(nf90_inq_varid(ncid,"obsV",o_v_varid))
      call check(nf90_inq_varid(ncid,"bkgU",b_u_varid));  call check(nf90_inq_varid(ncid,"bkgV",b_v_varid))
      call check(nf90_inq_varid(ncid,"errU",e_u_varid));  call check(nf90_inq_varid(ncid,"errV",e_v_varid))
      call check(nf90_get_var(ncid,o_u_varid,o_u_dump));  call check(nf90_get_var(ncid,o_v_varid,o_v_dump))
      call check(nf90_get_var(ncid,b_u_varid,b_u_dump));  call check(nf90_get_var(ncid,b_v_varid,b_v_dump))
      call check(nf90_get_var(ncid,e_u_varid,e_u_dump));  call check(nf90_get_var(ncid,e_v_varid,e_v_dump))

      ALLOCATE(var_u(4,nobs)); ALLOCATE(var_v(4,nobs))
      var_u(1,:) = o_u_dump;  var_u(2,:) = b_u_dump;  var_u(3,:) = -999.;  var_u(4,:) = e_u_dump
      var_v(1,:) = o_v_dump;  var_v(2,:) = b_v_dump;  var_v(3,:) = -999.;  var_v(4,:) = e_v_dump

      call check(nf90_close(ncid))
 end subroutine read_amv
 !-----------------------------------------------------




!8~17 stl
 !-----------------------------------------------------
 subroutine read_stl(infile, lon_dump, lat_dump, satid_dump, ch_dump, qc_dump, var, var_e)
 !-----------------------------------------------------
     integer:: status, status1, status2
     character(len=*), intent(in) :: infile
     integer :: ncid, nobs_varid, nobs, nchan_varid, nchan, nsat_varid, nsat
     integer :: lon_varid, lat_varid, satid_varid, ch_varid
     integer :: o_varid, b_varid, e_varid, c_varid, qc_varid
     real, dimension(:),     allocatable, intent(out) :: lon_dump, lat_dump, satid_dump, ch_dump
     real, dimension(:,:),   allocatable, intent(out) :: qc_dump, var_e
     real, dimension(:,:,:), allocatable, intent(out) :: var
     real, dimension(:,:),   allocatable :: o_dump, b_dump, e_dump, c_dump

     !Read File

      call check(nf90_open(trim(infile),nf90_nowrite,ncid))
     
      status = nf90_inq_dimid(ncid, "nchan", nchan_varid);  status = nf90_Inquire_Dimension(ncid, nchan_varid, len = nchan)
      status = nf90_inq_dimid(ncid, "nobs", nobs_varid);    status = nf90_Inquire_Dimension(ncid, nobs_varid, len = nobs)
      status = nf90_inq_dimid(ncid, "nsat", nsat_varid);    status = nf90_Inquire_Dimension(ncid, nsat_varid, len = nsat)

      ALLOCATE(lon_dump(nobs));  ALLOCATE(lat_dump(nobs));  ALLOCATE(satid_dump(nobs));  ALLOCATE(ch_dump(nchan))
      call check(nf90_inq_varid(ncid,"longitude",lon_varid));  call check(nf90_inq_varid(ncid,"latitude",lat_varid))
      call check(nf90_inq_varid(ncid,"satid",satid_varid));    call check(nf90_inq_varid(ncid,"channel",ch_varid))
      call check(nf90_get_var(ncid,lon_varid,lon_dump));       call check(nf90_get_var(ncid,lat_varid,lat_dump))
      call check(nf90_get_var(ncid,satid_varid,satid_dump));   call check(nf90_get_var(ncid,ch_varid,ch_dump))

      ALLOCATE(o_dump(nchan,nobs));  ALLOCATE(b_dump(nchan,nobs))
      ALLOCATE(c_dump(nchan,nobs));  ALLOCATE(e_dump(nchan,nsat));  ALLOCATE(qc_dump(nchan,nobs))
      call check(nf90_inq_varid(ncid,"obsTB",o_varid));  call check(nf90_inq_varid(ncid,"bkgTB",b_varid))
      call check(nf90_inq_varid(ncid,"corTB",c_varid));  call check(nf90_inq_varid(ncid,"errTB",e_varid))
      call check(nf90_inq_varid(ncid,"obsqc",qc_varid))
      call check(nf90_get_var(ncid,o_varid,o_dump));     call check(nf90_get_var(ncid,b_varid,b_dump))
      call check(nf90_get_var(ncid,c_varid,c_dump));     call check(nf90_get_var(ncid,e_varid,e_dump))
      call check(nf90_get_var(ncid,qc_varid,qc_dump))

      ALLOCATE(var(4,nchan,nobs)); ALLOCATE(var_e(nchan,nsat))
      var(1,:,:) = o_dump;  var(2,:,:) = b_dump;  var(3,:,:) = c_dump;  var(4,:,:) = -999.
      var_e(:,:) = e_dump

      call check(nf90_close(ncid))
 end subroutine read_stl
 !-----------------------------------------------------

 subroutine read_stl1(infile, ch_dump)
 !-----------------------------------------------------
     integer:: status
     character(len=*), intent(in) :: infile
     integer :: ncid, nobs_varid, nobs, nchan_varid, nchan, nsat_varid, nsat
     integer :: lon_varid, lat_varid, satid_varid, ch_varid
     integer :: o_varid, b_varid, e_varid, c_varid, qc_varid
     real, dimension(:), allocatable, intent(out) :: ch_dump

      call check(nf90_open(trim(infile),nf90_nowrite,ncid))
      status = nf90_inq_dimid(ncid, "nchan", nchan_varid);  status = nf90_Inquire_Dimension(ncid, nchan_varid, len = nchan)

      ALLOCATE(ch_dump(nchan))
      call check(nf90_inq_varid(ncid,"channel",ch_varid))
      call check(nf90_get_var(ncid,ch_varid,ch_dump))
      call check(nf90_close(ncid))
 end subroutine read_stl1
 !-----------------------------------------------------


 !-----------------------------------------------------
 subroutine check(status)
 !-----------------------------------------------------
 !NetCDF function check
 integer,intent (in) :: status
 if(status /= nf90_noerr) then
 print *, trim(nf90_strerror(status)),status
 stop "NetCDF Read Stopped"
 end if
 end subroutine check

end module sub_read

