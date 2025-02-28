program main

use sub_read
use sub_map_count
use sub_make_map_count_ncdf

  implicit none

  character (len=256) :: sdate0,edate0,nt0,mod_name,log_dir,out_dir,obsname, ob4da_f,ob4da_fname,undef_char
  character (len=256) :: mdlname,mod_var,var_name,long_name,units,stl_name
  character(len=14)   :: input_name(17)

  integer :: stat,stat1,num,nt,iy,it,iz,ix
  integer :: miss_count,undef_num
  integer, parameter :: nx=121, ny=61
  real, parameter    :: miss=-999.

!------------------------------------------------------------
! READ Variable ---------------------------------------------
!------------------------------------------------------------
  integer :: ich, ichk, istl, idx_use
  !var info
  integer,dimension(:),allocatable :: sat_list
  real,dimension(:),allocatable :: lon_dump, lat_dump, ch_dump, satid_dump, prs_dump
  real,dimension(:,:),allocatable :: qc_dump
  !var (4,:) = (1=o, 2=b, 3=c, 4=e)
  real,dimension(:,:),allocatable   :: var2d, var_u_2d, var_v_2d, var_t_2d, var_q_2d, var_rh_2d, var_ps_2d
  real,dimension(:,:),allocatable   :: var_e !amsr2 ~ mwhs2 (err var has diff dimension)
  real,dimension(:,:,:),allocatable :: var3d !amsr2 ~ mwhs2

!------------------------------------------------------------
! Calculate stat --------------------------------------------
!------------------------------------------------------------
  !! integer, parameter :: pressure level
  integer, parameter :: nprs=9
  integer, parameter :: gprs=7
  integer, dimension(:),allocatable  :: llprs, prs_max, prs_min, prs_max1, prs_min1
  integer, dimension(:),allocatable  :: llprs2, prs_max2, prs_min2

  !@@ MAP
  real, dimension(:,:), allocatable :: gnum0
  real, dimension(:,:,:), allocatable :: gnum
!  real, allocatable :: gnum(:,:,:)
  !4:time  3:nvar          other lon lat
  real,dimension(:,:,:,:),allocatable :: B_surface, B_grndgnss, B_scat
  !5:time, 4:nprs, 3:nvar, other lon lat
  real,dimension(:,:,:,:,:),allocatable :: B_aircraft, B_sonde, B_gpsro, B_amv
  !4:time, 3:chan,         other lon lat
  real,dimension(:,:,:,:),allocatable :: B_amsr2, B_amsua, B_atms, B_cris, B_iasi, B_mhs, B_mwhs2
  real,dimension(:,:,:,:),allocatable :: B_csrgk2a, B_csrhima, B_csrmsg


!-----------------------------------------------------------
! KSH var --> FORTRAN --------------------------------------
!-----------------------------------------------------------
  call getenv("NT",nt0)
  call getenv("SDATE",sdate0)
  call getenv("EDATE",edate0)
  call getenv("LOG_DIR",log_dir)
  call getenv("OUT_DIR",out_dir)
  call getenv("OBSNAME",obsname)
  call getenv("MDL",mdlname)
  read(nt0,*,iostat=stat) nt
  print*,nt

!----------------------------------------------------------
! READ FILE------------------------------------------------
   !----------------------------------------------------------
  !PRESSURE LEVEL
  ALLOCATE(prs_min(nprs)); ALLOCATE(prs_max(nprs)); ALLOCATE(llprs(nprs))
  ALLOCATE(prs_min1(nprs)); ALLOCATE(prs_max1(nprs))
  prs_max = (/1075, 950, 875, 775, 600, 400, 250, 150, 75/)*100.          ! + range
  prs_min = (/950, 875, 775, 600, 400, 250, 150, 75, 0/)*100.             ! - range
  prs_max1=((/1000, 925, 850, 700, 500, 300, 200, 100, 50/)*100.)+1000     ! + 10hpa
  prs_min1=((/1000, 925, 850, 700, 500, 300, 200, 100, 50/)*100.)-1000     ! - 10hpa
  llprs   = (/1000, 925, 850, 700, 500, 300, 200, 100, 50/)
  
  ALLOCATE(prs_min2(gprs)); ALLOCATE(prs_max2(gprs)); ALLOCATE(llprs2(gprs))
  prs_max2=((/520, 320, 210, 110, 55, 28, 12/)*100.)
  prs_min2=((/480, 280, 190, 90,  45, 22, 8/)*100.)
  llprs2   = (/500, 300, 200, 100, 50, 25, 10/)

  !STA_MERGER (TIME)
  ALLOCATE(B_gpsro(nx,ny,1,gprs,nt)); ALLOCATE(B_aircraft(nx,ny,3,nprs,nt)); ALLOCATE(B_grndgnss(nx,ny,1,nt));  ALLOCATE(B_scat(nx,ny,2,nt))
  ALLOCATE(B_sonde(nx,ny,4,nprs,nt)); ALLOCATE(B_surface(nx,ny,6,nt));  ALLOCATE(B_amv(nx,ny,2,nprs,nt))  

  
  print*,"-** FILE LIST LOCATION :: ",log_dir
  print*, "*---- FILE READ, MAKE_MAP, MERGE ----*"
  !FILE OPEN
  open(21,file=trim(log_dir)//'/'//trim(mdlname)//'_ob4da_'//trim(obsname)//'list.txt', action='read')
  open(31,file=trim(log_dir)//'/'//trim(mdlname)//'_ob4da_undef_'//trim(obsname)//'.txt', action='read')
  data input_name / "gpsro","aircraft","grndgnss","scatwind","sonde","surface","amv",&
              "amsr2","amsua","atms","cris","csrgk2a","csrhima","csrmsg","iasi","mhs","mwhs2" /

  it=1
  do !1
   read(21,*,iostat=stat) ob4da_f 
   read(31,*,iostat=stat1) undef_char,undef_num
   if(stat/=0) exit
   ob4da_fname=trim(ob4da_f)
   print*,ob4da_fname
     

  do istl=1,size(input_name) !2
    if (index(trim(undef_char), trim(input_name(istl)))/=0 .and. undef_num .eq. 0) then
    if (index(ob4da_fname,trim(input_name(istl)))/=0 .and. istl .eq. 1) then !gpsro
      call read_gpsro(ob4da_fname, lon_dump, lat_dump, satid_dump, prs_dump, var2d); idx_use=0      
      if (allocated(gnum)) deallocate(gnum)
      ALLOCATE(gnum(nx,ny,gprs))
      call func_MAP_prs_avg(prs_dump, lon_dump, lat_dump, var2d, gnum, prs_min2, prs_max2, idx_use)
      B_gpsro(:,:,1,:,it) = gnum(:,:,:); DEALLOCATE(gnum)

    else if (index(ob4da_fname,trim(input_name(istl)))/=0 .and. istl .eq. 2) then !aircraft
      call read_aircraft(ob4da_fname, lon_dump, lat_dump, prs_dump, var_u_2d, var_v_2d, var_t_2d); idx_use=0
      ALLOCATE(gnum(nx,ny,nprs))                                                
      call func_MAP_prs_avg(prs_dump, lon_dump, lat_dump, var_u_2d, gnum, prs_min, prs_max, idx_use)
      B_aircraft(:,:,1,:,it) = gnum(:,:,:); DEALLOCATE(gnum)
      ALLOCATE(gnum(nx,ny,nprs))
      call func_MAP_prs_avg(prs_dump, lon_dump, lat_dump, var_v_2d, gnum, prs_min, prs_max, idx_use)
      B_aircraft(:,:,2,:,it) = gnum(:,:,:); DEALLOCATE(gnum)
      ALLOCATE(gnum(nx,ny,nprs))
      call func_MAP_prs_avg(prs_dump, lon_dump, lat_dump, var_t_2d, gnum, prs_min, prs_max, idx_use)
      B_aircraft(:,:,3,:,it) = gnum(:,:,:); DEALLOCATE(gnum)

    else if (index(ob4da_fname,trim(input_name(istl)))/=0 .and. istl .eq. 3) then !grndgnss
      call read_grndgnss(ob4da_fname, lon_dump, lat_dump, var2d);   idx_use=0
      ALLOCATE(gnum0(nx,ny))
      call func_MAP_sfc_avg(lon_dump, lat_dump, var2d, gnum0, idx_use)
      B_grndgnss(:,:,1,it)  = gnum0(:,:); DEALLOCATE(gnum0)

    else if (index(ob4da_fname,trim(input_name(istl)))/=0 .and. istl .eq. 4) then !scatwind
      call read_scatwind(ob4da_fname, lon_dump, lat_dump, satid_dump, var_u_2d, var_v_2d);   idx_use=0
      ALLOCATE(gnum0(nx,ny))
      call func_MAP_sfc_avg(lon_dump, lat_dump, var_u_2d, gnum0, idx_use)
      B_scat(:,:,1,it)  = gnum0(:,:); DEALLOCATE(gnum0)
      ALLOCATE(gnum0(nx,ny))
      call func_MAP_sfc_avg(lon_dump, lat_dump, var_v_2d, gnum0, idx_use)
      B_scat(:,:,2,it)  = gnum0(:,:); DEALLOCATE(gnum0)

    else if (index(ob4da_fname,trim(input_name(istl)))/=0 .and. istl .eq. 5) then !sonde
      call read_sonde(ob4da_fname, lon_dump, lat_dump, prs_dump, var_u_2d, var_v_2d, var_t_2d, var_q_2d);  idx_use=1      
      ALLOCATE(gnum(nx,ny,nprs))
      call func_MAP_prs_avg(prs_dump, lon_dump, lat_dump, var_u_2d, gnum, prs_min1, prs_max1, idx_use)
      B_sonde(:,:,1,:,it)  = gnum(:,:,:); DEALLOCATE(gnum)
      ALLOCATE(gnum(nx,ny,nprs))
      call func_MAP_prs_avg(prs_dump, lon_dump, lat_dump, var_v_2d, gnum, prs_min1, prs_max1, idx_use)
      B_sonde(:,:,2,:,it)  = gnum(:,:,:); DEALLOCATE(gnum)
      ALLOCATE(gnum(nx,ny,nprs))
      call func_MAP_prs_avg(prs_dump, lon_dump, lat_dump, var_t_2d, gnum, prs_min1, prs_max1, idx_use)
      B_sonde(:,:,3,:,it)  = gnum(:,:,:); DEALLOCATE(gnum)
      ALLOCATE(gnum(nx,ny,nprs))
      call func_MAP_prs_avg(prs_dump, lon_dump, lat_dump, var_q_2d, gnum, prs_min1, prs_max1, idx_use)
      B_sonde(:,:,4,:,it)  = gnum(:,:,:); DEALLOCATE(gnum)

    else if (index(ob4da_fname,trim(input_name(istl)))/=0 .and. istl .eq. 6) then !surface
      call read_surface(ob4da_fname, lon_dump, lat_dump, var_u_2d, var_v_2d, var_t_2d, var_q_2d, var_rh_2d, var_ps_2d);  idx_use=1
      ALLOCATE(gnum0(nx,ny))
      call func_MAP_sfc_avg(lon_dump, lat_dump, var_u_2d, gnum0, idx_use)
      B_surface(:,:,1,it)  = gnum0(:,:); DEALLOCATE(gnum0)
      ALLOCATE(gnum0(nx,ny))
      call func_MAP_sfc_avg(lon_dump, lat_dump, var_v_2d, gnum0, idx_use)
      B_surface(:,:,2,it)  = gnum0(:,:); DEALLOCATE(gnum0)
      ALLOCATE(gnum0(nx,ny))
      call func_MAP_sfc_avg(lon_dump, lat_dump, var_t_2d, gnum0, idx_use)
      B_surface(:,:,3,it)  = gnum0(:,:); DEALLOCATE(gnum0)
      ALLOCATE(gnum0(nx,ny))
      call func_MAP_sfc_avg(lon_dump, lat_dump, var_q_2d, gnum0, idx_use)
      B_surface(:,:,4,it)  = gnum0(:,:); DEALLOCATE(gnum0)
      ALLOCATE(gnum0(nx,ny))
      call func_MAP_sfc_avg(lon_dump, lat_dump, var_rh_2d, gnum0, idx_use)
      B_surface(:,:,5,it)  = gnum0(:,:); DEALLOCATE(gnum0)
      ALLOCATE(gnum0(nx,ny))
      call func_MAP_sfc_avg(lon_dump, lat_dump, var_ps_2d, gnum0, idx_use)
      B_surface(:,:,6,it)  = gnum0(:,:); DEALLOCATE(gnum0)

    else if (index(ob4da_fname,trim(input_name(istl)))/=0 .and. istl .eq. 7) then !amv
      call read_amv(ob4da_fname, lon_dump, lat_dump, satid_dump, prs_dump, var_u_2d, var_v_2d);  idx_use=0
      ALLOCATE(gnum(nx,ny,nprs))
      call func_MAP_prs_avg(prs_dump, lon_dump, lat_dump, var_u_2d, gnum, prs_min, prs_max, idx_use)
      B_amv(:,:,1,:,it)  = gnum(:,:,:); DEALLOCATE(gnum)
      ALLOCATE(gnum(nx,ny,nprs))
      call func_MAP_prs_avg(prs_dump, lon_dump, lat_dump, var_v_2d, gnum, prs_min, prs_max, idx_use)
      B_amv(:,:,2,:,it)  = gnum(:,:,:); DEALLOCATE(gnum)
    end if
  
    else
    !MISSING VALUE
     if (istl .lt. 8) then
        if (istl .eq. 1) B_gpsro(:,:,1,:,it)    = miss
        if (istl .eq. 2) B_aircraft(:,:,:,:,it) = miss
        if (istl .eq. 3) B_grndgnss(:,:,1,it)   = miss
        if (istl .eq. 4) B_scat(:,:,:,it)       = miss
        if (istl .eq. 5) B_sonde(:,:,:,:,it)    = miss
        if (istl .eq. 6) B_surface(:,:,:,it)    = miss
        if (istl .eq. 7) B_amv(:,:,:,:,it)      = miss
     end if

    end if


    if (index(ob4da_fname,trim(input_name(istl)))/=0 .and. istl .ge. 8 .and. istl .le. 17 ) then !amsr2 ~ mwhs2
     if (index(trim(undef_char), trim(input_name(istl)))/=0 .and. undef_num .eq. 0) then
      call read_stl(ob4da_fname, lon_dump, lat_dump, satid_dump, ch_dump, qc_dump, var3d, var_e)
        !missing value
        where (var3d(1,:,:) .le. miss .or. var3d(2,:,:) .le. miss) var3d(1,:,:) = miss
        where (var3d(1,:,:) .le. miss .or. var3d(2,:,:) .le. miss) var3d(2,:,:) = miss
        where (var3d(1,:,:) .le. miss .or. var3d(2,:,:) .le. miss) var3d(3,:,:) = miss
        ALLOCATE(gnum(nx,ny,size(ch_dump)))
        if (it .eq. 1 .and. istl .eq. 8) then
         ALLOCATE(B_amsr2(nx,ny,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 9) then
         ALLOCATE(B_amsua(nx,ny,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 10) then
         ALLOCATE(B_atms(nx,ny,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 11) then
         ALLOCATE(B_cris(nx,ny,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 12) then
         ALLOCATE(B_csrgk2a(nx,ny,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 13) then
         ALLOCATE(B_csrhima(nx,ny,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 14) then
         ALLOCATE(B_csrmsg(nx,ny,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 15) then
         ALLOCATE(B_iasi(nx,ny,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 16) then
         ALLOCATE(B_mhs(nx,ny,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 17) then
         ALLOCATE(B_mwhs2(nx,ny,size(ch_dump),nt))
        end if

        !SATID SELECTION
        if (istl .eq. 8)  ALLOCATE(sat_list(1)); if (istl .eq. 8) sat_list = [122]
        if (istl .eq. 9)  ALLOCATE(sat_list(4)); if (istl .eq. 9) sat_list = [3, 5, 209, 223]
        if (istl .eq. 10) ALLOCATE(sat_list(2)); if (istl .eq. 10) sat_list = [224, 225]
        if (istl .eq. 15) ALLOCATE(sat_list(2)); if (istl .eq. 15) sat_list = [3, 5]
        if (istl .eq. 16) ALLOCATE(sat_list(4)); if (istl .eq. 16) sat_list = [3, 5, 209, 223]
        if (istl .eq. 11) ALLOCATE(sat_list(1)); if (istl .eq. 11) sat_list = [-999]
        if (istl .eq. 12) ALLOCATE(sat_list(1)); if (istl .eq. 12) sat_list = [-999]
        if (istl .eq. 13) ALLOCATE(sat_list(1)); if (istl .eq. 13) sat_list = [-999]
        if (istl .eq. 14) ALLOCATE(sat_list(1)); if (istl .eq. 14) sat_list = [-999]
        if (istl .eq. 17) ALLOCATE(sat_list(1)); if (istl .eq. 17) sat_list = [-999]

        call func_MAP_chn_avg(lon_dump, lat_dump, satid_dump, sat_list, qc_dump, var3d, gnum) 
        if (istl .eq. 8)  B_amsr2(:,:,:,it)   = gnum(:,:,:);  if (istl .eq. 9)  B_amsua(:,:,:,it)   = gnum(:,:,:)
        if (istl .eq. 10) B_atms(:,:,:,it)    = gnum(:,:,:);  if (istl .eq. 11) B_cris(:,:,:,it)    = gnum(:,:,:)
        if (istl .eq. 12) B_csrgk2a(:,:,:,it) = gnum(:,:,:);  if (istl .eq. 13) B_csrhima(:,:,:,it) = gnum(:,:,:)
        if (istl .eq. 14) B_csrmsg(:,:,:,it)  = gnum(:,:,:);  if (istl .eq. 15) B_iasi(:,:,:,it)    = gnum(:,:,:)
        if (istl .eq. 16) B_mhs(:,:,:,it)     = gnum(:,:,:);  if (istl .eq. 17) B_mwhs2(:,:,:,it)   = gnum(:,:,:)

        DEALLOCATE(sat_list); DEALLOCATE(gnum)

       else

        call read_stl1(ob4da_fname, ch_dump) 
        if (it .eq. 1 .and. istl .eq. 8) then
         ALLOCATE(B_amsr2(nx,ny,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 9) then
         ALLOCATE(B_amsua(nx,ny,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 10) then
         ALLOCATE(B_atms(nx,ny,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 11) then
         ALLOCATE(B_cris(nx,ny,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 12) then 
         ALLOCATE(B_csrgk2a(nx,ny,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 13) then
         ALLOCATE(B_csrhima(nx,ny,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 14) then
         ALLOCATE(B_csrmsg(nx,ny,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 15) then
         ALLOCATE(B_iasi(nx,ny,size(ch_dump),nt))
        else if (it.eq. 1 .and. istl .eq. 16) then
         ALLOCATE(B_mhs(nx,ny,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 17) then
         ALLOCATE(B_mwhs2(nx,ny,size(ch_dump),nt))
        end if
    
        if (istl .eq. 8)  B_amsr2(:,:,:,it)   = miss;  if (istl .eq. 9)  B_amsua(:,:,:,it)   = miss
        if (istl .eq. 10) B_atms(:,:,:,it)    = miss;  if (istl .eq. 11) B_cris(:,:,:,it)    = miss
        if (istl .eq. 12) B_csrgk2a(:,:,:,it) = miss;  if (istl .eq. 13) B_csrhima(:,:,:,it) = miss
        if (istl .eq. 14) B_csrmsg(:,:,:,it)  = miss;  if (istl .eq. 15) B_iasi(:,:,:,it)    = miss
        if (istl .eq. 16) B_mhs(:,:,:,it)     = miss;  if (istl .eq. 17) B_mwhs2(:,:,:,it)   = miss

       end if

    end if    
 
 end do !2
                                

  !MAKE NETCDF OUTPUT  
   if (index(ob4da_fname,trim(input_name(1)))/=0 .and. it .eq. nt) then
    call ncdf_prs_map(B_gpsro, nx, ny, nt, obsname, mdlname, out_dir, sdate0, edate0, llprs2)
   else if (index(ob4da_fname,trim(input_name(2)))/=0 .and. it .eq. nt) then
    call ncdf_prs_map(B_aircraft, nx, ny, nt, obsname, mdlname, out_dir, sdate0, edate0, llprs)
   else if (index(ob4da_fname,trim(input_name(3)))/=0 .and. it .eq. nt) then
    call ncdf_nch_map(B_grndgnss, nx, ny, nt, obsname, mdlname, out_dir, sdate0, edate0)
   else if (index(ob4da_fname,trim(input_name(4)))/=0 .and. it .eq. nt) then
    call ncdf_nch_map(B_scat, nx, ny, nt, obsname, mdlname, out_dir, sdate0, edate0)
   else if (index(ob4da_fname,trim(input_name(5)))/=0 .and. it .eq. nt) then
    call ncdf_prs_map(B_sonde, nx, ny, nt, obsname, mdlname, out_dir, sdate0, edate0, llprs)
   else if (index(ob4da_fname,trim(input_name(6)))/=0 .and. it .eq. nt) then
    call ncdf_nch_map(B_surface, nx, ny, nt, obsname, mdlname, out_dir, sdate0, edate0)
   else if (index(ob4da_fname,trim(input_name(7)))/=0 .and. it .eq. nt) then
    call ncdf_prs_map(B_amv, nx, ny, nt, obsname, mdlname, out_dir, sdate0, edate0, llprs)

   else if (index(ob4da_fname,trim(input_name(8)))/=0 .and. it .eq. nt) then
   call ncdf_ch_map(B_amsr2, nx, ny, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
  else if (index(ob4da_fname,trim(input_name(9)))/=0 .and. it .eq. nt) then
   call ncdf_ch_map(B_amsua, nx, ny, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
  else if (index(ob4da_fname,trim(input_name(10)))/=0 .and. it .eq. nt) then
   call ncdf_ch_map(B_atms, nx, ny, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
  else if (index(ob4da_fname,trim(input_name(11)))/=0 .and. it .eq. nt) then
   call ncdf_ch_map(B_cris, nx, ny, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
  else if (index(ob4da_fname,trim(input_name(12)))/=0 .and. it .eq. nt) then
   call ncdf_ch_map(B_csrgk2a, nx, ny, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
  else if (index(ob4da_fname,trim(input_name(13)))/=0 .and. it .eq. nt) then
   call ncdf_ch_map(B_csrhima, nx, ny, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
  else if (index(ob4da_fname,trim(input_name(14)))/=0 .and. it .eq. nt) then
   call ncdf_ch_map(B_csrmsg, nx, ny, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
  else if (index(ob4da_fname,trim(input_name(15)))/=0 .and. it .eq. nt) then
   call ncdf_ch_map(B_iasi, nx, ny, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
  else if (index(ob4da_fname,trim(input_name(16)))/=0 .and. it .eq. nt) then
   call ncdf_ch_map(B_mhs, nx, ny, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
  else if (index(ob4da_fname,trim(input_name(17)))/=0 .and. it .eq. nt) then
   call ncdf_ch_map(B_mwhs2, nx, ny, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
   end if

   if (it .lt. nt) then
     it=it+1
   else
     it=1
   end if

  end do !1


end program




