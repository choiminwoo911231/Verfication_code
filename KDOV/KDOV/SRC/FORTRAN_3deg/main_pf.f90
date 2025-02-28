program main

use sub_read
use sub_stat
use sub_make_pf_ncdf

  implicit none

  character (len=256) :: sdate0,edate0,nt0,mod_name,log_dir,out_dir,obsname, ob4da_f,ob4da_fname,undef_char
  character (len=256) :: mdlname,mod_var,var_name,long_name,units,stl_name
  character(len=14)   :: input_name(17)

  integer :: stat,stat1,num,nx,ny,nz,nt,iy,it,iz,ix
  integer :: miss_count,undef_num
  integer, parameter :: miss=-999.

!------------------------------------------------------------
! READ Variable ---------------------------------------------
!------------------------------------------------------------
  integer :: ich, ichk, istl
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
  ! FOR GPSRO
  integer, dimension(:),allocatable  :: llprs2, prs_max2, prs_min2
  !! OB_stat (var_num)
  real,dimension(:),allocatable   :: OB_stat, CB_stat
  real,dimension(:,:),allocatable :: OBm_stat, CBm_stat
  real,dimension(:,:,:),allocatable :: OB_stat_prs, CB_stat_prs
  !! 1:stat(std,avg), 2:nvar, 3:time
  real,dimension(:,:,:),allocatable :: OB_grndgnss, OB_scat, OB_surface
  real,dimension(:,:,:),allocatable :: CB_grndgnss, CB_scat, CB_surface
  !! 1:stat(std,avg), 2:nvar, 3:pressure, 3:time
  real,dimension(:,:,:,:),allocatable :: OB_gpsro, OB_aircraft, OB_sonde, OB_amv
!  real,dimension(:,:,:,:),allocatable :: CB_gpsro, CB_aircraft, CB_sonde, CB_amv
  !! 1:stat(std,avg), 2:nchan, 3:time
  real,dimension(:,:,:),allocatable :: OB_amsr2, OB_amsua, OB_atms, OB_cris, OB_iasi, OB_mhs, OB_mwhs2, OB_csrgk2a, OB_csrhima, OB_csrmsg
  real,dimension(:,:,:),allocatable :: CB_amsr2, CB_amsua, CB_atms, CB_cris, CB_iasi, CB_mhs, CB_mwhs2, CB_csrgk2a, CB_csrhima, CB_csrmsg


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
  prs_max = (/1075, 950, 875, 775, 600, 400, 250, 150, 70/)*100.
  prs_min = (/950, 875, 775, 600, 400, 250, 150, 75, 30/)*100.
  llprs   = (/1000, 925, 850, 700, 500, 300, 200, 100, 50/)

  !FOR SONDE PRES
  prs_max1=((/1000, 925, 850, 700, 500, 300, 200, 100, 50/)*100.)+1000
  prs_min1=((/1000, 925, 850, 700, 500, 300, 200, 100, 50/)*100.)-1000

  !FOR GPSRO PRES
  ALLOCATE(prs_min2(gprs)); ALLOCATE(prs_max2(gprs)); ALLOCATE(llprs2(gprs))
  llprs2  = (/500, 300, 200, 100, 50, 25, 10/)
  prs_max2=((/520, 320, 210, 110, 55, 28, 12/)*100.)
  prs_min2=((/480, 280, 190, 90,  45, 22, 8/)*100.)
  
  !STAT_MERGE (TIME)
  ALLOCATE(OB_gpsro(1,2,gprs,nt)); ALLOCATE(OB_aircraft(3,2,nprs,nt)); ALLOCATE(OB_grndgnss(2,1,nt));  ALLOCATE(OB_scat(2,2,nt))
  ALLOCATE(OB_sonde(4,2,nprs,nt)); ALLOCATE(OB_surface(2,6,nt));  ALLOCATE(OB_amv(2,2,nprs,nt))
  ALLOCATE(CB_grndgnss(2,1,nt));  ALLOCATE(CB_scat(2,2,nt)); ALLOCATE(CB_surface(2,6,nt))


  print*,"-** FILE LIST LOCATION :: ",log_dir
  print*, "*---- FILE READ, MAKE_Profile, MERGE ----*"
  !FILE READ & MERGE
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
      call read_gpsro(ob4da_fname, lon_dump, lat_dump, satid_dump, prs_dump, var2d)
      ALLOCATE(OB_stat_prs(1,2,gprs))
      call stat_pf_gpsro(prs_dump, var2d, OB_stat_prs, prs_min2, prs_max2)
      OB_gpsro(:,:,:,it)=OB_stat_prs(:,:,:);  DEALLOCATE(OB_stat_prs)

    else if (index(ob4da_fname,trim(input_name(istl)))/=0 .and. istl .eq. 2) then !aircraft
      call read_aircraft(ob4da_fname, lon_dump, lat_dump, prs_dump, var_u_2d, var_v_2d, var_t_2d)       
      ALLOCATE(OB_stat_prs(3,2,nprs))
      call stat_pf_aircraft(prs_dump, var_u_2d, var_v_2d, var_t_2d, OB_stat_prs, prs_min, prs_max)
      OB_aircraft(:,:,:,it) = OB_stat_prs(:,:,:);  DEALLOCATE(OB_stat_prs)

    else if (index(ob4da_fname,trim(input_name(istl)))/=0 .and. istl .eq. 3) then !grndgnss
      call read_grndgnss(ob4da_fname, lon_dump, lat_dump, var2d)
      ALLOCATE(OBm_stat(2,1)); ALLOCATE(CBm_stat(2,1))
      call stat_pf_gnss(var2d, OBm_stat, CBm_stat)
      OB_grndgnss(:,:,it) = OBm_stat(:,:);    DEALLOCATE(OBm_stat)
      CB_grndgnss(:,:,it) = CBm_stat(:,:);    DEALLOCATE(CBm_stat)

    else if (index(ob4da_fname,trim(input_name(istl)))/=0 .and. istl .eq. 4) then !scatwind
      call read_scatwind(ob4da_fname, lon_dump, lat_dump, satid_dump, var_u_2d, var_v_2d)
      ALLOCATE(OBm_stat(2,2)); ALLOCATE(CBm_stat(2,2))
      call stat_pf_scat(var_u_2d, var_v_2d, OBm_stat, CBm_stat)
      OB_scat(:,:,it) = OBm_stat(:,:);   DEALLOCATE(OBm_stat)
      CB_scat(:,:,it) = CBm_stat(:,:);   DEALLOCATE(CBm_stat)

    else if (index(ob4da_fname,trim(input_name(istl)))/=0 .and. istl .eq. 5) then !sonde
      call read_sonde(ob4da_fname, lon_dump, lat_dump, prs_dump, var_u_2d, var_v_2d, var_t_2d, var_q_2d)
      ALLOCATE(OB_stat_prs(4,2,nprs))
      call stat_pf_sonde(prs_dump, var_u_2d, var_v_2d, var_t_2d, var_q_2d, OB_stat_prs, prs_min1, prs_max1)
      OB_sonde(:,:,:,it) = OB_stat_prs(:,:,:);     DEALLOCATE(OB_stat_prs)
!      CB_sonde(:,:,it) = CB_stat_prs(:,:);     DEALLOCATE(CB_stat_prs)

    else if (index(ob4da_fname,trim(input_name(istl)))/=0 .and. istl .eq. 6) then !surface
      call read_surface(ob4da_fname, lon_dump, lat_dump, var_u_2d, var_v_2d, var_t_2d, var_q_2d, var_rh_2d, var_ps_2d)
      ALLOCATE(OBm_stat(2,6)); ALLOCATE(CBm_stat(2,6))
      call stat_pf_surface(var_u_2d, var_v_2d, var_t_2d, var_q_2d, var_rh_2d, var_ps_2d, OBm_stat, CBm_stat)
      OB_surface(:,:,it) = OBm_stat(:,:);   DEALLOCATE(OBm_stat)
      CB_surface(:,:,it) = CBm_stat(:,:);   DEALLOCATE(CBm_stat)

    else if (index(ob4da_fname,trim(input_name(istl)))/=0 .and. istl .eq. 7) then !amv
      call read_amv(ob4da_fname, lon_dump, lat_dump, satid_dump, prs_dump, var_u_2d, var_v_2d)
      ALLOCATE(OB_stat_prs(2,2,nprs))
      call stat_pf_amv(prs_dump, var_u_2d, var_v_2d, OB_stat_prs, prs_min, prs_max)
      OB_amv(:,:,:,it) = OB_stat_prs(:,:,:);       DEALLOCATE(OB_stat_prs)
    end if

    else
    !MISSING VALUE
     if (istl .lt. 8) then
      if (istl .eq. 1)  OB_gpsro(:,1,:,it)    = miss;  if (istl .eq. 2)  OB_aircraft(:,:,:,it) = miss
      if (istl .eq. 3)  OB_grndgnss(:,:,it)   = miss;  if (istl .eq. 4)  OB_scat(:,:,it)       = miss
      if (istl .eq. 5)  OB_sonde(:,:,:,it)    = miss;  if (istl .eq. 6)  OB_surface(:,:,it)    = miss
      if (istl .eq. 7)  OB_amv(:,:,:,it)      = miss

      if (istl .eq. 3)  CB_grndgnss(:,:,it) = miss;  if (istl .eq. 4)  CB_scat(:,:,it)      = miss
      if (istl .eq. 6)  CB_surface(:,:,it)  = miss

     end if
    end if


    if (index(ob4da_fname,trim(input_name(istl)))/=0 .and. istl .ge. 8 .and. istl .le. 17 ) then !amsr2 ~ mwhs2
     if (index(trim(undef_char), trim(input_name(istl)))/=0 .and. undef_num .eq. 0) then
      call read_stl(ob4da_fname, lon_dump, lat_dump, satid_dump, ch_dump, qc_dump, var3d, var_e)
        !missing value
        where (var3d(1,:,:) .le. miss .or. var3d(2,:,:) .le. miss .or. var3d(3,:,:) .le. miss) var3d(1,:,:) = miss
        where (var3d(1,:,:) .le. miss .or. var3d(2,:,:) .le. miss .or. var3d(3,:,:) .le. miss) var3d(2,:,:) = miss
        where (var3d(1,:,:) .le. miss .or. var3d(2,:,:) .le. miss .or. var3d(3,:,:) .le. miss) var3d(3,:,:) = miss

        !ALLOCATE
        ALLOCATE(OBm_stat(2,size(ch_dump))); ALLOCATE(CBm_stat(2,size(ch_dump)))
        if (it .eq. 1 .and. istl .eq. 8) then
         ALLOCATE(OB_amsr2(2,size(ch_dump),nt)); ALLOCATE(CB_amsr2(2,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 9) then              
         ALLOCATE(OB_amsua(2,size(ch_dump),nt)); ALLOCATE(CB_amsua(2,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 10) then              
         ALLOCATE(OB_atms(2,size(ch_dump),nt)); ALLOCATE(CB_atms(2,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 11) then              
         ALLOCATE(OB_cris(2,size(ch_dump),nt)); ALLOCATE(CB_cris(2,size(ch_dump),nt)) 
        else if (it .eq. 1 .and. istl .eq. 12) then              
         ALLOCATE(OB_csrgk2a(2,size(ch_dump),nt)); ALLOCATE(CB_csrgk2a(2,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 13) then              
         ALLOCATE(OB_csrhima(2,size(ch_dump),nt)); ALLOCATE(CB_csrhima(2,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 14) then              
         ALLOCATE(OB_csrmsg(2,size(ch_dump),nt)); ALLOCATE(CB_csrmsg(2,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 15) then              
         ALLOCATE(OB_iasi(2,size(ch_dump),nt)); ALLOCATE(CB_iasi(2,size(ch_dump),nt)) 
        else if (it .eq. 1 .and. istl .eq. 16) then              
         ALLOCATE(OB_mhs(2,size(ch_dump),nt)); ALLOCATE(CB_mhs(2,size(ch_dump),nt))
        else if (it .eq. 1 .and. istl .eq. 17) then              
         ALLOCATE(OB_mwhs2(2,size(ch_dump),nt)); ALLOCATE(CB_mwhs2(2,size(ch_dump),nt))
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

        call stat_pf_stl(qc_dump, ch_dump, satid_dump, sat_list, var3d, var_e, OBm_stat, CBm_stat)
        if (istl .eq. 8)  OB_amsr2(:,:,it)   = OBm_stat(:,:);  if (istl .eq. 9)  OB_amsua(:,:,it)   = OBm_stat(:,:)
        if (istl .eq. 10) OB_atms(:,:,it)    = OBm_stat(:,:);  if (istl .eq. 11) OB_cris(:,:,it)    = OBm_stat(:,:)
        if (istl .eq. 12) OB_csrgk2a(:,:,it) = OBm_stat(:,:);  if (istl .eq. 13) OB_csrhima(:,:,it) = OBm_stat(:,:)
        if (istl .eq. 14) OB_csrmsg(:,:,it)  = OBm_stat(:,:);  if (istl .eq. 15) OB_iasi(:,:,it)    = OBm_stat(:,:)
        if (istl .eq. 16) OB_mhs(:,:,it)     = OBm_stat(:,:);  if (istl .eq. 17) OB_mwhs2(:,:,it)   = OBm_stat(:,:)

        if (istl .eq. 8)  CB_amsr2(:,:,it)   = CBm_stat(:,:);  if (istl .eq. 9)  CB_amsua(:,:,it)   = CBm_stat(:,:)
        if (istl .eq. 10) CB_atms(:,:,it)    = CBm_stat(:,:);  if (istl .eq. 11) CB_cris(:,:,it)    = CBm_stat(:,:)
        if (istl .eq. 12) CB_csrgk2a(:,:,it) = CBm_stat(:,:);  if (istl .eq. 13) CB_csrhima(:,:,it) = CBm_stat(:,:)
        if (istl .eq. 14) CB_csrmsg(:,:,it)  = CBm_stat(:,:);  if (istl .eq. 15) CB_iasi(:,:,it)    = CBm_stat(:,:)
        if (istl .eq. 16) CB_mhs(:,:,it)     = CBm_stat(:,:);  if (istl .eq. 17) CB_mwhs2(:,:,it)   = CBm_stat(:,:)
        DEALLOCATE(OBm_stat); DEALLOCATE(CBm_stat); DEALLOCATE(var3d); DEALLOCATE(sat_list)

       else !missing value
        call read_stl1(ob4da_fname, ch_dump)
        if (istl .eq. 8 .and. .not. allocated(OB_amsr2)) then
         ALLOCATE(OB_amsr2(2,size(ch_dump),nt)); ALLOCATE(CB_amsr2(2,size(ch_dump),nt))
        else if (istl .eq. 9 .and. .not. allocated(OB_amsua)) then 
         ALLOCATE(OB_amsua(2,size(ch_dump),nt)); ALLOCATE(CB_amsua(2,size(ch_dump),nt))
        else if (istl .eq. 10 .and. .not. allocated(OB_atms)) then
         ALLOCATE(OB_atms(2,size(ch_dump),nt)); ALLOCATE(CB_atms(2,size(ch_dump),nt))
        else if (istl .eq. 11 .and. .not. allocated(OB_cris)) then
         ALLOCATE(OB_cris(2,size(ch_dump),nt)); ALLOCATE(CB_cris(2,size(ch_dump),nt))
        else if (istl .eq. 12 .and. .not. allocated(OB_csrgk2a)) then
         ALLOCATE(OB_csrgk2a(2,size(ch_dump),nt)); ALLOCATE(CB_csrgk2a(2,size(ch_dump),nt))
        else if (istl .eq. 13 .and. .not. allocated(OB_csrhima)) then
         ALLOCATE(OB_csrhima(2,size(ch_dump),nt)); ALLOCATE(CB_csrhima(2,size(ch_dump),nt))
        else if (istl .eq. 14 .and. .not. allocated(OB_csrmsg)) then
         ALLOCATE(OB_csrmsg(2,size(ch_dump),nt)); ALLOCATE(CB_csrmsg(2,size(ch_dump),nt))
        else if (istl .eq. 15 .and. .not. allocated(OB_iasi)) then
         ALLOCATE(OB_iasi(2,size(ch_dump),nt)); ALLOCATE(CB_iasi(2,size(ch_dump),nt)) 
        else if (istl .eq. 16 .and. .not. allocated(OB_mhs)) then
         ALLOCATE(OB_mhs(2,size(ch_dump),nt)); ALLOCATE(CB_mhs(2,size(ch_dump),nt)) 
        else if (istl .eq. 17 .and. .not. allocated(OB_mwhs2)) then
         ALLOCATE(OB_mwhs2(2,size(ch_dump),nt)); ALLOCATE(CB_mwhs2(2,size(ch_dump),nt))
        end if

         if (istl .eq. 8)  OB_amsr2(:,:,it)   = miss;  if (istl .eq. 9)  OB_amsua(:,:,it)   = miss
         if (istl .eq. 10) OB_atms(:,:,it)    = miss;  if (istl .eq. 11) OB_cris(:,:,it)    = miss
         if (istl .eq. 12) OB_csrgk2a(:,:,it) = miss;  if (istl .eq. 13) OB_csrhima(:,:,it) = miss
         if (istl .eq. 14) OB_csrmsg(:,:,it)  = miss;  if (istl .eq. 15) OB_iasi(:,:,it)    = miss
         if (istl .eq. 16) OB_mhs(:,:,it)     = miss;  if (istl .eq. 17) OB_mwhs2(:,:,it)   = miss
         if (istl .eq. 8)  CB_amsr2(:,:,it)   = miss;  if (istl .eq. 9)  CB_amsua(:,:,it)   = miss
         if (istl .eq. 10) CB_atms(:,:,it)    = miss;  if (istl .eq. 11) CB_cris(:,:,it)    = miss
         if (istl .eq. 12) CB_csrgk2a(:,:,it) = miss;  if (istl .eq. 13) CB_csrhima(:,:,it) = miss
         if (istl .eq. 14) CB_csrmsg(:,:,it)  = miss;  if (istl .eq. 15) CB_iasi(:,:,it)    = miss
         if (istl .eq. 16) CB_mhs(:,:,it)     = miss;  if (istl .eq. 17) CB_mwhs2(:,:,it)   = miss

        end if           

    end if

   end do !2

   !MAKE NETCDF OUTPUT  
   if (index(ob4da_fname,trim(input_name(1)))/=0 .and. it .eq. nt) then
    call ncdf_nch_prs_pf(OB_gpsro, llprs2, nt, obsname, mdlname, out_dir, sdate0, edate0)
   else if (index(ob4da_fname,trim(input_name(2)))/=0 .and. it .eq. nt) then
    call ncdf_nch_prs_pf(OB_aircraft, llprs, nt, obsname, mdlname, out_dir, sdate0, edate0)
   else if (index(ob4da_fname,trim(input_name(3)))/=0 .and. it .eq. nt) then
    call ncdf_nch_pf1(OB_grndgnss, CB_grndgnss, nt, obsname, mdlname, out_dir, sdate0, edate0)
   else if (index(ob4da_fname,trim(input_name(4)))/=0 .and. it .eq. nt) then
    call ncdf_nch_pf1(OB_scat, CB_scat, nt, obsname, mdlname, out_dir, sdate0, edate0)
   else if (index(ob4da_fname,trim(input_name(5)))/=0 .and. it .eq. nt) then
    call ncdf_nch_prs_pf(OB_sonde, llprs, nt, obsname, mdlname, out_dir, sdate0, edate0)
   else if (index(ob4da_fname,trim(input_name(6)))/=0 .and. it .eq. nt) then
    call ncdf_nch_pf1(OB_surface, CB_surface, nt, obsname, mdlname, out_dir, sdate0, edate0)
   else if (index(ob4da_fname,trim(input_name(7)))/=0 .and. it .eq. nt) then
    call ncdf_nch_prs_pf(OB_amv, llprs, nt, obsname, mdlname, out_dir, sdate0, edate0)

   else if (index(ob4da_fname,trim(input_name(8)))/=0 .and. it .eq. nt) then
    call ncdf_ch_pf(OB_amsr2, CB_amsr2, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
   else if (index(ob4da_fname,trim(input_name(9)))/=0 .and. it .eq. nt) then
    call ncdf_ch_pf(OB_amsua, CB_amsua, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
   else if (index(ob4da_fname,trim(input_name(10)))/=0 .and. it .eq. nt) then
    call ncdf_ch_pf(OB_atms, CB_atms, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
   else if (index(ob4da_fname,trim(input_name(11)))/=0 .and. it .eq. nt) then
    call ncdf_ch_pf(OB_cris, CB_cris, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
   else if (index(ob4da_fname,trim(input_name(12)))/=0 .and. it .eq. nt) then
    call ncdf_ch_pf(OB_csrgk2a, CB_csrgk2a, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
   else if (index(ob4da_fname,trim(input_name(13)))/=0 .and. it .eq. nt) then
    call ncdf_ch_pf(OB_csrhima, CB_csrhima, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
   else if (index(ob4da_fname,trim(input_name(14)))/=0 .and. it .eq. nt) then
    call ncdf_ch_pf(OB_csrmsg, CB_csrmsg, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
   else if (index(ob4da_fname,trim(input_name(15)))/=0 .and. it .eq. nt) then
    call ncdf_ch_pf(OB_iasi, CB_iasi, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
   else if (index(ob4da_fname,trim(input_name(16)))/=0 .and. it .eq. nt) then
    call ncdf_ch_pf(OB_mhs, CB_mhs, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
   else if (index(ob4da_fname,trim(input_name(17)))/=0 .and. it .eq. nt) then
    call ncdf_ch_pf(OB_mwhs2, CB_mwhs2, nt, ch_dump, obsname, mdlname, out_dir, sdate0, edate0)
   end if

                                  
   if (it .lt. nt) then
     it=it+1
   else
     it=1
   end if

  end do !1


end program




