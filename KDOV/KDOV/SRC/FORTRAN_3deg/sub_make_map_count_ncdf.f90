module sub_make_map_count_ncdf
  use netcdf
  implicit none

  contains


  subroutine ncdf_ch_map(bkg, nx, ny, nt, ch_info, obsname, mdlname, out_dir, sd, ed)
     character (len=256), intent(in)      :: obsname, out_dir, sd, ed, mdlname
     character (len=256)                  :: fname_out
     integer, intent(in)                  :: nx, ny, nt
     real, dimension(:), intent(in)    :: ch_info
     real, dimension(:,:,:,:), intent(in) :: bkg
     integer :: nch
     integer :: ncid1
     integer :: varid00, varid01, varid02
     integer :: varid1, varid2, varid3
     integer :: nx_dimid, ny_dimid, nt_dimid, nch_dimid 
     integer, dimension(4) :: data_dimid_4d
     integer, dimension(1) :: data_dimid_x, data_dimid_y, data_dimid_t, data_dimid_ch
   
     real,dimension(nx) :: lon_itp
     real,dimension(ny) :: lat_itp
           

     nch = size(bkg,3)
     CALL MAKE_VIRTUAL_GRID(nx, ny, lon_itp, lat_itp)

     fname_out=trim(trim(out_dir)//trim(mdlname)//'/OB4DA_MAP_'//trim(sd)//'_'//trim(ed)//'_COUNT_'//trim(obsname)//'.nc')
     print*,"-** OUTPUT OB4DA MAP FILE NAME :: ",fname_out
     call check(nf90_create(trim(fname_out),NF90_NETCDF4,ncid1))
     call check(nf90_def_dim(ncid1, "lon", nx, nx_dimid))
     call check(nf90_def_dim(ncid1, "lat", ny, ny_dimid))
     call check(nf90_def_dim(ncid1, "time", nt, nt_dimid))
     call check(nf90_def_dim(ncid1, "channel", nch, nch_dimid))
     data_dimid_x =(/nx_dimid/)
     data_dimid_y =(/ny_dimid/)
     data_dimid_t =(/nt_dimid/)
     data_dimid_ch=(/nch_dimid/)
     data_dimid_4d=(/ nx_dimid, ny_dimid, nch_dimid, nt_dimid /)

     call check(nf90_def_var(ncid1, "lon",NF90_REAL, data_dimid_x, varid00))
     call check(nf90_put_att(ncid1, varid00, "long_name", "lon:axis = X"))
     call check(nf90_def_var(ncid1, "lat",NF90_REAL, data_dimid_y, varid01))
     call check(nf90_put_att(ncid1, varid01, "long_name", "lat:axis = Y"))
     call check(nf90_def_var(ncid1, "channel",NF90_REAL, data_dimid_ch, varid02))
     call check(nf90_put_att(ncid1, varid02, "long_name", "channel"))
     call check(nf90_inq_varid(ncid1,"lon",varid00))
     call check(nf90_inq_varid(ncid1,"lat",varid01))
     call check(nf90_inq_varid(ncid1,"channel",varid02))
     call check(nf90_put_var(ncid1,varid00,lon_itp))
     call check(nf90_put_var(ncid1,varid01,lat_itp))
     call check(nf90_put_var(ncid1,varid02,ch_info))

     call check(nf90_def_var(ncid1, "Data_num",NF90_REAL, data_dimid_4d, varid1))
     call check(nf90_put_var(ncid1, varid1,  bkg))

     call check(nf90_close(ncid1))
 
  end subroutine ncdf_ch_map



  subroutine ncdf_nch_map(bkg, nx, ny, nt, obsname, mdlname, out_dir, sd ,ed)
     character (len=256), intent(in) :: obsname, out_dir, sd, ed, mdlname
     character (len=256)             :: fname_out
     integer, intent(in)             :: nx, ny, nt
     real, dimension(:,:,:,:), intent(in) :: bkg
     integer :: nvar, ivar
     integer :: ncid1
     integer :: varid00, varid01, varid02
     integer :: varid1, varid2, varid3
     integer :: nx_dimid, ny_dimid, nt_dimid, nvar_dimid
     integer, dimension(4) :: data_dimid_4d
     integer, dimension(1) :: data_dimid_x, data_dimid_y, data_dimid_t, data_dimid_var
     integer, dimension(:), allocatable :: var_dim
   
     real,dimension(nx) :: lon_itp
     real,dimension(ny) :: lat_itp

    
     nvar = size(bkg,3)
     ALLOCATE(var_dim(nvar))
     do ivar = 1,nvar
      var_dim(ivar) = ivar-1
     end do
     CALL MAKE_VIRTUAL_GRID(nx, ny, lon_itp, lat_itp)
 
     fname_out=trim(trim(out_dir)//trim(mdlname)//'/OB4DA_MAP_'//trim(sd)//'_'//trim(ed)//'_COUNT_'//trim(obsname)//'.nc')
     print*,"-** OUTPUT OB4DA MAP FILE NAME :: ",fname_out
     call check(nf90_create(trim(fname_out),NF90_NETCDF4,ncid1))
     call check(nf90_def_dim(ncid1, "lon", nx, nx_dimid))
     call check(nf90_def_dim(ncid1, "lat", ny, ny_dimid))
     call check(nf90_def_dim(ncid1, "time", nt, nt_dimid))
     call check(nf90_def_dim(ncid1, "variable", nvar, nvar_dimid))
     data_dimid_x  =(/nx_dimid/)
     data_dimid_y  =(/ny_dimid/)
     data_dimid_t  =(/nt_dimid/)
     data_dimid_var=(/nvar_dimid/)
     data_dimid_4d =(/ nx_dimid, ny_dimid, nvar_dimid, nt_dimid /)

     call check(nf90_def_var(ncid1, "lon",NF90_REAL, data_dimid_x, varid00))
     call check(nf90_put_att(ncid1, varid00, "long_name", "lon:axis = X"))
     call check(nf90_def_var(ncid1, "lat",NF90_REAL, data_dimid_y, varid01))
     call check(nf90_put_att(ncid1, varid01, "long_name", "lat:axis = Y"))
     call check(nf90_def_var(ncid1, "variable",NF90_REAL, data_dimid_var, varid02))
     call check(nf90_put_att(ncid1, varid02, "long_name", "except(gpsro, grndgnss), 1:u, 2:v, 3:t, 4:q, 5:rh, 6:ps"))

     call check(nf90_inq_varid(ncid1,"lon",varid00))
     call check(nf90_inq_varid(ncid1,"lat",varid01))
     call check(nf90_inq_varid(ncid1,"variable",varid02))
     call check(nf90_put_var(ncid1,varid00,lon_itp))
     call check(nf90_put_var(ncid1,varid01,lat_itp))
     call check(nf90_put_var(ncid1,varid02,var_dim))

     call check(nf90_def_var(ncid1, "Data_num",NF90_REAL, data_dimid_4d, varid1))
     call check(nf90_put_var(ncid1, varid1,  bkg))

     call check(nf90_close(ncid1))

  end subroutine ncdf_nch_map





  subroutine ncdf_prs_map(bkg, nx, ny, nt, obsname, mdlname, out_dir, sd ,ed, prs)
     character (len=256), intent(in) :: obsname, out_dir, sd, ed, mdlname
     character (len=256)             :: fname_out
     integer, intent(in)             :: nx, ny, nt
     real, dimension(:,:,:,:,:), intent(in) :: bkg
     integer, dimension(:),      intent(in) :: prs
     integer :: nvar, ivar, nprs
     integer :: ncid1
     integer :: varid00, varid01, varid02, varid03
     integer :: varid1, varid2
     integer :: nx_dimid, ny_dimid, nt_dimid, nvar_dimid, nprs_dimid
     integer, dimension(5) :: data_dimid_5d
     integer, dimension(1) :: data_dimid_x, data_dimid_y, data_dimid_t, data_dimid_var, data_dimid_prs
     integer, dimension(:), allocatable :: var_dim

     real,dimension(nx) :: lon_itp
     real,dimension(ny) :: lat_itp

     nprs = size(prs)
     nvar = size(bkg,3)
     ALLOCATE(var_dim(nvar))
     do ivar = 1,nvar
      var_dim(ivar) = ivar-1
     end do
     CALL MAKE_VIRTUAL_GRID(nx, ny, lon_itp, lat_itp)

     fname_out=trim(trim(out_dir)//trim(mdlname)//'/OB4DA_MAP_'//trim(sd)//'_'//trim(ed)//'_COUNT_'//trim(obsname)//'.nc')
     print*,"-** OUTPUT OB4DA MAP FILE NAME :: ",fname_out
     call check(nf90_create(trim(fname_out),NF90_NETCDF4,ncid1))
     call check(nf90_def_dim(ncid1, "lon", nx, nx_dimid))
     call check(nf90_def_dim(ncid1, "lat", ny, ny_dimid))
     call check(nf90_def_dim(ncid1, "time", nt, nt_dimid))
     call check(nf90_def_dim(ncid1, "variable", nvar, nvar_dimid))
     call check(nf90_def_dim(ncid1, "pressure", nprs, nprs_dimid))
     data_dimid_x  =(/nx_dimid/)
     data_dimid_y  =(/ny_dimid/)
     data_dimid_t  =(/nt_dimid/)
     data_dimid_var=(/nvar_dimid/)
     data_dimid_prs=(/nprs_dimid/)
     data_dimid_5d =(/ nx_dimid, ny_dimid, nvar_dimid, nprs_dimid, nt_dimid /)

     call check(nf90_def_var(ncid1, "lon",NF90_REAL, data_dimid_x, varid00))
     call check(nf90_put_att(ncid1, varid00, "long_name", "lon:axis = X"))
     call check(nf90_def_var(ncid1, "lat",NF90_REAL, data_dimid_y, varid01))
     call check(nf90_put_att(ncid1, varid01, "long_name", "lat:axis = Y"))
     call check(nf90_def_var(ncid1, "variable",NF90_REAL, data_dimid_var, varid02))
     call check(nf90_put_att(ncid1, varid02, "long_name", "except(gpsro, grndgnss), 1:u, 2:v, 3:t, 4:q, 5:rh, 6:ps"))
     call check(nf90_def_var(ncid1, "pressure",NF90_REAL, data_dimid_prs, varid03))
     call check(nf90_put_att(ncid1, varid03, "long_name", "pressure level"))


     call check(nf90_inq_varid(ncid1,"lon",varid00))
     call check(nf90_inq_varid(ncid1,"lat",varid01))
     call check(nf90_inq_varid(ncid1,"variable",varid02))
     call check(nf90_inq_varid(ncid1,"pressure",varid03))
     call check(nf90_put_var(ncid1,varid00,lon_itp))
     call check(nf90_put_var(ncid1,varid01,lat_itp))
     call check(nf90_put_var(ncid1,varid02,var_dim))
     call check(nf90_put_var(ncid1,varid03,prs))

     call check(nf90_def_var(ncid1, "Data_num",NF90_REAL, data_dimid_5d, varid1))
     call check(nf90_put_var(ncid1, varid1,  bkg))

     call check(nf90_close(ncid1))

  end subroutine ncdf_prs_map






!------------------------------------------------------------
SUBROUTINE MAKE_VIRTUAL_GRID(nnx, nny, lon_itps, lat_itps)

 implicit none

 integer :: isub,jsub,ksub,ssub,aa

 integer :: nnx,nny
 real, parameter :: dlon=3.0, dlat=3.0
 real, dimension(nnx):: lon_itps
 real, dimension(nny):: lat_itps, lat_itp

 lat_itp(1)=-90.
 do jsub = 2,nny
   ksub=jsub-1
   lat_itp(jsub) = -90. + ksub * dlat
 end do

 aa=0
 do isub = nny,1,-1
   aa=aa+1
   lat_itps(aa) = lat_itp(isub)
 end do

 lon_itps(1)=0.
 do isub = 2, nnx+1
   ssub=isub-1
   lon_itps(isub) = ssub * dlon
 end do

 RETURN
END SUBROUTINE
!------------------------------------------------------------



!-----------------------------------------------------
 subroutine check(status)
!-----------------------------------------------------
!NetCDF function check
  use netcdf
  integer,intent (in) :: status
  if(status /= nf90_noerr) then
  print *, trim(nf90_strerror(status)),status
  stop "NetCDF MAKE MAP find grid error --> Stopped"
  end if
 end subroutine check

end module sub_make_map_count_ncdf
