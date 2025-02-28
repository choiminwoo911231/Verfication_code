module sub_make_pf_ncdf
  use netcdf
  implicit none

  contains

  subroutine ncdf_ch_pf(var, var1, nt, ch_info, obsname, mdlname, out_dir, sd, ed)
     character (len=256), intent(in)    :: obsname, out_dir, sd, ed, mdlname
     character (len=256)                :: fname_out
     integer, intent(in)                :: nt
     real, dimension(:), intent(in)     :: ch_info
     real, dimension(:,:,:), intent(in)   :: var, var1
     integer :: nch, nstat
     integer :: ncid1
     integer :: varid00
     integer :: varid1, varid2
     integer :: nt_dimid, nch_dimid, nstat_dimid
     integer, dimension(3) :: data_dimid_2d
     integer, dimension(1) :: data_dimid_t, data_dimid_ch, data_dimid_stat

     nstat= size(var,1)
     nch = size(var,2)
     fname_out=trim(trim(out_dir)//trim(mdlname)//'/OB4DA_PF_'//trim(sd)//'_'//trim(ed)//'_STAT_'//trim(obsname)//'.nc')
     print*,"-** OUTPUT OB4DA PF FILE NAME :: ",fname_out
     call check(nf90_create(trim(fname_out),NF90_NETCDF4,ncid1))
     call check(nf90_def_dim(ncid1, "time", nt, nt_dimid))
     call check(nf90_def_dim(ncid1, "channel", nch, nch_dimid))
     call check(nf90_def_dim(ncid1, "stat", nstat, nstat_dimid))
     data_dimid_t =(/nt_dimid/)
     data_dimid_ch=(/nch_dimid/)
     data_dimid_2d=(/ nstat_dimid, nch_dimid, nt_dimid /)

     call check(nf90_def_var(ncid1, "channel",NF90_REAL, data_dimid_ch, varid00))
     call check(nf90_put_att(ncid1, varid00, "long_name", "channel"))
     call check(nf90_inq_varid(ncid1,"channel",varid00))
     call check(nf90_put_var(ncid1,varid00,ch_info))

     call check(nf90_def_var(ncid1, "OmB",NF90_REAL, data_dimid_2d, varid1))
     call check(nf90_put_var(ncid1, varid1,  var))
     call check(nf90_def_var(ncid1, "CmB",NF90_REAL, data_dimid_2d, varid2))
     call check(nf90_put_var(ncid1, varid2,  var1))

     call check(nf90_close(ncid1))
 
  end subroutine ncdf_ch_pf




  subroutine ncdf_nch_pf1(var, var1, nt, obsname, mdlname, out_dir, sd ,ed)
     character (len=256), intent(in)  :: obsname, out_dir, sd, ed, mdlname
     character (len=256)              :: fname_out
     integer, intent(in)              :: nt
     real, dimension(:,:,:), intent(in) :: var, var1
     integer :: nvar, ivar, nstat
     integer :: ncid1
     integer :: varid00
     integer :: varid1, varid2
     integer :: nt_dimid, nvar_dimid, nstat_dimid
     integer, dimension(3) :: data_dimid_2d
     integer, dimension(1) :: data_dimid_t, data_dimid_var, data_dimid_stat
     integer, dimension(:), allocatable :: var_dim
  
     nstat= size(var,1) 
     nvar = size(var,2)
     ALLOCATE(var_dim(nvar))     
     do ivar = 1,nvar
      var_dim(ivar) = ivar-1
     end do

     fname_out=trim(trim(out_dir)//trim(mdlname)//'/OB4DA_PF_'//trim(sd)//'_'//trim(ed)//'_STAT_'//trim(obsname)//'.nc')
     print*,"-** OUTPUT OB4DA PF FILE NAME :: ",fname_out
     call check(nf90_create(trim(fname_out),NF90_NETCDF4,ncid1))
     call check(nf90_def_dim(ncid1, "time", nt, nt_dimid))
     call check(nf90_def_dim(ncid1, "variable", nvar, nvar_dimid))
     call check(nf90_def_dim(ncid1, "stat", nstat, nstat_dimid))
     data_dimid_t   =(/nt_dimid/)
     data_dimid_var =(/nvar_dimid/)
     data_dimid_stat=(/nstat_dimid/)
     data_dimid_2d  =(/ nstat_dimid, nvar_dimid, nt_dimid /)

     call check(nf90_def_var(ncid1, "variable",NF90_REAL, data_dimid_var, varid00))
     call check(nf90_put_att(ncid1, varid00, "long_name", "1:u, 2:v, 3:t, 4:q, 5:rh, 6:ps"))
     call check(nf90_inq_varid(ncid1,"variable",varid00))
     call check(nf90_put_var(ncid1,varid00,var_dim))

     call check(nf90_def_var(ncid1, "OmB",NF90_REAL, data_dimid_2d, varid1))
     call check(nf90_put_var(ncid1, varid1,  var))
     call check(nf90_def_var(ncid1, "CmB",NF90_REAL, data_dimid_2d, varid2))
     call check(nf90_put_var(ncid1, varid2,  var1))

     call check(nf90_close(ncid1))

  end subroutine ncdf_nch_pf1


 
!  subroutine ncdf_nch_pf(var, var1, nt, obsname, mdlname, out_dir, sd ,ed)     
!     character (len=256), intent(in)  :: obsname, out_dir, sd, ed, mdlname      
!     character (len=256)              :: fname_out                              
!     integer, intent(in)              :: nt                                     
!     real, dimension(:,:,:), intent(in) :: var, var1                            
!     integer :: nvar, ivar                                                      
!     integer :: ncid1                                                           
!     integer :: varid00                                                         
!     integer :: varid1, varid2                                                  
!     integer :: nt_dimid, nvar_dimid                                            
!     integer, dimension(2) :: data_dimid_2d                                     
!     integer, dimension(1) :: data_dimid_t, data_dimid_var                      
!     integer, dimension(:), allocatable :: var_dim                              
!                                                                                
!     nvar = size(var,1)                                                         
!     ALLOCATE(var_dim(nvar))                                                    
!     do ivar = 1,nvar                                                           
!      var_dim(ivar) = ivar-1                                                    
!     end do                                                                     
!                                                                                
!     fname_out=trim(trim(out_dir)//trim(mdlname)//'/OB4DA_PF_'//trim(sd)//'_'//trim(ed)//'_STAT_'//trim(obsname)//'.nc')
!     print*,"-** OUTPUT OB4DA PF FILE NAME :: ",fname_out                       
!     call check(nf90_create(trim(fname_out),NF90_NETCDF4,ncid1))                
!     call check(nf90_def_dim(ncid1, "time", nt, nt_dimid))                      
!     call check(nf90_def_dim(ncid1, "variable", nvar, nvar_dimid))              
!     data_dimid_t  =(/nt_dimid/)                                                
!     data_dimid_var=(/nvar_dimid/)                                              
!     data_dimid_2d =(/ nvar_dimid, nt_dimid /)                               
!                                                                                
!     call check(nf90_def_var(ncid1, "variable",NF90_REAL, data_dimid_var, varid00))
!     call check(nf90_put_att(ncid1, varid00, "long_name", "1:u, 2:v, 3:t, 4:q, 5:rh, 6:ps"))
!     call check(nf90_inq_varid(ncid1,"variable",varid00))                       
!     call check(nf90_put_var(ncid1,varid00,var_dim))                            
!                                                                                
!     call check(nf90_def_var(ncid1, "OmB",NF90_REAL, data_dimid_2d, varid1))    
!     call check(nf90_put_var(ncid1, varid1,  var))                              
!     call check(nf90_def_var(ncid1, "CmB",NF90_REAL, data_dimid_2d, varid2))    
!     call check(nf90_put_var(ncid1, varid2,  var1))                             
!                                                                                
!     call check(nf90_close(ncid1))                                              
!                                                                                
!  end subroutine ncdf_nch_pf



  subroutine ncdf_nch_prs_pf(var, lprs, nt, obsname, mdlname, out_dir, sd ,ed)
     character (len=256), intent(in)    :: obsname, out_dir, sd, ed, mdlname
     character (len=256)                :: fname_out
     integer, intent(in)                :: nt
     integer, dimension(:), intent(in)  :: lprs
     real, dimension(:,:,:,:), intent(in) :: var 
     integer :: nvar, ivar, nprs, nstat
     integer :: ncid1
     integer :: varid00, varid01
     integer :: varid1 
     integer :: nt_dimid, nvar_dimid, nprs_dimid, nstat_dimid
     integer, dimension(4) :: data_dimid_3d
     integer, dimension(1) :: data_dimid_t, data_dimid_var, data_dimid_prs, data_dimid_stat
     integer, dimension(:), allocatable :: var_dim
  
     nstat= size(var,2)
     nvar = size(var,1)
     ALLOCATE(var_dim(nvar))
     do ivar = 1,nvar
      var_dim(ivar) = ivar-1
     end do
     nprs = size(lprs)

     fname_out=trim(trim(out_dir)//trim(mdlname)//'/OB4DA_PF_'//trim(sd)//'_'//trim(ed)//'_STAT_'//trim(obsname)//'.nc')
     print*,"-** OUTPUT OB4DA PF FILE NAME :: ",fname_out
     call check(nf90_create(trim(fname_out),NF90_NETCDF4,ncid1))
     call check(nf90_def_dim(ncid1, "time", nt, nt_dimid))
     call check(nf90_def_dim(ncid1, "variable", nvar, nvar_dimid))
     call check(nf90_def_dim(ncid1, "pressure", nprs, nprs_dimid))
     call check(nf90_def_dim(ncid1, "stat", nstat, nstat_dimid))

     data_dimid_t   =(/nt_dimid/)
     data_dimid_var =(/nvar_dimid/)
     data_dimid_prs =(/nprs_dimid/)
     data_dimid_stat=(/nstat_dimid/)

     data_dimid_3d =(/ nvar_dimid, nstat_dimid, nprs_dimid, nt_dimid /)

     call check(nf90_def_var(ncid1, "variable",NF90_REAL, data_dimid_var, varid00))
     call check(nf90_put_att(ncid1, varid00, "long_name", "1:u, 2:v, 3:t, 4:q, 5:rh, 6:ps"))
     call check(nf90_inq_varid(ncid1,"variable",varid00))
     call check(nf90_put_var(ncid1,varid00,var_dim))

     call check(nf90_def_var(ncid1, "pressure",NF90_REAL, data_dimid_prs, varid01))
     call check(nf90_put_att(ncid1, varid01, "long_namep", "pressure"))
     call check(nf90_inq_varid(ncid1,"pressure",varid01))
     call check(nf90_put_var(ncid1,varid01,lprs))

     call check(nf90_def_var(ncid1, "OmB",NF90_REAL, data_dimid_3d, varid1))
     call check(nf90_put_var(ncid1, varid1,  var))

     call check(nf90_close(ncid1))

  end subroutine ncdf_nch_prs_pf



!-----------------------------------------------------
 subroutine check(status)
!-----------------------------------------------------
!NetCDF function check
  use netcdf
  integer,intent (in) :: status
  if(status /= nf90_noerr) then
  print *, trim(nf90_strerror(status)),status
  stop "NetCDF MAKE PF error --> Stopped"
  end if
 end subroutine check

end module sub_make_pf_ncdf
