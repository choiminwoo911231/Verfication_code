module sub_find_grid
  implicit none
  

  contains

!------------------------------------------------------
!Function FIND IX, IY GRID from obs latlon (not include channel)
   subroutine func_MAP_prs_avg(prs_dump, lon_dump, lat_dump, var, OB_stat_prs, B_stat_prs, prs_min, prs_max, idx_use)
    integer, intent(in)                :: idx_use
    integer, dimension(:), intent(in)  :: prs_min, prs_max
    real, dimension(:),    intent(in)  :: lon_dump, lat_dump
    real, dimension(:,:),  intent(in)  :: var
    real, dimension(:),    intent(in)  :: prs_dump
    real, dimension(:,:,:), intent(inout) :: OB_stat_prs, B_stat_prs 
    real, dimension(:,:,:), allocatable   :: var_stat, bkg
    real, parameter :: miss=-999.

    call func_prs(prs_dump, lon_dump, lat_dump, var, var_stat, bkg, prs_min, prs_max, idx_use); OB_stat_prs=var_stat; B_stat_prs=bkg
   end subroutine func_MAP_prs_avg
!------------------------------------------------------


!------------------------------------------------------
!Function STAT_PRS
!------------------------------------------------------
   subroutine func_prs(prs, lon, lat, var, stat, bkg, prs_min, prs_max, idx_use)
    integer, parameter :: nnx=121, nny=61
    real, parameter    :: miss=-999.
!    real, parameter    :: botlon=0., botlat=-90., numlat=60., lon_gap=3.0, lat_gap=3.0
    real, parameter    :: botlon=0., botlat=-90., numlat=60., lon_gap=3.0, lat_gap=3.0
    real, dimension(:,:,:), allocatable :: var_MAP1, var_MAP2 
    real, dimension(:,:,:), allocatable :: gnum
    integer  :: iobs, nobs, ix, iy, iprs, dnum, i,id
    real, dimension(:), allocatable  :: pix, piy
    logical, dimension(:), allocatable  :: mask

    integer, intent(in)                 :: idx_use
    integer, dimension(:), intent(in)   :: prs_min, prs_max
    real, dimension(:),    intent(in)   :: lon, lat
    real, dimension(:,:),  intent(in)   :: var
    real, dimension(:),    intent(in)   :: prs
    real, dimension(:,:,:), allocatable, intent(out) :: stat, bkg

    ALLOCATE(stat(nnx,nny,size(prs_min)))
    ALLOCATE(bkg(nnx,nny,size(prs_min)))
    ALLOCATE(var_MAP1(nnx,nny,size(prs_min)))
    ALLOCATE(var_MAP2(nnx,nny,size(prs_min)))
    ALLOCATE(gnum(nnx,nny,size(prs_min)))
    var_MAP1(:,:,:)=0.; var_MAP2(:,:,:)=0.; gnum(:,:,:)=0.
    nobs = size(var,2)

    do iprs=1,size(prs_min) !1
     ALLOCATE(mask(size(prs)))
     if (idx_use .eq. 1) then !2
      mask = prs(var(5,:)) .gt. prs_min(iprs) .and. prs(var(5,:)) .le. prs_max(iprs)
      do iobs = 1,nobs !3
       if (mask(var(5,iobs)) .eqv. .True.) then !4
        if (var(1,iobs) .gt. miss .and. var(2,iobs) .gt. miss) then !5
         ix=int(((lon(var(5,iobs))-botlon)/lon_gap)+1)
         iy=int(numlat-((lat(var(5,iobs))-botlat)/lat_gap)+1)
         var_MAP1(ix,iy,iprs) = var_MAP1(ix,iy,iprs) + var(1,iobs)
         var_MAP2(ix,iy,iprs) = var_MAP2(ix,iy,iprs) + var(2,iobs)
         gnum(ix,iy,iprs) = gnum(ix,iy,iprs) + 1
        end if !5
       end if !4
      end do !3
     else !2
      mask = prs(:) .gt. prs_min(iprs) .and. prs(:) .le. prs_max(iprs)
      do iobs = 1,nobs !3
       if (mask(iobs) .eqv. .True.) then !4
        if (var(1,iobs) .gt. miss .and. var(2,iobs) .gt. miss) then !5
         ix=int(((lon(iobs)-botlon)/lon_gap)+1)
         iy=int(numlat-((lat(iobs)-botlat)/lat_gap)+1)
         var_MAP1(ix,iy,iprs) = var_MAP1(ix,iy,iprs) + var(1,iobs)
         var_MAP2(ix,iy,iprs) = var_MAP2(ix,iy,iprs) + var(2,iobs)
         gnum(ix,iy,iprs) = gnum(ix,iy,iprs) + 1
        end if !5
       end if !4
      end do !3
     end if !2

     where (gnum(:,:,iprs) .gt. 2)
      var_MAP1(:,:,iprs) = var_MAP1(:,:,iprs)/gnum(:,:,iprs)
      var_MAP2(:,:,iprs) = var_MAP2(:,:,iprs)/gnum(:,:,iprs)
     elsewhere
      var_MAP1(:,:,iprs) = miss
      var_MAP2(:,:,iprs) = miss
     endwhere
    DEALLOCATE(mask)
    end do !1

    stat(:,:,:)  = var_MAP1(:,:,:) - var_MAP2(:,:,:)
    bkg(:,:,:)   = var_MAP2(:,:,:)
    !miss value
    where(var_MAP1(:,:,:) .le. miss) stat(:,:,:)  = miss
    where(var_MAP2(:,:,:) .le. miss) stat(:,:,:)  = miss

    DEALLOCATE(var_MAP1); DEALLOCATE(var_MAP2); DEALLOCATE(gnum)


   end subroutine func_prs
!------------------------------------------------------


!------------------------------------------------------
!Function FIND IX, IY GRID from obs latlon (surface, scat, grndgnss)
   subroutine func_MAP_sfc_avg(lon, lat, var, OB_stat, CB_stat, B_stat, idx_use)
   integer, parameter :: nnx=121, nny=61
   real, parameter    :: miss=-999.
   real, parameter    :: botlon=0., botlat=-90., numlat=60., lon_gap=3.0, lat_gap=3.0

   integer, intent(in)                 :: idx_use
   real, dimension(:),   intent(in)    :: lon, lat
   real, dimension(:,:), intent(in)    :: var
   real, dimension(:,:), intent(inout) :: OB_stat, CB_stat, B_stat

   real, dimension(:,:,:), allocatable :: var_MAP
   real, dimension(:,:,:), allocatable :: gnum
   integer  :: iobs, nobs, ivar, nvar, ix, iy

   ALLOCATE(var_MAP(size(var,1),nnx,nny))
   ALLOCATE(gnum(size(var,1),nnx,nny))
   var_MAP(:,:,:) = 0.; gnum(:,:,:)=0.
   nobs = size(var,2)
   nvar = size(var,1)
 
   do ivar = 1,3  
    gnum(ivar,:,:)=0
    do iobs = 1,nobs
     if (idx_use .eq. 1) then
      if (var(ivar,iobs) .ne. miss) then
       ix=int(((lon(var(5,iobs))-botlon)/lon_gap)+1)
       iy=int(numlat-((lat(var(5,iobs))-botlat)/lat_gap)+1)
       var_MAP(ivar,ix,iy) = var_MAP(ivar,ix,iy) + var(ivar,iobs)
       gnum(ivar,ix,iy) = gnum(ivar,ix,iy) + 1
      endif
     else
      if (var(ivar,iobs) .ne. miss) then
       ix=int(((lon(iobs)-botlon)/lon_gap)+1)
       iy=int(numlat-((lat(iobs)-botlat)/lat_gap)+1)
       var_MAP(ivar,ix,iy) = var_MAP(ivar,ix,iy) + var(ivar,iobs)
       gnum(ivar,ix,iy) = gnum(ivar,ix,iy) + 1
      endif 
     end if
    end do

    where (gnum(ivar,:,:) .gt. 0)
     var_MAP(ivar,:,:) = var_MAP(ivar,:,:)/gnum(ivar,:,:)
    elsewhere
     var_MAP(ivar,:,:) = miss
    endwhere
   end do
 
   OB_stat(:,:) = var_MAP(1,:,:) - var_MAP(2,:,:)
   CB_stat(:,:) = var_MAP(3,:,:) - var_MAP(2,:,:)
   B_stat(:,:)  = var_MAP(2,:,:)

   !miss value
   where(var_MAP(1,:,:) .le. miss) OB_stat(:,:) = miss
   where(var_MAP(1,:,:) .le. miss) CB_stat(:,:) = miss
   where(var_MAP(2,:,:) .le. miss) OB_stat(:,:) = miss
   where(var_MAP(2,:,:) .le. miss) CB_stat(:,:) = miss

   DEALLOCATE(var_MAP); DEALLOCATE(gnum)
   end subroutine func_MAP_sfc_avg
!-----------------------------------------------------



!------------------------------------------------------ 
!Function FIND IX, IY GRID from obs latlon (include chan = stl)
   subroutine func_MAP_chn_avg (lon_dump, lat_dump, satid, sat_list, qc, var, OB_stat, CB_stat, B_stat)
   integer, parameter :: nnx=121, nny=61
   real, parameter    :: miss=-999.
   real, parameter    :: botlon=0., botlat=-90., numlat=60., lon_gap=3.0, lat_gap=3.0

   integer, dimension(:), intent(in)       :: sat_list
   real, dimension(:), intent(in)          :: lon_dump, lat_dump, satid
   real, dimension(:,:), intent(in)        :: qc
   real, dimension(:,:,:), intent(in)      :: var
   real, dimension(:,:,:), intent(inout)   :: OB_stat, CB_stat, B_stat

   real, dimension(:,:,:,:), allocatable   :: var_MAP
   real, dimension(:,:,:), allocatable     :: var1, gnum
   integer  :: iobs, nobs, ivar, nchan, ix, iy, ich, i

   ALLOCATE(var1(size(var,1),size(var,2),size(var,3)))      
   ALLOCATE(var_MAP(size(var,1),nnx,nny,size(var,2)))      
   ALLOCATE(gnum(nnx,nny,size(var,2)))
   var_MAP(:,:,:,:) = 0.0; gnum(:,:,:) =0.
   nobs = size(lon_dump)
   nchan = size(var,2)

   var1(:,:,:)=var(:,:,:)
   if (sat_list(1) .ne. -999) then
   var1(:,:,:)=miss
   !SATID_FILTER
   do ich = 1,size(var,2)
    do i =1,size(sat_list)
     where (satid(:) == sat_list(i))
      var1(1,ich,:) = var(1,ich,:)
      var1(2,ich,:) = var(2,ich,:)
      var1(3,ich,:) = var(3,ich,:)
     end where
    end do
   end do
   end if
  

   do ich = 1,nchan
    gnum(:,:,ich)=0
    do iobs = 1,nobs
     ix=int(((lon_dump(iobs)-botlon)/lon_gap)+1)
     iy=int(numlat-((lat_dump(iobs)-botlat)/lat_gap)+1)

     if (var(1,ich,iobs) .gt. miss) then
      if (qc(ich,iobs) .eq. 0) then
       var_MAP(1,ix,iy,ich) = var_MAP(1,ix,iy,ich) + var1(1,ich,iobs)
       var_MAP(2,ix,iy,ich) = var_MAP(2,ix,iy,ich) + var1(2,ich,iobs)
       var_MAP(3,ix,iy,ich) = var_MAP(3,ix,iy,ich) + var1(3,ich,iobs)
       gnum(ix,iy,ich) = gnum(ix,iy,ich) + 1
      endif
     endif
    end do

    where (gnum(:,:,ich) .gt. 0)
     var_MAP(1,:,:,ich) = var_MAP(1,:,:,ich)/gnum(:,:,ich)
     var_MAP(2,:,:,ich) = var_MAP(2,:,:,ich)/gnum(:,:,ich)
     var_MAP(3,:,:,ich) = var_MAP(3,:,:,ich)/gnum(:,:,ich)
    elsewhere
     var_MAP(1,:,:,ich) = miss
     var_MAP(2,:,:,ich) = miss
     var_MAP(3,:,:,ich) = miss
    endwhere
   end do

   OB_stat(:,:,:) = var_MAP(1,:,:,:) - var_MAP(2,:,:,:)
   CB_stat(:,:,:) = var_MAP(3,:,:,:) - var_MAP(2,:,:,:)
   B_stat(:,:,:)  = var_MAP(2,:,:,:)

   !miss value
   where(var_MAP(1,:,:,:) .le. miss) OB_stat(:,:,:) = miss
   where(var_MAP(2,:,:,:) .le. miss) OB_stat(:,:,:) = miss
   where(var_MAP(1,:,:,:) .le. miss) CB_stat(:,:,:) = miss
   where(var_MAP(2,:,:,:) .le. miss) CB_stat(:,:,:) = miss


   DEALLOCATE(var_MAP); DEALLOCATE(gnum)
   end subroutine func_MAP_chn_avg
!-----------------------------------------------------




end module sub_find_grid


