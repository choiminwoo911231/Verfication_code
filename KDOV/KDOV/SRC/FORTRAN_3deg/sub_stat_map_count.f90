module sub_map_count
  implicit none
  

  contains


!------------------------------------------------------
!Function STAT_PRS
!------------------------------------------------------
   subroutine func_MAP_prs_avg (prs, lon, lat, var, gnum1, prs_min, prs_max, idx_use)
    integer, parameter :: nnx=121, nny=61
    real, parameter    :: miss=-999.
    real, parameter    :: botlon=0., botlat=-90., numlat=60., lon_gap=3.0, lat_gap=3.0
    integer  :: iobs, nobs, ix, iy, iprs, i,id

    integer, intent(in)                   :: idx_use
    integer, dimension(:),  intent(in)    :: prs_min, prs_max
    real, dimension(:),     intent(in)    :: lon, lat
    real, dimension(:,:),   intent(in)    :: var
    real, dimension(:),     intent(in)    :: prs
    real, dimension(:,:,:), intent(inout) :: gnum1
    real, dimension(:,:,:), allocatable :: var_MAP
    logical, dimension(:), allocatable    :: mask

    if (ALLOCATED(var_MAP)) DEALLOCATE(var_MAP)
    ALLOCATE(var_MAP(nnx,nny,size(prs_min)))
    var_MAP(:,:,:)=0.
    nobs = size(var,2)

    do iprs=1,size(prs_min) !1
     if (ALLOCATED(mask)) DEALLOCATE(mask)
     ALLOCATE(mask(size(prs)))
     if (idx_use .eq. 1) then !2
      mask = prs(var(5,:)) .gt. prs_min(iprs) .and. prs(var(5,:)) .le. prs_max(iprs)
      do iobs = 1,nobs !3  (O, B, C  not include E)
        if (mask(var(5,iobs)) .eqv. .True.) then !4
         if (var(1,iobs) .gt. miss .or. var(2,iobs) .gt. miss) then !5
          ix=int(((lon(var(5,iobs))-botlon)/lon_gap)+1)
          iy=int(numlat-((lat(var(5,iobs))-botlat)/lat_gap)+1)
          var_MAP(ix,iy,iprs) = var_MAP(ix,iy,iprs) + 1
         end if !5
        end if !4
      end do !3
     else !2
      mask = prs(:) .gt. prs_min(iprs) .and. prs(:) .le. prs_max(iprs)
      do iobs = 1,nobs !3
       if (mask(iobs) .eqv. .True.) then !4
        if (var(1,iobs) .gt. miss .or. var(2,iobs) .gt. miss) then !5
         ix=int(((lon(iobs)-botlon)/lon_gap)+1)
         iy=int(numlat-((lat(iobs)-botlat)/lat_gap)+1)
         var_MAP(ix,iy,iprs) = var_MAP(ix,iy,iprs) + 1
        end if !5
       end if !4
      end do !3
     end if !2
    end do

   gnum1(:,:,:) = var_MAP(:,:,:)

   DEALLOCATE(var_MAP)
   end subroutine func_MAP_prs_avg
!------------------------------------------------------


!------------------------------------------------------
!Function FIND IX, IY GRID from obs latlon (surface, scat, grndgnss)
   subroutine func_MAP_sfc_avg(lon, lat, var, gnum1, idx_use)
   integer, parameter :: nnx=121, nny=61
   real, parameter    :: miss=-999.
   real, parameter    :: botlon=0., botlat=-90., numlat=60., lon_gap=3.0, lat_gap=3.0

   integer, intent(in)                 :: idx_use
   real, dimension(:),   intent(in)    :: lon, lat
   real, dimension(:,:), intent(in)    :: var

   real, dimension(:,:), intent(inout) :: gnum1
   real, dimension(:,:), allocatable :: var_MAP 
   integer  :: iobs, nobs, ivar, nvar, ix, iy

   ALLOCATE(var_MAP(nnx,nny))
   var_MAP(:,:)=0.
   nobs = size(var,2)
   nvar = size(var,1)
 
    var_MAP(:,:)=0
    do iobs = 1,nobs
     if (idx_use .eq. 1) then
      if (var(2,iobs) .ne. miss) then
       ix=int(((lon(var(5,iobs))-botlon)/lon_gap)+1)
       iy=int(numlat-((lat(var(5,iobs))-botlat)/lat_gap)+1)
       var_MAP(ix,iy) = var_MAP(ix,iy) + 1
      endif
     else
      if (var(2,iobs) .ne. miss) then
       ix=int(((lon(iobs)-botlon)/lon_gap)+1)
       iy=int(numlat-((lat(iobs)-botlat)/lat_gap)+1)
       var_MAP(ix,iy) = var_MAP(ix,iy) + 1
      endif 
     end if
    end do
   gnum1(:,:) = var_MAP(:,:)
 
   DEALLOCATE(var_MAP)
   end subroutine func_MAP_sfc_avg
!-----------------------------------------------------



!------------------------------------------------------ 
!Function FIND IX, IY GRID from obs latlon (include chan = stl)
   subroutine func_MAP_chn_avg (lon_dump, lat_dump, satid, sat_list, qc, var, gnum1)
   integer, parameter :: nnx=121, nny=61
   real, parameter    :: miss=-999.
   real, parameter    :: botlon=0., botlat=-90., numlat=60., lon_gap=3.0, lat_gap=3.0

   integer, dimension(:), intent(in)       :: sat_list
   real, dimension(:),    intent(in)          :: lon_dump, lat_dump, satid
   real, dimension(:,:), intent(in)        :: qc
   real, dimension(:,:,:), intent(in)      :: var

   real, dimension(:,:,:), allocatable     :: var1
   real, dimension(:,:,:), intent(inout)  :: gnum1
   real, dimension(:,:,:), allocatable :: var_MAP
   integer  :: iobs, nobs, ivar, nchan, ix, iy, ich, i

   ALLOCATE(var1(size(var,1),size(var,2),size(var,3)))      
   ALLOCATE(var_MAP(nnx,nny,size(var,2)))
!   var_MAP(:,:,:) =0.
   nobs = size(lon_dump)
   nchan = size(var,2)

   var1(:,:,:)=var(:,:,:)
   if (sat_list(1) .ne. -999) then
    var1(:,:,:)=miss   !initialization
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
    var_MAP(:,:,ich)=0
    do iobs = 1,nobs
     ix=int(((lon_dump(iobs)-botlon)/lon_gap)+1)
     iy=int(numlat-((lat_dump(iobs)-botlat)/lat_gap)+1)

     if (var1(1,ich,iobs) .gt. miss) then
      if (qc(ich,iobs) .eq. 0) then
       var_MAP(ix,iy,ich) = var_MAP(ix,iy,ich) + 1
      endif
     endif
    end do
   end do

   gnum1(:,:,:) = var_MAP(:,:,:)
   
   DEALLOCATE(var_MAP)
   end subroutine func_MAP_chn_avg
!-----------------------------------------------------




end module sub_map_count


