module sub_stat
  implicit none

  contains

!------------------------------------------------------
!gpsro
!------------------------------------------------------

   subroutine stat_pf_gpsro(prs_dump, var1, OB_stat_prs, prs_min, prs_max)
     real, dimension(:,:), intent(in)    :: var1 !1:var 2:nprs
     real, dimension(:),   intent(inout) :: prs_dump
     real, dimension(:,:,:), intent(inout) :: OB_stat_prs !1:var 2:nprs
     integer, dimension(:), intent(in)   :: prs_min, prs_max
     real, dimension(:,:), allocatable     :: OmB
     

     ALLOCATE(OmB(2,size(prs_min)))
     call func_OB_prs(prs_dump, var1(1,:), var1(2,:), OmB, prs_min, prs_max);  OB_stat_prs(1,1,:) = OmB(1,:)
     call func_OB_prs(prs_dump, var1(1,:), var1(2,:), OmB, prs_min, prs_max);  OB_stat_prs(1,2,:) = OmB(2,:)

   end subroutine stat_pf_gpsro
!------------------------------------------------------


!------------------------------------------------------
!aircraft
!------------------------------------------------------

   subroutine stat_pf_aircraft(prs_dump, var1, var2, var3, OB_stat_prs, prs_min, prs_max)
     real, dimension(:,:), intent(in)       :: var1, var2, var3
     real, dimension(:),   intent(inout)    :: prs_dump
     real, dimension(:,:,:), intent(inout)  :: OB_stat_prs
     integer, dimension(:), intent(in)      :: prs_min, prs_max
     real, dimension(:,:), allocatable      :: OmB

     ALLOCATE(OmB(2,size(prs_min)))
     call func_OB_prs(prs_dump,var1(1,:),var1(2,:),OmB,prs_min, prs_max);  OB_stat_prs(1,1,:) = OmB(1,:)
     call func_OB_prs(prs_dump,var2(1,:),var2(2,:),OmB,prs_min, prs_max);  OB_stat_prs(2,1,:) = OmB(1,:)
     call func_OB_prs(prs_dump,var3(1,:),var3(2,:),OmB,prs_min, prs_max);  OB_stat_prs(3,1,:) = OmB(1,:)
     call func_OB_prs(prs_dump,var1(1,:),var1(2,:),OmB,prs_min, prs_max);  OB_stat_prs(1,2,:) = OmB(2,:)
     call func_OB_prs(prs_dump,var2(1,:),var2(2,:),OmB,prs_min, prs_max);  OB_stat_prs(2,2,:) = OmB(2,:)
     call func_OB_prs(prs_dump,var3(1,:),var3(2,:),OmB,prs_min, prs_max);  OB_stat_prs(3,2,:) = OmB(2,:)

   end subroutine stat_pf_aircraft
!------------------------------------------------------


!------------------------------------------------------
!grndgnss
!------------------------------------------------------

   subroutine stat_pf_gnss(var1, OB_stat, CB_stat)
     real, dimension(:,:), intent(in)    :: var1
     real, dimension(:,:),   intent(inout) :: OB_stat, CB_stat
     real  :: OmB, OmB_m

     call func_OB(var1(1,:),var1(2,:),  OmB);   OB_stat(1,1) = OmB
     call func_OBm(var1(1,:),var1(2,:), OmB_m);   OB_stat(2,1) = OmB_m
     call func_OB(var1(3,:),var1(2,:),  OmB);   CB_stat(1,1) = OmB
     call func_OBm(var1(3,:),var1(2,:), OmB_m);   CB_stat(2,1) = OmB_m

   end subroutine stat_pf_gnss
!------------------------------------------------------


!------------------------------------------------------
!scat
!------------------------------------------------------

   subroutine stat_pf_scat(var1, var2, OB_stat, CB_stat)
     real, dimension(:,:), intent(in)     :: var1, var2
     real, dimension(:,:),   intent(inout)  :: OB_stat, CB_stat
     real  :: OmB, OmB_m

     call func_OB(var1(1,:),var1(2,:),OmB);  OB_stat(1,1) = OmB
     call func_OB(var2(1,:),var2(2,:),OmB);  OB_stat(1,2) = OmB
     call func_OBm(var1(1,:),var1(2,:),OmB_m);  OB_stat(2,1) = OmB_m
     call func_OBm(var2(1,:),var2(2,:),OmB_m);  OB_stat(2,2) = OmB_m

     call func_OB(var1(3,:),var1(2,:),OmB);  CB_stat(1,1) = OmB
     call func_OB(var2(3,:),var2(2,:),OmB);  CB_stat(1,2) = OmB
     call func_OBm(var1(3,:),var1(2,:),OmB_m);  CB_stat(2,1) = OmB_m
     call func_OBm(var2(3,:),var2(2,:),OmB_m);  CB_stat(2,2) = OmB_m
   end subroutine stat_pf_scat
!------------------------------------------------------


!------------------------------------------------------
!sonde
!------------------------------------------------------

   subroutine stat_pf_sonde(prs_dump, var1, var2, var3, var4, OB_stat_prs, prs_min, prs_max)
     real, dimension(:,:), intent(in)     :: var1, var2, var3, var4
     real, dimension(:),   intent(inout)  :: prs_dump
     real, dimension(:,:,:), intent(inout)  :: OB_stat_prs
     integer, dimension(:), intent(in)    :: prs_min, prs_max
     real, dimension(:,:), allocatable      :: OmB

     ALLOCATE(OmB(2,size(prs_min)))
     call func_OB_sonde_prs(prs_dump,var1(1,:),var1(2,:),var1(5,:),OmB,prs_min, prs_max);  OB_stat_prs(1,1,:) = OmB(1,:)
     call func_OB_sonde_prs(prs_dump,var2(1,:),var2(2,:),var2(5,:),OmB,prs_min, prs_max);  OB_stat_prs(2,1,:) = OmB(1,:)
     call func_OB_sonde_prs(prs_dump,var3(1,:),var3(2,:),var3(5,:),OmB,prs_min, prs_max);  OB_stat_prs(3,1,:) = OmB(1,:)
     call func_OB_sonde_prs(prs_dump,var4(1,:),var4(2,:),var4(5,:),OmB,prs_min, prs_max);  OB_stat_prs(4,1,:) = OmB(1,:)
     call func_OB_sonde_prs(prs_dump,var1(1,:),var1(2,:),var1(5,:),OmB,prs_min, prs_max);  OB_stat_prs(1,2,:) = OmB(2,:)
     call func_OB_sonde_prs(prs_dump,var2(1,:),var2(2,:),var2(5,:),OmB,prs_min, prs_max);  OB_stat_prs(2,2,:) = OmB(2,:)
     call func_OB_sonde_prs(prs_dump,var3(1,:),var3(2,:),var3(5,:),OmB,prs_min, prs_max);  OB_stat_prs(3,2,:) = OmB(2,:)
     call func_OB_sonde_prs(prs_dump,var4(1,:),var4(2,:),var4(5,:),OmB,prs_min, prs_max);  OB_stat_prs(4,2,:) = OmB(2,:)

   end subroutine stat_pf_sonde
!------------------------------------------------------


!------------------------------------------------------
!surface
!------------------------------------------------------

   subroutine stat_pf_surface(var1, var2, var3, var4, var5, var6, OB_stat, CB_stat)
     real, dimension(:,:), intent(in)     :: var1, var2, var3, var4, var5, var6
     real, dimension(:,:),   intent(inout)  :: OB_stat, CB_stat
     real  :: OmB, OmB_m

     call func_OB(var1(1,:),var1(2,:),OmB);  OB_stat(1,1)  = OmB
     call func_OB(var2(1,:),var2(2,:),OmB);  OB_stat(1,2)  = OmB
     call func_OB(var3(1,:),var3(2,:),OmB);  OB_stat(1,3)  = OmB
     call func_OB(var4(1,:),var4(2,:),OmB);  OB_stat(1,4)  = OmB
     call func_OB(var5(1,:),var5(2,:),OmB);  OB_stat(1,5)  = OmB
     call func_OB(var6(1,:),var6(2,:),OmB);  OB_stat(1,6)  = OmB
     call func_OBm(var1(1,:),var1(2,:),OmB_m);  OB_stat(2,1)  = OmB_m
     call func_OBm(var2(1,:),var2(2,:),OmB_m);  OB_stat(2,2)  = OmB_m
     call func_OBm(var3(1,:),var3(2,:),OmB_m);  OB_stat(2,3)  = OmB_m
     call func_OBm(var4(1,:),var4(2,:),OmB_m);  OB_stat(2,4)  = OmB_m
     call func_OBm(var5(1,:),var5(2,:),OmB_m);  OB_stat(2,5)  = OmB_m
     call func_OBm(var6(1,:),var6(2,:),OmB_m);  OB_stat(2,6)  = OmB_m

     call func_OB(var1(3,:),var1(2,:),OmB);  CB_stat(1,1)  = OmB
     call func_OB(var2(3,:),var2(2,:),OmB);  CB_stat(1,2)  = OmB
     call func_OB(var3(3,:),var3(2,:),OmB);  CB_stat(1,3)  = OmB
     call func_OB(var4(3,:),var4(2,:),OmB);  CB_stat(1,4)  = OmB
     call func_OB(var5(3,:),var5(2,:),OmB);  CB_stat(1,5)  = OmB
     call func_OB(var6(3,:),var6(2,:),OmB);  CB_stat(1,6)  = OmB
     call func_OBm(var1(3,:),var1(2,:),OmB_m);  CB_stat(2,1)  = OmB_m
     call func_OBm(var2(3,:),var2(2,:),OmB_m);  CB_stat(2,2)  = OmB_m
     call func_OBm(var3(3,:),var3(2,:),OmB_m);  CB_stat(2,3)  = OmB_m
     call func_OBm(var4(3,:),var4(2,:),OmB_m);  CB_stat(2,4)  = OmB_m
     call func_OBm(var5(3,:),var5(2,:),OmB_m);  CB_stat(2,5)  = OmB_m
     call func_OBm(var6(3,:),var6(2,:),OmB_m);  CB_stat(2,6)  = OmB_m

   end subroutine stat_pf_surface
!------------------------------------------------------

!------------------------------------------------------
!amv
!------------------------------------------------------

   subroutine stat_pf_amv(prs_dump, var1, var2, OB_stat_prs, prs_min, prs_max)
     real, dimension(:,:), intent(in)     :: var1, var2
     real, dimension(:),   intent(inout)  :: prs_dump
     real, dimension(:,:,:), intent(inout)  :: OB_stat_prs
     integer, dimension(:), intent(in)    :: prs_min, prs_max
     real, dimension(:,:), allocatable      :: OmB

     ALLOCATE(OmB(2,size(prs_min)))
     call func_OB_prs(prs_dump,var1(1,:),var1(2,:),OmB,prs_min, prs_max);  OB_stat_prs(1,1,:) = OmB(1,:) !var1 std
     call func_OB_prs(prs_dump,var2(1,:),var2(2,:),OmB,prs_min, prs_max);  OB_stat_prs(2,1,:) = OmB(1,:) !var2 std
     call func_OB_prs(prs_dump,var1(1,:),var1(2,:),OmB,prs_min, prs_max);  OB_stat_prs(1,2,:) = OmB(2,:) !var1 mean
     call func_OB_prs(prs_dump,var2(1,:),var2(2,:),OmB,prs_min, prs_max);  OB_stat_prs(2,2,:) = OmB(2,:) !var2 mean


   end subroutine stat_pf_amv
!------------------------------------------------------

!------------------------------------------------------
!Function STDDEV
!------------------------------------------------------

   subroutine func_OB(O, B, OB)
     real, dimension(:), allocatable :: dump
     real, dimension(:), intent(in)  :: O, B
     real, intent(out) :: OB
     real  :: dump1, avg

     ALLOCATE(dump(size(O)))
     dump = O-B
     avg   = sum(dump)/size(O)
     dump1 = sum((dump-avg)**2)
     OB    = sqrt(dump1/size(O))
     
   end subroutine func_OB
!------------------------------------------------------

!------------------------------------------------------                         
!Function MEAN
!------------------------------------------------------                         
                                                                                
   subroutine func_OBm(O, B, avg)                                            
     real, dimension(:), allocatable :: dump                                    
     real, dimension(:), intent(in)  :: O, B                                    
     real, intent(out) :: avg                                               
     real  :: dump1                                                             

     ALLOCATE(dump(size(O)))
     dump = O-B
     avg   = sum(dump)/size(O)

   end subroutine func_OBm                                                       
!------------------------------------------------------ 




!------------------------------------------------------
!Function for pressure
!------------------------------------------------------
   subroutine func_OB_prs(prs, O, B, OB, prs_min, prs_max)
     real, dimension(:), allocatable    :: dump, OO,BB
     integer :: idx
     real, dimension(:), intent(in)    :: O, B, prs
     real, dimension(:,:), intent(inout) :: OB
     integer, dimension(:), intent(in) :: prs_min, prs_max
     real  :: avg, dump1
     integer :: i, thsrd, miss_c

     do i=1,size(prs_min)
      allocate(dump(size(O))); dump(:)=-999.
      where(prs(:) .gt. prs_min(i) .and. prs(:) .le. prs_max(i)) 
       dump(:) = O(:)-B(:)
      end where
      where(O(:) .le. -999.) dump(:)=0.
      where(B(:) .le. -999.) dump(:)=0.
      where(dump(:) .le. -999.) dump(:)=0.
      miss_c = count(dump(:) .eq. 0.)
      avg    = sum(dump)/(size(O)-miss_c)
      dump1  = sum((dump-avg)**2)
      OB(1,i)  = sqrt(dump1/(size(O)-miss_c))
      OB(2,i)  = avg
      deallocate(dump)
     end do
   end subroutine func_OB_prs
!------------------------------------------------------


!------------------------------------------------------
!Function for pressure_sonde
!------------------------------------------------------
   subroutine func_OB_sonde_prs(prs, O, B, pidx, OB, prs_min, prs_max)
     real, dimension(:), allocatable    :: dump, OO,BB
     integer :: idx
     real, dimension(:), intent(in)    :: O, B, prs, pidx
     real, dimension(:,:), intent(inout) :: OB
     integer, dimension(:), intent(in) :: prs_min, prs_max
     real  :: avg, dump1
     integer :: i, thsrd, miss_c
  
     do i=1,size(prs_min)
      allocate(dump(size(O))); dump(:)=-999.
      where(prs(pidx) .gt. prs_min(i) .and. prs(pidx) .le. prs_max(i))
!      where(prs(:) .gt. lprs(i)-thsrd .and. prs(:) .le. lprs(i)+thsrd)
       dump(:) = O(:)-B(:)
      end where
      where(O(:) .le. -999.) dump(:)=0.
      where(B(:) .le. -999.) dump(:)=0.
      where(dump(:) .le. -999.) dump(:)=0.
      miss_c = count(dump(:) .eq. 0.)
      avg    = sum(dump)/(size(O)-miss_c)
      dump1  = sum((dump-avg)**2)
      OB(1,i)  = sqrt(dump1/(size(O)-miss_c))
      OB(2,i)  = avg
      deallocate(dump)
     end do
   end subroutine func_OB_sonde_prs
!------------------------------------------------------



!------------------------------------------------------
!stl
!------------------------------------------------------
   subroutine stat_pf_stl(qc_dump, ch_dump, satid_dump, sat_list, var, vare, OBm_stat, CBm_stat)
     integer, dimension(:),  intent(in)    :: sat_list
     real, dimension(:),     intent(in)    :: ch_dump, satid_dump
     real, dimension(:,:),   intent(in)    :: qc_dump, vare
     real, dimension(:,:,:), intent(in)    :: var
     real, dimension(:,:),   intent(inout) :: OBm_stat, CBm_stat

     real, dimension(:,:),   allocatable   :: dump0, dump00, dump2, dump3, dump4, dump5
     real                                  :: avg, imsi, imsi1, avg1
     integer                               :: ich, miss_o, miss_c, i, miss_d

     !O-B stddev
     ALLOCATE(dump0(size(ch_dump),size(var,3)))
     ALLOCATE(dump00(size(ch_dump),size(var,3)))
     ALLOCATE(dump2(size(ch_dump),size(var,3)))
     ALLOCATE(dump3(size(ch_dump),size(var,3)))
     ALLOCATE(dump4(size(ch_dump),size(var,3)))
     ALLOCATE(dump5(size(ch_dump),size(var,3)))

     !QC_FLAG
     dump3(:,:) = var(1,:,:) - var(2,:,:)
     dump5(:,:) = var(3,:,:) - var(2,:,:)
     where (qc_dump(:,:) .ne. 0)
      dump3(:,:) = -999.
      dump5(:,:) = -999.
     end where
     where (var(1,:,:) .le. -999.) dump3(:,:)=-999.
     where (var(2,:,:) .le. -999.) dump3(:,:)=-999.
     where (var(3,:,:) .le. -999.) dump5(:,:)=-999.
     where (var(2,:,:) .le. -999.) dump5(:,:)=-999.

     dump2(:,:)=dump3(:,:)
     dump4(:,:)=dump5(:,:)
     if (sat_list(1) .ne. -999) then
     dump2(:,:)=-999.
     dump4(:,:)=-999.
     !SATID_FILTER
     do ich = 1,size(ch_dump)
      do i =1,size(sat_list)
       where (satid_dump(:) == sat_list(i))
        dump2(ich,:) = dump3(ich,:)
        dump4(ich,:) = dump5(ich,:)
       end where
      end do
     end do
     end if

     where (dump2(:,:) .le. -999.)  dump2(:,:)=0.
     where (dump2(:,:) .gt. 99999.) dump2(:,:)=0.
     where (dump4(:,:) .le. -999.)  dump4(:,:)=0.
     where (dump4(:,:) .gt. 99999.) dump4(:,:)=0.
     !CALCULATE
     do ich = 1,size(ch_dump)
       miss_o        = count(dump2(ich,:) .eq. 0.)
       miss_c        = count(dump4(ich,:) .eq. 0.)
       avg           = sum(dump2(ich,:))/(size(var,3)-miss_o)
       avg1          = sum(dump4(ich,:))/(size(var,3)-miss_c)
       dump0(ich,:)   = ((dump2(ich,:) - avg)**2)
       dump00(ich,:)  = ((dump4(ich,:) - avg1)**2)
       where (dump2(ich,:) .eq. 0.) dump0(ich,:)  = 0.
       where (dump4(ich,:) .eq. 0.) dump00(ich,:) = 0.
       imsi          = sum(dump0(ich,:))
       imsi1         = sum(dump00(ich,:))
       OBm_stat(1,ich)  = sqrt(imsi/(size(var,3)-miss_o))
       CBm_stat(1,ich)  = sqrt(imsi1/(size(var,3)-miss_c))
       OBm_stat(2,ich)  = avg
       CBm_stat(2,ich)  = avg1
     end do
   end subroutine stat_pf_stl
!------------------------------------------------------


end module sub_stat

