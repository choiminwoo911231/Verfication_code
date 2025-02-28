main   = main_map
main00 = main_map_count
main1  = main_pf
module1  = sub_read
module2  = sub_stat_map
module22 = sub_stat_map_count
module3  = sub_stat_pf
module4  = sub_make_map_ncdf
module44 = sub_make_map_count_ncdf
module5  = sub_make_pf_ncdf


exe  = cal_ob4da_map_$(MDL)_$(OBSNAME)
exe0 = cal_ob4da_map_count_$(MDL)_$(OBSNAME)
exe1 = cal_ob4da_pf_$(MDL)_$(OBSNAME)
src_path = $(HOME)/SRC/FORTRAN_3deg
ncdf_lib = -I/opt/kma/kma_lib/apps/netcdf-fortran/4.5.2/INTEL/200/include -L/opt/kma/kma_lib/apps/netcdf-fortran/4.5.2/INTEL/200/lib -lnetcdf -lnetcdff

all_map: compile_map run_map
all_map_count: compile_map_count run_map_count
all_pf: compile_pf run_pf

compile_map:
	cd $(src_path); \
	ifort -c $(module1).f90 -o $(module1).o $(ncdf_lib)
	ifort -c $(module2).f90 -o $(module2).o
	ifort -c $(module4).f90 -o $(module4).o $(ncdf_lib)
	ifort -mcmodel=large -c $(main).f90 -o $(main).o
	ifort $(main).f90 $(module1).o $(module2).o $(module4).o -o $(exe).exe -O2 $(ncdf_lib)
 
compile_map_count:
	cd $(src_path); \
	ifort -c $(module1).f90 -o $(module1).o $(ncdf_lib)
	ifort -c $(module22).f90 -o $(module22).o
	ifort -c $(module44).f90 -o $(module44).o $(ncdf_lib)
	ifort -mcmodel=large -c $(main00).f90 -o $(main00).o
	ifort $(main00).f90 $(module1).o $(module22).o $(module44).o -o $(exe0).exe -O2 $(ncdf_lib)

compile_pf:
	cd $(src_path); \
	ifort -c $(module1).f90 -o $(module1).o $(ncdf_lib)
	ifort -c $(module3).f90 -o $(module3).o
	ifort -c $(module5).f90 -o $(module5).o $(ncdf_lib)
	ifort -mcmodel=large -c $(main1).f90 -o $(main1).o
	ifort $(main1).f90 $(module1).o $(module3).o $(module5).o -o $(exe1).exe -O2 $(ncdf_lib)

run_map:
	cd $(src_path); \
	./$(exe).exe

run_map_count:
	cd $(src_path); \
	./$(exe0).exe

run_pf:
	cd $(src_path); \
	./$(exe1).exe

clean_pf:
	rm -f $(exe1).exe

clean_map_count:                                                                       
	rm -f $(exe0).exe  

clean_map:
	rm -f $(exe).exe
