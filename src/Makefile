#----------------------------------------------------
# Makefile for DFTOOLS

#  note that make.macro is a link to the ad hoc ../Macrolib/macro.<machine> file
#  DFTOOLS uses the same macro then CDFTOOLS

include make.macro
#-----------------------------------------------------

BINDIR = ../bin

VPATH = $(BINDIR)

EXEC = change_unit_precip_jmm dft_q2_comp upside_down decumul calc_q2 change_unit_precip \
       calc_q2_6H upside_down_var change_unit_precip_raw  mk_lsm_var mk_lsm_from_rawfile \
       decumul_yearly mk_daily_mean mk_monthly_mean add_feb_29 dft_timeseries \
       dft_decumul_daily.f90
#-----------------------------------------------------

all: $(EXEC)

decumul: decumul.f90
	$(F90) decumul.f90 -o $(BINDIR)/decumul $(FFLAGS)

decumul_yearly: decumul_yearly.f90
	$(F90) decumul_yearly.f90 -o $(BINDIR)/decumul_yearly $(FFLAGS)

dft_decumul_daily: dft_decumul_daily.f90
	$(F90) dft_decumul_daily.f90 -o $(BINDIR)/dft_decumul_daily $(FFLAGS)

mk_daily_mean: mk_daily_mean.f90
	$(F90) mk_daily_mean.f90 -o $(BINDIR)/mk_daily_mean $(FFLAGS)

mk_monthly_mean: mk_monthly_mean.f90
	$(F90) mk_monthly_mean.f90 -o $(BINDIR)/mk_monthly_mean $(FFLAGS)

calc_q2: calc_q2.f90
	$(F90) calc_q2.f90 -o $(BINDIR)/calc_q2 $(FFLAGS)

dft_q2_comp: dft_q2_comp.f90
	$(F90) dft_q2_comp.f90 -o $(BINDIR)/dft_q2_comp $(FFLAGS)

change_unit_precip: change_unit_precip.f90
	$(F90) change_unit_precip.f90 -o $(BINDIR)/change_unit_precip $(FFLAGS)

change_unit_precip_jmm: change_unit_precip_jmm.f90
	$(F90) change_unit_precip_jmm.f90 -o $(BINDIR)/change_unit_precip_jmm $(FFLAGS)

mk_lsm_var: mk_lsm_var.f90
	$(F90) mk_lsm_var.f90 -o $(BINDIR)/mk_lsm_var $(FFLAGS)

upside_down: upside_down.f90
	$(F90) upside_down.f90 -o $(BINDIR)/upside_down $(FFLAGS)

upside_down_var: upside_down_var.f90
	$(F90) upside_down_var.f90 -o $(BINDIR)/upside_down_var $(FFLAGS)

shift180: shift180.f90
	$(F90) shift180.f90 -o $(BINDIR)/shift180 $(FFLAGS)

calc_q2_6H: calc_q2_6H.f90
	$(F90) calc_q2_6H.f90 -o $(BINDIR)/calc_q2_6H $(FFLAGS)

change_unit_precip_raw: change_unit_precip_raw.f90
	$(F90) change_unit_precip_raw.f90 -o $(BINDIR)/change_unit_precip_raw $(FFLAGS)


mk_lsm_from_rawfile: mk_lsm_from_rawfile.f90
	$(F90) mk_lsm_from_rawfile.f90 -o $(BINDIR)/mk_lsm_from_rawfile $(FFLAGS)

add_feb_29: add_feb_29.f90
	$(F90) add_feb_29.f90 -o $(BINDIR)/add_feb_29 $(FFLAGS)

dft_timeseries: dft_timeseries.f90
	$(F90) dft_timeseries.f90 -o $(BINDIR)/dft_timeseries $(FFLAGS)

test_adjustment: test_adjustment.f90
	$(F90) test_adjustment.f90 -o $(BINDIR)/test_adjustment

#---------------------------------------------------------

clean:
	\rm -f *~
	cd $(BINDIR) ; \rm -f $(EXEC) *~

#--------------------------------------------------------------
