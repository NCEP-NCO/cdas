all:
	cd gribw_lib.cd ; make -f gribwlib.make
	cd cdas_cycle.fd ; make
	cd cdas_ex_grib2.cd ; make
	cd cdas_ex_level.cd ; make
	cd cdas_fix_snowc.cd ; make
	cd cdas_igb.fd ; make
	cd cdas_mrf.fd ; make
	cd cdas_pgb.fd ; make
	cd cdas_prepdate.fd ; make
	cd cdas_sgb.fd ; make
	cd cdas_sig2sig.fd ; make
	cd cdas_sigmatime.fd ; make
	cd cdas_ssi.fd ; make
	cd cdas_fix_afwa_snow.cd ; make
	cd cdas_snowd2snowc.cd ; make
	cp cdas*/cdas* ../exec

clean:
	cd gribw_lib.cd ; make -f gribwlib.make clean
	cd cdas_cycle.fd ; make clean
	cd cdas_ex_grib2.cd ; make clean
	cd cdas_ex_level.cd ; make clean
	cd cdas_fix_snowc.cd ; make clean
	cd cdas_igb.fd ; make clean
	cd cdas_mrf.fd ; make clean
	cd cdas_pgb.fd ; make clean
	cd cdas_prepdate.fd ; make clean
	cd cdas_sgb.fd ; make clean
	cd cdas_sig2sig.fd ; make clean
	cd cdas_sigmatime.fd ; make clean
	cd cdas_ssi.fd ; make clean
	cd cdas_fix_afwa_snow.cd ; make clean
	cd cdas_snowd2snowc.cd ; make clean
	touch ../exec/junk ; rm ../exec/*
