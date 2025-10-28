#!./bin/bash
. clean_local.sh
gfortran $GFORTRAN_DEBUG_FLAGS var_rec_t_prototype6.F90 ${CONDA_PREFIX}/lib/libblas.so ${CONDA_PREFIX}/lib/liblapack.so