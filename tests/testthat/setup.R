# CRAN allows at most two cores during checks. dann's prediction loop is
# parallelized, so cap it for the whole test suite.
tidydann::tidydann_set_threads(2)
