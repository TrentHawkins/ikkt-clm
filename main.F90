!     ifort v2017.2

!     ifort -module "ifort/module" -c -w main.F90 -mkl=sequential -lmkl_lapack95 -lmkl_intel_ilp64 -lmkl_core -lpthread -lm -ldl

#     include "getopts.F90"
#     include "precision.F90"

#     include "tensor/tensor.F90"

#     include "monte_carlo/random_number_generator.F90"
#     include "monte_carlo/average.F90"
#     include "monte_carlo/time.F90"
#     include "monte_carlo/monte_carlo.F90"

#     include "tools/brent_minimization.F90"
#     include "tools/conjugate_gradient.F90"

#     include "ikkt/interface.F90"
#     include "ikkt/constants.F90"
#     include "ikkt/fields.F90"
#     include "ikkt/complex_langevin.F90"
#     include "ikkt/observables.F90"
