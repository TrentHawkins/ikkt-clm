!     IKKT-CLM/T/ifort -module "../ifort/modules" -assume source_include -I include -fast -w energy-momentum-stress-analysis.F90
!                      -mkl=sequential -lmkl_blas95_lp64 -lmkl_lapack95_lp64 -lmkl_core -lpthread -lm -ldl

#     include "../system/getopts.F90"
#     include "../system/precision.F90"

#     include "../tools/insert_sort.F90"
#     include "../tools/constants.F90"
#     include "../tools/tensor/tensor.F90"
!     include "../tools/tensor/tensor.operations.F90"
!     include "../tools/tensor/tensor.procedures.F90"

#     include "../simulation/fields.F90"

#     include "../analysis/autocorrelation_time/autocorrelation_time.F90"
#     include "../analysis/jack_knife/jack_knife.F90"


      program energy_momentum_stress_analysis


            use::insertion_sort
            use::mathematical_constants
            use::tensor_type

            use::fields

            use::autocorrelation_time
            use::jack_knife


            implicit none


            character(*),parameter::           format_energy_momentum_stress_K=GENERICGK
            character(*),parameter::text_field_format_energy_momentum_stress_K=GENERICAK

            real(KK),parameter::tolerance_energy_momentum_stress_K=TOLERANCEK

            character(:),allocatable::record_name
            character(:),allocatable::output_name

            complex(KK),dimension( :                          ,&
                                   :                          ),allocatable::energy_momentum_stress
            complex(KK),dimension( :                          ),allocatable::energy_momentum_stress_eigenvalues
            complex(KK)                                                    ::energy_momentum_stress_trace,&
                                                                             energy_momentum_stress_symmetric_norm

            real(KK)::time
            real(KK)::step


            call process_energy_momentum_stress(record_name,&
                                                output_name)


      contains


            subroutine process_energy_momentum_stress(ifilename,&
                                                      ofilename)


                  use::mathematical_constants
                  use::insertion_sort
                  use::tensor_type

                  use::fields

                  use::lapack95,only:geev


                  implicit none


                  character(*),intent(in   )::ifilename
                  character(*),intent(in   )::ofilename

                  integer::istream
                  integer::ostream

                  intrinsic:: abs
                  intrinsic::real


                  if(allocated(energy_momentum_stress)) &
                   deallocate (energy_momentum_stress)
                     allocate (energy_momentum_stress(0:boson_degrees_of_freedom-1,&
                                                      0:boson_degrees_of_freedom-1))

                  if(allocated(energy_momentum_stress_eigenvalues)) &
                   deallocate (energy_momentum_stress_eigenvalues)
                     allocate (energy_momentum_stress_eigenvalues(0:boson_degrees_of_freedom-1))

                   open(newunit=istream,file=ifilename)
                   open(newunit=ostream,file=ofilename)

                  do

                      read(istream,1000,end=100) time,&
                                                 step,energy_momentum_stress,&
                                                      energy_momentum_stress_trace

                     energy_momentum_stress_symmetric_norm=norm(energy_momentum_stress&
                                                     -transpose(energy_momentum_stress))

                     call geev(energy_momentum_stress,&
                               energy_momentum_stress_eigenvalues)

                     write(ostream,1001,advance="no") "trace and symmetric norm "

                     write(ostream,format_energy_momentum_stress_K) time,&
                                                                    step,energy_momentum_stress_trace,&
                                                                         energy_momentum_stress_symmetric_norm

                     write(ostream,1001,advance="no") "eigenvalues unproccessed "

                     write(ostream,format_energy_momentum_stress_K) time,&
                                                                    step,energy_momentum_stress_eigenvalues,&
                                                                   polar(energy_momentum_stress_eigenvalues)

                     call sort(energy_momentum_stress_eigenvalues,boson_degrees_of_freedom,intrinsic_abs )

                     write(ostream,1001,advance="no") "eigenvalues sorted(abs ) "

                     write(ostream,format_energy_momentum_stress_K) time,&
                                                                    step,energy_momentum_stress_eigenvalues,&
                                                                   polar(energy_momentum_stress_eigenvalues)

                     call sort(energy_momentum_stress_eigenvalues,boson_degrees_of_freedom,intrinsic_real)

                     write(ostream,1001,advance="no") "eignevalues sorted(real) "

                     write(ostream,format_energy_momentum_stress_K) time,&
                                                                    step,energy_momentum_stress_eigenvalues,&
                                                                   polar(energy_momentum_stress_eigenvalues)

               end do

              100 continue

                  close(        ostream              )
                  close(        istream              )


             1000 format(a,*(g28.17))
             1001 format(a)


        end subroutine process_energy_momentum_stress!ifilename,&
!                                                     ofilename


            subroutine read_options_and_arguments()


                  use,intrinsic::iso_fortran_env,only:stdin => input_unit,&
                                                      stdout=>output_unit,&
                                                      stderr=> error_unit


                  implicit none


                  character(CHAR)::optarg  ! currently parsed argument value if not an option


                  call get_command_argument(1,optarg)

                  allocate(character(len(trim(adjustl(optarg))))::record_name)
                             record_name=trim(adjustl(optarg))

                  call get_command_argument(2,optarg)

                  allocate(character(len(trim(adjustl(optarg))))::output_name)
                             output_name=trim(adjustl(optarg))


        end subroutine read_options_and_arguments!


  end program energy_momentum_stress_analysis
