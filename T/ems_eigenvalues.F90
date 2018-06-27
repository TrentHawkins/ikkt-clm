!     IKKT-CLM/T/ifort -module "../ifort/modules" -assume source_include -I include -fast -w energy-momentum-stress-analysis.F90
!                      -o ems.out -mkl=sequential -lmkl_blas95_lp64 -lmkl_lapack95_lp64 -lmkl_core -lpthread -lm -ldl

#     include "../system/getopts.F90"
#     include "../system/precision.F90"

#     include "../tools/insert_sort.F90"
#     include "../tools/constants.F90"
#     include "../tools/tensor/tensor.F90"
!     include "../tools/tensor/tensor.operations.F90"
!     include "../tools/tensor/tensor.procedures.F90"

#     include "../analysis/autocorrelation_time/autocorrelation_time.F90"
#     include "../analysis/jack_knife/jack_knife.F90"


      program energy_momentum_stress_analysis


            use::get_options

            use::insertion_sort
            use::mathematical_constants
            use::tensor_type

            use::autocorrelation_time
            use::jack_knife


            implicit none


            character(*),parameter::           format_energy_momentum_stress_K=GENERICGK
            character(*),parameter::text_field_format_energy_momentum_stress_K=GENERICAK
            integer     ,parameter::size_field_format_energy_momentum_stress_K=GENERICNK

            real(KK),parameter::tolerance_energy_momentum_stress_K=TOLERANCEK

            character(:),allocatable::record_name
            character(:),allocatable::output_name

            integer::inner_degrees_of_freedom
            integer::boson_degrees_of_freedom
            integer::fermi_degrees_of_freedom

            complex(KK),dimension( :                          ,&
                                   :                          ),allocatable::energy_momentum_stress
            complex(KK),dimension( :                          ),allocatable::energy_momentum_stress_eigenvalues
            complex(KK)                                                    ::energy_momentum_stress_trace,&
                                                                             energy_momentum_stress_symmetric_norm

            real(KK)::time
            real(KK)::step


            call read_options_and_arguments()
            call process_energy_momentum_stress(record_name,&
                                                output_name)


      contains


            subroutine process_energy_momentum_stress(ifilename,&
                                                      ofilename)


                  use::mathematical_constants
                  use::insertion_sort
                  use::tensor_procedures

                  use::lapack95,only:geev


                  implicit none


                  character(*),intent(in   )::ifilename
                  character(*),intent(in   )::ofilename

                  integer::istream
                  integer::ostream

                  integer::index

                  intrinsic:: abs
                  intrinsic::real


                  call read_ems_parameters()

                  if(allocated(energy_momentum_stress))&
                   deallocate (energy_momentum_stress)
                     allocate (energy_momentum_stress(0:boson_degrees_of_freedom-1,&
                                                      0:boson_degrees_of_freedom-1))

                  if(allocated(energy_momentum_stress_eigenvalues))&
                   deallocate (energy_momentum_stress_eigenvalues)
                     allocate (energy_momentum_stress_eigenvalues(0:boson_degrees_of_freedom-1))

                   open(newunit=istream,file=ifilename)
                   open(newunit=ostream,file=ofilename)

!                  read(istream,   *             )
!                  read(istream,   *             )

!                 write(ostream,1000,advance="no") "#    "

!                 write(ostream,text_field_format_energy_momentum_stress_K,advance="no") eigen_header("life-time")
!                 write(ostream,text_field_format_energy_momentum_stress_K,advance="no") eigen_header("time-step")

!                 do index=0,boson_degrees_of_freedom-1,+1

!                    write(ostream,text_field_format_energy_momentum_stress_K,advance="no") eigen_header(" re.eignevalue.",index)
!                    write(ostream,text_field_format_energy_momentum_stress_K,advance="no") eigen_header(" im.eignevalue.",index)

!             end do!index=0,boson_degrees_of_freedom-1,+1

!                 do index=0,boson_degrees_of_freedom-1,+1

!                    write(ostream,text_field_format_energy_momentum_stress_K,advance="no") eigen_header("mod.eignevalue.",index)
!                    write(ostream,text_field_format_energy_momentum_stress_K,advance="no") eigen_header("arg.eignevalue.",index)

!             end do!index=0,boson_degrees_of_freedom-1,+1

!                 write(ostream,text_field_format_energy_momentum_stress_K,advance="no") eigen_header(         "trace")
!                 write(ostream,text_field_format_energy_momentum_stress_K,advance="no") eigen_header("symmetric_norm")
!                 write(ostream,                                         *             )

!                 write(ostream,1000,advance="no") "#    "

!                 do index=1,4*(boson_degrees_of_freedom+1)*size_field_format_energy_momentum_stress_K,+1

!                    write(ostream,1000,advance="no") "─"

!             end do!index=1,4*(boson_degrees_of_freedom+1)*size_field_format_energy_momentum_stress_K,+1

!                 write(ostream,                                         *             )

                  do index=1,100

                      read(istream,   *,end=100) time,&
                                                 step,energy_momentum_stress,&
                                                      energy_momentum_stress_trace

                     energy_momentum_stress_symmetric_norm=norm(energy_momentum_stress&
                                                     -transpose(energy_momentum_stress))

                     call geev(energy_momentum_stress,&
                               energy_momentum_stress_eigenvalues)

                     write(ostream,1000,advance="no") "NONE"

                     write(ostream,format_energy_momentum_stress_K) time,&
                                                                    step,energy_momentum_stress_eigenvalues   ,&
                                                                   polar(energy_momentum_stress_eigenvalues)  ,&
                                                                         energy_momentum_stress_trace         ,&
                                                                         energy_momentum_stress_symmetric_norm

                     call sort(energy_momentum_stress_eigenvalues,boson_degrees_of_freedom-1,intrinsic_abs )

                     write(ostream,1000,advance="no") "NORM"

                     write(ostream,format_energy_momentum_stress_K) time,&
                                                                    step,energy_momentum_stress_eigenvalues   ,&
                                                                   polar(energy_momentum_stress_eigenvalues)  ,&
                                                                         energy_momentum_stress_trace         ,&
                                                                         energy_momentum_stress_symmetric_norm

                     call sort(energy_momentum_stress_eigenvalues,boson_degrees_of_freedom-1,intrinsic_real)

                     write(ostream,1000,advance="no") "REAL"

                     write(ostream,format_energy_momentum_stress_K) time,&
                                                                    step,energy_momentum_stress_eigenvalues   ,&
                                                                   polar(energy_momentum_stress_eigenvalues)  ,&
                                                                         energy_momentum_stress_trace         ,&
                                                                         energy_momentum_stress_symmetric_norm

               end do

              100 continue

                  close(        ostream              )
                  close(        istream              )

                  if(allocated(energy_momentum_stress_eigenvalues))&
                   deallocate (energy_momentum_stress_eigenvalues)

                  if(allocated(energy_momentum_stress))&
                   deallocate (energy_momentum_stress)


             1000 format(a)


        end subroutine process_energy_momentum_stress!ifilename,&
!                                                     ofilename


            subroutine read_ems_parameters()


                  implicit none


                   read(*,*) inner_degrees_of_freedom,&
                             boson_degrees_of_freedom
                             fermi_degrees_of_freedom=2 &
                          **(boson_degrees_of_freedom/2 &
                                                     -1)


        end subroutine read_ems_parameters!


            function eigen_header(header,index)


                  implicit none


                  character(*),intent(in   )         ::header
                  integer     ,intent(in   ),optional::index

                  character(len(header))::eigen_header


                  if(present(index)) then

                     write(eigen_header,1002) header,".",index

                  else

                     write(eigen_header,1003) header

              end if!present(index)


             1002 format(a<size_field_format_energy_momentum_stress_K-6>,a1,i1,4x)
             1003 format(a<size_field_format_energy_momentum_stress_K-4>,      4x)


        end function eigen_header!header,index


            subroutine read_options_and_arguments()


                  use,intrinsic::iso_fortran_env,only:stdin => input_unit,&
                                                      stdout=>output_unit,&
                                                      stderr=> error_unit

                  use::get_options


                  implicit none


                  character             ::option_character ! currently parsed option
                  character(len_argname)::option_argument  ! currently parsed argument value if not an option
                  integer               ::argument_length  ! currently parsed argument true length
                  integer               ::status           ! stat of parsing option
                  integer               ::argument_index=0 ! current argument running index starting where the options end
                  integer               ::argument_number  ! number of remaining indices options aside

!                 type(option)::options() ! list of long options

!                 integer::index,time_stamp(8)


                  call get_command_argument(argument_index+1,option_argument)

                  allocate(character(len(trim(adjustl(option_argument))))::record_name)
                             record_name=trim(adjustl(option_argument))

                  call get_command_argument(argument_index+2,option_argument)

                  allocate(character(len(trim(adjustl(option_argument))))::output_name)
                             output_name=trim(adjustl(option_argument))


        end subroutine read_options_and_arguments!


  end program energy_momentum_stress_analysis
