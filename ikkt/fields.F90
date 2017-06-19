#     ifndef IKKT_FIELDS_F90
#     define IKKT_FIELDS_F90

#     include "main/precision.F90"

#     include "tensor/tensor.F90"

#     include "ikkt/constants.F90"


      module fields


            use::lapack95,only:ggev

            use::tensor_type

            use::constants


            implicit none


            logical,public::configuration_loaded=.false.
            logical,public::start_field_is_hot

            character(:),allocatable,public::conf_file_name

            integer,parameter,public::inner_degrees_of_freedom=4,&
                                      boson_degrees_of_freedom=6,&
                                      fermi_degrees_of_freedom=2 &
                                   **(boson_degrees_of_freedom/2 &
                                                              -1)

            integer,parameter,public::n=inner_degrees_of_freedom,&
                                      d=boson_degrees_of_freedom,&
                                      p=fermi_degrees_of_freedom

            integer,parameter,public::n_size=inner_degrees_of_freedom &
                                            *inner_degrees_of_freedom,&
                                      a_size=boson_degrees_of_freedom &
                                            *inner_degrees_of_freedom &
                                            *inner_degrees_of_freedom,&
                                      f_size=fermi_degrees_of_freedom &
                                            *inner_degrees_of_freedom &
                                            *inner_degrees_of_freedom

            real(KK),parameter,public::stddev=sqrt(.20000e+1_KK)

            complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                  0:inner_degrees_of_freedom-1,&
                                  0:boson_degrees_of_freedom-1),public::a,boson_noise

            complex(KK),dimension(0:f_size-1),public::m_eigenvalues0,&
                                                      m_eigenvalues1,fermi_noise
            complex(KK),dimension(0:f_size-1,&
                                  0:f_size-1),public::m,&
                                                     cm,&
                                                    cmm,&
                                                      m_kernel
            complex(KK),dimension(0:f_size-1,&
                                  0:f_size-1,0:inner_degrees_of_freedom-1,&
                                             0:inner_degrees_of_freedom-1,&
                                             0:boson_degrees_of_freedom-1),public::ma


            public::make_fields
            public::load_fields
            public::save_fields

            private::make_m
            private::make_cm
            private::make_cmm

            private::make_ma

            private::make_m_eigenvalues

            public::make_m_kernel
            public::update_fermion_matrix


      contains


            subroutine tracelessify()


                  implicit none


                  integer::mu


                  do mu=1,inner_degrees_of_freedom,+1

                     call make_traceless(a(:,:,mu))

              end do!mu= ,inner_degrees_of_freedom,+1


        end subroutine tracelessify!


            subroutine make_boson_noise(stddev)


                  implicit none


                  real(KK)::stddev

                  integer::mu


                  do mu=0,boson_degrees_of_freedom-1,+1

                     call                 random_number(boson_noise(:,:,mu))
                                                        boson_noise(:,:,mu)   &
                    =stddev*sqrt(-2*           log(real(boson_noise(:,:,mu))))&
                           * exp( 2*pi*im_unit*   aimag(boson_noise(:,:,mu)))
                     call                make_traceless(boson_noise(:,:,mu))

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end subroutine make_boson_noise!stddev


            subroutine make_fermi_noise(stddev)


                  implicit none


                  real(KK)::stddev


                  call                 random_number(fermi_noise)
                                                     fermi_noise   &
                 =stddev*sqrt(-2*           log(real(fermi_noise)))&
                        * exp( 2*pi*im_unit*   aimag(fermi_noise))
                                                     fermi_noise&
                                        =m_kernel.o.fermi_noise



        end subroutine make_fermi_noise!stddev


            subroutine make_fields(start_field_is_hot)


                  implicit none


                  logical,intent(in   )::start_field_is_hot


                  if(start_field_is_hot) then

                     call make_boson_noise(stddev)

                     a=boson_noise

                  else

                     a=zero

              end if!start_field_is_hot


        end subroutine make_fields!start_field_is_hot


            subroutine load_fields()


                  implicit none


                  integer::unit
                  integer::mu


                   open(newunit=unit,file=conf_file_name)

                  do mu=0,boson_degrees_of_freedom-1,+1

                     call  read(unit,a(:,:,mu))

              end do!mu=0,boson_degrees_of_freedom-1,+1

                   read(unit,*)

                  close(unit)


        end subroutine load_fields


            subroutine save_fields()


                  implicit none


                  integer::unit
                  integer::mu


                   open(newunit=unit,file=conf_file_name)

                  do mu=0,boson_degrees_of_freedom-1,+1

                     call write(unit,a(:,:,mu))

              end do!mu=0,boson_degrees_of_freedom-1,+1

                  write(unit,*)

                  close(unit)


        end subroutine save_fields


            subroutine make_m()


                  implicit none


                  integer::mu,j2,i2,j1,i1

                  complex(KK),dimension(0:n_size-1,&
                                        0:n_size-1)::a_delta

                  m=zero

                  do mu=0,boson_degrees_of_freedom-1,+1

                     do i2=0,inner_degrees_of_freedom-1,+1

                        do j2=0,inner_degrees_of_freedom-1,+1

                           do i1=0,inner_degrees_of_freedom-1,+1

                              do j1=0,inner_degrees_of_freedom-1,+1

                                 a_delta(n*i1+j1,n*i2+j2)=a(j1,i2,mu)*delta(j2,i1)&
                                                         -a(j2,i1,mu)*delta(j1,i2)

              end             do!j1=0,inner_degrees_of_freedom-1,+1

              end          do!i1=0,inner_degrees_of_freedom-1,+1

              end       do!j2=0,inner_degrees_of_freedom-1,+1

              end    do!i2=0,inner_degrees_of_freedom-1,+1

                     m(:,:)=m(:,:)+(gamma(:,:,mu).x.a_delta(:,:))

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end subroutine make_m


            subroutine make_cm()


                  implicit none


                  cm=conjugate(m)


        end subroutine make_cm


            subroutine make_cmm()


                  implicit none


                  cmm=real(cm.o.m)


        end subroutine make_cmm


            subroutine make_ma()


                  implicit none


                  integer::i ,j ,mu,j2,i2,a2,j1,i1,a1

                  complex(KK),dimension(0:n_size-1,&
                                        0:n_size-1)::delta_3


                  do j=0,inner_degrees_of_freedom-1,+1

                     do i=0,inner_degrees_of_freedom-1,+1

                           do i2=0,inner_degrees_of_freedom-1,+1

                              do j2=0,inner_degrees_of_freedom-1,+1

                                 do i1=0,inner_degrees_of_freedom-1,+1

                                    do j1=0,inner_degrees_of_freedom-1,+1

                                       delta_3(n*i1+j1,n*i2+j2)=delta(j1,j)*delta(i,i2)*delta(j2,i1)&
                                                               -delta(j2,j)*delta(i,i1)*delta(j1,i2)

              end                   do!j1=0,inner_degrees_of_freedom-1,+1

              end                do!i1=0,inner_degrees_of_freedom-1,+1

              end             do!j2=0,inner_degrees_of_freedom-1,+1

              end          do!i2=0,inner_degrees_of_freedom-1,+1

                        do mu=0,boson_degrees_of_freedom-1,+1

                           ma(:,:,i,j,mu)=gamma(:,:,mu).x.delta_3(:,:)

              end       do!mu=0,boson_degrees_of_freedom-1,+1

              end    do!i=0,inner_degrees_of_freedom-1,+1

              end do!j=0,inner_degrees_of_freedom-1,+1


        end subroutine make_ma


            subroutine make_m_eigenvalues


                  implicit none


                  call ggev(m,m_kernel,m_eigenvalues0,m_eigenvalues1)


        end subroutine make_m_eigenvalues


            subroutine make_m_kernel()


                  implicit none


                  m_kernel=matrix(fermi_degrees_of_freedom,re_unit).x.delta_super_traceless


        end subroutine make_m_kernel


            subroutine update_fermion_matrix()


                  implicit none


                  call make_m()
                  call make_cm()
                  call make_cmm()

                  call make_ma()

                  call make_m_eigenvalues()


        end subroutine update_fermion_matrix


            subroutine print_fermion_matrix()


                  implicit none


                  integer::unit


                   open(newunit=unit,file="fermion.matrix")

                  call write(   unit,m)
            !     call write(   unit,cm)
            !     call write(   unit,cmm)

                  close(        unit                      )


        end subroutine print_fermion_matrix!unit


  end module fields


#  endif
