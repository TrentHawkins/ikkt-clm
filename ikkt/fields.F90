#     ifndef IKKT_FIELDS_F90
#     define IKKT_FIELDS_F90

#     include "precision.F90"
#     include "interface.F90"

#     include "tensor/tensor.F90"

#     include "ikkt/constants.F90"


      module fields


            use lapack95,only:geev

            use interface

            use tensor_type

            use constants


            implicit none


            integer,public,parameter::n=inner_degrees_of_freedom
            integer,public,parameter::d=boson_degrees_of_freedom
            integer,public,parameter::p=fermi_degrees_of_freedom

            integer,public,parameter::n_size= inner_degrees_of_freedom&
                                            * inner_degrees_of_freedom
            integer,public,parameter::a_size= boson_degrees_of_freedom* n_size
            integer,public,parameter::f_size= fermi_degrees_of_freedom*(n_size-1)

            complex(KK),dimension(1:inner_degrees_of_freedom,&
                                  1:inner_degrees_of_freedom,&
                                  1:boson_degrees_of_freedom),public::a

            complex(KK),dimension(1:f_size),public::m_eigenvalues
            complex(KK),dimension(1:f_size,&
                                  1:f_size),public::m,cm,cmm

            complex(KK),dimension(1:f_size,&
                                  1:f_size,1:inner_degrees_of_freedom,&
                                           1:inner_degrees_of_freedom,&
                                           1:boson_degrees_of_freedom),public::ma


            public::make_fields
            public::load_fields
            public::save_fields

            private::make_m
            private::make_cm
            private::make_cmm

            private::make_ma

            private::make_m_eigenvalues

            public::update_fermion_matrix


      contains


            function fermion_noise()


                  implicit none


                  complex(KK),dimension(1:f_size)::fermion_noise


                  call random_number(fermion_noise(:))


        end function fermion_noise


            function boson_noise()


                  implicit none


                  complex(KK),dimension(1:inner_degrees_of_freedom,&
                                        1:inner_degrees_of_freedom,&
                                        1:boson_degrees_of_freedom)::boson_noise

                  integer::mu


                  do mu=1,boson_degrees_of_freedom,+1

                     call random_number(boson_noise(:,:,mu))

              end do!mu=1,boson_degrees_of_freedom,+1


        end function boson_noise


            subroutine make_fields(is_hot)


                  implicit none


                  logical,intent(in   )::is_hot


                  if(is_hot) then

                     a=boson_noise()

                  else

                     a= .00000e+0

              end if!is_hot


        end subroutine make_fields!is_hot


            subroutine load_fields(conf_file_name)


                  implicit none


                  character(*),intent(in   )::conf_file_name

                  integer::unit
                  integer::mu


                   open(newunit=unit,file=conf_file_name)

                  do mu=1,boson_degrees_of_freedom,+1

                     call  read(unit,a(:,:,mu))

              end do!mu=1,boson_degrees_of_freedom,+1

                   read(*)

                  close(unit)


        end subroutine load_fields!conf_file_name


            subroutine save_fields(conf_file_name)


                  implicit none


                  character(*),intent(in   )::conf_file_name

                  integer::unit
                  integer::mu


                   open(newunit=unit,file=conf_file_name)

                  do mu=1,boson_degrees_of_freedom,+1

                     call write(unit,a(:,:,mu))

              end do!mu=1,boson_degrees_of_freedom,+1

                  write(*)

                  close(unit)


        end subroutine save_fields!conf_file_name


            subroutine make_m()


                  implicit none


                  integer::mu,j2,i2,j1,i1

                  complex(KK),dimension(1:n_size,&
                                        1:n_size)::a_delta0,&
                                                   a_delta1,&
                                                   a_delta2

                  m= .00000e+0

                  do mu=1,boson_degrees_of_freedom,+1

                     do j2=1,inner_degrees_of_freedom,+1

                        do i2=1,inner_degrees_of_freedom,+1

                           do j1=1,inner_degrees_of_freedom,+1

                              do i1=1,inner_degrees_of_freedom,+1

                                 a_delta0(n*i1+j1,n*i2+j2)= a(j1,i2,mu)*delta(j2,i1) &
                                                          - a(j2,i1,mu)*delta(j1,i2)
                                 a_delta1(n*i1+j1,n*i2+j2)=(a(n ,i2,mu)*delta(j2,n ) &
                                                          - a(j2,n ,mu)*delta(n ,i2))*delta(i1,j1)
                                 a_delta2(n*i1+j1,n*i2+j2)=(a(j1,n ,mu)*delta(n ,i1) &
                                                          - a(n ,i1,mu)*delta(j1,n ))*delta(i2,j2)

              end             do!i1=1,inner_degrees_of_freedom,+1

              end          do!j1=1,inner_degrees_of_freedom,+1

              end       do!i2=1,inner_degrees_of_freedom,+1

              end    do!j2=1,inner_degrees_of_freedom,+1

                     m(:,:)=m(:,:)+gamma(:,:,mu).x.(a_delta0(:,:) &
                                  -                 a_delta1(:,:) &
                                  -                 a_delta2(:,:))

              end do!mu=1,boson_degrees_of_freedom,+1


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

                  complex(KK),dimension(1:n_size,&
                                        1:n_size)::delta_30,&
                                                   delta_31,&
                                                   delta_32


                  do mu=1,boson_degrees_of_freedom,+1

                     do j =1,inner_degrees_of_freedom,+1

                        do i =1,inner_degrees_of_freedom,+1

                           do j2=1,inner_degrees_of_freedom,+1

                              do i2=1,inner_degrees_of_freedom,+1

                                 do j1=1,inner_degrees_of_freedom,+1

                                    do i1=1,inner_degrees_of_freedom,+1

                                       delta_30(n*i1+j1,n*i2+j2)= delta(j1,j ) &
                                                                * delta(i ,i2) &
                                                                * delta(j2,i1) &
                                                                - delta(j2,j ) &
                                                                * delta(i ,i1) &
                                                                * delta(j1,i2)
                                       delta_31(n*i1+j1,n*i2+j2)=(delta(n ,j ) &
                                                                * delta(i ,i2) &
                                                                * delta(j2,n ) &
                                                                - delta(j2,j ) &
                                                                * delta(i ,n ) &
                                                                * delta(n ,i2))*delta(i1,j1)
                                       delta_32(n*i1+j1,n*i2+j2)=(delta(j1,j ) &
                                                                * delta(i ,n ) &
                                                                * delta(n ,i1) &
                                                                - delta(n ,j ) &
                                                                * delta(i ,i1) &
                                                                * delta(j1,n ))*delta(i2,j2)

              end                   do!i1=1,inner_degrees_of_freedom,+1

              end                do!j1=1,inner_degrees_of_freedom,+1

              end             do!i2=1,inner_degrees_of_freedom,+1

              end          do!j2=1,inner_degrees_of_freedom,+1

                           ma(:,:,i,j,mu)=gamma(:,:,mu).x.(delta_30(:,:) &
                                                        -  delta_31(:,:) &
                                                        -  delta_32(:,:))

              end       do!i =1,inner_degrees_of_freedom,+1

              end    do!j =1,inner_degrees_of_freedom,+1

              end do!mu=1,boson_degrees_of_freedom,+1


        end subroutine make_ma


            subroutine make_m_eigenvalues


                  implicit none


                  call geev(m,m_eigenvalues)


        end subroutine make_m_eigenvalues


            subroutine update_fermion_matrix()


                  implicit none


                  call make_m()
                  call make_cm()
                  call make_cmm()

                  call make_ma()

                  call make_m_eigenvalues()


        end subroutine update_fermion_matrix


  end module fields


#  endif
