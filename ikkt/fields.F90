#     ifndef IKKT_FIELDS_F90
#     define IKKT_FIELDS_F90

#     include "main/precision.F90"
#     include "main/mathematical_constants.F90"

#     include "tensor/tensor.F90"

#     include "ikkt/constants.F90"


      module fields


            use::lapack95,only:geev,&
                               ggev

            use::mathematical_constants

            use::tensor_type

            use::constants


            implicit none


            logical,public::configuration_loaded=.false.
            logical,public::start_field_is_noisy=.false.
            logical,public::massive_deformations=.false.

            character(:),allocatable,public::conf_file_name

            integer,parameter,public::inner_degrees_of_freedom=8,&
                                      boson_degrees_of_freedom=6,&
                                      fermi_degrees_of_freedom=2 &
                                   **(boson_degrees_of_freedom/2 &
                                                              -1)

            integer,parameter,public::n=inner_degrees_of_freedom,&
                                      d=boson_degrees_of_freedom,&
                                      p=fermi_degrees_of_freedom

            integer,parameter,public::n_size= inner_degrees_of_freedom    &
                                            * inner_degrees_of_freedom   ,&
                                      a_size= boson_degrees_of_freedom    &
                                            *(inner_degrees_of_freedom+1) &
                                            *(inner_degrees_of_freedom+1),&
                                      f_size= fermi_degrees_of_freedom    &
                                            * inner_degrees_of_freedom    &
                                            * inner_degrees_of_freedom

            real(KK),parameter,public::standard_deviation=sqrt(+.20000e+1_KK)
      !     real(KK),parameter,public::standard_deviation=     +.10000e+1_KK

            real(KK),                                        parameter,public::epsilon   = +.10000e+1_KK
            real(KK),dimension(0:boson_degrees_of_freedom-1),parameter,public::boson_mass=[+.50000e+0_KK,&
                                                                                           +.50000e+0_KK,&
                                                                                           +.10000e+1_KK,&
                                                                                           +.20000e+1_KK,&
                                                                                           +.40000e+1_KK,&
                                                                                           +.80000e+1_KK]

            real(KK),                                        parameter,public::fermi_mass= +.12500e+1_KK

            complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                  0:inner_degrees_of_freedom  ,&
                                  0:boson_degrees_of_freedom-1),public::a,boson_noise

#           ifndef OPTIMAL

            complex(KK),dimension(0:f_size-1),public::f,fermi_noise,m_eigenvalues_
            complex(KK),dimension(0:f_size-1,&
                                  0:f_size-1),public::m,cm,cmm
            complex(KK),dimension(0:f_size-1,&
                                  0:f_size-1,0:inner_degrees_of_freedom  ,&
                                             0:inner_degrees_of_freedom  ,&
                                             0:boson_degrees_of_freedom-1),public::ma

#           else

            complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                  0:inner_degrees_of_freedom  ,&
                                  0:fermi_degrees_of_freedom-1),public::f,fermi_noise!m_eigenvalues_
      !     complex(KK),dimension(0:inner_degrees_of_freedom  ,&
      !                           0:inner_degrees_of_freedom  ,&
      !                           0:fermi_degrees_of_freedom-1,&
      !                           0:inner_degrees_of_freedom  ,&
      !                           0:inner_degrees_of_freedom  ,&
      !                           0:fermi_degrees_of_freedom-1),public::m,cm,cmm,m_kernel
      !     complex(KK),dimension(0:inner_degrees_of_freedom  ,&
      !                           0:inner_degrees_of_freedom  ,&
      !                           0:fermi_degrees_of_freedom-1,&
      !                           0:inner_degrees_of_freedom  ,&
      !                           0:inner_degrees_of_freedom  ,&
      !                           0:fermi_degrees_of_freedom-1,&
      !                           0:inner_degrees_of_freedom  ,&
      !                           0:inner_degrees_of_freedom  ,&
      !                           0:boson_degrees_of_freedom-1),public::ma

#        endif

            public::make_boson_noise
            public::make_fermi_noise

            public::make_fields
            public::load_fields
            public::save_fields

#           ifndef OPTIMAL

            private::make_m
            private::make_cm
            private::make_cmm

            private::make_ma

            private::make_m_eigenvalues

            public::update_fermion_matrix

#        endif


      contains


            subroutine make_boson_noise(standard_deviation)


                  implicit none


                  real(KK)::standard_deviation

                  integer::mu


                  do mu=0,boson_degrees_of_freedom-1,+1

                     call random_number(boson_noise(:,:,mu))

                                                                 boson_noise(:,:,mu)   &
                    =standard_deviation*sqrt(-2        *log(real(boson_noise(:,:,mu))))&
                                       * exp( 2*pi*im_unit*aimag(boson_noise(:,:,mu)))

            !        call make_hermitian(boson_noise(:,:,mu),+.10000e+1_KK)
                     call make_traceless(boson_noise(:,:,mu))

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end subroutine make_boson_noise!standard_deviation


            subroutine make_fermi_noise(standard_deviation)


                  implicit none


                  real(KK)::standard_deviation

#                 ifndef OPTIMAL


                  call random_number(fermi_noise)

                                                              fermi_noise   &
                 =standard_deviation*sqrt(-2        *log(real(fermi_noise)))&
                                    * exp( 2*pi*im_unit*aimag(fermi_noise))

#                 else

                  integer::a_


                  do a_=0,fermi_degrees_of_freedom-1,+1

                     call random_number(fermi_noise(:,:,a_))

                                                                 fermi_noise(:,:,a_)   &
                    =standard_deviation*sqrt(-2        *log(real(fermi_noise(:,:,a_))))&
                                       * exp( 2*pi*im_unit*aimag(fermi_noise(:,:,a_)))

                     call make_traceless(fermi_noise(:,:,a_))

              end do!a_=0,fermi_degrees_of_freedom-1,+1

#              endif


        end subroutine make_fermi_noise!standard_deviation


            subroutine make_fields()


                  implicit none


                  if(start_field_is_noisy) then

                     call make_boson_noise(standard_deviation)

                     a=real(boson_noise)

                  else

                     a=zero

              end if!start_field_is_noisy


        end subroutine make_fields!


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


        end subroutine load_fields!


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


        end subroutine save_fields!


#           ifndef OPTIMAL


            subroutine make_m()


                  implicit none


                  integer::mu,j2,i2,j1,i1

                  complex(KK),dimension(0:n_size-2,&
                                        0:n_size-2)::a_delta

                  m=zero

                   open(unit=11,file="a.delta")

                  do mu=0,boson_degrees_of_freedom-1,+1

                     a_delta(:,:)=zero

                     do i2=0,inner_degrees_of_freedom-1,+1

                        do j2=0,inner_degrees_of_freedom-1,+1

                           do i1=0,inner_degrees_of_freedom-1,+1

                              do j1=0,inner_degrees_of_freedom-1,+1

                                 a_delta(n*i1+j1,n*i2+j2)= a(j1,i2,mu)*delta(j2,i1)              &
                                                         - a(j2,i1,mu)*delta(j1,i2)              &
                                                         -(a(n ,i2,mu)*delta(j2,n )              &
                                                         - a(j2,n ,mu)*delta(n ,i2))*delta(i1,j1)&
                                                         -(a(j1,n ,mu)*delta(n ,i1)              &
                                                         - a(n ,i1,mu)*delta(j1,n ))*delta(i2,j2)

              end             do!j1=0,inner_degrees_of_freedom-1,+1

              end          do!i1=0,inner_degrees_of_freedom-1,+1

              end       do!j2=0,inner_degrees_of_freedom-1,+1

              end    do!i2=0,inner_degrees_of_freedom-1,+1

                     m(:,:)&
                    =m(:,:)+(gamma(:,:,mu).x.a_delta(:,:))

              end do!mu=0,boson_degrees_of_freedom-1,+1

                  if(massive_deformations) then

                     select case(boson_degrees_of_freedom)

                     case(6)

                        do i2=0,inner_degrees_of_freedom-1,+1

                           do j2=0,inner_degrees_of_freedom-1,+1

                              do i1=0,inner_degrees_of_freedom-1,+1

                                 do j1=0,inner_degrees_of_freedom-1,+1

                                    a_delta(n*i1+j1,n*i2+j2)=delta(j1,i2)&
                                                            *delta(j2,i1)

              end                do!j1=0,inner_degrees_of_freedom-1,+1

              end             do!i1=0,inner_degrees_of_freedom-1,+1

              end          do!j2=0,inner_degrees_of_freedom-1,+1

              end       do!i2=0,inner_degrees_of_freedom-1,+1

                     m(:,:)&
                    =m(:,:)+(gamma(:,:,boson_degrees_of_freedom).x.a_delta(:,:))*fermi_mass

              end    select!case(boson_degrees_of_freedom)

              end if!massive_deformations


        end subroutine make_m!


            subroutine make_cm()


                  implicit none


                  cm=conjugate(m)


        end subroutine make_cm!


            subroutine make_cmm()


                  implicit none


                  cmm=hermitian(cm.o.m)


        end subroutine make_cmm!


            subroutine make_ma()


                  implicit none


                  integer::i ,j ,mu,j2,i2,a2,j1,i1,a1

                  complex(KK),dimension(0:n_size-1,&
                                        0:n_size-1)::delta_3


                  do j=0,inner_degrees_of_freedom  ,+1

                     do i=0,inner_degrees_of_freedom  ,+1

                           do i2=0,inner_degrees_of_freedom-1,+1

                              do j2=0,inner_degrees_of_freedom-1,+1

                                 do i1=0,inner_degrees_of_freedom-1,+1

                                    do j1=0,inner_degrees_of_freedom-1,+1

                                       delta_3(n*i1+j1,n*i2+j2)= delta(j1,j)*delta(i,i2)*delta(j2,i1)              &
                                                               - delta(j2,j)*delta(i,i1)*delta(j1,i2)              &
                                                               -(delta(n ,j)*delta(i,i2)*delta(j2,n )              &
                                                               - delta(j2,j)*delta(i,n )*delta(n ,i2))*delta(i1,j1)&
                                                               -(delta(j1,j)*delta(i,n )*delta(n ,i1)              &
                                                               - delta(n ,j)*delta(i,i1)*delta(j1,n ))*delta(i2,j2)

              end                   do!j1=0,inner_degrees_of_freedom-1,+1

              end                do!i1=0,inner_degrees_of_freedom-1,+1

              end             do!j2=0,inner_degrees_of_freedom-1,+1

              end          do!i2=0,inner_degrees_of_freedom-1,+1

                        do mu=0,boson_degrees_of_freedom-1,+1

                           ma(:,:,i,j,mu)=gamma(:,:,mu).x.delta_3(:,:)

              end       do!mu=0,boson_degrees_of_freedom-1,+1

              end    do!i=0,inner_degrees_of_freedom  ,+1

              end do!j=0,inner_degrees_of_freedom  ,+1


        end subroutine make_ma!


            subroutine make_m_eigenvalues


                  implicit none


                  call geev(m,m_eigenvalues_)


        end subroutine make_m_eigenvalues!


            subroutine update_fermion_matrix()


                  implicit none


                  call make_m()
                  call make_cm()
                  call make_cmm()

                  call make_ma()

                  call make_m_eigenvalues()

                  call print_fermion_matrix()


        end subroutine update_fermion_matrix!


            subroutine print_fermion_matrix()


                  implicit none


                  integer::unit


                   open(newunit=unit,file="fermion.matrix")

            !     call write(   unit,m)
            !     call write(   unit,cm)
            !     call write(   unit,cmm)

                  call write(unit,m_eigenvalues_)

                  close(        unit                      )


        end subroutine print_fermion_matrix!


#        endif


  end module fields


#  endif
