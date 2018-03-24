#     ifndef FIELDS_F90
#     define FIELDS_F90

#     include "../system/precision.F90"
#     include "../system/text_format.F90"

#     include "../tensor/mathematical_constants.F90"
#     include "../tensor/tensor.F90"

#     include "../ikkt/constants.F90"


      module fields


            use::lapack95,only:geev,&
                               ggev

            use::text_formatting
            use::mathematical_constants

            use::tensor_type

            use::constants


            implicit none


            character(*),private,parameter::           format_stat_K=INTEGERGK
            character(*),private,parameter::text_field_format_stat_K=INTEGERAK

            character(*),private,parameter::           format_mass_K=REALGK
            character(*),private,parameter::text_field_format_mass_K=REALAK

            real(KK),private,parameter::two

            logical,public::configuration_loaded=.false.
            logical,public::start_field_is_noisy=.false.
            logical,public::fermions_included_in=.false.
            logical,public::massive_deformations=.false.

            character(:),allocatable,public::conf_file_name

            integer,public::inner_degrees_of_freedom,n,n_size
            integer,public::boson_degrees_of_freedom,d,a_size
            integer,public::fermi_degrees_of_freedom,p,f_size

            real(KK),                                                    public::boson_mass_factor
            real(KK),dimension( :                          ),allocatable,public::boson_mass
            real(KK),                                                    public::fermi_mass

            complex(KK),dimension( :                          ,&
                                   :                          ,&
                                   :                          ),allocatable,public::a

#           ifdef OPTIMAL

            complex(KK),dimension( :                          ,&
                                   :                          ,&
                                   :                          ),allocatable,public::f

#           else

            complex(KK),dimension( :        ),allocatable,public::f,m_eigenvalues_
            complex(KK),dimension( :        ,&
                                   :        ),allocatable,public::m,cm,cmm
            complex(KK),dimension( :        ,&
                                   :        , :                          ,&
                                              :                          ,&
                                              :                          ),allocatable,public::ma

            private::make_m
            private::make_cm
            private::make_cmm
            private::make_ma
            private::make_m_eigenvalues

            public::update_fermion_matrix

#        endif

            public::make_fields
            public::load_fields
            public::save_fields
            public::boson_noise
            public::fermi_noise

            private:: read_field
            private::write_field


!           interface  read

!              module procedure  read_field

!       end interface  read

!           interface write

!              module procedure write_field

!       end interface write


      contains


            subroutine make_fields()


                  implicit none


                  integer::mu


                  call read_field_parameters()

                  a=boson_noise(two)


        end subroutine make_fields!


            subroutine load_fields(file_name)


                  implicit none


                  character(*),intent(in   )::file_name

                  integer::unit
                  integer::iostat
                  integer::mu


                  call read_field_parameters()

                   open(newunit=unit,file=trim(file_name))

                  call  read_field(unit,iostat=iostat)

                  close(        unit                     )


        end subroutine load_fields!file_name


            subroutine save_fields(file_name)


                  implicit none


                  character(*),intent(in   )::file_name

                  integer::unit
                  integer::iostat
                  integer::mu


                   open(newunit=unit,file=trim(file_name))

                  call write_field(unit,iostat=iostat)

                  close(        unit                     )


        end subroutine save_fields!file_name


            function boson_noise(standard_deviation)


                  implicit none


                  real(KK),intent(in   )::standard_deviation

                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1)::boson_noise

                  integer::mu


                  do mu=0,boson_degrees_of_freedom-1,+1

                     call random_number(boson_noise(:,:,mu))

                                                                  boson_noise(:,:,mu)   &
                    =standard_deviation*sqrt(           -log(real(boson_noise(:,:,mu))))&
                                       * exp(two*pi*im_unit*aimag(boson_noise(:,:,mu)))

                     call make_hermitian(boson_noise(:,:,mu))
                     call make_traceless(boson_noise(:,:,mu))

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end function boson_noise!standard_deviation


            function fermi_noise(standard_deviation)


                  implicit none


                  real(KK),intent(in   )::standard_deviation

#                 ifndef OPTIMAL

                  complex(KK),dimension(0:f_size-1)::fermi_noise


                  call random_number(fermi_noise)

                                                               fermi_noise   &
                 =standard_deviation*sqrt(           -log(real(fermi_noise)))&
                                    * exp(two*pi*im_unit*aimag(fermi_noise))

#                 else

                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1)::fermi_noise

                  integer::a_


                  do a_=0,fermi_degrees_of_freedom-1,+1

                     call random_number(fermi_noise(:,:,a_))

                                                                  fermi_noise(:,:,a_)   &
                    =standard_deviation*sqrt(           -log(real(fermi_noise(:,:,a_))))&
                                       * exp(two*pi*im_unit*aimag(fermi_noise(:,:,a_)))

                     call make_traceless(fermi_noise(:,:,a_))

              end do!a_=0,fermi_degrees_of_freedom-1,+1

#              endif


        end function fermi_noise!standard_deviation


            subroutine  read_field(unit,iostat)


                  implicit none


                  integer,intent(inout)         ::unit
                  integer,intent(  out),optional::iostat


                  if(present(iostat)) then

                     do mu=0,boson_degrees_of_freedom-1,+1

                        call  read(unit,a(:,:,mu),iostat=iostat)

              end    do!mu=0,boson_degrees_of_freedom-1,+1

                      read(unit,*,iostat=iostat,end=114)

              114    return

                  else

                     do mu=0,boson_degrees_of_freedom-1,+1

                        call  read(unit,a(:,:,mu))

              end    do!mu=0,boson_degrees_of_freedom-1,+1

                      read(unit,*)

              end if!present(iostat)


        end subroutine  read_field!unit,iostat


            subroutine write_field(unit,iostat)


                  implicit none


                  integer,intent(inout)         ::unit
                  integer,intent(  out),optional::iostat


                  if(present(iostat)) then

                     do mu=0,boson_degrees_of_freedom-1,+1

                        call write(unit,a(:,:,mu),iostat=iostat)

              end    do!mu=0,boson_degrees_of_freedom-1,+1

                     write(unit,*,iostat=iostat,end=115)

              115    return

                  else

                     do mu=0,boson_degrees_of_freedom-1,+1

                        call write(unit,a(:,:,mu))

              end    do!mu=0,boson_degrees_of_freedom-1,+1

                     write(unit,*)

              end if!present(iostat)


        end subroutine write_field!unit,iostat


#           ifndef OPTIMAL


            subroutine make_m()


                  implicit none


                  integer::mu,j2,i2,j1,i1

                  complex(KK),dimension(0:n_size-2,&
                                        0:n_size-2)::a_delta

                  m=zero

                  do mu=0,boson_degrees_of_freedom-1,+1

                     a_delta(:,:)=zero

                     do i2=0,inner_degrees_of_freedom-1,+1

                        do j2=0,inner_degrees_of_freedom-1,+1

                           if(inner_degrees_of_freedom*i2+j2==n_size-1) exit

                           do i1=0,inner_degrees_of_freedom-1,+1

                              do j1=0,inner_degrees_of_freedom-1,+1

                                 if(inner_degrees_of_freedom*i1+j1==n_size-1) exit

                                 a_delta(inner_degrees_of_freedom*i1+j1,&
                                         inner_degrees_of_freedom*i2+j2)= a(j1,i2,mu)*delta(j2,i1)              &
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

                     do i2=0,inner_degrees_of_freedom-1,+1

                        do j2=0,inner_degrees_of_freedom-1,+1

                           if(inner_degrees_of_freedom*i2+j2==n_size-1) exit

                           do i1=0,inner_degrees_of_freedom-1,+1

                              do j1=0,inner_degrees_of_freedom-1,+1

                                 if(inner_degrees_of_freedom*i1+j1==n_size-1) exit

                                 a_delta(n*i1+j1,n*i2+j2)=delta(j1,i2)&
                                                         *delta(j2,i1)

              end             do!j1=0,inner_degrees_of_freedom-1,+1

              end          do!i1=0,inner_degrees_of_freedom-1,+1

              end       do!j2=0,inner_degrees_of_freedom-1,+1

              end    do!i2=0,inner_degrees_of_freedom-1,+1

                     select case(boson_degrees_of_freedom)

                     case(6)

                        m(:,:)&
                       =m(:,:)+(gamma(:,:,d).x.a_delta(:,:))*fermi_mass

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


                  do j=0,inner_degrees_of_freedom-1,+1

                     do i=0,inner_degrees_of_freedom-1,+1

                           do i2=0,inner_degrees_of_freedom-1,+1

                              do j2=0,inner_degrees_of_freedom-1,+1

                                 if(inner_degrees_of_freedom*i2+j2==n_size-1) exit

                                 do i1=0,inner_degrees_of_freedom-1,+1

                                    do j1=0,inner_degrees_of_freedom-1,+1

                                       if(inner_degrees_of_freedom*i1+j1==n_size-1) exit

                                       delta_3(inner_degrees_of_freedom*i1+j1,&
                                               inner_degrees_of_freedom*i2+j2)= delta(j1,j)*delta(i,i2)*delta(j2,i1)              &
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

              end    do!i=0,inner_degrees_of_freedom-1,+1

              end do!j=0,inner_degrees_of_freedom-1,+1


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


        end subroutine update_fermion_matrix!


#        endif


  end module fields


#  endif
