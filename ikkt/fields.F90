#     ifndef FIELDS_F90
#     define FIELDS_F90

#     include "system/precision.F90"
#     include "system/text_format.F90"
#     include "main/mathematical_constants.F90"

#     include "tensor/tensor.F90"

#     include "ikkt/constants.F90"


      module fields


            use::lapack95,only:geev,&
                               ggev

            use::text_formatting
            use::mathematical_constants

            use::tensor_type

            use::constants


            implicit none


            character(*),private,parameter::           format_stat_K=INTEGERGK,&
                                            text_field_format_stat_K=INTEGERAK

            character(*),private,parameter::           format_mass_K=REALGK,&
                                            text_field_format_mass_K=REALAK

            logical,public::configuration_loaded=.false.,&
                            start_field_is_noisy=.false.,&
                            fermions_included_in=.false.,&
                            massive_deformations=.false.

            character(:),allocatable,public::stat_file_name,&
                                             conf_file_name

            integer,public::inner_degrees_of_freedom,n,n_size,&
                            boson_degrees_of_freedom,d,a_size,&
                            fermi_degrees_of_freedom,p,f_size

            real(KK),                                                    public::boson_epsilon,&
                                                                                 fermi_mass
            real(KK),dimension( :                          ),allocatable,public::boson_mass

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

            private::make_m            ,&
                     make_cm           ,&
                     make_cmm          ,&
                     make_ma           ,&
                     make_m_eigenvalues

            public::update_fermion_matrix

#        endif

            public::boson_noise,&
                    fermi_noise

            public::make_fields,&
                    load_fields,&
                    save_fields


      contains


            subroutine  read_field_parameters(unit)


                  implicit none


                  integer,intent(in   )::unit

                  integer::mu


                  write(   *,"(2a)",advance="no") "inner_degrees_of_freedom: ",t_yellow
                   read(unit,   *               )  inner_degrees_of_freedom
                  write(   *,"( a)",advance="no")                              t_normal
                  write(   *,"(2a)",advance="no") "boson_degrees_of_freedom: ",t_yellow
                   read(unit,   *               )  boson_degrees_of_freedom
                  write(   *,"( a)",advance="no")                              t_normal

                                               fermi_degrees_of_freedom=2 &
                                            **(boson_degrees_of_freedom/2 &
                                                                       -1)
                                       n     = inner_degrees_of_freedom-1
                                       d     = boson_degrees_of_freedom-1
                                       p     = fermi_degrees_of_freedom-1
                                       n_size= inner_degrees_of_freedom&
                                             * inner_degrees_of_freedom
                                       a_size= boson_degrees_of_freedom* n_size
                                       f_size= fermi_degrees_of_freedom*(n_size-1)

                  write(   *,   *               )

                  if(massive_deformations) then

                     write(   *,"(2a)",advance="no") "boson_epsilon: ",t_yellow
                      read(unit,   *               )  boson_epsilon
                     write(   *,"( a)",advance="no")                   t_normal

                     write(   *,   *               )

                     if(allocated(boson_mass)) deallocate(boson_mass)
                                                 allocate(boson_mass(0:boson_degrees_of_freedom-1))

                     do mu=0,boson_degrees_of_freedom-1,+1

                        write(   *,"(a,i2,2a)",advance="no") "boson_mass(",mu+1,"): ",t_yellow
                         read(unit,        *               )  boson_mass(  mu    )
                        write(   *,"(      a)",advance="no")                          t_normal

              end    do!mu=0,boson_degrees_of_freedom-1,+1

                     write(   *,        *               )

                     if(fermions_included_in) then

                        write(   *,"(2a)",advance="no") "fermi_mass: ",t_yellow
                         read(unit,   *               )  fermi_mass
                        write(   *,"( a)",advance="no")                t_normal

                        write(   *,   *               )

              end    if!fermions_included_in

              end if!massive_deformations

                  if(allocated(a)) deallocate(a)
                                     allocate(a(0:inner_degrees_of_freedom-1,&
                                                0:inner_degrees_of_freedom-1,&
                                                0:boson_degrees_of_freedom-1))

                  if(fermions_included_in) then

#                    ifdef OPTIMAL

                     if(allocated(f)) deallocate(f)
                                        allocate(f(0:inner_degrees_of_freedom-1,&
                                                   0:inner_degrees_of_freedom-1,&
                                                   0:fermi_degrees_of_freedom-1))

#                    else

                     if(allocated(f)) deallocate(f)
                                        allocate(f(0:f_size-1))

                     if(allocated(m_eigenvalues_)) deallocate(m_eigenvalues_)
                                                     allocate(m_eigenvalues_(0:f_size-1))

                     if(allocated( m )) deallocate( m )
                                          allocate( m (0:f_size-1,&
                                                       0:f_size-1))
                     if(allocated(cm )) deallocate(cm )
                                          allocate(cm (0:f_size-1,&
                                                       0:f_size-1))
                     if(allocated(cmm)) deallocate(cmm)
                                          allocate(cmm(0:f_size-1,&
                                                       0:f_size-1))

                     if(allocated(ma)) deallocate(ma)
                                         allocate(ma(0:f_size-1,&
                                                     0:f_size-1,0:inner_degrees_of_freedom-1,&
                                                                0:inner_degrees_of_freedom-1,&
                                                                0:boson_degrees_of_freedom-1))

#                 endif

              end if!fermions_included_in


        end subroutine  read_field_parameters!unit


            subroutine write_field_parameters(unit)


                  implicit none


                  integer,intent(in   )::unit

                  integer::mu


                  if(allocated(a)) deallocate(a)

                  if(fermions_included_in) then

                     if(allocated(f)) deallocate(f)

#                    ifndef OPTIMAL

                     if(allocated(m_eigenvalues_)) deallocate(m_eigenvalues_)

                     if(allocated( m )) deallocate( m )
                     if(allocated(cm )) deallocate(cm )
                     if(allocated(cmm)) deallocate(cmm)

                     if(allocated(ma)) deallocate(ma)

#                 endif

              end if!fermions_included_in

                  write(unit,format_stat_K)  inner_degrees_of_freedom
                  write(unit,format_stat_K)  boson_degrees_of_freedom

                  if(massive_deformations) then

                     write(unit,format_mass_K)  boson_epsilon

                     do mu=0,boson_degrees_of_freedom-1,+1

                        write(unit,format_mass_K)  boson_mass(  mu    )

              end    do!mu=0,boson_degrees_of_freedom-1,+1

                     if(allocated(boson_mass)) deallocate(boson_mass)

                     if(fermions_included_in) write(unit,format_mass_K)  fermi_mass

              end if!massive_deformations

                  write(unit,*)


        end subroutine write_field_parameters!unit


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
                    =standard_deviation*sqrt(                    -log(real(boson_noise(:,:,mu))))&
                                       * exp(.20000e+1_KK*pi*im_unit*aimag(boson_noise(:,:,mu)))

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
                 =standard_deviation*sqrt(                    -log(real(fermi_noise)))&
                                    * exp(.20000e+1_KK*pi*im_unit*aimag(fermi_noise))

#                 else

                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1)::fermi_noise

                  integer::a_


                  do a_=0,fermi_degrees_of_freedom-1,+1

                     call random_number(fermi_noise(:,:,a_))

                                                                           fermi_noise(:,:,a_)   &
                    =standard_deviation*sqrt(                    -log(real(fermi_noise(:,:,a_))))&
                                       * exp(.20000e+1_KK*pi*im_unit*aimag(fermi_noise(:,:,a_)))

                     call make_traceless(fermi_noise(:,:,a_))

              end do!a_=0,fermi_degrees_of_freedom-1,+1

#              endif


        end function fermi_noise!standard_deviation


            subroutine make_fields()


                  implicit none


                  integer::mu


                  call  read_field_parameters(5)

                     a=boson_noise(.20000e+1_KK)


        end subroutine make_fields!


            subroutine load_fields()


                  implicit none


                  integer::unit
                  integer::mu


                   open(newunit=unit,file=stat_file_name)

                  call  read_field_parameters(unit)

                  close(unit)

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

                   open(newunit=unit,file=stat_file_name)

                  call write_field_parameters(unit)

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
