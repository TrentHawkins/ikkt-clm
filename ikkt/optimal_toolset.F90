#     ifndef IKKT_OPTIMAL_TOOLSET_F90
#     define IKKT_OPTIMAL_TOOLSET_F90

#     include "main/precision.F90"

#     include "tensor/tensor.F90"

#     include "ikkt/fields.F90"


      module optimal_toolset


            use::fields

            use::tensor_type


            implicit none


            public::uv,vv,mv,cmv,um,ucm,ucmmv,vcmmv,cmmv,ucmm,umav,vmav


      contains


            function uv(u,v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::u,v
                  complex(KK)::uv

                  integer::a_


                  uv=zero

                  do a_=0,fermi_degrees_of_freedom-1,+1

                     uv&
                    =uv+trace(u(:,:,a_).c.v(:,:,a_))

              end do!a_=0,fermi_degrees_of_freedom-1,+1


        end function uv!u,v


            function vv(v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::v
                  real(KK)::vv

                  integer::a_


                  vv=+.00000e+0_KK

                  do a_=0,fermi_degrees_of_freedom-1,+1

                     vv&
                    =vv+norm(v(:,:,a_))

              end do!a_=0,fermi_degrees_of_freedom-1,+1


        end function vv!v


            function mv(a,v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::v
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1)::mv

                  integer::a1,a2,m_


                  mv=zero

                  do m_=0,boson_degrees_of_freedom-1,+1

                     do a2=0,fermi_degrees_of_freedom-1,+1

                        do a1=0,fermi_degrees_of_freedom-1,+1

                           mv(:,:,a1)&
                          =mv(:,:,a1)+gamma(a1,a2,m_)*transpose(a(:,:,m_).commutation.v(:,:,a2))

              end       do!a1=0,fermi_degrees_of_freedom-1,+1

              end    do!a2=0,fermi_degrees_of_freedom-1,+1

              end do!m_=0,boson_degrees_of_freedom-1,+1


        end function mv!a,v


            function cmv(a,v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::v
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1)::cmv

                  integer::a1,a2,m_


                  cmv=zero

                  do m_=0,boson_degrees_of_freedom-1,+1

                     do a2=0,fermi_degrees_of_freedom-1,+1

                        do a1=0,fermi_degrees_of_freedom-1,+1

                           cmv(:,:,a1)&
                          =cmv(:,:,a1)-gamma_conjugate(a1,a2,m_)*transpose(conjg(a(:,:,m_)).commutation.v(:,:,a2))

              end       do!a1=0,fermi_degrees_of_freedom-1,+1

              end    do!a2=0,fermi_degrees_of_freedom-1,+1

              end do!m_=0,boson_degrees_of_freedom-1,+1


        end function cmv!a,v


            function um(u,a)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::u
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1)::um

                  integer::a1,a2,m_


                  um=zero

                  do m_=0,boson_degrees_of_freedom-1,+1

                     do a2=0,fermi_degrees_of_freedom-1,+1

                        do a1=0,fermi_degrees_of_freedom-1,+1

                           um(:,:,a2)&
                          =um(:,:,a2)-gamma(a1,a2,m_)*transpose(a(:,:,m_).commutation.conjg(u(:,:,a1)))

              end       do!a1=0,fermi_degrees_of_freedom-1,+1

              end    do!a2=0,fermi_degrees_of_freedom-1,+1

              end do!m_=0,boson_degrees_of_freedom-1,+1


        end function um!u,a


            function ucm(u,a)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::u
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1)::ucm

                  integer::a1,a2,m_


                  ucm=zero

                  do m_=0,boson_degrees_of_freedom-1,+1

                     do a2=0,fermi_degrees_of_freedom-1,+1

                        do a1=0,fermi_degrees_of_freedom-1,+1

                           ucm(:,:,a2)&
                          =ucm(:,:,a2)+gamma_conjugate(a1,a2,m_)*conjugate(a(:,:,m_).commutation.u(:,:,a1))

              end       do!a1=0,fermi_degrees_of_freedom-1,+1

              end    do!a2=0,fermi_degrees_of_freedom-1,+1

              end do!m_=0,boson_degrees_of_freedom-1,+1


        end function ucm!u,a


            function ucmmv(u,a,v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::u,v
                  complex(KK)::ucmmv

                  integer::a1,a2,m1,m2


                  ucmmv=zero

                  do m2=0,boson_degrees_of_freedom-1,+1

                     do m1=0,boson_degrees_of_freedom-1,+1

                        do a2=0,fermi_degrees_of_freedom-1,+1

                           do a1=0,fermi_degrees_of_freedom-1,+1

                              ucmmv&
                             =ucmmv+gamma_core(a1,a2,m1,m2)*trace((a(:,:,m1).commutation.u(:,:,a1))&
                                                          .c.     (a(:,:,m2).commutation.v(:,:,a2)))

              end          do!a1=0,fermi_degrees_of_freedom-1,+1

              end       do!a2=0,fermi_degrees_of_freedom-1,+1

              end    do!m1=0,boson_degrees_of_freedom-1,+1

              end do!m2=0,boson_degrees_of_freedom-1,+1


        end function ucmmv!u,a,v


            function vcmmv(a,v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::v
                  real(KK)::vcmmv

                  integer::a1,a2,m1,m2


                  vcmmv=+.00000e+0_KK

                  do m2=0,boson_degrees_of_freedom-1,+1

                     do m1=0,boson_degrees_of_freedom-1,+1

                        do a2=0,fermi_degrees_of_freedom-1,+1

                           do a1=0,fermi_degrees_of_freedom-1,+1

                              vcmmv&
                             =vcmmv+gamma_core(a1,a2,m1,m2)*trace((a(:,:,m1).commutation.v(:,:,a1))&
                                                          .c.     (a(:,:,m2).commutation.v(:,:,a2)))

              end          do!a1=0,fermi_degrees_of_freedom-1,+1

              end       do!a2=0,fermi_degrees_of_freedom-1,+1

              end    do!m1=0,boson_degrees_of_freedom-1,+1

              end do!m2=0,boson_degrees_of_freedom-1,+1


        end function vcmmv!a,v


            function cmmv(a,v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::v
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1)::cmmv

                  integer::a1,a2,m1,m2


                  cmmv=zero

                  do m2=0,boson_degrees_of_freedom-1,+1

                     do m1=0,boson_degrees_of_freedom-1,+1

                        do a2=0,fermi_degrees_of_freedom-1,+1

                           do a1=0,fermi_degrees_of_freedom-1,+1

                              cmmv(:,:,a1)&
                             =cmmv(:,:,a1)+gamma_core(a1,a2,m1,m2)*     (conjugate(a(:,:,m1)).commutation.&
                                                                                  (a(:,:,m2) .commutation.v(:,:,a2)))

              end          do!a1=0,fermi_degrees_of_freedom-1,+1

              end       do!a2=0,fermi_degrees_of_freedom-1,+1

              end    do!m1=0,boson_degrees_of_freedom-1,+1

              end do!m2=0,boson_degrees_of_freedom-1,+1


        end function cmmv!a,v


            function ucmm(a,v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::v
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1)::ucmm

                  integer::a1,a2,m1,m2


                  ucmm=zero

                  do m2=0,boson_degrees_of_freedom-1,+1

                     do m1=0,boson_degrees_of_freedom-1,+1

                        do a2=0,fermi_degrees_of_freedom-1,+1

                           do a1=0,fermi_degrees_of_freedom-1,+1

                              ucmm(:,:,a2)&
                             =ucmm(:,:,a2)+gamma_core(a1,a2,m1,m2)*conjg(conjugate(a(:,:,m2)).commutation.&
                                                                                  (a(:,:,m1) .commutation.v(:,:,a1)))

              end          do!a1=0,fermi_degrees_of_freedom-1,+1

              end       do!a2=0,fermi_degrees_of_freedom-1,+1

              end    do!m1=0,boson_degrees_of_freedom-1,+1

              end do!m2=0,boson_degrees_of_freedom-1,+1


        end function ucmm!a,v


            function umav(u,v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::u,v
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1)::umav

                  integer::a1,a2,m_


                  umav=zero

                  do m_=0,boson_degrees_of_freedom-1,+1

                     do a2=0,fermi_degrees_of_freedom-1,+1

                        do a1=0,fermi_degrees_of_freedom-1,+1

                           umav(:,:,m_)&
                          =umav(:,:,m_)-gamma(a1,a2,m_)*(conjg(u(:,:,a1)).commutation.v(:,:,a2))

              end       do!a1=0,fermi_degrees_of_freedom-1,+1

              end    do!a2=0,fermi_degrees_of_freedom-1,+1

              end do!m_=0,boson_degrees_of_freedom-1,+1


        end function umav!u,v


            function vmav(v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::v
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1)::vmav

                  integer::a1,a2,m_


                  vmav=zero

                  do m_=0,boson_degrees_of_freedom-1,+1

                     do a2=0,fermi_degrees_of_freedom-1,+1

                        do a1=0,fermi_degrees_of_freedom-1,+1

                           vmav(:,:,m_)&
                          =vmav(:,:,m_)-gamma(a1,a2,m_)*(conjg(v(:,:,a1)).commutation.v(:,:,a2))

              end       do!a1=0,fermi_degrees_of_freedom-1,+1

              end    do!a2=0,fermi_degrees_of_freedom-1,+1

              end do!m_=0,boson_degrees_of_freedom-1,+1


        end function vmav!v


  end module optimal_toolset


#  endif
