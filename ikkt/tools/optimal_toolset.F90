#     ifndef OPTIMAL_TOOLSET_F90
#     define OPTIMAL_TOOLSET_F90

#     include "system/precision.F90"

#     include "tensor/tensor.F90"

#     include "ikkt/fields.F90"


      module optimal_toolset


            use::fields

            use::tensor_type


            implicit none


            public::ucv,mv,cmv,um,ucm,cmmv,ucmm,ucmmv,umav


      !     interface operator( .o. )

      !        module procedure uov

      ! end interface operator( .o. )

      !     interface operator( .c. )

      !        module procedure ucv

      ! end interface operator( .c. )

      !     interface operator( .m. )

      !        module procedure  mv
      !        module procedure um

      ! end interface operator( .m. )

      !     interface operator( .cm. )

      !        module procedure  cmv
      !        module procedure ucm

      ! end interface operator( .cm. )

      !     interface operator( .cmm. )

      !        module procedure  cmmv
      !        module procedure ucmm
      !        module procedure ucmmv

      ! end interface operator( .cmm. )

      !     interface operator( .ma. )

      !        module procedure umav

      ! end interface operator( .ma. )


      contains


            function uov(u,v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::u,v
                  complex(KK)::uov

                  integer::a_


                  uov=+.00000e+0_KK

                  do a_=0,fermi_degrees_of_freedom-1,+1

                     uov&
                    =uov+trace(u(:,:,a_).o.v(:,:,a_))

              end do!a_=0,fermi_degrees_of_freedom-1,+1


        end function uov!u,v


            function ucv(u,v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::u,v
                  real(KK)::ucv

                  integer::a_


                  ucv=+.00000e+0_KK

                  do a_=0,fermi_degrees_of_freedom-1,+1

                     ucv&
                    =ucv+trace(u(:,:,a_).c.v(:,:,a_))

              end do!a_=0,fermi_degrees_of_freedom-1,+1


        end function ucv!u,v


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

                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1)::av_va

                  integer::a1,a2,m_


                  mv=zero

                  do m_=0,boson_degrees_of_freedom-1,+1

                     do a2=0,fermi_degrees_of_freedom-1,+1

                        av_va(:,:)=transpose(a(:,:,m_).commutation.v(:,:,a2))

                        do a1=0,fermi_degrees_of_freedom-1,+1

                           mv(:,:,a1)&
                          =mv(:,:,a1)+gamma(a1,a2,m_)*(av_va(:,:)&
                                                     - av_va(n,n)*delta(:,:))

              end       do!a1=0,fermi_degrees_of_freedom-1,+1

              end    do!a2=0,fermi_degrees_of_freedom-1,+1

              end do!m_=0,boson_degrees_of_freedom-1,+1

                  if(massive_deformations) then

                     select case(boson_degrees_of_freedom)

                     case(6)

                        do a2=0,fermi_degrees_of_freedom-1,+1

                           do a1=0,fermi_degrees_of_freedom-1,+1

                              mv(:,:,a1)&
                             =mv(:,:,a1)+gamma(a1,a2,d)*transpose(v(:,:,a2)&
                                                                - v(n,n,a2)*delta(:,:))

              end          do!a1=0,fermi_degrees_of_freedom-1,+1

              end       do!a2=0,fermi_degrees_of_freedom-1,+1

              end    select!case(boson_degrees_of_freedom)

              end if!massive_deformations


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

                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1)::av_va

                  integer::a1,a2,m_


                  cmv=zero

                  do m_=0,boson_degrees_of_freedom-1,+1

                     do a2=0,fermi_degrees_of_freedom-1,+1

                        av_va(:,:)=transpose(conjg(a(:,:,m_)).commutation.v(:,:,a2))

                        do a1=0,fermi_degrees_of_freedom-1,+1

                           cmv(:,:,a1)&
                          =cmv(:,:,a1)-conjugate_gamma(a1,a2,m_)*(av_va(:,:)&
                                                                - av_va(n,n)*delta(:,:))

              end       do!a1=0,fermi_degrees_of_freedom-1,+1

              end    do!a2=0,fermi_degrees_of_freedom-1,+1

              end do!m_=0,boson_degrees_of_freedom-1,+1

                  if(massive_deformations) then

                     select case(boson_degrees_of_freedom)

                     case(6)

                        do a2=0,fermi_degrees_of_freedom-1,+1

                           do a1=0,fermi_degrees_of_freedom-1,+1

                              cmv(:,:,a1)&
                             =cmv(:,:,a1)+conjugate_gamma(a1,a2,d)*transpose(v(:,:,a2)&
                                                                           - v(n,n,a2)*delta(:,:))

              end          do!a1=0,fermi_degrees_of_freedom-1,+1

              end       do!a2=0,fermi_degrees_of_freedom-1,+1

              end    select!case(boson_degrees_of_freedom)

              end if!massive_deformations


        end function cmv!a,v


            function um(u,a)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::u
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1)::um

                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1)::ua_au

                  integer::a1,a2,m_


                  um=zero

                  do m_=0,boson_degrees_of_freedom-1,+1

                     do a1=0,fermi_degrees_of_freedom-1,+1

                        ua_au(:,:)=transpose(conjg(u(:,:,a1)).commutation.a(:,:,m_))

                        do a2=0,fermi_degrees_of_freedom-1,+1

                           um(:,:,a2)&
                          =um(:,:,a2)+gamma(a1,a2,m_)*(ua_au(:,:)&
                                                     - ua_au(n,n)*delta(:,:))

              end       do!a2=0,fermi_degrees_of_freedom-1,+1

              end    do!a1=0,fermi_degrees_of_freedom-1,+1

              end do!m_=0,boson_degrees_of_freedom-1,+1

                  if(massive_deformations) then

                     select case(boson_degrees_of_freedom)

                     case(6)

                        do a1=0,fermi_degrees_of_freedom-1,+1

                           do a2=0,fermi_degrees_of_freedom-1,+1

                              um(:,:,a2)&
                             =um(:,:,a2)+gamma(a1,a2,d)*conjugate(u(:,:,a1)&
                                                                - u(n,n,a1)*delta(:,:))

              end          do!a2=0,fermi_degrees_of_freedom-1,+1

              end       do!a1=0,fermi_degrees_of_freedom-1,+1

              end    select!case(boson_degrees_of_freedom)

              end if!massive_deformations


        end function um!u,a


            function ucm(u,a)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::u
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1)::ucm

                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1)::ua_au

                  integer::a1,a2,m_


                  ucm=zero

                  do m_=0,boson_degrees_of_freedom-1,+1

                     do a1=0,fermi_degrees_of_freedom-1,+1

                        ua_au(:,:)=conjugate(u(:,:,a1).commutation.a(:,:,m_))

                        do a2=0,fermi_degrees_of_freedom-1,+1

                           ucm(:,:,a2)&
                          =ucm(:,:,a2)-conjugate_gamma(a1,a2,m_)*(ua_au(:,:)&
                                                                - ua_au(n,n)*delta(:,:))

              end       do!a2=0,fermi_degrees_of_freedom-1,+1

              end    do!a1=0,fermi_degrees_of_freedom-1,+1

              end do!m_=0,boson_degrees_of_freedom-1,+1

                  if(massive_deformations) then

                     select case(boson_degrees_of_freedom)

                     case(6)

                        do a1=0,fermi_degrees_of_freedom-1,+1

                           do a2=0,fermi_degrees_of_freedom-1,+1

                              ucm(:,:,a1)&
                             =ucm(:,:,a1)+conjugate_gamma(a1,a2,d)*conjugate(u(:,:,a2)&
                                                                          - u(n,n,a2)*delta(:,:))

              end          do!a2=0,fermi_degrees_of_freedom-1,+1

              end       do!a1=0,fermi_degrees_of_freedom-1,+1

              end    select!case(boson_degrees_of_freedom)

              end if!massive_deformations


        end function ucm!u,a


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


                  cmmv=cmv(a,(mv(a,v)))


        end function cmmv!a,v


            function ucmm(u,a)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::u
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1)::ucmm


                  ucmm=um(ucm(u,a),a)


        end function ucmm!u,a


            function ucmmv(u,a,v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::u,v
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  real(KK)::ucmmv


            !     ucmmv=ucv(u,cm(a,mv(a,v)))=ucv(u,cmmv(a,v))

                  ucmmv=uov(ucm(u,a),mv(a,v))

            !     ucmmv=uov(um(ucm(u,a),a),v)=uov(ucmm(u,a),v)

        end function ucmmv!u,a,v


            function umav(u,v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::u,v
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1)::umav

                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1)::uv_vu

                  integer::a1,a2,m_


                  umav=zero

                  do a2=0,boson_degrees_of_freedom-1,+1

                     do a1=0,fermi_degrees_of_freedom-1,+1

                        uv_vu(:,:)=conjg(u(:,:,a1)).commutation.v(:,:,a2)

                        do m_=0,fermi_degrees_of_freedom-1,+1

                           umav(:,:,m_)&
                          =umav(:,:,m_)-gamma(a1,a2,m_)*uv_vu(:,:)

              end       do!m_=0,fermi_degrees_of_freedom-1,+1

              end    do!a1=0,fermi_degrees_of_freedom-1,+1

              end do!a2=0,boson_degrees_of_freedom-1,+1


        end function umav!u,v


  end module optimal_toolset


#  endif
