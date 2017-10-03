#     ifndef PRECISION_F90
#     define PRECISION_F90

#     define CHAR 1023

!                                             +127 i04.03
!                                           +32767 i06.05
!                                      +2147483647 i11.10
!                             +9223372036854775807 i20.19
!         +170141183460469231731687303715884105727 i40.39

#     define  INTEGERG0                   "(sp,  x,i04.03   )"
#     define  INTEGERG1                   "(sp,  x,i06.05   )"
#     define  INTEGERG2                   "(sp,  x,i11.10   )"
#     define  INTEGERG3                   "(sp,  x,i20.19   )"
#     define  INTEGERG4                   "(sp,  x,i40.39   )"

#     define  INTEGERA0                   "(     x,a04      )"
#     define  INTEGERA1                   "(     x,a06      )"
#     define  INTEGERA2                   "(     x,a11      )"
#     define  INTEGERA3                   "(     x,a20      )"
#     define  INTEGERA4                   "(     x,a40      )"

!                                     +__.__       f06.04
!                                    +._____e+_    e10.05e1
!                                +.340282347e+39   e15.09e2
!                        +.17976931348623157e+309  e24.17e3
!     +.118973149535723176508575932662800702e+4933 e44.36e4

#     define     REALG0                   "(sp,  x,f06.02   )"
#     define     REALG1                   "(sp,  x,e10.05e1 )"
#     define     REALG2                   "(sp,  x,e15.09e2 )"
#     define     REALG3                   "(sp,  x,e24.17e3 )"
#     define     REALG4                   "(sp,  x,e44.36e4 )"

#     define     REALA0                   "(     x,a06      )"
#     define     REALA1                   "(     x,a10      )"
#     define     REALA2                   "(     x,a15      )"
#     define     REALA3                   "(     x,a24      )"
#     define     REALA4                   "(     x,a44      )"

#     define  COMPLEXG0                   "(sp,2(x,f06.02  ))"
#     define  COMPLEXG1                   "(sp,2(x,e10.05e1))"
#     define  COMPLEXG2                   "(sp,2(x,e15.09e2))"
#     define  COMPLEXG3                   "(sp,2(x,e24.17e3))"
#     define  COMPLEXG4                   "(sp,2(x,e44.36e4))"

#     define  COMPLEXA0                   "(   2(x,a06     ))"
#     define  COMPLEXA1                   "(   2(x,a10     ))"
#     define  COMPLEXA2                   "(   2(x,a15     ))"
#     define  COMPLEXA3                   "(   2(x,a24     ))"
#     define  COMPLEXA4                   "(   2(x,a44     ))"

#     define TOLERANCE0                           .01
#     define TOLERANCE1                          .001
#     define TOLERANCE2                        .00001
#     define TOLERANCE3                  .00000000001
#     define TOLERANCE4 .0000000000000000000000000001

#     define         KK         08
#     define          K          3

#     define  INTEGERGK  INTEGERG3
#     define  INTEGERAK  INTEGERA3

#     define     REALGK     REALG3
#     define     REALAK     REALA3

#     define  COMPLEXGK  COMPLEXG3
#     define  COMPLEXAK  COMPLEXA3

#     define TOLERANCEK TOLERANCE3

#  endif
