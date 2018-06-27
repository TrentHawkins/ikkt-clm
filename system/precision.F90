#     ifndef SYSTEM_PRECISION_F90
#     define SYSTEM_PRECISION_F90

#     define CHAR 1023

!                                             +127 i04.03
!                                           +32767 i06.05
!                                      +2147483647 i11.10
!                             +9223372036854775807 i20.19
!         +170141183460469231731687303715884105727 i40.39

#     define  INTEGERG0                   "(ss, 3x,i04.03,2x)"
#     define  INTEGERG1                   "(ss, 3x,i06.05,3x)"
#     define  INTEGERG2                   "(ss, 3x,i11.10,4x)"
#     define  INTEGERG3                   "(ss, 3x,i20.19,5x)"
!     define  INTEGERG4                   "(ss, 3x,i40.39,6x)"

#     define  INTEGERA0                   "(    3x,a04   ,2x)"
#     define  INTEGERA1                   "(    3x,a06   ,3x)"
#     define  INTEGERA2                   "(    3x,a11   ,4x)"
#     define  INTEGERA3                   "(    3x,a20   ,5x)"
!     define  INTEGERA4                   "(    3x,a40   ,6x)"

#     define  INTEGERN0                             04
#     define  INTEGERN1                             06
#     define  INTEGERN2                             11
#     define  INTEGERN3                             20
!     define  INTEGERN4                             40

!                                     +.____e+     e08.04e0
!                                    +._____e+_    e10.05e1
!                                +.340282347e+39   e15.09e2
!                        +.17976931348623157e+309  e24.17e3
!     +.118973149535723176508575932662800702e+4933 e44.36e4

!     define     REALG0                   "(sp,  x,e08.04e0 )"
#     define     REALG1                   "(sp,  x,e10.05e1 )"
#     define     REALG2                   "(sp,  x,e15.09e2 )"
#     define     REALG3                   "(sp,  x,e24.17e3 )"
#     define     REALG4                   "(sp,  x,e44.36e4 )"

!     define     REALA0                   "(     x,a08      )"
#     define     REALA1                   "(     x,a10      )"
#     define     REALA2                   "(     x,a15      )"
#     define     REALA3                   "(     x,a24      )"
#     define     REALA4                   "(     x,a44      )"

!     define  COMPLEXG0                   "(sp,2(x,e08.04e0))"
#     define  COMPLEXG1                   "(sp,2(x,e10.05e1))"
#     define  COMPLEXG2                   "(sp,2(x,e15.09e2))"
#     define  COMPLEXG3                   "(sp,2(x,e24.17e3))"
#     define  COMPLEXG4                   "(sp,2(x,e44.36e4))"

!     define  COMPLEXA0                   "(   2(x,a08     ))"
#     define  COMPLEXA1                   "(   2(x,a10     ))"
#     define  COMPLEXA2                   "(   2(x,a15     ))"
#     define  COMPLEXA3                   "(   2(x,a24     ))"
#     define  COMPLEXA4                   "(   2(x,a44     ))"

!     define  GENERICG0                   "(sp,*(x,e08.04e0))"
#     define  GENERICG1                   "(sp,*(x,e10.05e1))"
#     define  GENERICG2                   "(sp,*(x,e15.09e2))"
#     define  GENERICG3                   "(sp,*(x,e24.17e3))"
#     define  GENERICG4                   "(sp,*(x,e44.36e4))"

!     define  GENERICA0                   "(   *(x,a08     ))"
#     define  GENERICA1                   "(   *(x,a10     ))"
#     define  GENERICA2                   "(   *(x,a15     ))"
#     define  GENERICA3                   "(   *(x,a24     ))"
#     define  GENERICA4                   "(   *(x,a44     ))"

#     define  GENERICN0                             08
#     define  GENERICN1                             10
#     define  GENERICN2                             15
#     define  GENERICN3                             24
!     define  GENERICN4                             44

!     define TOLERANCE0     .10000e-4
#     define TOLERANCE1     .10000e-1
#     define TOLERANCE2     .10000e-1
#     define TOLERANCE3     .10000e-5
#     define TOLERANCE4 .100000000e-20

#     define         KK         08
#     define          K          3

#     define  INTEGERGK  INTEGERG3
#     define  INTEGERAK  INTEGERA3
#     define  INTEGERNK  INTEGERN3

#     define     REALGK     REALG3
#     define     REALAK     REALA3

#     define  COMPLEXGK  COMPLEXG3
#     define  COMPLEXAK  COMPLEXA3

#     define  GENERICGK  GENERICG3
#     define  GENERICAK  GENERICA3
#     define  GENERICNK  GENERICN3

#     define TOLERANCEK TOLERANCE3

#  endif
