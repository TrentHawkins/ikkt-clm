#     ifndef SIGNALS_F90
#     define SIGNALS_F90

!            SIGHUP     Term    Hangup detected on controlling terminal or death of controlling process
!            SIGINT     Term    Interrupt from keyboard
!            SIGQUIT    Core    Quit from keyboard
!            SIGILL     Core    Illegal Instruction
!            SIGABRT    Core    Abort signal from abort(3)
!            SIGFPE     Core    Floating-point exception
!            SIGKILL    Term    Kill signal
!            SIGSEGV    Core    Invalid memory reference
!            SIGPIPE    Term    Broken pipe: write to pipe with noreaders; see pipe(7)

#     define SIGHUP   1
#     define SIGINT   2
#     define SIGQUIT  3
#     define SIGILL   4
#     define SIGABRT  6
#     define SIGFPE   8
#     define SIGKILL  9
#     define SIGSEGV 11
#     define SIGPIPE 13


      subroutine signal_actions(handler)


            implicit none


            external::handler


!           call signal(SIGHUP,handler)
            call signal(SIGINT,handler)
!           call signal(SIGFPE,handler)

!           call signal(SIGQUIT,handler)
!           call signal(SIGABRT,handler)


  end subroutine signal_actions!handler


#  endif
