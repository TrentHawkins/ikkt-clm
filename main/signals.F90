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

#  endif
