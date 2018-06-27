#!/bin/bash --


   filename="$(basename -- ${0})"
   basename="${filename%.sh}"

   exit_status=0

   while  getopts : option
   do

      case   ${option} in

      :)

         echo "${basename}: option -${OPTARG} requires an argument" >&2
         exit 1

         ;;

      ?)

         echo "${basename}: invalid option -${OPTARG}" >&2
         exit 1

         ;;

      esac # ${option} in

   done # getopts : option

   shift $((OPTIND-1))

   printf "\n\033[0mifort -module \"../ifort/modules\" -assume source_include -I include -fast -w ems_eigenvalues.F90 -o ems.out"
   printf " -mkl=sequential -lmkl_blas95_lp64 -lmkl_lapack95_lp64 -lmkl_core -lpthread -lm -ldl\n\n\033[31m"

   ifort -module "../ifort/modules" \
         -assume source_include      \
         -I include                   \
         -fast                         \
         -w ems_eigenvalues.F90         \
         -o ems_eigenvalues.out          \
         -mkl=sequential                  \
         -lmkl_blas95_lp64                 \
         -lmkl_lapack95_lp64                \
         -lmkl_core                          \
         -lpthread                            \
         -lm                                   \
         -ldl

   [ $? -ne 0 ] && exit_status=1

   printf "\n\033[0mifort -module \"../ifort/modules\" -assume source_include -I include -fast"
   printf " -w ../analysis/autocorrelation_time/main.F90-o autocorrelation_time.main.out\n\n\033[31m"

   ifort -module "../ifort/modules"               \
         -assume source_include                    \
         -I include                                 \
         -fast                                       \
         -w ../analysis/autocorrelation_time/main.F90 \
         -o ../analysis/autocorrelation_time/main.out

   [ $? -ne 0 ] && exit_status=1

   printf "\n\033[0mifort -module \"../ifort/modules\" -assume source_include -I include -fast"
   printf " -w ../analysis/jack_knife/main.F90-o jack_knife.main.out\n\n\033[31m"

   ifort -module "../ifort/modules"     \
         -assume source_include          \
         -I include                       \
         -fast                             \
         -w ../analysis/jack_knife/main.F90 \
         -o ../analysis/jack_knife/main.out

   [ $? -ne 0 ] && exit_status=1

   [ ${exit_status} -ne 0 ] && exit 1

   printf "\033[0m"

   argc=${#}

   if   [ ${argc} = 0 ]
   then

      echo "${basename}: aborting with ${argc} source directories to process"
      exit 1

   fi # [ ${argc} = 0 ]

   inner_degrees_of_freedom=("016" "032" "064" "096" "128")

   fermion_masses=("0.10"  "0.20"  "0.30"  "0.40"  "0.50"  "0.60"  "0.70"  "0.80"  "0.90"  "1.00"  "1.10"  "1.20" )
   ssb_parameters=("0.020" "0.040" "0.060" "0.080" "0.100" "0.120" "0.140" "0.160" "0.180" "0.200" "0.220" "0.240")

   d="010"

                           data_directory="${1%/}"
   eigenvalues_filename="${data_directory}/aaaD${d}.eigenvalues.ems.out"

   begin=1
     end=120

   for    a in ${fermion_masses[@]}
   do

                a_eigenvalues_filename="${data_directory}/aaaD${d}a${a}.eigenvalues.ems.out"
      echo > "${a_eigenvalues_filename}"

      for    em in ${ssb_parameters[@]}
      do

                   em_a_eigenvalues_filename="${data_directory}/aaaD${d}a${a}e${em}.eigenvalues.ems.out"
         echo > "${em_a_eigenvalues_filename}"

         for    n in ${inner_degrees_of_freedom}
         do

                             pathname="aaaD${d}N${n}"
            file_basename="${pathname}a${a}e${em}"

                      collated_eigenvalues_data_filename="${data_directory}/${file_basename}.eigenvalues.ems.out"
            echo > "${collated_eigenvalues_data_filename}"

            for    ((node=${begin};node<=${end};++node))
            do

                           data_filename="${data_directory}/${pathname}/${file_basename}.${node}.ems.out"
               eigenvalues_data_filename="${data_directory}/${pathname}/${file_basename}.${node}.eigenvalues.ems.out"

               [ ! -f "${data_filename}" ] && continue

               ./ems_eignevalue.out "${data_filename}" "${eigenvalues_data_filename}"
                                cat                    "${eigenvalues_data_filename}" >> "${collated_eigenvalues_data_filename}"

            done # ((node=${begin};node<=${end};++node))

            ../analysis/jack_knife/main.out "${collated_eigenvalues_data_filename}" >> "${em_a_eigenvalues_filename}"

         done # n in ${inner_degrees_of_freedom}

      done # em in ${ssb_parameters[@]}

   done # a in ${fermion_masses[@]}
