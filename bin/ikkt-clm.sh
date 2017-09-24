#!/bin/bash --


   A=$(tput cols)

#  separator() {

#     NUMBER=$1 # number of times to repeat
#     PREFIX=$2 # regular # bold # italics # underlined # blinking # inverted
#               # black # red # green # yellow # blue # magenta # cyan # white
#               # white # cyan # magenta # blue # yellow # green # red # black
#     STRING=$3 # input string to repeat
#     SUFFIX=$4 # newline # tab # continue

#     VECTOR=$(printf "%-${NUMBER}s" "${STRING}")

#     printf "${PREFIX}${VECTOR// /$STRING}${SUFFIX}"

#  } # separator()


   A=$(($(tput cols)+2))

   separator() {

      NUMBER=$1 # number of times to repeat
      STRING=$2 # input string to repeat

      VECTOR=$(printf "%-${NUMBER}s" "${STRING}")

      printf "${VECTOR// /$STRING}"

   } # separator()


   preprocessor=""

       parallel=""
   mkl_parallel="sequential"

   switch=""
   binary="a.out"

                    OPTIONS=":D:Po:tcafgmh"

   while  getopts ${OPTIONS} OPTION
   do

      case                 ${OPTION} in

      'D')

         preprocessor="${preprocessor} -D ${OPTARG}"

         ;; # Add preprocessor flags that affect the simulation make up.
            # BLAS: use BLAS for core linear algebra operations instead of FORTRAN intrinsics.
            # OPTIMAL: use custom linear algebra operations optimizations tailored for the fermion matrix and fields.

      'P')

             parallel=" -parallel"
         mkl_parallel=  "parallel"

         ;;

      'o')

         binary="${OPTARG}"

         ;; # Choose base BINARY_NAME for the simulation compiled binary file.

      's')

         switch="${switch} --skip-time"

         ;; # Use a fixed time to skip before measuring
            # observables. This is supposed tobe at least an
            # order of magnitude larger than the average
            # timestep, and usually is comparable to the
            # autocorrelation time of the time history.

      't')

         switch="${switch} --variable-timestep"

         ;; # Use variable timestep with fixed timestep as
            # average. This alelviates drift divergence due to
            # instabilities in either a poorly chosen starting
            # configuration or a large timestep.

      'a')

         switch="${switch} --noisy-start-field"

         ;; # Start with a (gausssian) noisy (hot) initial
            # configuration instead of the default zero (cold)
            # initial configuration. Overriden by -c
            # when loading a previously saved configuration.

      'c')

         switch="${switch} --load-save"

         ;; # Load simulation stats and field configuration
            # from a previous simulation. This helps in
            # storing thermalized configurations, and reusing
            # instead of repeating thermalizations. Overrides
            # -a when not starting a simulation from
            # scratch.

      'f')

         switch="${switch} --fermions-included"

         ;; # Include fermions in simulation, for simulating
            # the full IKKT model.

      'g')

         switch="${switch} --gauge-cooling"

         ;; # Applythe standard gauge-cooling
            # post-processing to configurations after each
            # langevin step. This alleviates simulation
            # journeys in the imaginary direction.

      'm')

         switch="${switch} --mass-deformations"

         ;; # Include massive deformations in modifying the
            # IKKT drift used in the Complex Langevin method.
            # In particular for the boson model, a new
            # coupling parameter is added, along with masses
            # for the gauge bosons. In the full model with
            # fermions, a fermionic mass is added as well.

      'h'|':'|'?')

         switch="${switch} --help"

         ;; # Print help screen.

      esac                #{OPTION}

   done # getopts ${OPTIONS} OPTION

   switch="${switch} --"

   shift $((${OPTIND}-1))


   echo

   module_path="ifort/modules" ; mkdir --parents -- "${module_path}"
   binary_path="ifort/bin"     ; mkdir --parents -- "${binary_path}" ; binary="${binary_path}/${binary}"

   printf \
"\033[32;1mifort\033[22m -module \"${module_path}\" -fast${parallel} -nowarn -o ${binary}${preprocessor}\
 \033[1mmain/main.F90\033[22m -mkl=${mkl_parallel} -lmkl_blas95_lp64 -lmkl_lapack95_lp64 -lmkl_core -lpthread -lm -ldl\n\n\033[31m"\
 1> >(fold --width=80 --spaces)

   ifort -module "${module_path}" \
         -fast                     \
         ${parallel}                \
         -nowarn                     \
         -o ${binary}                 \
         ${preprocessor} main/main.F90 \
         -mkl=${mkl_parallel}           \
         -lmkl_blas95_lp64               \
         -lmkl_lapack95_lp64              \
         -lmkl_core                        \
         -lpthread                          \
         -lm                                 \
         -ldl 2> >(fold --width=80 --spaces)

   if   [ $? -ne 0 ]
   then

      exit 1

   fi # [ $? -ne 0 ]

   printf "\033[0m"

   if   [[ "${switch}" =~ "--help" ]]
   then

      ${binary} --help

   else

                         simulation_path="${PWD#"/bin"}/data"

                                             basename="${1}"
                input="${simulation_path}/.${basename}.in"
      echo > "${input}"

      echo -e "\033[0m"

      declare -a time_setting ; printf "\033[0mtime_setting: \033[33m" ; read -a time_setting
      declare -a measure_skip ; printf "\033[0mmeasure_skip: \033[33m" ; read -a measure_skip
      declare -a average_step ; printf "\033[0maverage_step: \033[33m" ; read -a average_step

      echo -e "\033[0m"

      declare -a inner_degrees_of_freedom ; printf "\033[0minner_degrees_of_freedom: \033[33m" ; read -a inner_degrees_of_freedom
      declare -a boson_degrees_of_freedom ; printf "\033[0mboson_degrees_of_freedom: \033[33m" ; read -a boson_degrees_of_freedom

      echo -e "\033[0m"

      if   [[ "${switch}" =~ "--mass-deformations" ]]
      then

         declare -a boson_epsilon ; printf "\033[0mboson_epsilon: \033[33m" ; read -a boson_epsilon

         echo -e "\033[0m"

         declare -a boson_mass ; printf "\033[0mboson_mass: \033[33m" ; read -a boson_mass

         echo -e "\033[0m"

         if   [[ "${switch}" =~ "--fermions-included" ]]
         then

            declare -a fermi_mass ; printf "\033[0mfermi_mass: \033[33m" ; read -a fermi_mass

            echo -e "\033[0m"

         fi # [[ "${switch}" =~ "--fermions-included" ]]

      fi # [[ "${switch}" =~ "--mass-deformations" ]]

      if   [[ "${switch}" =~ "--skip-time" ]]
      then

         for    t_time in ${time_setting[@]}
         do

            simulation_path="${simulation_path}.${t_time}"

            for    t_skip in ${measure_skip[@]}
            do

               simulation_path="${simulation_path}.${t_skip}"

               for    t_step in ${average_step[@]}
               do

                  simulation_path="${simulation_path}.${t_step}"

                  for    n_ in ${inner_degrees_of_freedom[@]}
                  do

                     simulation_path="${simulation_path}.${n_}"

                     for    d_ in ${boson_degrees_of_freedom[@]}
                     do

                        simulation_path="${simulation_path}.${d_}"

                        printf "% 15.8e time setting\n" ${t_time}  > "${input}"
                        printf "% 15.8e measure skip\n" ${t_skip} >> "${input}"
                        printf "% 15.8e average step\n" ${t_step} >> "${input}"

                        printf "% 11i     inner_degrees_of_freedom\n" ${n_} >> "${input}"
                        printf "% 11i     boson_degrees_of_freedom\n" ${d_} >> "${input}"

                        if   [[ "${switch}" =~ "--mass-deformations" ]]
                        then

                           for    b_mass in ${boson_epsilon[@]}
                           do

                              printf "  ${b_mass}  boson_epsilon" >> "${input}"

                              for    mu in ${boson_degrees_of_freedom[d_]}
                              do

                                 printf "  ${boson_mass[mu]}  boson mass ${mu}" >> "${input}"

                              done # mu in ${boson_degrees_of_freedom[d_]}

                              if   [[ "${switch}" =~ "--fermions-included" ]]
                              then

                                 for    f_mass in ${boson_epsilon[@]}
                                 do

                                    printf "  ${f_mass}  fermi_mass" >> "${input}"

                                    ${binary} ${switch} ${basename} < ${input}

                                 done # f_mass in ${boson_epsilon[@]}

                              # else

                                 ${binary} ${switch} ${basename} < ${input}

                              fi # [[ "${switch}" =~ "--fermions-included" ]]

                           done # b_mass in ${boson_epsilon[@]}

                        # else

                           ${binary} ${switch} ${basename} < ${input}

                        fi # [[ "${switch}" =~ "--mass-deformations" ]]

                     done # d_ in ${boson_degrees_of_freedom[@]}

                  done # n_ in ${inner_degrees_of_freedom[@]}

               done # t_step in ${average_step[@]}

            done # t_skip in ${measure_skip[@]}

         done # t_time in ${time_setting[@]}

      else

         for    t_time in ${time_setting[@]}
         do

            simulation_path="${simulation_path}.${t_time}"

            for    t_step in ${average_step[@]}
            do

               simulation_path="${simulation_path}.${t_step}"

               for    n_ in ${inner_degrees_of_freedom[@]}
               do

                  simulation_path="${simulation_path}.${n_}"

                  for    d_ in ${boson_degrees_of_freedom[@]}
                  do

                     simulation_path="${simulation_path}.${d_}"

                     printf "% 15.8e time setting\n" ${t_time}  > "${input}"
                     printf "% 15.8e average step\n" ${t_step} >> "${input}"

                     printf "% 11i     inner_degrees_of_freedom\n" ${n_} >> "${input}"
                     printf "% 11i     boson_degrees_of_freedom\n" ${d_} >> "${input}"

                     if   [[ "${switch}" =~ "--mass-deformations" ]]
                     then

                        for    b_mass in ${boson_epsilon[@]}
                        do

                           printf "  ${b_mass}  boson_epsilon" >> "${input}"

                           for    mu in ${boson_degrees_of_freedom[d_]}
                           do

                              printf "  ${boson_mass[mu]}  boson mass ${mu}" >> "${input}"

                           done # mu in ${boson_degrees_of_freedom[d_]}

                           if   [[ "${switch}" =~ "--fermions-included" ]]
                           then

                              for    f_mass in ${boson_epsilon[@]}
                              do

                                 printf "  ${f_mass}  fermi_mass" >> "${input}"

                                 ${binary} ${switch} ${basename} < ${input}

                              done # f_mass in ${boson_epsilon[@]}

                           else

                              ${binary} ${switch} ${basename} < ${input}

                           fi # [[ "${switch}" =~ "--fermions-included" ]]

                        done # b_mass in ${boson_epsilon[@]}

                     # else

                        ${binary} ${switch} ${basename} < ${input}

                     fi # [[ "${switch}" =~ "--mass-deformations" ]]

                  done # d_ in ${boson_degrees_of_freedom[@]}

               done # n_ in ${inner_degrees_of_freedom[@]}

            done # t_step in ${average_step[@]}

         done # t_time in ${time_setting[@]}

      if # [[ "${switch}" =~ "--skip-time" ]]

   fi # [[ "${switch}" =~ "--help" ]]

