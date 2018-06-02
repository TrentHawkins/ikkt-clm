#!/bin/bash --


   filename="$(basename -- ${0})"
   basename="${filename%.sh}"

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

   argc=${#}

   if   [ ${argc} -eq 0 ]
   then

      echo "${basename}: aborting with ${argc} source directories to process"
      exit 1

   fi # [ ${argc} -eq 0 ]

   argv=${@}

   fermion_masses=(0.30 0.50 0.70 0.90 1.00 1.10)

   spontaneous_symmetry_breaking_parameters=(0.020 0.040 0.060 0.080 0.100 0.120 0.140 0.160 0.180 0.200)

   for    data_directory in ${argv}
   do

      if   [ ! -d "${data_directory}" ]
      then

         echo "${basename}: ${data_directory}: skipping source directory that does not exist"
         continue

      fi # [ ! -d "${data_directory}" ]

                               header="${data_directory}"
#     data_directory="$(pwd)/${header}"

      for    a in ${fermion_masses[@]}
      do

         for    em in ${spontaneous_symmetry_breaking_parameters[@]}
         do

            for    node in {1..60}
            do

               collated_data_filename=${header}a${a}e${em}_ems.${node}.out

               echo > ${collated_data_filename}

               for    data_filename in ${header}a${a}e${em}_d*.${node}.out

                  if   [ ! -f ${data_filename} ]
                  then

                     rm ${collated_data_filename}
                     break

                  fi # [ ! -f ${data_filename} ]

                  ( grep -v "^#" ${data_filename} | grep "^TMN " ${data_filename} | cut -c 4- ) >> ${collated_data_filename}

               done # data_filename in ${header}a${a}e${em}_d*.${node}.out

            done # node in {1..60}

         done # em in ${spontaneous_symmetry_breaking_parameters[@]}

      done # a in ${fermion_masses[@]}

   done # data_directory in ${argv}
