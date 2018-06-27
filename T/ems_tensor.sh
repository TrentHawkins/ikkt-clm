#!/bin/bash --


   filename="$(basename -- ${0})"
   basename="${filename%.sh}"

   overwrite=0

   while  getopts :f option
   do

      case   ${option} in

      f)

         overwrite=1

         ;;

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

   if   [ ${argc} = 0 ]
   then

      echo "${basename}: aborting with ${argc} source directories to process".${node}.out
      exit 1

   fi # [ ${argc} = 0 ].${node}.out

   argv=${@}

   fermion_masses=(0.10  0.20  0.30  0.40  0.50  0.60  0.70  0.80  0.90  1.00  1.10  1.20 )
   ssb_parameters=(0.020 0.040 0.060 0.080 0.100 0.120 0.140 0.160 0.180 0.200 0.220 0.240)

   dimension=10

   begin=1
     end=120

   for    data_directory in ${argv}
   do

      if   [ ! -d "${data_directory}" ]
      then

         echo "${basename}: ${data_directory}: skipping source directory that does not exist"
         continue

      fi # [ ! -d "${data_directory}" ]

                data_directory="${data_directory%/}"
      header="${data_directory}/${data_directory##*/}"

      for    a in ${fermion_masses[@]}
      do

         for    em in ${ssb_parameters[@]}
         do

            for    ((node=${begin};node<=${end};++node))
            do

               collated_data_filename=${header}a${a}e${em}.${node}.ems.out

               if   [ -f ${collated_data_filename} ] && [ ${overwrite} = 0 ]
               then

                  printf   "\033[33mskipped\033[0m ${data_filename##*/} into \033[1m${collated_data_filename##*/}\033[0m"
                  continue

               fi # [ -f ${collated_data_filename} ] && [ ${overwrite} = 0 ]

#              echo  "${collated_data_filename}"
               echo > ${collated_data_filename}

#                                   ls ${header}a${a}e${em}_d*.${node}.out
               for    data_filename in ${header}a${a}e${em}_d*.${node}.out
               do

                  if   [ ! -f ${data_filename} ]
                  then

                     rm ${collated_data_filename}
                     break

                  fi # [ ! -f ${data_filename} ]

                  printf   "\033[31mwriting\033[0m ${data_filename##*/} into \033[1m${collated_data_filename##*/}\033[0m"

                  ( grep -v "^#" ${data_filename} | grep "^TMN " ${data_filename} | cut -c 4- ) >> ${collated_data_filename}

                  printf "\r\033[32mwritten\033[0m ${data_filename##*/} into \033[1m${collated_data_filename##*/}\033[0m\n"

               done # data_filename in ${header}a${a}e${em}_d*.${node}.out

            done # ((node=${begin};node<=${end};++node))

         done # em in ${ssb_parameters[@]}

      done # a in ${fermion_masses[@]}

   done # data_directory in ${argv}
