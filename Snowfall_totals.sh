#!/bin/bash
# This program is connected to the Snowfall_totals.f90 program.
#
# To access the remote drive use the command below:
# sudo mount -t drvfs u: /mnt/u 
   
################################################################
#1.  Date
#2.  j coordinate in 89x89 grid
#3.  i coordinate in 89x89 grid
#4.  Longitude of 89x89 cell (decimal degrees)
#5.  Latitude of 89x89 cell (decimal degrees)
#6.  Maximum elevation of stations reporting snowfall in cell (m)
#7.  Minimum elevation of stations reporting snowfall in cell (m)
#8.  Mean snow depth of stations reporting snowfall in cell (mm)
#9.  Median snow depth of stations reporting snowfall in cell (mm)
#10. Number of stations reporting in file
#11. Number of stations reporting > 10mm snowfall in cell
#12. Number of stations reporting > 25mm snowfall in cell
#13. Number of stations reporting > 50mm snowfall in cell
#14. Number of stations reporting > 100mm snowfall in cell
#15. NOAA cell area
#16. NOAA snow cover flag (1=snow covered) (not included)
 

 pgm=Snowfall_total.exe

# set -x

##### Home Directory ######
#datadir=/mnt/e/School/Thesis/SnowDepth_Data/Outputs
#outputdir=/mnt/e/School/Thesis/SnowDepth_Data/Outputs

##### School Directory ######
datadir=/mnt/u/Research/Snowfall_data/Outputs
outputdir=/mnt/u/Research/Snowfall_data/Outputs

start_time=$(date +%s)
#echo $(date)

 

for i in {12..27}
do
	for j in {33..41}
		
		do 
		 if [[ $list =~ $i$j ]]; then
			 continue 							# If the number is in the list of cells not being used then it will skip this cell and proceed to the next cell.
		 fi

	mkdir -p $datadir/$i$j/Snowfall
		mkdir -p $datadir/$i$j/Snowfall/Junk




    
 for filein in `ls -d $datadir/$i$j/${i}_${j}_Snowfall.txt`
   

   do
      filename=`basename $filein|sed 's/\.txt//'`
	fileout=$outputdir/$i$j/Snowfall/
	fileout2=$outputdir/$i$j/Snowfall/Junk/

    echo $filename
	echo $fileout
	
	#      create the namelist file
      cat <<EOF > cdh.nml
&cdh_nml
inputfile="${filein}",
outputfile="${fileout}${i}${j}_SplitDate.txt",
outputfile2="${fileout}${i}${j}_SnwfCountDate.txt",
outputfile3="${fileout}${i}${j}_TotalSnowfall.txt",
outputfile4="${fileout}${i}${j}_DaySnowfall.txt",
outputfile5="${fileout}${i}${j}_SeasonalSnowfall.txt",
outputfile6="${fileout2}${i}${j}_Junk.txt",
/
EOF

      #run the excutable
     ./$pgm
      
      if [ $? -ne 0 ]; then
        echo "$run finished with error"
          exit
      fi
   
	  
           
		 done
      done
	 done


#rm -f data1.bin
end_time=$(date +%s)
runtime=$((($(date +%s)-$start_time)/60))
echo "END OF SCRIPT at $(date) in $runtime minutes"