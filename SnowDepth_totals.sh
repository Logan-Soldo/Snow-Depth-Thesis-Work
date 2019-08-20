#!/bin/bash
# This program is connected to the SnowDepth_totals.f90 program.
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
#8.  Mean snow depth of stations reporting snowfall in cell (cm)
#9.  Median snow depth of stations reporting snowfall in cell (cm)
#10. Number of stations reporting in file
#11. Number of stations reporting > 10cm snowfall in cell
#12. Number of stations reporting > 25cm snowfall in cell
#13. Number of stations reporting > 50cm snowfall in cell
#14. Number of stations reporting > 100cm snowfall in cell
#15. NOAA cell area
#16. NOAA snow cover flag (1=snow covered) (not included)
 

 pgm=SnowDepth_totals.exe

 set -x

##### Home Directory ######
#datadir=/mnt/e/School/Thesis/SnowDepth_Data/Outputs
#outputdir=/mnt/e/School/Thesis/SnowDepth_Data/Outputs

##### School Directory ######
datadir=/mnt/u/Research/SnowDepth_Data/Outputs
outputdir=/mnt/u/Research/SnowDepth_Data/Outputs


start_time=$(date +%s)
#echo $start_time

 
#list="2741 2740 2739 2738 2737 2641 2640 2639 2638 2541 2540 2539 2538 2441 2440 2439 2438 2341 2340 2339  2241 2240 2141"
list="2340 2439 2440 2540 2541 2641 2739 2740 2741 2737 2638 2539 2640 2341 2538"  # missing greater than 25% of data.

for i in {12..27}
do
	for j in {33..41}
		
		do 
		 if [[ $list =~ $i$j ]]; then
			 continue 							# If the number is in the list of cells not being used then it will skip this cell and proceed to the next cell.
		 fi



#		rm -r $datadir/$i$j/SnowDepth
		mkdir -p $datadir/$i$j/SnowDepth
		mkdir -p $datadir/All/PercentMiss/
			mkdir -p $datadir/$i$j/SnowDepth/Junk




    
 for filein in `ls -d $datadir/$i$j/${i}_${j}_SnowDepth.txt`
   

   do
      filename=`basename $filein|sed 's/\.txt//'`
	fileout=$outputdir/$i$j/SnowDepth/
	fileout2=$outputdir/All/PercentMiss/
	fileout3=$outputdir/$i$j/SnowDepth/Junk/

    echo $filename
	echo $fileout
	
	#      create the namelist file
      cat <<EOF > cdh.nml
&cdh_nml
inputfile="${filein}",
outputfile="${fileout}${i}${j}_SplitDate.txt",
outputfile2="${fileout}${i}${j}_CountDate.txt",
outputfile3="${fileout}${i}${j}_MeanSnowDepth.txt",
outputfile4="${fileout}${i}${j}_SDQuality.txt",
outputfile5="${fileout2}${i}${j}_PercentMiss.txt",
outputfile6="${fileout}${i}${j}_DaySnowDepth.txt",
outputfile7="${fileout}${i}${j}_SeasonalSnowDepth.txt",
outputfile8="${fileout3}${i}${j}_Junk.txt",
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
	 

	 # for ij in 2741 2740 2739 2738 2737 2641 2640 2639 2638 2541 2540 2539 2538 2441 2440 2439 2438 2341 2340 2339  2241 2240 2141
		 # do
			 # rm -r $datadir/$ij/SnowDepth/
		 # done

	rm $outputdir/All/PercentMiss/AllPercent_missing.txt
	cat $outputdir/All/PercentMiss/*.txt >> $outputdir/All/PercentMiss/AllPercent_missing.txt
    rm $outputdir/All/PercentMiss/*_PercentMiss.txt
	

#rm -f data1.bin
end_time=$(date +%s)
runtime=$((($(date +%s)-$start_time)/60))
echo "END OF SCRIPT at $(date) in $runtime minutes"