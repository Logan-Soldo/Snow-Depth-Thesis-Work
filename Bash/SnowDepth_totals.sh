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
#8.  Mean snow depth of stations reporting snowfall in cell (mm)
#9.  Median snow depth of stations reporting snowfall in cell (mm)
#10. Number of stations reporting in file
#11. Number of stations reporting > 10mm snowfall in cell
#12. Number of stations reporting > 25mm snowfall in cell
#13. Number of stations reporting > 50mm snowfall in cell
#14. Number of stations reporting > 100mm snowfall in cell
#15. NOAA cell area
#16. NOAA snow cover flag (1=snow covered) (not included)
 

 pgm=SnowDepth_totals.exe

 #set -x

##### Home Directory ######
#datadir=/mnt/e/School/Thesis/SnowDepth_Data/Outputs
#outputdir=/mnt/e/School/Thesis/SnowDepth_Data/Outputs

##### School Directory ######
datadir=/mnt/u/Research/SnowDepth_Data/Outputs
datadir2=/mnt/u/Research/Snowfall_Data/Outputs
outputdir=/mnt/u/Research/SnowDepth_Data/Outputs


start_time=$(date +%s)
echo $start_time

 
Blacklist="2741 2740 2739 2738 2737 2736 2641 2640 2639 2638 2637 2636 2541 2540 2539 2538 2441 2440 2439 2438 2341 2340 2339 2241 2240 2141" 

for i in {12..27}
do
	for j in {33..41}
		
		do 
		 if [[ $Blacklist =~ $i$j ]]; then
			 continue 							# If the number is in the list of cells not being used then it will skip this cell and proceed to the next cell.
		 fi
# for i in 26
	# do 
		# for j in 36
		# do


#		rm -r $datadir/$i$j/SnowDepth
		mkdir -p $datadir/$i$j/SnowDepth
		mkdir -p $datadir/All/PercentMiss/
			mkdir -p $datadir/$i$j/SnowDepth/Junk




    
 for filein in `ls -d $datadir/$i$j/SnowDepth/${i}${j}_CountDate.txt`
 do
 for filein2 in `ls -d $datadir2/$i$j/Snowfall/${i}${j}_SnwfCountDate.txt`
   

   do
      filename=`basename $filein|sed 's/\.txt//'`
	  filename2=`basename $filein2|sed 's/\.txt//'`
	fileout=$outputdir/$i$j/SnowDepth/
	fileout2=$outputdir/All/PercentMiss/
	fileout3=$outputdir/$i$j/SnowDepth/Junk/

    echo $filename
	echo $fileout
	
	#      create the namelist file
      cat <<EOF > cdh.nml
&cdh_nml
inputfile="${filein}",
inputfile2="${filein2}",
outputfile3="${fileout}${i}${j}_MeanSnowDepth.txt",
outputfile4="${fileout}${i}${j}_76SnowDepth.txt",
outputfile5="${fileout}${i}${j}_SDQuality.txt",
outputfile6="${fileout}${i}${j}_MonthAboveX.txt",
outputfile7="${fileout}${i}${j}_DaySnowDepth.txt",
outputfile8="${fileout}${i}${j}_MonthlyAverage.txt",
outputfile9="${fileout}${i}${j}_SeasonalSnowDepth.txt",
outputfile10="${fileout2}${i}${j}_PercentMiss.txt",
outputfile11="${fileout2}${i}${j}_PercentDepth.txt",
outputfile12="${fileout2}${i}${j}_PercentMonthlySD.txt",
outputfile13="${fileout2}${i}${j}_PercentMonthy76.txt",
outputfile14="${fileout3}${i}${j}_Junk.txt",
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
	done
	 

	 # for ij in 2741 2740 2739 2738 2737 2641 2640 2639 2638 2541 2540 2539 2538 2441 2440 2439 2438 2341 2340 2339  2241 2240 2141
		 # do
			 # rm -r $datadir/$ij/SnowDepth/
		 # done
		 
	echo "Concatenating files and removing residuals"
	
	rm -r $outputdir/All/PercentMiss/All*.txt
	
	cat $outputdir/All/PercentMiss/*_PercentMiss.txt >> $outputdir/All/PercentMiss/AllPercent_missing.txt
	cat $outputdir/All/PercentMiss/*_PercentDepth.txt >> $outputdir/All/PercentMiss/AllPercent_Depth.txt
	cat $outputdir/All/PercentMiss/*_PercentMonthlySD.txt >> $outputdir/All/PercentMiss/AllPercent_MonthlySD.txt
	cat $outputdir/All/PercentMiss/*_PercentMonthy76.txt >> $outputdir/All/PercentMiss/AllPercent_Monthly76.txt
   
    rm $outputdir/All/PercentMiss/*_PercentMiss.txt
    rm $outputdir/All/PercentMiss/*_PercentDepth.txt
	rm $outputdir/All/PercentMiss/*_PercentMonthlySD.txt
	rm $outputdir/All/PercentMiss/*_PercentMonthy76.txt

#rm -f data1.bin

end_time=$(date +%s)
runtime=$((($(date +%s)-$start_time)/60))
echo "END OF SCRIPT at $(date) in $runtime minutes"
