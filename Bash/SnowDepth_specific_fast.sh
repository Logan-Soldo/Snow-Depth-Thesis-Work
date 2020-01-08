#!/bin/bash
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
 

 pgm=SnowDepth_extract

# set -x

##### Home Directory ######
datadir=/mnt/e/School/Thesis/SnowDepth_Data/snwd
outputdir=/mnt/e/School/Thesis/SnowDepth_Data/Outputs

##### School Directory ######
#datadir=/mnt/u/Research/SnowDepth_Data/snwd
#outputdir=/mnt/u/Research/SnowDepth_Data/Outputs

start_time=$(date +%s)
echo $(date)

 list="2741 2740 2739 2738 2737 2641 2640 2639 2638 2541 2540 2539 2538 2441 2440 2439 2438 2341 2340 2339  2241 2240 2141" 
 # files=$datadir/*.txt 
# # IFS=' '


    for string in $list
	 do
	  # do
	  i=${string:0:2}
	  j=${string:2:2}
		# for i in ${list:0:2}
			# do
			# for j in ${list:2:2}
				# do 
	

#		 mkdir -p /mnt/e/School/Thesis/Python_Code/Snowfall_Data/1960s/Outputs
		 mkdir -p $outputdir/$i$j/
	 done


			
#		 for filein in `ls -d $datadir/*.txt`
		   

	#	   do
#			  filename=`basename $filein|sed 's/\.txt//'`
			fileout=$outputdir/
			echo $fileout
   
   
# This function will find a matching i and j then extract to file.
awkarray=$(
  # substitute space for newline
  <<<$list tr ' ' '\n' | 
  # remove empty newlines
  sed '/^$/d' | 
  # add 'a[' to the beginning of the line
  # and add ']' to the ending of the line
  # so we have a[number] a[anothernumber] etc.
  sed 's/^/a[/; s/$/]/'
)
awk -vfileout="$fileout" '
  BEGIN {'"$awkarray"'}
  $3 $2 in a { 
    print $0 > fileout $3 "_" $2 "_SnowDepth.txt"
  }
' "$datadir"/*.txt


for string in $list	
do
	 i=${string:0:2}
	 j=${string:2:2}
	mv /$outputdir/${i}_${j}_SnowDepth.txt /$outputdir/$i$j/${i}_${j}_SnowDepth.txt
done
		
#		cat $outputdir/$i$j/*.txt > $outputdir/$i$j/${i}${j}_Snowfall.txt
		
	#	echo ${i}${j}_SnowDepth.txt
		
	  if [ $? -ne 0 ]; then
		echo "$run finished with error"
		  exit
	  fi
   
		echo "Moving to next file..."
		echo
	  
		   




#rm -f data1.bin
end_time=$(date +%s)
runtime=$((($(date +%s)-$start_time)/60))
echo "END OF SCRIPT at $(date) in $runtime minutes"
#echo "Moving to next decade..."
