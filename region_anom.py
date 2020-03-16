import pandas as pd


data = pd.read_fwf("U:\Research\SnowDepth_Data\Outputs\All\PercentMiss\AllPercent_Decade76.txt",usecols = [0,1,2,3,9,10,11,12,13,14,15])

print(data)