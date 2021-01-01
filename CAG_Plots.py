# -*- coding: utf-8 -*-
"""
Plotting Climate At a Glance.
"""
import pandas as pd
import matplotlib.pyplot as plt
from scipy.stats import linregress
from scipy.stats import stats

def read_file(region):
    print(region)
    while True:
        try:
            precipitation = pd.read_csv('E:\School\Thesis\Climate At a Glance\Data\%s Precipitation.csv'%(region))
            precipitation['Date'] =precipitation['Date'].astype(str)
            precipitation['Date']=precipitation['Date'].str[0:4]
            precipitation['Date'] = precipitation['Date'].astype(int)
            
            temperature = pd.read_csv('E:\School\Thesis\Climate At a Glance\Data\%s Temperature.csv'%(region))
            temperature['Date'] =temperature['Date'].astype(str)
            temperature['Date']=temperature['Date'].str[0:4] 
            temperature['Date'] =temperature['Date'].astype(int)

            return precipitation, temperature

        except:
            print("Problem Finding File. Exiting")
            break
        
def stats(df):
    x = df['Date']
    y = df['Value']
    slope, intercept, r, p, stderr = linregress(x, y)
    regress = slope*x + intercept
    decadal_change = round(slope*10,2)
    
    df['regress'] = regress
    #df = df.set_index('Date')
    return df,decadal_change
      
def plot(prcp,temp,region):
    fig,axes = plt.subplots(2,1,constrained_layout=True)

    prcp, decadal_change = stats(prcp)    
    prcp.plot(x='Date',y='Value',color='green',ax=axes[0],label='_nolegend_',xlim=(1966,2018),grid=True,xticks=(range(1966,2018,10)))
    prcp.plot(x='Date',y='regress',ax=axes[0],color="black",label = "1966-2018 \n %s in/Decade"%decadal_change,grid=True)
    ylim = axes[0].get_ylim()
    ymin = round(ylim[0],0)
    ymax = round(ylim[1],0)+2
    axes[0].set_ylim(ymin,ymax)
    axes[0].set_yticks(range(int(ymin),int(ymax),int((ymax-ymin)/4)))    
    axes[0].set_title("%s: Precipitation (November-April)"%region,loc="left",fontsize=10)    
    axes[0].legend(loc=2, bbox_to_anchor=(0.7, 1.35),fontsize=8)
  
    axes[0].set_ylabel("Precipitation (in)")


    temp, decadal_change = stats(temp)        
    temp.plot(x='Date',y='Value',color='orange',ax=axes[1],xlim=(1966,2018),label='_nolegend_',grid=True,xticks=(range(1966,2018,10)))
    temp.plot(x='Date',y='regress',ax=axes[1],color="black",label = "1966-2018 \n %s °F/Decade"%decadal_change,grid=True)
    ylim = axes[1].get_ylim()
    ymin = round(ylim[0],0)
    ymax = round(ylim[1],0)+2
    axes[1].set_ylim(ymin,ymax)
    axes[1].set_yticks(range(int(ymin),int(ymax),int((ymax-ymin)/4)))    
    print(ylim)
    axes[1].set_title("%s: Temperature (November-April)"%region,loc="left",fontsize=10)   
    axes[1].legend(loc=2, bbox_to_anchor=(0.7, 1.35),fontsize=8)

    axes[1].set_ylabel("Temperature (°F)")
    
    savefig(region)
    
    
def savefig(region):    
    plt.savefig("E:\School\Thesis\Climate At a Glance\Plots\%s.jpg" %(region),dpi=600,bbox_inches='tight')  # Need to vary this formatting between graphics.
    plt.show()
    
    print("Plot Saved")
    print()
        
def main():
    regions = ["Northern Rockies","South", "Upper Midwest"]
    
    for region in regions:
        prcp,temp= read_file(region)
        plot(prcp,temp,region)
        


if __name__ == "__main__":
    main()      