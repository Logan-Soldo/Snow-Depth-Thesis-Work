import numpy as np
import matplotlib.dates
import matplotlib.pyplot as plt
#import matplotlib.dates
from datetime import datetime
from matplotlib.ticker import AutoMinorLocator
import pandas as pd
import math
from scipy.interpolate import interp1d as interp
from scipy.stats import linregress
from scipy.stats import stats


def read_file(CellList,filelist,region_select,input_location):
    '''
    Here is where the files are read and added to a list to be analyzed. 
    The output "celllist" is produced depending on the inputs by the user 
    and the files that they select.
    '''
    while True:
        filelist = []
        if input_location == "1":
            list_files = ["76SnowDepth.txt","DaySnowDepth.txt","MeanSnowDepth.txt"
                          ,"MonthlyAverage.txt","SDQuality.txt","SeasonalSnowDepth.txt",
                          "MonthAboveX.txt"]
            print(list_files)
            file = input("Input a file name: ")               
            try:   
                if region_select == "5":
                    for i in CellList:
                        temp_list = []
                        for cell in i:
                            flist = open("U:\Research\SnowDepth_Data\Outputs\%s\SnowDepth\%s_%s" %(cell,cell,file))
                            temp_list.append(flist)
                        filelist.append(temp_list)
                else:
                    for cell in CellList:            
                        open("U:\Research\SnowDepth_Data\Outputs\%s\SnowDepth\%s_%s" %(cell,cell,file))
                        filelist.append("U:\Research\SnowDepth_Data\Outputs\%s\SnowDepth\%s_%s" %(cell,cell,file))
                return file,filelist
            except FileNotFoundError:
                print("file doesn't exist")
        elif input_location == "2":
            list_files = ["AllPercent_Decade76.txt","AllPercent_Depth.txt","AllPercent_missing.txt","AllPercent_Monthly76.txt","AllPercent_MonthlySD.txt"]
            print(list_files)
            file = input("Input a file name: ") 
            try:
                filelist = pd.read_fwf("U:\Research\SnowDepth_Data\Outputs\All\PercentMiss\%s" %(file),header=None)                  
                return file,filelist
            except:
                print("file doesn't exist")
                break
        else:
            print("try again")

def region_define(region_select):
    '''
    This section selects cells only with the region selected by user.
    '''
    CellList = []
    reg_file = pd.read_fwf("U:\Research\SnowDepth_Data\Outputs\All\PercentMiss\AllPercent_Depth.txt",header=None)

    sort_region = reg_file.loc[reg_file[11] == int(region_select)]
#    cell_reg = sort_region[[0,1,11]]
    cell_reg = sort_region[0].astype(str) + sort_region[1].astype(str)  
    CellList = cell_reg.tolist()
    
    return CellList

def all_region(region_select):
    '''
    Reads in a file containing regions and sorts based on the regions. 
    Each identifier is split into a list of regions.
    '''
    CellList = []
    reg_file = pd.read_fwf("U:\Research\SnowDepth_Data\Outputs\All\PercentMiss\AllPercent_Depth.txt",header=None)
    
    for i in range(4):
        sort_region = reg_file.loc[reg_file[11] == int(i+1)]
        cell_reg = sort_region[0].astype(str) +sort_region[1].astype(str)
        datalist = cell_reg.tolist()
        CellList.append(datalist)
    return CellList
        
    
def get_header(filelist,region_select):
    '''
    displays the headers from files and returns the header as well as list of files.
    '''
    header=[]
    if region_select == "5":
    
        for f in filelist:
              #  print(f)
              for i in f:
                line = i.readline()
                split_line = line.split()
    else:
        for f in filelist:
            f_open= open(f)
            line = f_open.readline()
        header=[]
        split_line = line.split()
        for l in line:
            header.append(split_line[1])
        header = header[0]
        f_open.close()
    return header,line,split_line


def pick_var(var):
    '''
    Selects the variables from the header of the text file input.
    '''
    while True:
        input4 = input("Select from the list of columns: ")             
        if input4 in var:
            head = input4
            var_index = var.index(head) 
            return head,var_index                  
        else:
             print("variable does not exist, try again")       
        
def row_file(Blacklist,x1,x2):
    '''
    Splits and organizes files in range of input rows.    
    '''
    row_list = []   
    for ij in range(int(x1),int(x2)+1):
         if str(ij) in str(Blacklist):
            continue
         else:
            row_list.append(str(ij)) 
    return row_list

def col_file(Blacklist,y1,y2):
    '''
    Splits and organizes files in range of input columns.
    '''
    col_list = []   
    for ij in range(int(y1),int(y2)+1,100):
         if str(ij) in str(Blacklist):
            continue
         else:
            col_list.append(str(ij)) 
    return col_list        

def total_file(Blacklist):
    '''
    Splits and organizes files in range of the entire study area.
    '''
    CellList = []
    for i in range(12,28):
        for j in range(33,42):
            if (str(i)+str(j)) in str(Blacklist):
                continue
            else:
                CellList.append(str(i)+str(j))
    return CellList

def title_gen(input3,region):
    '''
    Function to generate titles and figure names based on inputs by user.
    '''
    if input3 == "MonthlyAverage.txt":
        title = "Average Monthly Sum of Snow Depth: Region %s" %(region)
        savefig = "Monthly\R%sMonthlyDepth" %(region)
    elif input3 == "MonthAboveX.txt":
        title = "Sum of Days Above 2.54cm: Region %s" %(region)
        savefig = "SOD\SOD_R%s" %(region)
    elif input3 == "DaySnowDepth.txt":
        title = "Average Daily Snow Depth: 1966-2018"
        savefig = "DayDepth"
    elif input3 == "76SnowDepth.txt":
        title = "Season Length: Region %s" %(region)
        savefig = "SeasonLength\SL%s" %(region)

    xaxis = "test"
    yaxis = "test"
    return title,yaxis,xaxis,savefig


######## Below this are all of the plotting functions. ##########
def Set_Month(filelist,header,input3,selection,region_select):               # Use this example for global plotting application
    '''
    Introducing subplotting here as a demo.
    '''
    data_list = []
    for fname in filelist:
        try:
            data= np.loadtxt((open(fname).readlines()[:-1]), skiprows=1, dtype=None)  # Coming out of loop too early, need to build a list of lists.
            data_list.append(data)
        except IOError:
            continue

    title,yaxis,xaxis,figname = plt_select1(selection,header,input3,region_select)
    if input3 != "MonthAboveX.txt":
        yaxis = "Sum of Snow\nDepth(cm)"
        xaxis = "Year"
    else:
        yaxis = "Days"
        xaxis = "Year"
    Month_plot(title,data_list,header,yaxis,xaxis,input3,region_select)
    return figname
    
def Plot76(filelist,header,plt_index,selection,region_select,input3):           # Added global plotting     
    '''
    Work in progress. This contains old code that has not been updated to the 
    more recent method of plotting. Should still work if needed.
    '''
    y_list = []
    for fname in filelist:
        try:
            data=np.loadtxt(fname,skiprows=1)
           # plt.title(header)
            x = data[:,0]
            y_temp = data[:,plt_index]
            y_list.append(y_temp)
      #      plt.plot(x,y,label=fname[35:39])
        except IOError:
            continue
    
    title,yaxis,xaxis,figname = plt_select1(selection,header,input3,region_select)
    yaxis = "Days"
    xaxis = "Year"
    Gen_plot(x,y_list,selection,header,region_select,xaxis,yaxis,title)
    return figname

def Day_mean(filelist,header,plt_index,selection,region_select,input3):                  # Added global plotting 
    '''
    This function takes in the files requested by the user and creates of an
    array for all the files. The title and axis are hardcoded in here to speed
    up the process.
    '''
    y_list = []
    for fname in filelist:
        data_list = []
        try:
            for f in fname:
                data=np.loadtxt(f,skiprows=0)
                data_list.append(data)
            print(len(data_list))
            y_list.append(data_list)
        except IOError:
            continue
    title,yaxis,xaxis,figname = plt_select1(selection,header,input3,region_select)
    yaxis = "Snow Depth(cm)"
    xaxis = "Day of Year"
    Day_plot(title,y_list,selection,header,region_select,xaxis,yaxis)
    return figname
    
def plt_select1(selection,header,input3,region_select):
    '''
    This function determnes whether a title will be generated. For testing 
    purposes, 'n' is input to produce a generic title and file name that will 
    be overwritten every time.
    '''
    fname_lst = []
    generate_title = input("Would you like to generate a title? (y/n): ")    
    if generate_title == "n":
        if selection == "all": 
            title = header + " "+ selection + " " + "Cells"
            yaxis = "test"
            xaxis = "test"            
            figname = "test"
        elif selection == "region":
             title = header + " "+ "Region" + " " + region_select 
             yaxis = "test"
             xaxis = "test"             
             figname = "test"
        else:
            title = header + " "+ selection + " " + "%s to %s" %(fname_lst[0],fname_lst[-1])
            yaxis = "test"
            xaxis = "test"
            figname = "test"
    else:
        title,yaxis,xaxis,figname = title_gen(input3,region_select)
    

    plt.rc('font', family='serif')  
    plt.rc('xtick', labelsize='x-small')
    plt.rc('ytick', labelsize='x-small')

    return title,yaxis,xaxis,figname    
 
def Month_plot(title,data_list,header,yaxis,xaxis,input3,region):
    '''
    Used for plotting Monthly Data, use this as an example for future
    figures that will require subplots.
    '''
    fig,sub =  plt.subplots(3, 2, figsize=(12,8),constrained_layout=True)
#    plt.figure(figsize=(12,8)) 
    fig.suptitle(title,fontsize=12)                    # Plotting title from above.
    #fig.autoscale()
    
    c = color_select()
    for i in range(len(c)):
        r,g,b = c[i]
        c[i] = (r / 255., g / 255., b / 255.)
        
    months = ["November","December","January","February","March","April"]
    sub = sub.ravel()  
 #   sub = fig.add_subplot(221)
    if region == "1":
        color = c[0]
    elif region == "2":
        color = c[1]
    elif region == "3":
        color = c[2]
        #color = "black"
    elif region == "4":
        color = c[3]
    
    print("% Change","CPD","APC","P-Value")    
    for i in range(6):
        y_list = []
        y=[]
        ymin = []
        ymax= []
        for j in data_list:
            x = j[:,0]
            y_temp = j[:,i+1]          # plt_index gets its index when variable is chosen by user.
            y_list.append(y_temp)
            y_list = np.array(y_list).tolist()
            
        y = np.mean(y_list,axis=0)
        ymin = np.min(y_list,axis=0)
        ymax = np.max(y_list,axis=0)
        per_90 = np.percentile(y_list,90,axis=0)
        per_10 = np.percentile(y_list,10,axis=0)
        per75 = np.percentile(y_list,75,axis=0)
        per25 = np.percentile(y_list,25,axis=0)
        
        #print(y)
        regress = linregress(x,y)
        lin_m = regress.slope
   #     ROC = (lin_m/np.mean(y))*100   # Likely not the proper method.
           
        lin_b = regress.intercept
#            lin_r = stats.pearsonr(x,y)
        lin_r = regress.rvalue
        r_round = round(lin_r,3)
        lin_p = regress.pvalue
#        print(lin_m, lin_b,lin_r,lin_p)
#
        y_2018 = (lin_m*2018) + lin_b
        y_1966 = (lin_m*1966) + lin_b
        PerROC = (((y_2018 - y_1966)/y_1966) * 100)/5.2       # Percent Change start to end
        CPD = lin_m * 10                                # Change per Decade
        APC = 100* (math.exp(lin_m)-1)
        
#            m_round = round(m,3)            
        print('{:.4}'.format(PerROC)+'%','{:.4}'.format(CPD),'{:.4}'.format(APC),'{:.4}'.format(lin_p))
  #      print(PerROC)        
        sub[i].set_xlim(1966,2017)
        if input3 != "MonthAboveX.txt":
            sub[i].set_ylim(bottom=0,top=(max(per_90)))
           # sub[i].set_yticks(range(0,int(max(per_90) + 100),100))
            textstr = '\n'.join((
                    'Change per Decade=%.0f'% (PerROC, )+'%',
                    '%.1f cm per decade' % (CPD, ),
                    'P-Value= %.2f' % (lin_p, )))
        else:
            sub[i].set_ylim(bottom=0,top=42)
            sub[i].set_yticks(range(0,31))
            textstr = '\n'.join((
                    'Change per Decade=%.0f'% (PerROC, )+'%',
                    '%.1f days per decade' % (CPD, ),
                    'P-Value= %.2f' % (lin_p, )))            
        
        sub[i].set_xticks(range(1966,2017,5))#,fontsize=8)
        sub[i].set_xticklabels(range(1966,2017,5),fontsize=10) 
        sub[i].locator_params(axis="y",tight=True,nbins=10)   # change nbins integer to reduce number of y ticks
        sub[i].tick_params(axis="y",labelsize=10)                  # Needs to be changed for every plot.
        sub[i].xaxis.set_minor_locator(AutoMinorLocator(5))
        sub[i].set_ylabel(yaxis,fontsize=10,multialignment="center")
        
        #  sub[i].labelsize('medium')
        sub[i].set_title(label=months[i])
        sub[i].grid()
#        sub[i].scatter(x,ymax,s=8,c="black")
#        sub[i].scatter(x,ymin,s=8,c="black")
        sub[i].plot(x,y,color=color,label="Average",linewidth="2") # change to "Average"?
        if header != "MeanOfDay":
            reg_plot = sub[i].plot(x,lin_m*x+lin_b,color="black",label="R-Value={}".format(r_round),linewidth="2",linestyle="dashed")   # Regression of average
    #    sub[i].plot(x,ymin,color=c[1],label="Min",linewidth="2",linestyle="dashed")
     #   sub[i].plot(x,ymax,color=c[1],label="Max",linewidth="2",linestyle="dashed")
        sub[i].fill_between(x, per25, per75, alpha=0.35, linewidth=0, color=color)

       # sub[i].legend(r_round,reg_plot)
       # sub[i].legend(plot,reg_plot)
        props = dict(boxstyle='round', facecolor='white', alpha=0.7)
        sub[i].text(0.98, 0.95, textstr,transform=sub[i].transAxes,bbox=props,
           va = "top",ha="right" )

def Day_plot(title,y_list,selection,header,region_select,xaxis,yaxis):
    '''
    This function is designed to create a plot of daily values. It is tested to
    work and display all regions (region select of 5). This function operates
    similarly to the Monthly Plots.
    '''
   # print(data_list)
    c = color_select()
    for i in range(len(c)):
        r,g,b = c[i]
        c[i] = (r / 255., g / 255., b / 255.)
    print(len(y_list))
#    print(len(y_list[0]))
    plt.rc('font', family='serif')  
    plt.rc('xtick', labelsize='x-small')
    plt.rc('ytick', labelsize='x-small')    
    
    plt.figure(figsize=(12,8))    
    plt.title(title,fontsize=12)                    # Plotting title from above
    plt.grid()

    count = 0

    for j in y_list:
        y2_list = []
        y3_list = []
        y=[]
        ymin = []
        ymax= []
        x = []
        dates = []
        
    #    plt.xticks(range(1966,2017,5),fontsize=10)
        plt.yticks(fontsize=10)
       # plt.plot(xnew,f2(xnew),color=c[0],label="Average",markevery=100) # change to "Average"?
        region_lab = "Region %s" %(count+1)        
        for k in j:
           # print(len(j))
        #    print(i)
            x_pre=k[:,0]
            y_temp = k[:,1]          # plt_index gets its index when variable is chosen by user.
            y2_list.append(y_temp)
            y2_list = np.array(y2_list).tolist()
         #   y3_list = y2_list[181:]+y2_list[:181]
            y_pre = np.mean(y2_list,axis=0)
            ymin = np.min(y2_list,axis=0)
            ymax = np.max(y2_list,axis=0)
#            per_90 = np.percentile(y2_list,90,axis=0)
#            per_10 = np.percentile(y2_list,10,axis=0)            
            regress = linregress(x_pre,y_pre)
            lin_m = regress.slope
            lin_b = regress.intercept
            lin_r = stats.pearsonr(x_pre,y_pre)
            

        per75 = np.percentile(y2_list,75,axis=0)
        per75 = np.array(per75).tolist()
        per75 = per75[181:] + per75[:181]
        
        per25 = np.percentile(y2_list,25,axis=0)
        per25 = np.array(per25).tolist()
        per25 = per25[181:] + per25[:181]    
        
        x_pre = np.array(x_pre).tolist()
       # print(x_pre)
        for m in x_pre[181:]:
            m = '01'+str(int(m))
          #  print(m)
            m = datetime.strptime(m,'%y%j')
            m = m.strftime('%b %d')
            dates.append(m)
        for m in x_pre[:181]:
            m = '02'+str(int(m))
          #  print(m)
            m = datetime.strptime(m,'%y%j')
            m = m.strftime('%b %d')
            dates.append(m)        
     #   print(dates)
        
      #  x =  matplotlib.dates.date2num(dates)
        y_pre = np.array(y_pre).tolist()
        y = y_pre[181:] + y_pre[:181]
        
      #  print(x)
        
        plt.plot(dates,y,color=c[count],label=region_lab,linewidth="2",markevery=100) # change to "Average"?
        if header != "MeanOfDay":
            plt.plot(x,lin_m*x+lin_b,color=c[j],label="Linear Regression",linewidth="2",linestyle="dashed")   # Regression of average
        plt.fill_between(dates, per25, per75, alpha=0.25, linewidth=0, color=c[count])        
        
        count += 1
 
    plt.minorticks_on()
    plt.ylabel(yaxis,fontsize=12)                  # Needs to be changed for every plot.
    plt.xlabel(xaxis,fontsize=12)
    plt.xticks(range(0,365,14),fontsize=10,rotation=45)
    plt.yticks(range(0,40,5),fontsize=10)
    plt.xlim(0,365)
    plt.ylim(0,40) 
    plt.legend(fontsize=10)
        
def Gen_plot(x,y_list,selection,header,region,xaxis,yaxis,title):
    y=[]
    fname_lst = []

    y_list = np.array(y_list).tolist()
    print(len(y_list))
    y = np.mean(y_list,axis=0)
    ymin = np.min(y_list,axis=0)
    ymax = np.max(y_list,axis=0)
    #per_75 = np.percentile(y_list,75,axis=0)
   # per_25 = np.percentile(y_list,25,axis=0)
    per_90 = np.percentile(y_list,90,axis=0)
    per_10 = np.percentile(y_list,10,axis=0)
    
    regress = linregress(x,y)
    lin_m = regress.slope
    lin_b = regress.intercept
    lin_r = stats.pearsonr(x,y)
    lin_p = regress.pvalue
    pval = round(lin_p,2)
#    r_round = round(r[0],3)
#    m_round = round(m,3)

    c = color_select()
    for i in range(len(c)):
        r,g,b = c[i]
        c[i] = (r / 255., g / 255., b / 255.)

    if region == "1":
        color = c[0]
    elif region == "2":
        color = c[1]
    elif region == "3":
        color = c[2]
        #color = "black"
    elif region == "4":
        color = c[3]        
#    f = interp(x,y)
#    f2 = interp(x,y,kind="cubic")
#    xnew = np.linspace(1966, 2017,num=1000)
    plt.rc('font', family='serif')  
    plt.rc('xtick', labelsize='x-small')
    plt.rc('ytick', labelsize='x-small')    
    
    plt.figure(figsize=(12,8))    
    plt.title(title,fontsize=12)                    # Plotting title from above
    plt.grid()
    plt.minorticks_on()
    plt.tick_params(which="minor",axis="both",direction="in")
    plt.ylabel(yaxis,fontsize=12)                  # Needs to be changed for every plot.
    plt.xlabel(xaxis,fontsize=12)
    plt.xticks(range(1966,2017,5),fontsize=10) 
    plt.xlim(1966,2017)
    plt.ylim(0,max(ymax)+10)
##    plt.xticks(range(1966,2017,5),fontsize=10)
    plt.yticks(range(0,int(max(ymax))+10,10),fontsize=10)
     #   plt.yticks(range(0,40,5),fontsize=10)

   # plt.plot(xnew,f2(xnew),color=c[0],label="Average",markevery=100) # change to "Average"?
    
    plt.plot(x,y,color=color,label="Average",linewidth="2",markevery=100) # change to "Average"?
    if header != "MeanOfDay":
        plt.plot(x,lin_m*x+lin_b,color="black",label="Linear Regression \nP-Value = %s" %(pval),linewidth="2",linestyle="dashed")   # Regression of average
    plt.fill_between(x, per_10, per_90, alpha=0.35, linewidth=0, color=color, label= "10th and 90th Percentiles")
#    plt.fill_between(x, ymin, ymax, alpha=0.35, linewidth=0, color=color)

#    plt.plot(x,ymin,color=c[1],label="Min",linewidth="2",linestyle="dashed")
#    plt.plot(x,ymax,color=c[1],label="Max",linewidth="2",linestyle="dashed")
    plt.legend(fontsize=10)


def savefig(figname):    
    plt.savefig("U:\\Research\\GIS\\Maps\\figures\\%s.jpg" %(figname),dpi=2000)  # Need to vary this formatting between graphics.
    plt.show()                    


    
def Plot_Decade76(file,CellList,col):           # DOES NOT WORK AS PLANNED NEED TO RETHINK
    file['ij'] = file['i'].astype(str)+ file['j'].astype(str)  
    plotlist = file[file['ij'].isin(CellList)]
    plotlist['ij'] = plotlist['ij'].astype(int)
    print(plotlist)          
    print(len(plotlist['ij']))
  #  x = plotlist[["1967-1977","1977-1987","1987-1997","1997-2007","2007-2017"]].values
  #  y = plotlist[["ij"]].values
    ax =  plotlist.plot(kind='scatter',y='ij',x="1967-1977")
    plotlist.plot(kind='scatter',y='ij',x="1977-1987",ax=ax)
    plotlist.plot(kind='scatter',y='ij',x="1987-1997",ax=ax)
    plotlist.plot(kind='scatter',y='ij',x="1997-2007",ax=ax)
    plotlist.plot(kind='scatter',y='ij',x="2007-2017",ax=ax)
    
    plt.show()
#    y = CellList
#    plt.plot(x,y)    
    
def SnowDepthMean(file,header,plt_index,split_line):
  #  print(header)
    for f in file:
        data=pd.read_csv(f,skiprows=1)
       # data["year"]#,index_col=["year","MaxReporting"]) 
     #   usecols=["year","MaxReporting"])

   # print(data)

def color_select():
    '''
    This function is called to generate colors for graphing.
    Color maps can be found here: 
    https://tableaufriction.blogspot.com/2012/11/finally-you-can-use-tableau-data-colors.html
    '''
    
    tableau20blind = [(0, 107, 164), (255, 128, 14), (171, 171, 171), (89, 89, 89),
             (95, 158, 209), (200, 82, 0), (137, 137, 137), (163, 200, 236),
             (255, 188, 121), (207, 207, 207)]
    color_blind = [(215,25,28),(253,174,97),(145,191,219),(44,123,182)]
    return color_blind


def main():
    pd.set_option('display.max_columns', None)  
    pd.set_option('display.expand_frame_repr', False)
    pd.set_option('max_colwidth', -1)
    Blacklist=[2741,2740,2739,2738,2737,2736,2641,2640,2639,2638,2637,2636,2541,2540,2539,2538,2441,2440,2439,2438,2341,2340,2339,2241,2240,2141]       
    while True:
        filelist = []
        input_location = input("input (1) for individual files, (2) for concatenated files: ")
        if input_location == "1":
            selection = input("display row,column, all,region or end?: ")
            if selection == "end":
                break
            if selection == "all":
                CellList = total_file(Blacklist)
                region_select = 0
            elif selection == "row" or selection == "column":
                region_select = 0
                input1= input("Input coordinate 1,or 'end': " )
                if input1.lower() == 'end':
                     break
                else:    
                    input2= (input("Input coordinate 2: " ))
                    if input2.lower() == 'end':
                        break
                    else:        
                        if selection.lower() == "row":
                            CellList = row_file(Blacklist,input1,input2)
                        elif selection.lower() == "column":
                            CellList = col_file(Blacklist,input1,input2)
            elif selection == "region":
                region_select = input("Select a region between 1-4 or 5 for all regions: ")
                if region_select.lower() == 'end':
                    break
                elif int(region_select) < 5:
                    CellList = region_define(region_select)
                else:
                    CellList = all_region(region_select)
            
            input3,filelist = read_file(CellList,filelist,region_select,input_location)            
            header,var,split_line = get_header(filelist,region_select)        
            print(var)        
            
            if input3 == "MonthlyAverage.txt":
                figname = Set_Month(filelist,header,input3,selection,region_select)                
                savefig(figname)
            elif input3 == "76SnowDepth.txt":
                header,plt_index = pick_var(split_line)             # Subplot changes should begin here.           
                figname = Plot76(filelist,header,plt_index,selection,region_select,input3)
                savefig(figname)

            elif input3 == "DaySnowDepth.txt":  # Goal is to get 4 subplots for this file with a plot for each region.
                header,plt_index = pick_var(split_line)             # Subplot changes should begin here.                         
                figname = Day_mean(filelist,header,plt_index,selection,region_select,input3)
                savefig(figname)
            elif input3 == "MeanSnowDepth.txt":
                header,plt_index = pick_var(split_line)             # Subplot changes should begin here.                           
                figname = SnowDepthMean(filelist,header,plt_index,split_line)
                savefig(figname)                
            elif input3== "MonthAboveX.txt":
                figname = Set_Month(filelist,header,input3,selection,region_select)                
                savefig(figname)                
        elif input_location == "2":
            selection = input("display row,column,region, all or end?: ")
            if selection == "end":
                break
            if selection == "all":
                CellList = total_file(Blacklist)
            elif selection == "row" or selection == "column":
                input1= input("Input coordinate 1,or 'end': " )
                if input1.lower() == 'end':
                     break
                else:    
                    input2= (input("Input coordinate 2: " ))
                    if input2.lower() == 'end':
                        break
                    else:       
                        if selection.lower() == "row":
                            CellList = row_file(Blacklist,input1,input2)
                        elif selection.lower() == "column":
                            CellList = col_file(Blacklist,input1,input2)
            elif selection == "region":
                region_select = (input("Select a region between 1-4: "))
                if region_select.lower() == 'end':
                    break
                else:
                    CellList = region_define(region_select)
                
                            
            input3,filelist = read_file(CellList,filelist,input_location)
            print(input3)
            if input3 == "AllPercent_Decade76.txt":
                col=["i","j","lat","lon","1967-1977","1977-1987","1987-1997",\
                "1997-2007","2007-2017","DecDif 1-2","DecDif 2-3","DecDif 3-4","DecDif 4-5"]
                filelist.columns = col
                Plot_Decade76(filelist,CellList,col)        
        else:
            break
    


    else:
        quit
    
if __name__ == "__main__":
    main()        
