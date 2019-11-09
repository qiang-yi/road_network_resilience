'''
Created on Dec 6, 2018

'''

import csv
import urllib
import json
import time
import sys
from itertools import cycle
import pause
import googlemaps
from datetime import datetime,timedelta

import dur_in_traffic

#google maps API Key:
key="YOURAPIKEY" 

path="D:/winter_storm/Jan_2019/tables/realtime_input_clv/"

time_ls=[]

#time1=datetime.strptime('Jan 19 2019  6:48', '%b %d %Y %H:%M') # UTC time: Eastern time 12:00pm Jan 20
time1=datetime.strptime('Jan 19 2019  15:50', '%b %d %Y %H:%M') # UTC time: Eastern time 11:00pm Jan 19
time2=datetime.strptime('Jan 19 2019  21:50', '%b %d %Y %H:%M') # UTC time: Eastern time 17:00pm, Jan 19
time3=datetime.strptime('Jan 20 2019  15:50', '%b %d %Y %H:%M') # UTC time: Eastern time 11:00am, Jan 20
time4=datetime.strptime('Jan 20 2019  21:50', '%b %d %Y %H:%M') # UTC time: Eastern time 17:00pm, Jan 20

# time1=datetime.strptime('Jan 18 2019  02:36', '%b %d %Y %H:%M') # UTC time: Eastern time 11:50pm Jan 20
# time2=datetime.strptime('Jan 18 2019  02:36', '%b %d %Y %H:%M') # UTC time: Eastern time 16:50pm, Jan 20
# time3=datetime.strptime('Jan 20 2019  02:37', '%b %d %Y %H:%M') # UTC time: Eastern time 10:00am, Jan 20
# time4=datetime.strptime('Jan 20 2019  02:38', '%b %d %Y %H:%M') # UTC time: Eastern time 17:00pm, Jan 20

local_time1=time1-timedelta(hours=10) # convert to HST
local_time2=time2-timedelta(hours=10) # convert to HST
local_time3=time3-timedelta(hours=10) # convert to HST
local_time4=time4-timedelta(hours=10) # convert to HST

timestamp1=int((time1- datetime(1970,1,1)).total_seconds())
timestamp2=int((time2- datetime(1970,1,1)).total_seconds())
timestamp3=int((time3- datetime(1970,1,1)).total_seconds())
timestamp4=int((time4- datetime(1970,1,1)).total_seconds())


time_ls.append([time1,local_time1,timestamp1,'time1_1.csv'])
time_ls.append([time2,local_time2,timestamp2,'time2_1.csv'])
time_ls.append([time3,local_time3,timestamp3,'time3_1.csv'])
time_ls.append([time4,local_time4,timestamp4,'time4_1.csv'])

for time in time_ls:
    
    UTC_time=time[0]
    localtime=time[1]
    ts=time[2]
    infile=time[3]
    infile=path+infile
    
    pause.until(localtime) # start the program at the local time
    
    outtime=UTC_time.strftime("%Y-%m-%d_%H%M")
    outfile=infile.replace('.csv',"_"+outtime+'.csv')
    print("local_time: "+str(localtime))
    print("UTC_time: "+str(UTC_time))
    print('now is :'+str(datetime.now()))
    print('input: '+infile)
    print('output: '+outfile)
    dur_in_traffic.direction(infile,outfile,'now',key)
