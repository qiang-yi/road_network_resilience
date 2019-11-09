'''
Created on Apr 1, 2018

'''
import csv
import urllib
import json
import time
import sys
import copy
from itertools import cycle

import googlemaps
from datetime import datetime

key="YOURAPIKEY" # your Google API key

path="D:/winter_storm/Feb_12/" # root directory of the project
in_file=path+'tables/detroit_tract_pt.csv' # the file contains coordinates of census tract centers
#in_file=path+'tables/test.csv'
tract_file = open(in_file, "r") # file to open
tract_data = csv.reader(tract_file)

facility_name='police department'

out_fname=in_file.replace(".csv","_"+facility_name+".csv")
out_file = open(out_fname, "wb")
writer = csv.writer(out_file)

#-------------search for nearby facilities for all spatial units (e.g. census tracts)------------
counter=0
error_count=0
for tract in tract_data:
    if counter==0:

        X=tract.index("POINT_X")
        Y=tract.index("POINT_Y")

        tract.extend(["facility_type", "facility_name", "facility_lat","facility_lng",'distance_rank'])
        writer.writerow(tract)

        counter=counter+1
        continue # skip header in the csv file
    if abs(float(tract[X])) < 0.1:
        print "tract point has no coordinates"
        continue #skip tracts that don't have coordinates

    loc=tract[Y]+', '+tract[X]

    gmaps = googlemaps.Client(key=key)
    print loc

    try:
        places_output = gmaps.places_nearby(location=loc, name=facility_name,rank_by='distance')
        print(places_output)
        counter=counter+1
        #directions_result = gmaps.directions(tract_loc, msa_loc, mode="driving",traffic_model="pessimistic",departure_time=datetime.now())
        #print(places_output['results'][0])
        #print(places_output)
        num=len(places_output['results'])
        select_num=5

        if num>=select_num:
            sub_place_ls=places_output['results'][0:select_num]

            for i in range(0,select_num):
                fname=sub_place_ls[i]['name'].encode('utf-8')
                flat=sub_place_ls[i]['geometry']['location']['lat']
                flng=sub_place_ls[i]['geometry']['location']['lng']

                tract1=copy.deepcopy(tract)
                tract1.extend([facility_name,fname,flat,flng,str(i)])
                print(tract1)
                writer.writerow(tract1)

                continue

        elif num<select_num:
            print('len < 0')
            tract1=copy.deepcopy(tract)
            tract1.extend([facility_name, 'no_facility_return','','','',''])
            writer.writerow(tract1)


    except:
        print sys.exc_info()[0]
        error_count=error_count+1


#-------------report result and write result into csv file------------
    #print directions_result
    print "processed "+str(counter) +" pairs"

    #tract.extend([distance_km,duration_min])
    #print tract
    #writer.writerow(tract)


out_file.close()
