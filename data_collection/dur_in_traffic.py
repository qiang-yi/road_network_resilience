'''
Created on Dec 6, 2018

'''

import csv
import urllib
import json
import time
import sys
from itertools import cycle

import googlemaps
from datetime import datetime,timedelta

def direction(infile,outfile,depart_t,key):
    tract_file = open(infile, "r") # file to open
    tract_data = csv.reader(tract_file)
    
    out_file = open(outfile, "wb")
    writer = csv.writer(out_file)
    
    counter=0
    error_count=0
    
    for tract in tract_data:
        if counter==0:
    
            tract_idx_X=tract.index("POINT_X")
            tract_idx_Y=tract.index("POINT_Y")
            msa_idx_X=tract.index("facility_lng")
            msa_idx_Y=tract.index("facility_lat")
            
            tract.extend(["Dist_km", "Dur_min"])
            writer.writerow(tract)
    
            counter=counter+1
            continue # skip header in the csv file
        if abs(float(tract[msa_idx_X])) < 0.1: 
            print "tract point has no coordinates"
            continue #skip tracts that don't have coordinates
    
    
        tract_loc=tract[tract_idx_Y]+', '+tract[tract_idx_X]
        fac_loc=tract[msa_idx_Y]+', '+tract[msa_idx_X]
    

        gmaps = googlemaps.Client(key=key)
        
        try:
            directions_result = gmaps.directions(fac_loc, tract_loc, mode="driving",departure_time=depart_t)
            #directions_result = gmaps.directions(tract_loc, msa_loc, mode="driving",traffic_model="pessimistic",departure_time=datetime.now())
            counter=counter+1
            
            error_count=0
            
            if len(directions_result)==0:
                print('len < 0')
                print(len(directions_result))
                distance_km="Cannot find direction"
                duration_min="Cannot find irection"
            else:
                distance_km = float(directions_result[0]['legs'][0]['distance']['value'])/float(1000)
                duration_min = float(directions_result[0]['legs'][0]['duration_in_traffic']['value'])/float(60)
                print directions_result[0]['legs'][0]
                
        except:
            print sys.exc_info()
            distance_km="No data retrieved"
            duration_min="No data retrieved"
            error_count=error_count+1
        
    
    #-------------report result and write result into csv file------------
        print ('inputfile: '+infile)
        #print directions_result    
        print "processed "+str(counter) +" pairs"
        print([distance_km,duration_min])
        tract.extend([distance_km,duration_min])
        print(tract)
        writer.writerow(tract)
        
    out_file.close()
    
    
    

    