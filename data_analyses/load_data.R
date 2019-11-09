# This file reads the collected data of travel and
#  of O-D pairs and preprocesses the data.

library(reshape2)
library(reshape)
library(data.table)
require(rgdal)
require(ggplot2)
library(broom)
library(ggmap)
library(sf)
library(scales)
library(dplyr)
library(plyr)
library(grid)
library(gridExtra)
library(scales)
library(mgcv)
library(RColorBrewer)
library(classInt)
library(cartography)
require(graphics)


# load csv
path="D:/winter_storm/Jan_2019/tables/"
outpath='.../Documents/UH_work/Submissions/Manuscripts/Trans_Resilience/figure/'
## ------------------ cleverland ---------------------------
#read the files
df_clv=read.csv(paste0(path,'realtime_input_clv/all_data.csv'), header=TRUE)
df_clv=select(df_clv,c('GEOID_Data','time','facility_type','facility_name','Dist_km_bm','Dur_min_bm','Dist_km_rt','Dur_min_rt','Dist_km_rg','Dur_min_rg'))

## convert GMT to eastern time
df_clv$time=as.POSIXlt(df_clv$time,format="%m/%d/%Y %H:%M",tz="America/New_York") - 5*60*60
df_clv$time=format(df_clv$time, "%m/%d/%y %H:%M") # format time
df_clv$time = factor(df_clv$time) # convert time to factors

#Correct the factor levels
levels(df_clv$time) <- c("01/19/19 11:00", levels(df_clv$time)[2:4])
levels(df_clv$time)[2]="01/19/19 17:00"

df_clv$facility_type = as.character(df_clv$facility_type)
df_clv$facility_type[df_clv$facility_type=="center"]  <- "CBD"
df_clv$facility_type[df_clv$facility_type=="fire"]  <- "Fire station"
df_clv$facility_type[df_clv$facility_type=="police"]  <- "Police department"
df_clv$facility_type[df_clv$facility_type=="hospital"]  <- "Hospital"
df_clv$facility_type[df_clv$facility_type=="grocery"]  <- "Grocery store"
df_clv$facility_type = as.factor(df_clv$facility_type)


df_clv$diff_bm=df_clv$Dur_min_rt-df_clv$Dur_min_bm
df_clv$r_diff_bm=(df_clv$Dur_min_rt-df_clv$Dur_min_bm)/df_clv$Dur_min_bm

df_clv$diff_rg=df_clv$Dur_min_rt-df_clv$Dur_min_rg
df_clv$r_diff_rg=(df_clv$Dur_min_rt-df_clv$Dur_min_rg)/df_clv$Dur_min_rg


# convert time to seconds
df_clv$time_s=as.numeric(as.POSIXct(df_clv$time,tz='EST', format = "%m/%d/%y %H:%M"))

time_ls = levels(df_clv$time)
fac_ls = levels(df_clv$facility_type)
time_ls2=sort(unique(df_clv$time_s))



# --------- read precipitation data ------------------
df_prep_clv=read.csv(paste0(path,'clv_prep.csv'), header=TRUE)




