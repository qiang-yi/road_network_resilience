
# This file combines the tables of data collected at different times into a single table
# Converts the time to UTC
# Join the normal travel times with the real-time travel times

## ---- combine result of cleverland-----
library(dplyr)
# read data
res_time1_1=read.csv("D:/winter_storm/Jan_2019/tables/realtime_input_clv/original_collection/time1_1_2019-01-19_1930.csv", header=TRUE)
res_time1_2=read.csv("D:/winter_storm/Jan_2019/tables/realtime_input_clv/original_collection/time1_2_2019-01-19_1930.csv", header=TRUE)

res_time2_1=read.csv("D:/winter_storm/Jan_2019/tables/realtime_input_clv/original_collection/time2_1_2019-01-19_2250.csv", header=TRUE)
res_time2_2=read.csv("D:/winter_storm/Jan_2019/tables/realtime_input_clv/original_collection/time2_2_2019-01-19_2250.csv", header=TRUE)

res_time3_1=read.csv("D:/winter_storm/Jan_2019/tables/realtime_input_clv/original_collection/time3_1_2019-01-20_1550.csv", header=TRUE)
res_time3_2=read.csv("D:/winter_storm/Jan_2019/tables/realtime_input_clv/original_collection/time3_2_2019-01-20_1550.csv", header=TRUE)

res_time4_1=read.csv("D:/winter_storm/Jan_2019/tables/realtime_input_clv/original_collection/time4_1_2019-01-20_2150.csv", header=TRUE)
res_time4_2=read.csv("D:/winter_storm/Jan_2019/tables/realtime_input_clv/original_collection/time4_2_2019-01-20_2150.csv", header=TRUE)

normal_time=read.csv("D:/winter_storm/Jan_2019/tables/clv_facilities_cbd_rank0_output.csv", header=TRUE)

# assign dates 
res_time1_1$time=as.POSIXlt('2019-1-19 16:00:00',tz="Europe/London")
res_time1_2$time=as.POSIXlt('2019-1-19 16:00:00',tz="Europe/London")

res_time2_1$time=as.POSIXlt('2019-1-19 22:00:00',tz="Europe/London")
res_time2_2$time=as.POSIXlt('2019-1-19 22:00:00',tz="Europe/London")

res_time3_1$time=as.POSIXlt('2019-1-20 16:00:00',tz="Europe/London")
res_time3_2$time=as.POSIXlt('2019-1-20 16:00:00',tz="Europe/London")

res_time4_1$time=as.POSIXlt('2019-1-20 22:00:00',tz="Europe/London")
res_time4_2$time=as.POSIXlt('2019-1-20 22:00:00',tz="Europe/London")

normal_time$time=as.POSIXlt('1970-1-1 00:00:00',tz="Europe/London")
normal_time = select(normal_time,c('GEOID_Data','facility_type','Dist_km','Dur_min'))


#combine all real-time results
res_all=do.call("rbind", list(res_time1_1, res_time1_2, res_time2_1, res_time2_2,res_time3_1, res_time3_2,res_time4_1, res_time4_2))

#Join normal time to real-time results
res_all=merge(res_all, normal_time, by=c("GEOID_Data", "facility_type"))

#Change column names
colnames(res_all)[30:31]=c('Dist_km_bm','Dur_min_bm')
colnames(res_all)[32:33]=c('Dist_km_rt','Dur_min_rt')
colnames(res_all)[35:36]=c('Dist_km_rg','Dur_min_rg')

#remove space in column names
res_all$facility_type = as.character(res_all$facility_type)
res_all$facility_type[res_all$facility_type=='fire station']='fire'
res_all$facility_type[res_all$facility_type=='police department']='police'

write.csv(res_all,"D:/winter_storm/Jan_2019/tables/realtime_input_clv/all_data.csv")
