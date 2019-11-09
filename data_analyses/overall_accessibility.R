# This file aggregates travel times of O-D pairs into Hansen accessibility index
# and create plots and maps for the accessibility index

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
library(chemCal)

#source('load_data.r')

# --------- Calculate overal  travel time increases  --------
# plot average reduction ratio of all facility types
df_agg_t = aggregate(. ~ time + facility_type, data=df_clv,mean)
df_agg_t=select(df_agg_t,c('time','time_s', 'facility_type','Dur_min_rt','Dur_min_bm','Dur_min_rg'))
df_agg_t$diff_bm=df_agg_t$Dur_min_rt-df_agg_t$Dur_min_bm
df_agg_t$r_diff_bm=(df_agg_t$Dur_min_rt-df_agg_t$Dur_min_bm)/df_agg_t$Dur_min_bm
df_agg_t$diff_rg=df_agg_t$Dur_min_rt-df_agg_t$Dur_min_rg
df_agg_t$r_diff_rg=(df_agg_t$Dur_min_rt-df_agg_t$Dur_min_rg)/df_agg_t$Dur_min_rg

df_agg_t=select(df_agg_t,c('time','time_s','facility_type','r_diff_bm','r_diff_rg'))

df_prep_clv2=df_prep_clv[df_prep_clv$ts >= (time_ls2[[1]]-3600) & df_prep_clv$ts <= time_ls2[[4]],]

# --------- line chart of average time reduction of the facilities (regular scale)-----------
p <- ggplot(data = df_agg_t, aes(x=time_s, y=r_diff_bm,group =facility_type)) + 
  geom_bar(data=df_prep_clv2,aes(x=ts,y=precipitation*5,group=1),stat = "identity", colour = gray(0.8), fill = gray(0.8))+
  geom_point(aes(color = facility_type),size=2) +
  #geom_line(data=df_prep_clv2,aes(x=ts,y=precipitation*5,group=1),linetype = "dashed",color="gray60",size=1)+
  geom_line(aes(color = facility_type),size=1) + 
  theme_bw()+
  labs(color = "Facility type")+
  xlab("Time") + ylab("% of travel time increase")+
  scale_x_continuous(breaks = time_ls2,labels=c(expression(italic(t)[1]),expression(italic(t)[2]),expression(italic(t)[3]),expression(italic(t)[4])))+
  scale_y_continuous(labels = percent,sec.axis = sec_axis(~./5, name = "Precipitation (inches)"))+
  theme(axis.title.x = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=12),
        legend.title =element_text(size=14),
        legend.text=element_text(size=12))
p
ggsave(paste0(outpath,"travel_time_increase.png"), width = 8, height = 5, units = "in",dpi = 600)


# --------- line chart of average time reduction of the facilities (log y scale)-----------
# First, create line chart (log y scale) with transparent background
p1 <- ggplot(data = df_agg_t, aes(x=time_s, y=r_diff_bm,group =facility_type)) + 
  #geom_bar(data=df_prep_clv2,aes(x=ts,y=precipitation*5,group=1),stat = "identity", colour = gray(0.8), fill = gray(0.8))+
  geom_point(aes(color = facility_type),size=2) +
  #geom_line(data=df_prep_clv2,aes(x=ts,y=precipitation*5,group=1),linetype = "dashed",color="gray60",size=1)+
  geom_line(aes(color = facility_type),size=1) + 
  theme_bw()+
  labs(color = "Facility type")+
  xlab("Time") + ylab("% of travel time increase")+
  scale_x_continuous(breaks = time_ls2,labels=c(expression(italic(t)[1]),expression(italic(t)[2]),expression(italic(t)[3]),expression(italic(t)[4])))+
  scale_y_continuous(labels = percent,trans=log_trans(), breaks=pretty_breaks(),sec.axis = sec_axis(~./5, name = "Precipitation (inches)"))+
  theme(axis.title.x = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=12),
        legend.title =element_text(size=14),
        legend.text=element_text(size=12),
        rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA))
p1
ggsave(paste0(outpath,"travel_time_increase_line.png"), width = 8, height = 5, units = "in",dpi = 600,bg = "transparent")

# Second, create bar chart (normal y scale) with transparent background
p2 <- ggplot(data = df_agg_t, aes(x=time_s, y=r_diff_bm,group =facility_type)) + 
  geom_bar(data=df_prep_clv2,aes(x=ts,y=precipitation*5,group=1),stat = "identity", colour = gray(0.8), fill = gray(0.8))+
  geom_point(aes(color = facility_type),size=2,alpha=0) +
  #geom_line(data=df_prep_clv2,aes(x=ts,y=precipitation*5,group=1),linetype = "dashed",color="gray60",size=1)+
  geom_line(aes(color = facility_type),size=1,alpha=0) + 
  theme_bw()+
  labs(color = "Facility type")+
  xlab("Time") + ylab("% of travel time increase")+
  scale_x_continuous(breaks = time_ls2,labels=c(expression(italic(t)[1]),expression(italic(t)[2]),expression(italic(t)[3]),expression(italic(t)[4])))+
  scale_y_continuous(labels = percent,sec.axis = sec_axis(~./5, name = "Precipitation (inches)"))+
  theme(axis.title.x = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=12),
        legend.title =element_text(size=14),
        legend.text=element_text(size=12),
        rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"))
p2
ggsave(paste0(outpath,"travel_time_increase_bar.png"), width = 8, height = 5, units = "in",dpi = 600,bg = "transparent")


#-------------------------------------------------------

# ----- Calculate overall accessibility ---------
for (fac in fac_ls){
  max_rg=max(df_clv$Dur_min_rg[df_clv$facility_type==fac])
  min_rg=min(df_clv$Dur_min_rg[df_clv$facility_type==fac])
  
  df_clv$Dur_min_rg2[df_clv$facility_type==fac]=1-(df_clv$Dur_min_rg[df_clv$facility_type==fac]-min_rg)/(max_rg-min_rg)
  df_clv$Dur_min_bm2[df_clv$facility_type==fac]=1-(df_clv$Dur_min_bm[df_clv$facility_type==fac]-min_rg)/(max_rg-min_rg)
  df_clv$Dur_min_rt2[df_clv$facility_type==fac]=1-(df_clv$Dur_min_rt[df_clv$facility_type==fac]-min_rg)/(max_rg-min_rg)
}

df_agg_idx = aggregate(. ~ time, data=df_clv,mean)
df_agg_idx$diff_bm2 = (df_agg_idx$Dur_min_rt2 - df_agg_idx$Dur_min_bm2)/df_agg_idx$Dur_min_bm2

#offset accessibility by the initial status
df_agg_idx$diff_bm2 = df_agg_idx$diff_bm2 - df_agg_idx$diff_bm2[[1]]

df_agg_idx$diff_rg2 = (df_agg_idx$Dur_min_rt2 - df_agg_idx$Dur_min_rg2)/df_agg_idx$Dur_min_rg2

# fit spline function
y=c(df_agg_idx$diff_bm2,0)
#y=insert(y,c(2,3,4,5),y[1:4]+diff(y)/2)
#x=c(df_agg_idx$time_s,1548068400)
x=c(df_agg_idx$time_s,1548046800)
#x=insert(x,c(2,3,4,5),x[1:4]+diff(x)/2)
plot(x,y,type="l",ylim=c(-0.08,0))
dgr=4
poly_m <- lm(y ~ poly(x,dgr))

xl <- seq(min(x),max(x), (max(x) - min(x))/1000)
out = unname(predict(poly_m,data.frame(x = xl)))
lines(xl, out, col='red', lwd=2)
infl=c(FALSE, diff(diff(out)>0)!=0)
inflection_pt=data.frame(x=xl[infl],y=out[infl])

xs=inflection_pt[1,1]
xe=inflection_pt[2,1]
xl2 <- seq(xs,xe, (xe - xs)/2000)
out2 = unname(predict(poly_m,data.frame(x = xl2)))
s1=min(abs(out2))
#s2=sort(abs(out2),partial=n-1)[n-1]
x0=xl2[abs(out2)==s1]
y0=unname(predict(poly_m,data.frame(x = x0)))
lsx=c(x[1],x0)
lsy=c(y[1],y0)


# Create boundaries for geo_ribbon
xs <- seq(min(xl),x0,length.out=1000)
ysmax <- rep(0, length(xs))
ysmin <- unname(predict(poly_m,data.frame(x = xs)))
df2 <- data.frame(xs, ysmin, ysmax)


# plot the real overall change of accessibility index
p <- ggplot() + 
  #geom_line(data=df_prep_clv2,aes(x=ts,y=precipitation/5,group=1),linetype = "dashed",color="black",size=1)+
  geom_bar(data=df_prep_clv2,aes(x=ts,y=precipitation/5,group=1),stat = "identity", colour = 'black', fill = 'white')+
  
#  geom_point(data = df_agg_idx, aes(x=time_s, y=diff_bm2,group =1)) +
  geom_line(aes(x=x, y=y,group =1),color='red',size=2) + 
  geom_hline(yintercept=0,lwd = 1)+
  geom_point(aes(x=x[1:4], y=y[1:4],group =1),color='black',size=4) +
  geom_point(aes(x=x[5], y=0,group =1),color='blue',size=4) +
  theme_bw()+
  xlab("Time") + ylab("Accessibility reduction")+
  scale_x_continuous(breaks = x,labels=c(expression(italic(t)[1]),expression(italic(t)[2]),expression(italic(t)[3]),expression(italic(t)[4]),expression(italic(t)[5]))) +
  scale_y_continuous(limits=c(NA,0.015),sec.axis = sec_axis(~.*5, name = "Precipitation (inches)",breaks = seq(0,0.08,0.02)))+
  theme(axis.title.x = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=12),
        legend.title =element_text(size=14),
        legend.text=element_text(size=12))
  
  #geom_xspline(aes(x=x,y=y),spline_shape=0.1) +
  #stat_function(data=data.frame(x=x,y=y),aes(x=x, y=y,group =1),fun = spl_fun)+
  #stat_smooth(aes(x=x,y=y,group=1), method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, dgr)) +
  #geom_ribbon(data=df2, aes(x=xs, ymin=ysmin, ymax=ysmax), fill="#BB000033") +
  #geom_point(aes(y=lsy,x=lsx),colour='blue',size = 5)+
  #geom_point(aes(y=inflection_pt[,2],x=inflection_pt[,1]),colour='red',size = 5)

#geom_smooth(method="glm", formula=y~poly(x,3), se=FALSE)
#p <-  geom_ribbon(data=df2, aes(x=xs, ymin=ysmin, ymax=ysmax), fill="#BB000033")
p

ggsave(paste0(outpath,"overall_accessibility.png"), width = 8, height = 5, units = "in",dpi = 600)
