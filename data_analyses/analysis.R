# This file contains programs of statistical analysis and 
# plots about the relationship between travel times and other variables

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
library(polynom)
setwd('D:/winter_storm/R_code')
source('load_data.r')
source('overall_accessibility.R')

# ----- Relation between regular travel time and travel time reduction at t2 -------
m_ls=c()
sp_ls=c()
i=1
for (fac in fac_ls){
  #fac='Center'
  t2=df_clv[(df_clv$time=='01/19/19 17:00')&(df_clv$facility_type==fac),]

  m = lm(t2$r_diff_bm ~ t2$Dur_min_rt)
  m=summary(m)
  m_ls[[i]]=m
  if (fac=='CBD'){
    sp_ls[[i]]<-ggplot(t2, aes(x=Dist_km_rt, y=r_diff_bm)) + geom_point(size=0.5)+
      theme_bw()+ ggtitle(fac) + theme(plot.title = element_text(hjust = 0.5))+
      xlab("Average travel distance (km)") + ylab("% of travel time increase at t2")+
      geom_smooth(method='gam',formula=y~s(x),se=FALSE)
  } else {
    sp_ls[[i]]<-ggplot(t2, aes(x=Dist_km_rt, y=r_diff_bm)) + geom_point(size=0.5)+
      theme_bw()+ ggtitle(fac) + theme(plot.title = element_text(hjust = 0.5))+
      xlab("Average travel distance (km)") + ylab("% of travel time increase at t2")+
      geom_smooth(method=lm,se=FALSE)
  }
  
  i=i+1
}

grid.arrange(grobs = sp_ls, ncol = 3)
g=arrangeGrob(grobs = sp_ls, ncol = 3)
ggsave(paste0(outpath,"rel_traveltime_reduction.png"),g, width = 10, height = 7, units = "in",dpi = 600)

# ------- plot the change of accessibility reduction ------------
df_agg_idx_geog = aggregate(. ~ time + GEOID_Data, data=df_clv,mean)
df_agg_idx_geog$diff_bm2 = (df_agg_idx_geog$Dur_min_rt2 - df_agg_idx_geog$Dur_min_bm2)/df_agg_idx_geog$Dur_min_bm2
df_agg_idx_geog$diff_rg2 = (df_agg_idx_geog$Dur_min_rt2 - df_agg_idx_geog$Dur_min_rg2)/df_agg_idx_geog$Dur_min_rg2


#offset the accessibility at t2 - t4 with the initial status.
geog_ls=unique(df_agg_idx_geog$GEOID_Data)
for (g in geog_ls){
  df_agg_idx_geog$diff_bm2_r[df_agg_idx_geog$GEOID_Data==g]=df_agg_idx_geog$diff_bm2[(df_agg_idx_geog$time_s==min(unique(df_clv$time_s)))&(df_agg_idx_geog$GEOID_Data==g)]
}
df_agg_idx_geog$diff_bm2_adj = df_agg_idx_geog$diff_bm2 - df_agg_idx_geog$diff_bm2_r

# add t5
zero=df_agg_idx_geog[df_agg_idx_geog$time_s==min(unique(df_clv$time_s)),]
zero$time="01/20/19 17:00"
zero$time_s=1548046800
df_agg_idx_geog2=rbind(df_agg_idx_geog,zero)

#add ending time
# add = df_agg_idx_geog[df_agg_idx_geog$time==time_ls[1],]
# add$time='01/21/19 6:00'
# add$time_s='1548068400'
# df_agg_idx_geog=rbind(df_agg_idx_geog,add)
# time_ls=unique(df_agg_idx_geog$time)
# time_ls2=unique(df_agg_idx_geog$time_s)

p <- ggplot() + 
  geom_line(data = df_agg_idx_geog2, aes(x=time_s, y=diff_bm2_adj,group =GEOID_Data,color =GEOID_Data))+
  # stat_smooth(data = df_agg_idx_geog, aes(x=time_s, y=diff_bm2_r,group =GEOID_Data,color = GEOID_Data),
  #             method = "lm", fill=NA,formula=y ~ poly(x, 3),size=0.1) +
  theme_bw()+
  xlab("Time") + ylab("Accessibility reduction")+
  scale_x_continuous(breaks = c(time_ls2,1548046800),labels=c(expression(italic(t)[1]),expression(italic(t)[2]),expression(italic(t)[3]),expression(italic(t)[4]),expression(italic(t)[5])))+
  theme(axis.title.x = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=12),
        legend.title =element_text(size=14),
        legend.text=element_text(size=12),
        legend.position="none")+
  geom_point(aes(x=x[1:4], y=y[1:4],group =1),color = 'black',size=5)+
  geom_line(aes(x=x, y=y,group =1),size=1.5)+
  geom_point(aes(x=1548046800, y=0,group =1),color = 'blue',size=5)
  #stat_function(data = df_agg_idx, aes(x=time_s, y=diff_bm2,group =1),fun = spl_fun)
  
p
ggsave(paste0(outpath,"ct_accessibility.png"), width = 8, height = 5, units = "in",dpi = 600)


# -------- linear interpolated resilience -----------------
df_vul=select(df_agg_idx_geog,c('time','time_s','GEOID_Data','diff_bm2_adj'))
df_vul=dcast(df_vul,GEOID_Data ~ time,value.var='diff_bm2_adj')
geoid_ls = unique(df_vul$GEOID_Data)


i=1
res_ls=c()

#geoid='14000US39035101101'
for (geoid in geoid_ls){
  end_time=1548046800
  x=c(time_ls2,end_time)
  y=unlist(as.list(unname(df_vul[df_vul$GEOID_Data==geoid,c(2,3,4,5)])))
  y=c(y,0)
  x_out=seq(x[1],x[length(x)],(x[length(x)]-x[1])/1000)

  int_pts=approx(x, y,xout=x_out, method = "linear")
  plot(x, y, main = "approx(.) and approxfun(.)")
  points(int_pts, col = 4, pch = "*")
 
  # Create boundaries for geo_ribbon
  xs <- x
  ysmax <- y
  ysmin <- 0
  df2 <- data.frame(xs, ysmin, ysmax)
  
  
  # plot the overall change of accessibility index
  p <- ggplot() + 
    geom_line(aes(x=x, y=y,group =1)) +
    #geom_point(aes(x=x, y=y,group =1)) +
    #geom_line() + 
    theme_bw()+
    xlab("Time") + ylab("Accessibility Index")+
    scale_x_continuous(breaks = time_ls2,labels=time_ls)+
    scale_y_continuous(limits=c(NA,0.01))+
    theme(axis.title.x = element_text(size=14),
          axis.text.x = element_text(size=10),
          axis.title.y = element_text(size=14),
          axis.text.y = element_text(size=12),
          legend.title =element_text(size=14),
          legend.text=element_text(size=12)) +
    #geom_xspline(aes(x=x,y=y),spline_shape=0.1) +
    #  stat_function(data=data.frame(x=x,y=y),aes(x=x, y=y,group =1),fun = spl_fun)+
    #stat_smooth(aes(x=x,y=y,group=1), method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, dgr)) +
    geom_ribbon(data=df2, aes(x=xs, ymin=ysmin, ymax=ysmax), fill="#BB000033") +
    geom_hline(yintercept=0,lwd = 1)
  
  #p
  if(y[2]>0){
    res_ls[i]=0
  }else{
    yy=int_pts[[2]]
    yy=yy[yy<0]
    res_ls[i]=sum(yy)
  }

  i=i+1

}

#----------- plot resilience --------------------
df_vul$res=1-abs(res_ls)/(max(abs(res_ls))-min(abs(res_ls)))


#create classes for accessibility, impact and resilience
#classes <- classIntervals(df_vul$impact, n = 8, style = "jenks")$brks
# var=df_vul$res
# min=min(var)
# max=max(var)
# std=sd(var)
# classes=getBreaks(v = var, method = "msd", k = 0.5, middle = TRUE)
# idx = findInterval(mean(var), classes)
# classes=c(classes[0:(idx+idx-1)],max)

# df_vul$classes <- cut(var,breaks=classes,include.lowest=TRUE)
# 
# #plot resilience index
# shp2=shp
# shp2 = merge(shp2,df_vul,by='GEOID_Data',)
# 
# shp_df <- tidy(shp2,region='GEOID_Data') # tidy the shapefile, only get the geometries
# shp_df <- merge(shp_df, shp2, by.x = "id", by.y="GEOID_Data")
# 
# #div_pal <- colorRampPalette(brewer.pal(name = "RdBu", n = 11))
# p <- ggplot(data = shp_df, aes(x = long, y = lat, group = group)) + 
#   geom_polygon(aes(fill = classes),color='dimgray',size=0.01) +
#   scale_fill_brewer(palette = "RdYlBu",direction=-1, drop = FALSE)
# #scale_fill_distiller(palette = "RdBu")
# 
# p



#------- relation between regular accssibility and impact and resilience -------
df_acs=read.csv(paste0(path,'census_data/income.csv'),header=TRUE)
df_acs=select(df_acs,c('GEOID_Data','HC01_EST_VC13','HC01_EST_VC15'))
colnames(df_acs)=c('GEOID_Data','med_income','m_income')
df_acs$med_income=as.numeric(as.character(df_acs$med_income))
df_acs$m_income=as.numeric(as.character(df_acs$m_income))
df_vul=merge(df_vul,df_acs,by='GEOID_Data')

# export benchmark accessibility to csv
df_acc=select(df_agg_idx_geog,c('time','time_s','GEOID_Data','Dur_min_rg2'))
df_acc=dcast(df_acc,GEOID_Data~time,value.var='Dur_min_rg2')
df_acc=select(df_acc,c(1,2))
colnames(df_acc)=c('GEOID_Data','reg_acc')
df_vul=merge(df_vul,df_acc,by='GEOID_Data')

# find outlier points
df_vul_nna=na.omit(df_vul)
q1=0.50
q2=0.25

inc_low1= quantile(df_vul_nna$m_income, q1,na.rm=TRUE)
inc_low2= quantile(df_vul_nna$m_income, q2,na.rm=TRUE)

impact_low1= quantile(df_vul_nna$`01/19/19 17:00`, q1,na.rm=TRUE)
impact_low2= quantile(df_vul_nna$`01/19/19 17:00`, q2,na.rm=TRUE)

res_low1=quantile(df_vul_nna$res, q1,na.rm=TRUE)
res_low2=quantile(df_vul_nna$res, q2,na.rm=TRUE)

inc_imp_ol1 = df_vul[(df_vul$`01/19/19 17:00`<impact_low1)&(df_vul$m_income<inc_low1),]
df_vul$low_imp_inc1=as.numeric((df_vul$`01/19/19 17:00`<impact_low1)&(df_vul$m_income<inc_low1))
inc_res_ol1 = df_vul[(df_vul$res<res_low1)&(df_vul$m_income<inc_low1),]
df_vul$low_res_inc1=as.numeric((df_vul$res<res_low1)&(df_vul$m_income<inc_low1))

inc_imp_ol2 = df_vul[(df_vul$`01/19/19 17:00`<impact_low2)&(df_vul$m_income<inc_low2),]
df_vul$low_imp_inc2=as.numeric((df_vul$`01/19/19 17:00`<impact_low2)&(df_vul$m_income<inc_low2))
inc_res_ol2 = df_vul[(df_vul$res<res_low2)&(df_vul$m_income<inc_low2),]
df_vul$low_res_inc2=as.numeric((df_vul$res<res_low2)&(df_vul$m_income<inc_low2))


write.csv(df_vul,paste0(path,'realtime_input_clv/df_vul_r3.csv'),row.names=FALSE)

pls=c()
mls=c()

mls[[1]] = lm(df_vul$reg_acc ~ df_vul$'01/19/19 17:00')
pls[[1]] <-ggplot(df_vul, aes(x=reg_acc, y=df_vul$'01/19/19 17:00')) + geom_point(size=0.5)+
  theme_bw()+ ggtitle('') + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Normal accessibility") + ylab("Accessibility reduction at t2")+
  stat_smooth(method=lm,se=FALSE)+
  ##additional for revision
  scale_x_continuous(trans=log_trans(), breaks=pretty_breaks())


mls[[2]] = lm(df_vul$reg_acc ~ df_vul$res)
pls[[2]] <-ggplot(df_vul, aes(x=reg_acc, y=res)) + geom_point(size=0.5)+
  theme_bw()+ ggtitle('') + theme(plot.title = element_text(hjust = 0.5))+ylim(c(0.25,NA))+
  xlab("Normal accessibility") + ylab("Resilience")+
  stat_smooth(method=lm,se=FALSE)
  ##additional for revision
  #scale_y_continuous(trans=log_trans(), breaks=pretty_breaks())+
  scale_x_continuous(trans=log_trans(), breaks=pretty_breaks())

mls[[3]] = lm(df_vul$'01/19/19 17:00'~df_vul$m_income)
pls[[3]] <-ggplot() + 
  geom_point(data=df_vul, aes(x=m_income, y=df_vul$'01/19/19 17:00'),size=0.5)+
#  geom_point(aes(x=inc_imp_ol1$m_income, y=inc_imp_ol1$'01/19/19 17:00'),size=1,color='orange') +
#  geom_point(aes(x=inc_imp_ol2$m_income, y=inc_imp_ol2$'01/19/19 17:00'),size=1,color='red') +
  theme_bw()+ ggtitle('') + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Mean income ($)") + ylab("Accessibility reduction at t2") +
  stat_smooth(data=df_vul, aes(x=m_income, y=df_vul$'01/19/19 17:00'),method=lm,se=FALSE)+
  ##additional for revision
  scale_x_continuous(trans=log_trans(), breaks=pretty_breaks(),labels = function(x) format(x, scientific = TRUE))+
  theme(axis.text.x = element_text(angle = 270))


mls[[4]] = lm(df_vul$res ~ df_vul$m_income)
pls[[4]] <-ggplot() + 
  geom_point(data=df_vul, aes(x=m_income, y=res),size=0.5)+
  geom_point(data=inc_res_ol1, aes(x=m_income, y=inc_res_ol1$res),size=1,color='orange') +
  geom_point(data=inc_res_ol2, aes(x=m_income, y=inc_res_ol2$res),size=1,color='red') +
  theme_bw()+ ggtitle('') + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Mean income ($)") + ylab("Resilience") +ylim(c(0.5,NA))+
  stat_smooth(data=df_vul, aes(x=m_income, y=df_vul$res),method=lm,se=FALSE)+
  ##additional for revision
  scale_x_continuous(trans=log_trans(), breaks=pretty_breaks(),labels = function(x) format(x, scientific = TRUE))+
  theme(axis.text.x = element_text(angle = 270))

# mls[[5]] = summary(lm(df_vul$m_income ~ df_vul$Dur_min_rg2))
# pls[[5]] <-ggplot(df_vul, aes(x=m_income, y=Dur_min_rg2)) + geom_point(size=0.5)+
#   theme_bw()+ ggtitle('') + theme(plot.title = element_text(hjust = 0.5))+
#   xlab("Mean income") + ylab("Average accessibility")+
#   geom_smooth(method=lm,se=FALSE)

grid.arrange(grobs = pls[1:4], ncol = 2)
g=arrangeGrob(grobs = pls[1:4], ncol = 2)
ggsave(paste0(outpath,"relations.png"),g, width = 12, height = 8, units = "in",dpi = 600)

# ------------ K-mean clustering -----------------
df_agg_idx_geog2=dcast(df_agg_idx_geog, GEOID_Data ~ time, value.var="diff_bm2")
agg_cluster <- kmeans(df_agg_idx_geog2[, 2:5], 4, nstart = 20)

# Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- df_agg_idx_geog2[,2:5]
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# k-mean clustering
agg_cluster <- kmeans(df_agg_idx_geog2[, 2:5], 5, nstart = 20)
centers=melt(agg_cluster$centers)

# plot the centers
p <- ggplot(data = centers, aes(x=Var2, y=value,group =Var1)) + 
  geom_line(aes(color = Var1))+
  theme_bw()+
  xlab("Time") + ylab("Accessibility Index")+
  theme(axis.title.x = element_text(size=14),
        axis.text.x = element_text(size=10),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=12),
        legend.title =element_text(size=14),
        legend.text=element_text(size=12),
        legend.position="none")
p
