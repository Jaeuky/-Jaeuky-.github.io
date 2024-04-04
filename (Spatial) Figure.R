setwd("D:/jaeuk/대학원/연구/2023 안심지하수/R_안심지하수2023/Spatial analysis/R-spatial-data/nyc")
#Package
{
  library(ggplot2)
  library(rgdal)
  library(tmap)
  library(gstat) # Use gstat's idw routine
  library(sp)    # Used for the spsample function
  library(spatstat)  # Used for the dirichlet tessellation function
  library(maptools)  # Used for conversion from SPDF to ppp
  library(raster)    # Used to clip out thiessen polygons
  library(maps)
  library(mapproj)
}
{
  library(reshape2)
  library(nparcomp)
  library(corrplot)
  library(Hmisc)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(vegan)
  library(cowplot)
  library(party)
  library(caret)
  library(rpart)
  library(e1071)
  library(rpart.plot)
  library(randomForest)
  library(rfUtilities)
  library(ltm)
  library(grid)
  library(lmtest)
  library(gridExtra)
  library(agricolae)
  library(indicspecies)
  library(tidyverse)
  library(showtext)
  library(RColorBrewer)
  library(Hmisc)
  library(MASS)
  library(rcompanion)#tukey normalization
  library(tidyverse)
  library(ggpubr)
  library(rstatix)
  library(datasets)
  library(multcompView)
  library(ggmap)
  library(raster)
  library(ggforce)
}
{
  library(raster)
  library("rgdal")
  library(rspat)
  library(gstat)
  library(isdas)
  library(spatstat)
  library(tidyverse)
  library(leaflet)
  library(magrittr)
  library(remotes)
  library(leaflet.extras)
  library(mapview)
  library(maps)
  library(mapdata)
  library(sp)
  library(giscoR)
  library("rnaturalearth")
  library("rnaturalearthdata")
  library(crosstalk)
  library(isdas)
  library(plotly)
  library(sf)
  library(spdep)
  library(tidyverse)
}
#===============================================================================
#충청남도 면 단위 행정구역 (관정 위치 포함) map
{
map_cn = readOGR("LSMD_ADM_SECT_UMD_44.shp")                 
head(map_cn)
class(map_cn)
map_cn$EMD_NM <- iconv(map_cn$EMD_NM,
                       from = 'CP949',
                       to = 'UTF-8',
                       sub = NA,
                       mark = TRUE,
                       toRaw = FALSE)
# 좌표 변환
to.crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"  
df_map_cn <- spTransform(map_cn, to.crs)

# fortify함수는 shp파일을 R의 데이터프레임으로 바꿔준다(좌표 변환 후 fortify하기) 
df_map_cn = fortify(df_map_cn)   
head(df_map_cn)
# 충청남도 읍면동 지도 시각화
# ggplot(data=df_map_cn, aes(x=long, y=lat, group=group))+
#   geom_polygon(fill='white', color='black')


##### 데이터 합치기
df_map_cn1 = fortify(map_cn)
head(map_cn)
map_cn_info = map_cn@data
head(map_cn_info)

# 0부터 시작하는 연속적인 인덱스 값 만들기
map_cn_info[, "id"] = (1:nrow(map_cn_info)) - 1
# EMD_NM을 읍면동으로 이름 바꾸기
names(map_cn_info)[2] <- c("읍면동")
head(map_cn_info)
# df_map_cn$id 문자열을 숫자로 변환
class(df_map_cn$id)
class(map_cn_info$id)
df_map_cn$id = as.numeric(df_map_cn$id)

# sg_chung = read.csv("충남, 평균후리사(k3).csv", header=T, as.is=T, row.names = 1, na="-", fileEncoding = "CP949", encoding = "UTF-8")
# sg_chung = read.csv("충남, 평균후리사(k4).csv", header=T, as.is=T, row.names = 1, na="-", fileEncoding = "CP949", encoding = "UTF-8")
sg_chung = read.csv("충남, 평균후리사(k5).csv", header=T, as.is=T, row.names = 1, na="-", fileEncoding = "CP949", encoding = "UTF-8")
# sg_chung = read.csv("충남, 평균후리사(k6).csv", header=T, as.is=T, row.names = 1, na="-", fileEncoding = "CP949", encoding = "UTF-8")
head(sg_chung)
str(sg_chung)
dim(sg_chung)

head(map_cn_info)

d <-left_join(map_cn_info,sg_chung, by='읍면동')

head(d)

##### left_join() 
head(df_map_cn)
head(d)
c<-left_join(df_map_cn,d, by='id')  # left_join(x,y, by='')에서 x,y 위치 순서에 따라 데이터 다르게 나온다
#h<-merge(d,df_map_cn, by='id')
head(c)

head(df_map_cn)
# ggplot(data=df_map_cn, aes(x=long, y=lat, group=group))+
#   geom_polygon(fill='white', color='black')+
#   theme_bw()+
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor.y = element_blank())

x = read.csv("안심지하수(17-22, GQI, HRA)GPS,WAWQI수정.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
str(x)
{
  x$No. = as.numeric(x$No.)
  x$Year = as.numeric(x$Year)
  x$latitude = as.numeric(x$latitude)
  x$longitude = as.numeric(x$longitude)
  x$GQI = as.numeric(x$GQI)
  x$General_Bacteria = as.numeric(x$General_Bacteria)
}
str(x)
water = subset(x, x$Age == "Adult")
water = subset(water, water$시도=="충청남도")
summary(water$GQI)
table(water$GQI.rank)

summary(water$HQ.total)
water = subset(water, water$Year == "2017"|water$Year == "2018"| water$Year == "2019"| water$Year == "2020"| water$Year == "2021"| water$Year == "2022")


ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group), color='black', fill="white", linewidth = 1)+ # Total(총인구수)
  geom_point(data = water, aes(x= longitude, y=latitude), size = 2, line = 'black', color = "dodgerblue")+
  theme_bw()+
  ylab("latitude")+
  xlab("longitude")+
  # ggtitle("Local Moran's index significance_WAWQI")+
  theme(panel.grid= element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title=element_text(size=20),
        legend.position = c(0.2,0.22)) +
  guides(fill=guide_legend(title=""))
}
#===============================================================================
#충청남도 수질인자별 기준 초과 빈도
{
x = read.csv("pollutant_info충남.csv")
x = x[c(1:13),]

x$pollutant <- factor(x$pollutant, levels = c("Total coliform","Escherichia coli","General Bacteria","Fecal coliform","NO3-N","Turbidity","As","Fe","Al","F","Mn","pH","Others"))
library(colorspace)
my_palette <- qualitative_hcl(13, "Dynamic")

count = ggplot(x, aes(fill=pollutant, y=num, x=pollutant, palette = "Set1")) + 
  geom_bar(position='stack', stat='identity')+
  theme(panel.background = element_rect(fill="white", color = "black"),
        axis.title.x = element_text(size=0),
        axis.text.x = element_text(size=20,angle=45,hjust=1), axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=25),
        plot.title = element_text(size = 25),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 0),
        legend.position = "NA")+
  scale_fill_manual(values = c("red2","dodgerblue3","forestgreen","darkblue","orange","firebrick4","seagreen","tomato3",
                                     "tan4","mediumvioletred","cornflowerblue","indianred1","darkgray"))+
                                       labs(x='', y='Counts')+ labs(fill = "") +
  scale_x_discrete(labels = c("Total coliform", expression(italic("Escherichia coli")), "General Bacteria", expression(paste("NO"[3],"-N")), "Fecal coliform", "Turbidity", "As", "F", "pH", "Fe", "Al", "Mn", "Others"))
count
}
{
  x = read.csv("pollutant_info충남.csv")
  x = x[-c(1:4),]
  x = x[c(1:9),]
  
  x$pollutant <- factor(x$pollutant, levels = c("NO3-N","Turbidity","As","Fe","Al","F","Mn","pH","Others"))
  library(colorspace)
  my_palette <- qualitative_hcl(9, "Dynamic")
  
  count = ggplot(x, aes(fill=pollutant, y=num, x=pollutant, palette = "Set1")) + 
    geom_bar(position='stack', stat='identity')+
    theme(panel.background = element_rect(fill="white", color = "black"),
          axis.title.x = element_text(size=0),
          axis.text.x = element_text(size=20,angle=45,hjust=1), axis.text.y = element_text(size=20),
          axis.title.y = element_text(size=25),
          plot.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 0),
          legend.position = "NA")+
    scale_fill_manual(values = c("red2","dodgerblue3","forestgreen","darkblue","orange","firebrick4","seagreen","tomato3",
                                       "tan4"))+
                                         labs(x='', y='Counts')+ labs(fill = "") +
    scale_x_discrete(labels = c(expression(paste("NO"[3],"-N")), "Turbidity", "As", "Fe", "Al", "F", "Mn", "pH", "Others"))
  count
}
#===============================================================================
#충청남도 보간법
{
  map_cn = readOGR("LSMD_ADM_SECT_UMD_44.shp")
  to.crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"  
  df_map_cn <- spTransform(map_cn, to.crs)
  head(df_map_cn)
  
  sg = read.csv("안심지하수(17-22, GQI, HRA)GPS,WAWQI수정.csv", header=T, as.is=T, row.names = 1, na="-", fileEncoding = "CP949", encoding = "UTF-8")
  head(sg)
  # sg$Age=="Adult"// sg$Age=="Child"
  sg_cn=subset(sg, sg$시도=="충청남도"& sg$Age=="Adult")
  sg_cn=subset(sg, sg$시도=="충청남도"& sg$Age=="Child")
  max(sg_cn$HQ.total)
  sg3 = subset(sg_cn, select=c("longitude", "latitude","NO3.N","Circumstance","Pass", "Reason.1","WAWQI.12","WAWQI.rank","B","F","General_Bacteria","As","HQ.total","Cl","pH"))
  head(sg3)
  sg4 <- na.omit(sg3[, c("longitude", "latitude","NO3.N","Circumstance","Pass", "Reason.1","WAWQI.12","WAWQI.rank","B","F","General_Bacteria","As","HQ.total","Cl","pH")])
  s_p <- SpatialPointsDataFrame(coords = sg4[, c("longitude", "latitude")], data = sg4[, c("NO3.N","Circumstance","Pass", "Reason.1","WAWQI.12","WAWQI.rank","B","F","General_Bacteria","As","HQ.total","Cl","pH")])
  class(s_p)
  head(s_p)
  s_p@bbox <- df_map_cn@bbox
  
  grd              <- as.data.frame(spsample(s_p, "regular", n=200000))
  names(grd)       <- c("longitude", "latitude")
  coordinates(grd) <- c("longitude", "latitude")
  gridded(grd)     <- TRUE # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  class(grd)
  head(grd)
  
  
  proj4string(s_p) <- proj4string(s_p) 
  proj4string(grd) <- proj4string(s_p) 
  
  #"NO3.N","Circumstance","Pass", "Reason.1","WAWQI.12","WAWQI.rank","B","F","General_Bacteria","As","HQ.total"
  {
    P.idw.WAWQI <- gstat::idw(formula =WAWQI.12 ~ 1, s_p, newdata=grd, idp=2.5)
    
    r.WAWQI       <- raster(P.idw.WAWQI)
    r.m.WAWQI     <- mask(r.WAWQI, df_map_cn)
    
    # WAWQI IDW (충청남도)
    tm_shape(map_cn) +
      tm_borders(col="black",
                 lwd=2)+
      tm_shape(r.m.WAWQI) + 
      tm_raster(n=10,
                palette = colorRampPalette(c("#006400","#9ACD32","yellow2","orange2","red2"))(5),
                auto.palette.mapping = F,
                title="WAWQI",
                labels=c("Excellent", "Good", "Poor", "Very Poor", "Unsuitable"),
                breaks=c(0,25,50,75,100,3000))+
      tm_layout(frame = T,
                main.title= ' ',
                main.title.position=c("center","top"),
                title.size=1,
                legend.position =c("left","bottom"),
                legend.title.size=1.5,
                legend.title.fontface = "bold",
                legend.text.size=1.2,
                # legend.text.fontface = "bold"
      )+
      tm_shape(map_cn) +
      tm_borders(col="black",
                 lwd=1.5)
    
    
    P.idw.HQ.total.ad <- gstat::idw(formula =HQ.total ~ 1, s_p, newdata=grd, idp=5)
    
    r.HQ.total.ad       <- raster(P.idw.HQ.total.ad)
    r.m.HQ.total.ad     <- mask(r.HQ.total.ad, df_map_cn)
    
    # HQ.total.ad IDW (충청남도)
    tm_shape(map_cn) +
      tm_borders(col="black",
                 lwd=2)+
      tm_shape(r.m.HQ.total.ad) + 
      tm_raster(n=10,
                palette = colorRampPalette(c("#006400","#9ACD32","yellow2","orange2","red2"))(5),
                auto.palette.mapping = F,
                title="HQ score",
                labels=c("0.0 - 0.5", "0.5 - 1.0", "1 - 5", "5 - 10", "10 <"),
                breaks=c(0,0.5,1,5,10,10000))+
      tm_layout(frame = T,
                main.title= ' ',
                main.title.position=c("center","top"),
                title.size=1,
                legend.position =c("left","bottom"),
                legend.title.size=1.5,
                legend.title.fontface = "bold",
                legend.text.size=1.2,
                # legend.text.fontface = "bold"
      )+
      tm_shape(map_cn) +
      tm_borders(col="black",
                 lwd=1.5)
    
    P.idw.General_Bacteria <- gstat::idw(formula =General_Bacteria ~ 1, s_p, newdata=grd, idp=10)
    
    r.General_Bacteria      <- raster(P.idw.General_Bacteria)
    r.m.General_Bacteria   <- mask(r.General_Bacteria, df_map_cn)
    
    # General_Bacteria IDW (충청남도)
    tm_shape(map_cn) +
      tm_borders(col="black",
                 lwd=2)+
      tm_shape(r.m.General_Bacteria) + 
      tm_raster(n=10,
                palette = colorRampPalette(c("#006400","#9ACD32","yellow2","orange2","red2"))(5),
                auto.palette.mapping = F,
                title="General Bacteria",
                labels=c("0 - 50", "50 - 100", "100 - 200", "200 - 500", "500 <"),
                breaks=c(0,50,100,200,500,36000))+
      tm_layout(frame = T,
                main.title= ' ',
                main.title.position=c("center","top"),
                title.size=1,
                legend.position =c("left","bottom"),
                legend.title.size=1.75,
                legend.title.fontface = "bold",
                legend.text.size=0.9,
                legend.text.fontface = "bold")+
      tm_shape(map_cn) +
      tm_borders(col="black",
                 lwd=1.5)
    
    P.idw.NO3.N <- gstat::idw(formula =NO3.N ~ 1, s_p, newdata=grd, idp=2)
    
    r.NO3.N      <- raster(P.idw.NO3.N)
    r.m.NO3.N    <- mask(r.NO3.N, df_map_cn)
    
    # NO3.N IDW (충청남도)
    tm_shape(map_cn) +
      tm_borders(col="black",
                 lwd=2)+
      tm_shape(r.m.NO3.N) + 
      tm_raster(n=10,
                palette = colorRampPalette(c("#006400","#9ACD32","yellow2","orange2","red2"))(5),
                auto.palette.mapping = F,
                title="NO3-N",
                labels=c("0 - 5", "5 - 10", "10 - 15", "15 - 20", "20 <"),
                breaks=c(0,5,10,15,20,80))+
      tm_layout(frame = T,
                main.title= ' ',
                main.title.position=c("center","top"),
                title.size=1,
                legend.position =c("left","bottom"),
                legend.title.size=1.75,
                legend.title.fontface = "bold",
                legend.text.size=0.9,
                legend.text.fontface = "bold")+
      tm_shape(map_cn) +
      tm_borders(col="black",
                 lwd=1.5)
    
    P.idw.Cl <- gstat::idw(formula =Cl ~ 1, s_p, newdata=grd, idp=2)
    r.Cl      <- raster(P.idw.Cl)
    r.m.Cl    <- mask(r.Cl, df_map_cn)
    # Cl IDW (충청남도)
    tm_shape(map_cn) +
      tm_borders(col="black",
                 lwd=2)+
      tm_shape(r.m.Cl) + 
      tm_raster(n=10,
                palette = colorRampPalette(c("#006400","#9ACD32","yellow2","orange2","red2"))(5),
                auto.palette.mapping = F,
                title="Cl",
                labels=c("0 - 125", "125 - 250", "250 - 400", "400 - 600", "600 <"),
                breaks=c(0,125,250,400,600,100000))+
      tm_layout(frame = T,
                main.title= ' ',
                main.title.position=c("center","top"),
                title.size=1,
                legend.position =c("left","bottom"),
                legend.title.size=1.75,
                legend.title.fontface = "bold",
                legend.text.size=0.9,
                legend.text.fontface = "bold")+
      tm_shape(map_cn) +
      tm_borders(col="black",
                 lwd=1.5)
    
    P.idw.F <- gstat::idw(formula =F ~ 1, s_p, newdata=grd, idp=2)
    
    r.F     <- raster(P.idw.F)
    r.m.F    <- mask(r.F, df_map_cn)
    
    # F IDW (충청남도)
    tm_shape(map_cn) +
      tm_borders(col="black",
                 lwd=3)+
      tm_shape(r.m.F) + 
      tm_raster(n=10,
                palette = colorRampPalette(c("#006400","#9ACD32","yellow2","orange2","red2"))(5),
                auto.palette.mapping = F,
                title="  F",
                labels=c("0.0 - 0.75", "0.75 - 1.5","1.5 - 3","3 - 5", "5 <"),
                breaks=c(0,0.75,1.5,3,5,100000))+
      tm_layout(frame = T,
                main.title= ' ',
                main.title.position=c("center","top"),
                title.size=1,
                legend.position =c("left","bottom"),
                legend.title.size=1.75,
                legend.title.fontface = "bold",
                legend.text.size=0.9,
                legend.text.fontface = "bold")+
      tm_shape(map_cn) +
      tm_borders(col="black",
                 lwd=1.5)
    
    P.idw.B <- gstat::idw(formula =B ~ 1, s_p, newdata=grd, idp=2)
    r.B      <- raster(P.idw.B)
    r.m.B    <- mask(r.B, df_map_cn)
    # B IDW (충청남도)
    tm_shape(map_cn) +
      tm_borders(col="black",
                 lwd=2)+
      tm_shape(r.m.B) + 
      tm_raster(n=10,
                palette = colorRampPalette(c("#006400","#9ACD32","yellow2","orange2","red2"))(5),
                auto.palette.mapping = F,
                title="B",
                labels=c("0 - 0.5", "0.5 - 1", "1 - 1.5", "1.5 - 2", "2 <"),
                breaks=c(0,0.5,1,1.5,2,100000))+
      tm_layout(frame = T,
                main.title= ' ',
                main.title.position=c("center","top"),
                title.size=1,
                legend.position =c("left","bottom"),
                legend.title.size=1.75,
                legend.title.fontface = "bold",
                legend.text.size=0.9,
                legend.text.fontface = "bold")+
      tm_shape(map_cn) +
      tm_borders(col="black",
                 lwd=1.5)
    
    
    P.idw.pH <- gstat::idw(formula =pH ~ 1, s_p, newdata=grd, idp=2)
    r.pH      <- raster(P.idw.pH)
    r.m.pH    <- mask(r.pH, df_map_cn)
    
    # pH IDW (충청남도)
    tm_shape(map_cn) +
      tm_borders(col="black",
                 lwd=2)+
      tm_shape(r.m.pH) + 
      tm_raster(n=10,
                palette = colorRampPalette(c("red2","yellow2","#006400","yellow2","red2"))(5),
                auto.palette.mapping = F,
                title="pH",
                labels=c("< 5", "5-6", "6-8", "8-9", "9 <"),
                breaks=c(0,5,6,8,9,20))+
      tm_layout(frame = T,
                main.title= ' ',
                main.title.position=c("center","top"),
                title.size=1,
                legend.position =c("left","bottom"),
                legend.title.size=1.75,
                legend.title.fontface = "bold",
                legend.text.size=0.9,
                legend.text.fontface = "bold")+
      tm_shape(map_cn) +
      tm_borders(col="black",
                 lwd=1.5)
    
  }
} 
#===============================================================================
#공간 자기상관 Global Moran 계산 및 산점도
{
  dat <- read.csv("안심지하수(17-22, GQI, HRA)GPS,WAWQI수정.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
  {
    dat$No. = as.numeric(dat$No.)
    dat$Year = as.numeric(dat$Year)
    dat$latitude = as.numeric(dat$latitude)
    dat$longitude = as.numeric(dat$longitude)
    dat$GQI = as.numeric(dat$GQI)
    dat$General_Bacteria = as.numeric(dat$General_Bacteria)
  }
  dat = subset(dat, dat$시도 == "충청남도")
  ad = subset(dat, dat$Age == "Adult")
  ch = subset(dat, dat$Age == "Child")
  
  #일단 0값들을 다 수정 => 공간분석을 위해선 0값 X
  ad[c(17:56)][ad[c(17:56)]==0]<-0.000001
  ch[c(17:56)][ch[c(17:56)]==0]<-0.000001
  str(ad)
  ad.net = ad[complete.cases(ad$longitude), ] #NA는 제거! (3개 행 제거)
  ch.net = ch[complete.cases(ch$longitude), ] #NA는 제거! (3개 행 제거)
  
  #데이터 표준화 (x축 변수= (수질인자 농도-평균)/표준편차) 즉 z-score 사용 -> 평균을 0으로 표준편차를 1로 만든다
  #Adult
  ad.net.s = scale(ad.net[c(17,19,20,22,23:56)])
  ad.net.s = as.data.frame(ad.net.s)
  ad.net = cbind(ad.net[c(1:11)], ad.net.s)
  summary(ad.net$General_Bacteria) #표준화가 잘 됐나 확인
  summary(ad.net$HQ.total) #표준화가 잘 됐나 확인
  #Child
  ch.net.s = scale(ch.net[c(17,19,20,22,23:56)])
  ch.net.s = as.data.frame(ch.net.s)
  ch.net = cbind(ch.net[c(1:11)], ch.net.s)
  summary(ch.net$General_Bacteria) #표준화가 잘 됐나 확인
  summary(ch.net$HQ.total) #표준화가 잘 됐나 확인
  
  #Good
  head(ad.net)
  head(ch.net)
  
  #이웃 관계 리스트 생성하기
  coords.ad <- cbind(ad.net$longitude, ad.net$latitude)
  coords.ch <- cbind(ch.net$longitude, ch.net$latitude)
  nb.ad <- knn2nb(knearneigh(coords.ad, k = 20)) # k-최근접 이웃 방식으로 이웃 관계 리스트 생성 => 고밀도 불균등 분포 데이터
  nb.ch <- knn2nb(knearneigh(coords.ch, k = 20)) # k-최근접 이웃 방식으로 이웃 관계 리스트 생성 => 고밀도 불균등 분포 데이터
  
  #공간 가중치 행렬 생성하기
  # dmat <- as.matrix (dist (cbind (ad.net$longitude, ad.net$latitude)))
  w.ad <- nb2listw (nb.ad, style = "minmax")
  w.ch <- nb2listw (nb.ch, style = "minmax")
  
#=================================================  
  #moran’s index 계산하기

  #NO3-N
  moran.test(ad.net$NO3.N, w.ad)
  mt <- moran.test(ad.net$NO3.N, w.ad)
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score 계산식 적용하기
  print(z_score) # z-score 값 출력하기
  
  #NO3.N
  x = ad.net$NO3.N #수질인자 선정
  wx <- lag.listw(w.ad, x) # 공간적 지연값 
  xwx.lm <- lm(wx ~ x) # 회귀모형 
  infl.xwx <- influence.measures(xwx.lm) # 영향력 측정 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # 영향력이 큰 점들의 인덱스 
  labels <- as.character(ad.net$id) # 레이블
  
  #moran’s index 산점도 그래프 그리기
  NO3.N.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # 데이터 프레임과 매핑 설정 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # 점의 색상을 수동으로 지정
    geom_smooth(method = "lm", se = FALSE) + # 회귀선 표시 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x 선 표시 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x 선 표시 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # 영향력이 큰 점들 표시 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # 영향력이 큰 점들에 레이블 붙이기 
    labs(title = expression(paste("Moran’s index scatterplot (", NO[3]~"-"~N, ")")),
         x = expression(paste("Standardized ", NO[3]~"-"~N)),
         y = expression(paste("Spatially lagged ", NO[3]~"-"~N))) + # y축 레이블 설정 
    theme_bw() + # 테마 설정 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-0.5,10) + ylim(-1,2.4)+
    annotate("text",x=7.5, y=2.1, label = "Moran's I = 0.172", size=7)
  
  NO3.N.moran
  #=================
  #Turbidity
  moran.test(ad.net$Turbidity, w.ad)
  mt <- moran.test(ad.net$Turbidity, w.ad)
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score 계산식 적용하기
  print(z_score) # z-score 값 출력하기
  
  #Turbidity
  x = ad.net$Turbidity #수질인자 선정
  wx <- lag.listw(w.ad, x) # 공간적 지연값 
  xwx.lm <- lm(wx ~ x) # 회귀모형 
  infl.xwx <- influence.measures(xwx.lm) # 영향력 측정 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # 영향력이 큰 점들의 인덱스 
  labels <- as.character(ad.net$id) # 레이블
  
  #moran’s index 산점도 그래프 그리기
  Turbidity.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # 데이터 프레임과 매핑 설정 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # 점의 색상을 수동으로 지정
    geom_smooth(method = "lm", se = FALSE) + # 회귀선 표시 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x 선 표시 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x 선 표시 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # 영향력이 큰 점들 표시 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # 영향력이 큰 점들에 레이블 붙이기 
    labs(title = "Moran’s index scatterplot (Turbidity)", # 제목 설정 
         x = "Standardized Turbidity", # x축 레이블 설정 
         y = "Spatially lagged Turbidity") + # y축 레이블 설정 
    theme_bw() + # 테마 설정 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-2,10) + ylim(-0.3,1)+
    annotate("text",x=7, y=0.9, label = "Moran's I = 0.002", size=7)
  
  Turbidity.moran
  #=================
  #As
  moran.test(ad.net$As, w.ad)
  mt <- moran.test(ad.net$As, w.ad)
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score 계산식 적용하기
  print(z_score) # z-score 값 출력하기
  
  #As
  x = ad.net$As #수질인자 선정
  wx <- lag.listw(w.ad, x) # 공간적 지연값 
  xwx.lm <- lm(wx ~ x) # 회귀모형 
  infl.xwx <- influence.measures(xwx.lm) # 영향력 측정 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # 영향력이 큰 점들의 인덱스 
  labels <- as.character(ad.net$id) # 레이블
  
  #moran’s index 산점도 그래프 그리기
  As.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # 데이터 프레임과 매핑 설정 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # 점의 색상을 수동으로 지정
    geom_smooth(method = "lm", se = FALSE) + # 회귀선 표시 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x 선 표시 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x 선 표시 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # 영향력이 큰 점들 표시 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # 영향력이 큰 점들에 레이블 붙이기 
    labs(title = "Moran’s index scatterplot (As)", # 제목 설정 
         x = "Standardized As", # x축 레이블 설정 
         y = "Spatially lagged As") + # y축 레이블 설정 
    theme_bw() + # 테마 설정 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-2,12) + ylim(-1,4)+ annotate("text",x=8.7, y=3.6, label = "Moran's I = 0.069", size=7)
  
  As.moran
  #=================
  #Fe
  moran.test(ad.net$Fe, w.ad)
  mt <- moran.test(ad.net$Fe, w.ad)
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score 계산식 적용하기
  print(z_score) # z-score 값 출력하기
  
  #Fe
  x = ad.net$Fe #수질인자 선정
  wx <- lag.listw(w.ad, x) # 공간적 지연값 
  xwx.lm <- lm(wx ~ x) # 회귀모형 
  infl.xwx <- influence.measures(xwx.lm) # 영향력 측정 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # 영향력이 큰 점들의 인덱스 
  labels <- as.character(ad.net$id) # 레이블
  
  #moran’s index 산점도 그래프 그리기
  Fe.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # 데이터 프레임과 매핑 설정 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # 점의 색상을 수동으로 지정
    geom_smooth(method = "lm", se = FALSE) + # 회귀선 표시 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x 선 표시 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x 선 표시 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # 영향력이 큰 점들 표시 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # 영향력이 큰 점들에 레이블 붙이기 
    labs(title = "Moran’s index scatterplot (Fe)", # 제목 설정 
         x = "Standardized Fe", # x축 레이블 설정 
         y = "Spatially lagged Fe") + # y축 레이블 설정 
    theme_bw() + # 테마 설정 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-1,4) + ylim(-0.3,0.8)+ annotate("text",x=2.7, y=0.7, label = "Moran's I = 0.004", size=7)
  
  Fe.moran
  #=================
  #Al
  moran.test(ad.net$Al, w.ad)
  mt <- moran.test(ad.net$Al, w.ad)
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score 계산식 적용하기
  print(z_score) # z-score 값 출력하기
  
  #Al
  x = ad.net$Al #수질인자 선정
  wx <- lag.listw(w.ad, x) # 공간적 지연값 
  xwx.lm <- lm(wx ~ x) # 회귀모형 
  infl.xwx <- influence.measures(xwx.lm) # 영향력 측정 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # 영향력이 큰 점들의 인덱스 
  labels <- as.character(ad.net$id) # 레이블
  
  #moran’s index 산점도 그래프 그리기
  Al.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # 데이터 프레임과 매핑 설정 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # 점의 색상을 수동으로 지정
    geom_smooth(method = "lm", se = FALSE) + # 회귀선 표시 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x 선 표시 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x 선 표시 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # 영향력이 큰 점들 표시 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # 영향력이 큰 점들에 레이블 붙이기 
    labs(title = "Moran’s index scatterplot (Al)", # 제목 설정 
         x = "Standardized Al", # x축 레이블 설정 
         y = "Spatially lagged Al") + # y축 레이블 설정 
    theme_bw() + # 테마 설정 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-2,15) + ylim(-0.5,2)+ annotate("text",x=10.5, y=1.85, label = "Moran's I = 0.074", size=7)
  
  Al.moran
  #==============
  #F
  moran.test(ad.net$F, w.ad) 
  mt <- moran.test(ad.net$F, w.ad) 
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score 계산식 적용하기
  print(z_score) # z-score 값 출력하기
  
  #F
  x = ad.net$F #수질인자 선정
  wx <- lag.listw(w.ad, x) # 공간적 지연값 
  xwx.lm <- lm(wx ~ x) # 회귀모형 
  infl.xwx <- influence.measures(xwx.lm) # 영향력 측정 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # 영향력이 큰 점들의 인덱스 
  labels <- as.character(ad.net$id) # 레이블
  #moran’s index 산점도 그래프 그리기
  F.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # 데이터 프레임과 매핑 설정 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # 점의 색상을 수동으로 지정
    geom_smooth(method = "lm", se = FALSE) + # 회귀선 표시 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x 선 표시 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x 선 표시 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # 영향력이 큰 점들 표시 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # 영향력이 큰 점들에 레이블 붙이기 
    labs(title = "Moran’s index scatterplot (F)", # 제목 설정 
         x = "Standardized F", # x축 레이블 설정 
         y = "Spatially lagged F") + # y축 레이블 설정 
    theme_bw() + # 테마 설정 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA") +
    # xlim(-0.5,5) + ylim(-0.3,0.8)+ 
    annotate("text",x=12, y=3.2, label = "Moran's I = 0.171", size=7)
  F.moran
  #==============
  #Mn
  moran.test(ad.net$Mn, w.ad)
  mt <- moran.test(ad.net$Mn, w.ad) 
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score 계산식 적용하기
  print(z_score) # z-score 값 출력하기
  
  #Mn
  x = ad.net$Mn #수질인자 선정
  wx <- lag.listw(w.ad, x) # 공간적 지연값 
  xwx.lm <- lm(wx ~ x) # 회귀모형 
  infl.xwx <- influence.measures(xwx.lm) # 영향력 측정 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # 영향력이 큰 점들의 인덱스 
  labels <- as.character(ad.net$id) # 레이블
  #moran’s index 산점도 그래프 그리기
  Mn.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # 데이터 프레임과 매핑 설정 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # 점의 색상을 수동으로 지정
    geom_smooth(method = "lm", se = FALSE) + # 회귀선 표시 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x 선 표시 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x 선 표시 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # 영향력이 큰 점들 표시 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # 영향력이 큰 점들에 레이블 붙이기 
    labs(title = "Moran’s index scatterplot (Mn)", # 제목 설정 
         x = "Standardized Mn", # x축 레이블 설정 
         y = "Spatially lagged Mn") + # y축 레이블 설정 
    theme_bw() + # 테마 설정 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-2,15) + ylim(-1,3)+ annotate("text",x=10, y=2.7, label = "Moran's I = 0.027", size=7)
  Mn.moran
  #==============
  #pH
  moran.test(ad.net$pH, w.ad)
  mt <- moran.test(ad.net$pH, w.ad) 
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score 계산식 적용하기
  print(z_score) # z-score 값 출력하기
  
  #pH
  x = ad.net$pH #수질인자 선정
  wx <- lag.listw(w.ad, x) # 공간적 지연값 
  xwx.lm <- lm(wx ~ x) # 회귀모형 
  infl.xwx <- influence.measures(xwx.lm) # 영향력 측정 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # 영향력이 큰 점들의 인덱스 
  labels <- as.character(ad.net$id) # 레이블
  #moran’s index 산점도 그래프 그리기
  pH.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # 데이터 프레임과 매핑 설정 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # 점의 색상을 수동으로 지정
    geom_smooth(method = "lm", se = FALSE) + # 회귀선 표시 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x 선 표시 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x 선 표시 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # 영향력이 큰 점들 표시 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # 영향력이 큰 점들에 레이블 붙이기 
    labs(title = "Moran’s index scatterplot (pH)", # 제목 설정 
         x = "Standardized pH", # x축 레이블 설정 
         y = "Spatially lagged pH") + # y축 레이블 설정 
    theme_bw() + # 테마 설정 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-4,5) + ylim(-2,3)+ annotate("text",x=2.8, y=2.7, label = "Moran's I = 0.348", size=7)
  pH.moran
}
#===============================================================================
#Local moran map - 행정구역별 시각화 (행정구역별 수질인자 평균 후 LISA 계산, k = 5), Adult 포함
{
  {
    dat <- read.csv("안심지하수 충남평균(HQ 수정).csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
    {
      dat$No. = as.numeric(dat$No.)
      dat$Year = as.numeric(dat$Year)
      dat$latitude = as.numeric(dat$latitude)
      dat$longitude = as.numeric(dat$longitude)
      dat$GQI = as.numeric(dat$GQI)
      dat$General_Bacteria = as.numeric(dat$General_Bacteria)
    }
    # dat = subset(dat, dat$시도 == "충청남도")
    ad = dat
    # ch = subset(dat, dat$Age == "Child")
    
    #일단 0값들을 다 수정 => 공간분석을 위해선 0값 X
    ad[c(9:48)][ad[c(9:48)]==0]<-0.000001
    # ch[c(10:48)][ch[c(10:48)]==0]<-0.000001
    str(ad)
    ad.net = ad[complete.cases(ad$longitude), ] #NA는 제거! (3개 행 제거)
    # ch.net = ch[complete.cases(ch$longitude), ] #NA는 제거! (3개 행 제거)
    
    #데이터 표준화 (x축 변수= (수질인자 농도-평균)/표준편차) 즉 z-score 사용 -> 평균을 0으로 표준편차를 1로 만든다
    #Adult
    ad.net.s = scale(ad.net[c(9:48)])
    ad.net.s = as.data.frame(ad.net.s)
    ad.net = cbind(ad.net[c(1:8)], ad.net.s)
    summary(ad.net$General_Bacteria) #표준화가 잘 됐나 확인
    summary(ad.net$HQ.total.ad) #표준화가 잘 됐나 확인
    summary(ad.net$HQ.total.ch) #표준화가 잘 됐나 확인
    
    #Good
    head(ad.net)
    
    #이웃 관계 리스트 생성하기
    coords.ad <- cbind(ad.net$longitude, ad.net$latitude)
    
    nb.ad <- knn2nb(knearneigh(coords.ad, k = 5)) # k-최근접 이웃 방식으로 이웃 관계 리스트 생성 => 고밀도 불균등 분포 데이터
    
    #공간 가중치 행렬 생성하기
    # dmat <- as.matrix (dist (cbind (ad.net$longitude, ad.net$latitude)))
    w.ad <- nb2listw (nb.ad, style = "minmax")
    
    lisa.pH <- localmoran(ad.net$pH, w.ad)
    lisa.pH = as.data.frame(lisa.pH)
    colnames(lisa.pH) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.pH$group <- NA # 새로운 열 생성
    lisa.pH$group[lisa.pH$Z.Ii > 0 & lisa.pH$Ii > 0] <- "High-High" # High-High 조건
    lisa.pH$group[lisa.pH$Z.Ii > 0 & lisa.pH$Ii < 0] <- "High-Low" # High-Low 조건
    lisa.pH$group[lisa.pH$Z.Ii < 0 & lisa.pH$Ii > 0] <- "Low-High" # Low-High 조건
    lisa.pH$group[lisa.pH$Z.Ii < 0 & lisa.pH$Ii < 0] <- "Low-Low" # Low-Low 조건
    head(lisa.pH)
    names(lisa.pH)[5] <- c("p.value")
    head(lisa.pH)
    
    lisa.NO3.N <- localmoran(ad.net$NO3.N, w.ad)
    lisa.NO3.N = as.data.frame(lisa.NO3.N)
    colnames(lisa.NO3.N) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.NO3.N$group <- NA # 새로운 열 생성
    lisa.NO3.N$group[lisa.NO3.N$Z.Ii > 0 & lisa.NO3.N$Ii > 0] <- "High-High" # High-High 조건
    lisa.NO3.N$group[lisa.NO3.N$Z.Ii > 0 & lisa.NO3.N$Ii < 0] <- "High-Low" # High-Low 조건
    lisa.NO3.N$group[lisa.NO3.N$Z.Ii < 0 & lisa.NO3.N$Ii > 0] <- "Low-High" # Low-High 조건
    lisa.NO3.N$group[lisa.NO3.N$Z.Ii < 0 & lisa.NO3.N$Ii < 0] <- "Low-Low" # Low-Low 조건
    head(lisa.NO3.N)
    names(lisa.NO3.N)[5] <- c("p.value")
    head(lisa.NO3.N)
    
    lisa.F <- localmoran(ad.net$F, w.ad)
    lisa.F = as.data.frame(lisa.F)
    colnames(lisa.F) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.F$group <- NA # 새로운 열 생성
    lisa.F$group[lisa.F$Z.Ii > 0 & lisa.F$Ii > 0] <- "High-High" # High-High 조건
    lisa.F$group[lisa.F$Z.Ii > 0 & lisa.F$Ii < 0] <- "High-Low" # Low-Low 조건
    lisa.F$group[lisa.F$Z.Ii < 0 & lisa.F$Ii > 0] <- "Low-High" # High-Low 조건
    lisa.F$group[lisa.F$Z.Ii < 0 & lisa.F$Ii < 0] <- "Low-Low" # Low-High 조건
    head(lisa.F)
    names(lisa.F)[5] <- c("p.value")
    head(lisa.F)
    
    lisa.Al <- localmoran(ad.net$Al, w.ad)
    lisa.Al = as.data.frame(lisa.Al)
    colnames(lisa.Al) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.Al$group <- NA # 새로운 열 생성
    lisa.Al$group[lisa.Al$Z.Ii > 0 & lisa.Al$Ii > 0] <- "High-High" # High-High 조건
    lisa.Al$group[lisa.Al$Z.Ii > 0 & lisa.Al$Ii < 0] <- "High-Low" # Low-Low 조건
    lisa.Al$group[lisa.Al$Z.Ii < 0 & lisa.Al$Ii > 0] <- "Low-High" # High-Low 조건
    lisa.Al$group[lisa.Al$Z.Ii < 0 & lisa.Al$Ii < 0] <- "Low-Low" # Low-High 조건
    head(lisa.Al)
    names(lisa.Al)[5] <- c("p.value")
    head(lisa.Al)
    
    lisa.As <- localmoran(ad.net$As, w.ad)
    lisa.As = as.data.frame(lisa.As)
    colnames(lisa.As) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.As$group <- NA # 새로운 열 생성
    lisa.As$group[lisa.As$Z.Ii > 0 & lisa.As$Ii > 0] <- "High-High" # High-High 조건
    lisa.As$group[lisa.As$Z.Ii > 0 & lisa.As$Ii < 0] <- "High-Low" # Low-Low 조건
    lisa.As$group[lisa.As$Z.Ii < 0 & lisa.As$Ii > 0] <- "Low-High" # High-Low 조건
    lisa.As$group[lisa.As$Z.Ii < 0 & lisa.As$Ii < 0] <- "Low-Low" # Low-High 조건
    head(lisa.As)
    names(lisa.As)[5] <- c("p.vAsue")
    head(lisa.As)
    
    lisa.Mn <- localmoran(ad.net$Mn, w.ad)
    lisa.Mn = as.data.frame(lisa.Mn)
    colnames(lisa.Mn) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.Mn$group <- NA # 새로운 열 생성
    lisa.Mn$group[lisa.Mn$Z.Ii > 0 & lisa.Mn$Ii > 0] <- "High-High" # High-High 조건
    lisa.Mn$group[lisa.Mn$Z.Ii > 0 & lisa.Mn$Ii < 0] <- "High-Low" # Low-Low 조건
    lisa.Mn$group[lisa.Mn$Z.Ii < 0 & lisa.Mn$Ii > 0] <- "Low-High" # High-Low 조건
    lisa.Mn$group[lisa.Mn$Z.Ii < 0 & lisa.Mn$Ii < 0] <- "Low-Low" # Low-High 조건
    head(lisa.Mn)
    names(lisa.Mn)[5] <- c("p.value")
    head(lisa.Mn)
    
    lisa.Fe <- localmoran(ad.net$Fe, w.ad)
    lisa.Fe = as.data.frame(lisa.Fe)
    colnames(lisa.Fe) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.Fe$group <- NA # 새로운 열 생성
    lisa.Fe$group[lisa.Fe$Z.Ii > 0 & lisa.Fe$Ii > 0] <- "High-High" # High-High 조건
    lisa.Fe$group[lisa.Fe$Z.Ii > 0 & lisa.Fe$Ii < 0] <- "High-Low" # Low-Low 조건
    lisa.Fe$group[lisa.Fe$Z.Ii < 0 & lisa.Fe$Ii > 0] <- "Low-High" # High-Low 조건
    lisa.Fe$group[lisa.Fe$Z.Ii < 0 & lisa.Fe$Ii < 0] <- "Low-Low" # Low-High 조건
    head(lisa.Fe)
    names(lisa.Fe)[5] <- c("p.value")
    head(lisa.Fe)
    
    lisa.Turbidity <- localmoran(ad.net$Turbidity, w.ad)
    lisa.Turbidity = as.data.frame(lisa.Turbidity)
    colnames(lisa.Turbidity) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.Turbidity$group <- NA # 새로운 열 생성
    lisa.Turbidity$group[lisa.Turbidity$Z.Ii > 0 & lisa.Turbidity$Ii > 0] <- "High-High" # High-High 조건
    lisa.Turbidity$group[lisa.Turbidity$Z.Ii > 0 & lisa.Turbidity$Ii < 0] <- "High-Low" # Low-Low 조건
    lisa.Turbidity$group[lisa.Turbidity$Z.Ii < 0 & lisa.Turbidity$Ii > 0] <- "Low-High" # High-Low 조건
    lisa.Turbidity$group[lisa.Turbidity$Z.Ii < 0 & lisa.Turbidity$Ii < 0] <- "Low-Low" # Low-High 조건
    head(lisa.Turbidity)
    names(lisa.Turbidity)[5] <- c("p.value")
    head(lisa.Turbidity)
    #=======================================
    ad.cn = cbind(ad.net[c(1:8)],
                  lisa.pH[5],lisa.pH[1],lisa.pH[4],lisa.pH[6],
                  lisa.NO3.N[5],lisa.NO3.N[1],lisa.NO3.N[4],lisa.NO3.N[6],
                  lisa.F[5],lisa.F[1],lisa.F[4],lisa.F[6],
                  lisa.Al[5],lisa.Al[1],lisa.Al[4],lisa.Al[6],
                  lisa.As[5],lisa.As[1],lisa.As[4],lisa.As[6],
                  
                  lisa.Mn[5],lisa.Mn[1],lisa.Mn[4],lisa.Mn[6],
                  lisa.Fe[5],lisa.Fe[1],lisa.Fe[4],lisa.Fe[6],
                  lisa.Turbidity[5],lisa.Turbidity[1],lisa.Turbidity[4],lisa.Turbidity[6])
    names(ad.cn)[9] <- c("pH.lisa.p")
    names(ad.cn)[10] <- c("pH.lisa.i")
    names(ad.cn)[11] <- c("pH.lisa.z")
    names(ad.cn)[12] <- c("pH.lisa.r")
    names(ad.cn)[13] <- c("NO3.N.lisa.p")
    names(ad.cn)[14] <- c("NO3.N.lisa.i")
    names(ad.cn)[15] <- c("NO3.N.lisa.z")
    names(ad.cn)[16] <- c("NO3.N.lisa.r")
    names(ad.cn)[17] <- c("F.lisa.p")
    names(ad.cn)[18] <- c("F.lisa.i")
    names(ad.cn)[19] <- c("F.lisa.z")
    names(ad.cn)[20] <- c("F.lisa.r")
    names(ad.cn)[21] <- c("Al.lisa.p")
    names(ad.cn)[22] <- c("Al.lisa.i")
    names(ad.cn)[23] <- c("Al.lisa.z")
    names(ad.cn)[24] <- c("Al.lisa.r")
    names(ad.cn)[25] <- c("As.lisa.p")
    names(ad.cn)[26] <- c("As.lisa.i")
    names(ad.cn)[27] <- c("As.lisa.z")
    names(ad.cn)[28] <- c("As.lisa.r")
    names(ad.cn)[29] <- c("Mn.lisa.p")
    names(ad.cn)[30] <- c("Mn.lisa.i")
    names(ad.cn)[31] <- c("Mn.lisa.z")
    names(ad.cn)[32] <- c("Mn.lisa.r")
    names(ad.cn)[33] <- c("Fe.lisa.p")
    names(ad.cn)[34] <- c("Fe.lisa.i")
    names(ad.cn)[35] <- c("Fe.lisa.z")
    names(ad.cn)[36] <- c("Fe.lisa.r")
    names(ad.cn)[37] <- c("Turbidity.lisa.p")
    names(ad.cn)[38] <- c("Turbidity.lisa.i")
    names(ad.cn)[39] <- c("Turbidity.lisa.z")
    names(ad.cn)[40] <- c("Turbidity.lisa.r")
  }
  
  # write.csv(ad.cn, "충남, 평균후리사(k5)(수질인자들).csv", na="-", fileEncoding = "CP949")
  
  ## 국가공간정보포털에서 충청남도 읍면동 파일 다운로드(로그인 필요) ##
  ##### 충청남도 읍면동 지도
  map_cn = readOGR("LSMD_ADM_SECT_UMD_44.shp")                 
  head(map_cn)
  class(map_cn)
  map_cn$EMD_NM <- iconv(map_cn$EMD_NM,
                         from = 'CP949',
                         to = 'UTF-8',
                         sub = NA,
                         mark = TRUE,
                         toRaw = FALSE)
  # 좌표 변환
  to.crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"  
  df_map_cn <- spTransform(map_cn, to.crs)
  
  # fortify함수는 shp파일을 R의 데이터프레임으로 바꿔준다(좌표 변환 후 fortify하기) 
  df_map_cn = fortify(df_map_cn)   
  head(df_map_cn)
  # 충청남도 읍면동 지도 시각화
  # ggplot(data=df_map_cn, aes(x=long, y=lat, group=group))+
  #   geom_polygon(fill='white', color='black')
  
  
  ##### 데이터 합치기
  df_map_cn1 = fortify(map_cn)
  head(map_cn)
  map_cn_info = map_cn@data
  head(map_cn_info)
  
  # 0부터 시작하는 연속적인 인덱스 값 만들기
  map_cn_info[, "id"] = (1:nrow(map_cn_info)) - 1
  # EMD_NM을 읍면동으로 이름 바꾸기
  names(map_cn_info)[2] <- c("읍면동")
  head(map_cn_info)
  # df_map_cn$id 문자열을 숫자로 변환
  class(df_map_cn$id)
  class(map_cn_info$id)
  df_map_cn$id = as.numeric(df_map_cn$id)
  #==============================================
  sg_chung = read.csv("충남, 평균후리사(k5)(수질인자들).csv", header=T, as.is=T, row.names = 1, na="-", fileEncoding = "CP949", encoding = "UTF-8")
  
  head(sg_chung)
  str(sg_chung)
  dim(sg_chung)
  
  head(map_cn_info)
  
  d <-left_join(map_cn_info,sg_chung, by='읍면동')
  
  head(d)
  
  ##### left_join() 
  head(df_map_cn)
  head(d)
  c<-left_join(df_map_cn,d, by='id')  # left_join(x,y, by='')에서 x,y 위치 순서에 따라 데이터 다르게 나온다
  #h<-merge(d,df_map_cn, by='id')
  head(c)
  #head(h)
  
  head(df_map_cn)
  
  #pH
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=pH.lisa.p), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    # ggtitle("Local Moran's index significance_WAWQI")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = c(0.2,0.22)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("green4", "grey","black"),
                      labels = c("Significant", "Not Significant", "No Samples"))
  
  # High-High는 local moran’s index와 z-score가 모두 양수
  # Low-Low는 local moran’s index와 z-score가 모두 음수
  # High-Low는 local moran’s index는 음수이고 z-score는 양수
  # Low-High는 local moran’s index는 양수이고 z-score는 음수
  
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=pH.lisa.r), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    # ggtitle("Spatial autocorrelation_WAWQI")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = c(0.2,0.27)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("red2","blue","grey","black"),
                      labels = c("High-High", "Low-Low", "Not Significant", "No samples"))
  #================================
  #NO3.N
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=NO3.N.lisa.p), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    # ggtitle("Local Moran's index significance_HQ (Child)")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = c(0.2,0.22)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("green4", "grey","black"),
                      labels = c("Significant", "Not Significant", "No Samples"))
  
  # High-High는 local moran’s index와 z-score가 모두 양수
  # Low-Low는 local moran’s index와 z-score가 모두 음수
  # High-Low는 local moran’s index는 음수이고 z-score는 양수
  # Low-High는 local moran’s index는 양수이고 z-score는 음수
  
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=NO3.N.lisa.r), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    # ggtitle("Spatial autocorrelation_HQ (Child)")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = c(0.2,0.27)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("red2","blue","grey","black"),
                      labels = c("High-High", "Low-Low", "Not Significant", "No samples"))
  #================================
  #F
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=F.lisa.p), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    # ggtitle("Local Moran's index significance_HQ (Child)")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = c(0.2,0.22)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("green4", "grey","black"),
                      labels = c("Significant", "Not Significant", "No Samples"))
  
  # High-High는 local moran’s index와 z-score가 모두 양수
  # Low-Low는 local moran’s index와 z-score가 모두 음수
  # High-Low는 local moran’s index는 음수이고 z-score는 양수
  # Low-High는 local moran’s index는 양수이고 z-score는 음수
  
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=F.lisa.r), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    # ggtitle("Spatial autocorrelation_HQ (Child)")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = c(0.2,0.27)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("red2","blue","grey","black"),
                      labels = c("High-High", "Low-Low", "Not Significant", "No samples"))
  
  #================================
  #Al
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=Al.lisa.p), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    # ggtitle("Local Moran's index significance_Al")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = c(0.2,0.22)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("green4", "grey","black"),
                      labels = c("Significant", "Not Significant", "No Samples"))
  
  # High-High는 local moran’s index와 z-score가 모두 양수
  # Low-Low는 local moran’s index와 z-score가 모두 음수
  # High-Low는 local moran’s index는 음수이고 z-score는 양수
  # Low-High는 local moran’s index는 양수이고 z-score는 음수
  
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=Al.lisa.r), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    # ggtitle("Spatial autocorrelation_Al")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = c(0.2,0.27)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("blue", "grey","grey","black"),
                      labels = c("Low-Low", "Not Significant", "No samples", "No samples"))
  
  #As
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=As.lisa.p), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    # ggtitle("Local Moran's index significance_As")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = c(0.2,0.22)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("green4", "grey","black"),
                      labels = c("Significant", "Not Significant", "No Samples"))
  
  # High-High는 local moran’s index와 z-score가 모두 양수
  # Low-Low는 local moran’s index와 z-score가 모두 음수
  # High-Low는 local moran’s index는 음수이고 z-score는 양수
  # Low-High는 local moran’s index는 양수이고 z-score는 음수
  
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=As.lisa.r), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    # ggtitle("Spatial autocorrelation_As")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = c(0.2,0.27)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("red2","blue","grey","black"),
                      labels = c("High-High", "Low-Low", "Not Significant", "No samples"))
  
  
  #Mn
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=Mn.lisa.p), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    # ggtitle("Local Moran's index significance_Mn")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = c(0.2,0.22)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("green4", "grey","black"),
                      labels = c("Significant", "Not Significant", "No Samples"))
  # High-High는 local moran’s index와 z-score가 모두 양수
  # Low-Low는 local moran’s index와 z-score가 모두 음수
  # High-Low는 local moran’s index는 음수이고 z-score는 양수
  # Low-High는 local moran’s index는 양수이고 z-score는 음수
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=Mn.lisa.r), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    # ggtitle("Spatial autocorrelation_Mn")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = c(0.2,0.27)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("red2","blue","grey","black"),
                      labels = c("High-High", "Low-Low", "Not Significant", "No samples"))
  
  
  #Fe
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=Fe.lisa.p), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    # ggtitle("Local Moran's index significance_Fe")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = c(0.2,0.22)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("green4", "grey","black"),
                      labels = c("Significant", "Not Significant", "No Samples"))
  # High-High는 local moran’s index와 z-score가 모두 양수
  # Low-Low는 local moran’s index와 z-score가 모두 음수
  # High-Low는 local moran’s index는 음수이고 z-score는 양수
  # Low-High는 local moran’s index는 양수이고 z-score는 음수
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=Fe.lisa.r), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    # ggtitle("Spatial autocorrelation_Fe")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = c(0.2,0.27)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("red2","blue","grey","black"),
                      labels = c("High-High", "Low-Low", "Not Significant", "No samples"))
  
  #Turbidity
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=Turbidity.lisa.p), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    # ggtitle("Local Moran's index significance_Turbidity")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = c(0.2,0.22)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("green4", "grey","black"),
                      labels = c("Significant", "Not Significant", "No Samples"))
  # High-High는 local moran’s index와 z-score가 모두 양수
  # Low-Low는 local moran’s index와 z-score가 모두 음수
  # High-Low는 local moran’s index는 음수이고 z-score는 양수
  # Low-High는 local moran’s index는 양수이고 z-score는 음수
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=Turbidity.lisa.r), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    # ggtitle("Spatial autocorrelation_Turbidity")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = c(0.2,0.27)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("red2","blue","grey","black"),
                      labels = c("High-High", "Low-Low", "Not Significant", "No samples"))
}
#===============================================================================
#Correlation length - 준법칙(variogram)-충청남도 전체 데이터로 (근데 Global moran과의 비교라서 이게 맞는듯?)
{
  dat <- read.csv("안심지하수(17-22, GQI, HRA)GPS,WAWQI수정.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
  {
    dat$No. = as.numeric(dat$No.)
    dat$Year = as.numeric(dat$Year)
    dat$latitude = as.numeric(dat$latitude)
    dat$longitude = as.numeric(dat$longitude)
    dat$GQI = as.numeric(dat$GQI)
    dat$General_Bacteria = as.numeric(dat$General_Bacteria)
  }
  dat = subset(dat, dat$시도 == "충청남도")
  ad = subset(dat, dat$Age == "Adult")
  ch = subset(dat, dat$Age == "Child")
  
  #일단 0값들을 다 수정 => 공간분석을 위해선 0값 X
  ad[c(17:56)][ad[c(17:56)]==0]<-0.000001
  ch[c(17:56)][ch[c(17:56)]==0]<-0.000001
  str(ad)
  ad.net = ad[complete.cases(ad$longitude), ] #NA는 제거! (3개 행 제거)
  ch.net = ch[complete.cases(ch$longitude), ] #NA는 제거! (3개 행 제거)
  
  #데이터 표준화 (x축 변수= (수질인자 농도-평균)/표준편차) 즉 z-score 사용 -> 평균을 0으로 표준편차를 1로 만든다
  #Adult
  ad.net.s = scale(ad.net[c(17,19,20,22,23:56)])
  ad.net.s = as.data.frame(ad.net.s)
  ad.net = cbind(ad.net[c(1:11)], ad.net.s)
  summary(ad.net$General_Bacteria) #표준화가 잘 됐나 확인
  summary(ad.net$HQ.total) #표준화가 잘 됐나 확인
  #Child
  ch.net.s = scale(ch.net[c(17,19,20,22,23:56)])
  ch.net.s = as.data.frame(ch.net.s)
  ch.net = cbind(ch.net[c(1:11)], ch.net.s)
  summary(ch.net$General_Bacteria) #표준화가 잘 됐나 확인
  summary(ch.net$HQ.total) #표준화가 잘 됐나 확인
  
  #Good
  head(ad.net)
  head(ch.net)
  
  # ==============================================================================
  #NO3-N
  # 데이터 프레임에서 위치와 수질 지표를 선택합니다
  data <- ad.net[, c("longitude", "latitude", "NO3.N")]
  head(data)
  
  # 데이터 프레임을 SpatialPointsDataFrame으로 변환합니다
  coordinates(data) <- ~longitude+latitude
  
  # 준법칙을 계산합니다
  v <- variogram(NO3.N~1, data = data)
  
  # 준법칙을 그래프로 그립니다
  plot(v)
  
  # 준법칙에서 상관 길이를 추정합니다
  # # 상관 길이는 준법칙이 특정 임계값에 도달하는 거리로 정의됩니다
  # threshold <- 0.05
  
  # 준법칙의 95% 백분위수를 임계값으로 설정
  threshold <- quantile(v$gamma, 0.95)
  cor_length <- min(v$dist[v$gamma > threshold])
  head(cor_length) #단위가 degree
  cor_length_km <- cor_length * 111.32
  head(cor_length_km) #단위 km
  #===============================================================================
  #F
  # 데이터 프레임에서 위치와 수질 지표를 선택합니다
  data <- ad.net[, c("longitude", "latitude", "F")]
  head(data)
  
  # 데이터 프레임을 SpatialPointsDataFrame으로 변환합니다
  coordinates(data) <- ~longitude+latitude
  
  # 준법칙을 계산합니다
  v <- variogram(F~1, data = data)
  
  # 준법칙을 그래프로 그립니다
  plot(v)
  
  # 준법칙에서 상관 길이를 추정합니다
  # # 상관 길이는 준법칙이 특정 임계값에 도달하는 거리로 정의됩니다
  # threshold <- 0.05
  
  # 준법칙의 95% 백분위수를 임계값으로 설정
  threshold <- quantile(v$gamma, 0.95)
  cor_length <- min(v$dist[v$gamma > threshold])
  head(cor_length)
  cor_length_km <- cor_length * 111.32
  head(cor_length_km)
  #===============================================================================
  #Al
  # 데이터 프레임에서 위치와 수질 지표를 선택합니다
  data <- ad.net[, c("longitude", "latitude", "Al")]
  head(data)
  
  # 데이터 프레임을 SpatialPointsDataFrame으로 변환합니다
  coordinates(data) <- ~longitude+latitude
  
  # 준법칙을 계산합니다
  v <- variogram(Al~1, data = data)
  
  # 준법칙을 그래프로 그립니다
  plot(v)
  
  # 준법칙에서 상관 길이를 추정합니다
  # # 상관 길이는 준법칙이 특정 임계값에 도달하는 거리로 정의됩니다
  # threshold <- 0.85
  
  # 준법칙의 95% 백분위수를 임계값으로 설정
  threshold <- quantile(v$gamma, 0.95)
  
  cor_length <- min(v$dist[v$gamma > threshold])
  head(cor_length)
  cor_length_km <- cor_length * 111.32
  head(cor_length_km)
  #===============================================================================
  #As
  # 데이터 프레임에서 위치와 수질 지표를 선택합니다
  data <- ad.net[, c("longitude", "latitude", "As")]
  head(data)
  
  # 데이터 프레임을 SpatialPointsDataFrame으로 변환합니다
  coordinates(data) <- ~longitude+latitude
  
  # 준법칙을 계산합니다
  v <- variogram(As~1, data = data)
  
  # 준법칙을 그래프로 그립니다
  plot(v)
  
  # 준법칙에서 상관 길이를 추정합니다
  # # 상관 길이는 준법칙이 특정 임계값에 도달하는 거리로 정의됩니다
  # threshold <- 0.05
  
  # 준법칙의 95% 백분위수를 임계값으로 설정
  threshold <- quantile(v$gamma, 0.95)
  
  cor_length <- min(v$dist[v$gamma > threshold])
  head(cor_length)
  cor_length_km <- cor_length * 111.32
  head(cor_length_km)
  #===============================================================================
  #Mn
  # 데이터 프레임에서 위치와 수질 지표를 선택합니다
  data <- ad.net[, c("longitude", "latitude", "Mn")]
  head(data)
  
  # 데이터 프레임을 SpatialPointsDataFrame으로 변환합니다
  coordinates(data) <- ~longitude+latitude
  
  # 준법칙을 계산합니다
  v <- variogram(Mn~1, data = data)
  
  # 준법칙을 그래프로 그립니다
  plot(v)
  
  # 준법칙에서 상관 길이를 추정합니다
  # # 상관 길이는 준법칙이 특정 임계값에 도달하는 거리로 정의됩니다
  # threshold <- 0.05
  # 
  
  # 준법칙의 95% 백분위수를 임계값으로 설정
  threshold <- quantile(v$gamma, 0.95)
  
  cor_length <- min(v$dist[v$gamma > threshold])
  head(cor_length)
  cor_length_km <- cor_length * 111.32
  head(cor_length_km)
  #===============================================================================
  #Fe
  # 데이터 프레임에서 위치와 수질 지표를 선택합니다
  data <- ad.net[, c("longitude", "latitude", "Fe")]
  head(data)
  
  # 데이터 프레임을 SpatialPointsDataFrame으로 변환합니다
  coordinates(data) <- ~longitude+latitude
  
  # 준법칙을 계산합니다
  v <- variogram(Fe~1, data = data)
  
  # 준법칙을 그래프로 그립니다
  plot(v)
  
  # 준법칙에서 상관 길이를 추정합니다
  # # 상관 길이는 준법칙이 특정 임계값에 도달하는 거리로 정의됩니다
  # threshold <- 0.05
  
  # 준법칙의 95% 백분위수를 임계값으로 설정
  threshold <- quantile(v$gamma, 0.95)
  
  cor_length <- min(v$dist[v$gamma > threshold])
  head(cor_length)
  cor_length_km <- cor_length * 111.32
  head(cor_length_km)
  #===============================================================================
  #Turbidity
  # 데이터 프레임에서 위치와 수질 지표를 선택합니다
  data <- ad.net[, c("longitude", "latitude", "Turbidity")]
  head(data)
  
  # 데이터 프레임을 SpatialPointsDataFrame으로 변환합니다
  coordinates(data) <- ~longitude+latitude
  
  # 준법칙을 계산합니다
  v <- variogram(Turbidity~1, data = data)
  
  # 준법칙을 그래프로 그립니다
  plot(v)
  
  # 준법칙에서 상관 길이를 추정합니다
  # # 상관 길이는 준법칙이 특정 임계값에 도달하는 거리로 정의됩니다
  # threshold <- 0.05
  
  # 준법칙의 95% 백분위수를 임계값으로 설정
  threshold <- quantile(v$gamma, 0.95)
  cor_length <- min(v$dist[v$gamma > threshold])
  head(cor_length)
  cor_length_km <- cor_length * 111.32
  head(cor_length_km)
  #===============================================================================
  #pH
  # 데이터 프레임에서 위치와 수질 지표를 선택합니다
  data <- ad.net[, c("longitude", "latitude", "pH")]
  head(data)
  
  # 데이터 프레임을 SpatialPointsDataFrame으로 변환합니다
  coordinates(data) <- ~longitude+latitude
  
  # 준법칙을 계산합니다
  v <- variogram(pH~1, data = data)
  
  # 준법칙을 그래프로 그립니다
  plot(v)
  
  # 준법칙에서 상관 길이를 추정합니다
  # # 상관 길이는 준법칙이 특정 임계값에 도달하는 거리로 정의됩니다
  # threshold <- 0.05
  
  # 준법칙의 95% 백분위수를 임계값으로 설정
  threshold <- quantile(v$gamma, 0.95)
  cor_length <- min(v$dist[v$gamma > threshold])
  head(cor_length)
  cor_length_km <- cor_length * 111.32
  head(cor_length_km)
  #===============================================================================
  #Correlation length 시각화
  
  # 데이터 불러오기
  cor <- read.csv("Correlation length(충남).csv")
  head(cor)
  # 변수 순서 설정
  # cor$Water.quality.parameters <- factor(cor$Water.quality.parameters, levels = c("pH", "NO3-N", "F", "Al", "As", "Mn", "Fe", "Turbidity"))
  cor$Water.quality.parameters <- factor(cor$Water.quality.parameters, levels = c("NO3-N", "F", "Mn", "pH", "Al", "As", "Fe", "Turbidity"))
  
  # Moran Index와 Correlation Length를 하나의 그래프로 표현
  ggplot(cor, aes(x=Water.quality.parameters)) +
    geom_bar(aes(y=correlation.length.total.km), stat="identity", fill="steelblue4", width = 0.5, color="black") +
    # annotation_custom(stripe, ymin=-Inf, ymax=Inf) +
    geom_line(aes(y=I*200), color="firebrick1", group=1, size = 2) +
    geom_point(aes(y=I*200), color="firebrick1", group=1, size = 5) +
    # scale_y_continuous(sec.axis = sec_axis(~./max(cor$correlation.length.total.km)*max(cor$I), name = "Moran's I")) +
    scale_y_continuous(sec.axis = sec_axis(~./200, name="Global Moran's I"))+
    labs(
      # title="Moran Index and Correlation Length by Variable",
      x="",
      y="Correlation length (km)") +
    theme(axis.title.y.right = element_text(color = "firebrick1"),
          axis.text.y.right = element_text(color = "firebrick1"),
          axis.title.y = element_text(color = "steelblue4")) +
    theme(panel.background = element_rect(fill="white", color = "black"),
          axis.title.x = element_text(size=0),
          axis.text.x = element_text(size=20,angle=50,hjust=1), axis.text.y = element_text(size=20),
          axis.title.y = element_text(size=23),
          plot.title = element_text(size = 0),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 0),
          legend.position = "NA") +
    scale_x_discrete(labels=c(expression(paste("NO"[3],"-N")),"F","Mn", "pH", "Al",  "As", "Fe", "Turbidity")) 
}
#===============================================================================
#충청남도 보간법 (선정된 수질인자들)
{
  map_cn = readOGR("LSMD_ADM_SECT_UMD_44.shp")
  to.crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"  
  df_map_cn <- spTransform(map_cn, to.crs)
  head(df_map_cn)
  
  sg = read.csv("안심지하수(17-22, GQI, HRA)GPS,WAWQI수정.csv", header=T, as.is=T, row.names = 1, na="-", fileEncoding = "CP949", encoding = "UTF-8")
  head(sg)
  # sg$Age=="Adult"// sg$Age=="Child"
  sg_cn=subset(sg, sg$시도=="충청남도"& sg$Age=="Adult")
  sg_cn=subset(sg, sg$시도=="충청남도"& sg$Age=="Child")
  max(sg_cn$HQ.total)
  sg3 = subset(sg_cn, select=c("longitude", "latitude","NO3.N","F","Mn", "pH","Al","As","Fe","Turbidity"))
  head(sg3)
  sg4 <- na.omit(sg3[, c("longitude", "latitude","NO3.N","F","Mn", "pH","Al","As","Fe","Turbidity")])
  s_p <- SpatialPointsDataFrame(coords = sg4[, c("longitude", "latitude")], data = sg4[, c("NO3.N","F","Mn", "pH","Al","As","Fe","Turbidity")])
  class(s_p)
  head(s_p)
  s_p@bbox <- df_map_cn@bbox
  
  grd              <- as.data.frame(spsample(s_p, "regular", n=200000))
  names(grd)       <- c("longitude", "latitude")
  coordinates(grd) <- c("longitude", "latitude")
  gridded(grd)     <- TRUE # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  class(grd)
  head(grd)
  
  
  proj4string(s_p) <- proj4string(s_p) 
  proj4string(grd) <- proj4string(s_p) 
  
  
  #==========================================================================
  P.idw.NO3.N <- gstat::idw(formula =NO3.N ~ 1, s_p, newdata=grd, idp=2)
  
  r.NO3.N      <- raster(P.idw.NO3.N)
  r.m.NO3.N    <- mask(r.NO3.N, df_map_cn)
  
  # NO3.N IDW (충청남도)
  tm_shape(map_cn) +
    tm_borders(col="black",
               lwd=2)+
    tm_shape(r.m.NO3.N) + 
    tm_raster(n=10,
              palette = colorRampPalette(c("#006400","#9ACD32","yellow2","orange2","red2"))(5),
              auto.palette.mapping = F,
              title="NO3-N",
              labels=c("0 - 5", "5 - 10", "10 - 15", "15 - 20", "20 <"),
              breaks=c(0,5,10,15,20,80))+
    tm_layout(frame = T,
              main.title= ' ',
              main.title.position=c("center","top"),
              title.size=1,
              legend.position =c("left","bottom"),
              legend.title.size=1.75,
              legend.title.fontface = "bold",
              legend.text.size=0.9,
              legend.text.fontface = "bold")+
    tm_shape(map_cn) +
    tm_borders(col="black",
               lwd=1.5)
  #---------------------------------------
  P.idw.F <- gstat::idw(formula =F ~ 1, s_p, newdata=grd, idp=2)
  
  r.F     <- raster(P.idw.F)
  r.m.F    <- mask(r.F, df_map_cn)
  
  # F IDW (충청남도)
  tm_shape(map_cn) +
    tm_borders(col="black",
               lwd=3)+
    tm_shape(r.m.F) + 
    tm_raster(n=10,
              palette = colorRampPalette(c("#006400","#9ACD32","yellow2","orange2","red2"))(5),
              auto.palette.mapping = F,
              title="F",
              labels=c("0.0 - 0.75", "0.75 - 1.5","1.5 - 2.5","2.5 - 3.5", "3.5 <"),
              breaks=c(0,0.75,1.5,2.5,3.5,100000))+
    tm_layout(frame = T,
              main.title= ' ',
              main.title.position=c("center","top"),
              title.size=1,
              legend.position =c("left","bottom"),
              legend.title.size=1.75,
              legend.title.fontface = "bold",
              legend.text.size=0.9,
              legend.text.fontface = "bold")+
    tm_shape(map_cn) +
    tm_borders(col="black",
               lwd=1.5)
  #------------------------------------------
  P.idw.Al <- gstat::idw(formula =Al ~ 1, s_p, newdata=grd, idp=2)
  r.Al      <- raster(P.idw.Al)
  r.m.Al    <- mask(r.Al, df_map_cn)
  
  # Al IDW (충청남도)
  tm_shape(map_cn) +
    tm_borders(col="black",
               lwd=2)+
    tm_shape(r.m.Al) + 
    tm_raster(n=10,
              palette = colorRampPalette(c("#006400","#9ACD32","yellow2","orange2","red2"))(5),
              auto.palette.mapping = F,
              title="Al",
              labels=c("0.0 - 0.1", "0.1 - 0.2","0.2 - 0.3","0.3 - 0.4", "0.4 <"),
              breaks=c(0,0.1,0.2,0.3,0.4,100000))+
    tm_layout(frame = T,
              main.title= ' ',
              main.title.position=c("center","top"),
              title.size=1,
              legend.position =c("left","bottom"),
              legend.title.size=1.75,
              legend.title.fontface = "bold",
              legend.text.size=0.9,
              legend.text.fontface = "bold")+
    tm_shape(map_cn) +
    tm_borders(col="black",
               lwd=1.5)
  #------------------------------------------
  P.idw.As <- gstat::idw(formula =As ~ 1, s_p, newdata=grd, idp=2)
  r.As      <- raster(P.idw.As)
  r.m.As    <- mask(r.As, df_map_cn)
  
  # As IDW (충청남도)
  tm_shape(map_cn) +
    tm_borders(col="black",
               lwd=2)+
    tm_shape(r.m.As) + 
    tm_raster(n=10,
              palette = colorRampPalette(c("#006400","#9ACD32","yellow2","orange2","red2"))(5),
              auto.palette.mapping = F,
              title="As",
              labels=c("0.0 - 0.005", "0.005 - 0.01","0.01 - 0.015","0.015 - 0.02", "0.02 <"),
              breaks=c(0,0.005,0.01,0.015,0.02,10000))+
    tm_layout(frame = T,
              main.title= ' ',
              main.title.position=c("center","top"),
              title.size=1,
              legend.position =c("left","bottom"),
              legend.title.size=1.75,
              legend.title.fontface = "bold",
              legend.text.size=0.9,
              legend.text.fontface = "bold")+
    tm_shape(map_cn) +
    tm_borders(col="black",
               lwd=1.5)
  #------------------------------------------
  P.idw.Fe <- gstat::idw(formula =Fe ~ 1, s_p, newdata=grd, idp=2)
  r.Fe      <- raster(P.idw.Fe)
  r.m.Fe    <- mask(r.Fe, df_map_cn)
  
  # Fe IDW (충청남도)
  tm_shape(map_cn) +
    tm_borders(col="black",
               lwd=2)+
    tm_shape(r.m.Fe) + 
    tm_raster(n=10,
              palette = colorRampPalette(c("#006400","#9ACD32","yellow2","orange2","red2"))(5),
              auto.palette.mapping = F,
              title="Fe",
              labels=c("0.0 - 0.15", "0.15 - 0.3","0.3 - 0.45","0.45 - 0.6", "0.6 <"),
              breaks=c(0,0.15,0.3,0.45,0.6,100000))+
    tm_layout(frame = T,
              main.title= ' ',
              main.title.position=c("center","top"),
              title.size=1,
              legend.position =c("left","bottom"),
              legend.title.size=1.75,
              legend.title.fontface = "bold",
              legend.text.size=0.9,
              legend.text.fontface = "bold")+
    tm_shape(map_cn) +
    tm_borders(col="black",
               lwd=1.5)
  
  #------------------------------------------
  P.idw.Turbidity <- gstat::idw(formula =Turbidity ~ 1, s_p, newdata=grd, idp=2)
  r.Turbidity      <- raster(P.idw.Turbidity)
  r.m.Turbidity    <- mask(r.Turbidity, df_map_cn)
  
  # Turbidity IDW (충청남도)
  tm_shape(map_cn) +
    tm_borders(col="black",
               lwd=2)+
    tm_shape(r.m.Turbidity) + 
    tm_raster(n=10,
              palette = colorRampPalette(c("#006400","#9ACD32","yellow2","orange2","red2"))(5),
              auto.palette.mapping = F,
              title="Turbidity",
              labels=c("0.0 - 0.5", "0.5 - 1","1 - 1.5","1.5 - 2", "2 <"),
              breaks=c(0,0.5,1,1.5,2,100000))+
    tm_layout(frame = T,
              main.title= ' ',
              main.title.position=c("center","top"),
              title.size=1,
              legend.position =c("left","bottom"),
              legend.title.size=1.75,
              legend.title.fontface = "bold",
              legend.text.size=0.9,
              legend.text.fontface = "bold")+
    tm_shape(map_cn) +
    tm_borders(col="black",
               lwd=1.5)
  #------------------------------------------
  P.idw.Mn <- gstat::idw(formula =Mn ~ 1, s_p, newdata=grd, idp=2)
  r.Mn      <- raster(P.idw.Mn)
  r.m.Mn    <- mask(r.Mn, df_map_cn)
  
  # Mn IDW (충청남도)
  tm_shape(map_cn) +
    tm_borders(col="black",
               lwd=2)+
    tm_shape(r.m.Mn) + 
    tm_raster(n=10,
              palette = colorRampPalette(c("#006400","#9ACD32","yellow2","orange2","red2"))(5),
              auto.palette.mapping = F,
              title="Mn",
              labels=c("0.0 - 0.15", "0.15 - 0.3","0.3 - 0.45","0.45 - 0.6", "0.6 <"),
              breaks=c(0,0.15,0.3,0.45,0.6,100000))+
    tm_layout(frame = T,
              main.title= ' ',
              main.title.position=c("center","top"),
              title.size=1,
              legend.position =c("left","bottom"),
              legend.title.size=1.75,
              legend.title.fontface = "bold",
              legend.text.size=0.9,
              legend.text.fontface = "bold")+
    tm_shape(map_cn) +
    tm_borders(col="black",
               lwd=1.5)
  
  #--------------------------------------
  P.idw.pH <- gstat::idw(formula =pH ~ 1, s_p, newdata=grd, idp=2)
  r.pH      <- raster(P.idw.pH)
  r.m.pH    <- mask(r.pH, df_map_cn)
  
  # pH IDW (충청남도)
  tm_shape(map_cn) +
    tm_borders(col="black",
               lwd=2)+
    tm_shape(r.m.pH) + 
    tm_raster(n=10,
              palette = colorRampPalette(c("red2","yellow2","#006400","yellow2","red2"))(5),
              auto.palette.mapping = F,
              title="pH",
              labels=c("< 5", "5-6", "6-8", "8-9", "9 <"),
              breaks=c(0,5,6,8,9,20))+
    tm_layout(frame = T,
              main.title= ' ',
              main.title.position=c("center","top"),
              title.size=1,
              legend.position =c("left","bottom"),
              legend.title.size=1.75,
              legend.title.fontface = "bold",
              legend.text.size=0.9,
              legend.text.fontface = "bold")+
    tm_shape(map_cn) +
    tm_borders(col="black",
               lwd=1.5)
  
  
} 
#===============================================================================
#Neighbor and local contribution
{
  dat <- read.csv("안심지하수(17-22, GQI, HRA)GPS,WAWQI수정.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
  {
    dat$No. = as.numeric(dat$No.)
    dat$Year = as.numeric(dat$Year)
    dat$latitude = as.numeric(dat$latitude)
    dat$longitude = as.numeric(dat$longitude)
    dat$GQI = as.numeric(dat$GQI)
    dat$General_Bacteria = as.numeric(dat$General_Bacteria)
  }
  dat = subset(dat, dat$시도 == "충청남도")
  ad = subset(dat, dat$Age == "Adult")
  
  #일단 0값들을 다 수정 => 공간분석을 위해선 0값 X
  ad[c(17:56)][ad[c(17:56)]==0]<-0.000001
  str(ad)
  ad.net = ad[complete.cases(ad$longitude), ] #NA는 제거! (3개 행 제거)
  
  #데이터 표준화 (x축 변수= (수질인자 농도-평균)/표준편차) 즉 z-score 사용 -> 평균을 0으로 표준편차를 1로 만든다
  #Adult
  ad.net.s = scale(ad.net[c(17,19,20,22,23:56)])
  ad.net.s = as.data.frame(ad.net.s)
  ad.net = cbind(ad.net[c(1:11)], ad.net.s)
  summary(ad.net$General_Bacteria) #표준화가 잘 됐나 확인
  summary(ad.net$HQ.total) #표준화가 잘 됐나 확인
  
  #Good
  head(ad.net)
  
  #이웃 관계 리스트 생성하기
  coords.ad <- cbind(ad.net$longitude, ad.net$latitude)
  nb.ad <- knn2nb(knearneigh(coords.ad, k = 20)) # k-최근접 이웃 방식으로 이웃 관계 리스트 생성 => 고밀도 불균등 분포 데이터
  
  #공간 가중치 행렬 생성하기
  w.ad <- nb2listw (nb.ad, style = "minmax")
  
  # 수질 인자 리스트를 생성합니다.
  parameters <- c("NO3.N", "F","Mn","pH","Al","As","Fe","Turbidity")  # 여기에 분석하려는 수질 인자의 이름을 추가하세요.
  
  # 각 수질 인자에 대해 이웃 기여도와 지역 기여도를 계산합니다.
  contributions <- data.frame()
  for (param in parameters) {
    ad.net$lag_param <- lag.listw(w.ad, ad.net[[param]])
    neighbor_contribution <- cor(ad.net[[param]], ad.net$lag_param)
    local_contribution <- 1 - neighbor_contribution
    contributions <- rbind(contributions, data.frame(Parameter = param, 
                                                     Neighbor = neighbor_contribution, 
                                                     Local = local_contribution))
  }
  
  # 데이터를 재구조화하여 ggplot이 사용할 수 있는 형식으로 만듭니다.
  contributions_melt <- melt(contributions, id.vars = "Parameter")
  
  # 이웃 기여도와 지역 기여도를 바 플롯으로 표시합니다.
  
  contributions_melt$Parameter <- factor(contributions_melt$Parameter, levels = c("NO3.N", "F", "Mn", "pH", "Al", "As", "Fe", "Turbidity"))
  head(contributions_melt)
  
  ggplot(contributions_melt, aes(x = Parameter, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "fill", width = 0.5, color="black") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("firebrick2", "steelblue"),
                      labels = c("Neighbor contribution", "Local contribution")) +
    labs(x = "Parameter", y = "Contribution (%)", fill = "") +
    theme_minimal() +
    labs(x = "", y = "Contribution (%)") +
    theme(panel.background = element_rect(fill = "white", color = "black"),  # 배경 실선 제거
          panel.grid.major = element_blank(),  # 주요 그리드 제거
          panel.grid.minor = element_blank(),  # 보조 그리드 제거
          legend.position = "top",  # 범례를 그림 위로 이동
          legend.direction = "horizontal",  # 범례를 가로로 설정
          legend.spacing.x = unit(0.5, "cm"),  # 범례 항목 사이의 간격 조정
          axis.text.x = element_text(size = 18, angle = 50, hjust = 1), 
          axis.text.y = element_text(size = 18),
          axis.title.y = element_text(size = 18,color = "black"),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 0)) +
    scale_x_discrete(labels = c(expression(paste("NO"[3],"-N")), "F", "Mn", "pH", "Al",  "As", "Fe", "Turbidity"))
}
{
  dat <- read.csv("cor,I,contribution(충남).csv")
  head(dat)
  
  dat$Water.quality.parameters <- factor(dat$Water.quality.parameters, levels = c("pH", "F", "NO3-N", "Al", "As", "Mn", "Fe", "Turbidity"))
  
  # Moran Index와 Correlation Length를 하나의 그래프로 표현
  ggplot(dat, aes(x=Water.quality.parameters)) +
    geom_bar(aes(y=Neighbor.contribution), stat="identity", fill="firebrick2", width = 0.5, color="black") +
    # annotation_custom(stripe, ymin=-Inf, ymax=Inf) +
    geom_line(aes(y=I), color="black", group=1, size = 2) +
    geom_point(aes(y=I), color="black", group=1, size = 5) +
    # scale_y_continuous(sec.axis = sec_axis(~./max(cor$correlation.length.total.km)*max(cor$I), name = "Moran's I")) +
    scale_y_continuous(sec.axis = sec_axis(~./1, name="Global Moran's I"))+
    labs(
      # title="Moran Index and Correlation Length by Variable",
      x="",
      y="Neighbor contribution") +
    theme(axis.title.y.right = element_text(color = "black"),
          axis.text.y.right = element_text(color = "black"),
          axis.title.y = element_text(color = "firebrick2")) +
    theme(panel.background = element_rect(fill="white", color = "black"),
          axis.title.x = element_text(size=0),
          axis.text.x = element_text(size=20,angle=50,hjust=1), axis.text.y = element_text(size=20),
          axis.title.y = element_text(size=23),
          plot.title = element_text(size = 0),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 0),
          legend.position = "NA") +
    scale_x_discrete(labels=c( "pH","F",expression(paste("NO"[3],"-N")), "Al",  "As","Mn", "Fe", "Turbidity")) 
}
#===============================================================================
#Landuse
setwd("D:/jaeuk/대학원/연구/2023 안심지하수/R_안심지하수2023")
#진짜 무수한,, 노력이 있었다, landuse를 계산하고자 하는... QGIS 파일,,, 참고,,,,
{
setwd("D:/jaeuk/대학원/연구/2023 안심지하수/R_안심지하수2023")
dat <- read.csv("충남 전체 교차 영역.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
head(dat)
data = dat[,c(1:7,21,22)]
head(data)

# A17의 각 클래스를 지정된 그룹으로 치환합니다.
data$A17 <- case_when(
  data$A17 %in% c(110,120,130,140,150,160) ~ "Residual",
  data$A17 %in% c(210,220,230,240,310,320,330,410,420,430,440) ~ "Industry",
  data$A17 %in% c(510,520,530,540,610,620,630) ~ "Cropland",
  data$A17 %in% c(550,640,740,750) ~ "Livestock",
  data$A17 %in% c(710,720,730) ~ "Forest",
  data$A17 == 920 ~ "Water",
  TRUE ~ "Others"  # 이외의 값은 모두 'Others'로 치환
)

# 'key'를 기준으로 'A17'의 각 클래스에 대한 'area'의 합계를 계산합니다.
data_sum <- data %>%
  group_by(key, A17) %>%
  summarise(area = sum(area, na.rm = TRUE))

# 'A17'의 각 클래스를 새로운 열로 변환하고, 해당 'area' 값을 채워넣습니다.
data_wide <- data_sum %>%
  spread(key = A17, value = area)

head(data_wide)

# 모든 열에서 NA 값을 0으로 바꿉니다.
data_wide[is.na(data_wide)] <- 0

head(data_wide)

# 'key' 열을 제외한 모든 열의 값을 0.000001265로 곱합니다.
data_wide <- data_wide %>%
  mutate_at(vars(-key), ~ . * 0.000001265)

head(data_wide)

# dat <- read.csv("안심지하수(충남).csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
dat <- read.csv("안심지하수(17-22, GQI, HRA)GPS,WAWQI수정.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
{
  dat$No. = as.numeric(dat$No.)
  dat$Year = as.numeric(dat$Year)
  dat$latitude = as.numeric(dat$latitude)
  dat$longitude = as.numeric(dat$longitude)
  dat$GQI = as.numeric(dat$GQI)
  dat$General_Bacteria = as.numeric(dat$General_Bacteria)
}
dat = subset(dat, dat$시도 == "충청남도")
ad = subset(dat, dat$Age == "Adult")
head(ad)

merged_data <- merge(ad, data_wide, by = "key")

# write.csv(merged_data, "충청남도 전체(landuse 포함).csv", na="-", fileEncoding = "CP949")
}
#-------------------
dat <- read.csv("충청남도 전체(landuse 포함).csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
head(dat)
#===============================================================================
#수질인자와 landuse 상관분석
#Landuse correlation
{
  dat <- read.csv("충청남도 전체(landuse 포함).csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
  {
    dat$No. = as.numeric(dat$No.)
    dat$Year = as.numeric(dat$Year)
    dat$latitude = as.numeric(dat$latitude)
    dat$longitude = as.numeric(dat$longitude)
    dat$GQI = as.numeric(dat$GQI)
    dat$General_Bacteria = as.numeric(dat$General_Bacteria)
  }
  
  # 수질 인자와 landuse 간의 상관 분석을 수행합니다.
  water_quality <- c("NO3.N", "F", "Mn", "pH", "Al", "As", "Fe", "Turbidity")
  landuse <- c("Residential", "Industrial", "Livestock", "Cropland", "Forest", "Water")
  
  correlation_results <- data.frame()
  
  for (wq in water_quality) {
    for (lu in landuse) {
      test_result <- cor.test(dat[[wq]], dat[[lu]], method = "spearman")
      correlation_results <- rbind(correlation_results, data.frame(
        WaterQuality = wq,
        LandUse = lu,
        Correlation = test_result$estimate,
        PValue = test_result$p.value
      ))
    }
  }
  # correlation_results$WaterQuality <- factor(correlation_results$WaterQuality, levels = c("Al", "As", "F", "Fe", "Mn", "NO3.N", "pH", "Turbidity"))
  correlation_results$WaterQuality <- factor(correlation_results$WaterQuality, levels = c("Turbidity", "pH", "NO3.N", "Mn", "Fe", "F", "As", "Al"))
  correlation_results$LandUse <- factor(correlation_results$LandUse, levels = c("Residential", "Industrial", "Livestock", "Cropland", "Forest", "Water"))
  
  head(correlation_results)
  
  # 상관 계수를 시각화합니다.
  ggplot(correlation_results, aes(x = LandUse, y = WaterQuality, fill = Correlation)) +
    geom_tile(color = "black") +
    geom_text(aes(label = ifelse(PValue < 0.05, round(Correlation, 2), "")), 
              color = ifelse(correlation_results$Correlation > 0, "firebrick2", "darkblue"), size = 4.8, fontface = "bold") +
    scale_fill_gradient2(low = "darkblue", high = "firebrick2", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1),
          axis.text.y = element_text(size = 12)) +
    scale_y_discrete(labels = c("Turbidity", "pH", expression(paste("NO"[3],"-N")),"Mn", "Fe", "F",  "As", "Al"))+
    theme(panel.background = element_rect(fill = "white", color = "white"),  # 배경 실선 제거
          panel.grid.major = element_blank(),  # 주요 그리드 제거
          panel.grid.minor = element_blank(),  # 보조 그리드 제거
          legend.position = "NA",  # 범례를 그림 위로 이동
          legend.direction = "horizontal",  # 범례를 가로로 설정
          axis.text.x = element_text(size = 16, angle = 50, hjust = 1),
          axis.text.y = element_text(size = 16),
          axis.title.x = element_text(size = 0,color = "black"),
          axis.title.y = element_text(size = 0,color = "black"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 0))
}
#----------------------
#Index와 landuse 상관분석
{
  dat <- read.csv("충청남도 전체(landuse 포함).csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
  {
    dat$No. = as.numeric(dat$No.)
    dat$Year = as.numeric(dat$Year)
    dat$latitude = as.numeric(dat$latitude)
    dat$longitude = as.numeric(dat$longitude)
    dat$GQI = as.numeric(dat$GQI)
    dat$General_Bacteria = as.numeric(dat$General_Bacteria)
  }
  
  head(dat)
  # 수질 인자와 landuse 간의 상관 분석을 수행합니다.
  # index <- c("WAWQI.12", "HQ.total.adult", "HQ.total.child", "DRASTIC")
  index <- c("WAWQI.12", "HQ.total.adult", "HQ.total.child")
  landuse <- c("Residential", "Industrial", "Livestock", "Cropland", "Forest", "Water")
  
  correlation_results <- data.frame()
  
  for (index in index) {
    for (lu in landuse) {
      test_result <- cor.test(dat[[index]], dat[[lu]], method = "spearman")
      correlation_results <- rbind(correlation_results, data.frame(
        index = index,
        LandUse = lu,
        Correlation = test_result$estimate,
        PValue = test_result$p.value
      ))
    }
  }
  # correlation_results$index <- factor(correlation_results$index, levels = c("WAWQI.12", "HQ.total.adult", "HQ.total.child"))
  # correlation_results$LandUse <- factor(correlation_results$LandUse, levels = c("Water", "Forest", "Cropland", "Livestock", "Industrial", "Residential"))
  
  correlation_results$index <- factor(correlation_results$index, levels = c("HQ.total.child", "HQ.total.adult", "WAWQI.12"))
  correlation_results$LandUse <- factor(correlation_results$LandUse, levels = c("Residential", "Industrial", "Livestock", "Cropland", "Forest", "Water"))
  
  head(correlation_results)
  
  # 상관 계수를 시각화합니다.
  ggplot(correlation_results, aes(x = LandUse, y = index, fill = Correlation)) +
    geom_tile(color = "black") +
    geom_text(aes(label = ifelse(PValue < 0.05, round(Correlation, 2), "")), 
              color = ifelse(correlation_results$Correlation > 0, "firebrick2", "darkblue"), size = 4.8, fontface = "bold") +
    scale_fill_gradient2(low = "darkblue", high = "firebrick2", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1),
          axis.text.y = element_text(size = 12)) +
    scale_x_discrete(labels = c("Residential", "Industrial", "Livestock", "Cropland", "Forest", "Water"))+
    scale_y_discrete(labels = c("HQ (child)","HQ (adult)", "WAWQI"))+
    theme(panel.background = element_rect(fill = "white", color = "white"),  # 배경 실선 제거
          panel.grid.major = element_blank(),  # 주요 그리드 제거
          panel.grid.minor = element_blank(),  # 보조 그리드 제거
          legend.position = "NA",  # 범례를 그림 위로 이동
          legend.direction = "horizontal",  # 범례를 가로로 설정
          axis.text.x = element_text(size = 16, angle = 50, hjust = 1),
          axis.text.y = element_text(size = 16),
          axis.title.x = element_text(size = 0,color = "black"),
          axis.title.y = element_text(size = 0,color = "black"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 0))
  
  
}
#===============================================================================
#중요도 분석
{
  dat <- read.csv("충청남도 전체(landuse 포함).csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
  {
    dat$No. = as.numeric(dat$No.)
    dat$Year = as.numeric(dat$Year)
    dat$latitude = as.numeric(dat$latitude)
    dat$longitude = as.numeric(dat$longitude)
    dat$GQI = as.numeric(dat$GQI)
    dat$General_Bacteria = as.numeric(dat$General_Bacteria)
  }
  
  head(dat)
  
  dat = dat[c(58,31,30,54,55,36,51,56,64,61,62,59,60,65)]
  head(dat)
  
  dat = as.data.frame(dat)
  head(dat)
  mset = dat
  head(mset)
  {mset$NO3.N[mset$NO3.N %in% "NA"] <- NA}
  head(mset)
  mset = mset[complete.cases(mset), ]
  inTraining <- createDataPartition(mset$NO3.N, p = .70, list = FALSE)
  train <- mset[ inTraining,]
  test  <- mset[-inTraining,]
  head(train)
  
  #importance
  #NO3-N
  model.rf.NO3.N <- train(NO3.N ~ Residential + Industrial+Livestock+Cropland+Forest+Water, data = train, method = 'rf',  ntry = 501)
  model.rf.NO3.N
  
  gbmImp.NO3.N <- varImp(model.rf.NO3.N, scale = FALSE)
  gbmImp.NO3.N
  gbmImp.NO3.N = as.data.frame(gbmImp.NO3.N$importance)
  head(gbmImp.NO3.N)
  
  #Al
  model.rf.Al <- train(Al ~ Residential + Industrial+Livestock+Cropland+Forest+Water, data = train, method = 'rf',  ntry = 501)
  model.rf.Al
  
  gbmImp.Al <- varImp(model.rf.Al, scale = FALSE)
  gbmImp.Al
  gbmImp.Al = as.data.frame(gbmImp.Al$importance)
  head(gbmImp.Al)
  
  #As
  model.rf.As <- train(As ~ Residential + Industrial+Livestock+Cropland+Forest+Water, data = train, method = 'rf',  ntry = 501)
  model.rf.As
  
  gbmImp.As <- varImp(model.rf.As, scale = FALSE)
  gbmImp.As
  gbmImp.As = as.data.frame(gbmImp.As$importance)
  head(gbmImp.As)
  
  #F
  model.rf.F <- train(F ~ Residential + Industrial+Livestock+Cropland+Forest+Water, data = train, method = 'rf',  ntry = 501)
  model.rf.F
  
  gbmImp.F <- varImp(model.rf.F, scale = FALSE)
  gbmImp.F
  gbmImp.F = as.data.frame(gbmImp.F$importance)
  head(gbmImp.F)
  
  #Fe
  model.rf.Fe <- train(Fe ~ Residential + Industrial+Livestock+Cropland+Forest+Water, data = train, method = 'rf',  ntry = 501)
  model.rf.Fe
  
  gbmImp.Fe <- varImp(model.rf.Fe, scale = FALSE)
  gbmImp.Fe
  gbmImp.Fe = as.data.frame(gbmImp.Fe$importance)
  head(gbmImp.Fe)
  
  #Mn
  model.rf.Mn <- train(Mn ~ Residential + Industrial+Livestock+Cropland+Forest+Water, data = train, method = 'rf',  ntry = 501)
  model.rf.Mn
  
  gbmImp.Mn <- varImp(model.rf.Mn, scale = FALSE)
  gbmImp.Mn
  gbmImp.Mn = as.data.frame(gbmImp.Mn$importance)
  head(gbmImp.Mn)
  
  #pH
  model.rf.pH <- train(pH ~ Residential + Industrial+Livestock+Cropland+Forest+Water, data = train, method = 'rf',  ntry = 501)
  model.rf.pH
  
  gbmImp.pH <- varImp(model.rf.pH, scale = FALSE)
  gbmImp.pH
  gbmImp.pH = as.data.frame(gbmImp.pH$importance)
  head(gbmImp.pH)
  
  #Turbidity
  model.rf.Turbidity <- train(Turbidity ~ Residential + Industrial+Livestock+Cropland+Forest+Water, data = train, method = 'rf',  ntry = 501)
  model.rf.Turbidity
  
  gbmImp.Turbidity <- varImp(model.rf.Turbidity, scale = FALSE)
  gbmImp.Turbidity
  gbmImp.Turbidity = as.data.frame(gbmImp.Turbidity$importance)
  head(gbmImp.Turbidity)
  
  a = cbind(gbmImp.Al,gbmImp.As,gbmImp.F, gbmImp.Fe,gbmImp.Mn,gbmImp.NO3.N,gbmImp.pH,gbmImp.Turbidity)
  head(a)
  names(a)[1] <- c("Al")
  names(a)[2] <- c("As")
  names(a)[3] <- c("F")
  names(a)[4] <- c("Fe")
  names(a)[5] <- c("Mn")
  names(a)[6] <- c("NO3-N")
  names(a)[7] <- c("pH")
  names(a)[8] <- c("Turbidity")
}
head(a)
# write.csv(a, "충청남도 landuse importance.csv", na="-", fileEncoding = "CP949")
#-------------------------------------------------------------------------------
{
# 데이터를 불러옵니다.
dat <- read.csv("충청남도 landuse importance.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
str(dat)
# 데이터를 0에서 1 사이로 스케일링합니다.
data <- sapply(dat[c(2:9)], rescale, to = c(0, 1))
data <- as.data.frame(data)

# 스케일링된 데이터를 원래의 데이터 프레임에 추가합니다.
dat <- cbind(dat[c(1)], data)

# 스케일링이 잘 되었는지 확인합니다.
summary(dat$Al)
summary(dat$F)

# 스케일링된 데이터를 출력합니다.
head(dat)
}
#===============================================================================
#Statistical summary
{
  setwd("D:/jaeuk/대학원/연구/2023 안심지하수/R_안심지하수2023")
  x <- read.csv("충청남도 전체(landuse 포함)(1).csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
  str(x)
  {
    x$No. = as.numeric(x$No.)
    x$Year = as.numeric(x$Year)
    x$HQ.total.adult = as.numeric(x$HQ.total.adult)
    x$HQ.total.child = as.numeric(x$HQ.total.child)
    x$latitude = as.numeric(x$latitude)
    x$longitude = as.numeric(x$longitude)
    x$GQI = as.numeric(x$GQI)
    x$General_Bacteria = as.numeric(x$General_Bacteria)
    x$SO4 = as.numeric(x$SO4)
    x$DRASTIC = as.numeric(x$DRASTIC)
    x$Cd = as.numeric(x$Cd)
    x$TH = as.numeric(x$TH)
    x$Pb = as.numeric(x$Pb)
  }
  head(x)
  str(x)
  
  summary(x$HQ.total.adult)
  summary(x$HQ.total.child)
  
  summary(x$General_Bacteria)
  summary(x$Pb)
  summary(x$F)
  summary(x$As)
  summary(x$Se)
  summary(x$Hg)
  summary(x$Cr)
  summary(x$NH3.N)
  summary(x$NO3.N)
  summary(x$Cd)
  summary(x$B)
  summary(x$Trichloroethylene)
  summary(x$Dichloromethane)
  summary(x$Benzene)
  summary(x$Toluene)
  summary(x$Ethylbenzene)
  summary(x$Xylene)
  summary(x$X1.1.dichloroethylene)
  summary(x$X1.4.dioxane)
  summary(x$TH)
  summary(x$COD)
  summary(x$Cu)
  summary(x$pH)
  summary(x$Zn)
  summary(x$Cl)
  summary(x$Fe)
  summary(x$Mn)
  summary(x$Turbidity)
  summary(x$SO4)
  summary(x$Al)
  
  sd(x$General_Bacteria)
  sd(x$Pb)
  sd(x$F)
  sd(x$As)
  sd(x$Se)
  sd(x$Hg)
  sd(x$Cr)
  sd(x$NH3.N)
  sd(x$NO3.N)
  sd(x$Cd)
  sd(x$B)
  sd(x$Trichloroethylene)
  sd(x$Dichloromethane)
  sd(x$Benzene)
  sd(x$Toluene)
  sd(x$Ethylbenzene)
  sd(x$Xylene)
  sd(x$X1.1.dichloroethylene)
  sd(x$X1.4.dioxane)
  sd(x$TH)
  sd(x$COD)
  sd(x$Cu)
  sd(x$pH)
  sd(x$Zn)
  sd(x$Cl)
  sd(x$Fe)
  sd(x$Mn)
  sd(x$Turbidity)
  sd(x$SO4)
  sd(x$Al)
}
#===============================================================================




