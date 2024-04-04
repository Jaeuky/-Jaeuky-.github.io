setwd("D:/jaeuk/대학원/연구/2023 안심지하수/R_안심지하수2023/Spatial analysis/R-spatial-data/nyc")
#Package
{
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
  library(ggplot2)
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
  library(dplyr)
  library(rfUtilities)
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
  library(ggplot2)
  library(multcompView)
  library(dplyr)
  library(ggmap)
  library(ggplot2)
  library(dplyr)
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
#남한 관정 point map
{
setwd("D:/jaeuk/대학원/연구/2023 안심지하수/R_안심지하수2023")
register_google(key="AIzaSyAaHbLKP-KubHx-qx8tNmxctR4ya6OVAMw")

# water <- read_excel("안심지하수(17-22, GQI, HRA)melt.xlsx")
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
summary(water$GQI)
table(water$GQI.rank)

summary(water$HQ.total)

water = subset(x, x$Age == "Adult")
water = subset(water, water$Year == "2017"|water$Year == "2018"| water$Year == "2019"| water$Year == "2020")


detach("package:lulcc", unload = TRUE)
detach("package:raster", unload = TRUE)
library(raster)

korea = getData("GADM", country='kor', level=1)
str(water)
water$Year = as.factor(water$Year)

water$latitude = as.numeric(water$latitude)
water$longitude = as.numeric(water$longitude)
str(water)

ggplot()+
  geom_polygon(data = korea, aes(x=long, y=lat, group=group), fill ="white", color = "black")+
  theme_classic()+
  xlim(125,132.3)+
  ylim(33,39) +
  geom_point(data = water, aes(x= longitude, y=latitude, color=factor(Year)), size = 1)+
  theme(#legend.position = c(130,35),
    legend.text = element_text(size=20),
    legend.key.size = unit(2,"cm"),
    legend.key.height = unit(2,"cm"),
    legend.title=element_blank(),
    axis.text = element_text(size=15),
    axis.title = element_text(size=18)) +
  xlab("longitude")+
  ylab("latitude") +
  scale_color_manual(values = c("#ED4B48","#4488FF","#98CB30","#F7CA4D","#666666","sienna"))
}
#===============================================================================
#Statistical summary
{
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

ad = subset(x, x$Age == "Adult")
ch = subset(x, x$Age == "Child")
Pass.ad = subset(ad, ad$Pass == "Pass")
Pass.ch = subset(ch, ch$Pass == "Pass")

summary(ad$HQ.total)
summary(ch$HQ.total)

summary(ad$CR.total)
summary(ch$CR.total)

table(ad$HQ.std)
table(ch$HQ.std)

summary(ad$General_Bacteria)
summary(ad$Pb)
summary(ad$F)
summary(ad$As)
summary(ad$Se)
summary(ad$Hg)
summary(ad$Cr)
summary(ad$NH3.N)
summary(ad$NO3.N)
summary(ad$Cd)
summary(ad$B)
summary(ad$Trichloroethylene)
summary(ad$Dichloromethane)
summary(ad$Benzene)
summary(ad$Toluene)
summary(ad$Ethylbenzene)
summary(ad$Xylene)
summary(ad$X1.1.dichloroethylene)
summary(ad$X1.4.dioxane)
summary(ad$TH)
summary(ad$COD)
summary(ad$Cu)
summary(ad$pH)
summary(ad$Zn)
summary(ad$Cl)
summary(ad$Fe)
summary(ad$Mn)
summary(ad$Turbidity)
summary(ad$SO4)
summary(ad$Al)

sd(ad$General_Bacteria)
sd(ad$Pb)
sd(ad$F)
sd(ad$As)
sd(ad$Se)
sd(ad$Hg)
sd(ad$Cr)
sd(ad$NH3.N)
sd(ad$NO3.N)
sd(ad$Cd)
sd(ad$B)
sd(ad$Trichloroethylene)
sd(ad$Dichloromethane)
sd(ad$Benzene)
sd(ad$Toluene)
sd(ad$Ethylbenzene)
sd(ad$Xylene)
sd(ad$X1.1.dichloroethylene)
sd(ad$X1.4.dioxane)
sd(ad$TH)
sd(ad$COD)
sd(ad$Cu)
sd(ad$pH)
sd(ad$Zn)
sd(ad$Cl)
sd(ad$Fe)
sd(ad$Mn)
sd(ad$Turbidity)
sd(ad$SO4)
sd(ad$Al)

table(ad$WAWQI.rank)

ad.HQ.up = subset(ad, ad$HQ.std == "High Risk")
ad.HQ.down = subset(ad, ad$HQ.std == "Low Risk")

ch.HQ.up = subset(ch, ch$HQ.std == "High Risk")
ch.HQ.down = subset(ch, ch$HQ.std == "Low Risk")

summary(ad.HQ.down$HQ.total)
summary(ad.HQ.up$HQ.total)

summary(ch.HQ.down$HQ.total)
summary(ch.HQ.up$HQ.total)

WAWQI.Excellent = subset(x, x$WAWQI.rank == "Excellent water quality")
WAWQI.Good = subset(x, x$WAWQI.rank == "Good water quality")
WAWQI.Poor = subset(x, x$WAWQI.rank == "Poor water quality")
WAWQI.Verypoor = subset(x, x$WAWQI.rank == "Very poor water quality")
WAWQI.Unsuitable = subset(x, x$WAWQI.rank == "Unsuitable for drinking")

summary(WAWQI.Excellent$WAWQI.12)
summary(WAWQI.Good$WAWQI.12)
summary(WAWQI.Poor$WAWQI.12)
summary(WAWQI.Verypoor$WAWQI.12)
summary(WAWQI.Unsuitable$WAWQI.12)

summary(ad$CR.total)
summary(ch$CR.total)
}
#===============================================================================
#연도별 적부 비율
{
x = read.csv("안심지하수(17-22, GQI, HRA)melt.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
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

ad = subset(x, x$Age == "Adult")
ch = subset(x, x$Age == "Child")
Pass.ad = subset(ad, ad$Pass == "Pass")
Pass.ch = subset(ch, ch$Pass == "Pass")

ad = droplevels(ad)
table(ad$Year)
table(ad$Pass, ad$Year)

ggplot(ad, aes(x=Year, fill = factor(Pass)))+
  geom_bar()+
  theme_bw()+
  ylab("Number of wells")+
  ylim(0,2500)+
  scale_x_continuous(breaks = c(2017,2018,2019,2020,2021,2022)) +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.88,0.92),
        legend.key.size = unit(1 , "cm"),
        legend.text = element_text(size = 20))+
  scale_fill_manual(values = c("#D95252","#4B5AE0"))+
  #annotate("text",x=2017, y=2150, label="2061", size= 10)+
  annotate("text",x=2017, y=1400, label="65.2 %", size= 8)+
  annotate("text",x=2017, y=350, label="34.8 %", size= 8)+
  #annotate("text",x=2018, y=2230, label="2142", size= 10)+
  annotate("text",x=2018, y=1470, label="64.3 %", size= 8)+
  annotate("text",x=2018, y=380, label="35.7 %", size= 8)+
  #annotate("text",x=2019, y=2110, label="2019", size= 10)+
  annotate("text",x=2019, y=1510, label="46.0 %", size= 8)+
  annotate("text",x=2019, y=545, label="54.0 %", size= 8) +
  annotate("text",x=2020, y=1500, label="53.5 %", size= 8) +
  annotate("text",x=2020, y=450, label="46.5 %", size= 8) +
  annotate("text",x=2021, y=1550, label="52.5 %", size= 8) +
  annotate("text",x=2021, y=500, label="47.5 %", size= 8) +
  annotate("text",x=2022, y=1450, label="59.1 %", size= 8) +
  annotate("text",x=2022, y=450, label="40.9 %", size= 8)
}
#===============================================================================
#연도별 부적합 원인
{
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
ad = subset(x, x$Age == "Adult")
ch = subset(x, x$Age == "Child")

nnp = subset(ad, ad$Pass == "Not Pass")
table(nnp$Year)
table(nnp$Year,nnp$Reason.1)
nnp$Reason.1 = factor(nnp$Reason.1, levels = c("Organic","Aesthetic Impact","Inorganic","Microorganism","Complex"))

ggplot(data = nnp, aes(x=Year, fill =Reason.1)) +
  geom_bar(position = "stack")+
  #ggtitle("연도별 부적합 원인")+
  theme_bw()+
  xlab("")+
  ylab("Number of wells")+
  ylim(0,1800)+
  scale_x_continuous(breaks = c(2017,2018,2019,2020,2021,2022)) +
  theme(axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.82,0.84), #,"none"
        legend.key.size = unit(1 , "cm"),
        legend.text = element_text(size = 18))+
  scale_fill_manual(values = c("#4F4659","#6374A6","#D9D273","#F2A057","#D95252") )+
  annotate("text",x=2017, y=250, label = "34.6%", size=8)+
  annotate("text",x=2017, y=750, label = "42.3%", size=8)+
  annotate("text",x=2017, y=1150, label = "18.4%", size=8)+
  annotate("text",x=2017, y=1380, label = "4.8%", size=8)+
  annotate("text",x=2018, y=230, label = "32.0%", size=8)+
  annotate("text",x=2018, y=740, label = "43.1%", size=8)+
  annotate("text",x=2018, y=1160, label = "20.5%", size=8)+
  annotate("text",x=2018, y=1390, label = "4.3%", size=8)+
  annotate("text",x=2019, y=110, label = "23.5%", size=8)+
  annotate("text",x=2019, y=420, label = "43.1%", size=8)+
  annotate("text",x=2019, y=760, label = "31.4%", size=8)+
  annotate("text",x=2019, y=960, label = "2.0%", size=8)+
  annotate("text",x=2020, y=170, label = "29.6%", size=8)+
  annotate("text",x=2020, y=600, label = "45.8%", size=8)+
  annotate("text",x=2020, y=970, label = "22.6%", size=8)+
  annotate("text",x=2020, y=1155, label = "2.0%", size=8)+
  annotate("text",x=2021, y=100, label = "21.9%", size=8)+
  annotate("text",x=2021, y=500, label = "55.7%", size=8)+
  annotate("text",x=2021, y=865, label = "20.5%", size=8)+
  annotate("text",x=2021, y=1030, label = "1.8%", size=8)+
  annotate("text",x=2022, y=170, label = "26.5%", size=8)+
  annotate("text",x=2022, y=670, label = "52.0%", size=8)+
  annotate("text",x=2022, y=1100, label = "18.7%", size=8)+
  annotate("text",x=2022, y=1280, label = "2.8%", size=8)
}
#===============================================================================
#수질인자별 기준 초과 빈도
setwd("D:/jaeuk/대학원/연구/2023 안심지하수/R_안심지하수2023")
{
x = read.csv("pollutant_info.csv")
head(x)
x$pollutant <- factor(x$pollutant, levels = c("Total coliform","Escherichia coli","General Bacteria","NO3-N","Fecal coliform","Turbidity","As","F","pH","Fe","Al","Mn","Others"))

x$pollutant <- factor(x$pollutant, levels = c(expression(paste("Total coliform")),
                                              expression(paste(italic("Escherichia coli"))),
                                              expression(paste("General Bacteria")),
                                              expression(paste("NO"[3],"-N")),
                                              expression(paste("Fecal coliform")),
                                              expression(paste("Turbidity")),
                                              expression(paste("As")),
                                              expression(paste("F")),
                                              expression(paste("pH")),
                                              expression(paste("Fe")),
                                              expression(paste("Al")),
                                              expression(paste("Mn")),
                                              expression(paste("Others"))))

library(colorspace)
my_palette <- qualitative_hcl(13, "Dynamic")

# my_palette <- rainbow_hcl(13, c = 300)
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
                                       # ggtitle("수질인자별 지하수 수질 기준 초과 빈도")+ 
  labs(x='', y='Counts')+ labs(fill = "") 
# count = set_palette(count, "Plotting")
count

x = read.csv("pollutant_info.csv")

x$pollutant <- factor(x$pollutant, levels = c("Total coliform","Escherichia coli","General Bacteria","NO3-N","Fecal coliform","Turbidity","As","F","pH","Fe","Al","Mn","Others"))

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
#===============================================================================
#WAWQI 등급별 관정수(barplot)
{
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

ad = subset(x, x$Age == "Adult")
ch = subset(x, x$Age == "Child")

summary(ad$WAWQI.rank)
table(ad$WAWQI.rank)
a = table(ad$WAWQI.rank)
head(a)
a = as.data.frame(a)

a$Var1 <- factor(a$Var1, levels = c("Unsuitable for drinking","Very poor water quality", "Poor water quality",
                                    "Good water quality", "Excellent water quality"))

count.a = ggplot(a, aes(fill=Var1, y=Freq, x=Var1, palette = "Set1")) + 
  geom_bar(position='stack', stat='identity')+
  theme(panel.background = element_rect(fill="white", color = "black"),
        axis.title.x = element_text(size=0),
        axis.text.x = element_text(size=20,angle=50,hjust=1), axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=25),
        plot.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 0),
        legend.position = "NA") +
  ggtitle("WAWQI 등급별 관정수")+ 
  labs(x='WAWQI rank', y='Number of wells')+ labs(fill = "")
count.a =set_palette(count.a, "Set1")
count.a

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

ad = subset(x, x$Age == "Adult")
ch = subset(x, x$Age == "Child")

summary(ad$WAWQI.rank)
table(ad$WAWQI.rank)
a = table(ad$WAWQI.rank)
head(a)
a = as.data.frame(a)

a$Var1 <- factor(a$Var1, levels = c("Excellent water quality", "Good water quality","Poor water quality",
                                    "Very poor water quality", "Unsuitable for drinking"),
                 labels = c("Excellent", "Good",  "Poor", "Very poor","Unsuitable"))

count.a = ggplot(a, aes(fill=Var1, y=Freq, x=Var1, palette = "Set1")) + 
  geom_bar(position='stack', stat='identity')+
  theme(panel.background = element_rect(fill="white", color = "black"),
        axis.title.x = element_text(size=0),
        axis.text.x = element_text(size=20,angle=50,hjust=1), axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=25),
        plot.title = element_text(size = 0),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 0),
        legend.position = "NA") +
  # ggtitle("WAWQI 등급별 관정수")+ 
  labs(x='WAWQI rank', y='Number of wells')+ labs(fill = "")
count.a =set_palette(count.a, "Set1")
count.a
}
#===============================================================================
#WAWQI 점수별 관정수(histogram)
{
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

ad = subset(x, x$Age == "Adult")
ch = subset(x, x$Age == "Child")
Pass.ad = subset(ad, ad$Pass == "Pass")
Pass.ch = subset(ch, ch$Pass == "Pass")

# library(ggbreak)
# WAWQI score Histogram
ggplot(ad, aes(x=WAWQI.12))+
  geom_histogram(colour = "white", binwidth = 10, fill = "Orange")+
  # geom_vline(xintercept = c(0.89,0.94), colour = "Grey1")+
  theme_bw()+
  ylab("Number of wells")+
  xlab("WAWQI score")+
  theme(panel.grid= element_blank(),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        title = element_text(size=25))+
  xlim(-5,200)+
  scale_y_break(c(600,1000), scales = c(0.4, 1000), expand = T, space = -1)+
  geom_vline(xintercept = 25, colour = "black", size = 1.5,  lty = "dashed")+
  geom_vline(xintercept = 50, colour = "black", size = 1.5,  lty = "dashed")+
  geom_vline(xintercept = 75, colour = "black", size = 1.5,  lty = "dashed")+
  geom_vline(xintercept = 100, colour = "black", size = 1.5,  lty = "dashed")
}
#===============================================================================
#WAWQI, HQ 점수 통계량
{
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
ad = subset(x, x$Age == "Adult")
ch = subset(x, x$Age == "Child")

summary(ad$HQ.total)
summary(ch$HQ.total)

summary(ad$CR.total)
summary(ch$CR.total)

summary(ad$WAWQI.12)
table(ad$WAWQI.rank)
}
#===============================================================================
#IDW 전국 시각화 map
{
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
  x = x[-16557,] #경북 석포면? 제거
  Q4 = subset(x, x$Age == "Child")
  Q4 = subset(x, x$Age == "Adult")
  Q4.s = subset(Q4, select=c("longitude", "latitude","WAWQI.12","WAWQI.rank",
                             "HQ.total","DRASTIC","General_Bacteria","pH","NO3.N","B","F","Cl"))
  
  head(Q4.s)
  
  Q4.s = Q4.s[complete.cases(Q4.s$longitude), ]
  class(Q4.s)
  
  s_p <- SpatialPointsDataFrame(coords = Q4.s[, c("longitude", "latitude")], data = Q4.s[, c("WAWQI.12","WAWQI.rank",
                                                                                             "HQ.total","DRASTIC","General_Bacteria","pH","NO3.N","B","F","Cl")])
  head(s_p)
  
  # ?getData
  
  ### P -> s_p, W -> korea ###
  ## Replace point boundary extent with that of Texas
  #raster package 껐다 켜기
  korea <- getData('GADM', country='kor', level=2)
  s_p@bbox <- korea@bbox
  # plot(korea_1)
  # tm_shape(korea) + tm_polygons() +
  #   tm_shape(s_p) +
  #   tm_dots(col="WAWQI.13.Q", palette = "YlOrRd", auto.palette.mapping = FALSE,size=0.5)+
  #   tm_legend(legend.outside=TRUE)
  
  ### P -> s_p, W -> korea ###
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(s_p, "regular", n=250000))
  names(grd)       <- c("longitude", "latitude")
  coordinates(grd) <- c("longitude", "latitude")
  gridded(grd)     <- TRUE # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  head(grd)
  
  
  # Add P's projection information to the empty grid
  proj4string(s_p) <- proj4string(s_p) # Temp fix until new proj env is adopted
  proj4string(grd) <- proj4string(s_p) 
  
  # "WAWQI","Age","HQ.total","NO3.N","Cl","F","As"
  # Interpolate the grid cells using a power value of 2 
  P.idw <- gstat::idw(formula =WAWQI.12 ~ 1, s_p, newdata=grd, idp=2)
  head(P.idw)
  # Convert to raster object then clip to Texas
  r       <- raster(P.idw)
  r.m     <- mask(r, korea)
  head(r)
  head(r.m)
  # Plot
  tm_shape(r.m) + 
    tm_raster(n=5, style = "fixed", 
              palette = c("green4","limegreen","yellow2","orange2","red2"),
              title="",
              breaks=c(0,25,50,75,100,2000))+
    tm_layout(main.title= 'WAWQI의 공간분포',
              main.title.size = 0,
              legend.position = c("right", "bottom"))+
    # tm_shape(s_p)
    # tm_dots(size=0.01,shape = 2)+
    tm_shape(korea)+
    tm_borders(col="black",lwd=2)
  
  
  P.idw <- gstat::idw(formula =HQ.total ~ 1, s_p, newdata=grd, idp=5)
  head(P.idw)
  # Convert to raster object then clip to Texas
  r       <- raster(P.idw)
  r.m     <- mask(r, korea)
  head(r)
  head(r.m)
  # Plot
  tm_shape(r.m) + 
    tm_raster(n=5, style = "fixed", 
              palette = c("green4","limegreen","yellow2","orange2","red2"),
              title="",
              breaks=c(0,0.5,1,5,10,2000))+
    tm_layout(main.title= 'HQ 점수의 공간분포',
              main.title.size = 0,
              legend.position = c("right", "bottom"))+
    # tm_shape(s_p)
    # tm_dots(size=0.01,shape = 2)+
    tm_shape(korea)+
    tm_borders(col="black",lwd=2)
  
  P.idw <- gstat::idw(formula =NO3.N ~ 1, s_p, newdata=grd, idp=3)
  head(P.idw)
  # Convert to raster object then clip to Texas
  r       <- raster(P.idw)
  r.m     <- mask(r, korea)
  head(r)
  head(r.m)
  # Plot
  tm_shape(r.m) + 
    tm_raster(title="NO3-N", breaks=c(0,5,10,15,20,1000),
              palette = c("green4","limegreen","yellow2","orange2","red2"))+
    tm_layout(main.title= 'NO3-N 농도 공간 분포',
              main.title.size = 3,
              legend.position = c(0.7,0.0),
              legend.text.size = 1,legend.title.size=2)+
    # tm_shape(s_p)
    # tm_dots(size=0.01,shape = 2)+
    tm_shape(korea)+
    tm_borders(col="black",lwd=2)
  
  P.idw <- gstat::idw(formula =Cl ~ 1, s_p, newdata=grd, idp=2)
  head(P.idw)
  # Convert to raster object then clip to Texas
  r       <- raster(P.idw)
  r.m     <- mask(r, korea)
  head(r)
  head(r.m)
  # Plot
  tm_shape(r.m) + 
    tm_raster(title="Cl", breaks=c(0,125,250,400,600,10000),
              palette = c("green4","limegreen","yellow2","orange2","red2"))+
    tm_layout(main.title= 'Cl 농도 공간 분포',
              main.title.size = 3,
              legend.position = c(0.7,0.028),
              legend.text.size = 1,legend.title.size=2)+
    # tm_shape(s_p)
    # tm_dots(size=0.01,shape = 2)+
    tm_shape(korea)+
    tm_borders(col="black",lwd=2)
  
  P.idw <- gstat::idw(formula =F ~ 1, s_p, newdata=grd, idp=2)
  head(P.idw)
  # Convert to raster object then clip to Texas
  r       <- raster(P.idw)
  r.m     <- mask(r, korea)
  head(r)
  head(r.m)
  # Plot
  tm_shape(r.m) + 
    tm_raster(title="F", breaks=c(0,0.75,1.5,3,5,10000),
              palette = c("green4","limegreen","yellow2","orange2","red2"))+
    tm_layout(main.title= 'F 농도 공간 분포',
              main.title.size = 3,
              legend.position = c(0.7,0.025),
              legend.text.size = 1,legend.title.size=2)+
    # tm_shape(s_p)
    # tm_dots(size=0.01,shape = 2)+
    tm_shape(korea)+
    tm_borders(col="black",lwd=2)
  
  P.idw <- gstat::idw(formula =B ~ 1, s_p, newdata=grd, idp=2) #1,3 해봐도 그림 별로 달라지지 않음
  head(P.idw)
  # Convert to raster object then clip to Texas
  r       <- raster(P.idw)
  r.m     <- mask(r, korea)
  head(r)
  head(r.m)
  # Plot
  tm_shape(r.m) + 
    tm_raster(title="B", breaks=c(0,0.5,1.0,1.5,2.0,1000),
              palette = c("green4","limegreen","yellow2","orange2","red2"))+
    tm_layout(main.title= 'B 농도 공간 분포',
              main.title.size = 3,
              legend.position = c(0.7,0.04),
              legend.text.size = 1,legend.title.size=2)+
    # tm_shape(s_p)
    # tm_dots(size=0.01,shape = 2)+
    tm_shape(korea)+
    tm_borders(col="black",lwd=2)
  
  P.idw <- gstat::idw(formula =General_Bacteria ~ 1, s_p, newdata=grd, idp=10)
  head(P.idw)
  # Convert to raster object then clip to Texas
  r       <- raster(P.idw)
  r.m     <- mask(r, korea)
  head(r)
  head(r.m)
  # Plot
  tm_shape(r.m) + 
    tm_raster(title="General Bacteria", breaks=c(0,50,100,200,500,10000),
              palette = c("green4","limegreen","yellow2","orange2","red2"))+
    tm_layout(main.title= 'General Bacteria 농도 공간 분포',
              main.title.size = 3,
              legend.position = c(0.7,0.04),
              legend.text.size = 1,legend.title.size=2)+
    # tm_shape(s_p)
    # tm_dots(size=0.01,shape = 2)+
    tm_shape(korea)+
    tm_borders(col="black",lwd=2)
  
  P.idw <- gstat::idw(formula =DRASTIC ~ 1, s_p, newdata=grd, idp=3)
  head(P.idw)
  # Convert to raster object then clip to Texas
  r       <- raster(P.idw)
  r.m     <- mask(r, korea)
  head(r)
  head(r.m)
  # Plot
  tm_shape(r.m) + 
    tm_raster(title="DRASTIC", breaks=c(49,79,87,95,104,137),
              palette = c("green4","limegreen","yellow2","orange2","red2"))+
    tm_layout(main.title= 'DRASTIC 점수 공간 분포',
              main.title.size = 3,
              legend.position = c(0.7,0.04),
              legend.text.size = 1,legend.title.size=2)+
    # tm_shape(s_p)
    # tm_dots(size=0.01,shape = 2)+
    tm_shape(korea)+
    tm_borders(col="black",lwd=2)
}
#===============================================================================
#HQ score Importance?
#===============================================================================
#HQ 기준별 WAWQI 점수
{
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
adch = subset(x, x$Age == "Adult" | x$Age == "Child")
ad = subset(x, x$Age == "Adult")
ch = subset(x, x$Age == "Child")
Pass.ad = subset(ad, ad$Pass == "Pass")
Pass.ch = subset(ch, ch$Pass == "Pass")
Pass.x = subset(x, x$Pass == "Pass")

compare_means(WAWQI.12~HQ.std, data = adch, group.by = "Age")

p <- ggboxplot(adch, x = "HQ.std", y = "WAWQI.12",
               fill = "HQ.std",  palette = "Set1",
               facet.by = "Age", outlier.colour = "NA", short.panel.labs = FALSE) +
  theme(legend.position = "none",
        strip.text = element_text(size = 25),
        axis.title.x = element_text(size=30),
        axis.text.x = element_text(size=25),
        axis.title.y = element_text(size=30),
        axis.text.y = element_text(size=25)) +
  ylim(0,100) +
  xlab("HQ score") + ylab("WAWQI score")
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.signif", label.x = 1.5, size = 10, vjust = 1,hjust = 0.5)
}
#===============================================================================
#충청남도 보간법
{
  library(rgdal)
  library(tmap)
  library(gstat) 
  library(sp)   
  library(spatstat)  
  library(maptools)  
  library(raster)    
  library(maps)
  library(mapproj)
  library(ggplot2)
  map_cn = readOGR("LSMD_ADM_SECT_UMD_44.shp")
  to.crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"  
  df_map_cn <- spTransform(map_cn, to.crs)
  head(df_map_cn)
  
  sg = read.csv("안심지하수(17-22, GQI, HRA)GPS,WAWQI수정.csv", header=T, as.is=T, row.names = 1, na="-", fileEncoding = "CP949", encoding = "UTF-8")
  head(sg)
  # sg$Age=="Adult"// sg$Age=="Child"
  sg_cn=subset(sg, sg$시도=="충청남도"& sg$Age=="Adult")
  # sg_cn=subset(sg, sg$시도=="충청남도"& sg$Age=="Child")
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
#행정구역별 시각화 (행정구역별 수질인자 평균 후 LISA 계산, k = 5), Adult 포함
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
    
    lisa.WAWQI.12 <- localmoran(ad.net$WAWQI.12, w.ad)
    lisa.WAWQI.12 = as.data.frame(lisa.WAWQI.12)
    colnames(lisa.WAWQI.12) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.WAWQI.12$group <- NA # 새로운 열 생성
    lisa.WAWQI.12$group[lisa.WAWQI.12$Z.Ii > 0 & lisa.WAWQI.12$Ii > 0] <- "High-High" # High-High 조건
    lisa.WAWQI.12$group[lisa.WAWQI.12$Z.Ii > 0 & lisa.WAWQI.12$Ii < 0] <- "High-Low" # High-Low 조건
    lisa.WAWQI.12$group[lisa.WAWQI.12$Z.Ii < 0 & lisa.WAWQI.12$Ii > 0] <- "Low-High" # Low-High 조건
    lisa.WAWQI.12$group[lisa.WAWQI.12$Z.Ii < 0 & lisa.WAWQI.12$Ii < 0] <- "Low-Low" # Low-Low 조건
    head(lisa.WAWQI.12)
    names(lisa.WAWQI.12)[5] <- c("p.value")
    head(lisa.WAWQI.12)
    
    lisa.HQ.ad <- localmoran(ad.net$HQ.total.ad, w.ad)
    lisa.HQ.ad = as.data.frame(lisa.HQ.ad)
    colnames(lisa.HQ.ad) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.HQ.ad$group <- NA # 새로운 열 생성
    lisa.HQ.ad$group[lisa.HQ.ad$Z.Ii > 0 & lisa.HQ.ad$Ii > 0] <- "High-High" # High-High 조건
    lisa.HQ.ad$group[lisa.HQ.ad$Z.Ii > 0 & lisa.HQ.ad$Ii < 0] <- "High-Low" # High-Low 조건
    lisa.HQ.ad$group[lisa.HQ.ad$Z.Ii < 0 & lisa.HQ.ad$Ii > 0] <- "Low-High" # Low-High 조건
    lisa.HQ.ad$group[lisa.HQ.ad$Z.Ii < 0 & lisa.HQ.ad$Ii < 0] <- "Low-Low" # Low-Low 조건
    head(lisa.HQ.ad)
    names(lisa.HQ.ad)[5] <- c("p.value")
    head(lisa.HQ.ad)
    
    lisa.HQ.ch <- localmoran(ad.net$HQ.total.ch, w.ad)
    lisa.HQ.ch = as.data.frame(lisa.HQ.ch)
    colnames(lisa.HQ.ch) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.HQ.ch$group <- NA # 새로운 열 생성
    lisa.HQ.ch$group[lisa.HQ.ch$Z.Ii > 0 & lisa.HQ.ch$Ii > 0] <- "High-High" # High-High 조건
    lisa.HQ.ch$group[lisa.HQ.ch$Z.Ii > 0 & lisa.HQ.ch$Ii < 0] <- "High-Low" # Low-Low 조건
    lisa.HQ.ch$group[lisa.HQ.ch$Z.Ii < 0 & lisa.HQ.ch$Ii > 0] <- "Low-High" # High-Low 조건
    lisa.HQ.ch$group[lisa.HQ.ch$Z.Ii < 0 & lisa.HQ.ch$Ii < 0] <- "Low-Low" # Low-High 조건
    head(lisa.HQ.ch)
    names(lisa.HQ.ch)[5] <- c("p.value")
    head(lisa.HQ.ch)
    
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
    
    lisa.B <- localmoran(ad.net$B, w.ad)
    lisa.B = as.data.frame(lisa.B)
    colnames(lisa.B) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.B$group <- NA # 새로운 열 생성
    lisa.B$group[lisa.B$Z.Ii > 0 & lisa.B$Ii > 0] <- "High-High" # High-High 조건
    lisa.B$group[lisa.B$Z.Ii > 0 & lisa.B$Ii < 0] <- "High-Low" # High-Low 조건
    lisa.B$group[lisa.B$Z.Ii < 0 & lisa.B$Ii > 0] <- "Low-High" # Low-High 조건
    lisa.B$group[lisa.B$Z.Ii < 0 & lisa.B$Ii < 0] <- "Low-Low" # Low-Low 조건
    head(lisa.B)
    names(lisa.B)[5] <- c("p.value")
    head(lisa.B)
    
    lisa.Pb <- localmoran(ad.net$Pb, w.ad)
    lisa.Pb = as.data.frame(lisa.Pb)
    colnames(lisa.Pb) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.Pb$group <- NA # 새로운 열 생성
    lisa.Pb$group[lisa.Pb$Z.Ii > 0 & lisa.Pb$Ii > 0] <- "High-High" # High-High 조건
    lisa.Pb$group[lisa.Pb$Z.Ii > 0 & lisa.Pb$Ii < 0] <- "High-Low" # Low-Low 조건
    lisa.Pb$group[lisa.Pb$Z.Ii < 0 & lisa.Pb$Ii > 0] <- "Low-High" # High-Low 조건
    lisa.Pb$group[lisa.Pb$Z.Ii < 0 & lisa.Pb$Ii < 0] <- "Low-Low" # Low-High 조건
    head(lisa.Pb)
    names(lisa.Pb)[5] <- c("p.value")
    head(lisa.Pb)
    
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
    
    lisa.GB <- localmoran(ad.net$General_Bacteria, w.ad)
    lisa.GB = as.data.frame(lisa.GB)
    colnames(lisa.GB) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.GB$group <- NA # 새로운 열 생성
    lisa.GB$group[lisa.GB$Z.Ii > 0 & lisa.GB$Ii > 0] <- "High-High" # High-High 조건
    lisa.GB$group[lisa.GB$Z.Ii > 0 & lisa.GB$Ii < 0] <- "High-Low" # Low-Low 조건
    lisa.GB$group[lisa.GB$Z.Ii < 0 & lisa.GB$Ii > 0] <- "Low-High" # High-Low 조건
    lisa.GB$group[lisa.GB$Z.Ii < 0 & lisa.GB$Ii < 0] <- "Low-Low" # Low-High 조건
    head(lisa.GB)
    names(lisa.GB)[5] <- c("p.value")
    head(lisa.GB)
    
    ad.cn = cbind(ad.net[c(1:8)],lisa.WAWQI.12[5],lisa.WAWQI.12[1],lisa.WAWQI.12[4], lisa.WAWQI.12[6],#여기서 ad.Q4는 위에서 원파일에 사분면 데이터 합친 데이터터
                  lisa.HQ.ad[5],lisa.HQ.ad[1],lisa.HQ.ad[4],lisa.HQ.ad[6],
                  lisa.HQ.ch[5],lisa.HQ.ch[1],lisa.HQ.ch[4],lisa.HQ.ch[6],
                  lisa.pH[5],lisa.pH[1],lisa.pH[4],lisa.pH[6],
                  lisa.NO3.N[5],lisa.NO3.N[1],lisa.NO3.N[4],lisa.NO3.N[6],
                  lisa.B[5],lisa.B[1],lisa.B[4],lisa.B[6],
                  lisa.Pb[5],lisa.Pb[1],lisa.Pb[4],lisa.Pb[6],
                  lisa.F[5],lisa.F[1],lisa.F[4],lisa.F[6],
                  lisa.GB[5],lisa.GB[1],lisa.GB[4],lisa.GB[6])
    names(ad.cn)[9] <- c("WAWQI.lisa.p")
    names(ad.cn)[10] <- c("WAWQI.lisa.i")
    names(ad.cn)[11] <- c("WAWQI.lisa.z")
    names(ad.cn)[12] <- c("WAWQI.lisa.r")
    names(ad.cn)[13] <- c("HQ.ad.lisa.p")
    names(ad.cn)[14] <- c("HQ.ad.lisa.i")
    names(ad.cn)[15] <- c("HQ.ad.lisa.z")
    names(ad.cn)[16] <- c("HQ.ad.lisa.r")
    names(ad.cn)[17] <- c("HQ.ch.lisa.p")
    names(ad.cn)[18] <- c("HQ.ch.lisa.i")
    names(ad.cn)[19] <- c("HQ.ch.lisa.z")
    names(ad.cn)[20] <- c("HQ.ch.lisa.r")
    names(ad.cn)[21] <- c("pH.lisa.p")
    names(ad.cn)[22] <- c("pH.lisa.i")
    names(ad.cn)[23] <- c("pH.lisa.z")
    names(ad.cn)[24] <- c("pH.lisa.r")
    names(ad.cn)[25] <- c("NO3.N.lisa.p")
    names(ad.cn)[26] <- c("NO3.N.lisa.i")
    names(ad.cn)[27] <- c("NO3.N.lisa.z")
    names(ad.cn)[28] <- c("NO3.N.lisa.r")
    names(ad.cn)[29] <- c("B.lisa.p")
    names(ad.cn)[30] <- c("B.lisa.i")
    names(ad.cn)[31] <- c("B.lisa.z")
    names(ad.cn)[32] <- c("B.lisa.r")
    names(ad.cn)[33] <- c("Pb.lisa.p")
    names(ad.cn)[34] <- c("Pb.lisa.i")
    names(ad.cn)[35] <- c("Pb.lisa.z")
    names(ad.cn)[36] <- c("Pb.lisa.r")
    names(ad.cn)[37] <- c("F.lisa.p")
    names(ad.cn)[38] <- c("F.lisa.i")
    names(ad.cn)[39] <- c("F.lisa.z")
    names(ad.cn)[40] <- c("F.lisa.r")
    names(ad.cn)[41] <- c("GB.lisa.p")
    names(ad.cn)[42] <- c("GB.lisa.i")
    names(ad.cn)[43] <- c("GB.lisa.z")
    names(ad.cn)[44] <- c("GB.lisa.r")
  }
  
  # write.csv(ad.cn, "충남, 평균후리사(k5)(HQ수정).csv", na="-", fileEncoding = "CP949")
  
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
  
  # sg_chung = read.csv("충남, 평균후리사(k3).csv", header=T, as.is=T, row.names = 1, na="-", fileEncoding = "CP949", encoding = "UTF-8")
  # sg_chung = read.csv("충남, 평균후리사(k4).csv", header=T, as.is=T, row.names = 1, na="-", fileEncoding = "CP949", encoding = "UTF-8")
  sg_chung = read.csv("충남, 평균후리사(k5)(HQ수정).csv", header=T, as.is=T, row.names = 1, na="-", fileEncoding = "CP949", encoding = "UTF-8")
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
  #head(h)
  
  head(df_map_cn)
  # ggplot(data=df_map_cn, aes(x=long, y=lat, group=group))+
  #   geom_polygon(fill='white', color='black')+
  #   theme_bw()+
  #   theme(panel.grid.major.x = element_blank(),
  #         panel.grid.minor.x = element_blank(),
  #         panel.grid.major.y = element_blank(),
  #         panel.grid.minor.y = element_blank())
  
  #WAWQI
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=WAWQI.lisa.p), color='black')+ # Total(총인구수)
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
  
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=WAWQI.lisa.r), color='black')+ # Total(총인구수)
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
  
  #HQ(child)
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=HQ.ch.lisa.p), color='black')+ # Total(총인구수)
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
  
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=HQ.ch.lisa.r), color='black')+ # Total(총인구수)
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
  
  #HQ(adult)
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=HQ.ad.lisa.p), color='black')+ # Total(총인구수)
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
  
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=HQ.ad.lisa.r), color='black')+ # Total(총인구수)
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
  
  
  #NO3-N
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=NO3.N.lisa.p), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    ggtitle("Local Moran's index significance_NO3-N")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=15),
          legend.title=element_text(size=15),
          legend.position = c(0.25,0.15)) +
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
    ggtitle("Spatial autocorrelation_NO3-N")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=15),
          legend.title=element_text(size=15),
          legend.position = c(0.25,0.18)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("red2", "blue","grey","black"),
                      labels = c("High-High", "Low-Low", "Not Significant", "No samples"))
  
  #G_B
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=GB.lisa.p), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    ggtitle("Local Moran's index significance_General Bacteria")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=15),
          legend.title=element_text(size=15),
          legend.position = c(0.25,0.15)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("green4", "grey","black"),
                      labels = c("Significant", "Not Significant", "No Samples"))
  
  # High-High는 local moran’s index와 z-score가 모두 양수
  # Low-Low는 local moran’s index와 z-score가 모두 음수
  # High-Low는 local moran’s index는 음수이고 z-score는 양수
  # Low-High는 local moran’s index는 양수이고 z-score는 음수
  
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=GB.lisa.r), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    ggtitle("Spatial autocorrelation_General Bacteria")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=15),
          legend.title=element_text(size=15),
          legend.position = c(0.25,0.18)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("red2","blue","grey","black"),
                      labels = c("High-High", "Low-Low", "Not Significant",  "No samples"))
  
  
  #pH
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=pH.lisa.p), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    ggtitle("Local Moran's index significance_pH")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=15),
          legend.title=element_text(size=15),
          legend.position = c(0.25,0.15)) +
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
    ggtitle("Spatial autocorrelation_pH")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=15),
          legend.title=element_text(size=15),
          legend.position = c(0.25,0.18)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("red2", "blue","grey","black"),
                      labels = c("High-High", "Low-Low", "Not Significant", "No samples"))
  
  
  #B
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=B.lisa.p), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    ggtitle("Local Moran's index significance_B")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=15),
          legend.title=element_text(size=15),
          legend.position = c(0.25,0.15)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("green4", "grey","black"),
                      labels = c("Significant", "Not Significant", "No Samples"))
  # High-High는 local moran’s index와 z-score가 모두 양수
  # Low-Low는 local moran’s index와 z-score가 모두 음수
  # High-Low는 local moran’s index는 음수이고 z-score는 양수
  # Low-High는 local moran’s index는 양수이고 z-score는 음수
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=B.lisa.r), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    ggtitle("Spatial autocorrelation_B")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=15),
          legend.title=element_text(size=15),
          legend.position = c(0.25,0.18)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("red2", "blue","grey","black"),
                      labels = c("High-High", "Low-Low", "Not Significant", "No samples"))
  
  #F
  ggplot()+ geom_polygon(data=c, aes(x=long, y=lat, group=group, fill=F.lisa.p), color='black')+ # Total(총인구수)
    theme_bw()+
    ylab("latitude")+
    xlab("longitude")+
    ggtitle("Local Moran's index significance_F")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=15),
          legend.title=element_text(size=15),
          legend.position = c(0.25,0.15)) +
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
    ggtitle("Spatial autocorrelation_F")+
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size=20),
          legend.text = element_text(size=15),
          legend.title=element_text(size=15),
          legend.position = c(0.25,0.18)) +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values = c("red2", "blue","grey","black"),
                      labels = c("High-High", "Low-Low", "Not Significant", "No samples"))
}
#===============================================================================
#각 지수별 핫스팟 별 통계랑 산출
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
  
  ad.cheongna = subset(ad, ad$읍면동 == "청라면")
  ad.cheongso = subset(ad, ad$읍면동 == "청소면")
  ad.daehoji = subset(ad, ad$읍면동 == "대호지면")
  ch.daehoji = subset(ch, ch$읍면동 == "대호지면")
  ad.godae = subset(ad, ad$읍면동 == "고대면")
  ch.godae = subset(ch, ch$읍면동 == "고대면")
  ad.seongmun = subset(ad, ad$읍면동 == "석문면")
  ad.songsan = subset(ad, ad$읍면동 == "송산면")
  ch.songsan = subset(ch, ch$읍면동 == "송산면")
  ad.beolgok = subset(ad, ad$읍면동 == "벌곡면")
  ad.songak = subset(ad, ad$읍면동 == "송악읍")
  ch.songak = subset(ch, ch$읍면동 == "송악읍")
  
  summary(ad$WAWQI.12)
  summary(ad.cheongna$WAWQI.12)
  summary(ad.cheongso$WAWQI.12)
  summary(ad.daehoji$WAWQI.12)
  summary(ad.godae$WAWQI.12)
  summary(ad.seongmun$WAWQI.12)
  summary(ad.songsan$WAWQI.12)
  summary(ad.beolgok$WAWQI.12)
  
  summary(ad$HQ.total)
  summary(ad.daehoji$HQ.total)
  summary(ad.godae$HQ.total)
  summary(ad.songak$HQ.total)
  summary(ad.songsan$HQ.total)
  
  summary(ch$HQ.total)
  summary(ch.daehoji$HQ.total)
  summary(ch.godae$HQ.total)
  summary(ch.songak$HQ.total)
  summary(ch.songsan$HQ.total)
  #-----------------------------------
  #Total
  #WAWQI parameters
  summary(ad$General_Bacteria)
  summary(ad$NO3.N)
  summary(ad$Turbidity)
  summary(ad$As)
  summary(ad$F)
  summary(ad$pH)
  summary(ad$Fe)
  summary(ad$Al)
  summary(ad$Mn)
  
  #HQ parameters
  summary(ad$Al)
  summary(ad$As)
  summary(ad$Cd)
  summary(ad$Cr)
  summary(ad$Cu)
  summary(ad$F)
  summary(ad$Fe)
  summary(ad$Hg)
  summary(ad$Mn)
  summary(ad$Pb)
  summary(ad$Se)
  summary(ad$Zn)
  
  #-----------------------------------
  #WAWQI hotspot
  summary(ad.cheongna$General_Bacteria)
  summary(ad.cheongna$NO3.N)
  summary(ad.cheongna$Turbidity)
  summary(ad.cheongna$As)
  summary(ad.cheongna$F)
  summary(ad.cheongna$pH)
  summary(ad.cheongna$Fe)
  summary(ad.cheongna$Al)
  summary(ad.cheongna$Mn)
  
  summary(ad.cheongso$General_Bacteria)
  summary(ad.cheongso$NO3.N)
  summary(ad.cheongso$Turbidity)
  summary(ad.cheongso$As)
  summary(ad.cheongso$F)
  summary(ad.cheongso$pH)
  summary(ad.cheongso$Fe)
  summary(ad.cheongso$Al)
  summary(ad.cheongso$Mn)
  
  summary(ad.daehoji$General_Bacteria)
  summary(ad.daehoji$NO3.N)
  summary(ad.daehoji$Turbidity)
  summary(ad.daehoji$As)
  summary(ad.daehoji$F)
  summary(ad.daehoji$pH)
  summary(ad.daehoji$Fe)
  summary(ad.daehoji$Al)
  summary(ad.daehoji$Mn)
  
  summary(ad.godae$General_Bacteria)
  summary(ad.godae$NO3.N)
  summary(ad.godae$Turbidity)
  summary(ad.godae$As)
  summary(ad.godae$F)
  summary(ad.godae$pH)
  summary(ad.godae$Fe)
  summary(ad.godae$Al)
  summary(ad.godae$Mn)
  
  summary(ad.seongmun$General_Bacteria)
  summary(ad.seongmun$NO3.N)
  summary(ad.seongmun$Turbidity)
  summary(ad.seongmun$As)
  summary(ad.seongmun$F)
  summary(ad.seongmun$pH)
  summary(ad.seongmun$Fe)
  summary(ad.seongmun$Al)
  summary(ad.seongmun$Mn)
  
  summary(ad.songsan$General_Bacteria)
  summary(ad.songsan$NO3.N)
  summary(ad.songsan$Turbidity)
  summary(ad.songsan$As)
  summary(ad.songsan$F)
  summary(ad.songsan$pH)
  summary(ad.songsan$Fe)
  summary(ad.songsan$Al)
  summary(ad.songsan$Mn)
  
  summary(ad.beolgok$General_Bacteria)
  summary(ad.beolgok$NO3.N)
  summary(ad.beolgok$Turbidity)
  summary(ad.beolgok$As)
  summary(ad.beolgok$F)
  summary(ad.beolgok$pH)
  summary(ad.beolgok$Fe)
  summary(ad.beolgok$Al)
  summary(ad.beolgok$Mn)
  
  #-----------------------------------
  #HQ adult hotspot
  summary(ad.daehoji$Al)
  summary(ad.daehoji$As)
  summary(ad.daehoji$Cd)
  summary(ad.daehoji$Cr)
  summary(ad.daehoji$Cu)
  summary(ad.daehoji$F)
  summary(ad.daehoji$Fe)
  summary(ad.daehoji$Hg)
  summary(ad.daehoji$Mn)
  summary(ad.daehoji$Pb)
  summary(ad.daehoji$Se)
  summary(ad.daehoji$Zn)
  
  summary(ad.godae$Al)
  summary(ad.godae$As)
  summary(ad.godae$Cd)
  summary(ad.godae$Cr)
  summary(ad.godae$Cu)
  summary(ad.godae$F)
  summary(ad.godae$Fe)
  summary(ad.godae$Hg)
  summary(ad.godae$Mn)
  summary(ad.godae$Pb)
  summary(ad.godae$Se)
  summary(ad.godae$Zn)
  
  summary(ad.songak$Al)
  summary(ad.songak$As)
  summary(ad.songak$Cd)
  summary(ad.songak$Cr)
  summary(ad.songak$Cu)
  summary(ad.songak$F)
  summary(ad.songak$Fe)
  summary(ad.songak$Hg)
  summary(ad.songak$Mn)
  summary(ad.songak$Pb)
  summary(ad.songak$Se)
  summary(ad.songak$Zn)
  
  summary(ad.songsan$Al)
  summary(ad.songsan$As)
  summary(ad.songsan$Cd)
  summary(ad.songsan$Cr)
  summary(ad.songsan$Cu)
  summary(ad.songsan$F)
  summary(ad.songsan$Fe)
  summary(ad.songsan$Hg)
  summary(ad.songsan$Mn)
  summary(ad.songsan$Pb)
  summary(ad.songsan$Se)
  summary(ad.songsan$Zn)
  
  #-----------------------------------
  #HQ child hotspot
  summary(ch.daehoji$Al)
  summary(ch.daehoji$As)
  summary(ch.daehoji$Cd)
  summary(ch.daehoji$Cr)
  summary(ch.daehoji$Cu)
  summary(ch.daehoji$F)
  summary(ch.daehoji$Fe)
  summary(ch.daehoji$Hg)
  summary(ch.daehoji$Mn)
  summary(ch.daehoji$Pb)
  summary(ch.daehoji$Se)
  summary(ch.daehoji$Zn)
  
  summary(ch.godae$Al)
  summary(ch.godae$As)
  summary(ch.godae$Cd)
  summary(ch.godae$Cr)
  summary(ch.godae$Cu)
  summary(ch.godae$F)
  summary(ch.godae$Fe)
  summary(ch.godae$Hg)
  summary(ch.godae$Mn)
  summary(ch.godae$Pb)
  summary(ch.godae$Se)
  summary(ch.godae$Zn)
  
  summary(ch.songak$Al)
  summary(ch.songak$As)
  summary(ch.songak$Cd)
  summary(ch.songak$Cr)
  summary(ch.songak$Cu)
  summary(ch.songak$F)
  summary(ch.songak$Fe)
  summary(ch.songak$Hg)
  summary(ch.songak$Mn)
  summary(ch.songak$Pb)
  summary(ch.songak$Se)
  summary(ch.songak$Zn)
  
  summary(ch.songsan$Al)
  summary(ch.songsan$As)
  summary(ch.songsan$Cd)
  summary(ch.songsan$Cr)
  summary(ch.songsan$Cu)
  summary(ch.songsan$F)
  summary(ch.songsan$Fe)
  summary(ch.songsan$Hg)
  summary(ch.songsan$Mn)
  summary(ch.songsan$Pb)
  summary(ch.songsan$Se)
  summary(ch.songsan$Zn)
}



























