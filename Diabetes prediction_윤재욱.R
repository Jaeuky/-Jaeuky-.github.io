{
  library(brnn)
  library(pls)
  library(pROC)
  library(mboost)
  library(e1071)
  library(arm)
  library(kernlab)
  library(caret)
  library(glmnet)
  library(mlbench)
  library(psych)
  library(tidyverse)
  library(vegan)
  library(ggplot2)
  library(reshape2)
  library(dplyr)
  library(rpart)
  library(randomForest)
  library(rfUtilities)
  library(nparcomp)
  library(lmtest)
  library(gridExtra)
  library(agricolae)
  library(indicspecies)
  library(caret)
  library(tidyverse)
  library(showtext)
  library(RColorBrewer)
  library(Hmisc)
  library(MASS)
  library(rcompanion)
  library(tidyverse)
  library(ggpubr)
  library(rstatix)
  library(datasets)
  library(ggplot2)
  library(multcompView)
  library(dplyr)#tukey normalization
  library(randomForest)
  library(rfUtilities)
  library(cluster)
  library(NbClust)
  library(kohonen)
  library(ggplot2)
  library(gridExtra)
  library(scales)
  library(readxl)
  library(timetk)
  library(forecast)
  library(forecast)
  library(tsoutliers)
  library(stringr)
  library(lubridate)
  library(dendroTools)
  library(knitr)
  library(caret)
  library(glmnet)
  library(mlbench)
  library(psych)
  library(tidyverse)
  library(TTR)
  library(tseries)
  library(VMDecomp)
  library(glue)
  library(readxl)
  library(Fgmutils)
  library(broom)
  library(purrr)
  library(quantregForest)
  library(ranger)
  library(h2o)
  library(boot)
  library(AugmenterR)
}
setwd("D:/jaeuk/R")
sa = read.csv("samadult.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")

#sa에서 사용할 컬럼 -> 1,3 -> FPX, HHX, 
#                   ->   7 -> SEX (성별, 범주형 , 1 = male, 2 = female)
#                   ->  13 -> AGE_P (나이, 연속형)

#                   ->  43 -> HYPEV(고혈압진단여부, 범주형, 1= yes, 2 = no, 7 = refused, 8 = not ascertained, 9=Dont know)
#                   ->  47 -> CHLEV(고콜레스테롤 진단 여부, 범주형, 1= yes, 2 = no, 7 = refused, 8 = not ascertained, 9=Dont know)

#                   -> 473 -> SMKSTAT2(현재 흡연 여부, 범주형, 1 =Every day, 2=some days, 3 = not at all, 7 = refused, 8 = not ascertained, 9 = dont know)
#                   -> 466 -> SMKREG (How old were you when you FIRST started to smoke fairly regularly?, 연속형, 6-84, 85 = 85 or older, 96 = never, 97= refused, 98 = not ascertained, 99 = dont know)
#                   -> 464 -> SMKQTNO (How long has it been since you quit smoking cigarettes?, 연속형, 01-94 1 - 94, 95 = 95+, 97=refused, 98=not ascertained, 99=dont know)
#                   -> 449 -> CIGSDA1(Number cigs per day, 연속형, 1-94, 95 = 95+, 97 = refused, 98 = not ascertained, 99 = dont know)

#                   -> 471 -> VIGNO (땀을 많이 흘리거나 호흡이나 심박수가 크게 증가하는 격렬한 신체 활동을 10분이상 얼마나 자주하는지?, 연속형, 0 = never, 996 = 운동 불가, 997 = refused, 998 = not ascertained, 999 = dont know)
#                   -> 476 -> VIGFREQW(활동적인 행동 횟수 1주당, 연속형, 00 = less than once per week, 01-28, 95=never, 96=unable, 97=refused, 98=not ascertained, 99 = dont know)
#                   -> 479 -> VIGMIN(활동적인 행동 시간 분, 연속형, 1-995, 997 = refused, 998=not ascertained, 999 = dont know)

#                   -> 456 -> MODNO (가볍거나 중간정도 운동을 10분이상 얼마나 자주하는지? 연속형, 0 = never, 996 = 운동 불가, 997 = refused, 998 = not ascertained, 999 = dont know)
#                   -> 477 -> MODFREQW(일반적인 행동 횟수 1주당, 연속형, 00 = less than once per week, 01-28, 95=never, 96=unable, 97=refused, 98=not ascertained, 99 = dont know)
#                   -> 480 -> MODMIN(일반적인 행동 시간 분, 연속형, 1-995, 997 = refused, 998=not ascertained, 999 = dont know))

#                   -> 486 -> BMI (Body MAss Index, 연속형, 0~100점, 1 - 9994 = 0.01-99.94, 9995= 99.95+, 9999=unknown)
#                   -> 484 -> AHEIGHT (Total heigth in inches, 연속형, 59-76 inches, 96 = NA, 97 = refused, 98 = not ascertained, 99 = dont know)
#                   -> 485 -> AWEIGHTP (Weight without shoes pound, 연속형, 100-299 100-299 pounds, 996 = NA, 997 = refused, 998 = not ascertained, 999 = dont know)
#                   -> 481 -> ALC12MWK(Freq drank alcohol: Days per week, 연속형, 00 =  less than one day per week, 01-07 = 일 수, 95 = did not drink in past year, 97 = refused, 98 = not ascertained, 99 = dont know)
#                   -> 441 -> ALCAMT(Average Alcohol drinks on days drank, 연속형, 01-94 잔, 95 = 95+ drinks, 97 = refused, 98 = not ascertained, 99 = dont know)

#                   -> 123 -> INSLN1 (Are you NOW taking insulin?. 범주형, 1 yes, 2 no, 7 refused, 8 not ascertained, 9 dont know)
#                   -> 277 -> ALDURA10 (How long have you had diabetes?, years unit, 연속형, 00 = less than a year, 01-84 1-84, 85 85+, 96 unknown, 97 Refused, 98 Not ascertained, 99 Don't know)
#                   -> 120 -> DIBEV1 (Have you EVER been told by a doctor or health professional that you have diabetes or sugar diabetes?, 범주형, 1 yes, 2 no, 3 Borderline or prediabetes, 7 refused, 8 not ascertained, 9 dont know)

sa.net = sa[,c(1,3,7,13,43,47,473,466,464,449,471,476,479,456,477,480,486,484,485,481,441,123,277,120)]
head(sa.net)

sa.net = subset(sa.net, sa.net$DIBEV1 == "1"|sa.net$DIBEV1 == "2")

head(sa.net)

# 컬럼 별로 각 값에 따라 값을 치환
{
  #HYPEV
  sa.net$HYPEV[sa.net$HYPEV > 4] <- NA #각 기준에 맞게 설정
  
  table(sa.net$HYPEV)
  summary(sa.net$HYPEV)
  head(sa.net)
  
  #CHLEV
  sa.net$CHLEV[sa.net$CHLEV > 4] <- NA #각 기준에 맞게 설정
  
  table(sa.net$CHLEV)
  summary(sa.net$CHLEV)
  
  #SMKSTAT2
  sa.net$SMKSTAT2[sa.net$SMKSTAT2 > 4] <- NA #각 기준에 맞게 설정
  
  table(sa.net$SMKSTAT2)
  summary(sa.net$SMKSTAT2)
  
  #SMKREG
  sa.net$SMKREG[sa.net$SMKREG > 96.5] <- NA #각 기준에 맞게 설정
  
  table(sa.net$SMKREG)
  summary(sa.net$SMKREG)
  
  #SMKQTNO
  sa.net$SMKQTNO[sa.net$SMKQTNO > 96] <- NA #각 기준에 맞게 설정
  
  table(sa.net$SMKQTNO)
  summary(sa.net$SMKQTNO)
  
  #CIGSDA1
  sa.net$CIGSDA1[sa.net$CIGSDA1 > 96] <- NA #각 기준에 맞게 설정
  
  table(sa.net$CIGSDA1)
  summary(sa.net$CIGSDA1)
  
  #VIGNO
  sa.net$VIGNO[sa.net$VIGNO > 995.5] <- NA #각 기준에 맞게 설정
  
  table(sa.net$VIGNO)
  summary(sa.net$VIGNO)
  
  # VIGFREQW
  sa.net$VIGFREQW[sa.net$VIGFREQW > 94.5] <- NA #각 기준에 맞게 설정
  
  table(sa.net$VIGFREQW)
  summary(sa.net$VIGFREQW)
  
  
  #VIGMIN
  sa.net$VIGMIN[sa.net$VIGMIN > 996.5] <- NA #각 기준에 맞게 설정
  
  table(sa.net$VIGMIN)
  summary(sa.net$VIGMIN)
  
  #MODNO
  sa.net$MODNO[sa.net$MODNO > 995.5] <- NA #각 기준에 맞게 설정
  
  table(sa.net$MODNO)
  summary(sa.net$MODNO)
  
  # MODFREQW
  sa.net$MODFREQW[sa.net$MODFREQW > 94.5] <- NA #각 기준에 맞게 설정
  
  table(sa.net$MODFREQW)
  summary(sa.net$MODFREQW)
  
  # MODMIN
  sa.net$MODMIN[sa.net$MODMIN > 996.5] <- NA #각 기준에 맞게 설정
  
  table(sa.net$MODMIN)
  summary(sa.net$MODMIN)
  
  # BMI
  sa.net$BMI[sa.net$BMI > 9995.5] <- NA #각 기준에 맞게 설정
  
  table(sa.net$BMI)
  summary(sa.net$BMI)
  
  #AHEIGHT
  sa.net$AHEIGHT[sa.net$AHEIGHT > 95.5] <- NA #각 기준에 맞게 설정
  
  table(sa.net$AHEIGHT)
  summary(sa.net$AHEIGHT)
  
  # AWEIGHTP
  sa.net$AWEIGHTP[sa.net$AWEIGHTP > 995.5] <- NA #각 기준에 맞게 설정
  
  table(sa.net$AWEIGHTP)
  summary(sa.net$AWEIGHTP)
  
  # ALC12MWK
  sa.net$ALC12MWK[sa.net$ALC12MWK > 94.5] <- NA #각 기준에 맞게 설정
  
  table(sa.net$ALC12MWK)
  summary(sa.net$ALC12MWK)
  
  # ALCAMT
  sa.net$ALCAMT[sa.net$ALCAMT > 96.5] <- NA #각 기준에 맞게 설정
  
  table(sa.net$ALCAMT)
  summary(sa.net$ALCAMT)
  
  # INSLN1
  sa.net$INSLN1[sa.net$INSLN1 > 4] <- NA #각 기준에 맞게 설정
  
  table(sa.net$INSLN1)
  summary(sa.net$INSLN1)
  
  # ALDURA10
  sa.net$ALDURA10[sa.net$ALDURA10 > 95.5] <- NA #각 기준에 맞게 설정
  
  table(sa.net$ALDURA10)
  summary(sa.net$ALDURA10)
}
head(sa.net)

#===============================================================================
#단위환산
# BMI 컬럼의 값에 0.01을 곱합니다.
sa.net$BMI <- sa.net$BMI * 0.01

# AHEIGHT 컬럼의 값을 인치에서 센티미터로 변환합니다. (1 inch = 2.54 cm)
sa.net$AHEIGHT <- sa.net$AHEIGHT * 2.54

# AWEIGHTP 컬럼의 값을 파운드에서 킬로그램으로 변환합니다. (1 pound = 0.453592 kg)
sa.net$AWEIGHTP <- sa.net$AWEIGHTP * 0.453592
#===============================================================================
sa.net.v = sa.net
#데이터 파악을 위한 통계분석
# 데이터 파악을 위한 통계분석
# 1. 당뇨 여부에 따른 BMI 점수
head(sa.net.v)
DO = subset(sa.net.v, sa.net.v$DIBEV1 == "1")
DN = subset(sa.net.v, sa.net.v$DIBEV1 == "2")
shapiro.test(DO$BMI) #p<0.05
shapiro.test(DN$BMI) #p<0.05

wilcox.test(DO$BMI, DN$BMI) #p-value < 2.2e-16 -> 통계적으로 유의한 차이가 있음

# 2. 당뇨 여부에 따른 AWEIGHTP
shapiro.test(DO$AWEIGHTP) #p<0.05
shapiro.test(DN$AWEIGHTP) #p<0.05

wilcox.test(DO$AWEIGHTP, DN$AWEIGHTP) #p-value < 2.2e-16 -> 통계적으로 유의한 차이가 있음

# 3. 당뇨 여부에 따른 AGE_P
shapiro.test(DO$AGE_P) #p<0.05
shapiro.test(DN$AGE_P) #p<0.05

wilcox.test(DO$AGE_P, DN$AGE_P) #p-value < 2.2e-16 -> 통계적으로 유의한 차이가 있음
#--------------
DOM = subset(DO, DO$SEX == "1")
DOF = subset(DO, DO$SEX == "2")
DNM = subset(DN, DN$SEX == "1")
DNF = subset(DN, DN$SEX == "2")
# 1. 성별 당뇨 여부에 따른 BMI 점수
shapiro.test(DOM$BMI) #p<0.05
shapiro.test(DNM$BMI) #p<0.05

wilcox.test(DOM$BMI, DNM$BMI) #p-value < 2.2e-16 -> 통계적으로 유의한 차이가 있음

shapiro.test(DOF$BMI) #p<0.05
shapiro.test(DNF$BMI) #p<0.05

wilcox.test(DOF$BMI, DNF$BMI) #p-value < 2.2e-16 -> 통계적으로 유의한 차이가 있음

# 2. 당뇨 여부에 따른 AWEIGHTP
shapiro.test(DOM$AWEIGHTP) #p<0.05
shapiro.test(DNM$AWEIGHTP) #p<0.05

wilcox.test(DOM$AWEIGHTP, DNM$AWEIGHTP) #p-value < 2.2e-16 -> 통계적으로 유의한 차이가 있음

shapiro.test(DOF$AWEIGHTP) #p<0.05
shapiro.test(DNF$AWEIGHTP) #p<0.05

wilcox.test(DOF$AWEIGHTP, DNF$AWEIGHTP) #p-value < 2.2e-16 -> 통계적으로 유의한 차이가 있음
# 3. 당뇨 여부에 따른 AGE_P
shapiro.test(DOM$AGE_P) #p<0.05
shapiro.test(DNM$AGE_P) #p<0.05

wilcox.test(DOM$AGE_P, DNM$AGE_P) #p-value < 2.2e-16 -> 통계적으로 유의한 차이가 있음

shapiro.test(DOF$AGE_P) #p<0.05
shapiro.test(DNF$AGE_P) #p<0.05

wilcox.test(DOF$AGE_P, DNF$AGE_P) #p-value < 2.2e-16 -> 통계적으로 유의한 차이가 있음
#===============================================================================
#Diabetes 기준별 BMI 점수
sa.net.v = sa.net
#----------------
# 결측치 처리
sa.net.v$BMI <- as.numeric(sa.net.v$BMI)
sa.net.v <- sa.net.v[!is.na(sa.net.v$BMI), ]

# 레이블 변경
sa.net.v$DIBEV1 <- factor(sa.net.v$DIBEV1, levels = c(1, 2), labels = c("Diagnosed", "Not Diagnosed"))

# 통계 검정
compare_means(BMI ~ DIBEV1, data = sa.net.v)

# 시각화
p <- ggboxplot(sa.net.v, x = "DIBEV1", y = "BMI",
               fill = "DIBEV1",  palette = "Set1", outlier.colour = "NA", short.panel.labs = FALSE) +
  theme(legend.position = "none",
        strip.text = element_text(size = 20),
        axis.title.x = element_text(size=25),
        axis.text.x = element_text(size=20),
        axis.title.y = element_text(size=25),
        axis.text.y = element_text(size=20)) +
  ylim(0,100) +
  xlab("Diabetes") + ylab("BMI score")
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.signif", label.x = 1.5, size = 10, vjust = 1,hjust = 0.5)
#----------------
sa.net.v = sa.net
# 레이블 변경
sa.net.v$DIBEV1 <- factor(sa.net.v$DIBEV1, levels = c(1, 2), labels = c("Diagnosed", "Not Diagnosed"))
sa.net.v$SEX <- factor(sa.net.v$SEX, levels = c(1, 2), labels = c("Male", "Female"))

# 통계 검정
compare_means(BMI ~ DIBEV1, data = sa.net.v, group.by = "SEX")

# 시각화
p <- ggboxplot(sa.net.v, x = "DIBEV1", y = "BMI",
               fill = "DIBEV1",  palette = "Set1",
               facet.by = "SEX", outlier.colour = "NA", short.panel.labs = FALSE) +
  theme(legend.position = "none",
        strip.text = element_text(size = 20),
        axis.title.x = element_text(size=25),
        axis.text.x = element_text(size=20),
        axis.title.y = element_text(size=25),
        axis.text.y = element_text(size=20)) +
  ylim(0,100) +
  xlab("Diabetes") + ylab("BMI score")
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.signif", label.x = 1.5, size = 10, vjust = 1,hjust = 0.5)

#===============================================================================
#Diabetes 기준별 AGE
sa.net.v = sa.net
#-------------------
# 결측치 처리
sa.net.v$AGE_P <- as.numeric(sa.net.v$AGE_P)
sa.net.v <- sa.net.v[!is.na(sa.net.v$AGE_P), ]

# 레이블 변경
sa.net.v$DIBEV1 <- factor(sa.net.v$DIBEV1, levels = c(1, 2), labels = c("Diagnosed", "Not Diagnosed"))

# 통계 검정
compare_means(AGE_P ~ DIBEV1, data = sa.net.v)

# 시각화
p <- ggboxplot(sa.net.v, x = "DIBEV1", y = "AGE_P",
               fill = "DIBEV1",  palette = "Set1",
               outlier.colour = "NA", short.panel.labs = FALSE) +
  theme(legend.position = "none",
        strip.text = element_text(size = 20),
        axis.title.x = element_text(size=25),
        axis.text.x = element_text(size=20),
        axis.title.y = element_text(size=25),
        axis.text.y = element_text(size=20)) +
  ylim(0,110) +
  xlab("Diabetes") + ylab("Age")
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.signif", label.x = 1.5, size = 10, vjust = 1,hjust = 0.5)
#--------------------
sa.net.v = sa.net
# 결측치 처리
sa.net.v$AGE_P <- as.numeric(sa.net.v$AGE_P)
sa.net.v <- sa.net.v[!is.na(sa.net.v$AGE_P), ]

# 레이블 변경
sa.net.v$DIBEV1 <- factor(sa.net.v$DIBEV1, levels = c(1, 2), labels = c("Diagnosed", "Not Diagnosed"))
sa.net.v$SEX <- factor(sa.net.v$SEX, levels = c(1, 2), labels = c("Male", "Female"))

# 통계 검정
compare_means(AGE_P ~ DIBEV1, data = sa.net.v, group.by = "SEX")

# 시각화
p <- ggboxplot(sa.net.v, x = "DIBEV1", y = "AGE_P",
               fill = "DIBEV1",  palette = "Set1",
               facet.by = "SEX", outlier.colour = "NA", short.panel.labs = FALSE) +
  theme(legend.position = "none",
        strip.text = element_text(size = 20),
        axis.title.x = element_text(size=25),
        axis.text.x = element_text(size=20),
        axis.title.y = element_text(size=25),
        axis.text.y = element_text(size=20)) +
  ylim(0,110) +
  xlab("Diabetes") + ylab("Age")
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.signif", label.x = 1.5, size = 10, vjust = 1,hjust = 0.5)
#===============================================================================
#Diabetes 기준별 WEIGHTP
sa.net.v = sa.net
head(sa.net.v)
# 결측치 처리
sa.net.v$AWEIGHTP <- as.numeric(sa.net.v$AWEIGHTP)
sa.net.v <- sa.net.v[!is.na(sa.net.v$AWEIGHTP), ]

# 레이블 변경
sa.net.v$DIBEV1 <- factor(sa.net.v$DIBEV1, levels = c(1, 2), labels = c("Diagnosed", "Not Diagnosed"))

# 통계 검정
compare_means(AWEIGHTP ~ DIBEV1, data = sa.net.v, group.by = "SEX")

# 시각화
p <- ggboxplot(sa.net.v, x = "DIBEV1", y = "AWEIGHTP",
               fill = "DIBEV1",  palette = "Set1",
               outlier.colour = "NA", short.panel.labs = FALSE) +
  theme(legend.position = "none",
        strip.text = element_text(size = 20),
        axis.title.x = element_text(size=25),
        axis.text.x = element_text(size=20),
        axis.title.y = element_text(size=25),
        axis.text.y = element_text(size=20)) +
  ylim(45,150) +
  xlab("Diabetes") + ylab("Weight (kg)")
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.signif", label.x = 1.5, size = 10, vjust = 1,hjust = 0.5)
#------------
sa.net.v = sa.net
# 결측치 처리
sa.net.v$AWEIGHTP <- as.numeric(sa.net.v$AWEIGHTP)
sa.net.v <- sa.net.v[!is.na(sa.net.v$AWEIGHTP), ]

# 레이블 변경
sa.net.v$DIBEV1 <- factor(sa.net.v$DIBEV1, levels = c(1, 2), labels = c("Diagnosed", "Not Diagnosed"))
sa.net.v$SEX <- factor(sa.net.v$SEX, levels = c(1, 2), labels = c("Male", "Female"))

# 통계 검정
compare_means(AWEIGHTP ~ DIBEV1, data = sa.net.v, group.by = "SEX")

# 시각화
p <- ggboxplot(sa.net.v, x = "DIBEV1", y = "AWEIGHTP",
               fill = "DIBEV1",  palette = "Set1",
               facet.by = "SEX", outlier.colour = "NA", short.panel.labs = FALSE) +
  theme(legend.position = "none",
        strip.text = element_text(size = 20),
        axis.title.x = element_text(size=25),
        axis.text.x = element_text(size=20),
        axis.title.y = element_text(size=25),
        axis.text.y = element_text(size=20)) +
  ylim(45,150) +
  xlab("Diabetes") + ylab("Weight (kg)")
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.signif", label.x = 1.5, size = 10, vjust = 1,hjust = 0.5)
#==============================================================================
sa.net.n = sa.net
head(sa.net.n)

#그럼 데이터 조합을 선정하자
#우선 NA가 많은 컬럼순서로 나열해보면, 
summary(sa.net.n[,1]) #0, 범주형             
summary(sa.net.n[,2]) #0, 범주형
summary(sa.net.n[,3]) #0, 범주형
summary(sa.net.n[,4]) #0, 연속형
summary(sa.net.n[,5]) #33, 범주형
summary(sa.net.n[,6]) #68, 범주형
summary(sa.net.n[,7]) #78, 범주형
summary(sa.net.n[,8]) #15082, 연속형
summary(sa.net.n[,9]) #18645, 연속형
summary(sa.net.n[,10]) #21903, 연속형
summary(sa.net.n[,11]) #799, 연속형
summary(sa.net.n[,12]) #12914, 연속형
summary(sa.net.n[,13]) #12975, 연속형
summary(sa.net.n[,14]) #821, 연속형
summary(sa.net.n[,15]) #9012, 연속형
summary(sa.net.n[,16]) #9117, 연속형
summary(sa.net.n[,17]) #764, 연속형
summary(sa.net.n[,18]) #1690, 연속형
summary(sa.net.n[,19]) #2187, 연속형
summary(sa.net.n[,20]) #8512, 연속형
summary(sa.net.n[,21]) #8600, 연속형
summary(sa.net.n[,22]) #20629, 범주형
summary(sa.net.n[,23]) #24168, 연속형
table(sa.net.n[,24]) #0

#1 -> 분류코드
#2 -> 분류코드
#3 -> 성별
#4 -> 나이 v
#5 -> 고혈압진단여부
#6 -> 고콜레스테롤 진단여부
#7 -> 현재 흡연 여부
#8 -> 첫 흡연 나이
#9 -> 금연기간
#10 -> 하루 흡연량
#11 -> 격렬 운동 횟수
#12 -> 격렬 운동 횟수 주당 v
#13 -> 격렬 운동 분 v
#14 -> 일반 운동 횟수
#15 -> 일반 운동 횟수 주당 v
#16 -> 일반 운동 분 v
#17 -> BMI v
#18 -> 키 v
#19 -> 몸무게 v
#20 -> 음주 횟수 주당 v
#21 -> 음주량 v
#22 -> 인슐린 복용 여부
#23 -> 당뇨 진단 받은지 얼마나 됐는지
#24 -> 당뇨 여부 v


#case 1 => 그냥,, NA 값 5000개 미만인 애들 다 선정
sa.net.n1 = sa.net.n[c(1,2,3,4,5,6,7,11,14,17,18,19,24)]
sa.net.n1 = sa.net.n1[complete.cases(sa.net.n1), ]
head(sa.net.n1)
#sa.net.n1 -> 21435, 11

#case 2 => 그냥,, NA 값 10000개 미만인 애들 다 선정
sa.net.n2 = sa.net.n[c(1,2,3,4,5,6,7,11,14,15,16,17,18,19,20,21,24)]
sa.net.n2 = sa.net.n2[complete.cases(sa.net.n2), ]
head(sa.net.n2)
#sa.net.n2 -> 10368, 17

#case 3 => 모든 연속형 선정
sa.net.n3 = sa.net.n[c(4,8,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24)]
sa.net.n3 = sa.net.n3[complete.cases(sa.net.n3), ]
head(sa.net.n3)
#sa.net.n3 -> 0, 17

#case 4 => 모든 연속형 선정이지만, NA가 20000 넘는건 제외 
sa.net.n4 = sa.net.n[c(4,8,9,11,12,13,14,15,16,17,18,19,20,21,24)]
sa.net.n4 = sa.net.n4[complete.cases(sa.net.n4), ]
head(sa.net.n4)
#sa.net.n4 -> 1612, 15

#case 5 => 모든 연속형 선정이지만, NA가 15000 넘는건 제외 
sa.net.n5 = sa.net.n[c(4,11,12,13,14,15,16,17,18,19,20,21,24)]
sa.net.n5 = sa.net.n5[complete.cases(sa.net.n5), ]
head(sa.net.n5)
#sa.net.n5 -> 6999, 13

#case 6 => 모든 연속형 선정이지만, NA가 10000 넘는건 제외 
sa.net.n6 = sa.net.n[c(4,11,14,15,16,17,18,19,20,21,24)] 
sa.net.n6 = sa.net.n6[complete.cases(sa.net.n6), ]
head(sa.net.n6)
#sa.net.n6 -> 10398, 13

#case 7 => 내가 선정한 연속형 변수들로~ ! 근데 이건 case 5랑 결과가 같음, 그럴거면 컬럼 번호 11,14를 포함하는게 나을듯? 아닌가 그냥 빼자, 빼는게 논리적으로 편함
sa.net.n7 = sa.net.n[c(3,4,12,13,15,16,17,18,19,20,21,24)] 
sa.net.n7 = sa.net.n7[complete.cases(sa.net.n7), ]
head(sa.net.n7)
#sa.net.n7 -> 6999, 11

#결과
head(sa.net.n7)
# write.csv(sa.net.n7, "sa.net.csv", na="-", fileEncoding = "CP949")
#===============================================================================
#예측 모델을 위한 NET 데이터를 생성함
{
  sa.net.n7 = read.csv("sa.net.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
  head(sa.net.n7)
  str(sa.net.n7)
  {
    sa.net.n7$AGE_P = as.numeric(sa.net.n7$AGE_P) 
    sa.net.n7$VIGFREQW = as.numeric(sa.net.n7$VIGFREQW) 
    sa.net.n7$VIGMIN = as.numeric(sa.net.n7$VIGMIN) 
    sa.net.n7$MODFREQW = as.numeric(sa.net.n7$MODFREQW) 
    sa.net.n7$MODMIN = as.numeric(sa.net.n7$MODMIN)
    sa.net.n7$BMI = as.numeric(sa.net.n7$BMI)
    sa.net.n7$AHEIGHT = as.numeric(sa.net.n7$AHEIGHT)
    sa.net.n7$AWEIGHTP = as.numeric(sa.net.n7$AWEIGHTP)
    sa.net.n7$ALC12MWK = as.numeric(sa.net.n7$ALC12MWK)
    sa.net.n7$ALCAMT = as.numeric(sa.net.n7$ALCAMT)
    sa.net.n7$DIBEV1 = as.character(sa.net.n7$DIBEV1)
  }
  str(sa.net.n7)
}
#===============================================================================
#이상치 제거
{
  # 연속형 변수들의 이름을 저장합니다.
  continuous_vars <- c("AGE_P", "VIGFREQW", "VIGMIN", "MODFREQW", "MODMIN", "BMI", "AHEIGHT","AWEIGHTP", "ALC12MWK", "ALCAMT")
  
  # 각 연속형 변수에 대해 이상치를 제거합니다.
  for (var in continuous_vars) {
    Q1 <- quantile(sa.net.n7[[var]], probs=0.25, na.rm = TRUE)
    Q3 <- quantile(sa.net.n7[[var]], probs=0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    # 이상치 조건
    outlier_condition <- (sa.net.n7[[var]] < (Q1 - 1.5 * IQR)) | (sa.net.n7[[var]] > (Q3 + 1.5 * IQR))
    
    # 이상치를 NA로 설정
    sa.net.n7[outlier_condition, var] <- NA
  }
  
  # 이상치가 있는 행을 제거합니다.
  sa.net.n7 <- na.omit(sa.net.n7)
}
#===============================================================================
#Multicollinearity test -> 다중공선성 검사는 독립변수들간의 상관분석을 통해 강한 상관관계가 있을시 제거를 통해 예측 성능을 높힘
{
  #독립변수들 간의 상관관계를 분석 
  head(sa.net.n7)
  # Extract the predictors and response variable
  predictors <- sa.net.n7[, 2:10]
  response <- sa.net.n7[, 11]
  
  # Set a threshold for high correlation (e.g., 0.7, but you can adjust as needed)
  high_correlation_threshold <- 0.7
  
  # Repeat until no highly correlated predictors are left
  repeat {
    # Calculate the correlation matrix for predictors
    cor_matrix <- cor(predictors, method="spearman")
    
    # Find highly correlated predictors
    highly_correlated <- findCorrelation(cor_matrix, cutoff = high_correlation_threshold)
    
    # If no highly correlated predictors are found, break the loop
    if(length(highly_correlated) == 0) {
      break
    }
    
    # Remove the first of the highly correlated predictors
    predictors <- predictors[,-highly_correlated[1]]
  }
  
  # Now, you can use 'predictors' for your model
  head(predictors)
  
  # 데이터를 생성
  sa.net.fm = cbind(predictors,sa.net.n7$DIBEV1)
  names(sa.net.fm)[9] <- c("DIBEV1")
  head(sa.net.fm)
  
  # write.csv(sa.net.fm, "sa.net.fm.csv", na="-", fileEncoding = "CP949")
}
#===============================================================================
#중요도 분석
{
  sa.net.fm = read.csv("sa.net.fm.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
  head(sa.net.fm)
  str(sa.net.fm)
  {
    sa.net.fm$AGE_P = as.numeric(sa.net.fm$AGE_P) 
    sa.net.fm$VIGFREQW = as.numeric(sa.net.fm$VIGFREQW) 
    sa.net.fm$VIGMIN = as.numeric(sa.net.fm$VIGMIN) 
    sa.net.fm$MODFREQW = as.numeric(sa.net.fm$MODFREQW) 
    sa.net.fm$MODMIN = as.numeric(sa.net.fm$MODMIN)
    sa.net.fm$BMI = as.numeric(sa.net.fm$BMI)
    sa.net.fm$AHEIGHT = as.numeric(sa.net.fm$AHEIGHT)
    sa.net.fm$ALC12MWK = as.numeric(sa.net.fm$ALC12MWK)
    sa.net.fm$DIBEV1 = as.character(sa.net.fm$DIBEV1)
  }
  str(sa.net.fm)
  
  # 데이터 준비
  inTraining <- createDataPartition(sa.net.fm$DIBEV1, p = .70, list = FALSE)
  train <- sa.net.fm[ inTraining,]
  test  <- sa.net.fm[-inTraining,]
  
  # 모델 학습
  # fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  # tunegrid <- expand.grid(.mtry=c(1:8)) # 특성의 수를 변경
  
  # model.rf <- train(DIBEV1 ~ ., data = train, method = 'rf', tuneGrid=tunegrid, trControl = fitControl, preProcess = c("center", "scale"), ntry = 501)
  
  # model.rf <- train(DIBEV1 ~ ., data = train, method = 'rf')
  
  # 중요도 분석
  rfImp <- varImp(model.rf, scale = FALSE)
  rfImp = as.data.frame(rfImp$importance)
  
  # write.csv(rfImp, file = "imporatance.csv", na="-", fileEncoding = "CP949")
  
  imp = read.csv("imporatance.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
  head(imp)
  
  # Overall 값이 높은 순서대로 정렬
  imp$X = reorder(imp$X, imp$Overall, FUN = max)
  
  # 그래프 그리기
  brk = ggplot(imp, aes(x=Overall, y=X)) +
    geom_bar(stat = "identity", position = "dodge", color="black", width = 0.5) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text.y = element_text(size=25),
          axis.text.x = element_text(size=20,angle = 0, vjust = 0.5, hjust =0.5),
          axis.title.y = element_text(size=25),
          axis.title.x = element_text(size=25),
          legend.position = c(0,3),
          legend.text = element_text(size = 25),
          legend.background = element_rect(fill="white", size = 0.5, linetype = "solid", colour = "black"))+
    labs(fill = "")+
    xlab("Importance")+ ylab(~paste("")) 
  brk
}


#===============================================================================
# Feature & Algorithm selection
{
  #Data
  sa.net.fm = read.csv("sa.net.fm.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
  head(sa.net.fm)
  str(sa.net.fm)
  {
    sa.net.fm$AGE_P = as.numeric(sa.net.fm$AGE_P) 
    sa.net.fm$VIGFREQW = as.numeric(sa.net.fm$VIGFREQW) 
    sa.net.fm$VIGMIN = as.numeric(sa.net.fm$VIGMIN) 
    sa.net.fm$MODFREQW = as.numeric(sa.net.fm$MODFREQW) 
    sa.net.fm$MODMIN = as.numeric(sa.net.fm$MODMIN)
    sa.net.fm$BMI = as.numeric(sa.net.fm$BMI)
    sa.net.fm$AHEIGHT = as.numeric(sa.net.fm$AHEIGHT)
    sa.net.fm$ALC12MWK = as.numeric(sa.net.fm$ALC12MWK)
    sa.net.fm$DIBEV1 = as.character(sa.net.fm$DIBEV1)
  }
  str(sa.net.fm)
  
  # 각 클래스별로 데이터를 분할합니다.
  data_class1 <- sa.net.fm[sa.net.fm$DIBEV1 == 1,] #251개
  data_class2 <- sa.net.fm[sa.net.fm$DIBEV1 == 2,] #4905개
  
  # 각 클래스별로 원하는 비율로 샘플링합니다.
  n_samples_class1 <- round(nrow(data_class1) * 1)
  n_samples_class2 <- round(nrow(data_class2) * 0.15)
  
  sampled_data_class1 <- data_class1[sample(nrow(data_class1), n_samples_class1), ]
  sampled_data_class2 <- data_class2[sample(nrow(data_class2), n_samples_class2), ]
  
  # 샘플링된 데이터를 합칩니다.
  sampled_data <- rbind(sampled_data_class1, sampled_data_class2)
  table(sampled_data$DIBEV1)
  str(sampled_data)
  
  #Feature combination
  {
    c3 = sampled_data[c(6,1,7,9)]
    c4 = sampled_data[c(6,1,7,5,9)]
    c5 = sampled_data[c(6,1,7,5,3,9)]
    c6 = sampled_data[c(6,1,7,5,3,4,9)]
    c7 = sampled_data[c(6,1,7,5,3,4,2,9)]
    c8 = sampled_data[c(6,1,7,5,3,4,2,8,9)]   
  }   
  
  datalist = list()
  for (i in 1:10) {
    {
      c3 = as.data.frame(c3)
      inTraining <- createDataPartition(c3$DIBEV1, p = .70, list = FALSE)
      train.c3 <- c3[ inTraining,]
      test.c3  <- c3[-inTraining,]
      
      c4 = as.data.frame(c4)
      inTraining <- createDataPartition(c4$DIBEV1, p = .70, list = FALSE)
      train.c4 <- c4[ inTraining,]
      test.c4  <- c4[-inTraining,]
      
      c5 = as.data.frame(c5)
      inTraining <- createDataPartition(c5$DIBEV1, p = .70, list = FALSE)
      train.c5 <- c5[ inTraining,]
      test.c5  <- c5[-inTraining,]
      
      c6 = as.data.frame(c6)
      inTraining <- createDataPartition(c6$DIBEV1, p = .70, list = FALSE)
      train.c6 <- c6[ inTraining,]
      test.c6  <- c6[-inTraining,]
      
      c7 = as.data.frame(c7)
      inTraining <- createDataPartition(c7$DIBEV1, p = .70, list = FALSE)
      train.c7 <- c7[ inTraining,]
      test.c7  <- c7[-inTraining,]
      
      c8 = as.data.frame(c8)
      inTraining <- createDataPartition(c8$DIBEV1, p = .70, list = FALSE)
      train.c8 <- c8[ inTraining,]
      test.c8  <- c8[-inTraining,]
    }
    #===
    {
      model.rf.3 <- train(DIBEV1 ~ ., data = train.c3, method = 'rf')
      model.rf.4 <- train(DIBEV1 ~ ., data = train.c4, method = 'rf')
      model.rf.5 <- train(DIBEV1 ~ ., data = train.c5, method = 'rf')
      model.rf.6 <- train(DIBEV1 ~ ., data = train.c6, method = 'rf')
      model.rf.7 <- train(DIBEV1 ~ ., data = train.c7, method = 'rf')
      model.rf.8 <- train(DIBEV1 ~ ., data = train.c8, method = 'rf')
      
      model.rf.3$results$Accuracy[2]
      model.rf.3$results$Kappa[2]
      model.rf.4$results$Accuracy[2]
      model.rf.4$results$Kappa[2]
      model.rf.5$results$Accuracy[2]
      model.rf.5$results$Kappa[2]
      model.rf.6$results$Accuracy[2]
      model.rf.6$results$Kappa[2]
      model.rf.7$results$Accuracy[2]
      model.rf.7$results$Kappa[2]
      model.rf.8$results$Accuracy[2]
      model.rf.8$results$Kappa[2]
      
      # pred.c3.rf <- predict(model.rf.3, test.c3)
      # ref.c3.rf = as.factor(test.c3$DIBEV1)
      # cm.c3.rf <- confusionMatrix(pred, ref.c3.rf)
      # cm.c3.rf$overall = as.data.frame(cm.c3.rf$overall)
      # cm.c3.rf$overall$`cm.c3.rf$overall`[1]
      
      #===
      model.gaussprRadial.3 <- train(DIBEV1 ~ .,data = train.c3, method = 'gaussprRadial')
      model.gaussprRadial.4 <- train(DIBEV1 ~ .,data = train.c4, method = 'gaussprRadial')
      model.gaussprRadial.5 <- train(DIBEV1 ~ .,data = train.c5, method = 'gaussprRadial')
      model.gaussprRadial.6 <- train(DIBEV1 ~ .,data = train.c6, method = 'gaussprRadial')
      model.gaussprRadial.7 <- train(DIBEV1 ~ .,data = train.c7, method = 'gaussprRadial')
      model.gaussprRadial.8 <- train(DIBEV1 ~ .,data = train.c8, method = 'gaussprRadial')
      
      model.gaussprRadial.3$results$Accuracy
      model.gaussprRadial.3$results$Kappa
      model.gaussprRadial.4$results$Accuracy
      model.gaussprRadial.4$results$Kappa
      model.gaussprRadial.5$results$Accuracy
      model.gaussprRadial.5$results$Kappa
      model.gaussprRadial.6$results$Accuracy
      model.gaussprRadial.6$results$Kappa
      model.gaussprRadial.7$results$Accuracy
      model.gaussprRadial.7$results$Kappa
      model.gaussprRadial.8$results$Accuracy
      model.gaussprRadial.8$results$Kappa
      #===
      model.svmRadial.3 <- train(DIBEV1 ~ .,data = train.c3, method = 'svmRadial')
      model.svmRadial.4 <- train(DIBEV1 ~ .,data = train.c4, method = 'svmRadial')
      model.svmRadial.5 <- train(DIBEV1 ~ .,data = train.c5, method = 'svmRadial')
      model.svmRadial.6 <- train(DIBEV1 ~ .,data = train.c6, method = 'svmRadial')
      model.svmRadial.7 <- train(DIBEV1 ~ .,data = train.c7, method = 'svmRadial')
      model.svmRadial.8 <- train(DIBEV1 ~ .,data = train.c8, method = 'svmRadial')
      
      model.svmRadial.3$results$Accuracy[1]
      model.svmRadial.3$results$Kappa[1]
      model.svmRadial.4$results$Accuracy[1]
      model.svmRadial.4$results$Kappa[1]
      model.svmRadial.5$results$Accuracy[1]
      model.svmRadial.5$results$Kappa[1]
      model.svmRadial.6$results$Accuracy[1]
      model.svmRadial.6$results$Kappa[1]
      model.svmRadial.7$results$Accuracy[1]
      model.svmRadial.7$results$Kappa[1]
      model.svmRadial.8$results$Accuracy[1]
      model.svmRadial.8$results$Kappa[1]
    }
    #===
    model.cp <- data.frame(Case = c("Rf.3","Rf.4","Rf.5","Rf.6","Rf.7","Rf.8",
                                    "Gaussian Radial.3","Gaussian Radial.4","Gaussian Radial.5","Gaussian Radial.6","Gaussian Radial.7","Gaussian Radial.8",
                                    "SVM Radial.3","SVM Radial.4","SVM Radial.5","SVM Radial.6","SVM Radial.7","SVM Radial.8"),
                           Accuracy = c(model.rf.3$results$Accuracy[2],model.rf.4$results$Accuracy[2],model.rf.5$results$Accuracy[2],model.rf.6$results$Accuracy[2],model.rf.7$results$Accuracy[2],model.rf.8$results$Accuracy[2],
                                        model.gaussprRadial.3$results$Accuracy,model.gaussprRadial.4$results$Accuracy,model.gaussprRadial.5$results$Accuracy,model.gaussprRadial.6$results$Accuracy,model.gaussprRadial.7$results$Accuracy,model.gaussprRadial.8$results$Accuracy,
                                        model.svmRadial.3$results$Accuracy[1],model.svmRadial.4$results$Accuracy[1],model.svmRadial.5$results$Accuracy[1],model.svmRadial.6$results$Accuracy[1],model.svmRadial.7$results$Accuracy[1],model.svmRadial.8$results$Accuracy[1]),
                           Kappa = c(model.rf.3$results$Kappa[2],model.rf.4$results$Kappa[2],model.rf.5$results$Kappa[2],model.rf.6$results$Kappa[2],model.rf.7$results$Kappa[2],model.rf.8$results$Kappa[2],
                                     model.gaussprRadial.3$results$Kappa,model.gaussprRadial.4$results$Kappa,model.gaussprRadial.5$results$Kappa,model.gaussprRadial.6$results$Kappa,model.gaussprRadial.7$results$Kappa,model.gaussprRadial.8$results$Kappa,
                                     model.svmRadial.3$results$Kappa[1],model.svmRadial.4$results$Kappa[1],model.svmRadial.5$results$Kappa[1],model.svmRadial.6$results$Kappa[1],model.svmRadial.7$results$Kappa[1],model.svmRadial.8$results$Kappa[1]))
    model.cp$i <- i
    datalist[[i]] <- model.cp
  }
  MFselect = do.call(rbind, datalist)
}
# write.csv(MFselect, file = "Diabetes F&A selection.csv", na="-", fileEncoding = "CP949")
{  
  MFselect.t = read.csv("Diabetes F&A selection.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
  head(MFselect.t)
  MFselect.t = MFselect.t[c(1,3,4,6)]
  head(MFselect.t)
  MFselect.t$Feature = as.character(MFselect.t$Feature)
}
{
  MFselect.t$Model <- factor(MFselect.t$Model, levels = c("Gaussian Radial", "Randomforest", "SVM Radial"))
  
  MFselect.t$Feature <- factor(MFselect.t$Feature, levels = c("3","4","5","6","7","8","9"))
  
  # modelnames <- c("BstLm", "Gaussian Radial", "Randomforest", "SVM Radial", "GBM", "GLM")
  
  Feature.plot.Accuracy <- ggplot(data = MFselect.t, aes(x = Feature, y = Accuracy, fill = Feature, group = Feature)) +
    geom_boxplot(outlier.colour = NA)+
    theme_bw() + #plot 영역에 박스, 배경제거
    theme(legend.position = c(""),
          strip.background =element_rect(fill="white"),
          strip.text = element_text(size = 25, face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),# panel.grid.minor는 보조 구분선
          plot.title = element_text(size = 30),
          axis.title.x = element_text(size=20),
          axis.text.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          axis.text.y = element_text(size=20)) +
    ggtitle("Feature & Algorithm Selection for Diabetes prediction") +
    ylab("Accuracy")+xlab("Number of Parameters")+
    # scale_fill_manual(values = c("red3", "orange2", "dodgerblue3", "green4", "purple", "deeppink3"))+
    facet_grid(~Model, scales = "free", space = "free")
  # geom_hline(yintercept = 0.9144, color = "red", lty = "dashed", size = 0.8)
  
  # scale_fill_manual(values = c("red2", "red4", "orange", "orange3", "dodgerblue3", "dodgerblue4","green2", "green4"))
  Feature.plot.Accuracy
  
  Feature.plot.Kappa <- ggplot(data = MFselect.t, aes(x = Feature, y = Kappa, fill = Feature, group = Feature)) +
    geom_boxplot(outlier.colour = NA)+
    theme_bw() + #plot 영역에 박스, 배경제거
    theme(legend.position = c(""),
          strip.background =element_rect(fill="white"),
          strip.text = element_text(size = 25, face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),# panel.grid.minor는 보조 구분선
          plot.title = element_text(size = 30),
          axis.title.x = element_text(size=20),
          axis.text.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          axis.text.y = element_text(size=20)) +
    ggtitle("Feature & Algorithm Selection for Diabetes prediction") +
    ylab("Kappa")+xlab("Number of Parameters")+
    # scale_fill_manual(values = c("red3", "orange2", "dodgerblue3", "green4", "purple", "deeppink3"))+
    facet_grid(~Model, scales = "free", space = "free")
  # geom_hline(yintercept = 0.9144, color = "red", lty = "dashed", size = 0.8)
  
  # scale_fill_manual(values = c("red2", "red4", "orange", "orange3", "dodgerblue3", "dodgerblue4","green2", "green4"))
  Feature.plot.Kappa
}

a=set_palette(Feature.plot.Accuracy, "Set1")
b=set_palette(Feature.plot.Kappa, "Set1")
a
b
MFselect.t = read.csv("Diabetes F&A selection.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
head(MFselect.t)
{ 
  MFselect.t.G = subset(MFselect.t, MFselect.t$Model== "Gaussian Radial")
  MFselect.t.G.3 = subset(MFselect.t.G, MFselect.t.G$Case== "Gaussian Radial.3")
  MFselect.t.G.4 = subset(MFselect.t.G, MFselect.t.G$Case== "Gaussian Radial.4")
  MFselect.t.G.5 = subset(MFselect.t.G, MFselect.t.G$Case== "Gaussian Radial.5")
  MFselect.t.G.6 = subset(MFselect.t.G, MFselect.t.G$Case== "Gaussian Radial.6")
  MFselect.t.G.7 = subset(MFselect.t.G, MFselect.t.G$Case== "Gaussian Radial.7")
  MFselect.t.G.8 = subset(MFselect.t.G, MFselect.t.G$Case== "Gaussian Radial.8")
  
  MFselect.t.R = subset(MFselect.t, MFselect.t$Model== "Randomforest")
  MFselect.t.R.3 = subset(MFselect.t.R, MFselect.t.R$Case== "Rf.3")
  MFselect.t.R.4 = subset(MFselect.t.R, MFselect.t.R$Case== "Rf.4")
  MFselect.t.R.5 = subset(MFselect.t.R, MFselect.t.R$Case== "Rf.5")
  MFselect.t.R.6 = subset(MFselect.t.R, MFselect.t.R$Case== "Rf.6")
  MFselect.t.R.7 = subset(MFselect.t.R, MFselect.t.R$Case== "Rf.7")
  MFselect.t.R.8 = subset(MFselect.t.R, MFselect.t.R$Case== "Rf.8")
  
  MFselect.t.S = subset(MFselect.t, MFselect.t$Model== "SVM Radial")
  MFselect.t.S.3 = subset(MFselect.t.S, MFselect.t.S$Case== "SVM Radial.3")
  MFselect.t.S.4 = subset(MFselect.t.S, MFselect.t.S$Case== "SVM Radial.4")
  MFselect.t.S.5 = subset(MFselect.t.S, MFselect.t.S$Case== "SVM Radial.5")
  MFselect.t.S.6 = subset(MFselect.t.S, MFselect.t.S$Case== "SVM Radial.6")
  MFselect.t.S.7 = subset(MFselect.t.S, MFselect.t.S$Case== "SVM Radial.7")
  MFselect.t.S.8 = subset(MFselect.t.S, MFselect.t.S$Case== "SVM Radial.8")
}
{  
  summary(MFselect.t.G.3) 
  summary(MFselect.t.G.4)
  summary(MFselect.t.G.5)
  summary(MFselect.t.G.6)
  summary(MFselect.t.G.7)
  summary(MFselect.t.G.8)
  
  summary(MFselect.t.R.3)
  summary(MFselect.t.R.4)
  summary(MFselect.t.R.5)
  summary(MFselect.t.R.6)
  summary(MFselect.t.R.7)
  summary(MFselect.t.R.8)
  
  summary(MFselect.t.S.3)
  summary(MFselect.t.S.4)
  summary(MFselect.t.S.5)
  summary(MFselect.t.S.6)
  summary(MFselect.t.S.7)
  summary(MFselect.t.S.8)
}

#===============================================================================
#비교를 위해 처음 데이터 세트로 모델 개발

#예측 모델을 위한 NET 데이터를 생성함
{
  sa.net.n7 = read.csv("sa.net.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
  head(sa.net.n7)
  str(sa.net.n7)
  {
    sa.net.n7$AGE_P = as.numeric(sa.net.n7$AGE_P) 
    sa.net.n7$VIGFREQW = as.numeric(sa.net.n7$VIGFREQW) 
    sa.net.n7$VIGMIN = as.numeric(sa.net.n7$VIGMIN) 
    sa.net.n7$MODFREQW = as.numeric(sa.net.n7$MODFREQW) 
    sa.net.n7$MODMIN = as.numeric(sa.net.n7$MODMIN)
    sa.net.n7$BMI = as.numeric(sa.net.n7$BMI)
    sa.net.n7$AHEIGHT = as.numeric(sa.net.n7$AHEIGHT)
    sa.net.n7$AWEIGHTP = as.numeric(sa.net.n7$AWEIGHTP)
    sa.net.n7$ALC12MWK = as.numeric(sa.net.n7$ALC12MWK)
    sa.net.n7$ALCAMT = as.numeric(sa.net.n7$ALCAMT)
    sa.net.n7$DIBEV1 = as.character(sa.net.n7$DIBEV1)
  }
  str(sa.net.n7)
}

sa.net.n7 = sa.net.n7[-1]
head(sa.net.n7)

# 샘플링할 비율 설정
sampling_rate <- 0.08

# 샘플링할 데이터의 수 계산
n_samples <- round(nrow(sa.net.n7) * sampling_rate)

# 랜덤 샘플링
sa.net.n7 <- sa.net.n7[sample(nrow(sa.net.n7), n_samples), ]


inTraining <- createDataPartition(sa.net.n7$DIBEV1, p = .8, list = FALSE)
train <- sa.net.n7[ inTraining,]
table(train$DIBEV1)
test  <- sa.net.n7[-inTraining,]

model <- train(DIBEV1~.,data = train, method = 'gaussprRadial') #
# saveRDS(model, "Diabetes_prediction(raw).rds")
model = readRDS("Diabetes_prediction(raw).rds")

model
# 모델의 성능 평가
pred <- predict(model, newdata = test[c(1:10)])
str(pred)
str(test$DIBEV1)
test$DIBEV1 = as.factor(test$DIBEV1)
# pred와 test[7]을 같은 레벨의 팩터로 변환
levels(pred) <- levels(test$DIBEV1)
cm <- confusionMatrix(pred, test$DIBEV1)

# 전체 통계 출력
print(cm$overall)

#===============================================================================
#최종 예측 모델

# 데이터 불러오기

sa.net.fm = read.csv("sa.net.fm.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
{
  # 데이터 전처리
  sa.net.fm$AGE_P = as.numeric(sa.net.fm$AGE_P) 
  sa.net.fm$VIGFREQW = as.numeric(sa.net.fm$VIGFREQW) 
  sa.net.fm$VIGMIN = as.numeric(sa.net.fm$VIGMIN) 
  sa.net.fm$MODFREQW = as.numeric(sa.net.fm$MODFREQW) 
  sa.net.fm$MODMIN = as.numeric(sa.net.fm$MODMIN)
  sa.net.fm$BMI = as.numeric(sa.net.fm$BMI)
  sa.net.fm$AHEIGHT = as.numeric(sa.net.fm$AHEIGHT)
  sa.net.fm$ALC12MWK = as.numeric(sa.net.fm$ALC12MWK)
  sa.net.fm$DIBEV1 = as.factor(sa.net.fm$DIBEV1)
  
  # 클래스별 데이터 분할
  data_class1 <- sa.net.fm[sa.net.fm$DIBEV1 == 1,]
  data_class2 <- sa.net.fm[sa.net.fm$DIBEV1 == 2,]
  
  # 클래스별 샘플링
  n_samples_class1 <- round(nrow(data_class1) * 1)
  n_samples_class2 <- round(nrow(data_class2) * 0.08)
  
  sampled_data_class1 <- data_class1[sample(nrow(data_class1), n_samples_class1), ]
  sampled_data_class2 <- data_class2[sample(nrow(data_class2), n_samples_class2), ]
  
  # 샘플링된 데이터 합치기
  sampled_data <- rbind(sampled_data_class1, sampled_data_class2)
  
  sampled_data = sampled_data[,c(6,1,7,5,3,4,9)]
  
  inTraining <- createDataPartition(sampled_data$DIBEV1, p = .8, list = FALSE)
  train <- sampled_data[ inTraining,]
  table(train$DIBEV1)
  test  <- sampled_data[-inTraining,]
}
# write.csv(train, file = "train saved.csv", na="-", fileEncoding = "CP949")
# write.csv(test, file = "test saved.csv", na="-", fileEncoding = "CP949")
train = read.csv("train saved.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
test = read.csv("test saved.csv", na="-", fileEncoding = "CP949", encoding = "UTF-8")
train = train[-1]
test = test[-1]
library(mlr)
# 훈련 데이터를 mlr 패키지가 사용할 수 있는 형태로 변환
train_task <- makeClassifTask(data = train, target = "DIBEV1")

# SVM 모델 설정
learner <- makeLearner("classif.svm", predict.type = "prob")

# 하이퍼파라미터 튜닝을 위한 그리드 설정
param_set <- makeParamSet(
  makeNumericParam("cost", lower = 0.1, upper = 10),
  makeNumericParam("gamma", lower = 0.01, upper = 1)
)

# 하이퍼파라미터 튜닝 방법 설정
ctrl <- makeTuneControlGrid()

# 하이퍼파라미터 튜닝 수행
res <- tuneParams(learner, train_task, resampling = cv5, par.set = param_set, control = ctrl)

# 최적의 하이퍼파라미터 출력
print(res$x)

# 최적의 하이퍼파라미터로 모델 훈련
learner <- setHyperPars(learner, par.vals = res$x)
model <- train(learner, train_task)

model
# saveRDS(model, "Diabetes_prediction(3).rds")
# model = readRDS("Diabetes_prediction(3).rds")
# saveRDS(model, "Diabetes_prediction(4).rds")
model = readRDS("Diabetes_prediction(4).rds")

# 테스트 데이터로 예측
pred <- predict(model, newdata = test[,1:6])

str(pred$data$response)
str(test$DIBEV1)
test$DIBEV1 = as.factor(test$DIBEV1)
# Confusion matrix 생성
cm <- confusionMatrix(pred$data$response, test$DIBEV1)

# Confusion matrix 출력
print(cm)
#----------------------
# Confusion Matrix 생성
cm <- confusionMatrix(factor(pred$data$response), factor(test$DIBEV1), dnn = c("Prediction", "Reference"))

# 데이터 프레임으로 변환
plt <- as.data.frame(cm$table)

# ggplot으로 시각화
ggplot(plt, aes(Reference, Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 7.5) +
  scale_fill_gradient(low = "white", high = brewer.pal(9, "Set3")[1]) +
  labs(x = "Reference", y = "Prediction") +
  scale_x_discrete(labels = c("Diabetes", "No Diabetes")) +
  scale_y_discrete(labels = c("Diabetes", "No Diabetes")) +
  theme_bw() +
  theme(legend.position = c(""),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(size = 25, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 23),
        axis.title.x = element_text(size=20),
        axis.text.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y = element_text(size=20)) +
  ggtitle("Confusion Matrix for Diabetes Prediction")

# 필요한 패키지 설치
if (!require(pROC)) install.packages("pROC")
if (!require(ggplot2)) install.packages("ggplot2")

# ROC Curve 생성
roc_obj <- roc(test$DIBEV1, as.numeric(pred$data$response))

# 데이터 프레임으로 변환
roc_df <- data.frame(
  FPR = 1 - roc_obj$specificities,  # False Positive Rate 계산
  TPR = roc_obj$sensitivities
)

# ggplot2로 시각화
ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(size=2) +
  labs(x = "False Positive Rate", y = "True Positive Rate", title = "ROC Curve") +
  theme_bw() +
  theme(legend.position = c(""),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(size = 25, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 25),
        axis.title.x = element_text(size=20),
        axis.text.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y = element_text(size=20)) +
  geom_text(aes(x = 0.5, y = 0.5, label = paste("AUC =", round(auc(roc_obj), 3))), hjust = 0, vjust = 0,size=6)
