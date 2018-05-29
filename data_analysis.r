######Data analysis script for 'Decision Variables in Visual Gravity Judgments on Parabolic Motion'


####################################################
######load packages and other code
####################################################
library(ggplot2)
source("parabolic.r")
library(binr)
#source("binvar.r")
#source("myUtils.r")
library(lme4)
library(tidyverse)
library(quickpsy)
library(gridExtra)
library(grid)
require(cowplot)
BlauUB <- "#07579C"
Yellow <- "#ffa007"
Red <- "#ff2407"

########################################################################################################
####################################################Experiment 1
########################################################################################################

####################################################
######load data
####################################################

a1 <- read.table(".../bjsummary1.log", header = TRUE)
a2 <- read.table(".../bjsummary2.log", header = TRUE)
a3 <- read.table(".../bjsummary3.log", header = TRUE)
a4 <- read.table(".../bjsummary4.log", header = TRUE)
b1 <- read.table(".../lj1.log", header = TRUE)
b2 <- read.table(".../lj2.log", header = TRUE)
b3 <- read.table(".../lj3.log", header = TRUE)
b4 <- read.table(".../lj4.log", header = TRUE)
c1 <- read.table(".../dasummary1.log", header = TRUE)
c2 <- read.table(".../dasummary2.log", header = TRUE)
c3 <- read.table(".../dasummary3.log", header = TRUE)
c4 <- read.table(".../dasummary4.log", header = TRUE)
d1 <- read.table(".../jmsummary1.log", header = TRUE)
d2 <- read.table(".../jmsummary2.log", header = TRUE)
d3 <- read.table(".../jmsummary3.log", header = TRUE)
d4 <- read.table(".../jmsummary4.log", header = TRUE)
e1 <- read.table(".../ccsummary1.log", header = TRUE)
e2 <- read.table(".../ccsummary2.log", header = TRUE)
e3 <- read.table(".../ccsummary3.log", header = TRUE)
e4 <- read.table(".../ccsummary4.log", header = TRUE)
f1 <- read.table(".../cmsummary1.log", header = TRUE)
f2 <- read.table(".../cmsummary2.log", header = TRUE)
f3 <- read.table(".../cmsummary3.log", header = TRUE)
f4 <- read.table(".../cmsummary4.log", header = TRUE)
g1 <- read.table(".../casummary1.log", header = TRUE)
g2 <- read.table(".../casummary2.log", header = TRUE)
g3 <- read.table(".../casummary3.log", header = TRUE)
g4 <- read.table(".../casummary4.log", header = TRUE)
h1 <- read.table(".../lksummary1.log", header = TRUE)
h2 <- read.table(".../lksummary2.log", header = TRUE)
h3 <- read.table(".../lksummary3.log", header = TRUE)
h4 <- read.table(".../lksummary4.log", header = TRUE)
i1 <- read.table(".../misummary1.log", header = TRUE)
i2 <- read.table(".../misummary2.log", header = TRUE)
i3 <- read.table(".../misummary3.log", header = TRUE)
i4 <- read.table(".../misummary4.log", header = TRUE)
#j1 <- read.table(".../gusummary1.log", header = TRUE) ###subject didn't follow instructions
#j2 <- read.table(".../gusummary2.log", header = TRUE)
#j3 <- read.table(".../gusummary3.log", header = TRUE)
#j4 <- read.table(".../gusummary4.log", header = TRUE)
k1 <- read.table(".../bosummary1.log", header = TRUE)
k2 <- read.table(".../bosummary2.log", header = TRUE)
k3 <- read.table(".../bosummary3.log", header = TRUE)
k4 <- read.table(".../bosummary4.log", header = TRUE)


####################################################
######add basic info
####################################################

######participant IDs
a1$id <- "s01"
a2$id <- "s01"
a3$id <- "s01"
a4$id <- "s01"
b1$id <- "s02"
b2$id <- "s02"
b3$id <- "s02"
b4$id <- "s02"
c1$id <- "s03"
c2$id <- "s03"
c3$id <- "s03"
c4$id <- "s03"
d1$id <- "s04"
d2$id <- "s04"
d3$id <- "s04"
d4$id <- "s04"
e1$id <- "s05"
e2$id <- "s05"
e3$id <- "s05"
e4$id <- "s05"
f1$id <- "s06"
f2$id <- "s06"
f3$id <- "s06"
f4$id <- "s06"
g1$id <- "s07"
g2$id <- "s07"
g3$id <- "s07"
g4$id <- "s07"
h1$id <- "s08"
h2$id <- "s08"
h3$id <- "s08"
h4$id <- "s08"
i1$id <- "s09"
i2$id <- "s09"
i3$id <- "s09"
i4$id <- "s09"
#j1$id <- "s10"
#j2$id <- "s10"
#j3$id <- "s10"
#j4$id <- "s10"
k1$id <- "s11"
k2$id <- "s11"
k3$id <- "s11"
k4$id <- "s11"

#####trial numbers
a2$trial <- seq(140,279,1)
b2$trial <- seq(140,279,1)
c2$trial <- seq(140,279,1)
d2$trial <- seq(140,279,1)
e2$trial <- seq(140,279,1)
f2$trial <- seq(140,279,1)
g2$trial <- seq(140,279,1)
h2$trial <- seq(140,279,1)
i2$trial <- seq(140,279,1)
#j2$trial <- seq(140,279,1)
k2$trial <- seq(140,279,1)

a3$trial <- seq(280,419,1)
b3$trial <- seq(280,419,1)
c3$trial <- seq(280,419,1)
d3$trial <- seq(280,419,1)
e3$trial <- seq(280,419,1)
f3$trial <- seq(280,419,1)
g3$trial <- seq(280,419,1)
h3$trial <- seq(280,419,1)
i3$trial <- seq(280,419,1)
#j3$trial <- seq(280,419,1)
k3$trial <- seq(280,419,1)

a4$trial <- seq(420,559,1)
b4$trial <- seq(420,559,1)
c4$trial <- seq(420,559,1)
d4$trial <- seq(420,559,1)
e4$trial <- seq(420,559,1)
f4$trial <- seq(420,559,1)
g4$trial <- seq(420,559,1)
h4$trial <- seq(420,559,1)
i4$trial <- seq(420,559,1)
#j4$trial <- seq(420,559,1)
k4$trial <- seq(420,559,1)


#####add height
a1$tall <- 1.85
a2$tall <- 1.85
a3$tall <- 1.85
a4$tall <- 1.85
b1$tall <- 1.73
b2$tall <- 1.73
b3$tall <- 1.73
b4$tall <- 1.73
c1$tall <- 1.88
c2$tall <- 1.88
c3$tall <- 1.88
c4$tall <- 1.88
d1$tall <- 1.70
d2$tall <- 1.70
d3$tall <- 1.70
d4$tall <- 1.70 
e1$tall <- 1.72
e2$tall <- 1.72
e3$tall <- 1.72
e4$tall <- 1.72
f1$tall <- 1.80
f2$tall <- 1.80
f3$tall <- 1.80
f4$tall <- 1.80
g1$tall <- 1.65
g2$tall <- 1.65
g3$tall <- 1.65
g4$tall <- 1.65
h1$tall <- 1.71
h2$tall <- 1.71
h3$tall <- 1.71
h4$tall <- 1.71
i1$tall <- 1.68
i2$tall <- 1.68
i3$tall <- 1.68
i4$tall <- 1.68
#j1$tall <- 1.75
#j2$tall <- 1.75
#j3$tall <- 1.75
#j4$tall <- 1.75
k1$tall <- 1.68
k2$tall <- 1.68
k3$tall <- 1.68
k4$tall <- 1.68

#####make one big data frame with eeeeeveryone and eeeeeeverything
x2 <- rbind(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4,e1,e2,e3,e4,f1,f2,f3,f4,g1,g2,g3,g4,h1,h2,h3,h4,i1,i2,i3,i4,k1,k2,k3,k4)


####################################################
######Calculate needed values
####################################################

#####"stimulus strength"/difference between gravities
x2$gdif <- x2$gval-9.82

#####add normalized column for psychometric "is larger than" function
x2$GLarge <- x2$Response
x2$GLarge[x2$stdFirst==0] <- 1-x2$Response[x2$stdFirst==0]

####add whether answer is correct or not, allocate randomly yes or no when gravities are the same
for(i in 1:length(x2$gval)){
  if (x2$gval[i]>9.820 & x2$stdFirst[i] == 0 & x2$Response[i] == 1){
    x2$correct[i] <- 0}
  else if (x2$gval[i]>9.820 & x2$stdFirst[i] == 1 & x2$Response[i] == 0){
    x2$correct[i] <- 0}
  else if (x2$gval[i]<9.820 & x2$stdFirst[i] == 0 & x2$Response[i] == 0){
    x2$correct[i] <- 0}
  else if (x2$gval[i]<9.820 & x2$stdFirst[i] == 1 & x2$Response[i] == 1){
    x2$correct[i] <- 0}
  else if (x2$gval[i] == 9.820){
    x2$correct[i] <- sample(0:1,1)}
  else {
    x2$correct[i] <- 1}
}


####Decision variables = differences in visual parameters
##Elevation Angle and derivatives at 60%
x2$ElevationYIndependent0.6_Dif <- ElevationAngleYIndependent(x2$ttc*0.6,x2$gval,x2$velV,x2$velH,x2$tall) -
  ElevationAngleYIndependent(x2$ttcStd*0.6,9.81,x2$velVStd,x2$velHStd,x2$tall)
x2$ElevationPrimeYIndependent0.6_Dif <- ElevationAnglePrimeYIndependent(x2$ttc*0.6,x2$gval,x2$velV,x2$velH,x2$tall) -
  ElevationAnglePrimeYIndependent(x2$ttcStd*0.6,9.81,x2$velVStd,x2$velHStd,x2$tall)

x2$ElevationYIndependent0.6_Dif_b <- AddBinnedValues(x2$ElevationYIndependent0.6_Dif)
x2$ElevationPrimeYIndependent0.6_Dif_b <- AddBinnedValues(x2$ElevationPrimeYIndependent0.6_Dif)

##Elevation Angle and derivatives at 75%
x2$ElevationYIndependent0.75_Dif <- ElevationAngleYIndependent(x2$ttc*0.75,x2$gval,x2$velV,x2$velH,x2$tall) -
  ElevationAngleYIndependent(x2$ttcStd*0.75,9.81,x2$velVStd,x2$velHStd,x2$tall)
x2$ElevationPrimeYIndependent0.75_Dif <- ElevationAnglePrimeYIndependent(x2$ttc*0.75,x2$gval,x2$velV,x2$velH,x2$tall) -
  ElevationAnglePrimeYIndependent(x2$ttcStd*0.75,9.81,x2$velVStd,x2$velHStd,x2$tall)

x2$ElevationYIndependent0.75_Dif_b <- AddBinnedValues(x2$ElevationYIndependent0.75_Dif)
x2$ElevationPrimeYIndependent0.75_Dif_b <- AddBinnedValues(x2$ElevationPrimeYIndependent0.75_Dif)


##########################
##Visual Angle Information
##########################
##Visual Angle and derivatives at 60%
x2$VisualAngleYI0.6_Dif <- getVisualAngleYIndependent(x2$ttc*0.6,x2$radius,x2$velH,x2$velV,x2$gval,y=x2$tall) -
  getVisualAngleYIndependent(x2$ttcStd*0.6,x2$radius,x2$velHStd,x2$velVStd,9.81,y=x2$tall)
x2$VisualAnglePrimeYI0.6_Dif <- getVisualAnglePrimeYIndependent(x2$ttc*0.6,x2$radius,x2$velH,x2$velV,x2$gval,y=x2$tall) -
  getVisualAnglePrimeYIndependent(x2$ttcStd*0.6,x2$radius,x2$velHStd,x2$velVStd,9.81,y=x2$tall)  

x2$VisualAngleYI0.6_Dif_b <- AddBinnedValues(x2$VisualAngleYI0.6_Dif)
x2$VisualAnglePrimeYI0.6_Dif_b <- AddBinnedValues(x2$VisualAnglePrimeYI0.6_Dif)

##Visual Angle and derivatives at 75%
x2$VisualAngleYI0.75_Dif <- getVisualAngleYIndependent(x2$ttc*0.75,x2$radius,x2$velH,x2$velV,x2$gval,y=x2$tall) -
  getVisualAngleYIndependent(x2$ttcStd*0.75,x2$radius,x2$velHStd,x2$velVStd,9.81,y=x2$tall)
x2$VisualAnglePrimeYI0.75_Dif <- getVisualAnglePrimeYIndependent(x2$ttc*0.75,x2$radius,x2$velH,x2$velV,x2$gval,y=x2$tall) -
  getVisualAnglePrimeYIndependent(x2$ttcStd*0.75,x2$radius,x2$velHStd,x2$velVStd,9.81,y=x2$tall)

x2$VisualAngleYI0.75_Dif_b <- AddBinnedValues(x2$VisualAngleYI0.75_Dif)
x2$VisualAnglePrimeYI0.75_Dif_b <- AddBinnedValues(x2$VisualAnglePrimeYI0.75_Dif)

##Visual Angle and derivatives at 88%
x2$VisualAngleYI0.88_Dif <- getVisualAngleYIndependent(x2$ttc*0.88,x2$radius,x2$velH,x2$velV,x2$gval,y=x2$tall) -
  getVisualAngleYIndependent(x2$ttcStd*0.88,x2$radius,x2$velHStd,x2$velVStd,9.81,y=x2$tall)
x2$VisualAngleYI0.88_Dif_b <- AddBinnedValues(x2$VisualAngleYI0.88_Dif)
x2$VisualAnglePrimeYI0.88_Dif <- getVisualAnglePrimeYIndependent(x2$ttc*0.88,x2$radius,x2$velH,x2$velV,x2$gval,y=x2$tall) -
  getVisualAnglePrimeYIndependent(x2$ttcStd*0.88,x2$radius,x2$velHStd,x2$velVStd,9.81,y=x2$tall)
x2$VisualAnglePrimeYI0.88_Dif_b <- AddBinnedValues(x2$VisualAnglePrimeYI0.88_Dif)


########informed guess about integration of cues (GS model estimate), again taken as difference/"stimulus strength"
#####at 75% of the trajectory
x2$gEstimate_75_Dif <- gEstimate(0.25*getTimetoYIsZero(x2$velV,x2$gval), 
                                 x2$gval,
                                 ElevationAnglePrimeYIndependent(x2$ttc*0.75,x2$gval,x2$velV,x2$velH,x2$tall),
                                 ElevationAngleYIndependent(x2$ttc*0.75,x2$gval,x2$velV,x2$velH,x2$tall),
                                 getVisualAngleYIndependent(x2$ttc*0.75,x2$radius,x2$velH,x2$velV,x2$gval,y=x2$tall),
                                 x2$radius) -
  gEstimate(0.25*getTimetoYIsZero(x2$velVStd,9.81), 
            9.81,
            ElevationAnglePrimeYIndependent(x2$ttcStd*0.75,9.81,x2$velVStd,x2$velHStd,x2$tall),
            ElevationAngleYIndependent(x2$ttcStd*0.75,9.81,x2$velVStd,x2$velHStd,x2$tall),
            getVisualAngleYIndependent(x2$ttcStd*0.75,0.033,x2$velHStd,x2$velVStd,9.81,y=x2$tall),
            0.033)

#####at 60% of the trajectory
x2$gEstimate_60_Dif <- gEstimate(0.4*getTimetoYIsZero(x2$velV,x2$gval), 
                                 x2$gval,
                                 ElevationAnglePrimeYIndependent(x2$ttc*0.6,x2$gval,x2$velV,x2$velH,x2$tall),
                                 ElevationAngleYIndependent(x2$ttc*0.6,x2$gval,x2$velV,x2$velH,x2$tall),
                                 getVisualAngleYIndependent(x2$ttc*0.6,x2$radius,x2$velH,x2$velV,x2$gval,y=x2$tall),
                                 x2$radius) -
  gEstimate(0.4*getTimetoYIsZero(x2$velVStd,9.81), 
            9.81,
            ElevationAnglePrimeYIndependent(x2$ttcStd*0.6,9.81,x2$velVStd,x2$velHStd,x2$tall),
            ElevationAngleYIndependent(x2$ttcStd*0.6,9.81,x2$velVStd,x2$velHStd,x2$tall),
            getVisualAngleYIndependent(x2$ttcStd*0.6,0.033,x2$velHStd,x2$velVStd,9.81,y=x2$tall),
            0.033)

x2$gEstimate_75_Dif_b <- AddBinnedValues(x2$gEstimate_75_Dif)
x2$gEstimate_60_Dif_b <- AddBinnedValues(x2$gEstimate_60_Dif)

x2$ttc_75_Dif <- ttc(x2$gval,
                     ElevationAnglePrimeYIndependent(x2$ttc*0.75,x2$gval,x2$velV,x2$velH,x2$tall),
                     ElevationAngleYIndependent(x2$ttc*0.75,x2$gval,x2$velV,x2$velH,x2$tall),
                     getVisualAngleYIndependent(x2$ttc*0.75,x2$radius,x2$velH,x2$velV,x2$gval,y=x2$tall),
                     x2$radius) -
  ttc(9.81,
      ElevationAnglePrimeYIndependent(x2$ttcStd*0.75,9.81,x2$velVStd,x2$velHStd,x2$tall),
      ElevationAngleYIndependent(x2$ttcStd*0.75,9.81,x2$velVStd,x2$velHStd,x2$tall),
      getVisualAngleYIndependent(x2$ttcStd*0.75,0.033,x2$velHStd,x2$velVStd,9.81,y=x2$tall),
      0.033)
x2$ttc_75_Dif_b <- AddBinnedValues(x2$ttc_75_Dif)

################other, simplified approximations based on GS model
####Y*/cosY
x2$ElevPrimeByCos_Dif <- ElevationAnglePrimeYIndependent(x2$ttc*0.75,x2$gval,x2$velV,x2$velH,x2$tall)/
  cos(ElevationAngleYIndependent(x2$ttc*0.75,x2$gval,x2$velV,x2$velH,x2$tall)) -
  ElevationAnglePrimeYIndependent(x2$ttcStd*0.75,9.81,x2$velVStd,x2$velHStd,x2$tall)/
  cos(ElevationAngleYIndependent(x2$ttcStd*0.75,9.81,x2$velVStd,x2$velHStd,x2$tall))
x2$ElevPrimeByCos_Dif_b <- AddBinnedValues(x2$ElevPrimeByCos_Dif)*(-1)
cor(x2$ElevPrimeByCos_Dif_b,x2$gval)

####Y*/O
x2$ElevPrimeByTheta_Dif <- ElevationAnglePrimeYIndependent(x2$ttc*0.75,x2$gval,x2$velV,x2$velH,x2$tall)/
  getVisualAngleYIndependent(x2$ttc*0.75,x2$radius,x2$velH,x2$velV,x2$gval,y=x2$tall) -
  ElevationAnglePrimeYIndependent(x2$ttcStd*0.75,9.81,x2$velVStd,x2$velHStd,x2$tall)/
  getVisualAngleYIndependent(x2$ttcStd*0.75,0.033,x2$velHStd,x2$velVStd,9.81,y=x2$tall)
x2$ElevPrimeByTheta_Dif_b <- AddBinnedValues(x2$ElevPrimeByTheta_Dif)*(-1)
cor(x2$ElevPrimeByTheta_Dif,x2$gval)

###Y*/O*sin Y
x2$ElevPrimeByThetaTimesCos_Dif <- ElevationAnglePrimeYIndependent(x2$ttc*0.75,x2$gval,x2$velV,x2$velH,x2$tall)/
  (getVisualAngleYIndependent(x2$ttc*0.75,x2$radius,x2$velH,x2$velV,x2$gval,y=x2$tall)*
     cos(ElevationAngleYIndependent(x2$ttc*0.75,x2$gval,x2$velV,x2$velH,x2$tall))) -
  ElevationAnglePrimeYIndependent(x2$ttcStd*0.75,9.81,x2$velVStd,x2$velHStd,x2$tall)/
  (getVisualAngleYIndependent(x2$ttcStd*0.75,0.033,x2$velHStd,x2$velVStd,9.81,y=x2$tall)*
     cos(ElevationAngleYIndependent(x2$ttcStd*0.75,9.81,x2$velVStd,x2$velHStd,x2$tall)))
x2$ElevPrimeByThetaTimesCos_Dif_b <- AddBinnedValues(x2$ElevPrimeByThetaTimesCos_Dif)*(-1)



##Calculate stimulus strengths for Drift Diffusion Modelling
##mean value across relevant part (0.3 til 0.8)
x2$ElevationPrimeYIndependent0.75_Dif_b/
  abs(range(x2$ElevationPrimeYIndependent0.75_Dif_b)[2]-range(x2$ElevationPrimeYIndependent0.75_Dif_b)[1])
x2$VisualAngleYI0.75_Dif_b_Stdd <- x2$VisualAngleYI0.75_Dif_b/
  abs(range(x2$VisualAngleYI0.75_Dif_b)[2]-range(x2$VisualAngleYI0.75_Dif_b)[1])
ElevationAnglePrimeYIndependent(x2$ttc*0.75,x2$gval,x2$velV,x2$velH,x2$tall) -
  ElevationAnglePrimeYIndependent(x2$ttcStd*0.75,9.81,x2$velVStd,x2$velHStd,x2$tall)
getVisualAngleYIndependent(x2$ttc*0.75,x2$radius,x2$velH,x2$velV,x2$gval,y=x2$tall) -
  getVisualAngleYIndependent(x2$ttcStd*0.75,x2$radius,x2$velHStd,x2$velVStd,9.81,y=x2$tall)

x2$ttc_b <- AddBinnedValues(x2$ttc)
x2$ttc_Dif_b <- AddBinnedValues(x2$ttc - x2$ttcStd)


############################################################
##### The parabolas (spatially) (FIGURE 1)
############################################################
vv <- c(3.7,5.2)
vh <- c(6,8.3)
ttc <- getTimetoYIsZero(6,9.82)
binsize <- 0.001
vhvv <- expand.grid(vh=vh,vv=vv)
aux <- NULL
gvals <- round(unique(x2$gval),3)
for(i in 1:nrow(vhvv)){
  for (j in gvals){
    ttc <- (2*vhvv$vv[i])/j
    tt <- seq(0,ttc,by=binsize)
    pG1 <- getXYpos(tt,vhvv$vh[i],vhvv$vv[i],g=j)
    aux <- rbind(aux,data.frame(path=i,time=tt,ttc=ttc,vh=vhvv$vh[i],vv=vhvv$vv[i],x=pG1$x,y=pG1$y,g=j,matched=0))}}
aux$VelCombination[aux$vv == 3.7 & aux$vh == 6] <- "3.7 & 6"
aux$VelCombination[aux$vv == 3.7 & aux$vh == 8.3] <- "3.7 & 8.3"
aux$VelCombination[aux$vv == 5.2 & aux$vh == 6] <- "5.2 & 6"
aux$VelCombination[aux$vv == 5.2 & aux$vh == 8.3] <- "5.2 & 8.3"
aux$x <- aux$x*(-1)
aux$y <- aux$y+0.565

factornames <- c(
  '6.874' = "0.7g",
  '7.856' = "0.8g",
  '8.838' = "0.9g",
  '9.82' = "1g",
  '10.802' = "1.1g",
  '11.784' = "1.2g",
  '12.766' = "1.3g")

myplot3 <- ggplot(aux,aes(x,y,color=factor(VelCombination),group=paste(g,vv,vh))) + 
  geom_line() + 
  facet_grid(g~., labeller = as_labeller(factornames)) +
  labs(x = "Distance (m)", y = "Height (m)") + 
  scale_color_manual(values=colorRampPalette(c(BlauUB,Yellow))(4),name=expression("v"["v0"]*" & v"["h"]*" (m/s)")) +
  annotate("rect", xmin = -12, xmax = 0, ymin = 1.65, ymax = 1.85,
           alpha = .2) +
  coord_fixed(ratio = 1) +
  coord_cartesian(ylim = c(0,2.5)) +
  scale_y_continuous(breaks = c(0,1,2)) +
  theme(legend.position = c(0.15,0.1),legend.background = element_rect(fill="white"))

ggsave(file="parabolas.jpg", w=8,h=8)


############################################################
##### Optic Flow information (FIGURE 2)
############################################################
#### (1) Elevation Angle
## (1.1) Get dfs
##
aux1 <- NULL
gvals <- round(unique(x2$gval),3)
for (j in gvals){
  x <- getDFfor_Elev_Prime_Rel(vh=6,vv=3.7,j,y=1.75)
  x$g <- j
  aux1 <- rbind(aux1,x)}
aux1 <- aux1[aux1$tScaledto100 != 100,]

aux2 <- NULL
gvals <- round(unique(x2$gval),3)
for (j in gvals){
  x <- getDFfor_Elev_Prime_Rel(vh=6,vv=5.2,j,y=1.75)
  x$g <- j
  aux2 <- rbind(aux2,x)}
aux2 <- aux2[aux2$tScaledto100 != 100,]

aux3 <- NULL
gvals <- round(unique(x2$gval),3)
for (j in gvals){
  x <- getDFfor_Elev_Prime_Rel(vh=8.3,vv=3.7,j,y=1.75)
  x$g <- j
  aux3 <- rbind(aux3,x)}
aux3 <- aux3[aux3$tScaledto100 != 100,]

aux4 <- NULL
gvals <- round(unique(x2$gval),3)
for (j in gvals){
  x <- getDFfor_Elev_Prime_Rel(vh=8.3,vv=5.2,j,y=1.75)
  x$g <- j
  aux4 <- rbind(aux4,x)}
aux4 <- aux4[aux4$tScaledto100 != 100,]
##

## (1.2) Plot it
##
GravLegendLabels <- c("0.7g","0.8g","0.9g","1g","1.1g","1.2g","1.3g")

myplot1.0 <- ggplot(aux1,aes(tScaledto100,ElevationYIndependent,color=factor(g))) + 
  geom_line(size = 1) + 
  labs(x = "Time (s)", y = expression(paste(gamma," (rad)"))) +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank(),legend.position = "none") + 
  labs(x="") + labs(title="")
myplot1.1 <- ggplot(aux1,aes(tScaledto100,ElevationAnglePrimeYIndependent,color=factor(g))) + 
  geom_line(size = 1) + 
  labs(x = "Time (s)", y = expression(paste(dot(gamma)," (rad/s)"))) +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank(),legend.position = "none") + 
  labs(x="") + labs(title="")
myplot1 <- plot_grid(myplot1.0,myplot1.1,ncol = 2) + 
  ggtitle(expression("v"["v0"]*" = 3.7 m/s & v"["h"]*" = 6 m/s"))

myplot2.0 <- ggplot(aux2,aes(tScaledto100,ElevationYIndependent,color=factor(g))) + 
  geom_line(size = 1) + 
  labs(x = "Time (s)", y = expression(paste(gamma," (rad)"))) +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank(),legend.position = "none") + 
  labs(x="") + labs(title="")
myplot2.1 <- ggplot(aux2,aes(tScaledto100,ElevationAnglePrimeYIndependent,color=factor(g))) + 
  geom_line(size = 1) + 
  labs(x = "Time (s)", y = expression(paste(dot(gamma)," (rad/s)"))) +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank(),legend.position = "none") + 
  labs(x="") + labs(title="")
myplot2 <- plot_grid(myplot2.0,myplot2.1,ncol = 2) +
  ggtitle(expression("v"["v0"]*" = 5.2 m/s & v"["h"]*" = 6 m/s"))

myplot3.0 <- ggplot(aux3,aes(tScaledto100,ElevationYIndependent,color=factor(g))) + 
  geom_line(size = 1) + 
  labs(x = "Time (s)", y = expression(paste(gamma," (rad)"))) +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank(),legend.position = "none") + 
  labs(x="") + labs(title="")
myplot3.1 <- ggplot(aux3,aes(tScaledto100,ElevationAnglePrimeYIndependent,color=factor(g))) + 
  geom_line(size = 1) + 
  labs(x = "Time (s)", y = expression(paste(dot(gamma)," (rad/s)"))) +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank(),legend.position = "none") + 
  labs(x="") + labs(title="")
myplot3 <- plot_grid(myplot3.0,myplot3.1,ncol = 2) +
  ggtitle(expression("v"["v0"]*" = 3.7 m/s & v"["h"]*" = 8.3 m/s"))

myplot4.0 <- ggplot(aux4,aes(tScaledto100,ElevationYIndependent,color=factor(g))) + 
  geom_line(size = 1) + 
  labs(x = "Time (s)", y = expression(paste(gamma," (rad)"))) +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank(),legend.position = "none") + 
  labs(x="% of trajectory") + labs(title="")
myplot4.1 <- ggplot(aux4,aes(tScaledto100,ElevationAnglePrimeYIndependent,color=factor(g))) + 
  geom_line(size = 1) + 
  labs(x = "Time (s)", y = expression(paste(dot(gamma)," (rad/s)"))) +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank(),legend.position = "none") + 
  labs(x="% of trajectory") + labs(title="")
myplot4 <- plot_grid(myplot4.0,myplot4.1,ncol = 2) +
  ggtitle(expression("v"["v0"]*" = 5.2 m/s & v"["h"]*" = 8.3 m/s"))

myplot0.1 <- plot_grid(myplot1, myplot2, myplot3, myplot4, ncol = 1) + ggtitle("Elevation Angle information")

p1 <- ggplot(aux1) + geom_line(aes(x=tScaled,y=ElevationAnglePrimeYIndependent,color=g)) + scale_color_gradientn(name = "Gravity\n(m/s²)", colours = palette7ColorsBluetoYellow)
legend1 <- g_legend(p1)
##

## (2) Visual Angle
## (1.1) Get dfs

aux1 <- NULL
gvals <- round(unique(x2$gval),3)
for (j in gvals){
  x <- getDFfor_VisualAngle_Prime_Rel(vh=6,vv=3.7,gval=j,y=1.75,size=0.033)
  x$g <- j
  aux1 <- rbind(aux1,x)}
aux11 <- data.frame(Values = c(aux1$VisualAngleYIndependent,aux1$VisualAnglePrimeYIndependent), t = c(aux1$tScaledto100,aux1$tScaledto100), label1 = c(rep("theta",700),rep("thetadot",700)), label2 = "6/3.7", Gravity = rep(aux1$g,2))

aux2 <- NULL
gvals <- round(unique(x2$gval),3)
for (j in gvals){
  x <- getDFfor_VisualAngle_Prime_Rel(vh=6,vv=5.2,gval=j,y=1.75,size=0.033)
  x$g <- j
  aux2 <- rbind(aux2,x)}
aux2$VelProf <- "6/5.2"
aux21 <- data.frame(Values = c(aux2$VisualAngleYIndependent,aux2$VisualAnglePrimeYIndependent), t = c(aux2$tScaledto100,aux2$tScaledto100), label1 = c(rep("theta",700),rep("thetadot",700)), label2 = "6/5.2", Gravity = rep(aux2$g,2))

aux3 <- NULL
gvals <- round(unique(x2$gval),3)
for (j in gvals){
  x <- getDFfor_VisualAngle_Prime_Rel(vh=8.3,vv=3.7,gval=j,y=1.75,size=0.033)
  x$g <- j
  aux3 <- rbind(aux3,x)}
aux3$VelProf <- "8.33/3.7"
aux31 <- data.frame(Values = c(aux3$VisualAngleYIndependent,aux3$VisualAnglePrimeYIndependent), t = c(aux3$tScaledto100,aux3$tScaledto100), label1 = c(rep("theta",700),rep("thetadot",700)), label2 = "8.33/3.7", Gravity = rep(aux3$g,2))

aux4 <- NULL
gvals <- round(unique(x2$gval),3)
for (j in gvals){
  x <- getDFfor_VisualAngle_Prime_Rel(vh=8.3,vv=5.2,gval=j,y=1.75,size=0.033)
  x$g <- j
  aux4 <- rbind(aux4,x)}
aux4$VelProf <- "8.33/5.2"
aux41 <- data.frame(Values = c(aux4$VisualAngleYIndependent,aux4$VisualAnglePrimeYIndependent), t = c(aux4$tScaledto100,aux4$tScaledto100), label1 = c(rep("theta",700),rep("thetadot",700)), label2 = "8.33/5.2", Gravity = rep(aux4$g,2))

aux5 <- rbind(aux11,aux21,aux31,aux41)

# (1.2) Plot it

GravLegendLabels <- c("0.7g","0.8g","0.9g","1g","1.1g","1.2g","1.3g")





myplot1.0 <- ggplot(aux1,aes(tScaledto100,VisualAngleYIndependent,color=factor(g))) + 
  geom_line(size = 1) + 
  labs(x = "Time (s)", y = expression(paste(theta," (rad)"))) +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank(),legend.position = "none") + 
  labs(x="") + labs(title="")
myplot1.1 <- ggplot(aux1,aes(tScaledto100,VisualAnglePrimeYIndependent,color=factor(g))) + 
  geom_line(size = 1) + 
  labs(x = "Time (s)", y = expression(paste(dot(theta)," (rad/s)"))) +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank(),legend.position = "none") + 
  labs(x="") + labs(title="")
myplot1 <- plot_grid(myplot1.0,myplot1.1,ncol = 2) + 
  ggtitle(expression("v"["v0"]*" = 3.7 m/s & v"["h"]*" = 6 m/s"))

myplot2.0 <- ggplot(aux2,aes(tScaledto100,VisualAngleYIndependent,color=factor(g))) + 
  geom_line(size = 1) + 
  labs(x = "Time (s)", y = expression(paste(theta," (rad)"))) +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank(),legend.position = "none") + 
  labs(x="") + labs(title="")
myplot2.1 <- ggplot(aux2,aes(tScaledto100,VisualAnglePrimeYIndependent,color=factor(g))) + 
  geom_line(size = 1) + 
  labs(x = "Time (s)", y = expression(paste(dot(theta)," (rad/s)"))) +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank(),legend.position = "none") + 
  labs(x="") + labs(title="")
myplot2 <- plot_grid(myplot2.0,myplot2.1,ncol = 2) +
  ggtitle(expression("v"["v0"]*" = 5.2 m/s & v"["h"]*" = 6 m/s"))

myplot3.0 <- ggplot(aux3,aes(tScaledto100,VisualAngleYIndependent,color=factor(g))) + 
  geom_line(size = 1) + 
  labs(x = "Time (s)", y = expression(paste(theta," (rad)"))) +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank(),legend.position = "none") + 
  labs(x="") + labs(title="")
myplot3.1 <- ggplot(aux3,aes(tScaledto100,VisualAnglePrimeYIndependent,color=factor(g))) + 
  geom_line(size = 1) + 
  labs(x = "Time (s)", y = expression(paste(dot(theta)," (rad/s)"))) +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank(),legend.position = "none") + 
  labs(x="") + labs(title="")
myplot3 <- plot_grid(myplot3.0,myplot3.1,ncol = 2) +
  ggtitle(expression("v"["v0"]*" = 3.7 m/s & v"["h"]*" = 8.3 m/s"))

myplot4.0 <- ggplot(aux1,aes(tScaledto100,VisualAngleYIndependent,color=factor(g))) + 
  geom_line(size = 1) + 
  labs(x = "Time (s)", y = expression(paste(theta," (rad)"))) +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank(),legend.position = "none") + 
  labs(x="% of trajectory") + labs(title="")
myplot4.1 <- ggplot(aux4,aes(tScaledto100,VisualAnglePrimeYIndependent,color=factor(g))) + 
  geom_line(size = 1) + 
  labs(x = "Time (s)", y = expression(paste(dot(theta)," (rad/s)"))) +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank(),legend.position = "none") + 
  labs(x="% of trajectory") + labs(title="")
myplot4 <- plot_grid(myplot4.0,myplot4.1,ncol = 2) +
  ggtitle(expression("v"["v0"]*" = 5.2 m/s & v"["h"]*" = 8.3 m/s"))

myplot0.2 <- plot_grid(myplot1, myplot2, myplot3, myplot4, ncol = 1) + ggtitle("Visual Angle information")
ggsave("Development of Visual Angle information.jpg", w=5,h=10)

myplot <- plot_grid(myplot0.1,myplot0.2,ncol = 2)

p2 <- ggplot(aux2) + geom_line(aes(x=tScaled,y=VisualAngle,color=g)) + scale_color_gradientn(name = "Gravity\n(m/s²)", colours = palette7ColorsBluetoRed)
legend2 <- g_legend(p2)

myplot <- plot_grid(myplot0.1,myplot0.2,legend2,labels = c("A","B",""),rel_widths = c(5,5,1), ncol=3)
ggsave("Development of optic flow information.jpg", w=10,h=10)


############################################################
#####QuickPsy analysis
############################################################
#####prepare dataframe for QuickPsy
m <- x2[x2$id != "s03",] ##subject had chance level across all tested stimulus strengths
#####QuickPsy needs positive slopes
m$ElevationPrimeYIndependent0.6_Dif_b <- m$ElevationPrimeYIndependent0.6_Dif_b*(-1)
m$ElevationPrimeYIndependent0.75_Dif_b <- m$ElevationPrimeYIndependent0.75_Dif_b*(-1)
m$ttc_Dif <- m$ttc - m$ttcStd
m$ttc_Dif_b <- AddBinnedValues(m$ttc_Dif)*(-1)
m$ElevationYIndependent0.6_Dif_b <- m$ElevationYIndependent0.6_Dif_b*(-1)
m$ElevationYIndependent0.75_Dif_b <- m$ElevationYIndependent0.75_Dif_b*(-1)

##Standardizing
m$ElevPrimeByCos_Dif_b_Stdd <- m$ElevPrimeByCos_Dif_b/abs(range(m$ElevPrimeByCos_Dif_b)[2]-range(m$ElevPrimeByCos_Dif_b)[1])
m$ElevationPrimeYIndependent0.75_Dif_b_Stdd <- m$ElevationPrimeYIndependent0.75_Dif_b/
  abs(range(m$ElevationPrimeYIndependent0.75_Dif_b)[2]-range(m$ElevationPrimeYIndependent0.75_Dif_b)[1])
m$VisualAngleYI0.75_Dif_b_Stdd <- m$VisualAngleYI0.75_Dif_b/
  abs(range(m$VisualAngleYI0.75_Dif_b)[2]-range(m$VisualAngleYI0.75_Dif_b)[1])
m$ElevPrimeByTheta_Dif_b_Stdd <- m$ElevPrimeByTheta_Dif_b/
  abs(range(m$ElevPrimeByTheta_Dif_b)[2]-range(m$ElevPrimeByTheta_Dif_b)[1])
m$ElevPrimeByThetaTimesCos_Dif_b_Stdd <- m$ElevPrimeByThetaTimesCos_Dif_b/
  abs(range(m$ElevPrimeByThetaTimesCos_Dif_b)[2]-range(m$ElevPrimeByThetaTimesCos_Dif_b)[1])
m$gEstimate_75_Dif_b_Stdd <- m$gEstimate_75_Dif_b/
  abs(range(m$gEstimate_75_Dif_b)[2]-range(m$gEstimate_75_Dif_b)[1])
m$gval_Dif <- m$gval-9.81
m$gval_Dif_Stdd <- m$gval_Dif/
  abs(range(m$gval_Dif)[1]-range(m$gval_Dif)[2])
m$ttc_Dif_b_Stdd <- m$ttc_Dif_b/
  abs(range(m$ttc_Dif_b)[1]-range(m$ttc_Dif_b)[2])
m$velV_Dif <- m$velV - m$velVStd
m$velH_Dif <- m$velH - m$velHStd

####gravity as decision variable
##across all participants
fit1.0 <- quickpsy(m, gval, GLarge)
##separately for participants
fit1 <- quickpsy(m, gval, GLarge, grouping=.(id))
##divided by differences in vertical velocities (comparing parameters of psychometric function, correcting for multiple comparisons)
fit1.7 <- quickpsy(m, gval, GLarge, grouping=.(velV_Dif),ci=1-0.05/3)

##divided by differences in horizontal velocities (comparing parameters of psychometric function, correcting for multiple comparisons)
fit1.8 <- quickpsy(m, gval, GLarge, grouping=.(velH_Dif),ci=1-0.05/3) 



############################################################
##### Gravity by Velocites (FIGURE 3)
############################################################
m$velH_Dif <- as.character(m$velH_Dif)
m$velV_Dif <- as.character(m$velV_Dif)
fit1.7 <- quickpsy(m, gval, GLarge, grouping=.(velV_Dif),ci=1-0.05/3)
fit1.8 <- quickpsy(m, gval, GLarge, grouping=.(velH_Dif),ci=1-0.05/3)

Colors1 <- c(Red,"#ff8604","#ffd70d")
Colors2 <- c(BlauUB,"#19ffc9","#b812ff")

vVelocities <- plot(fit1.7) +
  labs(x = "Gravity (m/s²)", y = "Probability Larger than 1g") + 
  scale_x_continuous(breaks=c(8,10,12)) + 
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1)) +
  coord_cartesian(ylim = c(0,1)) +
  scale_color_manual(values=Colors1,name=expression("Difference in v"["v0"]*" (m/s)")) + 
  theme(legend.position = c(0.3, 0.85))

hVelocities <- plot(fit1.8) +
  labs(x = "Gravity (m/s²)", y = "Probability Larger than 1g") + 
  scale_x_continuous(breaks=c(8,10,12)) + 
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1)) +
  coord_cartesian(ylim = c(0,1)) +
  scale_color_manual(values=Colors2,name=expression("Difference in v"["h"]*" (m/s)")) + 
  theme(legend.position = c(0.3, 0.85))

a <- c("-1.5","0","1.5")
Par1VelV <- fit1.7$par
Par1VelV <- Par1VelV[Par1VelV$parn == "p1",]
Par1VelV <- round(Par1VelV$par,2)
Par2VelV <- fit1.7$par
Par2VelV <- Par2VelV[Par2VelV$parn == "p2",]
Par2VelV <- round(Par2VelV$par,2)
ParsVelV <- data.frame(velV_Dif=a, PSE=Par1VelV, SD = Par2VelV)

BarplotPar1VelV <- ggplot(ParsVelV, aes(x=velV_Dif, y=Par1VelV)) + 
  geom_bar(stat="identity",fill = Colors1) + 
  ylab("PSE (m/s²)") +
  xlab(expression("Difference in v"["v0"]*" (m/s)")) + 
  coord_cartesian(ylim = c(0,16)) +
  geom_text(aes(label=Par1VelV), vjust=-0.15, color="black") +
  geom_segment(aes(x = 1, y = 12.5, xend = 2, yend = 12.5), size = 1) +
  geom_segment(aes(x = 2, y = 14, xend = 3, yend = 14), size = 1) +
  geom_segment(aes(x = 1, y = 15.5, xend = 3, yend = 15.5), size = 1) +
  annotate("text",x = 1.5,y = 13, label = "*") +
  annotate("text",x = 2.5,y = 14.5, label = "*") +
  annotate("text",x = 2,y = 16, label = "*")

BarplotPar2VelV <- ggplot(ParsVelV, aes(x=velV_Dif, y=Par2VelV)) + 
  geom_bar(stat="identity",fill = Colors1) + 
  ylab("SD (m/s²)") +
  xlab(expression("Difference in v"["v0"]*" (m/s)")) + 
  coord_cartesian(ylim = c(0,5.2)) +
  geom_text(aes(label=Par2VelV), vjust=-0.15, color="black") +
  geom_segment(aes(x = 1, y = 4.4, xend = 2, yend = 4.4), size = 1) +
  geom_segment(aes(x = 2, y = 5, xend = 3, yend = 5), size = 1) +
  annotate("text",x = 1.5,y = 4.6, label = "*") +
  annotate("text",x = 2.5,y = 5.2, label = "*")

b <- c("-2.33","0","2.33")
Par1velH <- fit1.8$par
Par1velH <- Par1velH[Par1velH$parn == "p1",]
Par1velH <- round(Par1velH$par,2)
Par2velH <- fit1.8$par
Par2velH <- Par2velH[Par2velH$parn == "p2",]
Par2velH <- round(Par2velH$par,2)
ParsvelH <- data.frame(velH_Dif=b, PSE=Par1velH, SD = Par2velH)

BarplotPar1VelH <- ggplot(ParsvelH, aes(x=velH_Dif, y=Par1velH)) + 
  geom_bar(stat="identity",fill = Colors2) + 
  ylab("PSE (m/s²)") +
  xlab(expression("Difference in v"["h"]*" (m/s)")) + 
  #  coord_cartesian(ylim = c(0,16),xlim = c(0,3)) +
  geom_text(aes(label=Par1velH), vjust=-0.15, color="black") +
  geom_segment(aes(x = 1, y = 14.25, xend = 3, yend = 14.25), size = 1) +
  geom_segment(aes(x = 2, y = 12.25, xend = 3, yend = 12.25), size = 1) +
  annotate("text",x = 2,y = 14.75, label = "*") +
  annotate("text",x = 2.5,y = 12.75, label = "*")

BarplotPar2VelH <- ggplot(ParsvelH, aes(x=velH_Dif, y=Par2velH)) + 
  geom_bar(stat="identity",fill = Colors2) + 
  ylab("SD (m/s²)") +
  xlab(expression("Difference in v"["h"]*" (m/s)")) + 
  #  coord_cartesian(ylim = c(0,5),xlim = c(0,3)) +
  geom_text(aes(label=Par2velH), vjust=-0.15, color="black") +
  geom_segment(aes(x = 2, y = 4.6, xend = 3, yend = 4.6), size = 1) +
  annotate("text",x = 2.5,y = 4.9, label = "*")

BarplotsvelH <- plot_grid(BarplotPar1VelH,BarplotPar2VelH,labels = c("E","F"))
velHDifferences <- plot_grid(hVelocities,BarplotsvelH,ncol=1,rel_heights = c(4,1.5),labels = c("D","",""))
BarplotsvelV <- plot_grid(BarplotPar1VelV,BarplotPar2VelV,labels = c("B","C"))
velVDifferences <- plot_grid(vVelocities,BarplotsvelV,ncol=1,rel_heights = c(4,1.5),labels = c("A","",""))

VelocitiesDifferences <- plot_grid(velVDifferences,velHDifferences,rel_widths = c(1,1))

ggsave("GravityByVelocities.jpg",w=10,h=10)
#####



##Optic flow cues
##60% or 75% of the trajectory as point of interest? Absolute value of the angle or rate of change?
fit2.0.1 <- quickpsy(m, ElevationYIndependent0.6_Dif_b, GLarge)
fit2.0.1$aic
fit2.0.2 <- quickpsy(m, ElevationYIndependent0.75_Dif_b, GLarge)
fit2.0.2$aic

fit2.1.1 <- quickpsy(m, ElevationPrimeYIndependent0.6_Dif_b, GLarge)
fit2.1.1$aic
fit2.1.2 <- quickpsy(m, ElevationPrimeYIndependent0.75_Dif_b, GLarge)
fit2.1.2$aic

fit3.0.1 <- quickpsy(m, VisualAngleYI0.6_Dif_b, GLarge)
fit3.0.1$aic
fit3.0.2 <- quickpsy(m, VisualAngleYI0.75_Dif_b, GLarge)
fit3.0.2$aic
fit3.0.3 <- quickpsy(m, VisualAngleYI0.88_Dif_b, GLarge)
fit3.0.3$aic

fit3.1.1 <- quickpsy(m, VisualAnglePrimeYI0.6_Dif_b, GLarge)
fit3.1.1$aic
fit3.1.2 <- quickpsy(m, VisualAnglePrimeYI0.75_Dif_b, GLarge)
fit3.1.2$aic
fit3.1.3 <- quickpsy(m, VisualAnglePrimeYI0.88_Dif_b, GLarge)
fit3.1.3$aic
##Temporal derivative of Elevation Angle and Visual Angle at 75%


############################################################
##### QuickPsy 1 (FIGURE 4)
############################################################
fit1 <- quickpsy(m, gval_Dif_Stdd, GLarge, grouping=.(id))
fit2.1.4 <- quickpsy(m, ElevationPrimeYIndependent0.75_Dif_b_Stdd, GLarge, grouping=.(id))
fit3 <- quickpsy(m, VisualAngleYI0.75_Dif_b_Stdd, GLarge, grouping=.(id))

factornamesPsych <- c(
  's01' = "Subject 1",
  's02' = "Subject 2",
  's04' = "Subject 4",
  's05' = "Subject 5",
  's06' = "Subject 6",
  's07' = "Subject 7",
  's08' = "Subject 8",
  's09' = "Subject 9",
  's11' = "Subject 11")

PsychGrav3 <- plot(fit1) + facet_wrap(~id,  labeller = as_labeller(factornamesPsych)) +
  #coord_cartesian(x=c(-2,13),y=c(0,1)) +
  theme(legend.title=element_blank(),legend.position = "none") +
  #  theme(legend.title = element_blank(),legend.position = "none",axis.text=element_text(size=8),axis.title = element_text(size=10)) + 
  labs(x = "Gravity (m/s²)",y = "Probability Larger than 1g") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(title="Gravity") +
  scale_color_manual(values = rep(BlauUB,9)) + 
  scale_x_continuous(breaks=c(-0.3,0,0.3)) + 
  scale_y_continuous(breaks=c(0,0.5,1))


##(2) Elevation Angle
PsychElev <- plot(fit2.1.4) + facet_wrap(~id,  labeller = as_labeller(factornamesPsych)) +
  theme(legend.title=element_blank(),legend.position = "none") +
  labs(x = expression(paste(dot(gamma), " (rad/s)")),y = "Probability Larger than 1g") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(title="Rate of Change of Elevation Angle") +
  scale_color_manual(values = rep(Yellow,9)) + 
  scale_x_continuous(breaks=c(-0.1,0.2,0.5)) + 
  scale_y_continuous(breaks=c(0,0.5,1))

PsychVisu <- plot(fit3) + facet_wrap(~id,  labeller = as_labeller(factornamesPsych)) +
  theme(legend.title=element_blank(),legend.position = "none") +
  labs(x = expression(paste(theta, " (rad)")),y = "Probability Larger than 1g") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(title="Visual Angle") +
  scale_color_manual(values = rep(Red,9)) + 
  scale_x_continuous(breaks=c(-0.4,-0.1,0.2)) + 
  scale_y_continuous(breaks=c(0,0.5,1))


##Get SDs of psychometric functions
a <- fit1$par
a <- a[a$parn == "p2",]
a <- a$par
slopeGrav <- round(a,2)

b <- fit2.1.4$par
b <- b[b$parn == "p2",]
b <- b$par
slopeElev <- round(b,2)

c <- fit3$par
c <- c[c$parn == "p2",]
c <- c$par
slopeVisu <- round(c,2)

subjects <- c("01","02","04","05","06","07","08","09","11")

colorvalues <- rep(BlauUB,9)
colorvalues2 <- rep(Yellow,9)
colorValues3 <- rep(Red,9)

df <- data.frame(slopeGrav,slopeElev,slopeVisu,subjects)

slopesGrav <- ggplot(data=df, aes(x=subjects, y=slopeGrav)) + 
  geom_bar(stat="identity",fill = colorvalues) +
  ylab("SD (m/s²)") + 
  xlab("Subject") + 
  coord_cartesian(ylim = c(0,1.5)) +
  geom_text(aes(label=slopeGrav), vjust=-0.3, color="black", size=3.5) 

slopesElev <- ggplot(data=df, aes(x=subjects, y=slopeElev)) + 
  geom_bar(stat="identity",fill = colorvalues2) + 
  ylab("SD (rad/s)") + 
  xlab("Subject") + 
  coord_cartesian(ylim = c(0,1.5)) + 
  geom_text(aes(label=slopeElev), vjust=-0.3, color="black", size=3.5)

slopesVisu <- ggplot(data=df, aes(x=subjects, y=slopeVisu)) + 
  geom_bar(stat="identity",fill = colorValues3) + 
  ylab("SD (rad)") + 
  xlab("Subject") + 
  coord_cartesian(ylim = c(0,1.5)) + 
  geom_text(aes(label=slopeVisu), vjust=-0.3, color="black", size=3.5)

## Putting everything together
PsychGravDone <- plot_grid(PsychGrav3,slopesGrav,ncol = 1,rel_heights = c(6,1.5),labels=c("A","B"))
PsychElevDone <- plot_grid(PsychElev,slopesElev,ncol = 1,rel_heights = c(6,1.5),labels=c("C","D"))
PsychVisuDone <- plot_grid(PsychVisu,slopesVisu,ncol = 1,rel_heights = c(6,1.5),labels=c("E","F"))
plot_grid(PsychGravDone,PsychElevDone,PsychVisuDone,ncol=3)
ggsave("QuickPsyComplete.jpg",w=13,h=9.5)
#####



######Are there differences in cue reliance between types of parabolas?
####ELEVATION ANGLE
##divided by vertical velocities
fit2.3.1 <- quickpsy(m, ElevationPrimeYIndependent0.75_Dif_b, GLarge, grouping=.(velV))
fit2.3.1$aic

##divided by horizontal velocities
fit2.3.2 <- quickpsy(m, ElevationPrimeYIndependent0.75_Dif_b, GLarge, grouping=.(velH))
fit2.3.2$aic

##divided by vertical velocities and subjects
fit2.4.1 <- quickpsy(m, ElevationPrimeYIndependent0.75_Dif_b, GLarge, grouping=.(velV,id))
fit2.4.1$aic

##divided by horizontal velocities and subjects
fit2.4.2 <- quickpsy(m, ElevationPrimeYIndependent0.75_Dif_b, GLarge, grouping=.(velH,id))
fit2.4.2$aic

AIC_Elev_by_vv <- fit2.4.1$aic
AIC_Elev_by_vh <- fit2.4.2$aic

##per-subject differences in modelfit for vertical velocities
round(AIC_Elev_by_vv$aic[AIC_Elev_by_vv$velV==3.7]-AIC_Elev_by_vv$aic[AIC_Elev_by_vv$velV==5.2],1)

##per-subject differences in modelfit for horizontal velocities
round(AIC_Elev_by_vh$aic[AIC_Elev_by_vh$velH==8.33]-AIC_Elev_by_vh$aic[AIC_Elev_by_vh$velH==6.00],1)


####VISUAL ANGLE
##divided by vertical velocities
fit3.3.1 <- quickpsy(m, VisualAngleYI0.75_Dif_b, GLarge, grouping=.(velV)) 
# interpretation MISSING
round(fit3.3.1$aic$aic[1]-fit3.3.1$aic$aic[2],1)

##divided by horizontal velocities
fit3.3.2 <- quickpsy(m, VisualAngleYI0.75_Dif_b, GLarge, grouping=.(velH))
# interpretation MISSING
round(fit3.3.2$aic$aic[1]-fit3.3.2$aic$aic[2],1)

##divided by vertical velocities and subjects
fit3.4.1 <- quickpsy(m, VisualAngleYI0.75_Dif_b, GLarge, grouping=.(velV,id))
# interpretation MISSING
round(fit3.4.1$aic$aic[1:9]-fit3.4.1$aic$aic[10:18],1)

##divided by horizontal velocities and subjects
fit3.4.2 <- quickpsy(m, VisualAngleYI0.75_Dif_b, GLarge, grouping=.(velH,id))
# interpretation MISSING
round(fit3.4.2$aic$aic[1:9]-fit3.4.2$aic$aic[10:18],1)


AIC_Visual_by_vv <- fit3.4.1$aic
AIC_Visual_by_vh <- fit3.4.2$aic
#per-subject differences in modelfit
AIC_Visual_by_vv$aic[AIC_Visual_by_vv$velV==3.7]-AIC_Visual_by_vv$aic[AIC_Visual_by_vv$velV==5.2]
#per-subject differences in modelfit
-AIC_Visual_by_vh$aic[AIC_Visual_by_vh$velH==6]+AIC_Visual_by_vh$aic[AIC_Visual_by_vh$velH==8.33]


####Test overall flight time versus other decision variables
fit4 <- quickpsy(m, ttc_b, GLarge)
fit2.1 <- quickpsy(m, ElevationPrimeYIndependent0.75_Dif_b, GLarge)
fit3.1 <- quickpsy(m, VisualAngleYI0.75_Dif_b, GLarge)


##Overall flight time has worst AIC
round(fit2.1$aic$aic,1)
round(fit3.1$aic$aic,1)
round(fit4$aic$aic,1)

##lower model fits are not just a matter of lower correlations:
cor(m$ttc_b,m$GLarge)
cor(m$ElevationPrimeYIndependent0.75_Dif_b,m$GLarge)
cor(m$VisualAngleYI0.75_Dif_b,m$GLarge)


############################################################
##### Actual TTC versus 1g estimate (FIGURE 5)
############################################################

gval <- seq(0.7,1.3,0.1)*9.81
velV <- c(3.7,5.2)
velH <- c(6,8.3)
size <- 0.033
y <- 1.75

a <- c()
b <- c()
c <- c()
d <- c()
e <- c()
f <- c()
for (i in 1:2){
  vv <- velV[i]
  for (j in 1:2){
    vh <- velH[j]
    for (k in 1:7){
      g <- gval[k]
      t <- seq(-1,0,0.01)*getTimetoYIsZero(vv,g)
      gEstim <- gEstimate(t,g,ElevationAnglePrimeYIndependent(t,g,vv,vh,y),ElevationAngleYIndependent(t,g,vv,vh,y),getVisualAngle(t,size,vh,vv,g),size)
      a <- c(a,t)
      b <- c(b,gEstim)
      c <- c(c,rep(vv,101))
      d <- c(d,rep(vh,101))
      e <- c(e,rep(g,101))
      f <- seq(0,100,1)
    }
  }
}


df <- data.frame(t = a,gEstimate = b,velV = c, velH = d, g = e, percentage = f )

GravLegendLabels <- c("0.7g","0.8g","0.9g","1g","1.1g","1.2g","1.3g")

df5 <- df[df$percentage != 100,]
df1 <- df5[df5$velV == velV[1] & df5$velH == velH[1],]
df2 <- df5[df5$velV == velV[1] & df5$velH == velH[2],]
df3 <- df5[df5$velV == velV[2] & df5$velH == velH[1],]
df4 <- df5[df5$velV == velV[2] & df5$velH == velH[2],]

myplot1.0 <- ggplot(df1,aes(percentage,gEstimate,color=factor(g))) + 
  geom_line(size = 1) +
  labs(x = "Percentage of trajectory", y = "Estimated Gravity (m/s²)") +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank(),legend.position = "none") + 
  labs(title=expression("v"["v0"]*" = 3.7 m/s & v"["h"]*" = 6 m/s")) +
  coord_cartesian(y = c(0,50))
myplot1.1 <- ggplot(df2,aes(percentage,gEstimate,color=factor(g))) + 
  geom_line(size = 1) +
  labs(x = "Percentage of trajectory", y = "Estimated Gravity (m/s²)") +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank(),legend.position = "none") + 
  labs(title=expression("v"["v0"]*" = 3.7 m/s & v"["h"]*" = 8.3 m/s")) +
  coord_cartesian(y = c(0,50))

myplot1.2 <- ggplot(df3,aes(percentage,gEstimate,color=factor(g))) + 
  geom_line(size = 1) +
  labs(x = "Percentage of trajectory", y = "Estimated Gravity (m/s²)") +
  scale_color_manual(name="Gravity", labels=GravLegendLabels, values=palette7ColorsBluetoRed) + 
  theme(legend.title=element_blank()) + 
  labs(title=expression("v"["v0"]*" = 5.2 m/s & v"["h"]*" = 6 m/s")) +
  coord_cartesian(y = c(0,40)) + 
  labs(title="GS Gravity Predictions")
ggsave("GravityEstimates.jpg",width = 5, height = 5)
#####


#######fit for different approximations based on GS model
####Gravity
# overall
fit1.0 <- quickpsy(m, gval, GLarge)
round(fit1.0$aic$aic,1) #model fit
round(fit1.0$par[fit1.0$par$parn=="p2",]$par,2) #SD

#per subject
fit1 <- quickpsy(m, gval_Stdd, GLarge,grouping=.(id)) 
round(fit1$aic$aic,1) #model fit
round(fit1$par[fit1$par$parn=="p2",]$par,2) #SD

##Rate of Change of Elevation Angle alone
# overall
fit2.1.2 <- quickpsy(m, ElevationPrimeYIndependent0.75_Dif_b_Stdd, GLarge)
round(fit2.1.2$aic$aic,1) #model fit
round(fit2.1.2$par[fit2.1.2$par$parn=="p2",]$par,2) #SD

# per subject
fit2.1.3 <- quickpsy(m, ElevationPrimeYIndependent0.75_Dif_b_Stdd, GLarge,grouping = .(id))
round(fit2.1.3$aic$aic,1) #model fit
round(fit2.1.3$par[fit2.1.3$par$parn=="p2",]$par,2) #SD


####Y*/O 
#overall
fit6.1.0 <- quickpsy(m, ElevPrimeByTheta_Dif_b_Stdd, GLarge) 
round(fit6.1.0$aic,1) #model fit
round(fit6.1.0$par[fit6.1.0$par$parn=="p2",]$par,2) #SD

#per subject
fit6.1.1 <- quickpsy(m, ElevPrimeByTheta_Dif_b_Stdd, GLarge,grouping=.(id))
round(fit6.1.1$aic$aic,1) #model fit
round(fit6.1.1$par[fit6.1.1$par$parn=="p2",]$par,2) #SD

####full GS estimate
# overall
fit6.2.0 <- quickpsy(m, gEstimate_75_Dif_b_Stdd, GLarge)
round(fit6.2.0$aic$aic,1) #model fit
round(fit6.2.0$par[fit6.2.0$par$parn=="p2",]$par,2) #SD

# per subject
fit6.2.1 <- quickpsy(m, gEstimate_75_Dif_b_Stdd, GLarge, grouping=.(id))
round(fit6.2.1$aic$aic) #model fit
round(fit6.2.1$par[fit6.1.1$par$parn=="p2",]$par,2) #SD


####AIC overview per subject
AICs_YO <- fit6.1.1$aic #AICs for Y*/O
AICs_GSestimate <- fit6.2.1$aic #AICs for full GS estimate
AICs_Gravity <- fit1$aic #AICs for Gravity
AICs_YO$aic-AICs_Gravity$aic #AICs for Y*/O in comparison to Gravity
AICs_GSestimate$aic-AICs_Gravity$aic #AICs for GS estimate in comparison to Gravity



#########MixedPsy
####info about installation: https://mixedpsychophysics.wordpress.com/
setwd("C:/Users/Bjorn/Desktop/WD_R/MERpsychophysics.1")
source("MERpsychophysics.r")
setwd("C:/Users/Bjorn/Desktop/WD_R")
#########GLMM stuff for elevation angle and visual angle: To determine heterogeneity of used strategy
x2 <- x2[x2$id != "s03",]
x <- c()
y <- c()
z <- c()
a <- c()
b <- c()
c <- c()
d <- c()
e <- c()
f <- c()
g <- c()
h <- c()
q <- c()
for (i in 1:length(unique(x2$id))){
  i2 <- sort(unique(x2$id))
  i3 <- i2[i]
  for (j in 1:length(unique(x2$gval))){
    j2 <- sort(unique(x2$gval))
    j3 <- j2[j]
    for (k in 1:length(unique(x2$velV))){
      k2 <- sort(unique(x2$velV))
      k3 <- k2[k]
      for (l in 1:length(unique(x2$velH))){
        l2 <- sort(unique(x2$velH))
        l3 <- l2[l]
        x <- c(x,sum(x2$GLarge[x2$id == i3 & x2$gval == j3 & x2$velV == k3 & x2$velH == l3]))
        y <- c(y,i3)
        z <- c(z,k3)
        a <- c(a,j3)
        b <- c(b,l3)
        c <- c(c,(x2$ElevationPrimeYIndependent0.75_Dif_b[x2$id == i3 & x2$gval == j3 & x2$velV == k3 & x2$velH == l3])[1])
        d <- c(d,(x2$VisualAngleYI0.75_Dif_b[x2$id == i3 & x2$gval == j3 & x2$velV == k3 & x2$velH == l3])[1])
        e <- c(e,(x2$ttc[x2$id == i3 & x2$gval == j3 & x2$velV == k3 & x2$velH == l3])[1])
        f <- c(f,(x2$ElevationPrimeYIndependentDifAv_b[x2$id == i3 & x2$gval == j3 & x2$velV == k3 & x2$velH == l3])[1])
        g <- c(g,(x2$VisualAnglePrimeDifAv_b[x2$id == i3 & x2$gval == j3 & x2$velV == k3 & x2$velH == l3])[1])
        h <- c(h,(x2$ElevationPrimeYIndependent0.75_Dif[x2$id == i3 & x2$gval == j3 & x2$velV == k3 & x2$velH == l3])[1])
        q <- c(q,(x2$VisualAngleYI0.75_Dif[x2$id == i3 & x2$gval == j3 & x2$velV == k3 & x2$velH == l3])[1])
      }  
    }
  }
}

datafr3 <- data.frame(GLarge = x, Subject = y, velV = z, X = a, velH = b, Elev = c, Visu = d, ttc = e, Total = 20, Elev75 = h, Visu75 = q)
datafr3$Visu <- scale(datafr3$Visu, center = TRUE, scale = TRUE)
datafr3$Visu75 <- scale(datafr3$Visu75, center = TRUE, scale = TRUE)
datafr3$Elev <- scale(datafr3$Elev, center = TRUE, scale = TRUE)
datafr3$Elev75 <- scale(datafr3$Elev75, center = TRUE, scale = TRUE)
datafr3$ttc <- scale(datafr3$ttc, center = TRUE, scale = TRUE)


##Elev and Visu and interaction, only fixed effects, but with different groups
formula.mod1 <- cbind(GLarge, Total - GLarge) ~ Elev75 + Visu75 + (1 + Elev75 + Visu75 | Subject)
mod1 = glmer(formula.mod1,
             family = binomial(link = "probit"), 
             data = datafr3,
             control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(mod1)

formula.mod3 <- cbind(GLarge, Total - GLarge) ~ Elev75 + Visu75 + (1 | Subject)
mod3 = glmer(formula.mod3,
             family = binomial(link = "probit"), data = datafr3)
summary(mod3)

anova(mod1,mod3) ##Model 1 beats model 3



########################################################################################################
####################################################Experiment 2
########################################################################################################

####################################################
######load data
####################################################

a1 <- read.table(".../1hsummary1.log", header = TRUE)
a2 <- read.table(".../1hsummary2.log", header = TRUE)
a3 <- read.table(".../1hsummary3.log", header = TRUE)
a4 <- read.table(".../1hsummary4.log", header = TRUE)
a5 <- read.table(".../1lsummary1.log", header = TRUE)
a6 <- read.table(".../1lsummary2.log", header = TRUE)
a7 <- read.table(".../1lsummary3.log", header = TRUE)
a8 <- read.table(".../1lsummary4.log", header = TRUE)
b1 <- read.table(".../2hsummary1.log", header = TRUE)
b2 <- read.table(".../2hsummary2.log", header = TRUE)
b3 <- read.table(".../2hsummary3.log", header = TRUE)
b4 <- read.table(".../2hsummary4.log", header = TRUE)
b5 <- read.table(".../2lsummary1.log", header = TRUE)
b6 <- read.table(".../2lsummary2.log", header = TRUE)
b7 <- read.table(".../2lsummary3.log", header = TRUE)
b8 <- read.table(".../2lsummary4.log", header = TRUE)
c1 <- read.table(".../3hsummary1.log", header = TRUE)
c2 <- read.table(".../3hsummary2.log", header = TRUE)
c3 <- read.table(".../3hsummary3.log", header = TRUE)
c4 <- read.table(".../3hsummary4.log", header = TRUE)
c5 <- read.table(".../3lsummary1.log", header = TRUE)
c6 <- read.table(".../3lsummary2.log", header = TRUE)
c7 <- read.table(".../3lsummary3.log", header = TRUE)
c8 <- read.table(".../3lsummary4.log", header = TRUE)
d1 <- read.table(".../4hsummary1.log", header = TRUE)
d2 <- read.table(".../4hsummary2.log", header = TRUE)
d3 <- read.table(".../4hsummary3.log", header = TRUE)
d4 <- read.table(".../4hsummary4.log", header = TRUE)
d5 <- read.table(".../4lsummary1.log", header = TRUE)
d6 <- read.table(".../4lsummary2.log", header = TRUE)
d7 <- read.table(".../4lsummary3.log", header = TRUE)
d8 <- read.table(".../4lsummary4.log", header = TRUE)
e1 <- read.table(".../5hsummary1.log", header = TRUE)
e2 <- read.table(".../5hsummary2.log", header = TRUE)
e3 <- read.table(".../5hsummary3.log", header = TRUE)
e4 <- read.table(".../5hsummary4.log", header = TRUE)
e5 <- read.table(".../5lsummary1.log", header = TRUE)
e6 <- read.table(".../5lsummary2.log", header = TRUE)
e7 <- read.table(".../5lsummary3.log", header = TRUE)
e8 <- read.table(".../5lsummary4.log", header = TRUE)
f1 <- read.table(".../6hsummary1.log", header = TRUE)
f2 <- read.table(".../6hsummary2.log", header = TRUE)
f3 <- read.table(".../6hsummary3.log", header = TRUE)
f4 <- read.table(".../6hsummary4.log", header = TRUE)
f5 <- read.table(".../6lsummary1.log", header = TRUE)
f6 <- read.table(".../6lsummary2.log", header = TRUE)
f7 <- read.table(".../6lsummary3.log", header = TRUE)
f8 <- read.table(".../6lsummary4.log", header = TRUE)
g1 <- read.table(".../7hsummary1.log", header = TRUE)
g2 <- read.table(".../7hsummary2.log", header = TRUE)
g3 <- read.table(".../7hsummary3.log", header = TRUE)
g4 <- read.table(".../7hsummary4.log", header = TRUE)
g5 <- read.table(".../7lsummary1.log", header = TRUE)
g6 <- read.table(".../7lsummary2.log", header = TRUE)
g7 <- read.table(".../7lsummary3.log", header = TRUE)
g8 <- read.table(".../7lsummary4.log", header = TRUE)
h1 <- read.table(".../8hsummary1.log", header = TRUE)
h2 <- read.table(".../8hsummary2.log", header = TRUE)
h3 <- read.table(".../8hsummary3.log", header = TRUE)
h4 <- read.table(".../8hsummary4.log", header = TRUE)
h5 <- read.table(".../8lsummary1.log", header = TRUE)
h6 <- read.table(".../8lsummary2.log", header = TRUE)
h7 <- read.table(".../8lsummary3.log", header = TRUE)
h8 <- read.table(".../8lsummary4.log", header = TRUE)
i1 <- read.table(".../9hsummary1.log", header = TRUE)
i2 <- read.table(".../9hsummary2.log", header = TRUE)
i3 <- read.table(".../9hsummary3.log", header = TRUE)
i4 <- read.table(".../9hsummary4.log", header = TRUE)
i5 <- read.table(".../9lsummary1.log", header = TRUE)
i6 <- read.table(".../9lsummary2.log", header = TRUE)
i7 <- read.table(".../9lsummary3.log", header = TRUE)
i8 <- read.table(".../9lsummary4.log", header = TRUE)
j1 <- read.table(".../10hsummary1.log", header = TRUE)
j2 <- read.table(".../10hsummary2.log", header = TRUE)
j3 <- read.table(".../10hsummary3.log", header = TRUE)
j4 <- read.table(".../10hsummary4.log", header = TRUE)
j5 <- read.table(".../10lsummary1.log", header = TRUE)
j6 <- read.table(".../10lsummary2.log", header = TRUE)
j7 <- read.table(".../10lsummary3.log", header = TRUE)
j8 <- read.table(".../10lsummary4.log", header = TRUE)
k1 <- read.table(".../11hsummary1.log", header = TRUE)
k2 <- read.table(".../11hsummary2.log", header = TRUE)
k3 <- read.table(".../11hsummary3.log", header = TRUE)
k4 <- read.table(".../11hsummary4.log", header = TRUE)
k5 <- read.table(".../11lsummary1.log", header = TRUE)
k6 <- read.table(".../11lsummary2.log", header = TRUE)
k7 <- read.table(".../11lsummary3.log", header = TRUE)
k8 <- read.table(".../11lsummary4.log", header = TRUE)
l1 <- read.table(".../12hsummary1.log", header = TRUE)
l2 <- read.table(".../12hsummary2.log", header = TRUE)
l3 <- read.table(".../12hsummary3.log", header = TRUE)
l4 <- read.table(".../12hsummary4.log", header = TRUE)
l5 <- read.table(".../12lsummary1.log", header = TRUE)
l6 <- read.table(".../12lsummary2.log", header = TRUE)
l7 <- read.table(".../12lsummary3.log", header = TRUE)
l8 <- read.table(".../12lsummary4.log", header = TRUE)

####################################################
######add basic info
####################################################

a1$id <- "s01"
a2$id <- "s01"
a3$id <- "s01"
a4$id <- "s01"
a5$id <- "s01"
a6$id <- "s01"
a7$id <- "s01"
a8$id <- "s01"
b1$id <- "s02"
b2$id <- "s02"
b3$id <- "s02"
b4$id <- "s02"
b5$id <- "s02"
b6$id <- "s02"
b7$id <- "s02"
b8$id <- "s02"
c1$id <- "s03"
c2$id <- "s03"
c3$id <- "s03"
c4$id <- "s03"
c5$id <- "s03"
c6$id <- "s03"
c7$id <- "s03"
c8$id <- "s03"
d1$id <- "s04"
d2$id <- "s04"
d3$id <- "s04"
d4$id <- "s04"
d5$id <- "s04"
d6$id <- "s04"
d7$id <- "s04"
d8$id <- "s04"
e1$id <- "s05"
e2$id <- "s05"
e3$id <- "s05"
e4$id <- "s05"
e5$id <- "s05"
e6$id <- "s05"
e7$id <- "s05"
e8$id <- "s05"
f1$id <- "s06"
f2$id <- "s06"
f3$id <- "s06"
f4$id <- "s06"
f5$id <- "s06"
f6$id <- "s06"
f7$id <- "s06"
f8$id <- "s06"
g1$id <- "s07"
g2$id <- "s07"
g3$id <- "s07"
g4$id <- "s07"
g5$id <- "s07"
g6$id <- "s07"
g7$id <- "s07"
g8$id <- "s07"
h1$id <- "s08"
h2$id <- "s08"
h3$id <- "s08"
h4$id <- "s08"
h5$id <- "s08"
h6$id <- "s08"
h7$id <- "s08"
h8$id <- "s08"
i1$id <- "s09"
i2$id <- "s09"
i3$id <- "s09"
i4$id <- "s09"
i5$id <- "s09"
i6$id <- "s09"
i7$id <- "s09"
i8$id <- "s09"
j1$id <- "s10"
j2$id <- "s10"
j3$id <- "s10"
j4$id <- "s10"
j5$id <- "s10"
j6$id <- "s10"
j7$id <- "s10"
j8$id <- "s10"
k1$id <- "s11"
k2$id <- "s11"
k3$id <- "s11"
k4$id <- "s11"
k5$id <- "s11"
k6$id <- "s11"
k7$id <- "s11"
k8$id <- "s11"
l1$id <- "s12"
l2$id <- "s12"
l3$id <- "s12"
l4$id <- "s12"
l5$id <- "s12"
l6$id <- "s12"
l7$id <- "s12"
l8$id <- "s12"


a2$trial <- seq(140,279,1)
b2$trial <- seq(140,279,1)
c2$trial <- seq(140,279,1)
d2$trial <- seq(140,279,1)
e2$trial <- seq(140,279,1)
f2$trial <- seq(140,279,1)
g2$trial <- seq(140,279,1)
h2$trial <- seq(140,279,1)
i2$trial <- seq(140,279,1)
j2$trial <- seq(140,279,1)
k2$trial <- seq(140,279,1)
l2$trial <- seq(140,279,1)
a6$trial <- seq(140,279,1)
b6$trial <- seq(140,279,1)
c6$trial <- seq(140,279,1)
d6$trial <- seq(140,279,1)
e6$trial <- seq(140,279,1)
f6$trial <- seq(140,279,1)
g6$trial <- seq(140,279,1)
h6$trial <- seq(140,279,1)
i6$trial <- seq(140,279,1)
j6$trial <- seq(140,279,1)
k6$trial <- seq(140,279,1)
l6$trial <- seq(140,279,1)

a3$trial <- seq(280,419,1)
b3$trial <- seq(280,419,1)
c3$trial <- seq(280,419,1)
d3$trial <- seq(280,419,1)
e3$trial <- seq(280,419,1)
f3$trial <- seq(280,419,1)
g3$trial <- seq(280,419,1)
h3$trial <- seq(280,419,1)
i3$trial <- seq(280,419,1)
j3$trial <- seq(280,419,1)
k3$trial <- seq(280,419,1)
l3$trial <- seq(280,419,1)
a7$trial <- seq(280,419,1)
b7$trial <- seq(280,419,1)
c7$trial <- seq(280,419,1)
d7$trial <- seq(280,419,1)
e7$trial <- seq(280,419,1)
f7$trial <- seq(280,419,1)
g7$trial <- seq(280,419,1)
h7$trial <- seq(280,419,1)
i7$trial <- seq(280,419,1)
j7$trial <- seq(280,419,1)
k7$trial <- seq(280,419,1)
l7$trial <- seq(280,419,1)

a4$trial <- seq(420,559,1)
b4$trial <- seq(420,559,1)
c4$trial <- seq(420,559,1)
d4$trial <- seq(420,559,1)
e4$trial <- seq(420,559,1)
f4$trial <- seq(420,559,1)
g4$trial <- seq(420,559,1)
h4$trial <- seq(420,559,1)
i4$trial <- seq(420,559,1)
j4$trial <- seq(420,559,1)
k4$trial <- seq(420,559,1)
l4$trial <- seq(420,559,1)
a8$trial <- seq(420,559,1)
b8$trial <- seq(420,559,1)
c8$trial <- seq(420,559,1)
d8$trial <- seq(420,559,1)
e8$trial <- seq(420,559,1)
f8$trial <- seq(420,559,1)
g8$trial <- seq(420,559,1)
h8$trial <- seq(420,559,1)
i8$trial <- seq(420,559,1)
j8$trial <- seq(420,559,1)
k8$trial <- seq(420,559,1)
k8$trial <- seq(420,559,1)

a1$sizevar <- "high"
a2$sizevar <- "high"
a3$sizevar <- "high"
a4$sizevar <- "high"
a5$sizevar <- "low"
a6$sizevar <- "low"
a7$sizevar <- "low"
a8$sizevar <- "low"
b1$sizevar <- "high"
b2$sizevar <- "high"
b3$sizevar <- "high"
b4$sizevar <- "high"
b5$sizevar <- "low"
b6$sizevar <- "low"
b7$sizevar <- "low"
b8$sizevar <- "low"
c1$sizevar <- "high"
c2$sizevar <- "high"
c3$sizevar <- "high"
c4$sizevar <- "high"
c5$sizevar <- "low"
c6$sizevar <- "low"
c7$sizevar <- "low"
c8$sizevar <- "low"
d1$sizevar <- "high"
d2$sizevar <- "high"
d3$sizevar <- "high"
d4$sizevar <- "high"
d5$sizevar <- "low"
d6$sizevar <- "low"
d7$sizevar <- "low"
d8$sizevar <- "low"
e1$sizevar <- "high"
e2$sizevar <- "high"
e3$sizevar <- "high"
e4$sizevar <- "high"
e5$sizevar <- "low"
e6$sizevar <- "low"
e7$sizevar <- "low"
e8$sizevar <- "low"
f1$sizevar <- "high"
f2$sizevar <- "high"
f3$sizevar <- "high"
f4$sizevar <- "high"
f5$sizevar <- "low"
f6$sizevar <- "low"
f7$sizevar <- "low"
f8$sizevar <- "low"
g1$sizevar <- "high"
g2$sizevar <- "high"
g3$sizevar <- "high"
g4$sizevar <- "high"
g5$sizevar <- "low"
g6$sizevar <- "low"
g7$sizevar <- "low"
g8$sizevar <- "low"
h1$sizevar <- "high"
h2$sizevar <- "high"
h3$sizevar <- "high"
h4$sizevar <- "high"
h5$sizevar <- "low"
h6$sizevar <- "low"
h7$sizevar <- "low"
h8$sizevar <- "low"
i1$sizevar <- "high"
i2$sizevar <- "high"
i3$sizevar <- "high"
i4$sizevar <- "high"
i5$sizevar <- "low"
i6$sizevar <- "low"
i7$sizevar <- "low"
i8$sizevar <- "low"
j1$sizevar <- "high"
j2$sizevar <- "high"
j3$sizevar <- "high"
j4$sizevar <- "high"
j5$sizevar <- "low"
j6$sizevar <- "low"
j7$sizevar <- "low"
j8$sizevar <- "low"
k1$sizevar <- "high"
k2$sizevar <- "high"
k3$sizevar <- "high"
k4$sizevar <- "high"
k5$sizevar <- "low"
k6$sizevar <- "low"
k7$sizevar <- "low"
k8$sizevar <- "low"
l1$sizevar <- "high"
l2$sizevar <- "high"
l3$sizevar <- "high"
l4$sizevar <- "high"
l5$sizevar <- "low"
l6$sizevar <- "low"
l7$sizevar <- "low"
l8$sizevar <- "low"

a1$tall <- 1.85
a2$tall <- 1.85
a3$tall <- 1.85
a4$tall <- 1.85
a5$tall <- 1.85 
a6$tall <- 1.85
a7$tall <- 1.85
a8$tall <- 1.85
b1$tall <- 1.67
b2$tall <- 1.67
b3$tall <- 1.67
b4$tall <- 1.67
b5$tall <- 1.67 
b6$tall <- 1.67
b7$tall <- 1.67
b8$tall <- 1.67
c1$tall <- 1.72
c2$tall <- 1.72
c3$tall <- 1.72
c4$tall <- 1.72
c5$tall <- 1.72 
c6$tall <- 1.72
c7$tall <- 1.72
c8$tall <- 1.72
d1$tall <- 1.68
d2$tall <- 1.68
d3$tall <- 1.68
d4$tall <- 1.68
d5$tall <- 1.68 
d6$tall <- 1.68
d7$tall <- 1.68
d8$tall <- 1.68
e1$tall <- 1.87
e2$tall <- 1.87
e3$tall <- 1.87
e4$tall <- 1.87
e5$tall <- 1.87 
e6$tall <- 1.87
e7$tall <- 1.87
e8$tall <- 1.87
f1$tall <- 1.78
f2$tall <- 1.78
f3$tall <- 1.78
f4$tall <- 1.78
f5$tall <- 1.78 
f6$tall <- 1.78
f7$tall <- 1.78
f8$tall <- 1.78
g1$tall <- 1.70
g2$tall <- 1.70
g3$tall <- 1.70
g4$tall <- 1.70
g5$tall <- 1.70 
g6$tall <- 1.70
g7$tall <- 1.70
g8$tall <- 1.70
h1$tall <- 1.62
h2$tall <- 1.62
h3$tall <- 1.62
h4$tall <- 1.62
h5$tall <- 1.62 
h6$tall <- 1.62
h7$tall <- 1.62
h8$tall <- 1.62
i1$tall <- 1.71
i2$tall <- 1.71
i3$tall <- 1.71
i4$tall <- 1.71
i5$tall <- 1.71 
i6$tall <- 1.71
i7$tall <- 1.71
i8$tall <- 1.71
j1$tall <- 1.56
j2$tall <- 1.56
j3$tall <- 1.56
j4$tall <- 1.56
j5$tall <- 1.56 
j6$tall <- 1.56
j7$tall <- 1.56
j8$tall <- 1.56
k1$tall <- 1.80
k2$tall <- 1.80
k3$tall <- 1.80
k4$tall <- 1.80
k5$tall <- 1.80 
k6$tall <- 1.80
k7$tall <- 1.80
k8$tall <- 1.80
l1$tall <- 1.78
l2$tall <- 1.78
l3$tall <- 1.78
l4$tall <- 1.78
l5$tall <- 1.78 
l6$tall <- 1.78
l7$tall <- 1.78
l8$tall <- 1.78

x3 <- rbind(a1,a2,a3,a4,a5,a6,a7,a8,b1,b2,b3,b4,b5,b6,b7,b8,c1,c2,c3,c4,c5,c6,c7,c8,d1,d2,d3,d4,d5,d6,d7,d8,e1,e2,e3,e4,e5,e6,e7,e8,f1,f2,f3,f4,f5,f6,f7,f8,g1,g2,g3,g4,g5,g6,g7,g8,h1,h2,h3,h4,h5,h6,h7,h8,i1,i2,i3,i4,i5,i6,i7,i8,j1,j2,j3,j4,j5,j6,j7,j8,k1,k2,k3,k4,k5,k6,k7,k8,l1,l2,l3,l4,l5,l6,l7,l8)



x3$WhichCatFirst[x3$id == "s01"] <- "High"
x3$WhichCatFirst[x3$id == "s02"] <- "Low"
x3$WhichCatFirst[x3$id == "s03"] <- "High"
x3$WhichCatFirst[x3$id == "s04"] <- "Low"
x3$WhichCatFirst[x3$id == "s05"] <- "High"
x3$WhichCatFirst[x3$id == "s06"] <- "Low"
x3$WhichCatFirst[x3$id == "s07"] <- "High"
x3$WhichCatFirst[x3$id == "s08"] <- "Low"
x3$WhichCatFirst[x3$id == "s09"] <- "High"
x3$WhichCatFirst[x3$id == "s10"] <- "Low"
x3$WhichCatFirst[x3$id == "s11"] <- "High"
x3$WhichCatFirst[x3$id == "s12"] <- "Low"


####################################################
######Calculate needed values
####################################################

#"stimulus strength"/difference between gravities
x3$gdif <- abs(x3$gval-9.82)

#add normalized column for psychometric "is larger than" function
x3$GLarge <- x3$Response
x3$GLarge[x3$stdFirst==0] <- 1-x3$Response[x3$stdFirst==0]

#add correct
for(i in 1:length(x3$gval)){
  if (x3$gval[i]>9.820 & x3$stdFirst[i] == 0 & x3$Response[i] == 1){
    x3$correct[i] <- 0}
  else if (x3$gval[i]>9.820 & x3$stdFirst[i] == 1 & x3$Response[i] == 0){
    x3$correct[i] <- 0}
  else if (x3$gval[i]<9.820 & x3$stdFirst[i] == 0 & x3$Response[i] == 0){
    x3$correct[i] <- 0}
  else if (x3$gval[i]<9.820 & x3$stdFirst[i] == 1 & x3$Response[i] == 1){
    x3$correct[i] <- 0}
  else if (x3$gval[i] == 9.820){
    x3$correct[i] <- sample(0:1,1)}
  else {
    x3$correct[i] <- 1}
}

######Size Categories
x3$SizeCategory[x3$radius <= 0.033] <- "small"
x3$SizeCategory[x3$radius > 0.033] <- "big"

######Elevation Angle
x3$ElevationPrimeYIndependent0.75_Dif <- ElevationAnglePrimeYIndependent(x3$ttc*0.75,x3$gval,x3$velV,x3$velH,x3$tall) -
  ElevationAnglePrimeYIndependent(x3$ttcStd*0.75,9.81,x3$velVStd,x3$velHStd,x3$tall)
x3$ElevationPrimeYIndependent0.75_Dif_b <- AddBinnedValues(x3$ElevationPrimeYIndependent0.75_Dif)

#####Visual Angle
x3$VisualAngleYI0.75_Dif <- getVisualAngleYIndependent(x3$ttc*0.75,x3$radius,x3$velH,x3$velV,x3$gval,y=x3$tall) -
  getVisualAngleYIndependent(x3$ttcStd*0.75,x3$radius,x3$velHStd,x3$velVStd,9.81,y=x3$tall)
x3$VisualAngleYI0.75_Dif_b <- AddBinnedValues(x3$VisualAngleYI0.75_Dif)


####################################################
######QuickPsy
####################################################
n <- x3[x3$id!="s05" & x3$id!="s08",] ##both subjects have chance level across the whole stimulus range
n$ElevationPrimeYIndependent0.75_Dif_b <- n$ElevationPrimeYIndependent0.75_Dif_b*(-1) #QuickPsy can only deal with positive slopes


fit4 <- quickpsy(n, gval, GLarge, grouping=.(sizevar))
fit4$parcomparisons

fit5 <- quickpsy(n, gval, GLarge, grouping=.(SizeCategory))
fit5$parcomparisons


fit6 <- quickpsy(n, gval, GLarge, grouping=.(sizevar,id,ci=0.995))
a <- fit6$parcomparisons
a[a$id == a$id2,]

fit7 <- quickpsy(n, gval, GLarge, grouping=.(SizeCategory,id),ci=0.995)
b <- fit7$parcomparisons
b[b$id == b$id2,]

##model fits for gamma dot for size variability
fit9 <- quickpsy(n, ElevationPrimeYIndependent0.75_Dif_b, GLarge, grouping=.(sizevar))
fit9$aic$aic[fit9$aic$sizevar == "high"]-fit9$aic$aic[fit9$aic$sizevar == "low"]

##model fits for theta for size variability
fit10 <- quickpsy(n, VisualAngleYI0.75_Dif_b, GLarge, grouping=.(sizevar))
fit10$aic$aic[fit10$aic$sizevar == "high"]-fit10$aic$aic[fit10$aic$sizevar == "low"]

##model fits for gamma dot for big versus small
fit13 <- quickpsy(n, ElevationPrimeYIndependent0.75_Dif_b, GLarge, grouping=.(SizeCategory))
fit13$aic$aic[fit13$aic$SizeCategory == "big"]-fit13$aic$aic[fit13$aic$SizeCategory == "small"]

##model fits for theta for big versus small
fit14 <- quickpsy(n, VisualAngleYI0.75_Dif_b, GLarge, grouping=.(SizeCategory))
fit14$aic$aic[fit14$aic$SizeCategory == "big"]-fit14$aic$aic[fit14$aic$SizeCategory == "small"]


############################################################
##### Psychopy Experiment 2 (FIGURE 6)
############################################################

n <- x3[x3$id!="s05" & x3$id!="s08",]
fit1 <- quickpsy(n, gval, GLarge, grouping=.(sizevar,id), ci=0.995)
fit2 <- quickpsy(n, gval, GLarge, grouping=.(SizeCategory,id), ci=0.995)
fit3 <- quickpsy(n, gval, GLarge, grouping=.(SizeCategory2,id))

fit4 <- quickpsy(n, gval, GLarge, grouping=.(sizevar))
fit5 <- quickpsy(n, gval, GLarge, grouping=.(SizeCategory))
fit6 <- quickpsy(n, gval, GLarge, grouping=.(SizeCategory2))

factornamesPsych <- c(
  's01' = "Subject 1",
  's02' = "Subject 2",
  's03' = "Subject 3",
  's04' = "Subject 4",
  's06' = "Subject 6",
  's07' = "Subject 7",
  's09' = "Subject 9",
  's10' = "Subject 10",
  's11' = "Subject 11",
  's12' = "Subject 12")


a <- plot(fit1) + 
  labs(x = "Gravity (m/s²)", y = "Probability Larger than 1g") + 
  theme(legend.position="top") + 
  labs(title="LV vs. HV") +
  facet_wrap(~id,scales = "free_x", labeller = as_labeller(factornamesPsych)) +
  scale_color_manual(values=c(Yellow, LightYellow),name=expression("")) +
  scale_x_continuous(breaks=c(8,10,12)) + 
  scale_y_continuous(breaks=c(0,0.5,1)) +
  coord_cartesian(xlim = c(0.7*9.81,1.3*9.81),ylim = c(0,1))

b <- plot(fit2)  + 
  labs(x = "Gravity (m/s²)", y = "Probability Larger than 1g") +
  theme(legend.position="top") + 
  labs(title="Big vs. small") +
  facet_wrap(~id,scales = "free_x", labeller = as_labeller(factornamesPsych)) +
  scale_color_manual(values=c(Red, LightRed),name=expression("")) +
  scale_x_continuous(breaks=c(8,10,12)) + 
  scale_y_continuous(breaks=c(0,0.5,1)) +
  coord_cartesian(xlim = c(0.7*9.81,1.3*9.81),ylim = c(0,1))

part1 <- plot_grid(a,b,ncol = 2,labels = "AUTO")

d <- c("01","02","03","04","06","07","09","10","11","12","mean")

a4 <- fit4$parcomparisons
a <- fit1$parcomparisons
a1 <- a$dif[a$parn == "p2" & a$sizevar != a$sizevar2 & a$id == a$id2]
a3 <- data.frame(a1 = c(a1,a4$dif[2]),d,significancea = c(" "," "," "," "," "," "," "," ","*"," ","*"))

b4 <- fit5$parcomparisons
b <- fit2$parcomparisons
b1 <- c(b$dif[b$parn == "p1" & b$SizeCategory != b$SizeCategory2 & b$id == b$id2])
b3 <- data.frame(b1 = c(b1,b4$dif[1]),d,significanceb = c("*","*","*","*","*","*","*"," ","*","*","*"))

slopesa <- ggplot(a3, aes(x=d, y=a1)) + 
  geom_bar(stat="identity",fill = BlauUB) + 
  ylab("SD dif. (m/s²)") + 
  xlab("Subject") +
  coord_cartesian(ylim = c(-0.5,4.5)) +
  geom_text(aes(label=significancea), vjust=-0.1, color="black", size=5)

slopesb <- ggplot(b3, aes(x=d, y=b1)) + 
  geom_bar(stat="identity",fill = Red) + 
  ylab("Mean dif. (m/s²)") + 
  xlab("Subject") + 
  coord_cartesian(ylim = c(0,5.5)) +
  geom_text(aes(label=significanceb), vjust=-0.1, color="black", size=5)

part2 <- plot_grid(slopesa,slopesb,ncol = 2,labels = c("C","D"))
plot_grid(part1,part2,ncol = 1, rel_heights = c(3,1))

ggsave(file="experiment 2.jpg", w=10,h=10)
#####
