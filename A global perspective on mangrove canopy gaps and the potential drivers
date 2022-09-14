library(biomod2)
library(ggplot2)
library(gridExtra)
library(raster)
library(rasterVis)
library(rgdal)
library(ecospat)

C:/Users/mky/Directory/Final


setwd("C:/Users/mky/Directory/Final")
gap<-read.csv('pa.csv')
env  <- gap[, colnames(gap) %in% c("Gaps","bio1", "bio5", "bio6", "bio9","bio13","bio14","bio18","bio19","Lightning","Lat","Lon","cos.lon.","X","Y")]
View(env)

#env
current <- env[3:13]
RespName<-'Gaps'

#Presences/absences
resp.var<-as.numeric(env[,RespName])

#Lat and Lon coordinates
xy<-env[,2:3]

## -----------------------------------------------------------------------------
### Formating the data with the BIOMOD_FormatingData() function form the package biomod2

myBiomodData2 <- BIOMOD_FormatingData( resp.var = resp.var,
                                       expl.var = current,
                                       resp.xy = xy,
                                       resp.name =RespName)

BIOMOD_ModelingOptions()

## ----ESM.Modeling-------------------------------------------------------------
### Calibration of simple bivariate models
my.ESM2 <- ecospat.ESM.Modeling( data=myBiomodData2,
                                 models=c("GLM", "GBM", "RF", "GAM"),
                                 NbRunEval=10,
                                 DataSplit=50,
                                 weighting.score=c("AUC"),
                                 parallel=F)
#Number of cross-validations
my.ESM2$NbRunEval
#Number of bivariate models
my.ESM2$which.biva

#Models used
my.ESM2$models

## ----ESM.EnsembleModeling-----------------------------------------------------
### Evaluation and average of simple bivariate models to ESMs
my.ESM_EF2 <- ecospat.ESM.EnsembleModeling(my.ESM2,weighting.score=c("SomersD"),threshold=0)

## check number of failed models

my.ESM_EF2$failed

## get the Somers'D weights of the single bivariate models used to build the ESMs
fw<-my.ESM_EF2$weights

## get the evaluation scores for the individual models  for all ten runs
evscore<-my.ESM_EF2$ESM.evaluations
##save as dataframe
evscore<-as.data.frame(t(evscore))
write.csv( evscore , 'evaluationscores50.csv' )
View(evscore)
## get the variable contributions

vc<-ecospat.ESM.VarContrib(my.ESM2,my.ESM_EF2)

vc<-as.data.frame(t(vc))

View(vc)
write.csv( vc , 'varimportance50.csv' )

## Response plot
output.plot <- ecospat.ESM.responsePlot(ESM.EnsembleModeling.output = my.ESM_EF2,
                                        ESM.modeling.output = my.ESM2)
