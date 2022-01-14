library(biomod2)
library(ggplot2)
library(gridExtra)
library(raster)
library(rasterVis)
library(rgdal)
library(ecospat)




setwd("C:/Users/mky/Directory/ESMM")
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

#Defining individual model parameters
Gap_opt2 <- 
  BIOMOD_ModelingOptions(GLM = list( type = 'quadratic',interaction.level = 0),
                         CTA = list( method = 'class'),
                                     
    MAXENT.Phillips = list( path_to_maxent.jar = 'C:/Users/mky/Directory/ESMM',
                            memory_allocated = 512,
                            background_data_dir = 'default',
                            maximumbackground = 'default',
                            maximumiterations = 200,
                            visible = FALSE,
                            linear = TRUE,
                            quadratic = TRUE,
                            product = TRUE,
                            threshold = TRUE,
                            hinge = TRUE,
                            lq2lqptthreshold = 80,
                            l2lqthreshold = 10,
                            hingethreshold = 15,
                            beta_threshold = -1,
                            beta_categorical = -1,
                            beta_lqp = -1,
                            beta_hinge = -1,
                            betamultiplier = 1,
                            defaultprevalence = 0.5),
    ANN = list( NbCV = 5,
                size = NULL,
                decay = NULL,
                rang = 0.1,
                maxit = 200))

## ----ESM.Modeling-------------------------------------------------------------
### Calibration of simple bivariate models
my.ESM2 <- ecospat.ESM.Modeling( data=myBiomodData2,
                                 models=c("GLM","MAXENT.Phillips","CTA","ANN"),
                                 models.options=Gap_opt2,
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

my.ESM2$mymodels

s

## ----ESM.EnsembleModeling-----------------------------------------------------
### Evaluation and average of simple bivariate models to ESMs
my.ESM_EF2 <- ecospat.ESM.EnsembleModeling(my.ESM2,weighting.score=c("SomersD"),threshold=0)

#check number of failed models

my.ESM_EF2$failed


## get the weights of the single bivariate models used to build the ESMs
fw<-my.ESM_EF2$weights
failedweights<-as.data.frame(t(fw))

View(failedweights)
write.csv(failedweights,'failedweights50.csv')
##get the evaluation scores for the individual models  for all ten runs
evscore<-my.ESM_EF2$ESM.evaluations
##save as dataframe
evscore<-as.data.frame(t(evscore))
write.csv( evscore , 'evaluationscores50.csv' )
View(evscore)
## get the variable contributions of ESMs

vc<-ecospat.ESM.VarContrib(my.ESM2,my.ESM_EF2)

vc<-as.data.frame(t(vc))

View(vc)
write.csv( vc , 'varimportance50.csv' )

##Response plot
output.plot <- ecospat.ESM.responsePlot(ESM.EnsembleModeling.output = my.ESM_EF2,
                                        ESM.modeling.output = my.ESM2)

