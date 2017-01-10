# DESCRIPTION: Code for modeling H. clypeatus/dossenus abundance 
# for Desert Grassland and southwest Desert Scrub only
# PROJECT: Erin Brandt temperate-dependent signalling 
# DATA OWNER: Patrick Kelley
# CODE AUTHOR: Patrick Kelley (University of New Hampshire)
# CODE DATA: 04-August-2015
# R VERSION: R 3.0.2 (2013-09-25), 32-bit

# NOTE ON INSTALLATION OF glmmADMB
# Need to install from source
#install.packages("glmmADMB", repos="http://r-forge.r-project.org", type="source")

# LOAD LIBRARIES FROM LOCAL SOURCE FILE ("./source")
source("C:/Users/Patrick/Dropbox/spider_project_2012/habronattus_survey_project_2012/source/load_packages.r")

# GET CANDIDATE MODELS
model.set <- read.csv("C:/Users/Patrick/Dropbox/spider_project_2012/habronattus_survey_project_2012/data/abundance_model_set.csv", na.strings="")
colnames(model.set) <- gsub("\\.", "", colnames(model.set))
colnames(model.set) <- gsub("temp2", "I(temp^2)", colnames(model.set))

# build models
split.mod <- split(model.set, model.set$model_id)
mod.form.list <- list()
for (i in seq_along(split.mod)) {
  m <- split.mod[[i]]
  if(length(which(m[1, 2:ncol(m)]==1))==0) {mod.form <- "1"
  } else {
  m[1, which(m[1,]==1)] <- colnames(m)[which(m[1,]==1)]
  m <- m[, which(!is.na(m))]
  mod.form <- paste0(m[, 2:ncol(m)], collapse="+")
  }
  mod.form.list[[i]] <- mod.form
  }
  
# SET WORKING DIRECTORY
# need to change to your individual DropBox or local directory
setwd("C:/Users/Patrick/Dropbox/spider_project_2012/habronattus_survey_project_2012/data/processed_data/")

# GET TEMPERATURE DATA
master <- read.csv("degr_swds_master_temperature_data.csv")
master$sitebias <- as.factor(paste(substr(master$site, start=1, stop=1), master$habitat, sep="_"))


#=========================
# Process survey data for abundance
#=========================
# READ DATA
surveys <- read.csv("C:/Users/Patrick/Dropbox/spider_project_2012/habronattus_survey_project_2012/data/final_data.csv")

# Make observer bias a function of the transect (Madera/Hopkins) and sun/shade
surveys$sitebias <- substr(surveys$location, start=1, stop=6)
surveys$plottype <- substr(surveys$location, start=1, stop=9)
surveys$location <- gsub("h_brwo", "h_degr", surveys$location)
surveys[which(surveys$sitebias=="h_brwo"), ]$habitat <- "degr"
surveys$sitebias <- substr(surveys$location, start=1, stop=6)
surveys$plottype <- paste(surveys$sitebias, surveys$cover, sep="_")
surveys$site <- paste(surveys$habitat, surveys$cover, sep="_")
surveys$date.val <- strptime(surveys$date, format="%m/%d/%Y")
surveys$month <- substr(tolower(month.name[surveys$date.val$mon+1]), start=1, stop=3)
x <- substr(surveys$sitebias, start=1, stop=1)
if (any(x=="h")) {x[which(x=="h")] <- "hopkins"}
if (any(x=="m")) {x[which(x=="m")] <- "madera"}
surveys$site <- x

# NEXT SECTION NOT NECESSARY
# test distance from datalogger and point
newlog <- master[, c("logger_month", "easting", "northing", "site", "habitat", "month")]
newlog$month <- substr(newlog$month, start=1, stop=3)
newlog$logger.name <- paste(newlog$site, newlog$habitat, newlog$month, sep="_")

newlog <- newlog[-which(duplicated(newlog)), ]
newlog$month <- ldply(strsplit(as.character(newlog$logger_month), "_"))$V3
newlog$month <- substr(newlog$month, start=1, stop=3)

split.surv <- split(surveys, surveys$surv.id)
surv.out <- list()
for (i in seq_along(split.surv)) {
  surv <- split.surv[[i]]
  surv.name <- paste(surv$site, surv$habitat, surv$month, sep="_")
  surv.xy <- cbind(surv$long.mid, surv$lat.mid)
  surv.date <- strptime(surv$date, format="%m/%d/%Y")
  surv$total.synctime <- surv.date$yday + surv$time.mid/24
  surv$time <- surv$time.mid*60
  
  # match survey to temp sample
  if (!surv.name %in% newlog$logger.name) {next()}
  
  newlog2 <- newlog[which(newlog$logger.name==surv.name), ]
  newlog.xy <- cbind(newlog2$easting, newlog2$northing)
  
  newlog2$distance <- as.numeric(distm(x=surv.xy, y=newlog.xy, fun=distHaversine))# closest two points
  newlog2 <- newlog2[order(newlog2$distance), ]
  loggers <- newlog2$logger_month[1:2] # two closest loggers
  
  # select loggers
  newmaster <- master[which(master$logger_month==loggers), ]
   
  # model temperature data
  mod <- gam(value ~ s(synctime, bs="cc"), family=Gamma(link="log"), data=master) 
  newdat <- data.frame(synctime=surv$time)
  predval <- predict(mod, newdata=newdat, type="response")
  surv$temp <- predval
  surv$date.val <- NULL
  surv.out[[i]] <- surv
  }


#=========================
# Model temperature
#=========================
# FORMAT FOR UNMARKED
survdat <- ldply(surv.out)
survdat <- survdat[which(survdat$habitat=="degr" | survdat$habitat=="swds"), ]
split.plot <- split(survdat, survdat$plottype)

counts <- list()
habitat <- list()
cover <- list()
observer <- list()
time <- list()
slope <- list()
speed <- list()
duration <- list()
temperature <- list()
month <- list()

for (i in seq_along(split.plot)) {
  temp <- split.plot[[i]]
  
  # response var
  field.name <- "counts"
  variable <- temp$num.clyp
  temp.val <- data.frame(t(variable))
  colnames(temp.val) <- paste(field.name, 1:length(variable), sep="")  
  counts[[i]] <- temp.val
  
  field.name <- "hab"
  variable <- as.character(temp$habitat)
  temp.val <- data.frame(t(variable))
  colnames(temp.val) <- paste(field.name, 1:length(variable), sep="")  
  habitat[[i]] <- temp.val

  field.name <- "cover"
  variable <- as.character(temp$cover)
  temp.val <- data.frame(t(variable))
  colnames(temp.val) <- paste(field.name, 1:length(variable), sep="")  
  cover[[i]] <- temp.val
  
  # detection variables
  field.name <- "obs"
  variable <- as.character(temp$observer)
  temp.val <- data.frame(t(variable))
  colnames(temp.val) <- paste(field.name, 1:length(variable), sep="")  
  observer[[i]] <- temp.val
  
  field.name <- "time"
  variable <- temp$time
  temp.val <- data.frame(t(variable))
  colnames(temp.val) <- paste(field.name, 1:length(variable), sep="")  
  time[[i]] <- temp.val
  
  field.name <- "slope"
  variable <- temp$slopedegs.mid
  temp.val <- data.frame(t(variable))
  colnames(temp.val) <- paste(field.name, 1:length(variable), sep="")  
  slope[[i]] <- temp.val
  
  field.name <- "speed"
  variable <- temp$speed.mpm.mid
  temp.val <- data.frame(t(variable))
  colnames(temp.val) <- paste(field.name, 1:length(variable), sep="")  
  speed[[i]] <- temp.val
  
  field.name <- "duration"
  variable <- temp$duration.min
  temp.val <- data.frame(t(variable))
  colnames(temp.val) <- paste(field.name, 1:length(variable), sep="")  
  duration[[i]] <- temp.val
  
  field.name <- "temp"
  variable <- temp$temp
  temp.val <- data.frame(t(variable))
  colnames(temp.val) <- paste(field.name, 1:length(variable), sep="")  
  temperature[[i]] <- temp.val
  
  field.name <- "month"
  variable <- temp$month
  temp.val <- data.frame(t(variable))
  colnames(temp.val) <- paste(field.name, 1:length(variable), sep="")  
  month[[i]] <- temp.val
  }

  
counts <- ldply(counts)
habitat <- ldply(habitat)
cover <- ldply(cover)
observer <- ldply(observer)
time <- ldply(time)
slope <- ldply(slope)
speed <- ldply(speed)
duration <- ldply(duration)
temperature <- ldply(temperature)
month <- ldply(month)

  
# TRANSFORM INTO UMF
columns <- 9
counts2 <- counts[, 1:columns]
slope2 <- slope[, 1:columns]
speed2 <- speed[, 1:columns]
duration2 <- duration[, 1:columns]
observer2 <- observer[, 1:columns]
habitat2 <- habitat[, 1:columns]
cover2 <- cover[, 1:columns]
time2 <- time[, 1:columns]
temperature2 <- temperature[, 1:columns]
month2 <- month[, 1:columns]

#=====================================
# Create UNMARKED data frame
# Note: If only modeling detectability using this modeling
# framework--and since this approach is giving trouble
# with the model selection, why not simply use GAMs?
# SOLUTION: Skip "create unmarked frame" below and go to
# "MODEL" section.
#=====================================
# create unmarked frame
spidersUMF <-	unmarkedFrameOccu(
		y = data.frame(counts2),
		siteCovs = data.frame(habitat=habitat2[,1]),
		obsCovs = list(slope=slope2, duration=duration2, 
    observer=observer2, time=time2,
    speed=speed2, temp=temperature2, month=month2)
  )

# MODEL 	
# need to do model selection here
#mod <- occu(~ slope + duration + observer + speed + temp + I(temp^2) + observer:temp ~ 1, spidersUMF)

# MODEL LIST
mod <- list()
for (i in seq_along(mod.form.list)) {
  mform <- mod.form.list[[i]]
  mform <- as.formula(paste("\\~ ", mform, "\\~ 1", sep=""))
  mod[[i]] <- occu(mform, spidersUMF)
  }

# PREDICT
newdat <- data.frame(expand.grid(
  slope=mean(survdat$slopedegs.mid),
  duration= mean(survdat$duration),
  speed= mean(survdat$speed.mpm.mid),
  temp= seq(range(survdat$temp)[1], range(survdat$temp)[2], 0.05),
  observer=c("jpk", "cr", "doe", "mg")
  ))
pred <- predict(mod[[4]], newdata=newdat, type="det", appendDate=T)
newdat$pred <- pred$Predicted
newdat$se <- pred$SE
newdat$lower.ci <- pred$lower
newdat$upper.ci <- pred$upper
newdat$lower.se <- newdat$pred - (newdat$pred - newdat$lower)/1.96
newdat$upper.se <- newdat$pred + (newdat$upper - newdat$pred)/1.96


mg <- newdat[which(newdat$observer=="mg"), ]
plot(mg$temp, mg$pred, ylim=c(0,1), type="l", lwd=1)
par(new=T)
plot(mg$temp, mg$lower.se, ylim=c(0,1), type="l", lty=2, lwd=1)
par(new=T)
plot(mg$temp, mg$upper.se, ylim=c(0,1), type="l", lty=2, lwd=1)


# SAVE DATA CONDITIONED ON "MG" (PERSON WITH HIGHEST DETECTION)
file.dir <- "C:/Users/Patrick/Dropbox/spider_project_2012/habronattus_survey_project_2012/output/"
write.csv(newdat, paste(file.dir, "final_model_predictions.csv", sep=""), row.names=F)




