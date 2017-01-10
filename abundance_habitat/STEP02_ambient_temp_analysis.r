# DESCRIPTION: Code for processing temperature logger data
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

# SET WORKING DIRECTORY
# need to change to your individual DropBox or local directory
setwd("C:/Users/Patrick/Dropbox/spider_project_2012/habronattus_survey_project_2012/data/processed_data/")

# GET TEMPERATURE DATA
temp <- read.csv("all_temperature_data.csv")
temp$day.factor <- as.factor(temp$day.factor) # refactor 
temp$classtype <- paste(temp$month, temp$site, temp$habitat, temp$morning_light_class, temp$microhabitat, sep="_")
sort(unique(temp$classtype))

# CENSOR TO DEGR AND SWDS ONLY
# first, recode hopkins-brwo to hopkins-degr
temp[which(temp$site=="hopkins" & temp$habitat=="brwo"), ]$habitat <- "degr"
temp2 <- temp[which(temp$habitat=="swds" | temp$habitat=="degr"), ]
temp2$habitat <- factor(temp2$habitat) # refactor

# IMPORT AND PROCESS LOGGER INFORMATION
logger.info <- read.csv("C:/Users/Patrick/Dropbox/spider_project_2012/habronattus_survey_project_2012/data/raw_data/datalogger_climate/datalogger_information_2012.csv")
colnames(logger.info)[which(colnames(logger.info)=="X2012_period")] <- "month"
logger.info$logger_month <- paste(logger.info$logger_id, logger.info$month, sep="_")
start.time <- (paste(logger.info$deploy_date, logger.info$start_time, sep=" ") %>% strptime(format="%d-%b-%y %H:%M"))
end.time <- (paste(logger.info$end_date, logger.info$end_time, sep=" ") %>% strptime(format="%d-%b-%y %H:%M"))
logger.info$start.yday <- start.time$yday
logger.info$end.yday <- end.time$yday
logger.info$start.hour <- start.time$hour
logger.info$end.hour <- end.time$hour
logger.info$start.min <- start.time$min
logger.info$end.min <- end.time$min
logger.info$start.synctime <- logger.info$start.yday + logger.info$start.hour/24 + logger.info$start.min/60/24
logger.info$end.synctime <- logger.info$end.yday + logger.info$end.hour/24 + logger.info$end.min/60/24
logger.info <- logger.info[which(!is.na(logger.info$start.yday)),] # arbitrary column that will have NAs

# CENSOR ACCORDING TO START/STOP TIMES
data.split <- split(temp2, temp2$logger_month)
out <- list()
for (i in seq_along(data.split)) {
  tempdat <- data.split[[i]]
  
  # get logger information
  logdat <- logger.info[which(logger.info$logger_month==tempdat$logger_month[1]), ]
  tempdat <- tempdat[which(tempdat$total.synctime > logdat$start.synctime), ]
  tempdat <- tempdat[which(tempdat$total.synctime <= logdat$end.synctime), ]
  out[[i]] <- tempdat # output results
  }
master <- ldply(out)
master$groupvar <- as.factor(paste(master$month, master$habitat, sep="_"))
master$classtype <- paste(master$month, master$site, master$habitat, master$morning_light_class, master$microhabitat, sep="_")
write.csv(master, "C:/Users/Patrick/Dropbox/spider_project_2012/habronattus_survey_project_2012/data/processed_data/degr_swds_master_temperature_data.csv", row.names=F)
    
  
# MODEL USING GENERALIZED ADDITIVE MODELS
# yday as random effect, habitat an

mod <- gamm(value ~ month + s(synctime, bs="cc", by=month), random=list(yday=~1), data=master)

# PREDICTION FOR PLOTTING BY MONTH AND HABITAT
preddat <- data.frame(expand.grid(synctime=seq(1,1439,1), groupvar=unique(master$groupvar)))
fit <- predict(mod$gam, newdata=preddat, type="link",se.fit=T)$fit
se <- predict(mod$gam, newdata=preddat,type="link",se.fit=T)$se.fit
preddat$predicted_value <- as.numeric(exp(fit))
preddat$upper_95 <- as.numeric(exp(fit + 1.96*se))
preddat$lower_95 <- as.numeric(exp(fit - 1.96*se))

# PREDICTION FOR PLOTTING BY MONTH
preddat.month <- data.frame(expand.grid(synctime=seq(1,1440,1), month=unique(master$month)))
fit <- predict(mod.month$gam, newdata=preddat.month, type="link",se.fit=T)$fit
se <- predict(mod.month$gam, newdata=preddat.month,type="link",se.fit=T)$se.fit
preddat.month$predicted_value <- as.numeric(exp(fit))
preddat.month$upper_95 <- as.numeric(exp(fit + 1.96*se))
preddat.month$lower_95 <- as.numeric(exp(fit - 1.96*se))

# PREDICTION FOR PLOTTING TEMPERATURE (MONTH AND HABITAT AS RANDOM EFFECTS)
preddat.month <- data.frame(expand.grid(synctime=seq(1,1440,1), month=unique(master$month)))
fit <- predict(mod2$gam, newdata=preddat.month, type="link",se.fit=T)$fit
se <- predict(mod2$gam, newdata=preddat.month,type="link",se.fit=T)$se.fit
preddat.month$predicted_value <- as.numeric(exp(fit))
preddat.month$upper_95 <- as.numeric(exp(fit + 1.96*se))
preddat.month$lower_95 <- as.numeric(exp(fit - 1.96*se))

# save data
write.csv(preddat.month, "C:/Users/Patrick/Dropbox/spider_project_2012/habronattus_survey_project_2012/output/predicted_temps_by_month.csv", row.names=F)
  

#=========================
# PLOTTING
#=========================
preddat.month <- read.csv("C:/Users/Patrick/Dropbox/spider_project_2012/habronattus_survey_project_2012/output/predicted_temps_by_month.csv")


# divide data into April and June
apr.dat <- preddat.month[which(preddat.month$month=="april"), ]
jun.dat <- preddat.month[which(preddat.month$month=="june"), ]

# make cutoff (sunrise and sunset times)
apr.times <- c(5+54/60, 18+54/60) #sunrise=0554h, sunset=1854h
jun.times <- c(5+17/60, 19+33/60) #sunrise=0517h, sunset=1933h

# create plotting window
par(mfrow=c(1,1))

plot(apr.dat$synctime/60, apr.dat$predicted_value, type="l", lwd=2, col="white", xlim=c(0,24), ylim=c(0,60), axes=FALSE, ann=F)
polygon(
   x=c(apr.dat$synctime/60, rev(apr.dat$synctime/60)), 
   y=c(apr.dat$lower_95, rev(apr.dat$upper_95)), lwd=1, lty=2, xlim=c(0,24), ylim=c(0,60), , col="grey80", border=NA)
par(new=T)
plot(apr.dat$synctime/60, apr.dat$predicted_value, type="l", lwd=2, xlim=c(0,24), ylim=c(0,60), axes=F, ann=F)
polygon(
   x=c(jun.dat$synctime/60, rev(jun.dat$synctime/60)), 
   y=c(jun.dat$lower_95, rev(jun.dat$upper_95)), lwd=1, lty=2, xlim=c(0,24), ylim=c(0,60), , col="grey80", border=NA)
par(new=T)
plot(jun.dat$synctime/60, jun.dat$predicted_value, type="l", lwd=2, xlim=c(0,24), ylim=c(0,60), axes=F, ann=F)
axis(side=1, at=seq(0, 24, 4), labels=as.character(seq(0, 24, 4)))
axis(side=2, at=seq(0, 60, 10), labels=as.character(seq(0, 60, 10)), las=2)
mtext(side=1, "hour of day", line=3)
mtext(side=2, "temperature (Celsius)", line=3)
text(x=4, y=30, "June")
text(x=4, y=3, "April")

polygon(
  x=c(jun.times[1], jun.times[2], jun.times[2], jun.times[1]),
  y=c(57, 57, 56, 56),
  col="grey70",
  border=NA)
polygon(
  x=c(apr.times[1], apr.times[2], apr.times[2], apr.times[1]),
  y=c(59, 59, 60, 60),
  col="grey70",
  border=NA)

text(x=22.5, y=59.5, "April daytime", cex=0.8)
text(x=22.5, y=56.5, "June daytime", cex=0.8)




plot(jun.dat$synctime/60, jun.dat$predicted_value, type="l", lwd=2, xlim=c(0,24), ylim=c(0,60), axes=F, ann=F)





# plot June data (gray polygon shows daylight--may want to change to nighttime)
plot(jun.dat$synctime/60, jun.dat$predicted_value, type="l", lwd=2, col="white", xlim=c(0,24), ylim=c(0,50), axes=F, ann=F)
polygon(
  x=c(jun.times[1], jun.times[2], jun.times[2], jun.times[1]),
  y=c(-10, -10, 100, 100),
  col="grey90",
  border=NA)
par(new=T)
plot(jun.dat$synctime/60, jun.dat$predicted_value, type="l", lwd=2, xlim=c(0,24), ylim=c(0,50), axes=F, ann=F)
axis(side=1, at=seq(0, 24, 4), labels=as.character(seq(0, 24, 4)))
axis(side=2, at=seq(0, 50, 10), labels=as.character(seq(0, 50, 10)), las=2)
mtext(side=1, "hour of day", line=3)
mtext(side=2, "temperature (Celsius)", line=3)
mtext(side=3, "June", line=2)

