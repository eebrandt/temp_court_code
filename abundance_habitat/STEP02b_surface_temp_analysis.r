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
temp <- read.csv("surface_temperature.csv")


# TRUNCATE DATA
temp <- temp[, c("date", "time", "veg_type", "light_site", "northing_deg", "northing_min", "obj", "object_feature", "light_environ", "temp_fahren")]


# FIX DATA
temp$temp_celsius <- (temp$temp_fahren - 32) * 5 / 9

# OVERALL MODEL












# FIX SUBSTRATE TYPES
temp$obj_type <- "rock"
temp[which(temp$obj!="rock"), ]$obj_type <- "litter"

# FIX OTHER DATA
temp$northing_decimal <- temp$northing_deg + temp$northing_min/60
temp2 <- temp[which(!temp$veg_type=="cofo"), ]
temp2 <- temp2[-which(temp$veg_type=="brwo" & temp$northing_decimal>31.70), ]
temp2$veg_type <- as.factor(temp2$veg_type)

# FIX TIME
temp2 <- na.omit(temp2)
temp2$timedate <- strptime(temp2$date, format="%d-%b-%y")
temp2$month <- temp2$timedate$mon + 1
x <- matrix(unlist(strsplit(as.character(temp2$time), ":")), ncol=2, byrow=T)
temp2$dectime <- as.numeric(x[, 1]) + as.numeric(x[,2])/60
temp2$yearday <- temp2$timedate$yday

# OVERALL MODELING
mod1 <- gamm(temp_celsius ~ light_site + obj_type + light_environ + s(dectime), random=list(month=~1), data=temp2)
mod2 <- gamm(temp_celsius ~ obj_type + light_environ + s(dectime), random=list(month=~1), data=temp2)

temp2[which(temp2$light_environ=="partial_shade"), ]$light_environ <- "shade"
temp2$light_environ <- factor(temp2$light_environ)
temp2$obj_type <- factor(temp2$obj_type)


# APRIL
temp3 <- temp2[which(temp2$month=="4"), ]
mod <- gamm(temp_celsius ~ light_environ + s(dectime, by=light_environ), random=list(yearday=~1, veg_type=~1), data=temp3)
plot(mod$gam, all.terms=T, pages=1)

suntemp <- temp3[which(temp3$light_environ=="sun"), ]
sun <- data.frame(expand.grid(month="april", light_environ="sun", dectime=seq(range(suntemp$dectime)[1], range(suntemp$dectime)[2], 0.02)))
shadetemp <- temp3[which(temp3$light_environ=="shade"), ]
shade <- data.frame(expand.grid(month="april", light_environ="shade", dectime=seq(range(shadetemp$dectime)[1], range(shadetemp$dectime)[2], 0.02)))

sun$predfit <- predict(mod$gam, newdata=sun, type="response")
shade$predfit <- predict(mod$gam, newdata=shade, type="response")

aprilsun <- sun
aprilshade <- shade




# JUNE
temp3 <- temp2[which(temp2$month=="6"), ]
mod <- gamm(temp_celsius ~ light_environ + s(dectime, by=light_environ), random=list(yearday=~1, veg_type=~1), data=temp3)
plot(mod$gam, all.terms=T, pages=1)

suntemp <- temp3[which(temp3$light_environ=="sun"), ]
sun <- data.frame(expand.grid(month="june", light_environ="sun", dectime=seq(range(suntemp$dectime)[1], range(suntemp$dectime)[2], 0.02)))
shadetemp <- temp3[which(temp3$light_environ=="shade"), ]
shade <- data.frame(expand.grid(month="june", light_environ="shade", dectime=seq(range(shadetemp$dectime)[1], range(shadetemp$dectime)[2], 0.02)))

sun$predfit <- predict(mod$gam, newdata=sun, type="response")
shade$predfit <- predict(mod$gam, newdata=shade, type="response")

junesun <- sun
juneshade <- shade

# COMBINE
alldat <- rbind(aprilsun, aprilshade, junesun, juneshade)
colnames(alldat)[4] <- "celsius_deg"

write.csv(alldat, "new_substrate_temp_for_plotting.csv", row.names=F)

plot(aprilsun$dectime, aprilsun$predfit, type="l", ylim=c(0, 60))
par(new=T)
plot(aprilshade$dectime, aprilshade$predfit, type="l", ylim=c(0, 60))








# BASIC MODELING TO LOOK AT FACTORS THAT MAY INFLUENCE TEMP
# examine temp as interaction of rock size and time of day for each period
# examine temp as interaction of sun area and time of day for each period
temp_apr <- temp2[which(temp2$month=="4"), ]
temp_jun <- temp2[which(temp2$month=="6"), ]

# april rock size effect (qualitative)
rocks <- temp_apr[which(temp_apr$obj_type=="rock"),]
mod <- glm(temp_celsius ~ object_feature*dectime, data=rocks) # no influence of interaction between dectime and rock size
mod0 <- glm(temp_celsius ~ light_site*light_environ, data=rocks) # not including dectime, since all data are effectively paired; no interaction
mod1 <- glm(temp_celsius ~ light_site + light_environ, data=rocks) 
mod2 <- glm(temp_celsius ~ light_site, data=rocks) 
mod3 <- glm(temp_celsius ~ light_environ, data=rocks) 
# Likelihood ratio tests for each time period (Apr and June) show
# that mod3 is simplest, optimal model. 


# june rock size effect (qualitative)
rocks <- temp_jun[which(temp_jun$obj_type=="rock"),]
mod <- glm(temp_celsius ~ object_feature*dectime, data=rocks) # no influence of interaction between dectime and rock size (excluding "no data")
mod0 <- glm(temp_celsius ~ light_site*light_environ, data=rocks) # not including dectime, since all data are effectively paired; no interaction
mod1 <- glm(temp_celsius ~ light_site + light_environ, data=rocks) 
mod2 <- glm(temp_celsius ~ light_site, data=rocks) 
mod3 <- glm(temp_celsius ~ light_environ, data=rocks) 
# Likelihood ratio tests for each time period (Apr and June) show
# that mod3 is simplest, optimal model. 

temp2$month2 <- as.factor(temp2$month)
mod4 <- glm(temp_celsius ~ month2*light_environ, data=temp2) 


# COMBINE SHADE AND PARTIAL_SHADE (QUALITATIVE ASSESSMENT)
temp2[which(temp2$light_environ=="partial_shade"), ]$light_environ <- "shade"
temp2$light_environ <- as.factor(temp2$light_environ)

# April rocks in shade and sun
apr_rock_shade <- temp2[which(
  temp2$obj_type=="rock" & 
  temp2$light_environ=="shade" & 
  temp2$month2=="4"
  ), ]
mod <- gam(temp_celsius ~ s(dectime, k=4), data=apr_rock_shade)
range.vals <- range(apr_rock_shade$dectime)
newdat <- data.frame(dectime=seq(range.vals[1], range.vals[2], 0.01))
newdat$category <- "apr_rock_shade"
newdat01 <- newdat[, c("category", "dectime")]
pred01 <- predict(mod, newdat01)

apr_rock_sun <- temp2[which(
  temp2$obj_type=="rock" & 
  temp2$light_environ=="sun" & 
  temp2$month2=="4"
  ), ]
mod <- gam(temp_celsius ~ s(dectime, k=4), data=apr_rock_sun)
range.vals <- range(apr_rock_sun$dectime)
newdat <- data.frame(dectime=seq(range.vals[1], range.vals[2], 0.01))
newdat$category <- "apr_rock_sun"
newdat02 <- newdat[, c("category", "dectime")]
pred02 <- predict(mod, newdat02)

# April litter in shade and sun
apr_litter_shade <- temp2[which(
  temp2$obj_type=="litter" & 
  temp2$light_environ=="shade" & 
  temp2$month2=="4"
  ), ]
mod <- gam(temp_celsius ~ s(dectime, k=4), data=apr_litter_shade)
range.vals <- range(apr_litter_shade$dectime)
newdat <- data.frame(dectime=seq(range.vals[1], range.vals[2], 0.01))
newdat$category <- "apr_litter_shade"
newdat03 <- newdat[, c("category", "dectime")]
pred03 <- predict(mod, newdat03)

apr_litter_sun <- temp2[which(
  temp2$obj_type=="litter" & 
  temp2$light_environ=="sun" & 
  temp2$month2=="4"
  ), ]
mod <- gam(temp_celsius ~ s(dectime, k=4), data=apr_litter_sun)
range.vals <- range(apr_litter_sun$dectime)
newdat <- data.frame(dectime=seq(range.vals[1], range.vals[2], 0.01))
newdat$category <- "apr_litter_sun"
newdat04 <- newdat[, c("category", "dectime")]
pred04 <- predict(mod, newdat04)


# June rocks in shade and sun
jun_rock_shade <- temp2[which(
  temp2$obj_type=="rock" & 
  temp2$light_environ=="shade" & 
  temp2$month2=="6"
  ), ]
mod <- gam(temp_celsius ~ s(dectime, k=4), data=jun_rock_shade)
range.vals <- range(jun_rock_shade$dectime)
newdat <- data.frame(dectime=seq(range.vals[1], range.vals[2], 0.01))
newdat$category <- "jun_rock_shade"
newdat05 <- newdat[, c("category", "dectime")]
pred05 <- predict(mod, newdat05)

jun_rock_sun <- temp2[which(
  temp2$obj_type=="rock" & 
  temp2$light_environ=="sun" & 
  temp2$month2=="6"
  ), ]
mod <- gam(temp_celsius ~ s(dectime, k=4), data=jun_rock_sun)
range.vals <- range(jun_rock_sun$dectime)
newdat <- data.frame(dectime=seq(range.vals[1], range.vals[2], 0.01))
newdat$category <- "jun_rock_sun"
newdat06 <- newdat[, c("category", "dectime")]
pred06 <- predict(mod, newdat06)

# June litter in shade and sun
jun_litter_shade <- temp2[which(
  temp2$obj_type=="litter" & 
  temp2$light_environ=="shade" & 
  temp2$month2=="6"
  ), ]
mod <- gam(temp_celsius ~ s(dectime, k=4), data=jun_litter_shade)
range.vals <- range(jun_litter_shade$dectime)
newdat <- data.frame(dectime=seq(range.vals[1], range.vals[2], 0.01))
newdat$category <- "jun_litter_shade"
newdat07 <- newdat[, c("category", "dectime")]
pred07 <- predict(mod, newdat07)

jun_litter_sun <- temp2[which(
  temp2$obj_type=="litter" & 
  temp2$light_environ=="sun" & 
  temp2$month2=="6"
  ), ]
mod <- gam(temp_celsius ~ s(dectime, k=4), data=jun_litter_sun)
range.vals <- range(jun_litter_sun$dectime)
newdat <- data.frame(dectime=seq(range.vals[1], range.vals[2], 0.01))
newdat$category <- "jun_litter_sun"
newdat08 <- newdat[, c("category", "dectime")]
pred08 <- predict(mod, newdat08)

# COMBINE
all_predicted <- c(pred01, pred02, pred03, pred04, pred05, pred06, pred07, pred08)
all_newdat <- rbind(newdat01, newdat02, newdat03, newdat04, newdat05, newdat06, newdat07, newdat08)

all_newdat$predicted_celsius <- all_predicted
write.csv(all_newdat, "predicted_surface_temps.csv")

# GRAPH
section <- split(all_newdat, all_newdat$category)
for (i in seq_along(section)) {
  sect <- section[[i]]
  # strsplit(sect$category[1], "_")[[1]]
  plot(sect$dectime, sect$predicted_celsius, type="l", ylim=c(0,60), xlim=c(6, 18))
  par(new=T)
  }


