# Plots to generate Figure 1 in male/female temp paper: apparent abundance
# air temperature
# Erin Brandt 2-10-2016
# library
library("chron", lib.loc="/home/eebrandt/R/x86_64-pc-linux-gnu-library/3.2")

# Abundance Plot
# read in abundance data
abundance <- read.csv("~/Dropbox/spider_project_2012/habronattus_survey_project_2012/output/final_model_predictions_TIME_MADDIE.csv")
#normalize abundance - make it a non dimensional quantity that goes
#from zero to one
abundance$n_abundance <- abundance$predfit/max(abundance$upper.ci)
# normalize upper confidence interval
abundance$n_UCI <- abundance$upper.ci/max(abundance$upper.ci)
# normalize lower confidence interval
abundance$n_LCI <- abundance$lower.ci/max(abundance$upper.ci)

# plot abundance
# set up plot
plot( 0 , type = "n" , bty = "n" , xlab = "Time of Day" , ylab = "Normalized apparent abundance", xlim = c(8,16) , ylim = c(0,.5), axes = FALSE )
#draw polygon for confidence intervals
polygon(c(abundance$time.mid,rev(abundance$time.mid)),c(abundance$n_LCI,rev(abundance$n_UCI)),col="grey", border = NA)
# add line for plot
lines(n_abundance~time.mid, data = abundance,type = "l", lwd = 2)
# add x axis
axis(side = 1, at = c(8, 10, 12, 14,16))
# add y axis
axis(side = 2, at = c(0, 0.25, 0.5, 0.75, 1))
# draw box around them
box()

# Draw temperature plot
# read in air temps
temps <- read.csv("~/Dropbox/spider_project_2012/habronattus_survey_project_2012/output/predicted_temps_by_month.csv")
# put time into "real" terms (minutes)
realtime <- temps$synctime/60
# put times into a format recognized by R 
realtime <- times(realtime)
# put formatted times into data frame
temps$realtime <- times(realtime)

# subsets temperatures into april and june data sets
aprilonly <- subset(temps, temps$month == "april" & temps$realtime > 8 & temps$realtime < 15)
juneonly <- subset(temps, temps$month == "june" & temps$realtime > 8 & temps$realtime < 15)

#plot the june data
#note: only includes times that are also included on abundance data
plot(predicted_value~realtime, data = juneonly, 
pch = 2, ylim =c(0, 60), xlim = c(8,16), type = "l", lwd = 2,cex = 3, ylab = expression(paste("Temperature (",degree,"C)")),
xlab = "Hour of Day", col = "red", axes = FALSE)
# adds x axis
axis(side = 1, at = c(8,10,12,14,16))
# adds y axis
axis(side = 2, at = c(0, 20, 40, 60))
# adds april line
lines(predicted_value~realtime, data = aprilonly, lwd = 2, cex = 3,col = "blue")
# add box around plot
box()


