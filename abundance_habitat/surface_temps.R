setwd("/home/eebrandt/projects/dissertation/chapter_1/field_temps/data")
surface_temp <- read.csv("new_substrate_temp_for_plotting.csv", header = TRUE)

apr_rock_shade <- subset(surface_temp,category == "apr_rock_shade" & dectime > 8.41 & dectime < 15.0333)
apr_rock_sun <- subset(surface_temp, category =="apr_rock_sun"& dectime > 8.41 & dectime < 15.0333 )
apr_litter_shade <- subset(surface_temp, category == "apr_litter_shade"& dectime > 8.1 & dectime < 15.0333)
apr_litter_sun <- subset(surface_temp, category == "apr_litter_sun"& dectime > 8.41 & dectime < 15.0333)

ap_avg <- data.frame("time" = apr_rock_sun$dectime, "rock_sun" = apr_rock_sun$predicted_celsius, "litter_sun" = apr_litter_sun$predicted_celsius, "rock_shade" = apr_rock_shade$predicted_celsius, "litter_shade" = apr_litter_shade$predicted_celsius)
ap_avg$avg_sun <- rowMeans(ap_avg[,c("rock_sun", "litter_sun")], na.rm=TRUE)
ap_avg$avg_shade <- rowMeans(ap_avg[,c("rock_shade", "litter_shade")], na.rm=TRUE)


jun_rock_shade <- subset(surface_temp, category =="jun_rock_shade" & dectime > 8.41 & dectime < 15.0333)
jun_rock_sun <- subset(surface_temp, category =="jun_rock_sun"& dectime > 8.41 & dectime < 15.0333)
jun_litter_shade <- subset(surface_temp, category == "jun_litter_shade"& dectime > 8.41 & dectime < 15.0333)
jun_litter_sun <- subset(surface_temp, category == "jun_litter_sun"& dectime > 8.41 & dectime < 15.0333)

jun_avg <- data.frame("time" = jun_rock_sun$dectime, "rock_sun" = jun_rock_sun$predicted_celsius, "litter_sun" = jun_litter_sun$predicted_celsius, "rock_shade" = jun_rock_shade$predicted_celsius, "litter_shade" = jun_litter_shade$predicted_celsius)
jun_avg$avg_sun <- rowMeans(jun_avg[,c("rock_sun", "litter_sun")], na.rm=TRUE)
jun_avg$avg_shade <- rowMeans(jun_avg[,c("rock_shade", "litter_shade")], na.rm=TRUE)

par(mfrow=c(1,1))
plot(ap_avg$avg_sun~ap_avg$time, type="l", lwd = 2, col = "blue", xlim= c(8,16), ylim = c(0,60), axes = FALSE, ylab = expression(paste("temperature (",degree,"C)")), xlab = "hour of day")
axis(side = 1, at = c(8,10,12,14,16))
axis(side = 2, at = c(0, 20, 40, 60))
lines(ap_avg$avg_shade~ap_avg$time, type = "l", lwd = 2, col = "blue", lty = 2)
lines(jun_avg$avg_sun~jun_avg$time, type="l", lwd = 2, col = "red")
lines(jun_avg$avg_shade~jun_avg$time, type = "l", lwd = 2, lty = 2, col = "red")
rect(7.5, 16, 16.5, 24, col = rgb(160, 160, 160, 100, maxColorValue = 255))
rect(7.5, 49, 16.5, 32, col = rgb(160, 160, 160, 100, maxColorValue = 255))
rect(7.5, 32, 16.5, 24, col = rgb(160, 160, 160, 100, maxColorValue = 255))
box()
