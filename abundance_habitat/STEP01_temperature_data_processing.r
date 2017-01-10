#==========================================================================
# DESCRIPTION: Processing of temperature datalogger data
# PROJECT: Habronattus jumping spider project (2012)
# R VERSION: R 3.0.2, 32-bit
# DATA OWNER: J. Patrick Kelley
# CODE AUTHOR: J. Patrick Kelley
# CONTACT EMAIL: kelley@post.harvard.edu
#==========================================================================

# LOAD LIBRARIES FROM LOCAL SOURCE FILE ("./source")
source("C:/Users/Patrick/Dropbox/spider_project_2012/habronattus_survey_project_2012/source/load_packages.r")

# READ IN APRIL WEATHER DATA
setwd("C:/Users/Patrick/Dropbox/spider_project_2012/habronattus_survey_project_2012/")

# get datalogger information
datlog.orig <- read.csv("./data/raw_data/datalogger_climate/datalogger_information_2012.csv")
datlog <- datlog.orig
colnames(datlog)[1] <- "month"
datlog <- datlog[, c("logger_id", "site", "habitat", "morning_light_class", "microhabitat", "easting", "northing", "month")]
datlog$logger_month <- paste(datlog$logger_id, datlog$month, sep="_")
datlog$logger_id <- NULL
datlog$month <- NULL


# get temperature data
filelist <- list.files("./data/raw_data/datalogger_climate/temperature")
filelist <- filelist[grep(pattern="_temp", filelist)]
dat.set <- list()
time.data <- list()
for (i in seq_along(filelist)) {
  fileid <- filelist[[i]]
  file.dir <- paste(getwd(), "/data/raw_data/datalogger_climate/temperature/", fileid, sep="")
  d <- read.csv(file.dir)
  
  # Extract data from raw datalogger output
  d <- d[which(d[,1]=="Date/Time"):dim(d)[1],1]
  d <- matrix(d, ncol=3, byrow=T)
 	d <- cbind(matrix(rep(substr(fileid, 1, 6),nrow(d))), d)
  d <- d[-1, ]
  d <- data.frame(d)
  colnames(d) <- c("logger_id","time.info","units","value")
  d$value <- as.numeric(as.character(d$value))
  time.data[[i]] <- data.frame(logger_id=d$logger_id[1], datestart=head(d$time.info, 1), datestop=tail(d$time.info, 1))
  
  
  # CHECK TIME FORMAT (oddly, different between datasheets)
  d$time.info <- as.character(d$time.info)
  test <- substr(d$time.info, start=nchar(d$time.info)-1, stop=nchar(d$time.info))
  if (any(test=="PM")) {d$time.info <- strptime(d[, 2], format="%m/%d/%y %I:%M:%S %p")
   } else {d$time.info <- strptime(d[, 2], format="%m/%d/%Y %H:%M")}
  
  d$year <- d$time.info$year + 1900
  d$yday <- d$time.info$yday
	 d$hour <- d$time.info$hour
	 d$min <- d$time.info$min
	 d$synctime <- d$hour*60 + d$min
	 d$total.synctime <- d$yday+d$hour/24+d$min/60/24 # synch across year
  d$time.info <- NULL
  
  # add month to data
  d$month <- "april"
  if(any(d$yday >= 140)) {d$month <- "june"}
  d$logger_month <- paste(d$logger_id, d$month, sep="_")
  
  dat.set[[i]] <- merge(d, datlog)
}

dat.set <- ldply(dat.set)
time.data <- ldply(time.data)


# CENSORING DATA  (Manual input)
notes <- datlog.orig[which(datlog.orig$notes!=""),] # visualize notes
dat.set <- dat.set[which(!dat.set$logger_id=="log_F0"), ]
dat.set$logger_id <- factor(dat.set$logger_id)
dat.set$logger_month <- factor(dat.set$logger_month)
dat.set$day.factor <- as.factor(dat.set$yday)

x <- dat.set[which(dat.set$logger_month=="log_45_april"), ]

# SAVE TEMPERATURE DATA (in CSV format)
filename.csv <- "C:/Users/Patrick/Dropbox/spider_project_2012/habronattus_survey_project_2012/data/processed_data/all_temperature_data.csv"
write.csv(dat.set, filename.csv, row.names=F)


