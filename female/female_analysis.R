setwd("~/projects/dissertation/chapter_1/female_choice/data")
# load data file
female_all <- read.csv("trial_info.csv", sep = ",", header = TRUE, stringsAsFactors=FALSE)
setwd("~/projects/temp_trials/female_choice/analysis")
pdf(file = "plots.pdf")
par(mfrow=c(2,1))
# put numeric values in for accept and reject (1 and 0, respectively)
female_all$outcome[which(female_all$outcome=="accept")]<-1
female_all$outcome[which(female_all$outcome=="reject")]<-0

# divide up data between rounds 1 and 2, removing any rows for "reject" trials
round_1 <- subset(female_all, round == 1 & grepl("redo", outcome) == FALSE)

round_1$outcome<-as.numeric(round_1$outcome)
r1_warm <- subset(round_1, treatment == "warm")
r1_cool <- subset(round_1, treatment == "cold")


round_2 <- subset(female_all, round == 2 & grepl("redo", outcome) == FALSE)
round_2$outcome<-as.numeric(round_2$outcome)
r2_warm <- subset(round_2, treatment == "warm")
r2_cool <- subset(round_2, treatment == "cold")


warmlbls <- c("accept", "reject")
coollbls <- c("accept", "reject")

warmslices <- c(sum(r1_warm$outcome), (nrow(r1_warm) - sum(r1_warm$outcome)))
pctwarm <- round(warmslices/sum(warmslices)*100)
warmlbls <- paste(warmlbls, pctwarm)


warmlbls <- paste(warmlbls,"%",sep="")
pie(warmslices, labels = warmlbls, main="Warm Mating Rates - Round 1", col = c("red", "pink"), radius = 2)


coolslices <- c(sum(r1_cool$outcome), (nrow(r1_cool) - sum(r1_cool$outcome)))
pctcool <- round(coolslices/sum(coolslices)*100)
coollbls <- paste(coollbls, pctcool)
coollbls <- paste(coollbls,"%",sep="")
pie(coolslices, labels = coollbls, main="Cool Mating Rates - Round 1", col = c("blue", "light blue"))


# round 2


warmlbls <- c("accept", "reject")
coollbls <- c("accept", "reject")

warmslices <- c(sum(r2_warm$outcome), (nrow(r2_warm) - sum(r2_warm$outcome)))
pctwarm <- round(warmslices/sum(warmslices)*100)
warmlbls <- paste(warmlbls, pctwarm)
warmlbls <- paste(warmlbls,"%",sep="")
pie(warmslices, labels = warmlbls, main="Warm Mating Rates - Round 2", col = c("red", "pink"))


coolslices <- c(sum(r2_cool$outcome), (nrow(r2_cool) - sum(r2_cool$outcome)))
pctcool <- round(coolslices/sum(coolslices)*100)
coollbls <- paste(coollbls, pctcool)
coollbls <- paste(coollbls,"%",sep="")
pie(coolslices, labels = coollbls, main="Cool Mating Rates - Round 2", col = c("blue", "light blue"))

yes_r1 <-c(sum(r1_warm$outcome)/nrow(r1_warm), (sum(r1_cool$outcome)*100))
no_r1 <- c((nrow(r1_warm)/nrow(r1_warm) - yes_r1[1]), (nrow(r1_cool)/nrow(r1_cool) - yes_r1[2]))
r1 <- rbind(yes_r1, no_r1)

yes_r2 <-c(sum(r2_warm$outcome)/nrow(r2_warm), sum(r2_cool$outcome))
no_r2 <- c((nrow(r2_warm)/nrow(r2_warm) - yes_r2[1]), (nrow(r2_cool)/nrow(r2_cool) - yes_r2[2]))
r2 <- rbind(yes_r2, no_r2)

op <- par(mar = par("mar")/2)
#plot(1:10)
barplot(r1, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("red","pink", "blue", "light blue"),
        legend = c("accept", "reject"), beside=TRUE,
        names.arg=c("warm", "cool"))
box()

barplot(r2, main="Car Distribution by Gears and VS",
       xlab="Number of Gears", col=c("red","pink", "blue", "light blue"),
       legend = c("accept", "reject"), beside=TRUE,
       names.arg=c("warm", "cool"))
box()

dev.off()

