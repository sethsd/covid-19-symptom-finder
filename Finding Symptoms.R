library(calibrate)
library(gtrendsR)

dat1 <- read.csv("/users/seth/downloads/LossOfSmell - LossOfSmell (1) (1).csv")
symps <- read.csv("/users/seth/downloads/Symptoms - Sheet1 (2).csv")
symps$CorNow <- NA
symps$CorPrev <- NA
dat1 <- dat1[,c("Statename","Positive.Cases.Per.1.000.People")]
names(dat1) <- c("Statename","PositiveRate")
for (i in 1:length(symps$Symptom))  {
  
dat.use <- gtrends(keyword=c(as.character(symps$Code[i])),geo=c("US"),time="now 7-d")
dat.use2 <- gtrends(keyword=c(as.character(symps$Code[i])),geo=c("US"),time="2019-3-1 2019-3-29")

dat.loc <- dat.use$interest_by_region[,1:2]
names(dat.loc) <- c("Statename","Hits")
dat.loc <- merge(dat.loc,dat1,by=c("Statename"))

dat.loc2 <- dat.use2$interest_by_region[,1:2]
names(dat.loc2) <- c("Statename","Hits")
dat.loc2 <- merge(dat.loc2,dat1,by=c("Statename"))
dat.loc <- dat.loc[complete.cases(dat.loc$Hits),]
dat.loc2 <- dat.loc2[complete.cases(dat.loc2$Hits),]
symps$CorNow[i] <- cor(dat.loc$Hits,dat.loc$PositiveRate)
symps$CorPrev[i] <- cor(dat.loc2$Hits,dat.loc2$PositiveRate)
dat.loc <- dat.loc[,c("Statename","Hits")]
names(dat.loc) <- c("Statename","Positive7")
dat.loc2 <- dat.loc2[,c("Statename","Hits")]
names(dat.loc2) <- c("Statename","PositivePrev")

dat.tot <- merge(dat.loc,dat.loc2,by=c("Statename"))
dat.tot$Rise <- dat.tot$Positive7/dat.tot$PositivePrev
names(dat.tot)[4] <- as.character(symps$Symptom[i])
dat.tot <- dat.tot[,c("Statename",as.character(symps$Symptom[i]))]
if (i==1) {
  dat.keep <- dat.tot
}
if (i!=1) {
  dat.keep <- merge(dat.keep,dat.tot,by=c("Statename"))
}
}

symps2 <- symps
names(symps) <- c("Symptom","Code","Correlation: Covid-19 Cases p.c. & Google Search Rates, Past 7 Days","Correlation: Covid-19 Cases p.c. & Google Search Rates, Comparable Time Past Year")

# symps <- symps[-order(symps[,c("Correlation: Covid-19 Cases p.c. & Google Search Rates, Past 7 Days")])]
# names(symps) <- c("Symptom","Code","Correlation: Covid-19 Cases p.c. & Google Search Rates, Past 7 Days","Correlation: Covid-19 Cases p.c. & Google Search Rates, Comparable Time Past Year")

