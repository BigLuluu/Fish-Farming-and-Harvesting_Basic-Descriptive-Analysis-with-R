#---------------------------------------------------#
# Module 3 Project - Plotting Basics: Christina Jin #
#---------------------------------------------------#

# step 1: load libraries
install.packages("plyr")
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("moments")
library("plyr")
library("FSA")
library("FSAdata")
library("magrittr")
library("dplyr")
library("plotrix")
library("ggplot2")
library("moments")
library(colorspace)
library(RColorBrewer)
library(vcd)


# step 2: print first and last 3 records
bio <- read.csv("C:/Users/Lu_ki/OneDrive - Northeastern University/NU Master Courseworks/ALY6000/Course Materials/Module 3/3_Project/R Scripts and Plots/inchBio.csv")
bio
bio <- as.data.frame(bio)


# step 3: display head tail and structure of 'bio'
headtail(bio,n=3)
str(bio)
summary(bio)


# step 4: create 'counts' that counts and lists all the species records
counts <- c(length(unique(bio$species)), distinct(bio, bio$species))
counts


# step 5: display the names of the species
distinct(bio, bio$species)
unique(bio$species)


# step 6: create 'tmp' to display different species and the number of record for each 
tmp <- count(bio,species)
colnames(tmp) <- c("species", "count")
arrange(tmp,desc(count))
tmp


# step 7: create subset 'tmp2' of species variable and display the first five records
tmp2 <- subset(tmp[1:5, ])
tmp2


# step 8: create table 'w' of species variables and classify 'w'
w <- table(bio$species)
class(w)
w


# step 9: convert 'w' to data frame named 't' and display results
t = as.data.frame(w)
t


# step 10: extract and display the frequency values from data frame 't' 
subset(t,select = Freq)


# step 11: create table 'cSpec' from bio species attribute and confirm class
cSpec <- table(bio$species)
is.table(cSpec)
cSpec


# step 12: create 'cSpecPct' that displays the species and percentage of records for each species, and confirm class
cSpecPct <- round(cSpec/sum(cSpec),4)
is.table(cSpecPct)
cSpecPct


# step 13: convert 'cSpecPct' to data frame as 'u', confirm class
u <- as.data.frame(cSpecPct)
is.data.frame(u)
u


# step 14: create barplot of 'cSpec'
par(mar=c(5,8,4,2))
par(las=2)
barplot(cSpec,
        main="Fish Count",
        xlab="COUNTS",
        col = "lightgreen",
        horiz = TRUE,
        cex.names = 0.6)


# step 15: create barplot of 'cSpecPct'
par(mar=c(2,6,10,1))
par(las=1)
barplot(cSpecPct,
        horiz = TRUE,
        ylim = c(0,5),
        xlim = c(0,0.35),
        col = "gold",
        cex.names = 0.7,
        cex.axis = 0.7)
title ("Fish Relative Frequency", line=7)


# step 16: rearrange 'u' in descending order and save as 'd'
d <- arrange(u, desc(Freq))
d


# step 17-18: rename column names
colnames(d) <- c("Species", "RelFreq")
d


# step 18: add cumfreq, counts, and cumcounts to 'd'
agg <- aggregate(t$Freq, list(Species=t$Var1, RelFreq=u$Freq), mean)
agg

agg.d <- arrange(agg, desc(RelFreq))
agg.d
colnames(agg.d) <- c("Species", "RelFreq", "Counts")
agg.d

d <- cbind(agg.d, 
           CumFreq=cumsum(agg.d$RelFreq), 
           CumCounts=cumsum(agg.d$Counts))
d$RelFreq <- paste(round((d$RelFreq*100),0), "%")
d$CumFreq <- paste(round((d$CumFreq*100),0), "%")
d


# step 19: 
def_par <- par (mar=c(6.2,5,5,3.5))


# step 20: create barplot 'pc'
par(las=2)
pc = barplot(height=d$Counts,
        def_par,  
        width = 1,space = 0.15,
        legend = FALSE,
        border = NA,
        axes = F,
        ylim=c(0,3.05*max(d$Counts, na.rm = TRUE)),
        ylab = "Cummulative Counts",
        cex.names = 0.7,
        names.arg = d$Species)
title ("Species Pareto", line = 3)
      
  
# step 21: cumulative counts line ploy to 'pc'
lines(pc,d$CumCounts, type = "b", cex=0.7, pch=19, col="cyan4")


# step 22: draw box around the plot
box(lty=1, col = "grey")


# step 23: draw left axis
axis(2, at=d$CumCounts, 
     labels=d$CumCounts, 
     tick=TRUE, cex.axis=0.8,
     col.axis="grey62", col.ticks = "grey62")


# step 24: draw left axis
axis(4, at=d$CumCounts, 
     labels=d$CumFreq, 
     tick=TRUE, cex.axis=0.8,
     col.axis="cyan4", col.ticks = "cyan4")
title ("by Christina Lu Jin", line = 1, cex.main=0.8)


# Extra steps:
# omitting N/A values
bio_clean <- na.omit(bio[, c("species", "tl", "w", "scale")])
bio_clean
str(bio_clean)
summary(bio_clean)
# Selecting only the mature fishes
bio_mature <- filter(bio_clean, w > 10)
bio_mature
# count number of fishes of the mature ones for each kind
bio_mature_count <- count(bio_mature, species)
colnames(bio_mature_count) <- c("species", "count")
arrange(bio_mature_count,desc(count))
bio_mature_count

# create a series of scatter plots for weight in relationship of length
BlackCrappie <- bio_clean[bio_clean$species=="Black Crappie",]
Bluegill <- bio_clean[bio_clean$species=="Bluegill",]
BluntnoseMinnow <- bio_clean[bio_clean$species=="Bluntnose Minnow",]
IowaDarter <- bio_clean[bio_clean$species=="Iowa Darter",]
LargemouthBass <- bio_clean[bio_clean$species=="Largemouth Bass",]
Pumpkinseed <- bio_clean[bio_clean$species=="Pumpkinseed",]
TadpoleMadtom <- bio_clean[bio_clean$species=="Tadpole Madtom",]
YellowPerch <- bio_clean[bio_clean$species=="Yellow Perch",]

dev.off()
opar <- par(no.readonly = TRUE)
par(mfrow=c(2,4))
opar <- par(no.readonly=TRUE)
plot(BlackCrappie$tl,BlackCrappie$w,
     main="Black Crappie - Weight vs Length",
     xlab="Length", ylab="Weight",
     xlim=c(0,450), ylim=c(0,1100),
     pch=16, col=c("pink"))
plot(Bluegill$tl,Bluegill$w,
     main="Bluegill - Weight vs Length",
     xlab="Length", ylab="Weight",
     xlim=c(0,450), ylim=c(0,1100),
     pch=16, col=c("red"))
plot(BluntnoseMinnow$tl,BluntnoseMinnow$w,
     main="Bluntnose Minnow - Weight vs Length",
     xlab="Length", ylab="Weight",
     xlim=c(0,450), ylim=c(0,1100),
     pch=16, col=c("orange"))
plot(IowaDarter$tl,IowaDarter$w,
     main="Iowa Darter - Weight vs Length",
     xlab="Length", ylab="Weight",
     xlim=c(0,450), ylim=c(0,1100),
     pch=16, col=c("yellow"))
plot(LargemouthBass$tl,LargemouthBass$w,
     main="Largemouth Bass - Weight vs Length",
     xlab="Length", ylab="Weight",
     xlim=c(0,450), ylim=c(0,1100),
     pch=16, col=c("green"))
plot(Pumpkinseed$tl,Pumpkinseed$w,
     main="Pumpkinseed - Weight vs Length",
     xlab="Length", ylab="Weight",
     xlim=c(0,450), ylim=c(0,1100),
     pch=16, col=c("cyan"))
plot(TadpoleMadtom$tl,TadpoleMadtom$w,
     main="Tadpole Madtom - Weight vs Length",
     xlab="Length", ylab="Weight",
     xlim=c(0,450), ylim=c(0,1100),
     pch=16, col=c("blue"))
plot(YellowPerch$tl,YellowPerch$w,
     main="Yellow Perch - Weight vs Length",
     xlab="Length", ylab="Weight",
     xlim=c(0,450), ylim=c(0,1100),
     pch=16, col=c("purple"))


     
     
     