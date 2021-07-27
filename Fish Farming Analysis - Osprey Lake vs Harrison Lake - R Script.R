#-----------------------------------------#
# Module 2 Project - Plotting Basics: Jin
#-----------------------------------------#

# step 2-3: load libraries
install.packages("plyr")
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("moments")

# step 4: print first and last 3 records
BullTrout <- read.csv("BullTroutRML2.csv")
BullTrout
n <- nrow(BullTrout)
rbind(BullTrout[1:3,],BullTrout[(n-2):n,])

# step 5: remove all records except those from Harrison Lake
HarrisonBullTrout <- BullTrout[BullTrout$lake == "Harrison",]
HarrisonBullTrout
# step 6: display first and last 5 records
n <- nrow(HarrisonBullTrout)
rbind(HarrisonBullTrout[1:5,],HarrisonBullTrout[(n-4):n,])
      
# step 7-8: display structure & summary
str(HarrisonBullTrout)
summary(HarrisonBullTrout)
sd_h.age <- sd(HarrisonBullTrout$age)
sd_h.fl <- sd(HarrisonBullTrout$fl)
nor.sd_h.age <- c(sd(HarrisonBullTrout$age)/mean(HarrisonBullTrout$age))
nor.sd_h.fl <- c(sd(HarrisonBullTrout$fl)/mean(HarrisonBullTrout$fl))
sd_h.age
nor.sd_h.age
sd_h.fl
nor.sd_h.fl

# step 9: scatterplot for age vs fl
attach(HarrisonBullTrout)
opar <- par(no.readonly=TRUE)
plot(age~fl,
     xlim=c(0,500), ylim=c(0,15),
     main="Plot 1: Harrison Lake Trout Scatter",
     xlab="Fork Length(mm)", ylab="Age(yrs)",
     pch=19)

# step 10: plot "Age" histogram
hist(age, 
     ylab="Frequency", xlab="Age(yrs)", 
     main="Plot 2: Harrison Fish Age Distribution Histogram", 
     col.main="cadetblue",
     xlim=c(0,15), ylim=c(0,15), col="cadetblue")

# step 11: create an overdense plot and shade by Era
install.packages("RColorBrewer")
library(RColorBrewer)
n <- 8
mycolors <- brewer.pal(n,"Greens")
barplot(rep(1,n), col=mycolors)
print(mycolors)
green1 <- c(mycolors[6])
green1

colors <- c("green",green1)
colors <- colors[as.Date.numeric(HarrisonBullTrout$era)]

attach(HarrisonBullTrout)
plot(age~fl,
     xlim=c(0,500), ylim=c(0,15),
     main="Plot 3: Harrison Density Shaded by Era",
     xlab="Fork Length(mm)", ylab="Age(yrs)",
     pch=16, col=c("green",green1))
legend("topleft", inset=0.05, c("1977-80 Era", "1997-01 Era"),
       pch=19, col=c("green",green1), bty="n", 
       bg="transparent", pt.cex=2)

# step 12-13: create new object with first and last 3 rows from dataset
n <- nrow(HarrisonBullTrout)
tmp <- rbind(HarrisonBullTrout[1:3,], HarrisonBullTrout[(n-2):n,])
tmp
tmp$era

# step 14-15: creates vectors 
pchs <- c(3, 4)
pchs
cols <- c("red", "gray60")
cols

# step 16-17: covert tmp era to numeric values, initialize cols with tmp era
class(tmp$era)
tmp$era <- as.factor(tmp$era)
tmp$era
era_num <- as.numeric(as.factor(tmp$era))
era_num
pchs[era_num]
cols[era_num]

# step 18: create a plot of fl vs age with pchs and cols
plot(age~fl,
     xlim=c(0,500), ylim=c(0,15),
     main="Plot 4: Symbol & Color by Era",
     xlab="Fork Length(mm)", ylab="Age(yrs)",
     pch=pchs, col=cols)

# step 19: plot with regression line overlay
plot(age~fl,
     xlim=c(0,500), ylim=c(0,15),
     main="Plot 5: Regression Overlay",
     xlab="Fork Length(mm)", ylab="Age(yrs)",
     pch=pchs, col=cols)
abline(lm(age~fl), col="green", lwd=2)

# step 20: plot with legend overlay
plot(age~fl,
     xlim=c(0,500), ylim=c(0,15),
     main="Plot 6: Legend & Regression Overlay",
     xlab="Fork Length(mm)", ylab="Age(yrs)",
     pch=pchs, col=cols)
abline(lm(age~fl), col="green", lwd=2)
legend("topleft", inset=0.05, c("1977-80 Era", "1997-01 Era"), 
       pch=pchs, col=cols, bty="n", bg="transparent", pt.cex=2)  



# extra step 1: visualization of all 6 graphs
opar <- par(no.readonly = TRUE)
par(mfrow=c(2,3))
opar <- par(no.readonly=TRUE)

plot(age~fl,
     xlim=c(0,500), ylim=c(0,15),
     main="Plot 1: Harrison Lake Trout Scatter",
     xlab="Fork Length(mm)", ylab="Age(yrs)",
     pch=19)

hist(age, 
     ylab="Frequency", xlab="Age(yrs)", 
     main="Plot 2: Harrison Fish Age Distribution Histogram", 
     col.main="cadetblue",
     xlim=c(0,15), ylim=c(0,15), col="cadetblue")

plot(age~fl,
     xlim=c(0,500), ylim=c(0,15),
     main="Plot 3: Harrison Density Shaded by Era",
     xlab="Fork Length(mm)", ylab="Age(yrs)",
     pch=16, col=c("green",green1))
legend("topleft", inset=0.02, c("1977-80 Era", "1997-01 Era"),
       pch=19, col=c("green",green1), bty="n", 
       bg="transparent", pt.cex=2)
    
plot(age~fl,
     xlim=c(0,500), ylim=c(0,15),
     main="Plot 4: Symbol & Color by Era",
     xlab="Fork Length(mm)", ylab="Age(yrs)",
     pch=pchs, col=cols)

plot(age~fl,
     xlim=c(0,500), ylim=c(0,15),
     main="Plot 5: Regression Overlay",
     xlab="Fork Length(mm)", ylab="Age(yrs)",
     pch=pchs, col=cols)
abline(lm(age~fl), col="green", lwd=2)

plot(age~fl,
     xlim=c(0,500), ylim=c(0,15),
     main="Plot 6: Legend & Regression Overlay",
     xlab="Fork Length(mm)", ylab="Age(yrs)",
     pch=pchs, col=cols)
abline(lm(age~fl), col="green", lwd=2)
legend("topleft", inset=0.02, c("1977-80 Era", "1997-01 Era"), 
       pch=pchs, col=cols, bty="n", bg="transparent", pt.cex=2) 
par(opar)
    
# extra step 2: understand the difference in age and length in Osprey Lake
OspreyBullTrout <- BullTrout[BullTrout$lake == "Osprey",]
OspreyBullTrout
str(OspreyBullTrout)
summary(OspreyBullTrout)
sd_o.age <- sd(OspreyBullTrout$age)
sd_o.fl <- sd(OspreyBullTrout$fl)
nor.sd_o.age <- c(sd(OspreyBullTrout$age)/mean(OspreyBullTrout$age))
nor.sd_o.fl <- c(sd(OspreyBullTrout$fl)/mean(OspreyBullTrout$fl))
sd_o.age
nor.sd_o.age
sd_o.fl
nor.sd_o.fl

# extra step 3: discover how different river effect in age and length
par(mfrow=c(2,2))
boxplot(HarrisonBullTrout$age, OspreyBullTrout$age, 
        ylim=c(0,15), horizontal = TRUE,
        xlab="Age(yrs)", col=c("salmon", 'gold'),
        main="Plot 7: Age Distribution Harrison vs Osprey")
legend("topright", legend=c("Osprey", "Harrison"),
       inset=0.01, pch=15, col=c("gold", "salmon"),
       bty="n", bg="transparent", pt.cex=2) 
boxplot(HarrisonBullTrout$fl, OspreyBullTrout$fl, 
        ylim=c(0,700), horizontal = TRUE,
        xlab="Fork Length(mm)",col=c("salmon", 'gold'),
        main="Plot 8: Fork Length Distribution Harrison vs Osprey")
legend("bottomright", legend=c("Osprey", "Harrison"),
       inset=0.01, pch=15, col=c("gold", "salmon"),
       bty="n", bg="transparent", pt.cex=2) 

# extra step 4: discover how different era effect in age and length in Harrison
Era7780_HBT <- HarrisonBullTrout[HarrisonBullTrout$era == "1977-80",]
Era7780_HBT
Era9701_HBT <- HarrisonBullTrout[HarrisonBullTrout$era == "1997-01",]
Era9701_HBT
str(Era7780_HBT)
str(Era9701_HBT)
summary(Era7780_HBT)
summary(Era9701_HBT)
par(mfrow=c(2,2))
boxplot(Era9701_HBT$age, Era7780_HBT$age, 
        ylim=c(0,15), horizontal = TRUE, cex=3,
        xlab="Age(yrs)", col=c("lightblue", 'cadetblue'),
        main="Plot 7: Age Distribution 1977-80 vs 1997-01 in Harrison River")
legend("bottomright", legend=c("1977-80 Era", "1997-01 Era"),
       inset=c(-0.15,0.01), pch=15, col=c("cadetblue", "lightblue"),
       bty="n", bg="transparent", pt.cex=2) 
boxplot(Era9701_HBT$fl, Era7780_HBT$fl, 
        ylim=c(0,500), horizontal = TRUE,
        xlab="Fork Length(mm)",col=c("lightblue", 'cadetblue'),
        main="Plot 8: Length Distribution 1977-80 vs 1997-01 in Harrison River")
legend("topleft", legend=c("1977-80 Era", "1997-01 Era"),
       inset=0.01, pch=15, col=c("cadetblue", "lightblue"),
       bty="n", bg="transparent", pt.cex=2) 
    
# extra step 5: discover how different era effect in age and length in Osprey
Era7780_OBT <- OspreyBullTrout[OspreyBullTrout$era == "1977-80",]
Era7780_OBT
Era9701_OBT <- OspreyBullTrout[OspreyBullTrout$era == "1997-01",]
Era9701_OBT
str(Era7780_OBT)
str(Era9701_OBT)
summary(Era7780_OBT)
summary(Era9701_OBT)
par(mfrow=c(2,2))
boxplot(Era9701_OBT$age, Era7780_OBT$age, 
        ylim=c(0,15), horizontal = TRUE, cex=3,
        xlab="Age(yrs)", col=c("lightblue", 'cadetblue'),
        main="Plot 9: Age Distribution 1977-80 vs 1997-01 in Osprey River")
legend("topright", legend=c("1977-80 Era", "1997-01 Era"),
       inset=c(-0.15, 0.01), pch=15, col=c("cadetblue", "lightblue"),
       bty="n", bg="transparent", pt.cex=2) 
boxplot(Era9701_OBT$fl, Era7780_OBT$fl, 
        ylim=c(0,500), horizontal = TRUE,
        xlab="Fork Length(mm)",col=c("lightblue", 'cadetblue'),
        main="Plot 10: Length Distribution 1977-80 vs 1997-01 in Osprey River")
legend("topleft", legend=c("1977-80 Era", "1997-01 Era"),
       inset=0.01, pch=15, col=c("cadetblue", "lightblue"),
       bty="n", bg="transparent", pt.cex=2)

