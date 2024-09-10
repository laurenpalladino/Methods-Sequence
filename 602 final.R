#Q2 chi square

sports <- matrix(c(60, 42.5, 62.5, 90, 55, 30, 114, 122.5, 130,
                   12, 20, 15, 24, 10, 12.5), nrow=5, byrow=TRUE,
                 dimnames = list(c("Baseball", "Basketball", "Football", "Ice Hockey", "Soccer"),
                                 c("Democrats", "Independents", "Republicans")))

sports <- as.table(sports)
sports
chisq_sports <- chisq.test(sports, correct = FALSE)
chisq_sports
chisq_sports$observed
#validating residuals table
chisq_sports$expected

#calculating the p value from Q4
pt(2.42, 79, lower.tail = FALSE)

#Q5 biden/harris ANES memo
#downloading the packages needed
library(haven)
library(dplyr)
library(tidyr)
library(stargazer)
ANES <- read_dta("Downloads/ANES2020sample.dta")
View(ANES)
attach(ANES)

#clean biden thermometer
V201151[V201151 < 0] <- NA
V201151[V201151 > 100] <- NA
summary(V201151)

#clean harris thermometer
V201153[V201153 < 0] <- NA
V201153[V201153 > 100] <- NA
summary(V201153)

#clean democrats thermometer
V201156[V201156 < 0] <- NA
V201156[V201156 > 100] <- NA
summary(V201156)

#subsetting strong vs weak democrats
strongdem=subset(ANES, V201156 >=75)
weakdem=subset(ANES, V201156 <75)

#clean gender variable
V201600[V201600 < 0 ] <- NA
V201600[V201600 > 2] <- NA
summary(V201600)
gender <-(V201600)
men=subset(ANES, gender == 1)
women=subset(ANES, gender == 2)

#Test 1-do strong democrats prever biden over harris?
#H0: Strong dems rate biden and harris equally
#H1: Strong dems do not rate biden and harris equally (use two sided)
t.test(strongdem$V201151, strongdem$V201153, data=ANES, var.equal = FALSE, alternative = "two.sided")

#making a box plot of results
boxplot(strongdem$V201151, strongdem$V201153, data=ANES, outline=FALSE,
        names=c("Feelings for Biden", "Feelings for Harris"))
mtext("Feeling Thermometers for Biden/Harris",side=2,line=4,cex=1,outer=F)

#adding mean points to plot
points(mean(strongdem$V201151), pch=8, col="red3", cex=2, lwd=3)
points(x=2, mean(strongdem$V201153), pch=8, col="red3", cex=2, lwd=3)
legend("bottomleft", bg="transparent", legend=c("Mean"),
       col ="red3", pch=8, cex=1, box.lty=0)

#Test 2-do weak democrats prefer biden over harris?
#H0: Weak dems rate biden and harris equally
#H1: Weak dems do not rate biden and harris equally (use two sided)
t.test(weakdem$V201151, weakdem$V201153, data=ANES, var.equal = FALSE, alternative = "two.sided")

#making a box plot of the results
boxplot(weakdem$V201151, weakdem$V201153, data=ANES, outline=FALSE,
        names=c("Feelings for Biden", "Feelings for Harris"))
mtext("Feeling Thermometers for Biden/Harris",side=2,line=4,cex=1,outer=F)

#adding mean points to the plot
points(mean(weakdem$V201151), pch=8, col="red3", cex=2, lwd=3)
points(x=2, mean(weakdem$V201153), pch=8, col="red3", cex=2, lwd=3)
legend("topleft", bg="transparent", legend=c("Mean"),
       col ="red3", pch=8, cex=1, box.lty=0)

#OLS model 1- does the feeling thermometer for the dems predict feeling thermometer for Kamala?
dempt.kamala <- lm(V201153~V201156, data=ANES)
summary(dempt.kamala)

#export as LaTeX table
stargazer(dempt.kamala)

#making scatter
par(mfrow=c(1,1),oma=c(10,10,3,3),mar=c(1,1,1,1))
plot(V201156, V201153,type="n",axes=F,ylab="",xlab="",xlim=c(0,100),ylim=c(0,100))
points(V201156,V201153, pch=21,bg="skyblue",col="blue",cex=1.5)
axis(1,at = seq(0,100,20), label = seq(0,100,20), mgp = c(.8,2,1), cex.axis=1)
axis(2,at = seq(0,100,20), label = seq(0,100,20), mgp = c(.8,2,1), cex.axis=1,las=2)
mtext("Feeling Thermometer for the Democrats",side=1,line=4,cex=1,outer=F)
mtext("Feeling Thermometer for Kamala Harris",side=2,line=4,cex=1,outer=F)

#adding regression line
abline(dempt.kamala$coef[1],dempt.kamala$coef[2],col="black",lwd=3)

#OLS model 2- does the feeling thermometer for the dems predict feeling thermometer for Biden?
dempt.biden <- lm(V201151~V201156, data=ANES)
summary(dempt.biden)

#export as LaTeX table
stargazer(dempt.biden)

#making scatter
par(mfrow=c(1,1),oma=c(10,10,3,3),mar=c(1,1,1,1))
plot(V201156, V201151,type="n",axes=F,ylab="",xlab="",xlim=c(0,100),ylim=c(0,100))
points(V201156,V201151, pch=21,bg="skyblue",col="blue",cex=1.5)
axis(1,at = seq(0,100,20), label = seq(0,100,20), mgp = c(.8,2,1), cex.axis=1)
axis(2,at = seq(0,100,20), label = seq(0,100,20), mgp = c(.8,2,1), cex.axis=1,las=2)
mtext("Feeling Thermometer for the Democrats",side=1,line=4,cex=1,outer=F)
mtext("Feeling Thermometer for Joe Biden",side=2,line=4,cex=1,outer=F)

#adding regression line
abline(dempt.biden$coef[1],dempt.biden$coef[2],col="black",lwd=3)

#OLS model 3: does the feeling thermometer for the dems*interacted w gender predict feeling thermometer for Kamala?
dempt.genderkamala <- lm(V201153~V201156*gender, data=ANES)
summary(dempt.genderkamala)

#export as LaTeX table
stargazer(dempt.genderkamala)

#making regression lines for men and women
demft=seq(0, 100, 1)
pred.men=
  dempt.genderkamala$coef[1]+
  dempt.genderkamala$coef[2]*demft+
  dempt.genderkamala$coef[3]*1+
  dempt.genderkamala$coef[4]*demft*1

pred.women=
  dempt.genderkamala$coef[1]+
  dempt.genderkamala$coef[2]*demft+
  dempt.genderkamala$coef[3]*0+
  dempt.genderkamala$coef[4]*demft*0

#making scatter (women are pink points, men are blue points)
par(mfrow=c(1,1),oma=c(10,10,3,3),mar=c(1,1,1,1), xpd=TRUE)
plot(V201156, V201153,type="n",axes=F,ylab="",xlab="",xlim=c(0,100),ylim=c(0,100))
points(V201156,V201153, pch=21, bg=ifelse(gender==1,"steelblue2", "pink2"),cex=1.5)
axis(1,at = seq(0,100,20), label = seq(0,100,20), mgp = c(.8,2,1), cex.axis=1)
axis(2,at = seq(0,100,20), label = seq(0,100,20), mgp = c(.8,2,1), cex.axis=1,las=2)
mtext("Feeling Thermometer for the Democrats",side=1,line=4,cex=1,outer=F)
mtext("Feeling Thermometer for Kamala Harris",side=2,line=4,cex=1,outer=F)

#adding lines
lines(pred.men,lwd=3,col="steelblue2", lty=1)
lines(pred.women,lwd=3,col="pink2", lty=1)

#add a legend
legend("topleft", bg="transparent", legend=c("Men", "Women"), lwd=3,
       col =c("steelblue2", "pink2"),lty=1, cex=0.8,box.lty=1)

#OLS model 4: does the feeling thermometer for the dems*interacted w gender predict feeling thermometer for Biden?
dempt.genderbiden <- lm(V201151~V201156*gender, data=ANES)
summary(dempt.genderbiden)

#export as LaTeX table
stargazer(dempt.genderbiden)

#making regression lines for men and women
pred.men1=
  dempt.genderbiden$coef[1]+
  dempt.genderbiden$coef[2]*demft+
  dempt.genderbiden$coef[3]*1+
  dempt.genderbiden$coef[4]*demft*1

pred.women1=
  dempt.genderbiden$coef[1]+
  dempt.genderbiden$coef[2]*demft+
  dempt.genderbiden$coef[3]*0+
  dempt.genderbiden$coef[4]*demft*0

#making scatter
par(mfrow=c(1,1),oma=c(10,10,3,3),mar=c(1,1,1,1))
plot(V201156, V201151,type="n",axes=F,ylab="",xlab="",xlim=c(0,100),ylim=c(0,100))
points(V201156,V201151, pch=21,bg=ifelse(gender==1,"steelblue2", "pink2"), cex=1.5)
axis(1,at = seq(0,100,20), label = seq(0,100,20), mgp = c(.8,2,1), cex.axis=1)
axis(2,at = seq(0,100,20), label = seq(0,100,20), mgp = c(.8,2,1), cex.axis=1,las=2)
mtext("Feeling Thermometer for the Democrats",side=1,line=4,cex=1,outer=F)
mtext("Feeling Thermometer for Joe Biden",side=2,line=4,cex=1,outer=F)

#add lines
lines(pred.men1,lwd=3,col="steelblue2", lty=1)
lines(pred.women1,lwd=3,col="pink2", lty=1)

#add a legend
legend("topleft", bg="transparent", legend=c("Men", "Women"), lwd=3,
       col =c("steelblue2", "pink2"), lty=1, cex=0.8,box.lty=1)
