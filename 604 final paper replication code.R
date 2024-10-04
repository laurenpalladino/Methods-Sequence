#Lauren Palladino
#POL 604 final paper

library(haven)
library(survey)
library(tidyr)
library(stargazer)
library(effects)
library(ggplot2)
library(pscl)
source("http://www.michaelperess.com/r0.r")
data604 <- read_dta("Downloads/Tess_Covid_update_wvars.dta")
attach(data604)

#replicate table 1 no weights
logit1<-logit0(facemasks_dichotomous~Treatment + negstereotype_endorsement + Treatment*negstereotype_endorsement)
logit2<-logit0(individualrights_dichotomous~Treatment + negstereotype_endorsement + Treatment*negstereotype_endorsement)
logit3<-logit0(visitparks_dichotomous~Treatment + negstereotype_endorsement + Treatment*negstereotype_endorsement)
logit4<-logit0(Follow_Blacks_dichotomous~Treatment + negstereotype_endorsement + Treatment*negstereotype_endorsement)
print.models(list("Masks"=logit1,"Rights"=logit2, "Parks"=logit3, "Black"=logit4))

#add in survey weights + replicate
svydata <-  svydesign(id = ~ CaseId , weights = ~ WEIGHT, data = data604)
logit1w<-svyglm(facemasks_dichotomous~Treatment + negstereotype_endorsement + Treatment*negstereotype_endorsement,
             data=data604, family=binomial, design = svydata)
summary(logit1w)
logit2w<-svyglm(individualrights_dichotomous~Treatment + negstereotype_endorsement + Treatment*negstereotype_endorsement,
                data=data604, family=binomial, design = svydata)
summary(logit2w)
logit3w<-svyglm(visitparks_dichotomous~Treatment + negstereotype_endorsement + Treatment*negstereotype_endorsement,
                data=data604, family=binomial, design = svydata)
summary(logit3w)
logit4w<-svyglm(Follow_Blacks_dichotomous~Treatment + negstereotype_endorsement + Treatment*negstereotype_endorsement,
                data=data604, family=binomial, design = svydata)
summary(logit4w)
stargazer(logit1w, logit2w, logit3w, logit4w)

#logit unweighted MEs
f1<-facemasks_dichotomous~Treatment + negstereotype_endorsement + Treatment*negstereotype_endorsement
X1 <- model.X(f1)
ME1 <- delta.method(logit.me1,logit1$coef,logit1$V,X=X1)
ME1$theta <- ME1$est
ME1$ivs <- logit1$ivs[2:length(logit1$ivs)]
print.model(ME1)

f2<-individualrights_dichotomous~Treatment + negstereotype_endorsement + Treatment*negstereotype_endorsement
X2 <- model.X(f2)
ME2 <- delta.method(logit.me1,logit2$coef,logit2$V,X=X1)
ME2$theta <- ME2$est
ME2$ivs <- logit2$ivs[2:length(logit2$ivs)]
print.model(ME2)

f3<-visitparks_dichotomous~Treatment + negstereotype_endorsement + Treatment*negstereotype_endorsement
X3 <- model.X(f3)
ME3 <- delta.method(logit.me1,logit3$coef,logit3$V,X=X1)
ME3$theta <- ME3$est
ME3$ivs <- logit3$ivs[2:length(logit3$ivs)]
print.model(ME3)

f4<-Follow_Blacks_dichotomous~Treatment + negstereotype_endorsement + Treatment*negstereotype_endorsement
X4 <- model.X(f4)
ME4 <- delta.method(logit.me1,logit4$coef,logit4$V,X=X1)
ME4$theta <- ME4$est
ME4$ivs <- logit4$ivs[2:length(logit4$ivs)]
print.model(ME4)
print.models(list("Masks"=ME1,"Rights"=ME2, "Parks"=ME3, "Black"=ME4))

#breaking down by levels of stereotype endorsement
interact <- effect('Treatment*negstereotype_endorsement', logit1w, se=TRUE, na.rm=TRUE)
interactdf<-as.data.frame(interact, na.rm=TRUE)
interactdf$Treatment <- factor(interactdf$Treatment,  
                               level=c(0, 1),
                               labels=c("Control", "Treatment"))
interactdf$negstereotype_endorsement <- factor(interactdf$negstereotype_endorsement,
                              level=c(0, 0.5, 1),
                              labels=c("Low", "Medium", "High"))   
plot<-ggplot(data=drop_na(interactdf), aes(x=Treatment, y=fit, group=negstereotype_endorsement))+
  geom_line(aes(color=negstereotype_endorsement))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=negstereotype_endorsement),alpha=.2)+
  ylab("Wearing Masks is Unimportant")+
  xlab("Treatment")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
plot

interact <- effect('Treatment*negstereotype_endorsement', logit2w, se=TRUE, na.rm=TRUE)
interactdf<-as.data.frame(interact, na.rm=TRUE)
interactdf$Treatment <- factor(interactdf$Treatment,  
                               level=c(0, 1),
                               labels=c("Control", "Treatment"))
interactdf$negstereotype_endorsement <- factor(interactdf$negstereotype_endorsement,
                                               level=c(0, 0.5, 1),
                                               labels=c("Low", "Medium", "High"))   
plot<-ggplot(data=drop_na(interactdf), aes(x=Treatment, y=fit, group=negstereotype_endorsement))+
  geom_line(aes(color=negstereotype_endorsement))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=negstereotype_endorsement),alpha=.2)+
  ylab("Stay-In-Place Violates Rights")+
  xlab("Treatment")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
plot

interact <- effect('Treatment*negstereotype_endorsement', logit3w, se=TRUE)
interactdf<-as.data.frame(interact)
interactdf$Treatment <- factor(interactdf$Treatment,  
                               level=c(0, 1),
                               labels=c("Control", "Treatment"))
interactdf$negstereotype_endorsement <- factor(interactdf$negstereotype_endorsement,
                                               level=c(0, 0.5, 1),
                                               labels=c("Low", "Medium", "High"))   
plot<-ggplot(data=drop_na(interactdf), aes(x=Treatment, y=fit, group=negstereotype_endorsement))+
  geom_line(aes(color=negstereotype_endorsement))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=negstereotype_endorsement),alpha=.2)+
  ylab("Parks Should be Open w/out Restrictions")+
  xlab("Treatment")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
plot

interact <- effect('Treatment*negstereotype_endorsement', logit4w, se=TRUE)
interactdf<-as.data.frame(interact)
interactdf$Treatment <- factor(interactdf$Treatment,  
                               level=c(0, 1),
                               labels=c("Control", "Treatment"))
interactdf$negstereotype_endorsement <- factor(interactdf$negstereotype_endorsement,
                                               level=c(0, 0.5, 1),
                                               labels=c("Low", "Medium", "High"))   
plot<-ggplot(data=drop_na(interactdf), aes(x=Treatment, y=fit, group=negstereotype_endorsement))+
  geom_line(aes(color=negstereotype_endorsement))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=negstereotype_endorsement),alpha=.2)+
  ylab("Black People Don't Follow Social Distancing")+
  xlab("Treatment")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
plot

#extension: negative binomial model
covidbehav<-facemasks_dichotomous+individualrights_dichotomous+visitparks_dichotomous+Follow_Blacks_dichotomous
summary(covidbehav)
hist(covidbehav)

negbin1<-negbin0(covidbehav~Treatment+negstereotype_endorsement+Treatment*negstereotype_endorsement)
print.model(negbin1)

#rerunning negbin in baseR to make interaction plots
library(MASS)
negbin2<-glm.nb(covidbehav~Treatment+negstereotype_endorsement+Treatment*negstereotype_endorsement)
summary(negbin2)

interact <- effect('Treatment*negstereotype_endorsement', negbin2, se=TRUE)
interactdf<-as.data.frame(interact)
interactdf$Treatment <- factor(interactdf$Treatment,  
                               level=c(0, 1),
                               labels=c("Control", "Treatment"))
interactdf$negstereotype_endorsement <- factor(interactdf$negstereotype_endorsement,
                                               level=c(0, 0.5, 1),
                                               labels=c("Low", "Medium", "High"))   
plot<-ggplot(data=drop_na(interactdf), aes(x=Treatment, y=fit, group=negstereotype_endorsement))+
  geom_line(aes(color=negstereotype_endorsement))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=negstereotype_endorsement),alpha=.2)+
  ylab("Count of COVID attitudes")+
  xlab("Treatment")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
plot
