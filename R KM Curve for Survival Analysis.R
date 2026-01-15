library(survival)

library(ggplot2)
install.packages("survminer")
library(survminer)
##########loading the dataset

pbcbase <- read_csv("C:/Users/danit/Downloads/pbcbase.csv")
View(pbcbase)
pbc <- pbcbase
pbc
####change the formationg of data YYYY-MM-DD,

pbc$datein  <- as.Date(pbc$datein,  "%d%b%Y")
pbc$dateout <- as.Date(pbc$dateout, "%d%b%Y")
View(pbc)

# Make treatment a factor with labels
pbc$treat <- factor(pbc$treat, levels=c(1,2), labels=c("Placebo","Active"))

###Question 1 Kaplan-Meier estimates of overall survival. 
##Kaplan-Meier estimates of survival probabilities 𝑆(𝑡) = Pr(𝑇 > 𝑡) using:##

pbc.km <- survfit(Surv(time, d) ~ 1, data = pbc)
summary(pbc.km, times=c(1,5))

####An alternative is to use######

s <- summary(pbc.km)

head(data.frame(
  time = s$time,
  n.risk = s$n.risk,
  n.event = s$n.event,
  n.censor = s$n.censor,
  surv = s$surv
), 5)

####Pots KM curve

plot(pbc.km,
     xlab="Time since randomisation (years)",
     ylab="Survival probability")



### Survminer plot with risk table + CI###

ggsurvplot(pbc.km, data=pbc, risk.table=TRUE, conf.int=TRUE)

####KM by treatment groups and summery difference 

pbc.km.trt <- survfit(Surv(time, d) ~ treat, data = pbc)

plot(pbc.km.trt, conf.int=TRUE, col=c("black","grey"),
     xlab="Time since randomisation (years)", ylab="Survival probability")
legend("topright", legend=levels(pbc$treat), col=c("black","grey"), lty=1)

# Nice plot
ggsurvplot(pbc.km.trt, data=pbc, risk.table=TRUE, conf.int=TRUE, pval=FALSE)

#### survival 1 t0 5 years by group####

summary(pbc.km.trt, times=c(1,5))

###4 Null hypothesis for compare two groups 

S
## P value 

pval <- 1 - pchisq(lr$chisq, df=length(lr$n)-1)
pval
###art B — Whitehall (timescale/origin + SBP groups)
##Load + fix dates (this solves your earlier subtraction error)

#### load the dateset 
whitehall <- read_csv("C:/Users/danit/Downloads/whitehall.csv")
View(whitehall)
whl <- whitehall
View(whl)

###
whl$timebth <- as.Date(whl$timebth, "%d%b%Y")
whl$timein  <- as.Date(whl$timein,  "%d%b%Y")
whl$timeout <- as.Date(whl$timeout, "%d%b%Y")

# SBP group labels
whl$sbpgrp <- factor(whl$sbpgrp,
                     levels=1:4,
                     labels=c("<120","120–<140","140–<160","≥160"))


##Time-in-study scale KM + deaths + total person-time (years) Deaths

sum(whl$chd == 1, na.rm=TRUE)

##Total person-time (years)
##Time-in-study in days → years:
  
whl$fu_days <- as.numeric(whl$timeout - whl$timein)
whl$fu_yrs  <- whl$fu_days / 365.25

sum(whl$fu_yrs, na.rm=TRUE)

###KM (time since entry)

whl.km.study <- survfit(Surv(fu_yrs, chd) ~ 1, data=whl)
ggsurvplot(whl.km.study, data=whl, risk.table=TRUE, conf.int=TRUE,
           xlab="Years since cohort entry", ylab="Survival probability")

####Probability beyond 10 years:
  
  summary(whl.km.study, times=10)

##### Q2: Compare timescale/origin approaches (i), (ii), (iii)
  ###(i) Origin = entry date; at risk from entry (time-in-study)
    
    S_i <- with(whl, Surv(fu_yrs, chd))
  km_i <- survfit(S_i ~ 1, data=whl)
  summary(km_i)
  
##(ii) Origin = birth; at risk from birth (ignores delayed entry) 
    
whl$age_exit_yrs <- as.numeric(whl$timeout - whl$timebth) / 365.25
  S_ii <- with(whl, Surv(age_exit_yrs, chd))
  km_ii <- survfit(S_ii ~ 1, data=whl)
  summary(km_ii)
  
  ####(iii) Origin = birth; at risk starts at entry (delayed entry) 
  
  whl$age_entry_yrs <- as.numeric(whl$timein  - whl$timebth) / 365.25
  whl$age_exit_yrs  <- as.numeric(whl$timeout - whl$timebth) / 365.25
  
  S_iii <- with(whl, Surv(age_entry_yrs, age_exit_yrs, chd))
  km_iii <- survfit(S_iii ~ 1, data=whl)
  
  ####Plot all three (separately, easiest to interpret)
  
  ggsurvplot(km_i, data=whl, conf.int=TRUE,
             xlab="Years since entry", ylab="Survival probability",
             title="(i) Time since entry (origin=entry)")
  
  ggsurvplot(km_ii, data=whl, conf.int=TRUE,
             xlab="Age (years)", ylab="Survival probability",
             title="(ii) Age scale but delayed entry ignored (not recommended)")
  
  ggsurvplot(km_iii, data=whl, conf.int=TRUE,
             xlab="Age (years)", ylab="Survival probability",
             title="(iii) Age scale with delayed entry (recommended)")
  
  ####(i) shows survival as a function of time since joining study.
  
  #####(ii) usually looks “too optimistic/biased” because it incorrectly includes time before observation (immortal time / left truncation ignored).
  
#######(iii) is the correct way to estimate survival on the age timescale because it includes delayed entry (only at risk after cohort entry).
  
 ### Q3: KM by SBP group + log-rank
  
##########KM curves (time-in-study)
  
  km_sbp <- survfit(Surv(fu_yrs, chd) ~ sbpgrp, data=whl)
  ggsurvplot(km_sbp, data=whl, risk.table=TRUE, conf.int=TRUE,
             xlab="Years since entry", ylab="Survival probability",
             legend.title="SBP at entry")
  
#######Log-rank test
######Null hypothesis: survival functions are the same across the 4 SBP groups.
  
  lr_sbp <- survdiff(Surv(fu_yrs, chd) ~ sbpgrp, data=whl)
  lr_sbp




