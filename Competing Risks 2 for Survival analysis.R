
###Competing Risks 2 (cause-specific Cox vs Fine–Gray). 
###0) Load + create time (years since randomisation)

library(survival)
library(cmprsk)
View(aaatrial)
str(aaatrial)

aaatrial$time <- as.numeric(aaatrial$timeout - aaatrial$dateran) / 365.25

aaatrial$group_f <- factor(aaatrial$group, levels=c(0,1), labels=c("Control","Invited"))

table(aaatrial$deathtype)

summary(aaatrial$time)

###Question 1 — Cause-specific Cox models (group only)

###(a) Fit cause-specific model with competing risks via factor outcome
cox.cs <- coxph(Surv(time, as.factor(deathtype)) ~ group, data = aaatrial, id = id)
summary(cox.cs)

###(b) Equivalent separate fits
cox.AAA    <- coxph(Surv(time, deathtype == 2) ~ group, data = aaatrial)
cox.nonAAA <- coxph(Surv(time, deathtype == 1) ~ group, data = aaatrial)

summary(cox.AAA)
summary(cox.nonAAA)

#####Question 2 — CIFs from cause-specific Cox models (group 0 vs 1)
cinc.cs <- survfit(cox.cs, newdata = data.frame(group = c(0,1)))
summary(cinc.cs, times = c(1,5,10))

####Plot
plot(cinc.cs, lwd=2, lty=c(1,2,1,2), col=c("black","black","grey","grey"))
legend("topleft",
       c("Control, Non-AAA-death","Invited, Non-AAA-death",
         "Control, AAA-death","Invited, AAA-death"),
       lwd=2, lty=c(1,2,1,2), col=c("black","black","grey","grey"))

###Get CIF at time 5 (how to extract)
s5 <- summary(cinc.cs, times=5)

s5$strata
colnames(s5$pstate)
s5$pstate


##Usually columns are "0", "1", "2":
###CIF non-AAA at 5 years = pstate column "1"
###CIF AAA at 5 years = pstate column "2"

cif_nonAAA_5 <- s5$pstate[, "1"]
cif_AAA_5    <- s5$pstate[, "2"]

cif_nonAAA_5
cif_AAA_5

#####Question 3 — Add age at randomisation + fit cause-specific Cox
##Create age (years)
aaatrial$age <- as.numeric(aaatrial$dateran - aaatrial$dob) / 365.25
summary(aaatrial$age)

##Fit models
cox.cs.age <- coxph(Surv(time, as.factor(deathtype)) ~ group + age, data = aaatrial, id = id)
summary(cox.cs.age)

cox.AAA.age    <- coxph(Surv(time, deathtype == 2) ~ group + age, data = aaatrial)
cox.nonAAA.age <- coxph(Surv(time, deathtype == 1) ~ group + age, data = aaatrial)

summary(cox.AAA.age)
summary(cox.nonAAA.age)

####Question 4 — CIF at time 5 for four individuals (group × age)
####Fit CIFs from the cause-specific model with age:
  
cinc.cs.age <- survfit(cox.cs.age,
                         newdata = data.frame(group = c(0,1,0,1),
                                              age   = c(65,65,70,70)))
s5_ind <- summary(cinc.cs.age, times=5)

s5_ind$strata
colnames(s5_ind$pstate)
s5_ind$pstate

###Extract:
  
cif_nonAAA_5_ind <- s5_ind$pstate[, "1"]
cif_AAA_5_ind    <- s5_ind$pstate[, "2"]
cif_nonAAA_5_ind
cif_AAA_5_ind


##Then label them:
  
  people <- data.frame(
    person = 1:4,
    group  = c("Control","Invited","Control","Invited"),
    age    = c(65,65,70,70),
    CIF_nonAAA_5 = cif_nonAAA_5_ind,
    CIF_AAA_5    = cif_AAA_5_ind
  )
people










