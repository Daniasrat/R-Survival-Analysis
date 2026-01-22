library(survival)
library(cmprsk)
##### laoding the dataset####

aaatrial <- read_dta("C:/Users/danit/Downloads/aaatrial.dta")
View(aaatrial)
ls()
str(aaatrial)

###Question 1
##(a) How many deaths (all-cause), AAA-deaths, non-AAA deaths?
  # All-cause deaths

sum(aaatrial$alldeath == 1, na.rm = TRUE)

#### Cause breakdown using deathtype: 0=cens, 1=non-AAA, 2=AAA
table(aaatrial$deathtype)

####explicit counts:
sum(aaatrial$deathtype == 2, na.rm = TRUE)  # AAA deaths
sum(aaatrial$deathtype == 1, na.rm = TRUE)  # non-AAA deaths
sum(aaatrial$deathtype == 0, na.rm = TRUE)  # censored

#####(b) Create follow-up time in years since randomisation
######Dates are already Date, so subtraction works.

aaatrial$time <- as.numeric(aaatrial$timeout - aaatrial$dateran) / 365.25
summary(aaatrial$time)

#### Question 2 (All-cause mortality): KM + 5-year risk

km_all <- survfit(Surv(time, alldeath) ~ 1, data = aaatrial)

plot(km_all, xlab = "Years since randomisation", ylab = "Survival probability")

# Survival at 5 years:
s5 <- summary(km_all, times = 5)$surv
s5

# Risk (cumulative incidence of death from any cause) at 5 years:
risk5 <- 1 - s5
risk5

### Question 3: Surv() with competing risks (factor deathtype)

S_comp <- Surv(time = aaatrial$time, event = as.factor(aaatrial$deathtype))

head(S_comp, 20)


####Question 4: Aalen–Johansen cumulative incidence 

cinc <- survfit(Surv(time, as.factor(deathtype)) ~ 1, data = aaatrial)

plot(cinc, lwd = 2, col = c("black", "grey"))
legend("topleft", c("Non-AAA-death", "AAA-death"),
       lwd = 2, col = c("black", "grey"))


sum_c5 <- summary(cinc, times = 5)

# What columns you have:
sum_c5$pstate

colnames(sum_c5$pstate)


cif_nonAAA_5 <- sum_c5$pstate[ , "1"]
cif_AAA_5    <- sum_c5$pstate[ , "2"]

cif_nonAAA_5
cif_AAA_5

###Question 5: KM by group + 5-year risks + log-rank

aaatrial$group_f <- factor(aaatrial$group, levels = c(0,1), labels = c("Control","Invited"))

km_group <- survfit(Surv(time, alldeath) ~ group_f, data = aaatrial)

plot(km_group, col=c("black","grey"), lwd=2,
     xlab="Years since randomisation", ylab="Survival probability")
legend("bottomleft", levels(aaatrial$group_f), col=c("black","grey"), lwd=2)

### 5-year risk by group:
sum_g5 <- summary(km_group, times=5)
sum_g5$strata
sum_g5$surv
risk_by_group_5 <- 1 - sum_g5$surv
risk_by_group_5

# Log-rank test:
lr <- survdiff(Surv(time, alldeath) ~ group_f, data = aaatrial)
lr
p_lr <- 1 - pchisq(lr$chisq, df = length(lr$n)-1)
p_lr

#### Question 6: CIFs by group + Gray’s test

####(a) Aalen–Johansen CIF curves by group

cinc_group <- survfit(Surv(time, as.factor(deathtype)) ~ group_f, data = aaatrial)

plot(cinc_group, lwd=2, lty=c(1,2,1,2), col=c("black","black","grey","grey"))
legend("topleft",
       c("Control, Non-AAA-death","Invited, Non-AAA-death",
         "Control, AAA-death","Invited, AAA-death"),
       lwd=2, lty=c(1,2,1,2), col=c("black","black","grey","grey"))

####(b) CIFs at 5 years in each group

sum_cg5 <- summary(cinc_group, times = 5)

# inspect names first
sum_cg5$strata
colnames(sum_cg5$pstate)

# If pstate columns are "0","1","2":

pstate5 <- sum_cg5$pstate
pstate5

# CIFs are columns "1" and "2" (non-AAA and AAA)
cif_nonAAA_5_bygroup <- pstate5[, "1"]
cif_AAA_5_bygroup    <- pstate5[, "2"]

cif_nonAAA_5_bygroup
cif_AAA_5_bygroup

#### (c) Gray’s test (cmprsk)
gr <- cuminc(ftime = aaatrial$time,
             fstatus = aaatrial$deathtype,
             group = aaatrial$group)
gr

####### Question 7: Why Kaplan–Meier is wrong for AAA-death CIF

###(a) What Surv(time, aaadeath) does

S_aaa_simple <- Surv(time = aaatrial$time, event = aaatrial$aaadeath)
head(S_aaa_simple, 20)


#### (b) KM-based “CIF” for AAA death (actually 1-KM survival treating other deaths as censoring)

cinc_km <- survfit(Surv(time, aaadeath) ~ 1, data = aaatrial)

plot(cinc_km, fun="event", lwd=2,
     xlab="Years since randomisation", ylab="KM estimate of P(AAA-death by t)")


##Compare with Aalen–Johansen AAA CIF from Question 4:
## AAA CIF at 5 years from AJ
  
sum_c5 <- summary(cinc, times=5)
aj_aaa_5 <- sum_c5$pstate[, "2"]

## KM 'AAA CIF' at 5 years
km_aaa_5 <- summary(cinc_km, times=5)$surv
km_aaa_risk5 <- 1 - km_aaa_5

aj_aaa_5
km_aaa_risk5



















