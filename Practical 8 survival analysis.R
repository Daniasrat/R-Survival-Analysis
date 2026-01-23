
###Practical 8 (time-dependent variables) using jasa and jasa1.

### Setup + load data

library(survival)
library(tidyverse)
getwd()              # check where R is looking
list.files()         # confirm files are there

load("jasa.RData")
load("jasa1.RData")
Jasa <- jasa1
view(jasa)
data("jasa",  package = "survival")
data("jasa1", package = "survival")
ls()
str(jasa)
str(jasa1)
######Question 1 — What’s in the data?
##Rows + number of individuals

nrow(jasa); nrow(jasa1)

n_distinct(jasa$id)      # if id exists in jasa; if not, use row count as individuals
n_distinct(jasa1$id)

##If jasa has no id column, it’s one row per person so:
  
nrow(jasa)   # number of individuals

###How many received a transplant?, In jasa, transplant means “ever transplanted” (0/1):
 table(jasa$transplant) 
table(jasa$transplant)
sum(jasa$transplant==1, na.rm = TRUE)

####Look at individuals 6–10 in both datasets

jasa[6:10, c("futime","fustat","transplant","wait.time","age","surgery","tx.date")]

jasa1 %>% 
  filter(id %in% 6:10) %>% 
  arrange(id, start) %>% 
  select(id, start, stop, event, transplant)

####Question 2 — Naive KM/Cox by “ever transplanted” (and what’s wrong)
##Naive KM by “ever transplanted”


km_naive <- survfit(Surv(futime, fustat) ~ transplant, data=jasa)

plot(km_naive, col=c("black","red"), lwd=2,
     xlab="Days since acceptance", ylab="Survival probability")
legend("bottomleft", c("Never transplanted","Ever transplanted"),
       col=c("black","red"), lwd=2)

### Naive Cox
cox_naive <- coxph(Surv(futime, fustat) ~ transplant, data=jasa)
summary(cox_naive)


####How long are individuals 7 and 10 immortal?”

jasa[c(7,10), c("wait.time","futime","fustat","transplant")]


###Question 3 — Correct Cox with time-dependent transplant (using jasa1)

###where transplant(t)=0 before transplant and 1 after transplant.
## Fit Cox using start–stop format

cox_td <- coxph(Surv(start, stop, event) ~ transplant, data=jasa1)
summary(cox_td)

######Question 4 — Effect depends on time since transplant

### Step 1: Split by unique event times

cuts <- sort(unique(jasa1$stop[jasa1$event == 1]))
cuts <- cuts[cuts < max(jasa1$stop)]  # avoid edge issues

j1s <- survSplit(Surv(start, stop, event) ~ ., data=jasa1,
                 cut=cuts, start="start", end="stop", event="event")

###Step 2: Create time-since-transplant (0 pre-tx)

  j1s <- j1s %>%
  group_by(id) %>%
  mutate(tx_time = min(start[transplant == 1], na.rm=TRUE)) %>%
  ungroup()

# If never transplanted, tx_time becomes Inf; set it to NA
  
j1s$tx_time[is.infinite(j1s$tx_time)] <- NA

j1s <- j1s %>%
  mutate(tsince_tx = ifelse(transplant == 1, stop - tx_time, 0))

###Step 3: Categorise time since transplant (0–90, 91–186, >186)

j1s <- j1s %>%
  mutate(tsince_cat = case_when(
    transplant == 0 ~ "pre",
    tsince_tx <= 90 ~ "0-90",
    tsince_tx <= 186 ~ "91-186",
    TRUE ~ ">186"
  )) %>%
  mutate(tsince_cat = factor(tsince_cat, levels=c("pre","0-90","91-186",">186")))

#### Step 4: Fit Cox with interaction (transplant × time-since category)
##A simple way is to include the category directly (it already encodes pre vs post periods):
  
cox_ts <- coxph(Surv(start, stop, event) ~ tsince_cat, data=j1s)
summary(cox_ts)

#####Question 5 — Landmarking at 90 days (then try 30)

## Landmark at 90 days
L <- 90

j_land <- jasa %>%
  filter(futime > L) %>%                 # alive at landmark (not dead/censored before L)
  mutate(
    time_L = futime - L,                 # reset time origin to day L
    status_L = fustat,                   # event indicator unchanged
    tx_before_L = ifelse(wait.time <= L & transplant == 1, 1, 0)
  )

###KM + Cox at landmark:
  
  km_L <- survfit(Surv(time_L, status_L) ~ tx_before_L, data=j_land)
plot(km_L, col=c("black","red"), lwd=2,
     xlab=paste0("Days since landmark (",L,")"),
     ylab="Survival probability")

cox_L <- coxph(Surv(time_L, status_L) ~ tx_before_L, data=j_land)
summary(cox_L)

#####Question 6 — Post-transplant survival only
  
  post <- jasa %>%
  filter(transplant == 1) %>%
  mutate(
    post_time = futime - wait.time,   # time from transplant to death/censor
    post_event = fustat
  ) %>%
  filter(post_time >= 0)

###KM post-transplant survival; survival at 30 days and 1 year
  
km_post <- survfit(Surv(post_time, post_event) ~ 1, data=post)

plot(km_post, xlab="Days since transplant", ylab="Post-transplant survival")

summary(km_post, times=c(30, 365))$surv

###Cox model with age, surgery, calendar year of transplant
##Extract year from tx.date:
  
  post <- post %>%
  mutate(tx_year = as.integer(format(tx.date, "%Y")))

###Fit:
  
  cox_post <- coxph(Surv(post_time, post_event) ~ age + surgery + tx_year, data=post)
summary(cox_post)

### Assumptions Schoenfeld:
  
sch <- cox.zph(cox_post)
sch
plot(sch)

### Martingale residuals for age functional form:
  
mg <- resid(cox_post, type="martingale")
plot(post$age, mg, xlab="Age", ylab="Martingale residuals")
lines(lowess(post$age, mg), col="red", lwd=2)
abline(h=0, col="grey", lwd=2)




