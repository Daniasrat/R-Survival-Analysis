library(survival)
install.packages("mexhaz")
install.packages("flexsurv")
library(mexhaz)
library(flexsurv)
#### Loading the dataset####

pbcbase <- read_csv("C:/Users/danit/Downloads/pbcbase.csv")
View(pbcbase)
pbc <- pbcbase
pbc
####change the formationg of data YYYY-MM-DD,

pbc$datein  <- as.Date(pbc$datein,  "%d%b%Y")
pbc$dateout <- as.Date(pbc$dateout, "%d%b%Y")
View(pbc)

###counting death and censoring###
table(pbc$d)
#### earliest date of entry/exit####
min(pbc$datein); max(pbc$datein)
min(pbc$dateout); max(pbc$dateout)
####what happens for Id number 5 and 23 

pbc[pbc$id %in% c(5, 23),
    c("id", "datein", "dateout", "d", "time")]

####calculate the time of follow up from datein to dateout####
pbc$days_in_study <- as.numeric(pbc$dateout - pbc$datein)
days_in_study

pbc$years_from_dates <- pbc$days_in_study / 365.25



##5 survival mean##
Surv(time = pbc$time, event = pbc$d)
## other way of the function of survival##
Surv(time=as.numeric(pbc$dateout),event=pbc$d,origin=as.numeric(pbc$datein))

#####Part B (surv_data_practical1): Exponential model + MLE in R
###Under an exponential distribution with constant hazard λ:
 ###load the dataset 
surv_data_practical1 <- read_csv("C:/Users/danit/Downloads/surv_data_practical1.csv")
View(surv_data_practical1)
dta <- surv_data_practical1
View(dta)

lambda_hat <- sum(dta$d) / sum(dta$survtimes)
lambda_hat
##λ is a rate per unit time.
##Mean survival time for exponential is 
1/lambda_hat


#### load the dateset for Part C
whitehall <- read_csv("C:/Users/danit/Downloads/whitehall.csv")
View(whitehall)
whl <- whitehall
View(whl)
####change the date from string to real date
whl$timebth <- as.Date(whl$timebth, format = "%d%b%Y")
whl$timein  <- as.Date(whl$timein,  format = "%d%b%Y")
whl$timeout <- as.Date(whl$timeout, format = "%d%b%Y")


max(whl$timeout)
table(whl$timeout)[as.character(max(whl$timeout))]

#### latest and follow up

latest <- max(whl$timeout, na.rm = TRUE)
latest
sum(whl$timeout == latest, na.rm = TRUE)

#### 3) Create start/stop times (delayed entry) and then use Surv()
##If time origin = birth (age timescale)
##Compute age at entry/exit in days (or divide by 365.25 for years):

whl$age_entry_days <- as.numeric(whl$timein  - whl$timebth)
whl$age_exit_days  <- as.numeric(whl$timeout - whl$timebth)

# start-stop Surv object

S_age <- with(whl, Surv(age_entry_days, age_exit_days, chd))
S_age[1:10]

## Age in year##
whl$age_entry_yrs <- whl$age_entry_days / 365.25
whl$age_exit_yrs  <- whl$age_exit_days  / 365.25

S_age_yrs <- with(whl, Surv(age_entry_yrs, age_exit_yrs, chd))


 







