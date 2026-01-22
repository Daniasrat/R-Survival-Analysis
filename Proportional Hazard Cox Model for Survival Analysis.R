###Datasets required: rotterdam (part of the survival package)
##R packages required: survival, aod

library(survival)

install.packages("aod")
library(aod)

##### Loading 
data("rotterdam", package = "survival")
str(rotterdam)
rotterdam$dtime_yrs <- rotterdam$dtime / 365.25

##Question 1
#####events and follow-up range

sum(rotterdam$death == 1, na.rm = TRUE)
range(rotterdam$dtime_yrs, na.rm = TRUE)

# KM overall
km0 <- survfit(Surv(dtime_yrs, death) ~ 1, data = rotterdam)
summary(km0)
plot(km0, xlab="Years since surgery", ylab="Survival probability")
