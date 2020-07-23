colorectalDataset <- read.table("Colorectal_Cancer.csv", sep=",", header=T)

colorectalDataset$censor <- with(colorectalDataset, ifelse(status=='A', 0, 1))


#categorical variable

censor.freq = table(colorectalDataset$censor)
censor.prop = prop.table(censor.freq)
censor.table = as.data.frame(cbind(censor.freq, censor.prop))
colnames(censor.table) = c("Frequency", "Percent")
censor.table


group.freq = table(colorectalDataset$number)
group.prop = prop.table(group.freq)
group.table = as.data.frame(cbind(group.freq, group.prop))
colnames(group.table) = c("Frequency", "Percent")
group.table


#continuous variables

months.summary = summary(colorectalDataset$months)
months.summary


table.censor.by.group = table(colorectalDataset$censor, colorectalDataset$number)
table.censor.by.group.freq.table = cbind(table.censor.by.group, margin.table(table.censor.by.group, 1))
table.censor.by.group.freq.table = rbind(table.censor.by.group.freq.table, c(margin.table(table.censor.by.group, 2), 214))

table.censor.by.group.freq.table

library(MASS)
chisq.test(table.censor.by.group)

library(survival)
colorectalDataset$SurvObj = with(colorectalDataset, Surv(months, censor == 1))

#note that the SurvObj column is now in place
head(colorectalDataset)

###OVERALL SURVIVAL FOR COMBINED GROUPS/ENTIRE DATE SET
km.by.combine = survfit(formula = Surv(months, censor) ~ 1, data = colorectalDataset , conf.type = "log-log")

#plot(km.by.combine, xlab="months", ylab = "Survival Probability", title = "Combined Kaplan-Meier Curve for Colorectal Cancer")

library(survminer)

ggsurvplot(
  fit = survfit(Surv(months, censor) ~ 1, data = colorectalDataset), 
  xlab = "Time(Months)", 
  ylab = "Overall colorectal cancer survival probability",
  title = "Figure 1: Combined Kaplan-Meier Curve for Colorectal Cancer (n=77)",
  subtitle = "Note: Shaded areas are 95% confidence interval"
) 


km.by.combine

summary(km.by.combine)

#### FIVE YEAR/60 MONTH SURVIVAL PROBABILITY
summary(survfit(Surv(months, censor) ~ 1, data = colorectalDataset), times = 60)



km.by.group = survfit(formula = Surv(months, censor) ~ strata(number), data = colorectalDataset, conf.type = "log-log")

plot(km.by.group, lty=c(1,3), xlab="Months", ylab = "Survival Probability")
legend(7, 0.3, c("delayed", "not delayed"), lty=c(1,3))

ggsurvplot(
  fit = survfit(Surv(months, censor) ~ number, data = colorectalDataset), 
  xlab = "Time (Months)", 
  ylab = "Overall colorectal cancer survival probability",
  legend.title = "",
  legend.labs=c(">1 PM", "1 PM"),
  title = "Figure 2: Combined Kaplan-Meier Curve; PM>1 (n=31); PM=1 (n=46)"
) 



km.by.group

group.survival = survdiff(Surv(months, censor) ~ number, data=colorectalDataset, rho=0)

#### CHI SQUARE AND STATISTICAL SIGNIFANCE P-VALUE OF THE TWO GROUPS
#### CHI SQUARE AND STATISTICAL SIGNIFANCE P-VALUE OF THE TWO GROUPS
#### CHI SQUARE AND STATISTICAL SIGNIFANCE P-VALUE OF THE TWO GROUPS
#### CHI SQUARE AND STATISTICAL SIGNIFANCE P-VALUE OF THE TWO GROUPS

group.survival

km.by.group

summary(km.by.group)


summary(survfit(formula = Surv(months, censor) ~ strata(number), data = colorectalDataset, conf.type = "log-log"), times=60)





