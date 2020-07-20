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

km.by.group = survfit(formula = Surv(months, censor) ~ strata(number), data = colorectalDataset, conf.type = "log-log")

plot(km.by.group, lty=c(1,3), xlab="months", ylab = "Survival Probability")
legend(7, 0.3, c("delayed", "not delayed"), lty=c(1,3))

km.by.group

group.survival = survdiff(Surv(months, censor) ~ number, data=colorectalDataset, rho=0)

summary(km.by.group)


###
km.by.combine = survfit(formula = Surv(months, censor) ~ 1, data = colorectalDataset, conf.type = "log-log")

plot(km.by.combine, xlab="months", ylab = "Survival Probability")

km.by.combine

summary(km.by.combine)


