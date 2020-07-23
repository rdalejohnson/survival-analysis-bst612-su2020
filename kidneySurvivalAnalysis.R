kidneyDataset <- read.table("kidney_survival.csv", sep=",", header=T)


#categorical variable

censor.freq = table(kidneyDataset$censor)
censor.prop = prop.table(censor.freq)
censor.table = as.data.frame(cbind(censor.freq, censor.prop))
colnames(censor.table) = c("Frequency", "Percent")
censor.table


group.freq = table(kidneyDataset$Group)
group.prop = prop.table(group.freq)
group.table = as.data.frame(cbind(group.freq, group.prop))
colnames(group.table) = c("Frequency", "Percent")
group.table


#continuous variables

time.summary = summary(kidneyDataset$time)
time.summary


table.censor.by.group = table(kidneyDataset$censor, kidneyDataset$Group)
table.censor.by.group.freq.table = cbind(table.censor.by.group, margin.table(table.censor.by.group, 1))
table.censor.by.group.freq.table = rbind(table.censor.by.group.freq.table, c(margin.table(table.censor.by.group, 2), 214))

table.censor.by.group.freq.table

library(MASS)
chisq.test(table.censor.by.group)

library(survival)
kidneyDataset$SurvObj = with(kidneyDataset, Surv(time, censor == 1))

#note that the SurvObj column is now in place
head(kidneyDataset)

km.by.group = survfit(formula = Surv(time, censor) ~ strata(Group), data = kidneyDataset, conf.type = "log-log")

plot(km.by.group, lty=c(1,3), xlab="Time", ylab = "Survival Probability")
legend(7, 0.3, c("delayed", "not delayed"), lty=c(1,3))

km.by.group

group.survival = survdiff(Surv(time, censor) ~ Group, data=kidneyDataset, rho=0)

group.survival

summary(km.by.group)


###
km.by.combine = survfit(formula = Surv(time, censor) ~ 1, data = kidneyDataset, conf.type = "log-log")

plot(km.by.combine, xlab="Time", ylab = "Survival Probability")

km.by.combine

summary(km.by.combine)

survfit(Surv(time, censor) ~ 1, data = kidneyDataset)
survfit(Surv(time, censor) ~ strata(Group), data = kidneyDataset)





# SURVIVAL TO 60 MONTHS FOR ENTIRE SAMPLE AND EACH GROUP

summary(survfit(Surv(time, censor) ~ 1, data = kidneyDataset), times = 60)

summary(survfit(Surv(time, censor) ~ 1, data = kidneyDataset))


summary(survfit(formula = Surv(time, censor) ~ strata(Group), data = kidneyDataset, conf.type = "log-log"), times=60)


survfit(formula = Surv(time, censor) ~ strata(Group), data = kidneyDataset)


summary(km.by.combine)$table   #median survival time

summary(km.by.group)$table   #median survival time



library(survminer)

#median survival both groups
# survminer::surv_median(km.by.group)
# 
# survminer::surv_summary(km.by.group)
