# MLE Final Assignment, Timothy Elder

# Library Data, Log Sink, and Functions ----
setwd("/Volumes/timothyelder/Graduate/Maximum_Likelihood/assessment_final")

library(tidyverse)
library(Zelig)
library(survey)

nhhcs <- haven::read_dta("/Volumes/timothyelder/Graduate/Maximum_Likelihood/assessment_final/ICPSR_28961/DS0002/28961-0002-data.dta")

sink("elder_assessment_final.log", append=TRUE, split=TRUE)

#for getting mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# define an inverse logit function
inv.logit <- function(x) {
  1/(1+exp(-x))
}

#function for converting logits to probability, to use run logit2prob(coef(logit.object.here))
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# Recoding nhhcs from STATA ---- 

#Missing and non-response to NA
nhhcs[nhhcs == -1] <- NA #fill INAPPLICABLE/NOT ASCERTAINED with NA
nhhcs[nhhcs == -7] <- NA #fill Refused with NA
nhhcs[nhhcs == -8] <- NA #fill missing with NA

#Categorical Variables to Display Lables
nhhcs$MARSTAT <- labelled::to_factor(nhhcs$MARSTAT) #marriage status
nhhcs$DISCHARG <- labelled::to_factor(nhhcs$DISCHARG) #reason for discharge
nhhcs$WHRDISCH <- labelled::to_factor(nhhcs$WHRDISCH) #where go after discharge

# Dummies originally coded 1=YES, 2=NO, recode 2 == 0
nhhcs$READMSS <- plyr::mapvalues(nhhcs$READMSS, from = c(2), to = c(0))
nhhcs$DECEASED <- plyr::mapvalues(nhhcs$DECEASED, from = c(2), to = c(0))
nhhcs$SEX <- plyr::mapvalues(nhhcs$SEX, from = c(2), to = c(0))
nhhcs$ANYADDIR <- plyr::mapvalues(nhhcs$ANYADDIR, from = c(2), to = c(0))
nhhcs$LIVINGWL <- plyr::mapvalues(nhhcs$LIVINGWL, from = c(2), to = c(0))
nhhcs$DNR <- plyr::mapvalues(nhhcs$DNR, from = c(2), to = c(0))
nhhcs$NOHOSP <- plyr::mapvalues(nhhcs$NOHOSP, from = c(2), to = c(0))
nhhcs$FEEDRES <- plyr::mapvalues(nhhcs$FEEDRES, from = c(2), to = c(0))
nhhcs$MEDRES <- plyr::mapvalues(nhhcs$MEDRES, from = c(2), to = c(0))
nhhcs$COMFORT <- plyr::mapvalues(nhhcs$COMFORT, from = c(2), to = c(0))
nhhcs$POWER <- plyr::mapvalues(nhhcs$POWER, from = c(2), to = c(0))
nhhcs$ORGANDON <- plyr::mapvalues(nhhcs$ORGANDON, from = c(2), to = c(0))
nhhcs$NOADVANC <- plyr::mapvalues(nhhcs$NOADVANC, from = c(2), to = c(0))

#New variable for number of directives patient has
nhhcs$DIRECTIVES <- rowSums(nhhcs[,33:43] == 1)-1 

#loading survey design object
design <- svydesign(id = ~CASEID, strata = ~PSTRATA,  weights = ~SAMWT, data = subset(nhhcs, PHTYPE == 1))

#subsetting for current home health patients 
home <- nhhcs %>% 
  subset(PHTYPE == 1) %>% 
  select(CASEID, PATNUM, TOTCDDX, CDDX1, TOTALRX, TOTPROC, DIRECTIVES, LOSHH, READMSS, SEX, AGEATINT, HISPAN, RACEBLCK, RACEWHT, MARSTAT, MCAIDENR, ANYADDIR, LIVINGWL, DNR) %>%
  na.omit()

#for null model 
N00 <- home %>%
  select(ANYADDIR)

#fpr annual hospice discharges
discharge <- nhhcs %>% 
  subset(PHTYPE == 2) %>% 
  select(CASEID, PATNUM, PTAGYNUM, TOTCDDX, CDDX1, TOTALRX, TOTPROC, DIRECTIVES, READMSS, HOSPICEDAYS, DECEASED, DISCHARG, WHRDISCH, SEX, AGEATDIS, HISPAN, RACEASIA, RACEBLCK, RACEWHT, MARSTAT, VETERAN, MCARENR, MCAIDENR, ANYADDIR, LIVINGWL, DNR, NOHOSP, FEEDRES, MEDRES, COMFORT, POWER, PROXY, ORGANDON, NOADVANC, INPATIENT, PREHOME, LIVSMHOS, PREHOSP, POSTHOSP, SAMWT)

# nhhcs Exploration Home Health Patients ----

length(unique(home$CDDX1)) #count of number of different current diagnoses, by ICD Code

#counts of ICD primary diagnoses
icd.count <- home %>% group_by(CDDX1) %>% summarise(Freq=n()) 

count <- nhhcs %>% group_by(PHTYPE, HOSPICEDAYS) %>% summarise(Freq=n())

#histogram of total number of medications
ggplot(data.frame(home), aes(x=TOTALRX)) +
  geom_bar()


nrow(subset(home, ANYADDIR == 1)) / nrow(home) #only 30% of the home health patients have any advance directive, that is compared to 88% in the discharged hospice patients

#histogram of total number of diagnoses for home health patients 
ggplot(data.frame(home), aes(x=as.integer(TOTCDDX))) +
  geom_bar()

#histogram of total number of procedures for home health patients 
ggplot(data.frame(home), aes(x=as.integer(TOTPROC))) +
  geom_bar()

# histogram of length of stay for home health, may are more than 1095 days
ggplot(data.frame(home), aes(x=as.integer(LOSHH))) +
  geom_bar()

#same histogram excluding those who have been in home health for more than 1095 days 
home %>% 
  subset(LOSHH != 1095) %>%
  ggplot(aes(x=as.integer(LOSHH))) +
  geom_bar()

# histogram of age of patient at the time of the interview
ggplot(data.frame(home), aes(x=as.integer(AGEATINT))) +
  geom_bar()

#histogram of marriage status
ggplot(data.frame(home), aes(x=MARSTAT)) +
  geom_bar()

ad <- subset(home, ANYADDIR == 1) #subset nhhcs for anyone with an advance directive

ggplot(data.frame(home), aes(x=as.numeric(ANYADDIR))) +
  geom_bar()

#histogram of where patients were before admission to home health/hospice
ggplot(data.frame(nhhcs), aes(x=PREHOME)) +
  geom_bar()

#correlation matrix 
cormat <- round(cor(home),2)

# Models ----
length(na.omit(home$SEX))
length(na.omit(home$AGEATINT))
length(na.omit(home$LOSHH))
length(na.omit(home$READMSS))
length(na.omit(home$TOTCDDX))
length(na.omit(home$TOTALRX))
length(na.omit(home$TOTPROC))

#null model
N00 <- home %>% select(ANYADDIR)
null.dir.logit <- glm(ANYADDIR ~ ., 
                            family = "binomial", data = N00)

#using glm without survey weights
unweighted.dir.logit <- glm(ANYADDIR ~ SEX + AGEATINT + LOSHH + READMSS + TOTCDDX + TOTALRX + TOTPROC + MCAIDENR + MARSTAT, 
                 family = "binomial", data = home)

#using survey library for glm with weights
weighted.dir.logit <- svyglm(ANYADDIR ~ SEX + AGEATINT + LOSHH + READMSS + TOTCDDX + TOTALRX + TOTPROC, 
                             family = binomial, subset = PHTYPE == 1, design=design, na.action=na.exclude)

#using Zelig library for logit with weights
z.out1 <- zelig(ANYADDIR ~ SEX + AGEATINT + LOSHH + READMSS + TOTCDDX + TOTALRX + TOTPROC, model = "logit.survey",
                weights = home$SAMWT, data = home)

x.low <- setx(z.out1, AGEATINT= quantile(home$AGEATINT, 0.2))
x.high <- setx(z.out1, AGEATINT= quantile(home$AGEATINT, 0.8))
s.out1 <- sim(z.out1, x = x.low, x1 = x.high)
summary(s.out1)

plot(s.out1)

summary(dir.logit)

stargazer::stargazer(unweighted.dir.logit, weighted.dir.logit, type="text")

dir.logit$aic
BIC(dir.logit)

dir.logit <- weighted.dir.logit


#base proportion of directives for home health patients
proportion.directives <- sum(na.omit(home$ANYADDIR))/length(na.omit(home$ANYADDIR))

#for getting odds 
proportion.directives / (1 - proportion.directives)


# Simulating coefficients and plotting -----

logit2prob(coef(dir.logit))

summary(dir.logit)

logLik(dir.logit)

## odds ratios and 95% CI
exp(cbind(OR = coef(dir.logit), confint(dir.logit)))

#Getting Coefficients to calculate by hand probabilities
intercept <- coef(dir.logit)[1]
b_sex <- coef(dir.logit)[2]
b_age <- coef(dir.logit)[3]
b_loshh <- coef(dir.logit)[4]
b_readmit <- coef(dir.logit)[5]
b_diagnoses <- coef(dir.logit)[6]
b_meds <- coef(dir.logit)[7]
b_procedures <- coef(dir.logit)[8]

#plug and play to get logits under different values of IV 

#for mean capability ratio, with allies, mean trade and contiguous border
logits.onset.war <- intercept + mean(home$AGEATINT) * b_AGEATINT + 1 * b_allies + mean(home$trade) * b_trade + 1* b_contig
# the probability of war in the next year is 
logit2prob(logits.onset.war)

#for mean capability ratio, with allies, mean trade and contiguous border
logits.onset.war <- intercept + mean(home$AGEATINT) * b_AGEATINT + 1 * b_allies + mean(home$trade) * b_trade + 0* b_contig
# the probability of war in the next year is 
logit2prob(logits.onset.war)

# W&A Simulation and Estimation  ---- 
# how many rows?
ROWS <- 100

# define the range for the capability variable
age.range <- seq(min(home$AGEATINT), max(home$AGEATINT), length=ROWS)

# set a baseline condition, this is the configuration we will be looking at
x.baseline <- c(1,                 # intercept
                0,  # for men
                mean(home$AGEATINT),   # mean age
                median(home$LOSHH),   # median length of stay due to large right tail
                1,                     #readmitted patients
                mean(home$TOTCDDX),     #mean diagnoses
                mean(home$TOTALRX, na.rm=TRUE),     #mean meds
                0)    # No Procudes, the mode

# repeat for a matrix (note X, not x). Also note: the X matrix is _transposed_ from the
# usual, which simplifies the data load into the matrix.
X.baseline <- matrix(x.baseline, nrow=length(x.baseline), ncol=ROWS)

# replace the range *into* X.baseline, saving a new copy, this is for contiguous variable 
X.age <- X.baseline
X.age[3, ] <- age.range

# simulate the beta coefs, produces a thousand simulated coefficients for the model
B.sim <- MASS::mvrnorm(1000, coef(dir.logit), vcov(dir.logit))

# straight from W&A: obtain the link function evaluated over _simulated beta_ and fixed X
# the %*% is matrix multiplication
s.age <- inv.logit(B.sim %*% X.age) #each row is a simulation given the value of the capability ratio, from the min to the max

# obtain the quantiles from the s.age simulated data
s.age <- apply(s.age, 2, quantile, c(0.025, .5, .975))   # 95% CI and median

# extract and name rows
PrDir.lo <- s.age[1,]
PrDir.med <- s.age[2,]
PrDir.hi <- s.age[3,]

# build a tibble for ggplot
df <- tibble(age.range, PrDir.lo, PrDir.med, PrDir.hi)

### could just type out 'df' here, but only if ROWS=7

# nice ggplot
age.g <- ggplot(df, aes(x=age.range)) + 
  geom_line(aes(y=PrDir.med)) +
  geom_ribbon(aes(ymin=PrDir.lo, ymax=PrDir.hi), alpha=.2) +
  ggtitle("Probability of Advance Directive by Age") +
  xlab("Age") +
  ylab("Probability of having an Advance Directive") 

age.g

#Now by number of diagnoses
#define range for contiguous
diagnoses.range <- seq(min(home$TOTCDDX), max((home$TOTCDDX)), length=ROWS)

# again create the matrix of values, now with contig varying and capabability constant
# (contig is in the 4th  position)
X.diagnoses <- X.baseline
X.diagnoses[6,] <- diagnoses.range

# using the same B.sim as above (DO NOT resimulate)
# straight from W&A
s.diagnoses <- inv.logit(B.sim %*% X.diagnoses)
s.diagnoses <- apply(s.diagnoses, 2, quantile, c(0.025, .5, .975))   # 95% CI and median

# extract and name rows
PrDirDiagnoses.lo <- s.diagnoses[1,]
PrDirDiagnoses.med <- s.diagnoses[2,]
PrDirDiagnoses.hi <- s.diagnoses[3,]


df <- tibble(diagnoses.range, PrDirDiagnoses.lo, PrDirDiagnoses.med, PrDirDiagnoses.hi)

# nice ggplot
ggplot(df, aes(x=diagnoses.range)) + 
  geom_line(aes(y=PrDirDiagnoses.med)) +
  geom_ribbon(aes(ymin=PrDirDiagnoses.lo, ymax=PrDirDiagnoses.hi), alpha=.2)+
  ggtitle("Probability of Advance Directive by Number of Primary Diagnoses") +
  xlab("Number of Primary Diagnoses") +
  ylab("Probability of having an Advance Directive") 


# Model Assessment ----

#plotting residuals etc
#outputting to pdf
pdf("figures/dir_logit_modelfit_base.pdf")               # save to file
par(mfrow=c(2,2))
plot(unweighted.dir.logit)

dev.off()                                       # turn off pdf()
par(mfrow=c(1,1))

# table of predicted vote by actual values, unweigthed
pdir <- as.integer(predict(unweighted.dir.logit)>.5)
table(pdir, home$ANYADDIR)

# table of predicted vote by actual values, unweigthed
pdir <- as.integer(predict(weighted.dir.logit)>.5)
table(pdir, design$variables$ANYADDIR)

# pct_correct
pred_array <- as.array(table(pdir, home$ANYADDIR), dim=c(2,2))
right <- sum(diag(pred_array))
total <- sum(pred_array)
pct_correct <- right/total
pct_correct # 68% correctly predicted

# PRE (pct)
all0 <- sum(pred_array[,1])/total
all1 <- sum(pred_array[,2])/total
PRE <- ifelse (all1<all0, (1-pct_correct)/(1-all1), (1-pct_correct)/(1-all0))
PRE

# model likelihood
K <- (unweighted.dir.logit$rank)-1
df <- length(unweighted.dir.logit$y)
ModelLLK <- logLik(unweighted.dir.logit)
cat("Model Log-Likelihood", ModelLLK, "\n")
NullLLK <- logLik(null.dir.logit)
cat("Null Log-Likelihood", NullLLK, "\n")

# By hand
LR.byhand <- -2*(NullLLK - ModelLLK)
cat("Likelihood Ratio", LR.byhand, "; Chi^2 (df=", K, ")=", 1-pchisq(LR.byhand, K), "\n")

# Likeihood ratio from the glm
LR <- unweighted.dir.logit$null.deviance - unweighted.dir.logit$deviance
cat("Likeihood Ratio", LR, "; Chi^2 (df=", K, ") = ", 1-pchisq(LR,K), "\n")

# AIC and BIC
cat("AIC", AIC(unweighted.dir.logit), "BIC", BIC(unweighted.dir.logit), "\n")

# Brier score
wa_predicted<-fitted(unweighted.dir.logit)
wa_actual<-as.numeric(unweighted.dir.logit$model$ANYADDIR)
wa_pred.simp<-fitted(null.dir.logit)
brier_predicted <- verification::brier(wa_actual,wa_predicted)
brier_null <- verification::brier(wa_actual,wa_pred.simp)
cat("Brier scores for our model", brier_predicted$bs, "vs null", brier_null$bs, "\n")
wa_pred<-wa_predicted*0.0
wa_pred[wa_predicted>=.5]<-1
tab<-table(wa_pred,wa_actual)
names(tab)<-c("Observed 0","Observed 1")
rownames(tab)<-c("Predicted 0","Observed 1")
tab1<-xtable::xtable(table(wa_pred,wa_actual),caption="Predicted at c=.5 or greater.")
names(tab1)<-c("Observed 0","Observed 1")
row.names(tab1)<-c("Predicted 0","Predicted 1")
tab1 #latex table


# ROC plot, base R (Ward and Ahlquist)
# - ROCR::prediction takes two arguments: fitted values and "labels" (observed values)
wa_predicted<-fitted(unweighted.dir.logit)
wa_actual<-as.vector(null.dir.logit$model$ANYADDIR)
wa_pred<-ROCR::prediction(wa_predicted,wa_actual)
# - ROCR::performance takes three arguments: the prediction obj (from ROCR::prediction,
#         truepositive rate and false positive rate (specified as options)
wa_perf<-ROCR::performance(wa_pred,"tpr","fpr")
pdf(file = "figures/anydirective_unweighted_ROC.pdf", width=5, height=5, onefile=TRUE) 
par(las=1, bty="n")  
plot(wa_perf,main="ROC plot for UNWEIGHTED Any Directive Model", bty="n",lwd=1)
par(new=TRUE)
lines(wa_actual,wa_actual, lty="dashed")
dev.off()

# MICE
#checking for where data is missing
colSums(is.na(home)) 

#From mice package
mice::md.pattern(home)

#Visual representation of the Missing Data
aggr_plot <- VIM::aggr(home, col=c('royalblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(home), cex.axis=.7, gap=3, ylab=c("Histogram of missing home","Pattern"))
