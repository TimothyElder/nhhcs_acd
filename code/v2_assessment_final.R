# Timothy Elder, code for analysis of the National Home and Hospice 

# Library Data, Log Sink, and Functions ----
setwd("/Users/timothyelder/Documents/nhhcs_acd")

library(tidyverse)
library(srvyr)

nhhcs <- haven::read_dta("ICPSR_28961/DS0002/28961-0002-data.dta")

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

#loading survey object, selecting, subsetting, and recoding
nhhcs_design_srvyr <- nhhcs %>%
  as_survey_design(1, strata = PSTRATA, weight = SAMWT,
                   variables = c(CASEID, PATNUM, PHTYPE, TOTCDDX, CDDX1, TOTALRX, TOTPROC, DIRECTIVES, 
                                 LOSHH, READMSS, SEX, AGEATINT, HISPAN, RACEBLCK, RACEWHT, 
                                 MARSTAT, MCAIDENR, ANYADDIR, LIVINGWL, DNR, COGNFUNC, MARRIED, TERMINAL )) %>% 
  subset(PHTYPE == 1) %>%
  na.omit()

nhhcs_design_srvyr$variables %>% mutate_at(c("ANYADDIR","SEX", "READMSS", "MCAIDENR"), 
                                           funs(recode(.,`2`= 0)))

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
nhhcs$MCAIDENR <- plyr::mapvalues(nhhcs$MCAIDENR, from = c(2), to = c(0))
nhhcs$MCAIDENR <- plyr::mapvalues(nhhcs$MCAIDENR, from = c(3), to = c(1))

nhhcs$HISPAN <- plyr::mapvalues(nhhcs$HISPAN, from = c(2), to = c(0))
nhhcs$RACEBLCK <- plyr::mapvalues(nhhcs$RACEBLCK, from = c(2), to = c(0))
nhhcs$RACEWHT <- plyr::mapvalues(nhhcs$RACEWHT, from = c(2), to = c(0))

# New Variables
nhhcs$DIRECTIVES <- rowSums(nhhcs[,33:43] == 1)-1 #New variable for number of directives patient has
nhhcs$MARRIED <- ifelse(nhhcs$MARSTAT =='MARRIED', 1, 0) #new dummy for whether patient is currently married 
nhhcs$TERMINAL <- ifelse(nhhcs$LIFEXPEC==1, 1, 0) # new terminal for whether the patient is terminal 
nhhcs$log_LOSHH <- log1p(nhhcs$LOSHH) #logarithmic transformation of the LOSHH variable to acconut for skewness

#subsetting for current home health patients 
home <- nhhcs %>% 
  subset(PHTYPE == 1) %>% 
  select(CASEID, PATNUM, TOTCDDX, CDDX1, TOTALRX, TOTPROC, DIRECTIVES, LOSHH, READMSS, SEX, AGEATINT, HISPAN, RACEBLCK, RACEWHT, MARSTAT, MCAIDENR, ANYADDIR, LIVINGWL, DNR, COGNFUNC, MARRIED, TERMINAL, log_LOSHH) %>%
  na.omit()

e1071 ::skewness(home$LOSHH)

#for null model 
N00 <- home %>%
  select(ANYADDIR)

#for annual hospice discharges
discharge <- nhhcs %>% 
  subset(PHTYPE == 2) %>% 
  select(CASEID, PATNUM, PTAGYNUM, TOTCDDX, CDDX1, TOTALRX, TOTPROC, DIRECTIVES, READMSS, HOSPICEDAYS, DECEASED, DISCHARG, WHRDISCH, SEX, AGEATDIS, HISPAN, RACEASIA, RACEBLCK, RACEWHT, MARSTAT, VETERAN, MCARENR, MCAIDENR, ANYADDIR, LIVINGWL, DNR, NOHOSP, FEEDRES, MEDRES, COMFORT, POWER, PROXY, ORGANDON, NOADVANC, INPATIENT, PREHOME, LIVSMHOS, PREHOSP, POSTHOSP, SAMWT)

# Histograms for IVs ----

length(unique(home$CDDX1)) #count of number of different current diagnoses, by ICD Code

#counts of ICD primary diagnoses
icd.count <- home %>% group_by(CDDX1) %>% summarise(Freq=n()) 

count <- nhhcs %>% group_by(PHTYPE, HOSPICEDAYS) %>% summarise(Freq=n())

#histogram of total number of medications
rx.base.nottermst <- ggplot(data.frame(home), aes(x=TOTALRX)) +
  geom_bar(fill = 'royalblue') +
  ggtitle("Distribition of Number of Medications Patient Taking") +
  xlab("Number of Medications") +
  ylab("Frequency")

#histogram of total number of diagnoses for home health patients 
diagnoses.hist <- ggplot(data.frame(home), aes(x=TOTCDDX)) +
  geom_bar(fill = 'royalblue') +
  ggtitle("Distribition of Number of Primary Diagnoses Patient Has") +
  xlab("Number of Primary Diagnoses") +
  ylab("Frequency")

#histogram of total number of procedures for home health patients 
procedures.hist <- ggplot(data.frame(home), aes(x=TOTPROC)) +
  geom_bar(fill = 'royalblue') +
  ggtitle("Distribition of Number of Procedures") +
  xlab("Number of Primary Procedures") +
  ylab("Frequency")

#same histogram excluding those who have been in home health for more than 1095 days 
los.hist <- home %>% 
  subset(LOSHH != 1095) %>%
  ggplot(aes(x=as.integer(LOSHH))) +
  geom_histogram(binwidth = 10, color = "grey", fill ="royalblue") +
  ggtitle("Distribition of Length of Stay in Days") +
  xlab("Length of Stay (Days)") +
  ylab("Frequency")

los.hist <- los.hist + labs(title = "Distribition of Length of Stay in Days",
                  caption = "Excludes Patients whose length of stay exceeds 1095 days (N = 309)")

log.los.hist <- nhhcs %>% 
  subset(PHTYPE == 1) %>% 
  ggplot(aes(x=as.integer(log_LOSHH))) +
  geom_histogram(binwidth = 1, color = "grey", fill ="royalblue") +
  ggtitle("Distribition of Length of Stay in Days") +
  xlab("Length of Stay (Days)") +
  ylab("Frequency")

# histogram of age of patient at the time of the interview
age.hist <- ggplot(data.frame(home), aes(x=AGEATINT)) +
  geom_histogram(color = "grey", fill = 'royalblue', binwidth = 1) +
  ggtitle("Distribition of Age of Patients") +
  xlab("Age") +
  ylab("Frequency")


#histogram of marriage status
marriage.hist <- ggplot(data.frame(home), aes(x=MARSTAT)) +
  geom_bar(fill = 'royalblue') +
  ggtitle("Distribition of Number of Marriage Status") +
  xlab("Marriage Status") +
  ylab("Frequency")

#applying labels to cogfunc 
home$lab.cognfunc <- labelled::to_factor(home$COGNFUNC)

cognitive.hist <- ggplot(data.frame(home), aes(x=lab.cognfunc)) +
  geom_bar(fill = 'royalblue') +
  ggtitle("Distribition of Cognitive Function") +
  xlab("Cognitive Function") +
  ylab("Frequency") + 
  scale_x_discrete(labels = c('No Impairment','Very Litte','Some', 'Great Deal', 'Severe Impaitment'))

#for creating a grid of plots and then save
all.g <- gridExtra::grid.arrange(age.hist, los.hist, procedures.hist, rx.base.nottermst, diagnoses.hist, cognitive.hist, ncol=3)
ggsave("figures/distributions_ivs.pdf", all.g)

# Correlation Matrix of DV and IVs with viz ---- 
#selecting data
cordata <- home[, c(17,10, 11, 20, 21, 22, 8,  9, 3, 5, 6, 16)]

#calculating correlation matrix
cormat <- round(cor(cordata),2)
dim(cormat)
melted_cormat <- reshape2::melt(cormat) #reshape

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)

# Heatmap
cor.heatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  ggtitle("Correlation Matrix of DV and IVs") +
  coord_fixed()
  
ggsave("figures/cor_heatmap.pdf", cor.heatmap)

cor.heatmap.text<- cor.heatmap +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


ggsave("figures/cor_heatmap_text.pdf", cor.heatmap.text)

# Models ----

#null model
null.dir.logit <- glm(ANYADDIR ~ ., 
                            family = "binomial", data = N00)

#using glm without survey weights
unweighted.dir.logit <- glm(ANYADDIR ~ SEX + AGEATINT + COGNFUNC + MARRIED + TERMINAL + 
                              LOSHH + READMSS + TOTCDDX + TOTALRX + TOTPROC + MCAIDENR, 
                 family = "binomial", data = home)

#log transformation of 
log.los.dir.logit <- glm(ANYADDIR ~ SEX + AGEATINT + COGNFUNC + MARRIED + TERMINAL + 
                              log_LOSHH + READMSS + TOTCDDX + TOTALRX + TOTPROC + MCAIDENR, 
                            family = "binomial", data = home)

#including race 
race.dir.logit <- glm(ANYADDIR ~ SEX + AGEATINT + COGNFUNC + MARRIED + TERMINAL + 
                        log_LOSHH + READMSS + TOTCDDX + TOTALRX + TOTPROC + MCAIDENR + HISPAN +  RACEBLCK + RACEWHT, 
                            family = "binomial", data = home)


#using survey library for glm with weights, does not fit using maximum likelihood
weighted.dir.logit <- survey::svyglm(ANYADDIR ~ SEX + AGEATINT + COGNFUNC + MARRIED + TERMINAL + LOSHH + READMSS + TOTCDDX + TOTALRX + TOTPROC + MCAIDENR, family = binomial, design=nhhcs_design_srvyr)

#comparing the weighted and unweighted models 
stargazer::stargazer(unweighted.dir.logit, weighted.dir.logit, type = "text")

# Comparing the null, base and full models
stargazer::stargazer(unweighted.dir.logit, log.los.dir.logit, race.dir.logit, type = "text")

#base proportion of directives for home health patients
proportion.directives <- sum(na.omit(home$ANYADDIR))/length(na.omit(home$ANYADDIR))

#for getting odds 
proportion.directives / (1 - proportion.directives)

# Simulating coefficients and plotting -----
logit2prob(coef(unweighted.dir.logit))

summary(unweighted.dir.logit)

logLik(unweighted.dir.logit)

## odds ratios and 95% CI
exp(cbind(OR = coef(unweighted.dir.logit), confint(unweighted.dir.logit)))

#Getting Coefficients to calculate by hand probabilities
intercept <- coef(unweighted.dir.logit)[1]
b_sex <- coef(unweighted.dir.logit)[2]
b_age <- coef(unweighted.dir.logit)[3]
b_cognitive <- coef(unweighted.dir.logit)[4]
b_married <- coef(unweighted.dir.logit)[5]
b_terminal <- coef(unweighted.dir.logit)[6]
b_length_stay <- coef(unweighted.dir.logit)[7]
b_readmit <- coef(unweighted.dir.logit)[8]
b_diagnoses <- coef(unweighted.dir.logit)[9]
b_meds <- coef(unweighted.dir.logit)[10]
b_procedures <- coef(unweighted.dir.logit)[11]
b_medicaid <- coef(unweighted.dir.logit)[12]

#plug and play to get logits under different values of IV 

#for men, mean age, mean cognitive function, married, 6 moths or less prognosis, mean length of stay, readmitted, mean diagnoses, meds and procedures, on medicaid
women.adir <- intercept + 0 * b_sex  + mean(home$AGEATINT) * b_age + mean(home$COGNFUNC) * b_cognitive + 0 * b_married +
  0 * b_terminal + mean(home$log_LOSHH) * b_length_stay + 0 * b_readmit + mean(home$TOTCDDX) + mean(home$TOTALRX) * b_meds +
  mean(home$TOTPROC) * b_procedures + 0 * b_medicaid 

men.adir <- intercept + 1 * b_sex  + mean(home$AGEATINT) * b_age + mean(home$COGNFUNC) * b_cognitive + 0 * b_married +
  0 * b_terminal + mean(home$log_LOSHH) * b_length_stay + 0 * b_readmit + mean(home$TOTCDDX) + mean(home$TOTALRX) * b_meds +
  mean(home$TOTPROC) * b_procedures + 0 * b_medicaid 
  
# the probability of having an advance directive is
logit2prob(women.adir)
logit2prob(men.adir)

terminal.adir <- intercept + 0 * b_sex  + mean(home$AGEATINT) * b_age + mean(home$COGNFUNC) * b_cognitive + 0 * b_married +
  1 * b_terminal + mean(home$log_LOSHH) * b_length_stay + 0 * b_readmit + mean(home$TOTCDDX) + mean(home$TOTALRX) * b_meds +
  mean(home$TOTPROC) * b_procedures + 0 * b_medicaid 

notterminal.adir <- intercept + 0 * b_sex  + mean(home$AGEATINT) * b_age + mean(home$COGNFUNC) * b_cognitive + 0 * b_married +
  0 * b_terminal + mean(home$log_LOSHH) * b_length_stay + 0 * b_readmit + mean(home$TOTCDDX) + mean(home$TOTALRX) * b_meds +
  mean(home$TOTPROC) * b_procedures + 0 * b_medicaid 

logit2prob(terminal.adir)
logit2prob(notterminal.adir)

# W&A Simulation and Estimation  ---- 
# Specifies the granularity of the eventually plotted values
ROWS <- 100

# simulate the beta coefs, produces a thousand simulated coefficients for the model
#B.sim <- MASS::mvrnorm(1000, coef(log.los.dir.logit), vcov(log.los.dir.logit))

## ------------- Viz for difference in binary IVs --------------- ##
# define the range of intererest
age.range <- seq(min(home$AGEATINT), max(home$AGEATINT), length=ROWS)


# Model Assessment ----

#plotting residuals etc
pdf("figures/unweighted_dir_logit_modelfit_base.pdf")               # save to file
par(mfrow=c(2,2))
plot(unweighted.dir.logit)
dev.off()                                       # turn off pdf()
par(mfrow=c(1,1))

# table of predicted directives by actual values, unweigthed
pdir <- as.integer(predict(unweighted.dir.logit)>.5)
table(pdir, home$ANYADDIR)

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
wa_predicted <- fitted(unweighted.dir.logit)
wa_actual <- as.numeric(unweighted.dir.logit$model$ANYADDIR)
wa_pred.simp <- fitted(null.dir.logit)
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
wa_pred<-ROCR::prediction(wa_predicted, wa_actual)
# - ROCR::performance takes three arguments: the prediction obj (from ROCR::prediction,
#         truepositive rate and false positive rate (specified as options)
wa_perf<-ROCR::performance(wa_pred,"tpr","fpr")
pdf(file = "figures/anydirective_unweighted_ROC.pdf", width=5, height=5, onefile=TRUE) 
par(las=1, bty="n")  
plot(wa_perf,main="ROC plot for unweighted Any Directive Model", bty="n",lwd=1)
par(new=TRUE)
lines(wa_actual,wa_actual, lty="dashed")
dev.off()

# Examining Missing Data ---- 

#checking for where data is missing
colSums(is.na(gtd.s)) 

#From mice package
md.pattern(gtd.s)

#Visual representation of the Missing Data
aggr_plot <- VIM::aggr(pain.levels, col=c('royalblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(pain.levels), cex.axis=.7, gap=3, ylab=c("Histogram of missing pain.levels","Pattern"))


#looking at missingness between two variables 
marginplot(data[c(1,2)])


#Multiple Imputation using CART, 
#tempData <- mice(gtd.s,m=5,maxit=50,meth='cart',seed=500)
summary(tempData)

#latex ---- 
as.data.frame(home) %>%
  select(ANYADDIR, SEX, AGEATINT, MARRIED, TERMINAL, log_LOSHH, LOSHH, READMSS, TOTCDDX, TOTALRX, TOTPROC, MCAIDENR, HISPAN, RACEWHT, RACEBLCK) %>%
  stargazer::stargazer()
