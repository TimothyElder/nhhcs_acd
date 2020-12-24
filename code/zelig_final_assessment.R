library(Zelig)
library(tidyverse)

nhhcs <- haven::read_dta("/Volumes/timothyelder/Graduate/Maximum_Likelihood/assessment_final/ICPSR_28961/DS0002/28961-0002-data.dta")

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

#estimating model with Zelig
z.out1 <- zelig(ANYADDIR ~ SEX + AGEATINT + COGNFUNC + MARRIED + TERMINAL + 
                  LOSHH + READMSS + TOTCDDX + TOTALRX + TOTPROC + MCAIDENR, model = "logit", data = home,
                cite = FALSE)

summary(z.out1)

summary(z.out1, odds_ratios = TRUE)

x.high <- setx(z.out1, SEX = 1)

x.low <- setx(z.out1, SEX = 0)

s.out2 <- sim(z.out1, x = x.high, x1 = x.low)
summary(s.out2)

plot(s.out2)
