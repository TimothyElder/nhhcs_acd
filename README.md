# Prevalence and Predictors of Advance Care Directives for Home Health and Hospice Patients

### Description

This repository has code for analyses related to ongoing work which models the use of advance care directives by long term care patietns using the [National Home and Hospice Care Survey](https://www.cdc.gov/nchs/nhhcs/index.htm). There is a ["Data Brief"](https://www.cdc.gov/nchs/products/databriefs/db54.htm) which outlines some of the initial findings regarding the use of Advance Directives in the NHHCS populations, which includes discharged hospice patients and current home health patients, both of which are considered "long term care patients".

The NHHCS Survey includes survey weights which allow for inference to national representation from the sample. There is a dissertation which uses this survey quite well and which should be reviewed.

### Data
From the data dictionary: "The sample design of the 2007 National Home Health and Hospice Care Survey estimates the number of current home health patients at a given point in time and the number of hospice discharges in a year. This design requires the data user to always do separate analysis of patients and discharges, using the PHTYPE variable. For current home health patients, PHTYPE=1 and for annual hospice discharges, PHTYPE=2." There are equal numbers of each.

So the dataset doesn't have data regarding *current* hospice patients, only those that were discharged which includes those that died. It also means that certain of the variables only apply to one or the other patient types. 

Has ICD9 code for diagnoses.

I think the dataset could allow for HLM, with patients nested in agencies.

### Some ideas
Logit model for whether or not a patient has any advice directive. Then modeling the probabiity of having the different kinds of advance directives over length of stay in home health context,  

### Variables of interest PHTYPE=1, home health patients

CASEID, PATNUM, TAGYNUM, TOTCDDX, CDDX1, TOTALRX, LOSHH, READMSS, SEX, AGEATINT, HISPAN, RACEASIA, RACEBLCK, RACEWHT, MARSTAT, VETERAN, MCARENR, MCAIDNR, ANYADDIR, LIVINGWL, DNR, NOHOSP, FEEDRES, MEDRES, COMFORT, POWER, ORGANDON, NOADVANC, INPATIENT, PREHOME, POSTHOME, LIVSMHOS

### Variables of interest PHTYPE=2, hospice discharges
CASEID, PATNUM, TAGYNUM, TOTCDDX, CDDX1, TOTALRX, READMSS, HOSPICEDAYS, DECEASED, DISCHARG, WHRDISCH, SEX, AGEATDIS, HISPAN, RACEASIA, RACEBLCK, RACEWHT, MARSTAT, VETERAN, MCARENR, MCAIDNR, ANYADDIR, LIVINGWL, DNR, NOHOSP, FEEDRES, MEDRES, COMFORT, POWER, ORGANDON, NOADVANC, INPATIENT, PREHOME, LIVSMHOS, PREHOSP, POSTHOSP

### Factor Variable recodes
MARSTAT

### Binary Variable recodes
READMSS, SEX, AGEATINT, HISPAN, RACEASIA, RACEBLCK, RACEWHT, MARSTAT, VETERAN, MCARENR, MCAIDNR, ANYADDIR, LIVINGWL, DNR, NOHOSP, FEEDRES, MEDRES, COMFORT, POWER, ORGANDON, NOADVANC, INPATIENT, PREHOME, POSTHOME, LIVSMHOS

DECEASED, DISCHARG, WHRDISCH, SEX, AGEATDIS, HISPAN, RACEASIA, RACEBLCK, RACEWHT, MARSTAT, VETERAN, MCARENR, MCAIDNR, ANYADDIR, LIVINGWL, DNR, NOHOSP, FEEDRES, MEDRES, COMFORT, POWER, ORGANDON, NOADVANC, INPATIENT, PREHOME, LIVSMHOS, PREHOSP, POSTHOSP

### Model home

ANYADDIR ~ LOSHH + READMSS + SEX + AGEATINT + VETERAN

### Write-up outline

Intro: What are advance care plans? Their importance in medical decision making at the end of life?
Patient preferences at the end of life? What are the default procedures if individuals do not specify alternatives in the advance care directive?

Data: CDC data from the NHHCS, how it was collected, who it contains, the difference between home health and discharged hospice patients?
The measures of interest -> hypotheses -> the model

logit for whether indivdiual has ANY advance directive
What sort of advance directives do people have when they have any advance directive

look at the top ten ICD codes, what sort of things do they have as advance directives.

Results: Brilliant writeup

Figures: Some cool figures.

Prevalence of Advance Directives in the United States: A Systematic Review and Meta-analysis
