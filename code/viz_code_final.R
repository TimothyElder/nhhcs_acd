#Requires B_sim object from the v2_assessment_final.R code. 

## ------------- Viz for difference in binary IVs --------------- ##
# define the range of intererest
age.range <- seq(min(home$AGEATINT), max(home$AGEATINT), length=ROWS)

# Focal Variable is SEX -----
x.base.men <- c(1,                     # intercept
                     1,                     # for men
                     mean(home$AGEATINT),   # mean age
                     mean(home$COGNFUNC),                     # mean cognitive impairment
                     0,                     # Unmaarried
                     0,                     # Non-Terminal Patients
                     mean(home$log_LOSHH),    # mean of log Length of Stay
                     0,                     # Not Readmitted
                     mean(home$TOTCDDX),    # Mean number of diagnoses
                     mean(home$TOTALRX),    # Mean number of medication
                     mean(home$TOTPROC),    # Mean number of Procedures
                     0)                     # Not-Enrolled in medicaid

#create matrix of first binary condition by the number of columns in the above specified range, eqaul to the length of ROWS
x.base.men <- matrix(x.base.men, nrow=length(x.base.men), ncol=length(age.range))

#insert the the same range in to the proper coefficient position, this will change if model changes and should be the same for both matrices
x.base.men[3,] <- age.range #replacing with different capability values

# Create the matrix baseline for the second binary condition, same positon as above but should be the opposite value, 0 or 1
x.base.women <- c(1,                     # intercept
                 0,                     # for women
                 mean(home$AGEATINT),   # mean age
                 mean(home$COGNFUNC),                     # mean cognitive impairment
                 0,                     # Unmaarried
                 0,                     # Non-Terminal Patients
                 mean(home$log_LOSHH),    # mean of log Length of Stay
                 0,                     # Not Readmitted
                 mean(home$TOTCDDX),    # Mean number of diagnoses
                 mean(home$TOTALRX),    # Mean number of medication
                 mean(home$TOTPROC),    # Mean number of Procedures
                 0)                     # Not-Enrolled in medicaid

x.base.women <- matrix(x.base.women, nrow=length(x.base.women), ncol=length(age.range))

x.base.women[3,] <- age.range #replacing with different wage values

#matrix multiplication by the simulated coefficients
s.men <- inv.logit(B.sim %*% x.base.men) #matrix of predicted probabilities
s.women <- inv.logit(B.sim %*% x.base.women) #matrix of predicted probabilities

#collapse to get quantiles
s.men<-apply(s.men, 2, quantile, c(0.025, 0.5, .975))
s.women<-apply(s.women, 2, quantile, c(0.025, 0.5, .975))

# extract the quantiles and name for plotting
PrMen.lo <- s.men[1,]
PrMen.med <- s.men[2,]
PrMen.hi <- s.men[3,]

# extract and name rows
PrWomen.lo <- s.women[1,]
PrWomen.med <- s.women[2,]
PrWomen.hi <- s.women[3,]

#pack it all into a df to then plot
df <- tibble(age.range, PrMen.lo, PrMen.med, PrMen.hi, PrWomen.lo, PrWomen.med, PrWomen.hi)

#Plotting
line.men.v.women <- ggplot(df, aes(x=age.range)) +
  geom_line(aes(y=PrMen.med)) +
  geom_ribbon(aes(ymin=PrMen.lo, ymax=PrMen.hi), fill = "red", alpha=.2) +
  geom_line(aes(y=PrWomen.med)) +
  geom_ribbon(aes(ymin=PrWomen.lo, ymax=PrWomen.hi), fill = "blue", alpha=.2) +
  xlab("Age") +
  ylab("Probability of Having Advance Directive") +
  annotate("text", x=23, y=.25, label="Men") +
  annotate("text", x=60, y=.15, label="Women")

gender <- line.men.v.women + labs(title = "Probability of Advance Directive by Age and Gender")

ggsave("figures/ad_men_women_line.pdf", gender)


# Focal Variable is TERMINAL  ----
x.base.terminal <- c(1,                     # intercept
                  0,                     # for women
                  mean(home$AGEATINT),   # mean age
                  mean(home$COGNFUNC),                     # mean cognitive impairment
                  0,                     # Unmaarried
                  1,                     # Non-Terminal Patients
                  mean(home$log_LOSHH),    # mean of log Length of Stay
                  0,                     # Not Readmitted
                  mean(home$TOTCDDX),    # Mean number of diagnoses
                  mean(home$TOTALRX),    # Mean number of medication
                  mean(home$TOTPROC),    # Mean number of Procedures
                  0)                     # Not-Enrolled in medicaid

#create matrix of first binary condition by the number of columns in the above specified range, eqaul to the length of ROWS
X.base.terminal <- matrix(x.base.terminal, nrow=length(x.base.terminal), ncol=length(age.range))

#insert the the same range in to the proper coefficient position, this will change if model changes and should be the same for both matrices
X.base.terminal[3,] <- age.range #replacing with different capability values

# Create the matrix baseline for the second binary condition, same positon as above but should be the opposite value, 0 or 1
x.base.notterm <- c(1,                     # intercept
                     0,                     # for women
                     mean(home$AGEATINT),   # mean age
                     mean(home$COGNFUNC),                     # mean cognitive impairment
                     0,                     # Unmaarried
                     0,                     # Non-Terminal Patients
                     mean(home$log_LOSHH),    # mean of log Length of Stay
                     0,                     # Not Readmitted
                     mean(home$TOTCDDX),    # Mean number of diagnoses
                     mean(home$TOTALRX),    # Mean number of medication
                     mean(home$TOTPROC),    # Mean number of Procedures
                     0)                     # Not-Enrolled in medicaid

X.base.notterm <- matrix(x.base.notterm, nrow=length(x.base.notterm), ncol=length(age.range))

X.base.notterm[3,] <- age.range #replacing with different wage values

#matrix multiplication by the simulated coefficients
s.terminal <- inv.logit(B.sim %*% X.base.terminal) #matrix of predicted probabilities
s.notterm <- inv.logit(B.sim %*% X.base.notterm) #matrix of predicted probabilities

#collapse to get quantiles
s.terminal<-apply(s.terminal, 2, quantile, c(0.025, 0.5, .975))
s.notterm<-apply(s.notterm, 2, quantile, c(0.025, 0.5, .975))

# extract the quantiles and name for plotting
PrTerminal.lo <- s.terminal[1,]
PrTerminal.med <- s.terminal[2,]
PrTerminal.hi <- s.terminal[3,]

# extract and name rows
PrNotTerminal.lo <- s.notterm[1,]
PrNotTerminal.med <- s.notterm[2,]
PrNotTerminal.hi <- s.notterm[3,]

#pack it all into a df to then plot
df <- tibble(age.range, PrTerminal.lo, PrTerminal.med, PrTerminal.hi, PrNotTerminal.lo, PrNotTerminal.med, PrNotTerminal.hi)

#Plotting
line.term.v.nonterm <- ggplot(df, aes(x=age.range)) +
  geom_line(aes(y=PrTerminal.med)) +
  geom_ribbon(aes(ymin=PrTerminal.lo, ymax=PrTerminal.hi), fill = "red", alpha=.2) +
  geom_line(aes(y=PrNotTerminal.med)) +
  geom_ribbon(aes(ymin=PrNotTerminal.lo, ymax=PrNotTerminal.hi), fill = "blue", alpha=.2) +
  xlab("Age") +
  ylab("Probability of Having Advance Directive") +
  annotate("text", x=40, y=.4, label="Terminal") +
  annotate("text", x=50, y=.15, label="Not-Terminal")

terminal <- line.term.v.nonterm + labs(title = "Probability of Advance Directive by Age and Prognosis")

ggsave("figures/ad_terminal_not_line.pdf", terminal)


# Focal Variable is MARRIED -----
x.base.married <- c(1,                     # intercept
                0,                     # for wommen
                mean(home$AGEATINT),   # mean age
                mean(home$COGNFUNC),                     # mean cognitive impairment
                1,                     # Unmaarried
                0,                     # Non-Terminal Patients
                mean(home$log_LOSHH),    # mean of log Length of Stay
                0,                     # Not Readmitted
                mean(home$TOTCDDX),    # Mean number of diagnoses
                mean(home$TOTALRX),    # Mean number of medication
                mean(home$TOTPROC),    # Mean number of Procedures
                0)                     # Not-Enrolled in medicaid

#create matrix of first binary condition by the number of columns in the above specified range, eqaul to the length of ROWS
x.base.married <- matrix(x.base.married, nrow=length(x.base.married), ncol=length(age.range))

#insert the the same range in to the proper coefficient position, this will change if model changes and should be the same for both matrices
x.base.married[3,] <- age.range #replacing with different capability values

# Create the matrix baseline for the second binary condition, same positon as above but should be the opposite value, 0 or 1
x.base.notmarried <- c(1,                     # intercept
                    0,                     # for wommen
                    mean(home$AGEATINT),   # mean age
                    mean(home$COGNFUNC),                     # mean cognitive impairment
                    0,                     # Unmaarried
                    0,                     # Non-Terminal Patients
                    mean(home$log_LOSHH),    # mean of log Length of Stay
                    0,                     # Not Readmitted
                    mean(home$TOTCDDX),    # Mean number of diagnoses
                    mean(home$TOTALRX),    # Mean number of medication
                    mean(home$TOTPROC),    # Mean number of Procedures
                    0)                     # Not-Enrolled in medicaid

x.base.notmarried <- matrix(x.base.notmarried, nrow=length(x.base.notmarried), ncol=length(age.range))

x.base.notmarried[3,] <- age.range #replacing with different wage values

#matrix multiplication by the simulated coefficients
s.married <- inv.logit(B.sim %*% x.base.married) #matrix of predicted probabilities
s.notmarried <- inv.logit(B.sim %*% x.base.notmarried) #matrix of predicted probabilities

#collapse to get quantiles
s.married<-apply(s.married, 2, quantile, c(0.025, 0.5, .975))
s.notmarried<-apply(s.notmarried, 2, quantile, c(0.025, 0.5, .975))

# extract the quantiles and name for plotting
PrMarried.lo <- s.married[1,]
PrMarried.med <- s.married[2,]
PrMarried.hi <- s.married[3,]

# extract and name rows
PrNotMarried.lo <- s.notmarried[1,]
PrNotMarried.med <- s.notmarried[2,]
PrNotMarried.hi <- s.notmarried[3,]

#pack it all into a df to then plot
df <- tibble(age.range, PrMarried.lo, PrMarried.med, PrMarried.hi, PrNotMarried.lo, PrNotMarried.med, PrNotMarried.hi)

#Plotting
line.married.v.notmarried <- ggplot(df, aes(x=age.range)) +
  geom_line(aes(y=PrMarried.med)) +
  geom_ribbon(aes(ymin=PrMarried.lo, ymax=PrMarried.hi), fill = "red", alpha=.2) +
  geom_line(aes(y=PrNotMarried.med)) +
  geom_ribbon(aes(ymin=PrNotMarried.lo, ymax=PrNotMarried.hi), fill = "blue", alpha=.2) +
  xlab("Age") +
  ylab("Probability of Having Advance Directive") +
  annotate("text", x=23, y=.25, label="Married") +
  annotate("text", x=50, y=.15, label="Not-Married")

marstat <- line.married.v.notmarried + labs(title = "Probability of Advance Directive by Age and Marriage Status")

ggsave("figures/ad_married_not_married_line.pdf", marstat)


# Focal Variable is MEDICAIDENR -----
x.base.medicaid <- c(1,                     # intercept
                    0,                     # for wommen
                    mean(home$AGEATINT),   # mean age
                    mean(home$COGNFUNC),                     # mean cognitive impairment
                    0,                     # Unmaarried
                    0,                     # Non-Terminal Patients
                    mean(home$log_LOSHH),    # mean of log Length of Stay
                    0,                     # Not Readmitted
                    mean(home$TOTCDDX),    # Mean number of diagnoses
                    mean(home$TOTALRX),    # Mean number of medication
                    mean(home$TOTPROC),    # Mean number of Procedures
                    1)                     # Enrolled in medicaid

#create matrix of first binary condition by the number of columns in the above specified range, eqaul to the length of ROWS
x.base.medicaid <- matrix(x.base.medicaid, nrow=length(x.base.medicaid), ncol=length(age.range))

#insert the the same range in to the proper coefficient position, this will change if model changes and should be the same for both matrices
x.base.medicaid[3,] <- age.range #replacing with different capability values

# Create the matrix baseline for the second binary condition, same positon as above but should be the opposite value, 0 or 1
x.base.nomedicaid <- c(1,                     # intercept
                       0,                     # for wommen
                       mean(home$AGEATINT),   # mean age
                       mean(home$COGNFUNC),                     # mean cognitive impairment
                       0,                     # Unmaarried
                       0,                     # Non-Terminal Patients
                       mean(home$log_LOSHH),    # mean of log Length of Stay
                       0,                     # Not Readmitted
                       mean(home$TOTCDDX),    # Mean number of diagnoses
                       mean(home$TOTALRX),    # Mean number of medication
                       mean(home$TOTPROC),    # Mean number of Procedures
                       0)                     # Not-Enrolled in medicaid

x.base.nomedicaid <- matrix(x.base.nomedicaid, nrow=length(x.base.nomedicaid), ncol=length(age.range))

x.base.nomedicaid[3,] <- age.range #replacing with different wage values

#matrix multiplication by the simulated coefficients
s.medicaid <- inv.logit(B.sim %*% x.base.medicaid) #matrix of predicted probabilities
s.nomedicaid <- inv.logit(B.sim %*% x.base.nomedicaid) #matrix of predicted probabilities

#collapse to get quantiles
s.medicaid<-apply(s.medicaid, 2, quantile, c(0.025, 0.5, .975))
s.nomedicaid<-apply(s.nomedicaid, 2, quantile, c(0.025, 0.5, .975))

# extract the quantiles and name for plotting
PrMedicaid.lo <- s.medicaid[1,]
PrMedicaid.med <- s.medicaid[2,]
PrMedicaid.hi <- s.medicaid[3,]

# extract and name rows
PrNoMedicaid.lo <- s.nomedicaid[1,]
PrNoMedicaid.med <- s.nomedicaid[2,]
PrNoMedicaid.hi <- s.nomedicaid[3,]

#pack it all into a df to then plot
df <- tibble(age.range, PrMedicaid.lo, PrMedicaid.med, PrMedicaid.hi, PrNoMedicaid.lo, PrNoMedicaid.med, PrNoMedicaid.hi)

#Plotting
line.medicaid.v.nomedicaid <- ggplot(df, aes(x=age.range)) +
  geom_line(aes(y=PrMedicaid.med)) +
  geom_ribbon(aes(ymin=PrMedicaid.lo, ymax=PrMedicaid.hi), fill = "red", alpha=.2) +
  geom_line(aes(y=PrNoMedicaid.med)) +
  geom_ribbon(aes(ymin=PrNoMedicaid.lo, ymax=PrNoMedicaid.hi), fill = "blue", alpha=.2) +
  xlab("Age") +
  ylab("Probability of Having Advance Directive") +
  annotate("text", x=23, y=.25, label="Not-Enrolled") +
  annotate("text", x=60, y=.15, label="Enrolled")

medicaid <- line.medicaid.v.nomedicaid + labs(title = "Probability of Advance Directive by Age and Medicaid Status")

ggsave("figures/ad_medicaid_nomedicaid_line.pdf", medicaid)

all.g <- gridExtra::grid.arrange(gender, marstat, medicaid, terminal, ncol=2)


ggsave("figures/all_lines.pdf", all.g)
