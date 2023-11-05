library(readxl)
library(survival)
library(mstate)
library(haven)

##### Pattren A #######
######## A semi-parametric model for continuous variables #####
#### Data preparation and description----
# long format dataset
msebmt <- read.csv("msebmt_all.csv")
# covirates
covs_P <-   c("sex","age","BMI","educ","smoking_status","healthy_alcohol"
              ,"healthy_PA","Ethnic","employment_fac","income_fac"
              )
# transition matrix
tmat <- transMat(x = list(c(2,4), c(3,4), c(4),c()),
                 names = c("Baseline", "FCMD", "CMM", "Death"))
tmat

#### The semi-parametric model----
allname <- c( 'kdm_advance','phenoage_advance','frailty_5q') # input indicators
covs_allname <- c(covs_P, allname[i])
# expand.covs expands the dummy variables associated with these covariates
msebmt_allname <- expand.covs(msebmt,  covs_allname, append = TRUE,longnames = FALSE)
# run the model
#.1-.5 refers to five transitions
the.expression_allname <-  paste0('fit_allname <- coxph(Surv(Tstart, Tstop, status)~ ',
                                    allname[i], '.1 + ',
                                    allname[i], '.2 + ',
                                    allname[i], '.3 + ',
                                    allname[i], '.4 + ',
                                    allname[i], '.5 + ',
                                    "sex.1+sex.2+sex.3+sex.4+sex.5+",
                                    "smoking_status1.1+smoking_status1.2+smoking_status1.3+ smoking_status1.4+ smoking_status1.5+smoking_status2.1+smoking_status2.2+",
                                    "smoking_status2.3+ smoking_status2.4+ smoking_status2.5+",
                                    "educ.1+ educ.2+educ.3+educ.4+educ.5+",
                                    "age.1 +age.2+ age.3 +age.4 +age.5 +",
                                    "BMI.1 + BMI.2+ BMI.3+ BMI.4+ BMI.5 +",
                                    "employment_fac.1+ employment_fac.2+ employment_fac.3+ employment_fac.4+ employment_fac.5+",
                                    "income_fac.1+ income_fac.2+ income_fac.3+ income_fac.4+ income_fac.5+",
                                    "healthy_alcohol.1+ healthy_alcohol.2+healthy_alcohol.3+ healthy_alcohol.4+ healthy_alcohol.5+",
                                    "healthy_PA.1+healthy_PA.2+ healthy_PA.3 +healthy_PA.4+ healthy_PA.5+",
                                    "Ethnic.1+Ethnic.2+Ethnic.3+Ethnic.4+Ethnic.5+",
                                    "+strata(trans), data = msebmt_allname, method = 'breslow')"
  )

  eval(parse(text = the.expression_allname)) # run the expression
  results_allname <- summary(fit_allname)
  Para_allname <- data.frame(name =allname[i],
                                  beta= results_allname$coefficients[c(1:5),2],
                                  lower=results_allname$conf.int[c(1:5),3],
                                  upper=results_allname$conf.int[c(1:5),4],
                                  se=results_allname$coefficients[c(1:5),3],
                                  p=results_allname$coefficients[c(1:5),5],
                                  cindex=results_allname$concordance[1],
                                  cindexse=results_allname$concordance[2] )

Para_allname$re <- paste0(sprintf('%.2f',Para_allname$beta)," (",sprintf('%.2f',Para_allname$lower),", ",sprintf('%.2f',Para_allname$upper),")")
Para_allname %>% relocate(re,p,beta,lower,upper) -> Para_allname

######PATTERN B ######
######## A semi-parametric model for categorical variables #####
#### Data preparation and description----
#### Load data #####
msebmttt <- read.csv("msebmttt_all.csv")
# transition matrix
tmatt <- transMat(x = list(c(2,3,4,6), c(5,6), c(5,6),c(5,6),c(6),c()),
                  names = c("Baseline", "IHD","T2D","STROKE", "CMM", "Death"))
tmatt
# covirates
covs_P <- c("sex","age","BMI","educ","smoking_status","healthy_alcohol"
            ,"healthy_PA","Ethnic","Qualifications","employment_fac","income_fac"
            )

#### The semi-parametric model----
allname <- 'frailty_3q'
i=1
#.1-.11 refers to eleven transitions
covs_bio <- c(covs_P, allname[i])
msebmtt_bio <- expand.covs(msebmttt,  covs_bio, append = TRUE,longnames = FALSE)

the.expression_bio <-  paste0('fit_bio <- coxph(Surv(Tstart, Tstop, status)~ ',
                                allname[i], '1.1 + ',
                                allname[i], '1.2 + ',
                                allname[i], '1.3 + ',
                                allname[i], '1.4 + ',
                                allname[i], '1.5 + ',
                                allname[i], '1.6 + ',
                                allname[i], '1.7 + ',
                                allname[i], '1.8 + ',
                                allname[i], '1.9 + ',
                                allname[i], '1.10 + ',
                                allname[i], '1.11 + ',
                                allname[i], '2.1 + ',
                                allname[i], '2.2 + ',
                                allname[i], '2.3 + ',
                                allname[i], '2.4 + ',
                                allname[i], '2.5 + ',
                                allname[i], '2.6 + ',
                                allname[i], '2.7 + ',
                                allname[i], '2.8 + ',
                                allname[i], '2.9 + ',
                                allname[i], '2.10 + ',
                                allname[i], '2.11 + ',
                                "sex.1+sex.2+sex.3+sex.4+sex.5+sex.6+sex.7+sex.8+sex.9 + sex.10+ sex.11+",
                                "smoking_status1.1+smoking_status1.2+smoking_status1.3+ smoking_status1.4+ smoking_status1.5+smoking_status1.6+smoking_status1.7+smoking_status1.8+ smoking_status1.9+ smoking_status1.10+ smoking_status1.11+",
                                "smoking_status2.1+smoking_status2.2+smoking_status2.3+ smoking_status2.4+ smoking_status2.5+smoking_status2.6+smoking_status2.7+smoking_status2.8+ smoking_status2.9+ smoking_status2.10+ smoking_status2.11+",
                                "educ.1+educ.2+educ.3+educ.4+educ.5 +educ.6+educ.7+educ.8+educ.9+educ.10+educ.11+",
                                "age.1 +age.2+ age.3 +age.4 +age.5 + age.6 +age.7+ age.8 +age.9 +age.10 +age.11 +",
                                "BMI.1  + BMI.2+ BMI.3+ BMI.4+ BMI.5 +BMI.6  + BMI.7+ BMI.8+ BMI.9+ BMI.10 +BMI.11 +",
                                "healthy_alcohol.1+ healthy_alcohol.2+healthy_alcohol.3+ healthy_alcohol.4+ healthy_alcohol.5+",
                                "healthy_alcohol.6+ healthy_alcohol.7+healthy_alcohol.8+ healthy_alcohol.9+ healthy_alcohol.10+healthy_alcohol.11+",
                                "healthy_PA.1+healthy_PA.2+ healthy_PA.3 +healthy_PA.4+ healthy_PA.5+",
                                "healthy_PA.6+healthy_PA.7+ healthy_PA.8 +healthy_PA.9+ healthy_PA.10+healthy_PA.11+",
                                "employment_fac.1+ employment_fac.2+ employment_fac.3+ employment_fac.4+ employment_fac.5+employment_fac.6+ employment_fac.7+ employment_fac.8+ employment_fac.9+ employment_fac.10+employment_fac.11+",
                                "income_fac.1+ income_fac.2+ income_fac.3+ income_fac.4+ income_fac.5+  income_fac.6+ income_fac.7+ income_fac.8+ income_fac.9+ income_fac.10+income_fac.11+",
                                "Ethnic.1+Ethnic.2+Ethnic.3+Ethnic.4+Ethnic.5+Ethnic.6+Ethnic.7+Ethnic.8+Ethnic.9+Ethnic.10+Ethnic.11+",
                                "strata(trans), data = msebmtt_bio, method = 'breslow')"
  )

  eval(parse(text = the.expression_bio)) # run the expression
  results_bio <- summary(fit_bio)
  parameter_bio <- data.frame(bioage_name =allname[i],
                              beta= results_bio$coefficients[c(1:22),2],
                              lower=results_bio$conf.int[c(1:22),3],
                              upper=results_bio$conf.int[c(1:22),4],
                              se=results_bio$coefficients[c(1:22),3],
                              p=results_bio$coefficients[c(1:22),5],
                              cindex=results_bio$concordance[1],
                              cindexse=results_bio$concordance[2]
  )


Para_bio$re <- paste0(sprintf('%.2f',Para_bio$beta)," ( ",sprintf('%.2f',Para_bio$lower),", ",sprintf('%.2f',Para_bio$upper)," )")
Para_bio %>%
  relocate(re,p,beta,lower,upper) -> Para_bio

