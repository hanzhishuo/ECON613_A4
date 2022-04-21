library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(sampleSelection)
options(scipen=999)
indpath = "D:/duke/ECON613/A4/Data"

assign(paste0("dat_A4"), fread(paste0(indpath,"/","dat_A4.csv")))

#Exercise 1 Preparing the Data
#Q1
#create age variable
dat_A4 <- dat_A4 %>%
  mutate(age = 2019 - KEY_BDATE_Y_1997)

#create work_exp variable
dat_A4$CV_WKSWK_JOB_DLI.01_2019[is.na(dat_A4$CV_WKSWK_JOB_DLI.01_2019)] = 0
dat_A4$CV_WKSWK_JOB_DLI.02_2019[is.na(dat_A4$CV_WKSWK_JOB_DLI.02_2019)] = 0
dat_A4$CV_WKSWK_JOB_DLI.03_2019[is.na(dat_A4$CV_WKSWK_JOB_DLI.03_2019)] = 0
dat_A4$CV_WKSWK_JOB_DLI.04_2019[is.na(dat_A4$CV_WKSWK_JOB_DLI.04_2019)] = 0
dat_A4$CV_WKSWK_JOB_DLI.05_2019[is.na(dat_A4$CV_WKSWK_JOB_DLI.05_2019)] = 0
dat_A4$CV_WKSWK_JOB_DLI.06_2019[is.na(dat_A4$CV_WKSWK_JOB_DLI.06_2019)] = 0
dat_A4$CV_WKSWK_JOB_DLI.07_2019[is.na(dat_A4$CV_WKSWK_JOB_DLI.07_2019)] = 0
dat_A4$CV_WKSWK_JOB_DLI.08_2019[is.na(dat_A4$CV_WKSWK_JOB_DLI.08_2019)] = 0
dat_A4$CV_WKSWK_JOB_DLI.09_2019[is.na(dat_A4$CV_WKSWK_JOB_DLI.09_2019)] = 0
dat_A4$CV_WKSWK_JOB_DLI.10_2019[is.na(dat_A4$CV_WKSWK_JOB_DLI.10_2019)] = 0
dat_A4$CV_WKSWK_JOB_DLI.11_2019[is.na(dat_A4$CV_WKSWK_JOB_DLI.11_2019)] = 0

dat_A4 <- dat_A4 %>%
  mutate(work_exp = (CV_WKSWK_JOB_DLI.01_2019 + CV_WKSWK_JOB_DLI.02_2019 + CV_WKSWK_JOB_DLI.03_2019 + 
                       CV_WKSWK_JOB_DLI.04_2019 + CV_WKSWK_JOB_DLI.05_2019 + CV_WKSWK_JOB_DLI.06_2019 + 
                       CV_WKSWK_JOB_DLI.07_2019 + CV_WKSWK_JOB_DLI.08_2019 + CV_WKSWK_JOB_DLI.09_2019 + 
                       CV_WKSWK_JOB_DLI.10_2019 + CV_WKSWK_JOB_DLI.11_2019)/52 )

#Q2
#create education variables
#BIO DAD
#dat_A4$CV_HGC_BIO_DAD_1997[is.na(dat_A4$CV_HGC_BIO_DAD_1997)] = 0
dat_A4$CV_HGC_BIO_DAD_1997[dat_A4$CV_HGC_BIO_DAD_1997 == 95] = 0

dat_A4 <- dat_A4 %>%
  mutate(bio_dad_schooling = CV_HGC_BIO_DAD_1997)



#BIO MOM
#dat_A4$CV_HGC_BIO_MOM_1997[is.na(dat_A4$CV_HGC_BIO_MOM_1997)] = 0
dat_A4$CV_HGC_BIO_MOM_1997[dat_A4$CV_HGC_BIO_MOM_1997 == 95] = 0

dat_A4 <- dat_A4 %>%
  mutate(bio_mom_schooling = CV_HGC_BIO_MOM_1997)

#RES DAD
#dat_A4$CV_HGC_RES_DAD_1997[is.na(dat_A4$CV_HGC_RES_DAD_1997)] = 0
dat_A4$CV_HGC_RES_DAD_1997[dat_A4$CV_HGC_RES_DAD_1997 == 95] = 0

dat_A4 <- dat_A4 %>%
  mutate(res_dad_schooling = CV_HGC_RES_DAD_1997)


#RES MOM
#dat_A4$CV_HGC_RES_MOM_1997[is.na(dat_A4$CV_HGC_RES_MOM_1997)] = 0
dat_A4$CV_HGC_RES_MOM_1997[dat_A4$CV_HGC_RES_MOM_1997 == 95] = 0

dat_A4 <- dat_A4 %>%
  mutate(res_mom_schooling = CV_HGC_RES_MOM_1997)

#ind highest degree

dat_A4$ind_schooling = 0
dat_A4$ind_schooling[dat_A4$YSCH.3113_2019 == 1] = 0
dat_A4$ind_schooling[dat_A4$YSCH.3113_2019 == 2] = 4
dat_A4$ind_schooling[dat_A4$YSCH.3113_2019 == 3] = 12
dat_A4$ind_schooling[dat_A4$YSCH.3113_2019 == 4] = 14
dat_A4$ind_schooling[dat_A4$YSCH.3113_2019 == 5] = 16
dat_A4$ind_schooling[dat_A4$YSCH.3113_2019 == 6] = 18
dat_A4$ind_schooling[dat_A4$YSCH.3113_2019 == 7] = 23
dat_A4$ind_schooling[dat_A4$YSCH.3113_2019 == 8] = 22


#Q3
#plot income data
income_plot <- dat_A4 %>%
  select(YINC_1700_2019, age, KEY_SEX_1997, CV_BIO_CHILD_HH_U18_2019) %>%
  filter(YINC_1700_2019 > 0)

boxplot(YINC_1700_2019~age, data=income_plot)
boxplot(YINC_1700_2019~KEY_SEX_1997, data=income_plot) 

income_plot <- income_plot %>%
  filter(!is.na(CV_BIO_CHILD_HH_U18_2019))

boxplot(YINC_1700_2019~CV_BIO_CHILD_HH_U18_2019, data=income_plot)


#table the share of "0" income
income_age <- dat_A4 %>%
  select(YINC_1700_2019, age) %>%
  filter(!is.na(YINC_1700_2019)) %>%
  group_by(age) %>%
  mutate(count_0 = sum(YINC_1700_2019 ==0)) %>%
  mutate(count = n()) %>%
  mutate(share = count_0/count) %>%
  ungroup()%>%
  distinct(age, .keep_all = TRUE) %>%
  select(age, share)

income_gender <- dat_A4 %>%
  select(YINC_1700_2019, KEY_SEX_1997) %>%
  filter(!is.na(YINC_1700_2019)) %>%
  group_by(KEY_SEX_1997) %>%
  mutate(count_0 = sum(YINC_1700_2019 ==0)) %>%
  mutate(count = n()) %>%
  mutate(share = count_0/count) %>%
  ungroup()%>%
  distinct(KEY_SEX_1997, .keep_all = TRUE) %>%
  select(KEY_SEX_1997, share)

income_child <- dat_A4 %>%
  select(YINC_1700_2019, CV_BIO_CHILD_HH_U18_2019) %>%
  filter(!is.na(YINC_1700_2019)) %>%
  filter(!is.na(CV_BIO_CHILD_HH_U18_2019)) %>%
  group_by(CV_BIO_CHILD_HH_U18_2019) %>%
  mutate(count_0 = sum(YINC_1700_2019 ==0)) %>%
  mutate(count = n()) %>%
  mutate(share = count_0/count) %>%
  ungroup()%>%
  distinct(CV_BIO_CHILD_HH_U18_2019, .keep_all = TRUE) %>%
  select(CV_BIO_CHILD_HH_U18_2019, share)

income_mar <- dat_A4 %>%
  select(YINC_1700_2019, CV_MARSTAT_COLLAPSED_2019) %>%
  filter(!is.na(YINC_1700_2019)) %>%
  filter(!is.na(CV_MARSTAT_COLLAPSED_2019)) %>%
  group_by(CV_MARSTAT_COLLAPSED_2019) %>%
  mutate(count_0 = sum(YINC_1700_2019 ==0)) %>%
  mutate(count = n()) %>%
  mutate(share = count_0/count) %>%
  ungroup()%>%
  distinct(CV_MARSTAT_COLLAPSED_2019, .keep_all = TRUE) %>%
  select(CV_MARSTAT_COLLAPSED_2019, share)


#interpret



#Exercise 2 Heckman Selection Model

#age, work_exp, bio_dad_schooling, bio_mom_schooling, res_dad_schooling, res_mom_schooling
#ind_schooling

df_clean <- dat_A4 %>%
  filter(!is.na(bio_dad_schooling)) %>%
  filter(!is.na(bio_mom_schooling)) %>%
  filter(!is.na(res_dad_schooling)) %>%
  filter(!is.na(res_mom_schooling)) %>%
  filter(!is.na(YSCH.3113_2019)) 


df_clean$observed = df_clean$YINC_1700_2019 > 0
df_clean$observed[is.na(df_clean$YINC_1700_2019)] = FALSE


observed = df_clean$YINC_1700_2019 > 0
observed[is.na(df_clean$YINC_1700_2019)] = FALSE


# Q1: Specify and estimate an OLS model to explain the income variable(where income is positive)

lm_obs = lm(YINC_1700_2019 ~ age + work_exp + ind_schooling, data = df_clean[observed, ])

summary(lm_obs)



# Q2: Explain why the Heckman model can deal with the selection problem

#Heckman selection
#Step1: probit

probit = glm(observed ~ age + work_exp + bio_dad_schooling + bio_mom_schooling +
               res_dad_schooling + res_mom_schooling + 
               ind_schooling, data = df_clean, family = binomial(link = 'probit'))

summary(probit)

#inverse mills ratio
probit_predict = predict(probit)
mills = dnorm(probit_predict)/pnorm(probit_predict)
imr = mills[observed]

#Step2: Linear regression

lm_select = lm(YINC_1700_2019 ~ age + work_exp + ind_schooling + imr, data = df_clean[observed, ])

summary(lm_select)

#likelihood function
heckman_ll <- function(Y, X, Z, observed, par) {
  gamma = par[1:8]
  probit = X %*% gamma
  
  beta = par[9:12]
  lm = Z %*% beta
  
  sigma = par[13]
  rho = par[14]
 
  LH1 = 1-pnorm(probit[!observed])
  LH1[LH1 < 0.000001] = 0.000001
  
  LH2 = dnorm(Y, mean = lm, sd = sigma)
  LH2[LH2 < 0.000001] = 0.000001
  
  LH3 = pnorm((probit[observed] + rho/sigma * (Y - lm))/sqrt(1-rho^2))
  LH3[LH3 < 0.000001] = 0.000001
  
  LH = sum(log(LH1)) - log(sigma) + 
    sum(log(LH2)) + 
    sum(log(LH3))
  
  return(-LH)
}



X = model.matrix(probit)
Z = model.matrix(lm_select)

#generate initial value using package 

select = selection(observed ~ age + work_exp + bio_dad_schooling + bio_mom_schooling + 
                   res_dad_schooling + res_mom_schooling +  ind_schooling, 
                 YINC_1700_2019 ~ age + work_exp + ind_schooling, 
                 data = df_clean, method = '2step')

start = c(coef(select)[1:12], 1,0)

#optimize

heck_optim = optim(start, heckman_ll, X = X, Z = Z[, -5], Y = df_clean$YINC_1700_2019[observed],
                   observed = observed, method = 'BFGS', hessian = T)

#Exercise 3 Censoring
#Q1 plot a histogram
df_tobit <- dat_A4 %>%
  filter(!is.na(YINC_1700_2019)) %>%
  filter(YINC_1700_2019 > 0)

ggplot2::qplot(df_tobit$YINC_1700_2019, geom='histogram')

#propose a model to deal with the censoring problem: Tobit

#Estimate the appropriate model with the censored data


#likelihood function
tobit_ll <- function(Y, X, ul = -Inf, par) {
  sigma = exp(par[length(par)])
  beta = par[-length(par)]
  
  limit = ul
  indicator = Y < ul
  
  lm = X %*% beta
  
  LH = sum(indicator * log((1/sigma)*dnorm((Y-lm)/sigma))) +
    sum((1-indicator) * log(pnorm((lm - limit)/sigma)))
  
  return(-LH)
}



#generate initial value using package 
start_lm = lm(YINC_1700_2019 ~ age + work_exp + ind_schooling, data = df_tobit)
X = model.matrix(start_lm)
start2 = c(coef(start_lm), log_sigma = log(summary(start_lm)$sigma))


tobit_optim = optim(par = start2, tobit_ll, Y = df_tobit$YINC_1700_2019, X = X, ul = 100000,
                    method = 'BFGS')

#Exercise 4 Panel Data
#Explain the potential ability bias when trying to explain to understand the determinants of wages


#Exploit the panel demension of the data to propose a model to correct for the ability bias
assign(paste0("dat_A4_panel"), fread(paste0(indpath,"/","dat_A4_panel.csv")))


panel <- dat_A4_panel %>%
  select(-V1, -KEY_SEX_1997, -KEY_BDATE_M_1997, -KEY_BDATE_Y_1997, -CV_SAMPLE_TYPE_1997, -KEY_RACE_ETHNICITY_1997,
         -CV_HIGHEST_DEGREE_EVER_EDT_2010, -CV_HIGHEST_DEGREE_EVER_EDT_2011, -CV_HIGHEST_DEGREE_EVER_EDT_2013)

panel <- panel %>%
  rename(CV_HIGHEST_DEGREE_1998 = CV_HIGHEST_DEGREE_9899_1998,
         CV_HIGHEST_DEGREE_1999 = CV_HIGHEST_DEGREE_9900_1999,
         CV_HIGHEST_DEGREE_2000 = CV_HIGHEST_DEGREE_0001_2000,
         CV_HIGHEST_DEGREE_2001 = CV_HIGHEST_DEGREE_0102_2001,
         CV_HIGHEST_DEGREE_2002 = CV_HIGHEST_DEGREE_0203_2002,
         CV_HIGHEST_DEGREE_2003 = CV_HIGHEST_DEGREE_0304_2003,
         CV_HIGHEST_DEGREE_2004 = CV_HIGHEST_DEGREE_0405_2004,
         CV_HIGHEST_DEGREE_2005 = CV_HIGHEST_DEGREE_0506_2005,
         CV_HIGHEST_DEGREE_2006 = CV_HIGHEST_DEGREE_0607_2006,
         CV_HIGHEST_DEGREE_2007 = CV_HIGHEST_DEGREE_0708_2007,
         CV_HIGHEST_DEGREE_2008 = CV_HIGHEST_DEGREE_0809_2008,
         CV_HIGHEST_DEGREE_2009 = CV_HIGHEST_DEGREE_0910_2009,
         CV_HIGHEST_DEGREE_2010 = CV_HIGHEST_DEGREE_1011_2010,
         CV_HIGHEST_DEGREE_2011 = CV_HIGHEST_DEGREE_1112_2011,
         CV_HIGHEST_DEGREE_2013 = CV_HIGHEST_DEGREE_1314_2013,
         CV_HIGHEST_DEGREE_2015 = CV_HIGHEST_DEGREE_EVER_EDT_2015,
         CV_HIGHEST_DEGREE_2017 = CV_HIGHEST_DEGREE_EVER_EDT_2017,
         CV_HIGHEST_DEGREE_2019 = CV_HIGHEST_DEGREE_EVER_EDT_2019)

panel_long <- pivot_longer(panel, cols = -PUBID_1997,
                        names_to = c(".value", "year"),
                        names_pattern = "(.*)\\_(\\d+)")

#gen work_exp
panel_long$CV_WKSWK_JOB_DLI.01[is.na(panel_long$CV_WKSWK_JOB_DLI.01)] = 0
panel_long$CV_WKSWK_JOB_DLI.02[is.na(panel_long$CV_WKSWK_JOB_DLI.02)] = 0
panel_long$CV_WKSWK_JOB_DLI.03[is.na(panel_long$CV_WKSWK_JOB_DLI.03)] = 0
panel_long$CV_WKSWK_JOB_DLI.04[is.na(panel_long$CV_WKSWK_JOB_DLI.04)] = 0
panel_long$CV_WKSWK_JOB_DLI.05[is.na(panel_long$CV_WKSWK_JOB_DLI.05)] = 0
panel_long$CV_WKSWK_JOB_DLI.06[is.na(panel_long$CV_WKSWK_JOB_DLI.06)] = 0
panel_long$CV_WKSWK_JOB_DLI.07[is.na(panel_long$CV_WKSWK_JOB_DLI.07)] = 0
panel_long$CV_WKSWK_JOB_DLI.08[is.na(panel_long$CV_WKSWK_JOB_DLI.08)] = 0
panel_long$CV_WKSWK_JOB_DLI.09[is.na(panel_long$CV_WKSWK_JOB_DLI.09)] = 0
panel_long$CV_WKSWK_JOB_DLI.10[is.na(panel_long$CV_WKSWK_JOB_DLI.10)] = 0
panel_long$CV_WKSWK_JOB_DLI.11[is.na(panel_long$CV_WKSWK_JOB_DLI.11)] = 0
panel_long$CV_WKSWK_JOB_DLI.12[is.na(panel_long$CV_WKSWK_JOB_DLI.12)] = 0
panel_long$CV_WKSWK_JOB_DLI.13[is.na(panel_long$CV_WKSWK_JOB_DLI.13)] = 0
panel_long$CV_WKSWK_JOB_DLI.14[is.na(panel_long$CV_WKSWK_JOB_DLI.14)] = 0
panel_long$CV_WKSWK_JOB_DLI.15[is.na(panel_long$CV_WKSWK_JOB_DLI.15)] = 0


panel_long <- panel_long %>%
  mutate(work_exp = CV_WKSWK_JOB_DLI.01 + CV_WKSWK_JOB_DLI.02 + CV_WKSWK_JOB_DLI.03 + 
                       CV_WKSWK_JOB_DLI.04 + CV_WKSWK_JOB_DLI.05 + CV_WKSWK_JOB_DLI.06 + 
                       CV_WKSWK_JOB_DLI.07 + CV_WKSWK_JOB_DLI.08 + CV_WKSWK_JOB_DLI.09 + 
                       CV_WKSWK_JOB_DLI.10 + CV_WKSWK_JOB_DLI.11 + CV_WKSWK_JOB_DLI.12 +
                       CV_WKSWK_JOB_DLI.13 + CV_WKSWK_JOB_DLI.14 + CV_WKSWK_JOB_DLI.15)

#gen year of schooling
panel_long$degree = 0
panel_long$degree[panel_long$CV_HIGHEST_DEGREE == 1] = 4
panel_long$degree[panel_long$CV_HIGHEST_DEGREE == 2] = 12
panel_long$degree[panel_long$CV_HIGHEST_DEGREE == 3] = 14
panel_long$degree[panel_long$CV_HIGHEST_DEGREE == 4] = 16
panel_long$degree[panel_long$CV_HIGHEST_DEGREE == 5] = 18
panel_long$degree[panel_long$CV_HIGHEST_DEGREE == 6] = 23
panel_long$degree[panel_long$CV_HIGHEST_DEGREE == 7] = 22


panel_long <- panel_long %>%
  select(PUBID_1997, year, `YINC-1700`, CV_MARSTAT_COLLAPSED, work_exp, degree) %>%
  na.omit()


#gen marital status dummy
panel_long$marital_0 = 0
panel_long$marital_0[panel_long$CV_MARSTAT_COLLAPSED == 0] = 1

panel_long$marital_1 = 0
panel_long$marital_1[panel_long$CV_MARSTAT_COLLAPSED == 1] = 1

panel_long$marital_2 = 0
panel_long$marital_2[panel_long$CV_MARSTAT_COLLAPSED == 2] = 1

panel_long$marital_3 = 0
panel_long$marital_3[panel_long$CV_MARSTAT_COLLAPSED == 3] = 1

panel_long$marital_4 = 0
panel_long$marital_4[panel_long$CV_MARSTAT_COLLAPSED == 4] = 1

panel_long <- panel_long %>%
  select(-CV_MARSTAT_COLLAPSED)


#Within estimator
panel_within <- panel_long %>%
  group_by(PUBID_1997) %>%
  mutate(YINC_bar = mean(`YINC-1700`),
         work_bar = mean(work_exp),
         degree_bar = mean(degree),
         mar1_bar = mean(marital_1),
         mar2_bar = mean(marital_2),
         mar3_bar = mean(marital_3),
         mar4_bar = mean(marital_4)) %>%
  mutate(YINC_within = `YINC-1700` - YINC_bar,
         work_within = work_exp - work_bar,
         degree_within = degree - degree_bar,
         mar1_within = marital_1 - mar1_bar,
         mar2_within = marital_2 - mar2_bar,
         mar3_within = marital_3 - mar3_bar,
         mar4_within = marital_4 - mar4_bar,)

within = lm(YINC_within ~ 0 + work_within + degree_within + mar1_within + mar2_within + 
                 mar3_within + mar4_within, data = panel_within)

summary(within)
#between estimator
panel_between <- panel_long %>%
  group_by(PUBID_1997) %>%
  mutate(YINC_between = mean(`YINC-1700`),
         work_between = mean(work_exp),
         degree_between = mean(degree),
         mar1_between = mean(marital_1),
         mar2_between = mean(marital_2),
         mar3_between = mean(marital_3),
         mar4_between = mean(marital_4))

between = lm(YINC_between ~ 0 + work_between + degree_between + mar1_between + mar2_between + 
              mar3_between + mar4_between, data = panel_between)

summary(between)


#difference estimator
panel_diff <- panel_long %>%
  group_by(PUBID_1997) %>%
  mutate(YINC_lag = lag(`YINC-1700`, n = 1L),
         work_lag = lag(work_exp, n = 1L),
         degree_lag = lag(degree, n = 1L),
         mar1_lag = lag(marital_1, n = 1L),
         mar2_lag = lag(marital_2, n = 1L),
         mar3_lag = lag(marital_3, n = 1L),
         mar4_lag = lag(marital_4, n = 1L)) %>%
  mutate(YINC_diff = `YINC-1700` - YINC_lag,
         work_diff = work_exp - work_lag,
         degree_diff = degree - degree_lag,
         mar1_diff = marital_1 - mar1_lag,
         mar2_diff = marital_2 - mar2_lag,
         mar3_diff = marital_3 - mar3_lag,
         mar4_diff = marital_4 - mar4_lag,)

difference = lm(YINC_diff ~ 0 + work_diff + degree_diff + mar1_diff + mar2_diff + 
              mar3_diff + mar4_diff, data = panel_diff)

summary(difference)