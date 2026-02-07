# SARS-CoV-2 seroprevalence of Surinamese children and determinants of seropositivity in the CCREOH/MeKiTamara cohort

library(tidyverse)

# read in data
ImpactsOfCOVID19_Ranalysis <- read.csv("ImpactsOfCOVID19_Ranalysis.csv")

# select necessary columns and factor variables
# num_house is categorical
ImpactsOfCOVID19_Ranalysis <- ImpactsOfCOVID19_Ranalysis %>% 
  select(region_residence, sc_s1results, pos_household, dcovidprot3, 
         childactivities3, mask4, 
         distancing3, mother_vaccine,
         age_mother, age_motherc, num_house, covid_diagnosis, age_child, age_childc, gender_child, 
         ethnicity3, education3, employment2) %>%
  mutate(region_residence = as.factor(region_residence),
         sc_s1results = as.factor(sc_s1results),
         pos_household = as.factor(pos_household),
         dcovidprot3 = as.factor(dcovidprot3),
         childactivities3 = as.factor(childactivities3),
         mask4 = as.factor(mask4),
         distancing3 = as.factor(distancing3),
         mother_vaccine = as.factor(mother_vaccine),
         num_house = as.factor(num_house),
         covid_diagnosis = as.factor(covid_diagnosis), 
         gender_child = as.factor(gender_child), 
         ethnicity3 = as.factor(ethnicity3), 
         education3 = as.factor(education3), 
         employment2 = as.factor(employment2))

# remove rows with NA 
ImpactsOfCOVID19_clean <- na.omit(ImpactsOfCOVID19_Ranalysis)



#################### Label variables ####################

library(Hmisc)

# Adding labels for SARS-CoV-2 Seroprevalence (C2) its levels
ImpactsOfCOVID19_clean$sc_s1results <- factor(ImpactsOfCOVID19_clean$sc_s1results, levels = c(0, 1),
                                              labels = c("Seronegative", "Seropositive"))

label(ImpactsOfCOVID19_clean$sc_s1results) <- "SARS-CoV-2 Seroprevalence (C2)"

# Adding labels for Age of mother in years
label(ImpactsOfCOVID19_clean$age_mother) <- "Age of mother in years"
label(ImpactsOfCOVID19_clean$age_mother)

# Adding labels for Age of mother in years (centered at the mean)
label(ImpactsOfCOVID19_clean$age_motherc) <- "Age of mother in years"
label(ImpactsOfCOVID19_clean$age_motherc)

# Adding labels for Age of child in years
label(ImpactsOfCOVID19_clean$age_child) <- "Age of child in years"
label(ImpactsOfCOVID19_clean$age_child)

# Adding labels for Age of child in years (centered at the mean)
label(ImpactsOfCOVID19_clean$age_childc) <- "Age of child in years"
label(ImpactsOfCOVID19_clean$age_childc)

# Adding labels for gender_child and its levels
ImpactsOfCOVID19_clean$gender_child <- factor(ImpactsOfCOVID19_clean$gender_child, levels = c(0, 1),
                                              labels = c("Female", "Male"))

label(ImpactsOfCOVID19_clean$gender_child) <- "Gender"

# Adding labels for ethnicity3 and its levels
ImpactsOfCOVID19_clean$ethnicity3 <- factor(ImpactsOfCOVID19_clean$ethnicity3, levels = c(1, 2, 3),
                                            labels = c("Asian descent", "African descent", "Other (Indigenous/Amerindian, Mixed, Caucasian, Other)"))

label(ImpactsOfCOVID19_clean$ethnicity3) <- "Ethnicity"

# Adding labels for region_residence and its levels
ImpactsOfCOVID19_clean$region_residence <- factor(ImpactsOfCOVID19_clean$region_residence, levels = c(0, 1),
                                                  labels = c("Paramaribo region", "Nickerie region"))

label(ImpactsOfCOVID19_clean$region_residence) <- "Recruitment region"

# Adding labels for num_house and its levels
ImpactsOfCOVID19_clean$num_house <- factor(ImpactsOfCOVID19_clean$num_house, levels = c(0, 1),
                                           labels = c("4 or fewer", "5 or more"))

label(ImpactsOfCOVID19_clean$num_house) <- "Household size"

# Adding labels for education3 and its levels
ImpactsOfCOVID19_clean$education3 <- factor(ImpactsOfCOVID19_clean$education3, levels = c(0, 1),
                                            labels = c("None/primary/lower secondary", "Technical vocational/secondary/Higher education(Masters/Bachelor)/other"))

label(ImpactsOfCOVID19_clean$education3) <- "Level of education (mother)"

# Adding labels for employment2 and its levels
ImpactsOfCOVID19_clean$employment2 <- factor(ImpactsOfCOVID19_clean$employment2, levels = c(0, 1),
                                             labels = c("Employed", "Unemployed"))

label(ImpactsOfCOVID19_clean$employment2) <- "Employment"

# Adding labels for mother_vaccine and its levels
ImpactsOfCOVID19_clean$mother_vaccine <- factor(ImpactsOfCOVID19_clean$mother_vaccine, levels = c(0, 1),
                                                labels = c("Vaccinated", "Not vaccinated"))

label(ImpactsOfCOVID19_clean$mother_vaccine) <- "Mother COVID-19 vaccination"

# Adding labels for pos_household and its levels
ImpactsOfCOVID19_clean$pos_household <- factor(ImpactsOfCOVID19_clean$pos_household, levels = c(0, 1),
                                               labels = c("No positives", "Positives"))

label(ImpactsOfCOVID19_clean$pos_household) <- "COVID-19-positive household contacts"

# Adding labels for dcovidprot3 and its levels
ImpactsOfCOVID19_clean$dcovidprot3 <- factor(ImpactsOfCOVID19_clean$dcovidprot3, levels = c(0, 1),
                                             labels = c("5 or fewer", "6 or more"))

label(ImpactsOfCOVID19_clean$dcovidprot3) <- "Family protection actions during the COVID-19 pandemic"

# Adding labels for childactivities3 and its levels
ImpactsOfCOVID19_clean$childactivities3 <- factor(ImpactsOfCOVID19_clean$childactivities3, levels = c(1, 2, 3),
                                                  labels = c("None", "3 or fewer", "4 or more"))

label(ImpactsOfCOVID19_clean$childactivities3) <- "Social activities of children during the COVID-19 pandemic"

# Adding labels for mask4 and its levels
ImpactsOfCOVID19_clean$mask4 <- factor(ImpactsOfCOVID19_clean$mask4, levels = c(1, 2, 3),
                                       labels = c("Never/Rarely", "Sometimes", "Often/Always"))

label(ImpactsOfCOVID19_clean$mask4) <- "Use of mask by the child in social activities"

# Adding labels for distancing3 and its levels
ImpactsOfCOVID19_clean$distancing3 <- factor(ImpactsOfCOVID19_clean$distancing3, levels = c(1, 2, 3),
                                             labels = c("Never/Rarely", "Sometimes", "Often/Always"))

label(ImpactsOfCOVID19_clean$distancing3) <- "Distancing practice by the child in social activities"

# Adding labels for covid_diagnosis and its levels
ImpactsOfCOVID19_clean$covid_diagnosis <- factor(ImpactsOfCOVID19_clean$covid_diagnosis, levels = c(0, 1),
                                                 labels = c("No", "Yes"))

label(ImpactsOfCOVID19_clean$covid_diagnosis) <- "COVID-19 diagnosis of the child by a healthcare provider"



# Dataset's structure

str(ImpactsOfCOVID19_clean)

ImpactsOfCOVID19_clean

head(ImpactsOfCOVID19_clean)

summary(ImpactsOfCOVID19_clean)



# Plots and normality tests for continuous variables, and Mann-Whitney U test
## For table: Comparison of demographic characteristics of the study population by recruitment region in C2

# Age of mother
## Q-Q plot
qqnorm(ImpactsOfCOVID19_clean$age_mother, main = "Q-Q plot of age of mother")
qqline(ImpactsOfCOVID19_clean$age_mother, col = "red", lwd = 2)

## Shapiro-Wilk test of normality
shapiro.test(ImpactsOfCOVID19_clean$age_mother)

## Mann-Whitney U test (non-parametric) to compare age of mother by recruitment region
wilcox.test(age_mother ~ region_residence, data = ImpactsOfCOVID19_clean)



# Age of child
## Q-Q plot
qqnorm(ImpactsOfCOVID19_clean$age_child, main = "Q-Q plot of age of child")
qqline(ImpactsOfCOVID19_clean$age_child, col = "red", lwd = 2)

## Shapiro-Wilk test of normality
shapiro.test(ImpactsOfCOVID19_clean$age_child)

## Mann-Whitney U test (non-parametric) to compare age of child by recruitment region
wilcox.test(age_child ~ region_residence, data = ImpactsOfCOVID19_clean)



# Pearson's chi-square test of independence and contingency tables 
## For table: Comparison of demographic characteristics of the study population by recruitment region in C2

## Gender
chisq.test(ImpactsOfCOVID19_clean$gender_child, ImpactsOfCOVID19_clean$region_residence)

table(ImpactsOfCOVID19_clean$gender_child, ImpactsOfCOVID19_clean$region_residence)

## Ethnicity
chisq.test(ImpactsOfCOVID19_clean$ethnicity3, ImpactsOfCOVID19_clean$region_residence)

table(ImpactsOfCOVID19_clean$ethnicity3, ImpactsOfCOVID19_clean$region_residence)

## Household size
chisq.test(ImpactsOfCOVID19_clean$num_house, ImpactsOfCOVID19_clean$region_residence)

table(ImpactsOfCOVID19_clean$num_house, ImpactsOfCOVID19_clean$region_residence)

## Level of education (mother)
chisq.test(ImpactsOfCOVID19_clean$education3, ImpactsOfCOVID19_clean$region_residence)

table(ImpactsOfCOVID19_clean$education3, ImpactsOfCOVID19_clean$region_residence)

## Employment
chisq.test(ImpactsOfCOVID19_clean$employment2, ImpactsOfCOVID19_clean$region_residence)

table(ImpactsOfCOVID19_clean$employment2, ImpactsOfCOVID19_clean$region_residence)

## Family protection actions during the COVID-19 pandemic
chisq.test(ImpactsOfCOVID19_clean$dcovidprot3, ImpactsOfCOVID19_clean$region_residence)

table(ImpactsOfCOVID19_clean$dcovidprot3, ImpactsOfCOVID19_clean$region_residence)

## Social activities of children during the COVID-19 pandemic
chisq.test(ImpactsOfCOVID19_clean$childactivities3, ImpactsOfCOVID19_clean$region_residence)

table(ImpactsOfCOVID19_clean$childactivities3, ImpactsOfCOVID19_clean$region_residence)

## Use of mask by the child in social activities
chisq.test(ImpactsOfCOVID19_clean$mask4, ImpactsOfCOVID19_clean$region_residence)

table(ImpactsOfCOVID19_clean$mask4, ImpactsOfCOVID19_clean$region_residence)

## Distancing practice by the child in social activities
chisq.test(ImpactsOfCOVID19_clean$distancing3, ImpactsOfCOVID19_clean$region_residence)

table(ImpactsOfCOVID19_clean$distancing3, ImpactsOfCOVID19_clean$region_residence)

## COVID-19 diagnosis of the child by a healthcare provider
chisq.test(ImpactsOfCOVID19_clean$covid_diagnosis, ImpactsOfCOVID19_clean$region_residence)

table(ImpactsOfCOVID19_clean$covid_diagnosis, ImpactsOfCOVID19_clean$region_residence)

## COVID-19-positive household contacts
chisq.test(ImpactsOfCOVID19_clean$pos_household, ImpactsOfCOVID19_clean$region_residence)

table(ImpactsOfCOVID19_clean$pos_household, ImpactsOfCOVID19_clean$region_residence)

## Mother COVID-19 vaccination
chisq.test(ImpactsOfCOVID19_clean$mother_vaccine, ImpactsOfCOVID19_clean$region_residence)

table(ImpactsOfCOVID19_clean$mother_vaccine, ImpactsOfCOVID19_clean$region_residence)



# Univariate logistic regression
## For table: Summary of data and regression analyses of SARS-CoV-2 seronegative vs seropositive children in C2.


## univariate analysis for age of mother in years (centered)
u_age_motherc <- glm(sc_s1results ~ age_motherc,
                     data = ImpactsOfCOVID19_clean,
                     family = "binomial")

summary(u_age_motherc)

## unadjusted OR for age of mother in years (centered)
exp(coef(u_age_motherc)["age_motherc"])

## 95% CI for the OR for age of mother in years (centered)
exp(confint(u_age_motherc,
            parm = "age_motherc"))

## univariate analysis for age of children in years (centered)
u_age_childc <- glm(sc_s1results ~ age_childc,
                    data = ImpactsOfCOVID19_clean,
                    family = "binomial")

summary(u_age_childc)



## unadjusted OR for age of children in years (centered)
exp(coef(u_age_childc)["age_childc"])

## 95% CI for the OR for age of children in years (centered)
exp(confint(u_age_childc,
            parm = "age_childc"))



## levels of Gender
levels(ImpactsOfCOVID19_clean$gender_child)

## univariate analysis for Gender
u_gender_child <- glm(sc_s1results ~ gender_child,
                      data = ImpactsOfCOVID19_clean,
                      family = "binomial")

summary(u_gender_child)

## unadjusted OR for Gender
exp(coef(u_gender_child)["gender_childMale"])

## 95% CI for the unadjusted OR for Gender
exp(confint(u_gender_child,
            parm = "gender_childMale"))



# levels of Ethnicity
levels(ImpactsOfCOVID19_clean$ethnicity3)

# univariate analysis for Ethnicity
u_ethnicity3 <- glm(sc_s1results ~ ethnicity3,
                    data = ImpactsOfCOVID19_clean,
                    family = "binomial")

summary(u_ethnicity3)

# unadjusted OR for Ethnicity
exp(coef(u_ethnicity3)["ethnicity3African descent"])

# 95% CI for the unadjusted OR for Ethnicity
exp(confint(u_ethnicity3,
            parm = "ethnicity3African descent"))

# unadjusted OR for Ethnicity
exp(coef(u_ethnicity3)["ethnicity3Other Other (Indigenous/Amerindian, Mixed, Caucasian, Other)"])

# 95% CI for the unadjusted OR for Ethnicity
exp(confint(u_ethnicity3,
            parm = "ethnicity3Other Other (Indigenous/Amerindian, Mixed, Caucasian, Other)"))



## levels of Recruitment region
levels(ImpactsOfCOVID19_clean$region_residence)

## univariate analysis for Recruitment region
u_region_residence <- glm(sc_s1results ~ region_residence,
                          data = ImpactsOfCOVID19_clean,
                          family = "binomial")

summary(u_region_residence)

## unadjusted OR for Recruitment region
exp(coef(u_region_residence)["region_residenceNickerie region"])

## 95% CI for the unadjusted OR for Recruitment region
exp(confint(u_region_residence,
            parm = "region_residenceNickerie region"))



## levels of Household size
levels(ImpactsOfCOVID19_clean$num_house)

# univariate analysis for Household size
u_num_house <- glm(sc_s1results ~ num_house,
                   data = ImpactsOfCOVID19_clean,
                   family = "binomial")

summary(u_num_house)

# unadjusted OR for Household size
exp(coef(u_num_house)["num_house5 or more"])

## 95% CI for the unadjusted OR for Household size
exp(confint(u_num_house,
            parm = "num_house5 or more"))



## levels of Level of education (mother)
levels(ImpactsOfCOVID19_clean$education3)

## univariate analysis for Level of education (mother)
u_education3 <- glm(sc_s1results ~ education3,
                    data = ImpactsOfCOVID19_clean,
                    family = "binomial")

summary(u_education3)

# unadjusted OR for Level of education (mother)
exp(coef(u_education3)["education3Technical vocational/secondary/Higher education(Masters/Bachelor)/other"])

## 95% CI for the unadjusted OR for Level of education (mother)
exp(confint(u_education3,
            parm = "education3Technical vocational/secondary/Higher education(Masters/Bachelor)/other"))



## levels of Employment
levels(ImpactsOfCOVID19_clean$employment2)

## univariate analysis for Employment
u_employment2 <- glm(sc_s1results ~ employment2,
                     data = ImpactsOfCOVID19_clean,
                     family = "binomial")

summary(u_employment2)

## unadjusted OR for Employment
exp(coef(u_employment2)["employment2Unemployed"])

## 95% CI for the unadjusted OR for Employment
exp(confint(u_employment2,
            parm = "employment2Unemployed"))



## levels of Social activities of children during the COVID-19 pandemic
levels(ImpactsOfCOVID19_clean$childactivities3)

## univariate analysis for Social activities of children during the COVID-19 pandemic
u_childactivities3 <- glm(sc_s1results ~ childactivities3,
                          data = ImpactsOfCOVID19_clean,
                          family = "binomial")

summary(u_childactivities3)

## unadjusted OR for Social activities of children during the COVID-19 pandemic
exp(coef(u_childactivities3)["childactivities34 or more"])

## 95% CI for the unadjusted OR for Social activities of children during the COVID-19 pandemic
exp(confint(u_childactivities3,
            parm = "childactivities34 or more"))



## levels of Use of mask by child in social activities
levels(ImpactsOfCOVID19_clean$mask4)

## univariate analysis for Use of mask by child in social activities
u_mask4 <- glm(sc_s1results ~ mask4,
               data = ImpactsOfCOVID19_clean,
               family = "binomial")

summary(u_mask4)

## unadjusted OR for Use of mask by child in social activities
exp(coef(u_mask4)["mask4Sometimes"])

## 95% CI for the unadjusted OR for Use of mask by child in social activities
exp(confint(u_mask4,
            parm = "mask4Sometimes"))

## unadjusted OR for Use of mask by child in social activities
exp(coef(u_mask4)["mask4Often/Always"])

## 95% CI for the unadjusted OR for Use of mask by child in social activities
exp(confint(u_mask4,
            parm = "mask4Often/Always"))



## levels of Distancing practice by child in social activities
levels(ImpactsOfCOVID19_clean$distancing3)

## univariate analysis for Distancing practice by child in social activities
u_distancing3 <- glm(sc_s1results ~ distancing3,
                     data = ImpactsOfCOVID19_clean,
                     family = "binomial")

summary(u_distancing3)

## unadjusted OR for Distancing practice by child in social activities
exp(coef(u_distancing3)["distancing3Sometimes"])

## 95% CI for the unadjusted OR for Distancing practice by child in social activities
exp(confint(u_distancing3,
            parm = "distancing3Sometimes"))

# unadjusted OR for Distancing practice by child in social activities
exp(coef(u_distancing3)["distancing3Often/Always"])

## 95% CI for the unadjusted OR for Distancing practice by child in social activities
exp(confint(u_distancing3,
            parm = "distancing3Often/Always"))



## levels of COVID-19 diagnosis of the child by a healthcare provider
levels(ImpactsOfCOVID19_clean$covid_diagnosis)

## univariate analysis for COVID-19 diagnosis of the child by a healthcare provider
u_covid_diagnosis <- glm(sc_s1results ~ covid_diagnosis,
                         data = ImpactsOfCOVID19_clean,
                         family = "binomial")

summary(u_covid_diagnosis)

## unadjusted OR for COVID-19 diagnosis of the child by a healthcare provider
exp(coef(u_covid_diagnosis)["covid_diagnosisYes"])

## 95% CI for the unadjusted OR for COVID-19 diagnosis of the child by a healthcare provider
exp(confint(u_covid_diagnosis,
            parm = "covid_diagnosisYes"))



## levels of COVID-19-positive household contacts
levels(ImpactsOfCOVID19_clean$pos_household)

## univariate analysis for COVID-19-positive household contacts
u_pos_household <- glm(sc_s1results ~ pos_household,
                       data = ImpactsOfCOVID19_clean,
                       family = "binomial")

summary(u_pos_household)

## unadjusted OR for COVID-19-positive household contacts
exp(coef(u_pos_household)["pos_householdPositives"])

## 95% CI for the unadjusted OR for COVID-19-positive household contacts
exp(confint(u_pos_household,
            parm = "pos_householdPositives"))



## levels of Mother COVID-19 vaccination
levels(ImpactsOfCOVID19_clean$mother_vaccine)

## univariate analysis for Mother COVID-19 vaccination
u_mother_vaccine <- glm(sc_s1results ~ mother_vaccine,
                        data = ImpactsOfCOVID19_clean,
                        family = "binomial")

summary(u_mother_vaccine)

## unadjusted OR for Mother COVID-19 vaccination
exp(coef(u_mother_vaccine)["mother_vaccineNot vaccinated"])

## 95% CI for the unadjusted OR for Mother COVID-19 vaccination
exp(confint(u_mother_vaccine,
            parm = "mother_vaccineNot vaccinated"))



# For Supplementary Table: Comparison between models with main effects (no interaction) and models with interactions before binary logistic regression modeling.

## Interaction effects: Age of mother (centered) and Recruitment region
## Model 1: save model without interaction
nointer_mrm1 <- glm(sc_s1results ~ age_motherc + region_residence,
                    data = ImpactsOfCOVID19_clean,
                    family = "binomial")
## print results
summary(nointer_mrm1)


## Model 2: save model with interaction
inter_mrm2 <- glm(sc_s1results ~ age_motherc * region_residence,
                  data = ImpactsOfCOVID19_clean,
                  family = "binomial")

## print results
summary(inter_mrm2)

## Likelihood ratio test
anova(nointer_mrm1, inter_mrm2,
      test = "LRT")



## Interaction effects: Age of mother (centered) and Distancing practice by child in social activities
## Model 1: save model without interaction
nointer_drm1 <- glm(sc_s1results ~ age_motherc + distancing3,
                    data = ImpactsOfCOVID19_clean,
                    family = "binomial")
## print results
summary(nointer_drm1)

## Model 2: save model with interaction
inter_drm2 <- glm(sc_s1results ~ age_motherc * distancing3,
                  data = ImpactsOfCOVID19_clean,
                  family = "binomial")
## print results
summary(inter_drm2)

## Likelihood ratio test
anova(nointer_drm1, inter_drm2,
      test = "LRT")



## Interaction effects: Age of mother (centered) and Use of mask by child in social activities
## Model 1: save model without interaction
nointer_amm1 <- glm(sc_s1results ~ age_motherc + mask4,
                    data = ImpactsOfCOVID19_clean,
                    family = "binomial")
## print results
summary(nointer_amm1)

## Model 2: save model with interaction
inter_amm2 <- glm(sc_s1results ~ age_motherc * mask4,
                  data = ImpactsOfCOVID19_clean,
                  family = "binomial")
## print results
summary(inter_amm2)

## Likelihood ratio test
anova(nointer_amm1, inter_amm2,
      test = "LRT")



## Interaction effects: Age of mother (centered) and COVID-19-positive household contacts
## Model 1: save model without interaction
nointer_mpm1 <- glm(sc_s1results ~ age_motherc + pos_household,
                    data = ImpactsOfCOVID19_clean,
                    family = "binomial")
## print results
summary(nointer_mpm1)

## Model 2: save model with interaction
inter_mpm2 <- glm(sc_s1results ~ age_motherc * pos_household,
                  data = ImpactsOfCOVID19_clean,
                  family = "binomial")
## print results
summary(inter_mpm2)

## Likelihood ratio test
anova(nointer_mpm1, inter_mpm2,
      test = "LRT")



## Interaction effects: Mother COVID-19 vaccination and Age of mother (centered)
## Model 1: save model without interaction
noi_mmm1 <- glm(sc_s1results ~ mother_vaccine + age_motherc,
                data = ImpactsOfCOVID19_clean,
                family = "binomial")
## print results
summary(noi_mmm1)


## Model 2: save model with interaction
i_mmm2 <- glm(sc_s1results ~ mother_vaccine * age_motherc,
              data = ImpactsOfCOVID19_clean,
              family = "binomial")
## print results
summary(i_mmm2)

## Likelihood ratio test
anova(noi_mmm1, i_mmm2,
      test = "LRT")


## Interaction effects: Mother COVID-19 vaccination and Age of child (centered)
## Model 1: save model without interaction
noi_mcm1 <- glm(sc_s1results ~ mother_vaccine + age_childc,
                data = ImpactsOfCOVID19_clean,
                family = "binomial")
## print results
summary(noi_mcm1)


## Model 2: save model with interaction
i_mcm2 <- glm(sc_s1results ~ mother_vaccine * age_childc,
              data = ImpactsOfCOVID19_clean,
              family = "binomial")
## print results
summary(i_mcm2)

## Likelihood ratio test
anova(noi_mcm1, i_mcm2,
      test = "LRT")



## Interaction effects: Mother COVID-19 vaccination and gender
## Model 1: save model without interaction
noi_mgm1 <- glm(sc_s1results ~ mother_vaccine + gender_child,
                data = ImpactsOfCOVID19_clean,
                family = "binomial")
## print results
summary(noi_mgm1)


## Model 2: save model with interaction
i_mgm2 <- glm(sc_s1results ~ mother_vaccine * gender_child,
              data = ImpactsOfCOVID19_clean,
              family = "binomial")
## print results
summary(i_mgm2)

## Likelihood ratio test
anova(noi_mgm1, i_mgm2,
      test = "LRT")



## Interaction effects: Mother COVID-19 vaccination and ethnicity
## Model 1: save model without interaction
noi_mem1 <- glm(sc_s1results ~ mother_vaccine + ethnicity3,
                data = ImpactsOfCOVID19_clean,
                family = "binomial")
## print results
summary(noi_mem1)


## Model 2: save model with interaction
i_mem2 <- glm(sc_s1results ~ mother_vaccine * ethnicity3,
              data = ImpactsOfCOVID19_clean,
              family = "binomial")
## print results
summary(i_mem2)

## Likelihood ratio test
anova(noi_mem1, i_mem2,
      test = "LRT")



## Interaction effects: Mother COVID-19 vaccination and Recruitment region
## Model 1: save model without interaction
noi_mrm1 <- glm(sc_s1results ~ mother_vaccine + region_residence,
                data = ImpactsOfCOVID19_clean,
                family = "binomial")
## print results
summary(noi_mrm1)


## Model 2: save model with interaction
i_mrm2 <- glm(sc_s1results ~ mother_vaccine * region_residence,
              data = ImpactsOfCOVID19_clean,
              family = "binomial")
## print results
summary(i_mrm2)

## Likelihood ratio test
anova(noi_mrm1, i_mrm2,
      test = "LRT")



## Interaction effects: Mother COVID-19 vaccination and Household size
## Model 1: save model without interaction
noi_mnm1 <- glm(sc_s1results ~ mother_vaccine + num_house,
                data = ImpactsOfCOVID19_clean,
                family = "binomial")
## print results
summary(noi_mnm1)

## Model 2: save model with interaction
i_mnm2 <- glm(sc_s1results ~ mother_vaccine * num_house,
              data = ImpactsOfCOVID19_clean,
              family = "binomial")
## print results
summary(i_mnm2)

## Likelihood ratio test
anova(noi_mnm1, i_mnm2,
      test = "LRT")



## Interaction effects: Mother COVID-19 vaccination and Level of education (mother)
## Model 1: save model without interaction
noi_medm1 <- glm(sc_s1results ~ mother_vaccine + education3,
                 data = ImpactsOfCOVID19_clean,
                 family = "binomial")
## print results
summary(noi_medm1)


## Model 2: save model with interaction
i_medm2 <- glm(sc_s1results ~ mother_vaccine * education3,
               data = ImpactsOfCOVID19_clean,
               family = "binomial")
## print results
summary(i_medm2)

## Likelihood ratio test
anova(noi_medm1, i_medm2,
      test = "LRT")



## Interaction effects: Mother COVID-19 vaccination and employment
## Model 1: save model without interaction
noi_memm1 <- glm(sc_s1results ~ mother_vaccine + employment2,
                 data = ImpactsOfCOVID19_clean,
                 family = "binomial")
## print results
summary(noi_memm1)


## Model 2: save model with interaction
i_memm2 <- glm(sc_s1results ~ mother_vaccine * employment2,
               data = ImpactsOfCOVID19_clean,
               family = "binomial")
## print results
summary(i_memm2)

## Likelihood ratio test
anova(noi_memm1, i_memm2,
      test = "LRT")



## Interaction effects: Mother COVID-19 vaccination and Social activities of children during the COVID-19 pandemic
## Model 1: save model without interaction
noi_mchm1 <- glm(sc_s1results ~ mother_vaccine + childactivities3,
                 data = ImpactsOfCOVID19_clean,
                 family = "binomial")
## print results
summary(noi_mchm1)


## Model 2: save model with interaction
i_mchm2 <- glm(sc_s1results ~ mother_vaccine * childactivities3,
               data = ImpactsOfCOVID19_clean,
               family = "binomial")
## print results
summary(i_mchm2)

## Likelihood ratio test
anova(noi_mchm1, i_mchm2,
      test = "LRT")



## Interaction effects: Mother COVID-19 vaccination and Use of mask by child in social activities
## Model 1: save model without interaction
noi_mmam1 <- glm(sc_s1results ~ mother_vaccine + mask4,
                 data = ImpactsOfCOVID19_clean,
                 family = "binomial")
## print results
summary(noi_mmam1)


## Model 2: save model with interaction
i_mmam2 <- glm(sc_s1results ~ mother_vaccine * mask4,
               data = ImpactsOfCOVID19_clean,
               family = "binomial")
## print results
summary(i_mmam2)

## Likelihood ratio test
anova(noi_mmam1, i_mmam2,
      test = "LRT")



## Interaction effects: Mother COVID-19 vaccination and Distancing practice by child in social activities
## Model 1: save model without interaction
noi_mdm1 <- glm(sc_s1results ~ mother_vaccine + distancing3,
                data = ImpactsOfCOVID19_clean,
                family = "binomial")
## print results
summary(noi_mdm1)


## Model 2: save model with interaction
i_mdm2 <- glm(sc_s1results ~ mother_vaccine * distancing3,
              data = ImpactsOfCOVID19_clean,
              family = "binomial")
## print results
summary(i_mdm2)

## Likelihood ratio test
anova(noi_mdm1, i_mdm2,
      test = "LRT")



## Interaction effects: Mother COVID-19 vaccination and COVID-19 diagnosis of the child by a healthcare provider
## Model 1: save model without interaction
noi_mcom1 <- glm(sc_s1results ~ mother_vaccine + covid_diagnosis,
                 data = ImpactsOfCOVID19_clean,
                 family = "binomial")
## print results
summary(noi_mcom1)


## Model 2: save model with interaction
i_mcom2 <- glm(sc_s1results ~ mother_vaccine * covid_diagnosis,
               data = ImpactsOfCOVID19_clean,
               family = "binomial")
## print results
summary(i_mcom2)

## Likelihood ratio test
anova(noi_mcom1, i_mcom2,
      test = "LRT")



## Interaction effects: Mother COVID-19 vaccination and COVID-19-positive household contacts
## Model 1: save model without interaction
noi_mpom1 <- glm(sc_s1results ~ mother_vaccine + pos_household,
                 data = ImpactsOfCOVID19_clean,
                 family = "binomial")
## print results
summary(noi_mpom1)


## Model 2: save model with interaction
i_mpom2 <- glm(sc_s1results ~ mother_vaccine * pos_household,
               data = ImpactsOfCOVID19_clean,
               family = "binomial")
## print results
summary(i_mpom2)

## Likelihood ratio test
anova(noi_mpom1, i_mpom2,
      test = "LRT")



## Interaction effects: Age of mother and Household size
## Model 1: save model without interaction
nointer_mnm1 <- glm(sc_s1results ~ age_motherc + num_house,
                    data = ImpactsOfCOVID19_clean,
                    family = "binomial")
## print results
summary(nointer_mnm1)

## Model 2: save model with interaction
inter_mnm2 <- glm(sc_s1results ~ age_motherc * num_house,
                  data = ImpactsOfCOVID19_clean,
                  family = "binomial")
## print results
summary(inter_mnm2)

## Likelihood ratio test
anova(nointer_mnm1, inter_mnm2,
      test = "LRT")



## Interaction effects: Household size and COVID-19-positive household contacts
## Model 1: save model without interaction
nointer_npm1 <- glm(sc_s1results ~ num_house + pos_household,
                    data = ImpactsOfCOVID19_clean,
                    family = "binomial")
## print results
summary(nointer_npm1)


## Model 2: save model with interaction
inter_npm2 <- glm(sc_s1results ~ num_house * pos_household,
                  data = ImpactsOfCOVID19_clean,
                  family = "binomial")
## print results
summary(inter_npm2)

## Likelihood ratio test
anova(nointer_npm1, inter_npm2,
      test = "LRT")




# Comparing binary logistic regression models without and with the interaction term of age_motherc * distancing3

## Model m1 without interaction
m1 <- glm(sc_s1results ~ age_motherc + region_residence + num_house + 
            childactivities3 + mask4 + distancing3 +
            covid_diagnosis + pos_household + mother_vaccine,
          data = ImpactsOfCOVID19_clean,
          family = "binomial")

## print results
summary(m1)

## Model m2 with interaction term
m2 <- glm(sc_s1results ~ age_motherc + region_residence + num_house + 
            childactivities3 + mask4 + distancing3 + age_motherc * distancing3 +
            covid_diagnosis + pos_household + mother_vaccine,
          data = ImpactsOfCOVID19_clean,
          family = "binomial")

## print results
summary(m2)

## Likelihood ratio test
anova(m1, m2,
      test = "LRT")




# Adjusted odd ratios of the final model "m2" with interaction term

## load packages
library(gt)
library(finalfit)

## select the variables to be included in the model
dependent <- "sc_s1results"
independent_variables <- c("age_motherc", "region_residence", "num_house", "childactivities3", "mask4", "distancing3", 
                           "age_motherc * distancing3", "covid_diagnosis", "pos_household", "mother_vaccine")

## save results of the model
glmm2_model <- ImpactsOfCOVID19_clean |>
  glmmulti(dependent, independent_variables) |>
  fit2df(
    explanatory_name = "Variables",
    estimate_name = "Adjusted OR",
    estimate_suffix = "(95% CI)")

## print results
glmm2_model |>
  gt()



# Validation of the final model "m2" with interaction term

## Quality of the final model: GOF
## load package
library(blorr)
blr_test_hosmer_lemeshow(m2)




## Quality of the final model: Accuracy
## create a vector of predicted probabilities
pred_m2 <- predict(m2,
                   newdata = select(ImpactsOfCOVID19_clean, -sc_s1results), # remove real outcomes
                   type = "response")

## if probability < threshold, patient is considered seronegative
pred_outcomem2 <- ifelse(pred_m2 < 0.5,
                         0,
                         1)

## transform predictions into factor and set labels
pred_outcomem2 <- factor(pred_outcomem2,
                         levels = c(0, 1),
                         labels = c("Seronegative", "Seropositive"))

## compare observed vs. predicted outcome
tab_m2 <- table(ImpactsOfCOVID19_clean$sc_s1results, pred_outcomem2,
                dnn = c("observed", "predicted"))

## print results
tab_m2

## percentage of correct predictions or accuracy
accuracy_m2 <- sum(diag(tab_m2)) / sum(tab_m2)
accuracy_m2




## Quality of the final model: Sensitivity and specificity
## sensitivity
sensitivity_m2 <- tab_m2[2, 2] / (tab_m2[2, 2] + tab_m2[2, 1])
sensitivity_m2

## specificity
specificity_m2 <- tab_m2[1, 1] / (tab_m2[1, 1] + tab_m2[1, 2])
specificity_m2



## Quality of the final model: AUC and ROC curve
## load package
library(pROC)

## save roc object
res_m2 <- roc(sc_s1results ~ fitted(m2),
              data = ImpactsOfCOVID19_clean)

## plot ROC curve
ggroc(res_m2, legacy.axes = TRUE)

## print AUC
res_m2$auc

## load package
library(ggplot2)

## plot ROC curve with AUC in the title
ggroc(res_m2, legacy.axes = TRUE) +
  labs(title = paste0("AUC = ", round(res_m2$auc, 2)))



# Measure multicollinearity with variance inflation factors (VIF)

## load package
library(car)

## compute VIF for the final model
vif(m2)










# Assessing correlations between variables included in the final model 

## load package
library(tidyverse)

## read in data
ImpactsOfCOVID19_Ranalysis <- read.csv("ImpactsOfCOVID19_Ranalysis.csv")

## select necessary columns and factor variables
ImpactsOfCOVID19_Ranalysis <- ImpactsOfCOVID19_Ranalysis %>% 
  select(sc_s1results, pos_household, dcovidprot3, 
         childactivities3, mask4, 
         distancing3, mother_vaccine,
         age_mother, age_motherc, num_house, covid_diagnosis, age_child, age_childc, gender_child, 
         ethnicity3, region_residence, education3, employment2) %>%
  mutate(sc_s1results = as.factor(sc_s1results),
         pos_household = as.factor(pos_household),
         dcovidprot3 = as.factor(dcovidprot3),
         childactivities3 = as.factor(childactivities3),
         mask4 = as.factor(mask4),
         distancing3 = as.factor(distancing3),
         mother_vaccine = as.factor(mother_vaccine),
         num_house = as.factor(num_house),
         covid_diagnosis = as.factor(covid_diagnosis), 
         gender_child = as.factor(gender_child), 
         ethnicity3 = as.factor(ethnicity3), 
         region_residence = as.factor(region_residence), 
         education3 = as.factor(education3), 
         employment2 = as.factor(employment2))

## remove rows with NA 
ImpactsOfCOVID19_clean <- na.omit(ImpactsOfCOVID19_Ranalysis)




# Label variables

## load package
library(Hmisc)

## Adding labels for Age of mother (years)
label(ImpactsOfCOVID19_clean$age_mother) <- "Age of mother (years)"
label(ImpactsOfCOVID19_clean$age_mother)

## Adding labels for Age of mother (years) centered
label(ImpactsOfCOVID19_clean$age_motherc) <- "Age of mother (years)"
label(ImpactsOfCOVID19_clean$age_motherc)

## Adding labels for Age of child (years)
label(ImpactsOfCOVID19_clean$age_child) <- "Age of child (years)"
label(ImpactsOfCOVID19_clean$age_child)

## Adding labels for Age of child (years) centered
label(ImpactsOfCOVID19_clean$age_childc) <- "Age of child (years)"
label(ImpactsOfCOVID19_clean$age_childc)

## Adding labels for gender_child and its levels
ImpactsOfCOVID19_clean$gender_child <- factor(ImpactsOfCOVID19_clean$gender_child, levels = c(0, 1),
                                              labels = c("Female", "Male"))

label(ImpactsOfCOVID19_clean$gender_child) <- "Gender"

## Adding labels for ethnicity3 and its levels
ImpactsOfCOVID19_clean$ethnicity3 <- factor(ImpactsOfCOVID19_clean$ethnicity3, levels = c(1, 2, 3),
                                            labels = c("Asian descent", "African descent", "Other (Indigenous, Mixed, Caucasian, Other)"))

label(ImpactsOfCOVID19_clean$ethnicity3) <- "Ethnicity"

## Adding labels for region_residence and its levels
ImpactsOfCOVID19_clean$region_residence <- factor(ImpactsOfCOVID19_clean$region_residence, levels = c(0, 1),
                                                  labels = c("Paramaribo region", "Nickerie region"))

label(ImpactsOfCOVID19_clean$region_residence) <- "Recruitment region"

## Adding labels for num_house and its levels
ImpactsOfCOVID19_clean$num_house <- factor(ImpactsOfCOVID19_clean$num_house, levels = c(0, 1),
                                           labels = c("4 or fewer", "5 or more"))

label(ImpactsOfCOVID19_clean$num_house) <- "Household size"

## Adding labels for education3 and its levels
ImpactsOfCOVID19_clean$education3 <- factor(ImpactsOfCOVID19_clean$education3, levels = c(0, 1),
                                            labels = c("None/primary/lower secondary", "Technical vocational/secondary/Higher education(Masters/Bachelor)/other"))

label(ImpactsOfCOVID19_clean$education3) <- "Level of education (mother)"

## Adding labels for employment2 and its levels
ImpactsOfCOVID19_clean$employment2 <- factor(ImpactsOfCOVID19_clean$employment2, levels = c(0, 1),
                                             labels = c("Employed", "Unemployed"))

label(ImpactsOfCOVID19_clean$employment2) <- "Employment"

## Adding labels for mother_vaccine and its levels

ImpactsOfCOVID19_clean$mother_vaccine <- factor(ImpactsOfCOVID19_clean$mother_vaccine, levels = c(0, 1),
                                                labels = c("Vaccinated", "Not vaccinated"))

label(ImpactsOfCOVID19_clean$mother_vaccine) <- "Mother COVID-19 vaccination"

## Adding labels for pos_household and its levels

ImpactsOfCOVID19_clean$pos_household <- factor(ImpactsOfCOVID19_clean$pos_household, levels = c(0, 1),
                                               labels = c("No positives", "Positives"))

label(ImpactsOfCOVID19_clean$pos_household) <- "COVID-19-positive household contacts"

## Adding labels for dcovidprot3 and its levels
ImpactsOfCOVID19_clean$dcovidprot3 <- factor(ImpactsOfCOVID19_clean$dcovidprot3, levels = c(0, 1),
                                             labels = c("5 or fewer", "6 or more"))

label(ImpactsOfCOVID19_clean$dcovidprot3) <- "Family protection actions during the COVID-19 pandemic"

## Adding labels for childactivities3 and its levels
ImpactsOfCOVID19_clean$childactivities3 <- factor(ImpactsOfCOVID19_clean$childactivities3, levels = c(1, 2, 3),
                                                  labels = c("None", "3 or fewer", "4 or more"))

label(ImpactsOfCOVID19_clean$childactivities3) <- "Social activities of children during the COVID-19 pandemic"

# Adding labels for mask4 and its levels
ImpactsOfCOVID19_clean$mask4 <- factor(ImpactsOfCOVID19_clean$mask4, levels = c(1, 2, 3),
                                       labels = c("Never/Rarely", "Sometimes", "Often/Always"))

label(ImpactsOfCOVID19_clean$mask4) <- "Use of mask by child in social activities"

# Adding labels for distancing3 and its levels
ImpactsOfCOVID19_clean$distancing3 <- factor(ImpactsOfCOVID19_clean$distancing3, levels = c(1, 2, 3),
                                             labels = c("Never/Rarely", "Sometimes", "Often/Always"))

label(ImpactsOfCOVID19_clean$distancing3) <- "Distancing practice by child in social activities"

# Adding labels for covid_diagnosis and its levels
ImpactsOfCOVID19_clean$covid_diagnosis <- factor(ImpactsOfCOVID19_clean$covid_diagnosis, levels = c(0, 1),
                                                 labels = c("No", "Yes"))

label(ImpactsOfCOVID19_clean$covid_diagnosis) <- "COVID-19 diagnosis of the child by healthcare provider"



## maternal COVID-19 vaccination status and COVID-19 positive household contacts 


## distancing practice by the child and maternal COVID-19 vaccination status 
chisq.test(ImpactsOfCOVID19_clean$distancing3, ImpactsOfCOVID19_clean$mother_vaccine)

table(ImpactsOfCOVID19_clean$distancing3, ImpactsOfCOVID19_clean$mother_vaccine)

## COVID-19 diagnosis of the child by a healthcare provider and maternal COVID-19 vaccination status 
chisq.test(ImpactsOfCOVID19_clean$covid_diagnosis, ImpactsOfCOVID19_clean$mother_vaccine)

table(ImpactsOfCOVID19_clean$covid_diagnosis, ImpactsOfCOVID19_clean$mother_vaccine)

## household size and maternal COVID-19 vaccination status
chisq.test(ImpactsOfCOVID19_clean$num_house, ImpactsOfCOVID19_clean$mother_vaccine)

table(ImpactsOfCOVID19_clean$num_house, ImpactsOfCOVID19_clean$mother_vaccine)

## household size and COVID-19 positive household contacts
chisq.test(ImpactsOfCOVID19_clean$num_house, ImpactsOfCOVID19_clean$pos_household)

table(ImpactsOfCOVID19_clean$num_house, ImpactsOfCOVID19_clean$pos_household)

## household size and COVID-19 diagnosis of the child by a healthcare provider
chisq.test(ImpactsOfCOVID19_clean$num_house, ImpactsOfCOVID19_clean$covid_diagnosis)

table(ImpactsOfCOVID19_clean$num_house, ImpactsOfCOVID19_clean$covid_diagnosis)

## COVID-19 diagnosis of the child by a healthcare provider and COVID-19 positive household contacts 
chisq.test(ImpactsOfCOVID19_clean$pos_household, ImpactsOfCOVID19_clean$covid_diagnosis)

table(ImpactsOfCOVID19_clean$pos_household, ImpactsOfCOVID19_clean$covid_diagnosis)









# Contingency tables and Cohen's Kappa tests

# load package if necessary
library(tidyverse)

# read in data
ImpactsOfCOVID19_Ranalysis <- read.csv("ImpactsOfCOVID19_Ranalysis.csv")

# select necessary columns and factor variables
ImpactsOfCOVID19_Ranalysis <- ImpactsOfCOVID19_Ranalysis %>% 
  select(fc_nresults, fc_s1results) %>%
  mutate(fc_nresults = as.factor(fc_nresults),
         fc_s1results = as.factor(fc_s1results))

# remove rows with NA 
ImpactsOfCOVID19_clean <- na.omit(ImpactsOfCOVID19_Ranalysis)



# Label variables
## load package if necessary
library(Hmisc)

## Adding labels for fc_nresults and its levels
ImpactsOfCOVID19_clean$fc_nresults <- factor(ImpactsOfCOVID19_clean$fc_nresults, levels = c(0, 1),
                                             labels = c("Seronegative", "Seropositive"))

label(ImpactsOfCOVID19_clean$fc_nresults) <- "SARS-CoV-2 NP Seroprevalence (C1)"


## Adding labels for fc_s1results and its levels
ImpactsOfCOVID19_clean$fc_s1results <- factor(ImpactsOfCOVID19_clean$fc_s1results, levels = c(0, 1),
                                              labels = c("Seronegative", "Seropositive"))

label(ImpactsOfCOVID19_clean$fc_s1results) <- "SARS-CoV-2 S1 Seroprevalence (C1)"



# Dataset's structure
str(ImpactsOfCOVID19_clean)
ImpactsOfCOVID19_clean

## preview of the final data frame
head(ImpactsOfCOVID19_clean)

## basic descriptive statistics
summary(ImpactsOfCOVID19_clean)


# Crosstab 
library(crosstable)
library(dplyr)

crosstable(ImpactsOfCOVID19_clean, c(fc_nresults), by=fc_s1results, 
           total="both") %>% 
  as_flextable(keep_id=FALSE)




# Cohen's Kappa test
## Load package
library(psych)

## Create a contingency table of the two variables
conf_matrix <- table(ImpactsOfCOVID19_clean$fc_nresults, ImpactsOfCOVID19_clean$fc_s1results)

print(conf_matrix) 

## Perform Cohen's Kappa test
kappa_result <- cohen.kappa(conf_matrix)

## View the results
print(kappa_result)

## kappa value
kappa_result$kappa


# Cohen's Kappa test p-value
## Load package
library(irr)

## Perform Cohen's Kappa test with p-value
kappa_result <- kappa2(ImpactsOfCOVID19_clean[, c("fc_nresults", "fc_s1results")])

## View the results, including the p-value
print(kappa_result)









# load package if necessary
library(tidyverse)

# read in data
ImpactsOfCOVID19_Ranalysis <- read.csv("ImpactsOfCOVID19_Ranalysis.csv")

# select necessary columns and factor variables
ImpactsOfCOVID19_Ranalysis <- ImpactsOfCOVID19_Ranalysis %>% 
  select(sc_nresults, sc_s1results) %>%
  mutate(sc_nresults = as.factor(sc_nresults),
         sc_s1results = as.factor(sc_s1results))

# remove rows with NA 
ImpactsOfCOVID19_clean <- na.omit(ImpactsOfCOVID19_Ranalysis)



# Label variables
## load package if necessary
library(Hmisc)

## Adding labels for sc_nresults and its levels
ImpactsOfCOVID19_clean$sc_nresults <- factor(ImpactsOfCOVID19_clean$sc_nresults, levels = c(0, 1),
                                             labels = c("Seronegative", "Seropositive"))

label(ImpactsOfCOVID19_clean$sc_nresults) <- "SARS-CoV-2 NP Seroprevalence (C2)"


## Adding labels for sc_s1results and its levels
ImpactsOfCOVID19_clean$sc_s1results <- factor(ImpactsOfCOVID19_clean$sc_s1results, levels = c(0, 1),
                                              labels = c("Seronegative", "Seropositive"))

label(ImpactsOfCOVID19_clean$sc_s1results) <- "SARS-CoV-2 S1 Seroprevalence (C2)"



# Dataset's structure
str(ImpactsOfCOVID19_clean)
ImpactsOfCOVID19_clean

## preview of the final data frame
head(ImpactsOfCOVID19_clean)

## basic descriptive statistics
summary(ImpactsOfCOVID19_clean)



# Crosstab
## load package if necessary
library(crosstable)
library(dplyr)

crosstable(ImpactsOfCOVID19_clean, c(sc_nresults), by=sc_s1results, 
           total="both") %>% 
  as_flextable(keep_id=FALSE)



# Cohen's Kappa test
## load package if necessary
library(psych)

# Create a contingency table of the two variables
conf_matrix <- table(ImpactsOfCOVID19_clean$sc_nresults, ImpactsOfCOVID19_clean$sc_s1results)

print(conf_matrix) 

# Perform Cohen's Kappa test
kappa_result <- cohen.kappa(conf_matrix)

# View the results
print(kappa_result)

# kappa value
kappa_result$kappa




# Cohen's Kappa test p-value
## load package if necessary
library(irr)

# Perform Cohen's Kappa test with p-value
kappa_result <- kappa2(ImpactsOfCOVID19_clean[, c("sc_nresults", "sc_s1results")])

# View the results, including the p-value
print(kappa_result)









# load package if necessary
library(tidyverse)

# read in data
ImpactsOfCOVID19_Ranalysis <- read.csv("ImpactsOfCOVID19_Ranalysis.csv")

# select necessary columns and factor variables
ImpactsOfCOVID19_Ranalysis <- ImpactsOfCOVID19_Ranalysis %>% 
  select(fc_s1results, 
         agechild_catc1, gender_child, 
         ethnicity3) %>%
  mutate(fc_s1results = as.factor(fc_s1results),
         ethnicity3 = as.factor(ethnicity3),
         agechild_catc1 = as.factor(agechild_catc1),
         gender_child = as.factor(gender_child))

# remove rows with NA 
ImpactsOfCOVID19_clean <- na.omit(ImpactsOfCOVID19_Ranalysis)



# Label variables

## load package if necessary
library(Hmisc)

## Adding labels for fc_s1results and its levels
ImpactsOfCOVID19_clean$fc_s1results <- factor(ImpactsOfCOVID19_clean$fc_s1results, levels = c(0, 1),
                                              labels = c("Seronegative", "Seropositive"))

label(ImpactsOfCOVID19_clean$fc_s1results) <- "SARS-CoV-2 Seroprevalence (C1)"

## Adding labels for Age of child (years)
ImpactsOfCOVID19_clean$agechild_catc1 <- factor(ImpactsOfCOVID19_clean$agechild_catc1, levels = c(0, 1),
                                                labels = c("1-2", "3-4"))

label(ImpactsOfCOVID19_clean$agechild_catc1) <- "Age of child in years"

## Adding labels for gender_child and its levels
ImpactsOfCOVID19_clean$gender_child <- factor(ImpactsOfCOVID19_clean$gender_child, levels = c(0, 1),
                                              labels = c("Female", "Male"))

label(ImpactsOfCOVID19_clean$gender_child) <- "Gender"

## Adding labels for ethnicity3 and its levels
ImpactsOfCOVID19_clean$ethnicity3 <- factor(ImpactsOfCOVID19_clean$ethnicity3, levels = c(1, 2, 3),
                                            labels = c("Asian descent", "African descent", "Other (Indigenous/Amerindian, Mixed, Caucasian, Other)"))

label(ImpactsOfCOVID19_clean$ethnicity3) <- "Ethnicity"



# Dataset's structure

str(ImpactsOfCOVID19_clean)
ImpactsOfCOVID19_clean

## preview of the final data frame
head(ImpactsOfCOVID19_clean)

## basic descriptive statistics
summary(ImpactsOfCOVID19_clean)



# Crosstab
## load package if necessary
library(crosstable)
library(dplyr)

crosstable(ImpactsOfCOVID19_clean, c(agechild_catc1, ethnicity3, gender_child), 
           by=fc_s1results, 
           total="both") %>% 
  as_flextable(keep_id=FALSE)




## load package
library(binom)

# Overall crude seroprevalence estimates and 95% CIs

## 95% CIs for crude seroprevalence estimated using the exact binomial test of Wilson score interval method
## x is the count of seropositives
## n is the total sample size

seropos_count <- 93
totalsamplesize <- 275
prop.test(x = seropos_count, n = totalsamplesize, conf.level = 0.95, correct = FALSE)



## tabulate by age of child categorical
table_agechildcatc1 <- table(ImpactsOfCOVID19_clean$agechild_catc1, ImpactsOfCOVID19_clean$fc_s1results)
table_agechildcatc1

## 1-2 years group wilson score CI
ci_12 <- binom.confint(x = 17, n = 53, conf.level = 0.95, methods = "wilson")
ci_12

## 3-4 years group wilson score CI
ci_34 <- binom.confint(x = 76, n = 222, conf.level = 0.95, methods = "wilson")
ci_34



## tabulate by gender
table_gender <- table(ImpactsOfCOVID19_clean$gender_child, ImpactsOfCOVID19_clean$fc_s1results)
table_gender

## female wilson score CI
ci_female <- binom.confint(x = 51, n = 148, conf.level = 0.95, methods = "wilson")
ci_female

## male wilson score CI
ci_male <- binom.confint(x = 42, n = 127, conf.level = 0.95, methods = "wilson")
ci_male



## tabulate by ethnicity
table_ethnicity3 <- table(ImpactsOfCOVID19_clean$ethnicity3, ImpactsOfCOVID19_clean$fc_s1results)
table_ethnicity3

## Asian descent group wilson score CI
ci_asian <- binom.confint(x = 33, n = 96, conf.level = 0.95, methods = "wilson")
ci_asian

# African descent group wilson score CI
ci_african <- binom.confint(x = 35, n = 92, conf.level = 0.95, methods = "wilson")
ci_african

## Other (Indigenous/Amerindian, Mixed, Caucasian, Other) group wilson score CI
ci_other <- binom.confint(x = 25, n = 87, conf.level = 0.95, methods = "wilson")
ci_other



# Univariate analysis

## levels for agechild_catc1
levels(ImpactsOfCOVID19_clean$agechild_catc1)

## univariate analysis for agechild_cat
u_agechild_catc1 <- glm(fc_s1results ~ agechild_catc1,
                        data = ImpactsOfCOVID19_clean,
                        family = "binomial")

summary(u_agechild_catc1)

## unadjusted OR for agechild_cat
exp(coef(u_agechild_catc1)["agechild_catc13-4"])

## 95% CI for the unadjusted OR for agechild_catc1
exp(confint(u_agechild_catc1,
            parm = "agechild_catc13-4"))



## levels for gender_child
levels(ImpactsOfCOVID19_clean$gender_child)

## univariate analysis for agechild_cat
u_gender_child <- glm(fc_s1results ~ gender_child,
                      data = ImpactsOfCOVID19_clean,
                      family = "binomial")

summary(u_gender_child)

## unadjusted OR for gender_child
exp(coef(u_gender_child)["gender_childMale"])

## 95% CI for the unadjusted OR for gender_child
exp(confint(u_gender_child,
            parm = "gender_childMale"))



## levels for ethnicity3
levels(ImpactsOfCOVID19_clean$ethnicity3)

## univariate analysis for ethnicity3
u_ethnicity3 <- glm(fc_s1results ~ ethnicity3,
                    data = ImpactsOfCOVID19_clean,
                    family = "binomial")

summary(u_ethnicity3)

## unadjusted OR for ethnicity3
exp(coef(u_ethnicity3)["ethnicity3African descent"])

## 95% CI for the unadjusted OR for ethnicity3
exp(confint(u_ethnicity3,
            parm = "ethnicity3African descent"))

## unadjusted OR for ethnicity3
exp(coef(u_ethnicity3)["ethnicity3Other Other (Indigenous/Amerindian, Mixed, Caucasian, Other)"])

## 95% CI for the unadjusted OR for ethnicity3
exp(confint(u_ethnicity3,
            parm = "ethnicity3Other Other (Indigenous/Amerindian, Mixed, Caucasian, Other)"))









# load package if necessary
library(tidyverse)

# read in data
ImpactsOfCOVID19_Ranalysis <- read.csv("ImpactsOfCOVID19_Ranalysis.csv")

# select necessary columns and factor variables
ImpactsOfCOVID19_Ranalysis <- ImpactsOfCOVID19_Ranalysis %>% 
  select(sc_s1results, 
         agechild_cat,
         ethnicity3,
         gender_child,
         region_residence
  ) %>%
  mutate(sc_s1results = as.factor(sc_s1results),
         agechild_cat = as.factor(agechild_cat),
         ethnicity3 = as.factor(ethnicity3),
         gender_child = as.factor(gender_child),
         region_residence = as.factor(region_residence)
  )

# remove rows with NA 
ImpactsOfCOVID19_clean <- na.omit(ImpactsOfCOVID19_Ranalysis)



# Label variables

## load package if necessary
library(Hmisc)

## Adding labels for sc_s1results and its levels
ImpactsOfCOVID19_clean$sc_s1results <- factor(ImpactsOfCOVID19_clean$sc_s1results, levels = c(0, 1),
                                              labels = c("Seronegative", "Seropositive"))

label(ImpactsOfCOVID19_clean$sc_s1results) <- "SARS-CoV-2 Seroprevalence (C2)"


## Adding labels for Age of child (years)
ImpactsOfCOVID19_clean$agechild_cat <- factor(ImpactsOfCOVID19_clean$agechild_cat, levels = c(0, 1),
                                              labels = c("3-4", "5-6"))

label(ImpactsOfCOVID19_clean$agechild_cat) <- "Age of child in years"


## Adding labels for ethnicity3 and its levels
ImpactsOfCOVID19_clean$ethnicity3 <- factor(ImpactsOfCOVID19_clean$ethnicity3, levels = c(1, 2, 3),
                                            labels = c("Asian descent", "African descent", "Other (Indigenous/Amerindian, Mixed, Caucasian, Other)"))

label(ImpactsOfCOVID19_clean$ethnicity3) <- "Ethnicity"

# Adding labels for gender_child and its levels
ImpactsOfCOVID19_clean$gender_child <- factor(ImpactsOfCOVID19_clean$gender_child, levels = c(0, 1),
                                              labels = c("Female", "Male"))

label(ImpactsOfCOVID19_clean$gender_child) <- "Gender"


# Adding labels for region_residence and its levels
ImpactsOfCOVID19_clean$region_residence <- factor(ImpactsOfCOVID19_clean$region_residence, levels = c(0, 1),
                                                  labels = c("Paramaribo region", "Nickerie region"))

label(ImpactsOfCOVID19_clean$region_residence) <- "Recruitment region"




# Dataset's structure

str(ImpactsOfCOVID19_clean)
ImpactsOfCOVID19_clean

## preview of the final data frame
head(ImpactsOfCOVID19_clean)

## basic descriptive statistics
summary(ImpactsOfCOVID19_clean)


# Crosstab
## load package if necessary
library(crosstable)
library(dplyr)

crosstable(ImpactsOfCOVID19_clean, c(agechildcat, ethnicity3, gender_child, region_residence), 
           by=sc_s1results, 
           total="both") %>% 
  as_flextable(keep_id=FALSE)




## load package if necessary
library(binom)

# Overall crude seroprevalence of C2 and 95% CIs

## 95% CIs for crude seroprevalence estimated using the exact binomial test of Wilson score interval method
## x is the count of seropositives
## n is the total sample size

seropos_count <- 278
totalsamplesize <- 298
prop.test(x = seropos_count, n = totalsamplesize, conf.level = 0.95, correct = FALSE)



## tabulate by age of child categorical
table_agechildcat <- table(ImpactsOfCOVID19_clean$agechild_cat, ImpactsOfCOVID19_clean$sc_s1results)
table_agechildcat

## 3-4 years old group wilson score CI
ci_34 <- binom.confint(x = 74, n = 78, conf.level = 0.95, methods = "wilson")
ci_34

## 5-6 years old group wilson score CI
ci_56 <- binom.confint(x = 204, n = 220, conf.level = 0.95, methods = "wilson")
ci_56



## tabulate by ethnicity
table_ethnicity3 <- table(ImpactsOfCOVID19_clean$ethnicity3, ImpactsOfCOVID19_clean$sc_s1results)
table_ethnicity3

## Asian descent group wilson score CI
ci_asian <- binom.confint(x = 101, n = 107, conf.level = 0.95, methods = "wilson")
ci_asian

## African descent group wilson score CI
ci_african <- binom.confint(x = 86, n = 93, conf.level = 0.95, methods = "wilson")
ci_african

## Other group wilson score CI
ci_other <- binom.confint(x = 91, n = 98, conf.level = 0.95, methods = "wilson")
ci_other



## tabulate by gender
table_gender <- table(ImpactsOfCOVID19_clean$gender_child, ImpactsOfCOVID19_clean$sc_s1results)
table_gender

## female wilson score CI
ci_female <- binom.confint(x = 152, n = 161, conf.level = 0.95, methods = "wilson")
ci_female

## male wilson score CI
ci_male <- binom.confint(x = 126, n = 137, conf.level = 0.95, methods = "wilson")
ci_male



## tabulate by region_residence
table_region <- table(ImpactsOfCOVID19_clean$region_residence, ImpactsOfCOVID19_clean$sc_s1results)
table_region

## Paramaribo wilson score CI
ci_pa <- binom.confint(x = 194, n = 208, conf.level = 0.95, methods = "wilson")
ci_pa

## Nickerie wilson score CI
ci_ni <- binom.confint(x = 84, n = 90, conf.level = 0.95, methods = "wilson")
ci_ni




# Univariate analysis

## levels for agechild_cat
levels(ImpactsOfCOVID19_clean$agechild_cat)

## univariate analysis for agechild_cat
u_agechild_cat <- glm(sc_s1results ~ agechild_cat,
                      data = ImpactsOfCOVID19_clean,
                      family = "binomial")

summary(u_agechild_cat)

## unadjusted OR for agechild_cat
exp(coef(u_agechild_cat)["agechild_cat5-6"])

## 95% CI for the unadjusted OR for agechild_cat
exp(confint(u_agechild_cat,
            parm = "agechild_cat5-6"))



## levels for ethnicity3
levels(ImpactsOfCOVID19_clean$ethnicity3)

## univariate analysis for ethnicity3
u_ethnicity3 <- glm(sc_s1results ~ ethnicity3,
                    data = ImpactsOfCOVID19_clean,
                    family = "binomial")

summary(u_ethnicity3)

## unadjusted OR for ethnicity3
exp(coef(u_ethnicity3)["ethnicity3African descent"])

## 95% CI for the unadjusted OR for ethnicity3
exp(confint(u_ethnicity3,
            parm = "ethnicity3African descent"))

## unadjusted OR for ethnicity3
exp(coef(u_ethnicity3)["ethnicity3Other (Indigenous/Amerindian, Mixed, Caucasian, Other)"])

## 95% CI for the unadjusted OR for ethnicity3
exp(confint(u_ethnicity3,
            parm = "ethnicity3Other (Indigenous/Amerindian, Mixed, Caucasian, Other)"))



## levels for gender_child
levels(ImpactsOfCOVID19_clean$gender_child)

## univariate analysis for gender_child
u_gender_child <- glm(sc_s1results ~ gender_child,
                      data = ImpactsOfCOVID19_clean,
                      family = "binomial")

summary(u_gender_child)

## unadjusted OR for gender_child
exp(coef(u_gender_child)["gender_childMale"])

## 95% CI for the unadjusted OR for gender_child
exp(confint(u_gender_child,
            parm = "gender_childMale"))



## levels for region_residence
levels(ImpactsOfCOVID19_clean$region_residence)

## univariate analysis for region_residence
u_region_residence <- glm(sc_s1results ~ region_residence,
                      data = ImpactsOfCOVID19_clean,
                      family = "binomial")

summary(u_region_residence)

## unadjusted OR for region_residence
exp(coef(u_region_residence)["region_residenceNickerie region"])

## 95% CI for the unadjusted OR for region_residence
exp(confint(u_region_residence,
            parm = "region_residenceNickerie region"))









# Social activities of children and family protection actions by district of residence (N=300)

## load package if necessary
library(tidyverse)
library(dplyr)

# read in data
ImpactsOfCOVID19_Ranalysis <- read.csv("ImpactsOfCOVID19_Ranalysis.csv")

# select necessary columns and factor variables
ImpactsOfCOVID19_Ranalysis <- ImpactsOfCOVID19_Ranalysis %>% 
  select(region_residence, district_residence, dcovid_child___1, dcovid_child___2, dcovid_child___3, dcovid_child___4, 
         dcovid_child___5, dcovid_child___6, dcovid_child___7, dcovid_child___8, dcovid_child___9,
         dcovid_child___10, dcovid_child___11, dcovid_child___12, dcovid_child___13, dcovid_protect___1, dcovid_protect___2, dcovid_protect___3, dcovid_protect___4, 
         dcovid_protect___5, dcovid_protect___6, dcovid_protect___7, dcovid_protect___8, dcovid_protect___9,
         dcovid_protect___10) %>%
  mutate(
    region_residence = as.factor(region_residence),
    district_residence = as.factor(district_residence),
    across(starts_with("dcovid_child___"), ~ as.numeric(.)),
    across(starts_with("dcovid_protect___"), ~ as.numeric(.))
  )


# Label variables 

## load package if necessary
library(Hmisc)

## Adding labels for region_residence and its levels
ImpactsOfCOVID19_Ranalysis$region_residence <- factor(ImpactsOfCOVID19_Ranalysis$region_residence, levels = c(0, 1),
                                                      labels = c("Paramaribo region", "Nickerie region"))

label(ImpactsOfCOVID19_Ranalysis$region_residence) <- "Recruitment region"

## Adding labels for district_residence and its levels
ImpactsOfCOVID19_Ranalysis$district_residence <- factor(ImpactsOfCOVID19_Ranalysis$district_residence, levels = c(1, 2, 3, 4, 5, 6, 7),
                                                        labels = c("Paramaribo", "Wanica", "Nickerie", "Saramacca", "Commewijne", "Para", "Coronie"))

label(ImpactsOfCOVID19_Ranalysis$district_residence) <- "District of residence"



# Dataset's structure 

str(ImpactsOfCOVID19_Ranalysis)
ImpactsOfCOVID19_Ranalysis

## preview of the final data frame
head(ImpactsOfCOVID19_Ranalysis)

## basic descriptive statistics
summary(ImpactsOfCOVID19_Ranalysis)



# Convert from wide to long format
so_dcovid <- ImpactsOfCOVID19_Ranalysis %>%
  pivot_longer(cols = starts_with("dcovid_child___"), names_to = "Social_Actions", values_to = "Selected") %>%
  filter(Selected == 1)  # Keep only selected options

# Adding labels to each multiple choice
## Create a named vector of labels
social_labels <- c(
  dcovid_child___1 = "Went out to restaurants for eat-in service",
  dcovid_child___2 = "Went to essential stores",
  dcovid_child___3 = "Went to non-essential stores",
  dcovid_child___4 = "Attended in-person religious services",
  dcovid_child___5 = "Attended in-person community events",
  dcovid_child___6 = "Attended/participated in inperson sports-related activities",
  dcovid_child___7 = "Went to outdoor public places ",
  dcovid_child___8 = "Went to indoor public places",
  dcovid_child___9 = "Attended in-person social gatherings",
  dcovid_child___10 = "Went to a friend's house/invited a friend over",
  dcovid_child___11 = "Went on an airplane",
  dcovid_child___12 = "Traveled to a different district or country",
  dcovid_child___13 = "The child did not do any of the social activities"
)

# Map using named vector
so_dcovid <- so_dcovid %>%
  mutate(Social_Label = social_labels[Social_Actions])

# Convert from wide to long format
pro_dcovid <- ImpactsOfCOVID19_Ranalysis %>%
  pivot_longer(cols = starts_with("dcovid_protect___"), names_to = "Protection_Actions", values_to = "Selected") %>%
  filter(Selected == 1)  # Keep only selected options

# Adding labels to each multiple choice
## Create a named vector of labels
protection_labels <- c(
  dcovid_protect___1 = "Wash your hands for 20 seconds with soap and water",
  dcovid_protect___2 = "Use sanitizers",
  dcovid_protect___3 = "Wear a mask to cover nose and chin when out of the house",
  dcovid_protect___4 = "Stand 6 feet apart from people",
  dcovid_protect___5 = "Stay at home",
  dcovid_protect___6 = "Not touch your face",
  dcovid_protect___7 = "Use home medicines to protect health",
  dcovid_protect___8 = "Change your diet",
  dcovid_protect___9 = "None of the above",
  dcovid_protect___10 = "Other"
)

# Map using named vector
pro_dcovid <- pro_dcovid %>%
  mutate(Protection_Label = protection_labels[Protection_Actions])

# Grouped barplots by district_residence

# Grouped barplot of social activities of children-Count
ggplot(so_dcovid, aes(x = Social_Label, fill = district_residence)) +
  geom_bar(position = "dodge") +  # Use count with grouped bars
  coord_flip() +  # Horizontal bars
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                               "#FFFF33", "#A65628"
  ),
  name = "District of residence"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 85)) +  # y-axis starts at 0
  labs(
    title = "Social activities of children during the COVID-19 pandemic by district of residence",
    x = "",
    y = "Count",
    fill = "District of residence"
  ) +
  theme_grey()


# Grouped barplot of family protection actions-Count
ggplot(pro_dcovid, aes(x = Protection_Label, fill = district_residence)) +
  geom_bar(position = "dodge") +  # Use count with grouped bars
  coord_flip() +  # Horizontal bars
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                               "#FFFF33", "#A65628"
  ),
  name = "District of residence"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 125)) +  # y-axis starts at 0
  labs(
    title = "Family protection actions during the COVID-19 pandemic by district of residence",
    x = "",
    y = "Count",
    fill = "District of residence"
  ) +
  theme_grey()


# Combining the ggplot2 plots together 
## load package if necessary
library(ggplot2)
library(patchwork)

# Plot 1: social activities of children
p1 <- ggplot(so_dcovid, aes(x = Social_Label, fill = district_residence)) +
  geom_bar(position = "dodge") +  # Use count with grouped bars
  coord_flip() +  # Horizontal bars
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                               "#FFFF33", "#A65628"
  ),
  name = "District of residence"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 85)) +  # y-axis starts at 0
  labs(
    title = "Social activities of children during the COVID-19 pandemic",
    x = "",
    y = "Count",
    fill = "District of residence"
  ) +
  theme_grey()

# Plot 2: family protection actions
p2 <- ggplot(pro_dcovid, aes(x = Protection_Label, fill = district_residence)) +
  geom_bar(position = "dodge") +  # Use count with grouped bars
  coord_flip() +  # Horizontal bars
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                               "#FFFF33", "#A65628"
  ),
  name = "District of residence"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 125)) +  # y-axis starts at 0
  labs(
    title = "Family protection actions during the COVID-19 pandemic",
    x = "",
    y = "Count",
    fill = "District of residence"
  ) +
  theme_grey()


# Combine plots using patchwork
(p1) / (p2)


# Combine plots and label each panel with letters

(p1) / (p2) +
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.tag = element_text(size = 14, face = "bold"),  # style of label
    plot.tag.position = c(0, 1)  # (x, y) inside panel; (0,1) = top-left
  )









# Types of nose swab from children tested for COVID-19 at any point in time

## load package if necessary
library(tidyverse)

## read in data
ImpactsOfCOVID19_Ranalysis <- read.csv("ImpactsOfCOVID19_Ranalysis.csv")

## select necessary columns and factor variables
ImpactsOfCOVID19_Ranalysis <- ImpactsOfCOVID19_Ranalysis %>% 
  select(noseswab4, 
         typenoseswabtest3) %>%
  mutate(noseswab4 = as.factor(noseswab4),
         typenoseswabtest3 = as.factor(typenoseswabtest3))

## remove rows with NA 
ImpactsOfCOVID19_clean <- na.omit(ImpactsOfCOVID19_Ranalysis)



# Label variables

## load package if necessary
library(Hmisc)

## Adding labels for noseswab4 and its levels
ImpactsOfCOVID19_clean$noseswab4 <- factor(ImpactsOfCOVID19_clean$noseswab4, levels = c(1, 2, 3, 4),
                                              labels = c("Never tried to get the child tested", "Tried to get the child tested but was not able to", "Nose swab negative", "Nose swab positive"))

label(ImpactsOfCOVID19_clean$noseswab4) <- "Nose swab test results"

## Adding labels for typenoseswabtest3
ImpactsOfCOVID19_clean$typenoseswabtest3 <- factor(ImpactsOfCOVID19_clean$typenoseswabtest3, levels = c(0, 1, 2),
                                                labels = c("PCR", "Antigen", "I am not sure"))

label(ImpactsOfCOVID19_clean$typenoseswabtest3) <- "Type of nose swab test"




# Dataset's structure

str(ImpactsOfCOVID19_clean)
ImpactsOfCOVID19_clean

## preview of the final data frame
head(ImpactsOfCOVID19_clean)

## basic descriptive statistics
summary(ImpactsOfCOVID19_clean)


# Crosstab 
## load package if necessary
library(crosstable)
library(dplyr)

crosstable(ImpactsOfCOVID19_clean, c(typenoseswabtest3), by=noseswab4, 
           total="both") %>% 
  as_flextable(keep_id=FALSE)









# Variables not included in the final model due to low counts at their levels

## load package if necessary
library(tidyverse)

## read in data
ImpactsOfCOVID19_Ranalysis <- read.csv("ImpactsOfCOVID19_Ranalysis.csv")

## select necessary columns and factor variables
ImpactsOfCOVID19_Ranalysis <- ImpactsOfCOVID19_Ranalysis %>% 
  select(curfew3, 
         nowprot3) %>%
  mutate(curfew3 = as.factor(curfew3),
         nowprot3 = as.factor(nowprot3))

## remove rows with NA 
ImpactsOfCOVID19_clean <- na.omit(ImpactsOfCOVID19_Ranalysis)



# Label variables

## load package if necessary
library(Hmisc)

## Adding labels for curfew3 and its levels
ImpactsOfCOVID19_clean$curfew3 <- factor(ImpactsOfCOVID19_clean$curfew3, levels = c(1, 2, 3),
                                           labels = c("Never/Rarely", "Sometimes", "Often/Always"))

label(ImpactsOfCOVID19_clean$curfew3) <- "Curfew during the COVID-19 pandemic"

## Adding labels for nowprot3
ImpactsOfCOVID19_clean$nowprot3 <- factor(ImpactsOfCOVID19_clean$nowprot3, levels = c(1, 2, 3),
                                                   labels = c("None", "3 or fewer", "4 or more"))

label(ImpactsOfCOVID19_clean$nowprot3) <- "Family protection actions “now"


# Dataset's structure

str(ImpactsOfCOVID19_clean)
ImpactsOfCOVID19_clean

## preview of the final data frame
head(ImpactsOfCOVID19_clean)

## basic descriptive statistics
summary(ImpactsOfCOVID19_clean)
