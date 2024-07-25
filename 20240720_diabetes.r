rm(list = ls())

#Packages
library(tidyverse)       # Data manipulation and visualization
library(haven)           # Importing SPSS, Stata, SAS data
library(gtsummary)       # Summary statistics and tables
library(broom)           # Tidying model outputs
library(LogisticDx)      # Diagnostic tests for logistic regression
library(here)            # Manage file paths
library(writexl)         # Export data to Excel
library(sjPlot)          # Data visualization and tables
library(finalfit)        # Model fitting and reporting
library(dplyr)           # Data manipulation
library(rms)             # Regression modeling strategies
library(AICcmodavg)      # Model selection and multimodel inference
library(ResourceSelection) # Goodness-of-fit tests
library(readr)           # Reading rectangular text data
library(skimr)           # Summary statistics
library(visdat)          # Visualize missing data
library(lattice)         # Multivariate data visualization
library(VIM)             # Visualization and imputation of missing values
library(mice)            # Multiple imputation of missing data
library(ggplot2)         # Data visualization
library(ggdist)          # Visualizing distributions and uncertainty
library(sf)              # Handling spatial data
library(scales)          # Scale functions for visualization
library(sjstats)         # Statistical functions and summaries

### upload database ###
setwd("C:/Users/david/Documents/vaccination_uptake")
datos_ENHS <- read.csv("EESEadulto_2020.csv", sep =  ";", header = TRUE)

#selection of variables, renaming and filter
datos_ENHS_filter_pre <-
  datos_ENHS %>%
  dplyr::select(
    CCAA,
    SEXOa,
    EDADa,
    E2_1a,
    E4,
    E4b,
    ESTUDIOS,
    G21,
    G25c_12,
    G25a_12,
    G25b_12,
    G25c_12,
    N48,
    N58_3,
    N60a_1,
    N60a_2,
    N60a_3,
    N60a_4,
    O66,
    O78,
    O84_1,
    O84_2,
    O84_3,
    O84_4,
    O84_5,
    O84_6,
    P87_1a,
    P87_19a,
    Q88,
    R106,
    R107,
    R108_1,
    T112,
    V121,
    W127,
    X131,
    CLASE_PR,
    IMC,
    SEVERIDAD_DEPRESIVA,
    O84_1
  ) %>%
  dplyr::rename(
    "Autonomous_Comunity" = "CCAA",
    "Sex" = "SEXOa",
    "Age" = "EDADa",
    "Spanish_nationality" = "E2_1a",
    "Cohabitation" = "E4",
    "Marital_status" = "E4b",
    "Study_level" = "ESTUDIOS",
    "Health_perception" = "G21",
    "Ever_diabetes" = "G25a_12",
    "Last_12_months_diabetes" = "G25b_12",
    "Medical_diagnosis_diabetes" = "G25c_12",
    "Time_of_last_medical_visit" = "N48",
    "Hospitalization" = "O66",
    "Nurse_or_midwife_consultation" = "N58_3",
    "Homeopath_visit" = "N60a_1",
    "Acupuncturist_visit" = "N60a_2",
    "Naturist_visit" = "N60a_3",
    "Visit_another_alt_med" = "N60a_4",
    "Use_of_emergency_services" = "O78",
    "Public_insurance" = "O84_1",
    "Mutual_insurance" = "O84_2",
    "Private_insurance" = "O84_3",
    "Private_insurance2" = "O84_4",
    "Private_insurance3" = "O84_5",
    "No_health_insurance" = "O84_6",
    "Cold_medications" = "P87_1a",
    "Diabetes_medications" = "P87_19a",
    "Flu_vaccine" = "Q88",
    "No_medical_attention_due_to_waiting_list_last_12_months" = "R106",
    "No_medical_attention_due_to_transport_barriers" = "R107",
    "No_medical_attention_economical_barriers" = "R108_1",
    "Physical_activity" = "T112",
    "Tobacco" = "V121",
    "Alcohol" = "W127",
    "Social_support" = "X131",
    "Social_class" = "CLASE_PR",
    "BMI_factor" = "IMC",
    "Depressive_severity" = "SEVERIDAD_DEPRESIVA"
  )

##defining values NA
datos_ENHS_filter_pre[datos_ENHS_filter_pre == ''] <- NA
datos_ENHS_filter_pre[datos_ENHS_filter_pre == '98'] <- NA
datos_ENHS_filter_pre[datos_ENHS_filter_pre == '99'] <- NA

##keeing only diabetic population with data
datos_ENHS_filter <- datos_ENHS_filter_pre %>%
  dplyr::filter(Diabetes_medications == 1 |
                  Ever_diabetes == 1 |
                  Last_12_months_diabetes == 1) # "Diabetes_medications" = "P87_19a", "Ever_diabetes" = "G25a_12" "Last_12_months_diabetes" = "G25b_12",

##NA values in database principal
na_counts <- sapply(datos_ENHS_filter[, ], function(x)
  sum(is.na(x)))
total_na <- sum(rowSums(is.na(datos_ENHS_filter[, ])) > 0)
total_filtros <- nrow(datos_ENHS_filter)
porcentaje_total_na <- (total_na / total_filtros) * 100

#############################
###Descriptive statistics###
############################
mean_age <- mean(datos_ENHS_filter$Age, na.rm = TRUE)
sd_age <- sd(datos_ENHS_filter$Age, na.rm = TRUE)
n_age <- length(datos_ENHS_filter$Age)
min(datos_ENHS_filter$Age, na.rm = TRUE)
max(datos_ENHS_filter$Age, na.rm = TRUE)
se_age <- sd_age / sqrt(n_age)
z_score <- qnorm(0.975)
lower_CI <- mean_age - z_score * se_age
upper_CI <- mean_age + z_score * se_age

### % diabetic population vaccinated ###
total_vaccinated_diab <- datos_ENHS_filter %>%
  dplyr::filter(Flu_vaccine == "Vaccinated") %>%
  nrow()

vaccination_rate_diab<-
  (total_vaccinated_diab / nrow(datos_ENHS_filter)) * 100


### % total (including diabetic) population vaccinated ###
total_vaccinated <- datos_ENHS_filter_pre %>%
  dplyr::filter(Flu_vaccine == 1) %>%
  nrow()

vaccination_rate_total <-
  (total_vaccinated / nrow(datos_ENHS_filter_pre)) * 100


### % total (excluding diabetic) population vaccinated ###
##keeing only diabetic population with data
datos_ENHS_filter_no_diab <- datos_ENHS_filter_pre %>%
  dplyr::filter(Diabetes_medications != 1 |
                  Ever_diabetes != 1 |
                  Last_12_months_diabetes != 1)

total_vaccinated_excl <- datos_ENHS_filter_no_diab %>%
  dplyr::filter(Flu_vaccine == 1) %>%
  nrow()

vaccination_rate_excl <-
  (total_vaccinated_excl / nrow(datos_ENHS_filter_no_diab)) * 100

###########################
### Renaming categories ###
###########################
# recoding age
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Age_group = factor(
    case_when(Age >= 15 & Age <= 59 ~ "< 60 years",
              Age >= 60             ~ "> 60 years",
              TRUE ~ NA),
    levels = c("> 60 years", "< 60 years")
  ))

# age revisors suggestions analyses per groups
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Age_group_recod = factor(
    case_when(Age >= 15 & Age <= 19 ~ "15 - 19 years",
              Age >= 20 & Age <= 39 ~ "20 - 39 years",
              Age >= 40 & Age <= 59 ~ "40 - 59 years",
              Age >= 60 ~ "> 60 years",
              TRUE ~ NA),
    levels = c("15 - 19 years", 
               "20 - 39 years", 
               "40 - 59 years", 
               "> 60 years")
  ))


# recoding depressive severity
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Depressive_severity = factor(
    case_when(
      Depressive_severity == 1 ~ "None",
      Depressive_severity == 2 |
        Depressive_severity == 3 ~ "Moderate",
      Depressive_severity == 4 ~ "Moderately severe",
      Depressive_severity == 5 ~ "Severe",
      Depressive_severity == 9 ~ NA_character_ ## 9 = no answer
    ),
    levels = c("None", "Moderate", "Moedarately severe", "Severe")
  ))

# recoding BMI
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(BMI_factor = factor(
    case_when(
      BMI_factor == 1 ~ "Underweight",
      BMI_factor == 2 ~ "Normal",
      BMI_factor == 3 ~ "Overweight",
      BMI_factor == 4 ~ "Obesity",
      BMI_factor == 9 ~ NA_character_ ## 9, no answer
    )
  ))

# recoding social class
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Social_class = factor(
    case_when(
      Social_class == 1 | Social_class == 2 ~ "Directors and managers",
      Social_class == 3 ~ "Intermediate occupations and self-employed workers",
      Social_class == 4 ~ "Supervisors and technical skilled workers",
      Social_class == 5 ~ "Skilled and semi-skilled workers in the primary sector",
      Social_class == 6 ~ "Unskilled workers",
      Social_class %in% c(8, 9) ~ NA_character_ ##8 AND 9, don't know - no answer
    )
  ))

# recoding social support
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Social_support = factor(
    case_when(
      Social_support == 1 ~ "A lot",
      Social_support == 2 ~ "Somewhat",
      Social_support == 3 |
        Social_support == 4 | Social_support == 5 ~ "Little or nothing",
      Social_support %in% c(8, 9) ~ NA_character_ ##8 AND 9, don't know - no answer
    ),
    levels = c("A lot", "Somewhat",
               "Little or nothing")
  ))

# recoding health insurance
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Health_insurance = factor(
    case_when(
      Public_insurance == 1 ~ "Public",
      Mutual_insurance == 1 |
        Private_insurance == 1 |
        Private_insurance2 == 1 | Private_insurance3 == 1 ~ "Private",
      No_health_insurance == 1 ~ "No health insurance",
      TRUE ~ NA_character_
    ),
    levels = c("Public", "Private", "No health insurance")
  ))

# recodig pseudotherapy
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Pseudotherapy = factor(
    case_when(
      Homeopath_visit == 1 |
        Acupuncturist_visit == 1 |
        Naturist_visit == 1 | Visit_another_alt_med == 1 ~ "Yes",
      TRUE ~ "Never"
    )
  ))

# recoding marital status
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Marital_status = factor(
    case_when(
      Marital_status == 1 ~ "Single",
      Marital_status == 2 ~ "Married",
      Marital_status == 3 ~ "Widowed",
      Marital_status == 4 | Marital_status == 5  ~ "Divorced",
      Marital_status %in% c(8, 9) ~ NA_character_ ##8 AND 9, don't know - no answer
    ),
    levels = c("Single", "Married", "Widowed", "Divorced")
  ))

# recoding cohabitation
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Cohabitation = factor(
    case_when(
      Cohabitation == 1 | Cohabitation == 2  ~ "Yes",
      Cohabitation == 3 ~ "No",
      Cohabitation %in% c(8, 9) ~ NA_character_
    ),
    levels = c("Yes", "No")
  ))

# recoding nationality
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Spanish_nationality = factor(
    ifelse(Spanish_nationality == 1, "Yes", "No"),
    levels = c("Yes", "No")
  ))

# recoding sex
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Sex = factor(ifelse(Sex == 1, "Men", "Women"), levels = c("Men", "Women")))

# recoding autonomous community
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Autonomous_Comunity = factor(
    case_when(
      Autonomous_Comunity == 01 ~ "Andalucía",
      Autonomous_Comunity == 02 ~ "Aragón",
      Autonomous_Comunity == 03 ~ "Principado de Asturias",
      Autonomous_Comunity == 04 ~ "Illes Balears",
      Autonomous_Comunity == 05 ~ "Canarias",
      Autonomous_Comunity == 06 ~ "Cantabria",
      Autonomous_Comunity == 07 ~ "Castilla y León",
      Autonomous_Comunity == 08 ~ "Castilla-La Mancha",
      Autonomous_Comunity == 09 ~ "Cataluña/Catalunya",
      Autonomous_Comunity == 10 ~ "Comunitat Valenciana",
      Autonomous_Comunity == 11 ~ "Extremadura",
      Autonomous_Comunity == 12 ~ "Galicia",
      Autonomous_Comunity == 13 ~ "Comunidad de Madrid",
      Autonomous_Comunity == 14 ~ "Región de Murcia",
      Autonomous_Comunity == 15 ~ "Comunidad Foral de Navarra",
      Autonomous_Comunity == 16 ~ "País Vasco/Euskadi",
      Autonomous_Comunity == 17 ~ "La Rioja",
      Autonomous_Comunity == 18 ~ "Ciudad Autónoma de Ceuta",
      Autonomous_Comunity == 19 ~ "Ciudad Autónoma de Melilla",
      TRUE ~ NA_character_
    )
  ))

# recoing alcohol
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Alcohol = factor(
    case_when(
      Alcohol == 01 |
        Alcohol == 02 |
        Alcohol == 03 |
        Alcohol == 04 | Alcohol == 05  ~ "More than once per week",
      Alcohol == 06 |  Alcohol == 07 ~ "Once per month or less",
      Alcohol == 08 |
        Alcohol == 09 ~ "Never or not in the last 12 months",
      TRUE ~ NA_character_
    ),
    levels = c(
      "Never or not in the last 12 months",
      "Once per month or less",
      "More than once per week"
    )
  ))

# recoing tobacco
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Tobacco = factor(
    case_when(
      Tobacco == 1 | Tobacco == 2 | Tobacco == 3 ~ "Yes",
      Tobacco == 4 ~ "No",
      TRUE ~ NA_character_
    ),
    levels = c("Yes", "No")
  ))

# recoing physical activity
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Physical_activity = factor(
    case_when(
      Physical_activity == 1 ~ "Sedentary lifestyle",
      Physical_activity == 2 ~ "Occasional activity",
      Physical_activity == 3 ~ "Monthly activity",
      Physical_activity == 4 ~ "Weekly training",
      TRUE ~ NA_character_
    )
  ))

# recoding no medical attention due to economical barriers
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(No_medical_attention_economical_barriers = factor(
    case_when(
      No_medical_attention_economical_barriers == 1 ~ "Yes",
      No_medical_attention_economical_barriers == 2 |
        No_medical_attention_economical_barriers == 3 ~ "No",
      TRUE ~ NA_character_
    ),
    levels = c("No", "Yes")
  ))

# recoding no medical attention due to waiting list
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(No_medical_attention_due_to_waiting_list_last_12_months = factor(
    case_when(
      No_medical_attention_due_to_waiting_list_last_12_months == 1 ~ "Yes",
      No_medical_attention_due_to_waiting_list_last_12_months == 2 |
        No_medical_attention_due_to_waiting_list_last_12_months == 3 ~ "No",
      TRUE ~ NA_character_
    ),
    levels = c("Yes", "No")
  ))

# recoding no medical attention due to transport barriers
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(No_medical_attention_due_to_transport_barriers = factor(
    case_when(
      No_medical_attention_due_to_transport_barriers == 1 ~ "Yes",
      No_medical_attention_due_to_transport_barriers == 2 |
        No_medical_attention_due_to_transport_barriers == 3 ~ "No",
      TRUE ~ NA_character_
    ),
    levels = c("Yes", "No")
  ))

# recoding nurse or midwife consultation
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Nurse_or_midwife_consultation = factor(
    case_when(
      Nurse_or_midwife_consultation == 1 ~ "Yes",
      Nurse_or_midwife_consultation == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    levels = c("Yes", "No")
  ))

# recoding cold medications
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Cold_medications = factor(
    case_when(
      Cold_medications == 1 ~ "Yes",
      Cold_medications == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    levels = c("Yes", "No")
  ))

# recoding use of emergency services
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Use_of_emergency_services = factor(
    case_when(
      Use_of_emergency_services == 1 ~ "Yes",
      Use_of_emergency_services == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    levels = c("Yes", "No")
  ))

# recoding hospitalization
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Hospitalization = factor(
    case_when(
      Hospitalization == 1 ~ "Yes",
      Hospitalization == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    levels = c("Yes", "No")
  ))

# recoding flu vaccine
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Flu_vaccine = factor(
    case_when(
      Flu_vaccine == 1 ~ "Vaccinated",
      Flu_vaccine == 2 ~ "Unvaccinated",
      TRUE ~ NA_character_
    ),
    levels = c("Vaccinated", "Unvaccinated")
  ))

# recoding health perception
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Health_perception = factor(
    case_when(
      Health_perception %in% c(1, 2, 3) ~ "Good",
      Health_perception %in% c(4, 5) ~ "Poor",
      TRUE ~ NA_character_
    ),
    levels = c("Good", "Poor")
  ))

# recoding study level
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Study_level = factor(
    case_when(
      Study_level == 2  |
        Study_level == 3 ~ "Incomplete Primary Education",
      Study_level == 4  |
        Study_level == 5 ~ "Complete Primary Education",
      Study_level == 6  | Study_level == 7 ~ "Graduate",
      Study_level == 8  | Study_level == 9 ~ "Postgraduate",
      TRUE ~ NA_character_
    ),
    levels = c(
      "Incomplete Primary Education",
      "Complete Primary Education",
      "Graduate",
      "Postgraduate"
    )
  ))

# recoding time of last medical visit
datos_ENHS_filter <- datos_ENHS_filter %>%
  mutate(Time_of_last_medical_visit = factor(
    case_when(
      Time_of_last_medical_visit == 1 ~ "In the last 4 weeks",
      Time_of_last_medical_visit == 2 ~ "Between 4 weeks and 12 months",
      Time_of_last_medical_visit == 3 ~ "12 months or more ago",
      Time_of_last_medical_visit == 4 ~  NA,
      TRUE ~ NA_character_
    ),
    levels = c(
      "In the last 4 weeks",
      "Between 4 weeks and 12 months",
      "12 months or more ago",
      "Never"
    )
  ))

###General Descriptive table###
labels <- list(
  Sex = "Sex",
  Age_group = "Age group",
  Age_group_recod = "Age groups",
  Spanish_nationality = "Spanish nationality",
  Autonomous_Comunity = "Autonomous Community",
  Marital_status = "Marital status",
  Study_level = "Study level",
  Health_perception = "Health perception",
  Health_insurance = "Health insurance",
  Time_of_last_medical_visit = "Time of last medical visit",
  Depressive_severity = "Depressive severity",
  Social_class = "Social class",
  Social_support = "Social support",
  Alcohol = "Alcohol",
  Tobacco = "Tobacco",
  No_medical_attention_economical_barriers = "No medical attention for economical barriers",
  Nurse_or_midwife_consultation = "Nurse or midwife consultation",
  Cold_medications = "Cold medications",
  Use_of_emergency_services = "Use of emergency services",
  Hospitalization = "Hospitalization"
)

#table_diabetes_prev_strata#
table_diabetes_prev_strata <-
  datos_ENHS_filter %>%
  filter(!is.na(Age_group)) %>%
  dplyr::select(
    Flu_vaccine,
    Sex,
    Age_group,
    Spanish_nationality,
    Marital_status,
    Study_level,
    Health_perception,
    Health_insurance,
    Time_of_last_medical_visit,
    Depressive_severity,
    Social_class,
    Social_support,
    Alcohol,
    Tobacco,
    No_medical_attention_economical_barriers,
    Nurse_or_midwife_consultation,
    Cold_medications,
    Use_of_emergency_services,
    Hospitalization
  ) %>%
  droplevels() %>%
  tbl_strata(
    strata = Age_group,
    ~ .x %>%
      tbl_summary(
        by = Flu_vaccine,
        type = all_dichotomous() ~ "categorical",
        percent = "row",
        missing = "no",
        label = labels
      ) %>%
      add_overall() %>%
      add_p() %>%
      add_ci() %>%
      modify_header(label = "**Co-variables**") %>%
      modify_spanning_header(all_stat_cols() ~ "**Flu vaccination in diabetic population**") %>%
      bold_labels() %>%
      italicize_levels() %>%
      modify_footnote(p.value ~ "χ² and Fisher's exact tests")
  )

table_1 <- table_diabetes_prev_strata %>% as.data.frame()

# Write the list of data frames to an Excel file
write_xlsx(table_1, "table_diabetes_prev_strata.xlsx")



#table_diabetes_prev_strata#
table_diabetes_prev_rev <-
  datos_ENHS_filter %>%
  filter(!is.na(Age_group_recod)) %>%
  dplyr::select(
    Flu_vaccine,
    Sex,
    Age_group_recod,
    Spanish_nationality,
    Marital_status,
    Study_level,
    Health_perception,
    Health_insurance,
    Time_of_last_medical_visit,
    Depressive_severity,
    Social_class,
    Social_support,
    Alcohol,
    Tobacco,
    No_medical_attention_economical_barriers,
    Nurse_or_midwife_consultation,
    Cold_medications,
    Use_of_emergency_services,
    Hospitalization
  ) %>%
  droplevels() %>%
  tbl_summary(
        by = Flu_vaccine,
        type = all_dichotomous() ~ "categorical",
        percent = "row",
        missing = "no",
        label = labels
      ) %>%
      add_overall() %>%
      add_p() %>%
      add_ci() %>%
      modify_header(label = "**Co-variables**") %>%
      modify_spanning_header(all_stat_cols() ~ "**Flu vaccination in diabetic population**") %>%
      bold_labels() %>%
      italicize_levels() %>%
      modify_footnote(p.value ~ "χ² and Fisher's exact tests")

table_1_ages <- table_diabetes_prev_rev %>% as.data.frame()

# Write the list of data frames to an Excel file
write_xlsx(table_1_ages, "table_diabetes_prev_rev.xlsx")


#################################
######### MISSING VALUES ##########
#################################

##Frecuency of missing values
skim(datos_ENHS_filter)

# Ejecuta la función skim() en tus datos
skim_output <- skim(datos_ENHS_filter)

# Extrae el resumen de valores faltantes
skim_output$n_missing %>% sum()

# Muestra el resumen de valores faltantes
print(missing_values_summary)

## % of missing observations = 2.3%
plot_intro(datos_ENHS_filter)

## depresion status 4.7%)
plot_missing(datos_ENHS_filter)

## Looking for patterns in the NA values in those variables with > or close to 5% of NA values
visdat::vis_dat(datos_ENHS_filter)

aggr_plot_VIM_diabe2 <-
  aggr(
    datos_ENHS_filter,
    col = c('navyblue', 'red'),
    numbers =
      TRUE,
    sortVars = TRUE,
    labels = names(datos_ENHS_filter),
    cex.axis =
      .7,
    gap = 3,
    ylab = c("Histogram of missing data", "Pattern")
  )

###########################################
#### IMPUTING MISSING VALUES WITH MICE ####
###########################################

# diabetes_mice_rec <- mice(datos_ENHS_filter, m=5, maxit=50, meth='pmm',seed=500)
#
# imputed_mice_bmi <- complete(diabetes_mice_rec)
# imputed_social_sup <- complete(diabetes_mice_rec)
# imputed_social_class <- complete(diabetes_mice_rec)
# imputed_cold_medic <- complete(diabetes_mice_rec)
# imputed_depressive <- complete(diabetes_mice_rec)
# imputed_diab_medi <- complete(diabetes_mice_rec)
# imputed_medic_diag <- complete(diabetes_mice_rec)
# imputed_last_12 <- complete(diabetes_mice_rec)
#
# #only for those with >0,1 % of missing values
# colnames(imputed_mice_bmi)[colnames(imputed_mice_bmi) == "BMI_factor"] <- "BMI_factor_imp_mice"
# colnames(imputed_social_sup)[colnames(imputed_social_sup) == "Social_support"] <- "Social_support_imp_mice"
# colnames(imputed_social_class)[colnames(imputed_social_class) == "Social_class"] <- "Social_class_imp_mice"
# colnames(imputed_cold_medic)[colnames(imputed_cold_medic) == "Cold_medications"] <- "Cold_medications_imp_mice"
# colnames(imputed_depressive)[colnames(imputed_depressive) == "Depressive_severity"] <- "Depressive_severity_imp_mice"
# colnames(imputed_diab_medi)[colnames(imputed_diab_medi) == "Diabetes_medications"] <- "Diabetes_medications_imp_mice"
# colnames(imputed_medic_diag)[colnames(imputed_medic_diag) == "Medical_diagnosis_diabetes"] <- "Medical_diagnosis_diabetes_imp_mice"
# colnames(imputed_last_12)[colnames(imputed_last_12) == "Last_12_months_diabetes"] <- "Last_12_months_diabetes_imp_mice"
#
#
# datos_ENHS_filter$BMI_factor_imp_mice <- imputed_mice$BMI_factor_imp_mice
# datos_ENHS_filter$Social_support_imp_mice <- imputed_mice$Social_support_imp_mice
# datos_ENHS_filter$Social_class_imp_mice <- imputed_mice$Social_class_imp_mice
# datos_ENHS_filter$Cold_medications_imp_mice <- imputed_mice$Cold_medications_imp_mice
# datos_ENHS_filter$Depressive_severity_imp_mice <- imputed_mice$Depressive_severity_imp_mice
# datos_ENHS_filter$Diabetes_medications_imp_mice <- imputed_mice$Diabetes_medications_imp_mice
# datos_ENHS_filter$Medical_diagnosis_diabetes_imp_mice <- imputed_mice$Medical_diagnosis_diabetes_imp_mice
# datos_ENHS_filter$Last_12_months_diabetes_imp_mice <- imputed_mice$Last_12_months_diabetes_imp_mice



#########################################################################
########## MODEL manual backward regression stratified model #############
#########################################################################
flu_vaccine_complete_data_strata <- datos_ENHS_filter %>%
  select(
    Flu_vaccine,
    Sex,
    Age_group,
    Spanish_nationality,
    Marital_status,
    Study_level,
    Health_perception,
    Health_insurance,
    Time_of_last_medical_visit,
    Depressive_severity,
    Social_class,
    Social_support,
    Alcohol,
    Tobacco,
    No_medical_attention_economical_barriers,
    Nurse_or_midwife_consultation,
    Cold_medications,
    Use_of_emergency_services,
    Hospitalization
  )

##complete case analyses only
flu_vaccine_complete_data_strata <-
  flu_vaccine_complete_data_strata[complete.cases(flu_vaccine_complete_data_strata),] ## n=1890

## % of total missing values removed
missing_values_final_model <-
  (1 - nrow(flu_vaccine_complete_data_strata) / nrow(datos_ENHS_filter)) * 100

##all variables <0.2
model_1_strata_less.60 <-
  glm(
    Flu_vaccine ~  Sex + Spanish_nationality +
      Marital_status + Study_level + Health_perception + Health_insurance +
      Time_of_last_medical_visit + Depressive_severity + Social_class +
      Social_support + Alcohol + Tobacco + No_medical_attention_economical_barriers +
      Nurse_or_midwife_consultation + Cold_medications + Use_of_emergency_services + Hospitalization,
    data = subset(flu_vaccine_complete_data_strata, Age_group == "< 60 years"),
    family = binomial(link = 'logit')
  )

model_1_strata_less.60 %>% summary()

model_1_strata_over.60 <-
  glm(
    Flu_vaccine ~  Sex + Spanish_nationality +
      Marital_status + Study_level + Health_perception + Health_insurance +
      Time_of_last_medical_visit + Depressive_severity + Social_class +
      Social_support + Alcohol + Tobacco + No_medical_attention_economical_barriers +
      Nurse_or_midwife_consultation + Cold_medications + Use_of_emergency_services + Hospitalization,
    data = subset(flu_vaccine_complete_data_strata, Age_group == "> 60 years"),
    family = binomial(link = 'logit')
  )

model_1_strata_over.60 %>% summary()


##Keeping those <0.05 and sex
filter_significant_coeffs <- function(model, threshold = 0.05) {
  summary_data <- summary(model)
  coeffs <- coef(summary_data)
  p_values <- summary_data$coefficients[, "Pr(>|z|)"]
  significant_coeffs <- coeffs[p_values < threshold, , drop = FALSE]
  return(significant_coeffs)
}
significant_coeffs.less60 <- filter_significant_coeffs(model_1_strata_less.60, threshold = 0.05)
significant_coeffs.less60

significant_coeffs.over60 <- filter_significant_coeffs(model_1_strata_over.60, threshold = 0.05)
significant_coeffs.over60

############ TESTING INTERACTION TERMS ################
flu_vaccine_complete_data_strata <- datos_ENHS_filter %>%
  select(
    Flu_vaccine,
    Sex,
    Age_group,
    Study_level,
    Time_of_last_medical_visit,
    Social_support,
    No_medical_attention_economical_barriers,
    Nurse_or_midwife_consultation,
    Cold_medications
  )

##complete case analyses only
flu_vaccine_complete_data_strata_sign <-
  flu_vaccine_complete_data_strata[complete.cases(flu_vaccine_complete_data_strata),] ## n=1890


# Time_of_last_medical_visit * Nurse_or_midwife_consultation (interaction)
model_tlmvXnmc_int.60less <-
  glm(
    Flu_vaccine ~  Sex + Study_level + Time_of_last_medical_visit +
      No_medical_attention_economical_barriers + Social_support +
      Nurse_or_midwife_consultation + Time_of_last_medical_visit * Nurse_or_midwife_consultation +
      Cold_medications,
    data = subset(flu_vaccine_complete_data_strata_sign, Age_group == "< 60 years"),
    family = binomial(link = 'logit')
  )
model_tlmvXnmc_int.60less %>% summary()

model_tlmvXnmc_int.60over <-
  glm(
    Flu_vaccine ~  Sex + Study_level + Time_of_last_medical_visit +
      No_medical_attention_economical_barriers + Social_support +
      Nurse_or_midwife_consultation + Time_of_last_medical_visit * Nurse_or_midwife_consultation +
      Cold_medications,
    data = subset(flu_vaccine_complete_data_strata_sign, Age_group == "> 60 years"),
    family = binomial(link = 'logit')
  )

model_tlmvXnmc_int.60over %>% summary()

# Sex x Social_support (no interaction)
model_sexXss_int.60less <-
  glm(
    Flu_vaccine ~  Sex + Study_level + Time_of_last_medical_visit +
      No_medical_attention_economical_barriers + Social_support +
      Nurse_or_midwife_consultation + Cold_medications +
      Sex * Social_support,
    data = subset(flu_vaccine_complete_data_strata_sign, Age_group == "< 60 years"),
    family = binomial(link = 'logit')
  )
model_sexXss_int.60less %>% summary()

model_sexXss_int.60over <-
  glm(
    Flu_vaccine ~  Sex + Study_level + Time_of_last_medical_visit +
      No_medical_attention_economical_barriers + Social_support +
      Nurse_or_midwife_consultation + Cold_medications +
      Sex * Social_support,
    data = subset(flu_vaccine_complete_data_strata_sign, Age_group == "> 60 years"),
    family = binomial(link = 'logit')
  )
model_sexXss_int.60over %>% summary()


# Study_level x No_medical_attention_economical_barriers (no interaction)
model_slXnmaeb_int.60less <-
  glm(
    Flu_vaccine ~  Sex + Study_level + Time_of_last_medical_visit +
      No_medical_attention_economical_barriers + Social_support +
      Nurse_or_midwife_consultation + Cold_medications +
      Study_level * No_medical_attention_economical_barriers,
    data = subset(flu_vaccine_complete_data_strata_sign, Age_group == "< 60 years"),
    family = binomial(link = 'logit')
  )
model_slXnmaeb_int.60less %>% summary()

model_slXnmaeb_int.60over <-
  glm(
    Flu_vaccine ~  Sex + Study_level + Time_of_last_medical_visit +
      No_medical_attention_economical_barriers + Social_support +
      Nurse_or_midwife_consultation + Cold_medications +
      Study_level * No_medical_attention_economical_barriers,
    data = subset(flu_vaccine_complete_data_strata_sign, Age_group == "> 60 years"),
    family = binomial(link = 'logit')
  )
model_slXnmaeb_int.60over %>% summary()

# No_medical_attention_economical_barriers x Social_support (no interactio)
model_nmaebXss_int.60less <-
  glm(
    Flu_vaccine ~  Sex + Study_level + Time_of_last_medical_visit +
      No_medical_attention_economical_barriers + Social_support +
      Nurse_or_midwife_consultation + Cold_medications +
      No_medical_attention_economical_barriers * Social_support,
    data = subset(flu_vaccine_complete_data_strata_sign, Age_group == "< 60 years"),
    family = binomial(link = 'logit')
  )
model_nmaebXss_int.60less %>% summary()

model_nmaebXss_int.60over <-
  glm(
    Flu_vaccine ~  Sex + Study_level + Time_of_last_medical_visit +
      No_medical_attention_economical_barriers + Social_support +
      Nurse_or_midwife_consultation + Cold_medications +
      No_medical_attention_economical_barriers * Social_support,
    data = subset(flu_vaccine_complete_data_strata_sign, Age_group == "> 60 years"),
    family = binomial(link = 'logit')
  )
model_nmaebXss_int.60over %>% summary()

# Cold_medications x Time_of_last_medical_visit (no interaction)
model_cmXtlmv_int.60less <-
  glm(
    Flu_vaccine ~  Sex + Study_level + Time_of_last_medical_visit +
      No_medical_attention_economical_barriers + Social_support +
      Nurse_or_midwife_consultation + Cold_medications +
      Cold_medications * Time_of_last_medical_visit,
    data = subset(flu_vaccine_complete_data_strata_sign, Age_group == "< 60 years"),
    family = binomial(link = 'logit')
  )
model_cmXtlmv_int.60less %>% summary()

model_cmXtlmv_int.60over <-
  glm(
    Flu_vaccine ~  Sex + Study_level + Time_of_last_medical_visit +
      No_medical_attention_economical_barriers + Social_support +
      Nurse_or_midwife_consultation + Cold_medications +
      Cold_medications * Time_of_last_medical_visit,
    data = subset(flu_vaccine_complete_data_strata_sign, Age_group == "> 60 years"),
    family = binomial(link = 'logit')
  )
model_cmXtlmv_int.60over %>% summary()

#######################################################
## Final stratified model with significant variables ##
#######################################################
## final model with <0.05 p value + sex + age group as main confounders and interaction term
model_2_dt_strata_less_60 <-
  glm(
    Flu_vaccine ~  Sex + Study_level + Time_of_last_medical_visit +
      No_medical_attention_economical_barriers + Social_support +
      Nurse_or_midwife_consultation + Cold_medications +
      Time_of_last_medical_visit * Nurse_or_midwife_consultation,
    data = subset(flu_vaccine_complete_data_strata_sign, Age_group == "< 60 years"),
    family = binomial(link = 'logit')
  )

model_2_dt_strata_over_60 <-
  glm(
    Flu_vaccine ~  Sex + Study_level + Time_of_last_medical_visit +
      No_medical_attention_economical_barriers + Social_support +
      Nurse_or_midwife_consultation + Cold_medications +
      Time_of_last_medical_visit * Nurse_or_midwife_consultation,
    data = subset(flu_vaccine_complete_data_strata_sign, Age_group == "> 60 years"),
    family = binomial(link = 'logit')
  )

# summary table for each model
table_summary_less_60 <- tbl_regression(
  model_2_dt_strata_less_60,
  exponentiate = TRUE,
  include = c(
    "Sex",
    "Study_level",
    "Time_of_last_medical_visit",
    "No_medical_attention_economical_barriers",
    "Social_support",
    "Nurse_or_midwife_consultation",
    "Cold_medications"
  ),
  label = list(
    Sex = "Sex",
    Study_level = "Study level",
    Time_of_last_medical_visit = "Time of last medical visit",
    No_medical_attention_economical_barriers = "No medical attention for economical barriers",
    Social_support = "Social support",
    Nurse_or_midwife_consultation = "Nurse or midwife consultation",
    Cold_medications = "Cold medications"
  )
) %>%
  bold_labels() %>%
  italicize_levels()

table_summary_over_60 <- tbl_regression(
  model_2_dt_strata_over_60,
  exponentiate = TRUE,
  include = c(
    "Sex",
    "Study_level",
    "Time_of_last_medical_visit",
    "No_medical_attention_economical_barriers",
    "Social_support",
    "Nurse_or_midwife_consultation",
    "Cold_medications"
  ),
  label = list(
    Sex = "Sex",
    Study_level = "Study level",
    Time_of_last_medical_visit = "Time of last medical visit",
    No_medical_attention_economical_barriers = "No medical attention for economical barriers",
    Social_support = "Social support",
    Nurse_or_midwife_consultation = "Nurse or midwife consultation",
    Cold_medications = "Cold medications"
  )
) %>%
  bold_labels() %>%
  italicize_levels()


# merge the tables
combined_table_FINAL <- tbl_merge(
  list(table_summary_less_60, table_summary_over_60),
  tab_spanner = c("< 60 years", "> 60 years")
)

# export the table
combined_table_FINAL %>%
  as_tibble() %>%
  writexl::write_xlsx("table_2.xlsx")


#####################################################################
######################## UNADJUSTED MODEL ############################
######################################################################

# Function to fit simple logistic models and extract unadjusted ORs
get_unadjusted_or <- function(data, outcome, variables) {
  results <- lapply(variables, function(var) {
    # Fit simple logistic model
    formula <- as.formula(paste(outcome, "~", var))
    model <- glm(formula, data = data, family = binomial(link = 'logit'))

    # Create summary table
    tbl <- tbl_regression(model, exponentiate = TRUE, include = var)

    # Add variable label
    tbl <- tbl %>%
      bold_labels() %>%
      italicize_levels()

    return(tbl)
  })

  # Combine all tables into one
  combined_table <- tbl_stack(results)
  return(combined_table)
}

# Independent variables
variables <- c(
  "Sex",
  "Study_level",
  "Time_of_last_medical_visit",
  "No_medical_attention_economical_barriers",
  "Social_support",
  "Nurse_or_midwife_consultation",
  "Cold_medications"
)

# Outcome variable
outcome <- "Flu_vaccine"

# Stratified data by age group
data_less_60 <- subset(flu_vaccine_complete_data_strata_sign, Age_group == "< 60 years")
data_over_60 <- subset(flu_vaccine_complete_data_strata_sign, Age_group == "> 60 years")

# Obtain unadjusted ORs for each age group
unadjusted_or_less_60 <- get_unadjusted_or(data_less_60, outcome, variables)
unadjusted_or_over_60 <- get_unadjusted_or(data_over_60, outcome, variables)

# Combine tables
combined_unadjusted_table <- tbl_merge(
  list(unadjusted_or_less_60, unadjusted_or_over_60),
  tab_spanner = c("< 60 years", "> 60 years")
)

# Export the table
combined_unadjusted_table %>%
  as_tibble() %>%
  writexl::write_xlsx("unadjusted_or_table.xlsx")

# Print the table to the console (optional)
print(combined_unadjusted_table)



#####################################
###### MODEL WITH MICE IMPUTATIONS###
##############################################
#### full backward model with MICE imputations
##############################################
# flu_vaccine_complete_data_mice <- datos_ENHS_filter %>%
#   select(Flu_vaccine, BMI_factor_imp_mice, Sex, Age_group, Spanish_nationality,Marital_status,
#          Study_level,Health_perception, Health_insurance,
#          Time_of_last_medical_visit, Depressive_severity_imp_mice,
#          Social_support_imp_mice,Social_class_imp_mice,Alcohol,Tobacco,
#          No_medical_attention_economical_barriers,
#          Nurse_or_midwife_consultation, Cold_medications_imp_mice,Use_of_emergency_services,
#          Hospitalization)
#
# model_1_dt_mice <- glm(Flu_vaccine ~  Sex + Age_group + Spanish_nationality + BMI_factor_imp_mice +
#                     Marital_status + Study_level + Health_perception + Health_insurance +
#                     Time_of_last_medical_visit + Depressive_severity_imp_mice + Social_support_imp_mice +
#                     Social_class_imp_mice + Alcohol + Tobacco + No_medical_attention_economical_barriers +
#                     Nurse_or_midwife_consultation + Cold_medications_imp_mice + Use_of_emergency_services+ Hospitalization,
#                   data = flu_vaccine_complete_data_mice, family = binomial(link = 'logit'))
#
# model_1_dt_mice %>% summary()

##Keeping those <0.05 and sex
# filter_significant_coeffs <- function(model, threshold = 0.05) {
#   summary_data <- summary(model)
#   coeffs <- coef(summary_data)
#   p_values <- summary_data$coefficients[, "Pr(>|z|)"]
#   significant_coeffs <- coeffs[p_values < threshold, , drop = FALSE]
#   return(significant_coeffs)
# }
#
# significant_coeffs <- filter_significant_coeffs(model_1_dt_mice, threshold = 0.05)
# significant_coeffs
#
# # model 2 with MICE imputations
# model_2_dt_mice <- glm(Flu_vaccine ~  Sex + Age_group + Study_level + Time_of_last_medical_visit +
#                          Depressive_severity_imp_mice + No_medical_attention_economical_barriers
#                        + Social_support_imp_mice + Nurse_or_midwife_consultation + Cold_medications_imp_mice,
#                   data = flu_vaccine_complete_data_mice, family = binomial(link = 'logit'))
#
# model_2_dt_mice %>% summary()


######################################
####Testing fitness of final model####
######################################
##Maximum likelihood##
# interaction model vs no interaction final model
logLik(model_tlmvXnmc_int.60less) #better
logLik(model_tlmvXnmc_int.60over) #better
logLik(model_2_dt_strata_less_60)
logLik(model_2_dt_strata_over_60)

#AIC#
model_tlmvXnmc_int.60less$aic #better
model_tlmvXnmc_int.60over$aic #better
model_2_dt_strata_less_60$aic
model_2_dt_strata_over_60$aic

#BIC#
BIC(model_tlmvXnmc_int.60less)  #better
BIC(model_tlmvXnmc_int.60over)  #better
BIC(model_2_dt_strata_less_60)
BIC(model_2_dt_strata_over_60)

########################
###MODEL ASSUMPTIONS####
########################
##Collinearity
#No collinearity
library(car)
vif(model_2_dt_strata_over_60) ##VIF controlled without interaction terms
vif(model_2_dt_strata_over_60) ##VIF controlled without interaction terms

#######################
####MAP COVERAGE#######
#######################

## Map for diabetic population##

file.shp <-
  "recintos_autonomicas_inspire_peninbal_etrs89.shp" # official shapefile
ccaa <- st_read(file.shp)

prev_ccaa_flu <- datos_ENHS_filter %>%
  select(Flu_vaccine, Autonomous_Comunity) %>%
  filter(!is.na(Autonomous_Comunity)) %>%
  group_by(Autonomous_Comunity) %>%
  summarise(frequency = n(),
            coverage = round(sum(Flu_vaccine == "Vaccinated") / n() * 100, 1))

abbreviations <- c(
  "Andalucía" = "Andalucía",
  "Aragón" = "Aragón",
  "Principado de Asturias" = "Asturias",
  "Illes Balears" = "Baleares",
  "Canarias" = "Canarias",
  "Cantabria" = "Cantabria",
  "Castilla y León" = "Castilla y León",
  "Castilla-La Mancha" = "Castilla-La Mancha",
  "Cataluña/Catalunya" = "Cataluña",
  "Comunitat Valenciana" = "Valencia",
  "Extremadura" = "Extremadura",
  "Galicia" = "Galicia",
  "Comunidad de Madrid" = "Madrid",
  "Región de Murcia" = "Murcia",
  "Comunidad Foral de Navarra" = "Navarra",
  "País Vasco/Euskadi" = "País Vasco",
  "La Rioja" = "La Rioja",
  "Ciudad Autónoma de Ceuta" = "Ceuta",
  "Ciudad Autónoma de Melilla" = "Melilla"
)

prev_ccaa_flu

match_ccaa <- match(prev_ccaa_flu$Autonomous_Comunity, ccaa$NAMEUNIT)
prev_ccaa_flu$geom <- ccaa$geometry[match_ccaa]

map_diab_flu <- ggplot(data = prev_ccaa_flu) +
  geom_sf(aes(fill = coverage, geometry = geom)) +
  geom_sf(
    data = prev_ccaa_flu,
    fill = NA,
    color = "black",
    aes(geometry = geom)
  ) +
  scale_fill_gradientn(
    colours = c("darkred", "red", "orange", "yellow", "green"),
    breaks = seq(0, 100, by = 10),
    labels = scales::number_format(
      scale = 1,
      suffix = "",
      accuracy = 1
    ),
    guide = guide_colorbar(
      title = "Coverage (%)",
      direction = "horizontal",
      title.position = "top",
      title.theme = element_text(size = 8)
    )
  ) +
  geom_sf_label(
    data = prev_ccaa_flu,
    aes(label = coverage, geometry = geom),
    size = 2.8,
    color = "black",
    label.size = NA,
    fill = NA,
    nudge_y = -0.01
  ) +
  geom_sf_label(
    data = prev_ccaa_flu,
    aes(label = Autonomous_Comunity, geometry = geom),
    size = 2.8,
    color = "black",
    label.size = NA,
    fill = NA,
    nudge_y = 0.18
  ) +
  labs(fill = "Prevalence (%)") +
  theme(
    legend.position = "bottom",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank()
  )

map_diab_flu


file.plot.diab_flu <- "Map_vaccine_coverage_diab.png"
output_path <- "C:/Users/david/Desktop/EPH/Database_diabetes"
ggsave(
  filename = file.plot.diab_flu,
  plot = map_diab_flu,
  path = output_path,
  device = "png",
  width = 1100 / 300,
  height = 560 / 300,
  dpi = 300
)


### Statistical power ###
# Install and load the necessary package
library(pwr)

# Define the parameters
P1 <- 0.47  # Proportion of unvaccinated individuals in the sample
P0 <- 0.50  # Reference proportion (e.g., 50%)

# Calculate Cohen's h for proportions
h <- ES.h(P1, P0)

# Perform the power calculation
power_calculation <- pwr.p.test(
  h = h,
  n = 2193,
  sig.level = 0.05,
  alternative = "two.sided"
)

# Display the results
print(power_calculation)
