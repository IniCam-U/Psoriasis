library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(moments)

# Importing Excel
Psosol_data <- read_excel("Qol_Psoriasis.xlsx")
View(Psosol_data)
PsoProve_RCT <- read_excel("PsoProve2_RCT dataset.xlsx")
View(PsoProve_RCT)

# Join work_impair_0 from PsoProve_RCT to Psosol_data
Psosol_data <- Psosol_data %>%
  left_join(PsoProve_RCT %>% select(ID, PASI_week_bas, PASI_week_mid, PASI_week_end, Work_impair_0, Work_impair_52, Spe_visits, GP_visits, Phototx_qty, Systx_strt_wk, Psotop_qty), by = "ID")

# Define variables
phototx_cost <- 812
specialist_cost <- 3770.70
gp_cost <- 1862.40
systemicthx_cost <- 3839.70
psosol_cost <- 521
hicp_2011 <- 97.75
hicp_2022 <- 119.39
infla_factor <- hicp_2022 / hicp_2011

#Labeling the variables
Psosol_data$Group <- factor(Psosol_data$Group, levels = c(0, 1), labels = c("Placebo", "Treatment"))
Psosol_data$Gender <- factor(Psosol_data$Gender, levels = c(1, 2), labels = c("Male", "Female"))

#Exploring the demographics of the sample
gender_count <- table(Psosol_data$Gender, Psosol_data$Group)
gender_count
gender_prop <- prop.table(gender_count, margin = 1)
gender_prop
chi_sq_gender <- chisq.test(gender_count)
chi_sq_gender

summary_stats <- Psosol_data %>%
  group_by(Group) %>%
  summarise(
    Mean = mean(Age, na.rm = TRUE),
    Median = median(Age, na.rm = TRUE),
    Min = min(Age, na.rm = TRUE),
    Max = max(Age, na.rm = TRUE),
    Std_Dev = sd(Age, na.rm = TRUE)
  )
summary_stats

ggplot(Psosol_data, aes(x = Group, y = Age, fill = Group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age by Group", x = "Group", y = "Age") +
  theme_minimal()

ggplot(Psosol_data, aes(x = Age, color = Group, fill = Group)) +
  geom_density(alpha = 0.3) +
  labs(title = "Age Distribution Curve by Group", x = "Age", y = "Density") +
  theme_minimal()

ggplot(Psosol_data, aes(sample = Age, color = Group)) +
  stat_qq() + 
  stat_qq_line() +
  facet_wrap(~Group) +
  labs(title = "Q-Q Plot of Age by Group") +
  theme_minimal()

# Split the Age data by Group
placebo_age <- Psosol_data$Age[Psosol_data$Group == "Placebo"]
treatment_age <- Psosol_data$Age[Psosol_data$Group == "Treatment"]

# Shapiro-Wilk Test for normality
shapiro_placebo <- shapiro.test(placebo_age)
shapiro_treatment <- shapiro.test(treatment_age)

# Print Shapiro-Wilk test results
shapiro_placebo
shapiro_treatment

skewness_placebo <- skewness(placebo_age, na.rm = TRUE)
skewness_treatment <- skewness(treatment_age, na.rm = TRUE)

kurtosis_placebo <- kurtosis(placebo_age, na.rm = TRUE)
kurtosis_treatment <- kurtosis(treatment_age, na.rm = TRUE)

typeof(Psosol_data$pd_0)

#Exploring Quality of life variables
#Before treatment started, who was experiencing the worst quality of life? by age and group
mo_0_counts <- Psosol_data %>%
  group_by(Group, mo_0) %>%
  summarise(Count = n()) %>%
  ungroup() 
View(mo_0_counts)


#After treatment, who was experiencing the worst quality of life? by age and group
mo_52_counts <- Psosol_data %>%
  group_by(Group, mo_52) %>%
  summarise(Count = n()) %>%
  ungroup()
View(mo_52_counts)

# Define the value set
value_set <- list(
  mo2 = -0.069,
  mo3 = -0.314,
  sc2 = -0.104,
  sc3 = -0.214,
  ua2 = -0.036,
  ua3 = -0.094,
  pd2 = -0.123,
  pd3 = -0.386,
  ad2 = -0.071,
  ad3 = -0.236,
  at_least_one_2_or_3 = -0.081,
  at_least_one_3 = -0.269
)

# Function to calculate EQ-5D index
calculate_eq5d <- function(mo, sc, ua, pd, ad, value_set) {
  eq5d_index <- 1
  
  # Subtract values based on health states
  eq5d_index <- eq5d_index + ifelse(mo == 2, value_set$mo2, ifelse(mo == 3, value_set$mo3, 0))
  eq5d_index <- eq5d_index + ifelse(sc == 2, value_set$sc2, ifelse(sc == 3, value_set$sc3, 0))
  eq5d_index <- eq5d_index + ifelse(ua == 2, value_set$ua2, ifelse(ua == 3, value_set$ua3, 0))
  eq5d_index <- eq5d_index + ifelse(pd == 2, value_set$pd2, ifelse(pd == 3, value_set$pd3, 0))
  eq5d_index <- eq5d_index + ifelse(ad == 2, value_set$ad2, ifelse(ad == 3, value_set$ad3, 0))
  
  # Subtract additional values based on conditions
  if (any(c(mo, sc, ua, pd, ad) %in% c(2, 3))) {
    eq5d_index <- eq5d_index + value_set$at_least_one_2_or_3
  }
  if (any(c(mo, sc, ua, pd, ad) == 3)) {
    eq5d_index <- eq5d_index + value_set$at_least_one_3
  }
  
  return(eq5d_index)
}

# Calculate EQ-5D index for each observation
Psosol_data$eq5d_index_0 <- mapply(calculate_eq5d, Psosol_data$mo_0, Psosol_data$sc_0, Psosol_data$ua_0, Psosol_data$pd_0, Psosol_data$ad_0, MoreArgs = list(value_set))
Psosol_data$eq5d_index_52 <- mapply(calculate_eq5d, Psosol_data$mo_52, Psosol_data$sc_52, Psosol_data$ua_52, Psosol_data$pd_52, Psosol_data$ad_52, MoreArgs = list(value_set))
View(Psosol_data)

# Estimate the QALY, which is the average of eq5d_index_0 and eq5d_index_52 for each individual ID
Psosol_data <- Psosol_data %>%
  mutate(QALY = (eq5d_index_0 + eq5d_index_52) / 2)

# Calculate mean EQ-5D index at week 52 by treatment group
mean_eq_52 <- Psosol_data %>%
  group_by(Group, Gender) %>%
  summarise(mean_EQ5D_52 = mean(eq5d_index_52, na.rm = TRUE))

# View the resulting summary
print(mean_eq_52)

ggplot(Psosol_data, aes(x = eq5d_index_52, fill = Group)) +
  geom_density(alpha = 0.3) +
  labs(title = "Distribution of EQ-5D Index at Week 52 by Treatment Group", x = "EQ-5D Index at Week 52", y = "Density") +
  theme_minimal()

# Estimate the average PASI score
Psosol_data <- Psosol_data %>%
  mutate(PASI_score = 0.5 * ((PASI_week_bas + PASI_week_mid) / 2) + 
           0.5 * ((PASI_week_mid + PASI_week_end) / 2))

Psosol_data <- Psosol_data %>%
  mutate(
    Work_impair_0 = as.numeric(Work_impair_0),
    Work_impair_52 = as.numeric(Work_impair_52)
  )

Psosol_data <- Psosol_data %>%
  mutate(Avg_work_impairment = rowMeans(cbind(Work_impair_0, Work_impair_52), na.rm = TRUE))


# Function to assign monthly income based on age
get_monthly_income <- function(age) {
  case_when(
    age >= 25 & age <= 44 ~ 28600,
    age >= 45 & age <= 64 ~ 30950,
    age >= 65 & age <= 84 ~ 0,
    TRUE ~ NA_real_  # For ages outside the range, assign NA
  )
}

# Apply to your data 
Psosol_data <- Psosol_data %>%
  mutate(Ann_income_2022 = get_monthly_income(Age)* infla_factor * 12)

# Calculate Annual benefits 
get_benefit_rate <- function(age) {
  case_when(
    age >= 25 & age <= 44 ~ 0.32,
    age >= 45 & age <= 64 ~ 0.32,
    age >= 65 & age <= 84 ~ 0,
    TRUE ~ NA_real_  # For ages outside the range, assign NA
  )
}

Psosol_data <- Psosol_data %>%
  mutate(Ann_benefits = get_benefit_rate(Age) * Ann_income_2022)

# Estimate Psosol bottles consumed annually
Psosol_data <- Psosol_data %>%
  mutate(
    Ann_Psotop_bottle = ceiling((Psotop_qty * 52.1429) / 100))

Psosol_data <- Psosol_data %>%
  mutate(Systx_strt_wk = as.numeric(Systx_strt_wk))

Psosol_data <- Psosol_data %>%
  mutate(Systx_duration = if_else(!is.na(Systx_strt_wk),
                                52.1429 - Systx_strt_wk + 1,
                                0))  # If no start week, assume no systemic therapy

# Estimate productivity loss
Psosol_data <- Psosol_data %>%
  mutate(Productivity_loss = (Ann_income_2022 + Ann_benefits) * Avg_work_impairment)

# Estimate healthcare cost
Psosol_data <- Psosol_data %>%
  mutate(Ann_specialist_cost = (Spe_visits * specialist_cost))

Psosol_data <- Psosol_data %>%
  mutate(Ann_gp_cost = (GP_visits * gp_cost))

Psosol_data <- Psosol_data %>%
  mutate(Ann_phototx_cost = (Phototx_qty * phototx_cost))

Psosol_data <- Psosol_data %>%
  mutate(Ann_systemictx_cost = (Systx_duration * systemicthx_cost))

Psosol_data <- Psosol_data %>%
  mutate(Ann_psosol_cost = (Ann_Psotop_bottle * psosol_cost))

Psosol_data <- Psosol_data %>%
  mutate(healthcare_cost = (Ann_psosol_cost + Ann_systemictx_cost + Ann_phototx_cost + Ann_gp_cost + Ann_specialist_cost))

Psosol_data <- Psosol_data %>%
  mutate(total_cost = (healthcare_cost + Productivity_loss))

# Initialize dataframe and  store average cost and QALY 
CUA_summary <- Psosol_data %>%
  group_by(Group) %>%
  summarise(
    mean_total_cost = mean(total_cost, na.rm = TRUE),
    mean_healthcare_cost = mean(healthcare_cost, na.rm = TRUE),
    mean_productivity_loss = mean(Productivity_loss, na.rm = TRUE),
    mean_QALY = mean(QALY, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate ICER
ref <- CUA_summary %>% filter(Group == "Treatment")
comp <- CUA_summary %>% filter(Group == "Placebo")
delta_cost <- comp$mean_total_cost - ref$mean_total_cost
delta_QALY <- comp$mean_QALY - ref$mean_QALY
ICER <- delta_cost / delta_QALY
ICER