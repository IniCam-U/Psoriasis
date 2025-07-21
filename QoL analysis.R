library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(moments)

QoL_Pso <- read_excel("QoL_Psoriasis.xlsx")
View(QoL_Pso)
PsoProve_RCT <- read_excel("PsoProve2_RCT dataset.xlsx")
View(PsoProve_RCT)

# Join work_impair_0 from PsoProve_RCT to QoL_Pso
QoL_Pso <- QoL_Pso %>%
  left_join(PsoProve_RCT %>% select(ID, Work_impair_0, Work_impair_52, Spe_visits, GP_visits, Phototx_qty, Systx_strt_wk, Psotop_qty), by = "ID")


#Labeling the variables
QoL_Pso$Group <- factor(QoL_Pso$Group, levels = c(0, 1), labels = c("Placebo", "Treatment"))
QoL_Pso$Gender <- factor(QoL_Pso$Gender, levels = c(1, 2), labels = c("Male", "Female"))
#QoL_Pso$mo_0 <- factor(QoL_Pso$mo_0, levels = c(1, 2, 3), labels = c("None", "Some", "Extreme"))
#QoL_Pso$mo_52 <- factor(QoL_Pso$mo_52, levels = c(1, 2, 3), labels = c("None", "Some", "Extreme"))
#QoL_Pso$sc_0 <- factor(QoL_Pso$sc_0, levels = c(1, 2, 3), labels = c("None", "Some", "Extreme"))
#QoL_Pso$sc_52 <- factor(QoL_Pso$sc_52, levels = c(1, 2, 3), labels = c("None", "Some", "Extreme"))
#QoL_Pso$ua_0 <- factor(QoL_Pso$ua_0, levels = c(1, 2, 3), labels = c("None", "Some", "Extreme"))
#QoL_Pso$ua_52 <- factor(QoL_Pso$ua_52, levels = c(1, 2, 3), labels = c("None", "Some", "Extreme"))
#QoL_Pso$pd_0 <- factor(QoL_Pso$pd_0, levels = c(1, 2, 3), labels = c("None", "Some", "Extreme"))
#QoL_Pso$pd_52 <- factor(QoL_Pso$pd_52, levels = c(1, 2, 3), labels = c("None", "Some", "Extreme"))
#QoL_Pso$ad_0 <- factor(QoL_Pso$ad_0, levels = c(1, 2, 3), labels = c("None", "Some", "Extreme"))
#QoL_Pso$ad_52 <- factor(QoL_Pso$ad_52, levels = c(1, 2, 3), labels = c("None", "Some", "Extreme"))

#Exploring the demographics of the sample
gender_count <- table(QoL_Pso$Gender, QoL_Pso$Group)
gender_count
gender_prop <- prop.table(gender_count, margin = 1)
gender_prop
chi_sq_gender <- chisq.test(gender_count)
chi_sq_gender

summary_stats <- QoL_Pso %>%
  group_by(Group) %>%
  summarise(
    Mean = mean(Age, na.rm = TRUE),
    Median = median(Age, na.rm = TRUE),
    Min = min(Age, na.rm = TRUE),
    Max = max(Age, na.rm = TRUE),
    Std_Dev = sd(Age, na.rm = TRUE)
  )
summary_stats

ggplot(QoL_Pso, aes(x = Group, y = Age, fill = Group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age by Group", x = "Group", y = "Age") +
  theme_minimal()

ggplot(QoL_Pso, aes(x = Age, color = Group, fill = Group)) +
  geom_density(alpha = 0.3) +
  labs(title = "Age Distribution Curve by Group", x = "Age", y = "Density") +
  theme_minimal()

ggplot(QoL_Pso, aes(sample = Age, color = Group)) +
  stat_qq() + 
  stat_qq_line() +
  facet_wrap(~Group) +
  labs(title = "Q-Q Plot of Age by Group") +
  theme_minimal()

# Split the Age data by Group
placebo_age <- QoL_Pso$Age[QoL_Pso$Group == "Placebo"]
treatment_age <- QoL_Pso$Age[QoL_Pso$Group == "Treatment"]

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

typeof(QoL_Pso$pd_0)

#Exploring Quality of life variables
#Before treatment started, who was experiencing the worst quality of life? by age and group
mo_0_counts <- QoL_Pso %>%
  group_by(Group, mo_0) %>%
  summarise(Count = n()) %>%
  ungroup() 
View(mo_0_counts)


#After treatment, who was experiencing the worst quality of life? by age and group
mo_52_counts <- QoL_Pso %>%
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
QoL_Pso$eq5d_index_0 <- mapply(calculate_eq5d, QoL_Pso$mo_0, QoL_Pso$sc_0, QoL_Pso$ua_0, QoL_Pso$pd_0, QoL_Pso$ad_0, MoreArgs = list(value_set))
QoL_Pso$eq5d_index_52 <- mapply(calculate_eq5d, QoL_Pso$mo_52, QoL_Pso$sc_52, QoL_Pso$ua_52, QoL_Pso$pd_52, QoL_Pso$ad_52, MoreArgs = list(value_set))
View(QoL_Pso)

# Calculate the average of eq5d_index_0 and eq5d_index_52 for each individual ID
QoL_Pso <- QoL_Pso %>%
  mutate(QALY = (eq5d_index_0 + eq5d_index_52) / 2)

# Calculate mean EQ-5D index at week 52 by treatment group
mean_eq_52 <- QoL_Pso %>%
  group_by(Group, Gender) %>%
  summarise(mean_EQ5D_52 = mean(eq5d_index_52, na.rm = TRUE))

# View the resulting summary
print(mean_eq_52)

ggplot(QoL_Pso, aes(x = eq5d_index_52, fill = Group)) +
  geom_density(alpha = 0.3) +
  labs(title = "Distribution of EQ-5D Index at Week 52 by Treatment Group", x = "EQ-5D Index at Week 52", y = "Density") +
  theme_minimal()


