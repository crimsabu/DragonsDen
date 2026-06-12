#Downloading Haven and importing .sav data into R#
library(haven)
my_data <- read_sav("HIV_HTN Survey Dataset Timepoint Cleaned.sav")

#Taking a look at data to see if all imported well#
head(my_data)
library(dplyr)
glimpse(my_data)

#Looking at missingness because I saw a lot of red NAs#
colSums(is.na(my_data))

#Looking at timepoints#
names(my_data)
table(my_data$Timepoint, useNA = "always")

#Coding seen not answered into NA#
my_data[my_data == -999] <- NA

#Just keeping timepoints 1 and 2, coding into data dragon's den (DD)#
data_DD <- my_data %>%
  filter(Timepoint %in% c(1, 2))

# Double checking the result#
table(data_DD$Timepoint, useNA = "always")
nrow(data_DD)

# Seeing timepoint differences across mentally healthy days#
tapply(data_DD$HRQOL_3_4, data_DD$Timepoint, mean, na.rm = TRUE)

#seeing which IDs are repeated twice, iee. are present across both #
table(table(data_DD$PID))

# Let's check the raw numbers again in the original file
table(my_data$Timepoint)

# This keeps only 1s and 2s and handles NAs properly
data_DD <- my_data[which(my_data$Timepoint %in% c(1, 2)), ]

# Now check the table again
table(data_DD$Timepoint)

attendance <- table(data_DD$PID, data_DD$Timepoint)
head(attendance)

#calculating age#

library(dplyr)

data_DD <- data_DD %>%
  mutate(
    # Simple subtraction for birth year
    Age_at_T1 = 2019 - DOB_1,
    
    # Create the 3 Age Brackets based on this new age
    Age_Bracket_T1 = case_when(
      Age_at_T1 >= 18 & Age_at_T1 <= 30 ~ "Young (18-30)",
      Age_at_T1 >= 31 & Age_at_T1 <= 50 ~ "Middle (31-50)",
      Age_at_T1 > 50                   ~ "Older (51+)",
      TRUE                             ~ NA_character_
    )
  )

# Quick check to see the distribution
table(data_DD$Age_Bracket_T1)

#creating an ETI Composite Score 

eti_cols <- c("ETI_GT_1", "ETI_GT_2", "ETI_GT_3", "ETI_GT_4", "ETI_GT_5", 
              "ETI_GT_6", "ETI_GT_7", "ETI_GT_8", "ETI_GT_9", "ETI_GT_10")

# picking a random ETI column to check coding.
table(data_DD$ETI_GT_1, useNA = "always")

# mutating all coding across eti_cols
data_DD <- data_DD %>%
  mutate(across(all_of(eti_cols), ~ case_when(
    . == 1 ~ 1,      # Yes = 1
    . == 2 ~ 0,      # No = 0
    TRUE   ~ NA_real_ # -999 and others stay NA
  ))) %>%
  rowwise() %>%
  mutate(
    # Count how many questions they actually answered
    eti_answered = sum(!is.na(c_across(all_of(eti_cols)))),
    
    # Calculate the sum (the composite score)
    # Coding to only calculate if they answered at least 1 question to avoid 0s for missing data
    ETI_Composite = ifelse(eti_answered > 0, 
                           sum(c_across(all_of(eti_cols)), na.rm = TRUE), 
                           NA)
  ) %>%
  ungroup()

#verification and visualization of the ETI
summary(data_DD$ETI_Composite)

hist(data_DD$ETI_Composite, 
     main="Distribution of ETI General Trauma Scores", 
     xlab="Number of Trauma Events reported")

data_DD_filtered <- data_DD_filtered %>%
  mutate(Ethnicity_Composite = case_when(
    ETHN_1 == 1 ~ "Black/African American",
    ETHN_8 == 1 | ETHN_14 == 1 ~ "Hispanic/Latino",
    ETHN_4 == 1 ~ "White/Caucasian",
    # Grouping Asian, Pacific Islander, Native, and Other
    ETHN_2==1|ETHN_3==1|ETHN_5==1|ETHN_6==1|ETHN_7==1|ETHN_9==1|
      ETHN_10==1|ETHN_11==1|ETHN_12==1|ETHN_13==1|ETHN_15==1|
      ETHN_19==1|ETHN_18==1 ~ "Other/Multiracial/Asian",
    TRUE ~ "Unknown/Not Reported"
  ))

#checking the recoding of the ethnicity requirement

table(data_DD$Ethnicity_Composite)

# Keeping only Black and White participants bc of data structure
data_DD_filtered <- data_DD %>%
  filter(Ethnicity_Composite %in% c("Black/African American", "White/Caucasian")) %>%
  # Drop the unused levels so they don't show up in the regression summary
  mutate(Ethnicity_Composite = droplevels(as.factor(Ethnicity_Composite)))


#Checking other variables
table(data_DD$MARSTA, useNA = "always")
table(data_DD$INC, useNA = "always")
table(data_DD$ED, useNA = "always")
table(data_DD$EMPL, useNA = "always")
table(data_DD$Child, useNA = "always")

data_DD_filtered <- data_DD_filtered %>%
  mutate(
    # Cleaning Employment
    Employment_Clean = case_when(
      EMPL %in% c(1, 2, 3) ~ "Employed",
      EMPL == 6        ~ "Unemployed",
      EMPL %in% c(4, 5) ~ "Retired/Disabled",
      TRUE               ~ NA_character_
    ),
    # Cleaning Marital Status
    Marital_Clean = case_when(
      MARSTA %in% c(2, 5) ~  "Married/Partnered",
      MARSTA %in% c(1,3,4) ~ "Not married",
      TRUE                 ~ NA_character_
    )
  )

data_DD_filtered <- data_DD_filtered %>%
  mutate(
    # Recode 5 to 0, keep other numbers as is
    child_count = ifelse(Child == 5, 0, Child),
    # Ensure it's numeric for the model
    child_count = as.numeric(child_count)
  )

# Quick check to make sure the 5s are gone and the 0s are there
table(data_DD_filtered$child_count)


data_DD_filtered <- data_DD_filtered %>%
  mutate(
    # Cleaning Education
    Education_Clean = case_when(
      ED %in% c(1, 2, 3) ~ "High School or Below",
      ED %in% c(4)        ~ "Some College or Bachelors Degree",
      ED == 6 ~ "Post Graduate Education",
      TRUE               ~ NA_character_
    ))

table(data_DD_filtered$Education_Clean)

#############################################trying to pair the sample
library(tidyr)
library(dplyr)

data_wide <- data_DD_filtered%>%
  # 1. Filter for the two groups you want
  filter(Ethnicity_Composite %in% c("Black/African American", "White/Caucasian")) %>%
  # 2. Select the variables we've been cleaning
  select(PID, Timepoint, Age_Bracket_T1, Ethnicity_Composite, 
         ETI_Composite, HRQOL_3_4, Education_Clean, child_count, 
         Employment_Clean, Marital_Clean, INC) %>%
  # 3. Pivot to pair T1 and T2
  pivot_wider(
    id_cols = c(PID, Age_Bracket_T1, Ethnicity_Composite),
    names_from = Timepoint, 
    values_from = c(ETI_Composite, HRQOL_3_4,
                    Education_Clean, child_count, 
                    Employment_Clean, Marital_Clean, INC)
  )
#####################

# This is your final N for the Black/White longitudinal analysis
nrow(data_wide)

# This breaks it down so i can see if the groups are balanced
table(data_wide$Ethnicity_Composite)

table(data_wide$Age_Bracket_T1, data_wide$Ethnicity_Composite)
#############

data_wide <- data_wide %>%
  mutate(Age_Split = case_when(
    Age_Bracket_T1 == "Older (51+)" ~ "Older (51+)",
    TRUE ~ "Under 50" 
  ),
  Age_Split = factor(Age_Split, levels = c("Under 50", "Older (51+)")))

#chcking
table(data_wide$Age_Split, data_wide$Ethnicity_Composite)

#########################3
colnames(data_wide)
##########################333
library(MASS)

# Running the regression on th paired sample
model_final <- glm.nb(HRQOL_3_4_2 ~ 
                        ETI_Composite_1 * Age_Split +      # The Interaction
                        HRQOL_3_4_1 +                        # Control for baseline health
                        child_count_1 +                    # Control for kids (0-4+)
                        Ethnicity_Composite +              # Black vs. White
                        Education_Clean_1 +               #Education 
                        INC_1,                          # Control for money
                      data = data_wide)

# The moment of truth
summary(model_final)

############
colSums(is.na(data_wide))

################## retrying an imputed model.
library(haven)
library(dplyr)

#Strip the "haven_labelled" class from all variables
data_clean_for_mice <- data_wide %>%
  mutate(across(everything(), zap_labels)) %>%
  # Make sure factors are actually factors (not labelled numbers)
  mutate(
    Age_Split = as.factor(Age_Split),
    Ethnicity_Composite = as.factor(Ethnicity_Composite)
  ) %>%
  #Ensures everything else is a standard numeric double
  mutate(across(where(is.numeric), as.numeric))

# Now trying the imputation again
library(mice)
imputed_data <- mice(data_clean_for_mice, m=5, method='pmm', seed=500)

# Running the model on the "full" filled-in data
fit_imputed <- with(imputed_data, glm.nb(HRQOL_3_4_2 ~ 
                                           ETI_Composite_1 * Age_Split + 
                                           HRQOL_3_4_1 + Ethnicity_Composite))

# Pooling the results together
summary(pool(fit_imputed))

# Create a variable for "Did they finish?"
data_wide <- data_wide %>%
  mutate(finished = ifelse(is.na(HRQOL_3_4_2), "Dropped", "Finished"))

# Seeing if older people were more likely to finish than younger people
table(data_wide$Age_Split, data_wide$finished)

# Seeing if high-trauma people were more likely to drop out
t.test(ETI_Composite_1 ~ finished, data = data_wide)

table(data_wide$Age_Split, data_wide$finished)
###################################visualizing the initial model
library(sjPlot)
library(ggplot2)

# This creates the "Predicted Slopes" for each age group
plot_model(model_lean, type = "int", 
           mdl.term = "ETI_Composite_1:Age_Split",
           title = "The Age Buffer: Trauma Impact on Mental Health",
           axis.title = c("Trauma Severity (T1)", "Mentally Healthy Days (T2)"),
           legend.title = "Age Group") +
  theme_minimal() +
  scale_color_manual(values = c("Under 50" = "#E41A1C", "Older (51+)" = "#377EB8"))
##############################new visuals
install.packages("scico")
############################
library(sjPlot)
library(ggplot2)
library(scico) # This library provides beautiful, scientific palettes

# Ensure Age_Split is still a clean factor for labeling
data_wide$Age_Split <- factor(data_wide$Age_Split, levels = c("Under 50", "Older (51+)"))

#Generate the base prediction data
pred <- get_model_data(model_final, type = "int")

#Building the customized ggplot
ggplot(pred, aes(x = x, y = predicted, group = group, color = group, fill = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  scale_color_scico_d(palette = "batlow", begin = 0.1, end = 0.8) +
  scale_fill_scico_d(palette = "batlow", begin = 0.1, end = 0.8) +
  labs(
    title = "The Buffering Effect of Aging",
    subtitle = "Trauma Impact (T1) on Predicted Mentally Healthy Days (T2)",
    x = "Trauma Severity (ETI)",
    y = "Mentally Healthy Days (Predicted)",
    color = "Age Cohort",
    fill = "Age Cohort"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14, color = "gray40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray80"),
    text = element_text(family = "Arial") # Use a clean serif or sans-serif font
  )
############################################again

library(ggplot2)
library(sjPlot)

# New vis
pred <- get_model_data(model_final, type = "int")

ggplot(pred, aes(x = x, y = predicted, group = group, color = group)) +
  # Add a subtle, clean background area for the confidence intervals
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.12, color = NA) +
  # Thicker, distinct lines
  geom_line(size = 1.8) +
  # "Sunset and Midnight" Color Palette
  scale_color_manual(values = c("Under 50" = "#D95F02", "Older (51+)" = "#1F78B4")) +
  scale_fill_manual(values = c("Under 50" = "#D95F02", "Older (51+)" = "#1F78B4")) +
  labs(
    title = "The Protective Effect of Age on Trauma",
    subtitle = "Older adults (51+) maintain mental health stability despite higher trauma levels.",
    x = "Trauma Severity Index",
    y = "Predicted Healthy Days",
    color = NULL, # Removes the redundant legend title
    fill = NULL
  ) +
  theme_classic(base_size = 14) + 
  theme(
    legend.position = "top",
    legend.justification = "left",
    plot.title = element_text(face = "bold", size = 20, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 13, color = "gray30", margin = margin(b = 20)),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", size = 0.5), # Only horizontal grid lines
    text = element_text(color = "gray10")
  )
