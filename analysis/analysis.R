###################################################################################
####   PART 1: Discrete Choice Design   ###########################################
###################################################################################

# Load necessary libraries
library(support.CEs)
library(idefix)
library(AlgDesign)
library(dplyr)
library(tidyr)
library(ggplot2)

# Case:
# A policy intervention is planned. The Federal Government is considering the introduction 
# of a Federal Mobility Data Bill ("Mobilitätsdatengesetz"). This would force mobility providers to share
# data among each other, which could improve the efficiency and ease of multi-modal transport planning.
# Nevertheless the sharing of (possibly) personal data between mobility providers also raises questions
# around consumer preferences around personal data protection.
# We want to investigate citizen preferences around data sharing for multi-modal transport planning.
# We therefore use a SP survey with attributes for the kinds of data that could be shared and combine them with 
# the attributes of total travel time and route planning experience to test their preferences in relation.
# Additionally we investigate how user preferences change with different trip purpose scenarios 
# (Commute/Leisure vs. business Trip). Each scenario should have 16 choice tasks.

# Basic information about our DCE (without the intervention):
# Alternatives:     Integrated Route Planner & Multiple Route Planners
# Attributes:       Travel time (Door to Door), Route Planning Experience (Ease of use, Overall UX), Data shared between mobility providers
# Attribute levels: Time (20 min, 35 min, 50 min) - Experience (Very Easy, Manageable, Cumbersome) - Data shared (None, Anonymized occupancy data, Realtime location data)
# Constraints:      (1) Alternative Multiple Route Planners always has data shared => None
#                   (2) Alternative Multiple Route Planners is never faster than Integrated
#                   (3) Alternative Multiple Route Planners is never easier than Integrated

# For the first scenario, the purpose of the trip is commuting to work/school or leisure (Commute/Leisure Trip)
# For the second scenario, the purpose of the trip is conducting business in official capacity (business Trip).


### Reproducibility
set.seed(999)

## Choice-tasks for Scenario 1 (Status-Quo)

# Define attributes for each alternative for our full-factorial design for Scenario Status-Quo
attributes <- list(
  TravelTime_Integrated = c(20, 35, 50),      # Travel time in minutes  
  TravelTime_Multiple = c(20, 35, 50),        # Travel time in minutes                 
  Experience_Integrated = c("Very Easy","Manageable", "Cumbersome"),
  Experience_Multiple = c("Very Easy","Manageable", "Cumbersome"),
  DataShared_Integrated = c("None", "Anonymized occupancy data", "Realtime location data"),
  DataShared_Multiple = c("None")
)

# Create a full factorial design
design <- expand.grid(attributes)

# Left 243 initial choice sets after applying full factorial design

# Final choice sets after applying all constraints and exclusions (See Section 2.2 of the research paper):

choice_set<- c("S1_2035_EM_AN", "S1_2035_EM_RN", "S1_2035_EC_AN", "S1_2035_EC_RN", "S1_2035_MM_AN", "S1_2035_MM_RN", "S1_2050_EM_AN", "S1_2050_EM_RN", "S1_2050_EC_AN", "S1_2050_EC_RN", "S1_2050_MM_AN", "S1_2050_MM_RN", "S1_3535_EM_AN", "S1_3535_EC_AN", "S1_3535_EM_RN", "S1_3535_EC_RN",   #Scenario 1
               "S2_2035_EM_AN", "S2_2035_EM_RN", "S2_2035_EC_AN", "S2_2035_EC_RN", "S2_2035_MM_AN", "S2_2035_MM_RN", "S2_2050_EM_AN", "S2_2050_EM_RN", "S2_2050_EC_AN", "S2_2050_EC_RN", "S2_2050_MM_AN", "S2_2050_MM_RN", "S2_3535_EM_AN", "S2_3535_EC_AN", "S2_3535_EM_RN", "S2_3535_EC_RN")   #Scenario 2)

###################################################################################
####   PART 2: Load Data from Qualtrics and Preprocessing  ########################
###################################################################################

# Load necessary libraries
library(apollo)
library(ggplot2)
library(dplyr)

# Load data
my_data <- read.csv("latest_data.csv")

which(names(my_data) == "")

# Select only rows and columns we want
my_data <- my_data[-c(1:2), -c(1,2,3,8,10,11,12,13,16)] 
my_data <- my_data %>% mutate(id = row_number()) # Keep a user ID, because users answer more questions

# Select only the rows where consent was given
my_data <- my_data[my_data$Consent == 1, ]



##### Descriptive/ Summary statistics for preliminary questions

# Example: Plot age distribution, Answer numbers sourced from "Umkodierungswerte" in Qualtrics
age_plot <- my_data %>%
  mutate(Age_Label = recode(Age,
                            `1` = "18-25",
                            `2` = "25-35",
                            `3` = "35-45",
                            `11` = "45-55",
                            `13` = "55-65",
                            `4` = "65-75",
                            `14` = "75+",
                            `7` = "Prefer Not To Anwser"
  ))


# Create a bar plot for age distribution with custom labels
ggplot(age_plot, aes(x = Age_Label)) +
  geom_bar(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Age Distribution",
       x = "Age Group",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



###### DATA ENRICHING

### Example for my abbreviation schema for choice sets: S1_2035_EM_AN
# The infer_values function is custom built to resolve my abbreviation schema from code (from choice set/question) to label (for example A -> Data_shared_integrated ="Anonymized occupancy data")

infer_values <- function(target) {
  scenario <- substr(target, 1, 2)  # S1 or S2
  print(scenario)
  print("lol")
  travel_time_integrated <- as.numeric(substr(target, 4, 5))# First two digits
  print(travel_time_integrated)
  travel_time_multiple <- as.numeric(substr(target, 6, 7))  # Last two digits
  print(travel_time_multiple)
  experience_code <- substr(target, 9, 10)  # E, M, or C
  print(experience_code)
  data_code <- substr(target, 12, 13)  # N, A, or R
  print(data_code)
  
  # Infer route planning experience values from my abbreviation schema
  experience_integrated <- ifelse(substr(experience_code, 1, 1) == "E", "Very Easy",
                                  ifelse(substr(experience_code, 1, 1) == "M", "Manageable", "Cumbersome"))
  experience_multiple <- ifelse(substr(experience_code, 2, 2) == "E", "Very Easy",
                                ifelse(substr(experience_code, 2, 2) == "M", "Manageable", "Cumbersome"))
  
  
  # Infer data sharing values from my abbreviation schema
  data_shared_integrated <- ifelse(substr(data_code, 1, 1) == "A", "Anonymized occupancy data",
                                   ifelse(substr(data_code, 1, 1) == "N", "None", "Realtime location data"))
  data_shared_multiple <- ifelse(substr(data_code, 2, 2) == "A", "Anonymized occupancy data",
                                 ifelse(substr(data_code, 2, 2) == "N", "None", "Realtime location data"))
  
  
  # Return list of inferred values
  list(
    TravelTime_Integrated = travel_time_integrated,
    TravelTime_Multiple = travel_time_multiple,
    Experience_Integrated = experience_integrated,
    Experience_Multiple = experience_multiple,
    DataShared_Integrated = data_shared_integrated,
    DataShared_Multiple = data_shared_multiple,
    Scenario = as.numeric(substr(target, 2, 2))
  )
}

#### Create the individual extended tables (for each choice set)
# Loop through each question in the choice_set and create individual tables
choice_task_counter <- 1

for (S1_Q1_target in choice_set) {
  # Apply inferred values from schema
  inferred_values <- infer_values(S1_Q1_target)
  
  # new table
  assign(S1_Q1_target, my_data %>%
           select(-all_of(setdiff(choice_set, S1_Q1_target))) %>%
           rename(Choice = !!sym(S1_Q1_target)) %>%
           mutate(
             TravelTime_Integrated = inferred_values$TravelTime_Integrated,
             TravelTime_Multiple = inferred_values$TravelTime_Multiple,
             Experience_Integrated = inferred_values$Experience_Integrated,
             Experience_Multiple = inferred_values$Experience_Multiple,
             DataShared_Integrated = inferred_values$DataShared_Integrated,
             DataShared_Multiple = inferred_values$DataShared_Multiple,
             Scenario = inferred_values$Scenario,
             Choice_Task = row_number()
           )
  )
  
  choice_task_counter <- choice_task_counter + 1
}


#### Combine the extended tables for each choice set to one combined dataset
# prepare combining
data_frames_list <- mget(choice_set)

# Combine all tables into one
combined_data <- bind_rows(data_frames_list)
combined_data <- combined_data %>% arrange(id)

# Transform categorical variables into factors so apollo can process them
combined_data <- combined_data %>%
  mutate(
    Experience_Integrated = factor(Experience_Integrated, 
                                   levels = c("Very Easy", "Manageable", "Cumbersome")),
    Experience_Multiple = factor(Experience_Multiple, 
                                 levels = c("Very Easy", "Manageable", "Cumbersome")),
    DataShared_Integrated = factor(DataShared_Integrated, 
                                   levels = c("None", "Anonymized occupancy data", "Realtime location data")),
    DataShared_Multiple = factor(DataShared_Multiple, 
                                 levels = c("None", "Anonymized occupancy data", "Realtime location data"))
  )

  combined_data <- combined_data %>%
  mutate(
    Experience_Integrated = as.numeric(Experience_Integrated),
    Experience_Multiple = as.numeric(Experience_Multiple),
    DataShared_Integrated = as.numeric(DataShared_Integrated),
    DataShared_Multiple = as.numeric(DataShared_Multiple)
  )



###################################################################################
####   PART 3: Estimation of your MNL   ###########################################
###################################################################################

# Load necessary library
library(apollo)

### Initialize code
apollo_initialise()

database <- combined_data # Save your dataframe as database

## Remove NAs (when no alternative was chosen for a given choice task, i did not prevent this in qualtrics)
database$Choice <- ifelse(database$Choice == "", NA, database$Choice)
database <- database %>% filter(!is.na(Choice))




### Set core controls
apollo_control = list(
  modelName       = "Model_Basic",
  modelDescr      = "Basic MNL model",
  indivID         = "id",   # Change this variable to your id (stayed at id)
  nCores          = 1,
  outputDirectory = "output"
)


# ################################################################# #
#### Define model parameters                                    ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(
  asc_integrated_commuteleisure   = 0,  # ASC for Integrated Route Planner in the default scenario
  asc_integrated_business = 0, # ASC for Integrated Route Planner in Business scenario
  asc_multiple_commuteleisure   = 0,    # ASC for Multiple Route Planners in the default scenario
  asc_multiple_business = 0,  # ASC for Multiple Route Planners in Business scenario
  b_tt      = 0,         # Coefficient for Travel Time
  b_experience  = 0,     # Coefficient for Experience (e.g., Very Easy -> Cumbersome)
  b_data_none  = 0,      # Coefficient for Data Shared = None
  b_data_anon  = 0,      # Coefficient for Data Shared = Anonymized occupancy data
  b_data_realtime = 0    # Coefficient for Data Shared = Realtime location data
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta
apollo_fixed = c("b_data_none")  

apollo_inputs = apollo_validateInputs()
# ################################################################# #
#### Define the model                                            ####
# ################################################################# #

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality = "estimate"){
  
  ### Function initialization: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Include shifts for purpose of the trip (Commute/Leisure vs Business Trip Scenario)
  asc_integrated = asc_integrated_commuteleisure + asc_integrated_business * (Scenario == 2)
  asc_multiple   = asc_multiple_commuteleisure + asc_multiple_business * (Scenario == 2)
  
  ### Define the utility functions for each alternative
  V = list()
  V[["integrated"]]  = asc_integrated + b_tt * TravelTime_Integrated + b_experience * Experience_Integrated + b_data_anon * (DataShared_Integrated == 2) + b_data_realtime * (DataShared_Integrated == 3)
  V[["multiple"]] = asc_multiple + b_tt * TravelTime_Multiple + b_experience * Experience_Multiple + b_data_none * (DataShared_Multiple == 1)
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(integrated = 1, multiple = 2),
    choiceVar     = Choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observations for the same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of the function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### Model estimation                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### Model outputs                                               ####
# ################################################################# #

apollo_modelOutput(model, modelOutput_settings = list(printClassical = FALSE,
                                                      printT1 = TRUE,
                                                      printPVal = TRUE))

# TODO: Estimate more MNL models with demographic variables

###################################################################################
####   PART 4: Post-estimation ####################################################
###################################################################################

# ----------------------------------------------------------------- #
#---- Compute Attribute Importance                               ----
# ----------------------------------------------------------------- #

# Extract estimates from the model
model_estimates <- model$estimate

# Compute attribute importance for Experience
# Experience ranges from 1 (Very Easy) to 3 (Cumbersome), so the importance is calculated as:
experience_levels = c(1, 2, 3)
experience_importance = max(experience_levels * model_estimates[["b_experience"]]) - min(experience_levels * model_estimates[["b_experience"]])

# Compute attribute importance for Data Sharing
# DataSharing ranges from 1 (None) to 3 (Realtime location data)
data_shared_levels = c(1, 2, 3)
data_shared_importance = max(data_shared_levels * c(0, model_estimates[["b_data_anon"]], model_estimates[["b_data_realtime"]])) - 
  min(data_shared_levels * c(0, model_estimates[["b_data_anon"]], model_estimates[["b_data_realtime"]]))

# Compute attribute importance for Travel Time
# Travel time levels are 20, 35, and 50 minutes
tt_levels = c(20, 35, 50)
tt_importance = max(tt_levels * model_estimates[["b_tt"]]) - min(tt_levels * model_estimates[["b_tt"]])

# Define the attribute importance values
attribute_importance <- data.frame(
  Attribute = c("Experience", "Data Sharing", "Travel Time"),
  Importance = c(experience_importance, data_shared_importance, tt_importance)
)

# Calculate importance percentages
attribute_importance <- attribute_importance %>% mutate(Importance_percent = (Importance / sum(Importance)) * 100)

# ----------------------------------------------------------------- #
#---- Visualization of Attribute Importance                      ----
# ----------------------------------------------------------------- #

# Create the barplot using ggplot2
ggplot(attribute_importance, aes(x = Attribute, y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Attribute Importance",
       x = "Attributes",
       y = "Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create the pie chart using ggplot2
ggplot(attribute_importance, aes(x = "", y = Importance_percent, fill = Attribute)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_minimal() +
  labs(title = "Attribute Importance in %") +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        panel.grid = element_blank()) +
  geom_text(aes(label = paste0(round(Importance_percent, 1), "%")), 
            position = position_stack(vjust = 0.5),
            hjust = 0) +
  theme(legend.position = "right")

# ----------------------------------------------------------------- #
# ----     Computing Marginal Rate of Substitution              ----
# ----------------------------------------------------------------- #

# Compute the VTT per hour for integrated and multiple route planners
# Since there is no cost variable, we'll assume b_tt represents the impact of time directly.

# Given that b_tt might reflect disutility per minute, we convert this to an hourly basis
VTT_integrated = 60 * abs(model$estimate[["b_tt"]]) / abs(model$estimate[["b_experience"]])
VTT_multiple = 60 * abs(model$estimate[["b_tt"]]) / abs(model$estimate[["b_experience"]])

# The result reflects how much utility is lost per hour of travel time relative to the impact of experience.

# Display the VTT values
VTT_integrated
VTT_multiple

# Compute the MRS between travel time and data sharing
MRS_anon = model$estimate[["b_tt"]] / model$estimate[["b_data_anon"]]
MRS_realtime = model$estimate[["b_tt"]] / model$estimate[["b_data_realtime"]]

# Display the MRS values
cat("MRS between Travel Time and Anonymized Data Sharing:", MRS_anon, "\n")
cat("MRS between Travel Time and Real-Time Data Sharing:", MRS_realtime, "\n")

# Calculate the Value of Travel Time (VTT) equivalent in terms of minutes for real-time data sharing
VTT_realtime_in_minutes = (model$estimate[["b_data_realtime"]] / model$estimate[["b_tt"]]) * 60

# Display the result
cat("The value of not sharing real-time data, in terms of equivalent minutes of travel time:", VTT_realtime_in_minutes, "minutes\n")

# Calculate the Value of Travel Time (VTT) equivalent in terms of minutes for anonymized data sharing
VTT_anon_in_minutes = (model$estimate[["b_data_anon"]] / model$estimate[["b_tt"]]) * 60

# Display the result
cat("The value of not sharing anonymized occupancy data, in terms of equivalent minutes of travel time:", VTT_anon_in_minutes, "minutes\n")