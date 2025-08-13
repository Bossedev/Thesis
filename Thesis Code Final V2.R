setwd("C:/Users/Bosse/Documents/Thesis")
library(readr)
library(dplyr)
library(readxl)
##########
#short test
#sum(is.na(combined$gender))
#mean(is.na(combined$gender))
#table(combined$gender, useNA = "ifany")
#######
#missing_gender_ids <- projects %>%
#  left_join(profiles_a, by = c("freelance_identity_id" = "identity_id")) %>%
#  filter(is.na(gender)) %>%
#  distinct(freelance_identity_id)
#n_missing <- nrow(missing_gender_ids)
#cat("Number of project IDs without gender info:", n_missing, "\n")
#head(missing_gender_ids)

######################### Start of actual code
profiles_a <- read_csv("profiles_parta.csv")
profiles_b <- read_csv("profiles_partb.csv")
projects   <- read_csv("projects_contracted.csv")
matches    <- read_csv("matches_source.csv")
opportunity_channels <- read_csv("opportunity_match_source_channel.csv")

#RQ1
n_initial_matches <- n_distinct(projects$opportunity_id)
n_final_matches <- n_distinct(projects$project_id)

results <- data.frame(
  initial_matches = n_initial_matches,
  final_matches = n_final_matches,
  difference = n_initial_matches - n_final_matches,
  percent_difference = (n_initial_matches - n_final_matches) / n_initial_matches * 100,
  final_to_initial_ratio = n_final_matches / n_initial_matches
)

print(results)


######################################
projects_gender <- projects %>%
  left_join(profiles_a, by = c("freelance_identity_id" = "identity_id"))
#RQ2
# Calculate summary by gender
summary_by_gender <- projects_gender %>%
  group_by(gender) %>%
  summarise(
    initial_matches = n_distinct(opportunity_id),
    final_matches = n_distinct(project_id),
    difference = initial_matches - final_matches,
    percent_difference = (initial_matches - final_matches) / initial_matches * 100,
    final_to_initial_ratio = final_matches / initial_matches,
    .groups = "drop"
  )

print(summary_by_gender)

profiles_a %>%
  count(identity_id) %>%
  filter(n > 1)

n_initial_matches <- n_distinct(projects$opportunity_id)
n_final_matches <- n_distinct(projects$project_id)

results <- data.frame(
  initial_matches = n_initial_matches,
  final_matches = n_final_matches,
  difference = n_initial_matches - n_final_matches,
  percent_difference = (n_initial_matches - n_final_matches) / n_initial_matches * 100,
  final_to_initial_ratio = n_final_matches / n_initial_matches
)

print(results)
#reg <-lm(NumberofMatches ~ gender + nb_experiences + nb_educations + nb_likes + match_type, data = combined)
#print(reg)
######################################

########### SubRQ 2 hier
#importeer max 5000 regels
#profiles_a <- read.csv("profiles_parta.csv", nrow = 5000)
#profiles_b <- read.csv("profiles_partb.csv", nrow = 5000)
#projects   <- read.csv("projects_contracted.csv", nrow = 5000)
#matches    <- read.csv("matches_source.csv", nrow = 5000)

#combineer data op de identity_id column
combined <- left_join(matches, profiles_a, by = "identity_id")

#opschonen data
combined <- subset(combined, select = c(identity_id, archivedate, profile_creation_date, first_name.x, gender.x, preferred_family,
                                        nb_experiences, nb_educations,nb_recommendations_with_text, nb_likes, first_time_supermalter, is_selected_for_push,  
                                        eligible_malt_plus_sourcing,  seniority_as_freelancer_signup, sub_channel, match_source, rating, experience_level, NumberofMatches))

combined <- combined %>% rename(
  gender = gender.x,
  first_name = first_name.x)
combined$profile_creation_date <- as.Date(combined$profile_creation_date)
combined$archivedate <- as.Date(combined$archivedate)

combined$profile_age_days <- as.numeric(difftime(combined$archivedate, combined$profile_creation_date, units = "days"))
combined$profile_age_years <- combined$profile_age_days / 365.25

combined <- combined %>%
  mutate(match_type = case_when(
    match_source %in% c("AUTOMATIC_LAUNCH", "AUTOMATIC_MATCHING", "REVERSE_SOURCING") ~ "algorithmic",
    match_source %in% c("MANUAL_SOURCING_SEARCH", "CLIENT_INSERTION", "MANAGER_INSERTION", "AUTOMATIC_REVERSE_SOURCING") ~ "human",
    TRUE ~ NA_character_
  ))
sum(!is.na(combined$rating))  # Count of freelancers with rating
sum(is.na(combined$rating))   # Count of freelancers without rating
#max(combined$archivedate, na.rm = TRUE)

#,
#  human_matcher = ifelse(
#    match_source %in% c("AUTOMATIC_LAUNCH", "AUTOMATIC_MATCHING", "REVERSE_SOURCING"), 
#    0,  # algorithmic only
#    1   # human involvement (hybrid)
#  ))

####################################################################### ---- Analyses
t.test(NumberofMatches ~ gender, data = combined)
wilcox.test(NumberofMatches ~ gender, data = combined)
####################################################

reg <-lm(NumberofMatches ~ gender + nb_experiences + nb_educations + nb_likes + match_type, data = combined)
print(reg)
#simpele regressie
reg1 <- lm(rating ~ gender+ NumberofMatches, data = combined)
summary(reg1)

#regressie 
reg2 <- lm(rating ~ gender + NumberofMatches + nb_likes + sub_channel + match_source, data = combined)
summary(reg2)
reg6 <-lm(rating ~ gender + NumberofMatches + nb_likes + sub_channel + match_type, data = combined)
summary(reg6)
#regressie met interactie effect 
reg3 <- lm(rating ~ gender * nb_likes +sub_channel + NumberofMatches+ match_source+ profile_age_years + preferred_family	, data = combined)
summary(reg3)

#t-test voor verschil man en vrouw op hoeveelheid matches (er is dus een statisch verschil) 
t.test(NumberofMatches ~ gender, data = combined)
t.test(rating ~ gender, data = combined)

#Nu ook met de NBs
reg4 <- lm(rating ~ gender + nb_experiences + nb_educations +NumberofMatches+
            # nb_recommendations_with_text #Removed due to multicollinearity
           + nb_likes  + profile_age_years +  match_type,#preferred_family +
           data = combined)
summary(reg4)

#######################
#combined$preferred_family <- as.factor(combined$preferred_family)
#combined$experience_level <- as.factor(combined$experience_level)
#reg4_enhanced <- lm(NumberofMatches ~ gender + nb_experiences + nb_educations +
#                      nb_recommendations_with_text + nb_likes +
#                      experience_level + preferred_family + rating +
#                      sub_channel + match_source,
#                    data = combined)
#
#summary(reg4_enhanced)

install.packages("car")  #voor multicolli
library(car)
vif(reg4)

reg5 <- lm(rating ~ gender * nb_likes + 
             gender * nb_recommendations_with_text +
             nb_experiences + nb_educations + 
             sub_channel + match_type + NumberofMatches+ profile_age_years + preferred_family,
           data = combined)

summary(reg5)
##
mod_matches_tbl <- lm(
  NumberofMatches ~ gender + nb_experiences + nb_educations +
   # nb_recommendations_with_text #Removed due to multicollinearity
  + nb_likes + rating,
  data = combined
)
summary(mod_matches_tbl)
##
mod_h2 <- lm(rating ~ gender * nb_likes + gender * nb_recommendations_with_text +
               nb_experiences + nb_educations + NumberofMatches +
               profile_age_years + match_type,
             data = combined)

summary(mod_h2)
#Final H2 test 
mod_rating <- lm(NumberofMatches ~ gender * nb_likes + 
                   #gender * nb_recommendations_with_text + #Removed due to multicollinearity
                   gender * rating +
                   nb_experiences + nb_educations + 
                   profile_age_years + match_type,
                 data = combined)

summary(mod_rating)
############################
#reg5_enhanced <- lm(NumberofMatches ~ gender * nb_likes + 
#                      gender * nb_recommendations_with_text +
#                      nb_experiences + nb_educations + 
#                      experience_level + preferred_family + rating +
#                      sub_channel + match_source,
#                    data = combined)
#
#summary(reg5_enhanced)
############################
#Cost of bias vgm

#reg_revenue <- lm(booking_revenue_contracted ~ gender + 
#                    nb_likes + nb_recommendations_with_text + 
#                    nb_experiences + nb_educations +
#                    sub_channel + match_source,
#                  data = projects_gender)

#summary(reg_revenue)
#names(projects_gender)
profiles_a_clean <- profiles_a %>%
  distinct(identity_id, .keep_all = TRUE)

############# Gaat om deze:
projects_full <- projects %>%
  left_join(profiles_a_clean, by = c("freelance_identity_id" = "identity_id")) %>%
  left_join(matches, by = c("freelance_identity_id" = "identity_id"))
projects_full <- projects_full %>%
  mutate(gender = coalesce(gender.x, gender.y))
projects_full$profile_creation_date <- as.Date(projects_full$profile_creation_date)
projects_full$archivedate <- as.Date(projects_full$archivedate)

# Calculate profile age
projects_full$profile_age_days <- as.numeric(difftime(projects_full$archivedate, projects_full$profile_creation_date, units = "days"))
projects_full$profile_age_years <- projects_full$profile_age_days / 365.25
projects_full <- projects_full %>%
  mutate(match_type = case_when(
    match_source %in% c("AUTOMATIC_LAUNCH", "AUTOMATIC_MATCHING", "REVERSE_SOURCING") ~ "algorithmic",
    match_source %in% c("MANUAL_SOURCING_SEARCH", "CLIENT_INSERTION", "MANAGER_INSERTION", "AUTOMATIC_REVERSE_SOURCING") ~ "human",
    TRUE ~ NA_character_
  ))

reg_revenue <- lm(booking_revenue_contracted ~ gender + 
                    nb_likes + nb_recommendations_with_text + 
                    nb_experiences + nb_educations +
                    sub_channel + match_type + profile_age_years + preferred_family+ rating,
                  data = projects_full)

summary(reg_revenue)
#names(projects_full)
#table(projects_full$gender.x == projects_full$gender.y, useNA = "ifany")0

###################################### APA output krijgen
#reg_revenue_simple <- lm(booking_revenue_contracted ~ gender + nb_likes + nb_recommendations_with_text, 
#                         data = projects_full)
#summary(reg_revenue_simple)
#reg_revenue_enhanced <- lm(booking_revenue_contracted ~ gender + nb_likes + 
#                             nb_recommendations_with_text + experience_level + 
#                             preferred_family + rating,
#                           data = projects_full)
#
#summary(reg_revenue_enhanced)
n_distinct(profiles_a$identity_id)
n_distinct(projects$opportunity_id)
n_distinct(projects$project_id)
#n_distinct(matches$identity_id, matches$opportunity_id)
nrow(matches)
###########################################
vif(reg)
vif(reg1)
vif(reg2)
vif(reg6)
vif(reg4)

vif(mod_matches_tbl)
vif(reg_revenue)

vif(reg3,        type = "predictor")
vif(reg5,        type = "predictor")
vif(mod_h2,      type = "predictor")
vif(mod_rating,  type = "predictor")



######### Extra descriptives
range(combined$profile_creation_date, na.rm = TRUE)
#sapply(combined, function(x) sum(is.na(x))) # This shows all missing values per cat
#nrow(na.omit(combined))
################################################################################################################ 
library(dplyr)
library(ggplot2)

gender_counts <- combined %>%
  count(gender)

#lot the counts
ggplot(gender_counts, aes(x = gender, y = n, fill = gender)) +
  geom_col(width = 0.6, alpha = 0.7) +
  geom_text(aes(label = n),
            vjust = -0.5, size = 5) +
  labs(title = "Number of Freelancers by Gender",
       x = "Gender",
       y = "Number of Freelancers") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


#Robustness Checks
install.packages(c("nortest", "car"))
library(nortest)
library(car)

hist(combined$rating, main = "Histogram of Ratings", xlab = "Rating")
qqnorm(combined$rating)
qqline(combined$rating, col = "red")

ad.test(na.omit(combined$rating))
ad.test(na.omit(combined$NumberofMatches))
wilcox.test(rating ~ gender, data = combined)
wilcox.test(NumberofMatches ~ gender, data = combined)

#############
combined$gender <- as.factor(combined$gender)

#Boxplot
ggplot(combined, aes(x = gender, y = NumberofMatches, fill = gender)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  scale_y_log10() +
  labs(title = "Number of Matches by Gender (log scale)",
       x = "Gender",
       y = "Number of Matches (log scale)") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
#######################
gender_means <- combined %>%
  group_by(gender) %>%
  summarise(
    mean_matches = mean(NumberofMatches, na.rm = TRUE),
    se_matches = sd(NumberofMatches, na.rm = TRUE) / sqrt(n())  # standard error
  )

# create bar chart with error bars
ggplot(gender_means, aes(x = gender, y = mean_matches, fill = gender)) +
  geom_col(width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_matches - se_matches, ymax = mean_matches + se_matches),
                width = 0.2, color = "black") +
  geom_text(aes(label = round(mean_matches, 0)), vjust = -0.5, size = 5) +
  labs(title = "Mean Number of Matches by Gender",
       x = "Gender",
       y = "Mean Number of Matches") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#00008B", "#FF69B4"))
library(scales)

######################
ggplot(combined, aes(x = factor(nb_educations), fill = gender)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  labs(title = "Number of Educations per Freelancer by Gender",
       x = "Number of Educations",
       y = "Count of Freelancers",
       fill = "Gender") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("MAN" = "#00008B",    #  
                               "WOMAN" = "#FF69B4")) +  # 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#######################################

combined <- combined %>%
  mutate(profile_age_group = cut(profile_age_years,
                                 breaks = seq(0, 15, by = 1),   # bins of 1 year (adjust range as needed)
                                 right = FALSE,
                                 labels = paste0(seq(0, 14), "-", seq(1, 15), " yrs")))

#plot
ggplot(combined, aes(x = profile_age_group, fill = gender)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  labs(title = "Profile Age (Years on Platform) by Gender",
       x = "Profile Age Group",
       y = "Count of Freelancers",
       fill = "Gender") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("MAN" = "#00008B",    # dark blue
                               "WOMAN" = "#FF69B4")) +  # 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
##################### preferred family hier
ggplot(combined, aes(x = preferred_family, fill = gender)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  labs(title = "Preferred Family by Gender",
       x = "Preferred Family",
       y = "Count of Freelancers",
       fill = "Gender") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("MAN" = "#00008B",    # dark blue
                               "WOMAN" = "#FF69B4")) +  # orange
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
#################en nu exp level ook
ggplot(combined, aes(x = experience_level, fill = gender)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  labs(title = "Experience Level by Gender",
       x = "Experience Level",
       y = "Count of Freelancers",
       fill = "Gender") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("MAN" = "#00008B",
                               "WOMAN" = "#FF69B4")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

#######################################
match_source_plot <- combined %>%
  filter(!is.na(match_source)) %>%
  group_by(match_source, gender) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(match_source) %>%
  mutate(proportion = count / sum(count))

ggplot(match_source_plot, aes(x = match_source, y = proportion, fill = gender)) +
  geom_col(position = "fill") +  
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Gender Distribution by Match Source",
       x = "Match Source",
       y = "Proportion",
       fill = "Gender") +
  scale_fill_manual(values = c("MAN" = "#00008B",    
                               "WOMAN" = "#FF69B4"   
  )) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#################################################################
sub_channel_plot <- combined %>%
  filter(!is.na(sub_channel)) %>%
  group_by(sub_channel, gender) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(sub_channel) %>%
  mutate(proportion = count / sum(count))

# ✅ Plot
ggplot(sub_channel_plot, aes(x = sub_channel, y = proportion, fill = gender)) +
  geom_col(position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Gender Distribution by Sub Channel",
       x = "Sub Channel",
       y = "Proportion",
       fill = "Gender") +
  scale_fill_manual(values = c("MAN" = "#00008B",    # dark blue
                               "WOMAN" = "#FF69B4")) +  # orange
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
########################################################
library(scales)

match_type_plot <- combined %>%
  filter(!is.na(match_type)) %>%
  group_by(match_type, gender) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(match_type) %>%
  mutate(proportion = count / sum(count))

ggplot(match_type_plot, aes(x = match_type, y = proportion, fill = gender)) +
  geom_col(position = "fill") +
  geom_text(aes(label = paste0(round(proportion * 100, 1), "%")),
            position = position_fill(vjust = 0.5), color = "white", size = 5) + 
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Gender Distribution by Match Type",
       x = "Match Type",
       y = "Proportion",
       fill = "Gender") +
  scale_fill_manual(values = c("MAN" = "#00008B", "WOMAN" = "#FF69B4")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#######################################################
combined$rec_group <- ifelse(combined$nb_recommendations_with_text > 15, 
                             "15+", 
                             as.character(combined$nb_recommendations_with_text))

combined$rec_group <- factor(combined$rec_group, 
                             levels = c(as.character(0:15), "15+"))

ggplot(combined, aes(x = rec_group, fill = gender)) +
  geom_bar(position = "dodge", alpha = 0.9) +
  labs(title = "Number of Recommendations with Text by Gender",
       x = "Number of Recommendations",
       y = "Number of Freelancers",
       fill = "Gender") +
  scale_fill_manual(values = c("MAN" = "#00008B", "WOMAN" = "#FF69B4")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

##########################
