#Chance York
#Nov 12, 2024
#AI Transparency Survey

#### Packages ------------------

library(tidyverse)
library(broom)
library(rlang)
library(grid)
#install.packages("gridExtra")
library(gridExtra)
#install.packages("ggalluvial")
library(ggalluvial)
#install.packages("svglite")
library(svglite)


#### Options ------------------
colors <- rep(c("#DE81F6", "#9AE263", "#6D267B", "#A5F828",  "#D705F2", "#63458C", "#72D939",  "#A66641", "#A904BF",  "#314032"), 20)
palette(colors)

#Set segmentation colors
segment_colors <- c(`Social & Comfortable` = "#54ffbdff", 
                    `Confident Learners` = "#fff450ff", 
                    `Concerned Traditionalists` ="#ff4e5eff", 
                    `Principled Trailblazers` = "#7542e5ff", 
                    `Accepting & Mobile` = "#00fffdff")

today.date <- Sys.Date() 

#### Import -------------------
aidata <- read.csv2("/Users/Chance/Desktop/raw.csv", sep=",", na.strings="")

#### Functions ----------------
source("/Users/Chance/Desktop/aitrans/ai_functions.r")

#### Clean --------------------
#Use for Qualtrics, where row 2 is tossed and row 1 is saved as question list
clean.survey <- function(x) {
  question_list <- as.list(x[1, ])
  
  x <- x[-c(1, 2), ] %>%
    setNames(tolower(names(.))) %>% 
    rename(duration = duration..in.seconds., lang = userlanguage, captcha = q_recaptchascore) %>%
    select(-c(status, finished, recordeddate, responseid, distributionchannel, progress)) %>%
    mutate(across(c(duration, q4, q46_page.submit, captcha), as.numeric)) %>%
    mutate(across(c(startdate, enddate), \(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S"))) %>%
    mutate_if(is.character, ~ factor(stringr::str_replace_all(.x, "\\t", ""), levels = unique(stringr::str_replace_all(.x, "\\t", ""))))
  
  list(data = x, question_list = question_list)}

cleaned_data <- clean.survey(aidata)
aidata <- cleaned_data$data
question_list <- cleaned_data$question_list


#### Cint IDs ---------------------

completes <- aidata %>% 
  select(cintid, q7.1, captcha, duration, q46_page.submit, lang, q3) %>% 
  mutate(completed = case_when(!is.na(q7.1 ) ~"Complete", is.na(q7.1) ~"Incomplete"),
        qualityterm = case_when(q46_page.submit<=8.999~"Failed QT", q46_page.submit>=9~"Passed")) %>% 
 select(-c(q7.1))

#countries <- unique(completes$q3)

#for (country in countries) {
#    country_data <- completes %>% filter(q3 == country)
#    for (status in c("Complete", "Incomplete")) {
#      status_data <- country_data %>% filter(completed == status)
#      file_name <- paste0("/Users/Chance/Desktop/", country, "_", status, ".csv")
#      write.csv(status_data, file = file_name, row.names = FALSE) } }


#### QC  ---------------------

#Total obs = 11,772

#Remove incompletes
aidata <- aidata %>% 
  filter(!is.na(q7.1)) #Removes 4,952 obs

#Remove bots
aidata <- aidata %>% 
  filter(captcha>=.5) #Removes 481 obs

#Flag and remove outliers on survey duration
aidata <- aidata %>%
  mutate(lower_bound = quantile(duration, 0.25) - 1.5 * IQR(duration),
    upper_bound = quantile(duration, 0.75) + 1.5 * IQR(duration),
    is_outlier = ifelse(duration < lower_bound | duration > upper_bound, TRUE, FALSE))

ggplot(aidata, aes(y = duration)) +
  geom_boxplot() +
  labs(title = "Boxplot of Duration", y = "Duration") +
  theme_minimal()

aidata<- aidata %>%
  filter(!is_outlier) %>%  
  select(-lower_bound, -upper_bound, -is_outlier) #Removes 556 obs

ggplot(aidata, aes(y = duration)) +
  geom_boxplot() +
  labs(title = "Boxplot of Duration (outleirs removed)", y = "Duration") +
  theme_minimal()

#Remove test data
aidata <- aidata %>% 
  filter(as.POSIXct(startdate, format = "%Y-%m-%d %H:%M:%S") > as.POSIXct("2024-11-17", format = "%Y-%m-%d")) #Removes 8 obs

#Check and remove duplicate IDS
aidata <- aidata %>% 
  group_by(cintid) %>% 
  mutate(is_duplicate = n() > 1) %>% 
  ungroup()

aidata <- aidata %>% 
  filter(is_duplicate=="FALSE") #Removes 2 obs


#Check page submit timer, all complete reponses should be above 8.999
aidata <- aidata %>%
  mutate(flag_q46_timer = if_else(q46_page.submit <= 8.999, TRUE, FALSE))

aidata %>%
  summarise(count_flagged = sum(flag_q46_timer)) #Check completed

#### Randomization ------------
#Randomly Selected Browser for AI-Related Question
#Clean html tags
aidata <- aidata %>%
  mutate(primarybrowserchoice = gsub("\\s*<img[^>]+>", "", primarybrowserchoice),
         desktopbrowserchoice = gsub("\\s*<img[^>]+>", "", desktopbrowserchoice),
         mobilebrowserchoice = gsub("\\s*<img[^>]+>", "", mobilebrowserchoice)) %>% 
  mutate(primarybrowserchoice = str_trim(primarybrowserchoice), 
         r_browser = case_when(
           primarybrowserchoice %in% c("Firefox", "Chrome", "Safari", "Edge", "Brave", "Samsung", "DuckDuckGo", "Opera", "Vivaldi") ~ primarybrowserchoice,
           !is.na(primarybrowserchoice) ~ "Other",
           TRUE ~ NA_character_ ) )

#Browser Used by R Randomly Piped into AI Questions
bar_single_arranged(aidata, r_browser,xlab="Browser Randomly Piped in AI Feature Qs")
ggsave(file="plot.png", dpi=300, height=5, width=9, bg="white")

#Simplified List of Browsers
aidata <- aidata %>%
  mutate(r_browser_simple = case_when(r_browser %in% c("Firefox", "Chrome", "Safari", "Edge") ~ r_browser,
      !is.na(r_browser) ~ "Other",
      TRUE ~ NA_character_),
    r_browser_simple = factor(r_browser_simple, levels = c("Firefox", "Chrome", "Safari", "Edge", "Other")))

bar_single_arranged(aidata, r_browser_simple,xlab="Browser Randomly Piped in AI Feature Qs")
ggsave(file="plot.png", dpi=300, height=5, width=9, bg="white")

aidata <- aidata %>%
  mutate(firefox = ifelse(r_browser %in% c("Firefox"), 1, 0),
         chrome = ifelse(r_browser %in% c("Chrome"), 1, 0),
         edge = ifelse(r_browser %in% c("Edge"), 1, 0),
         safari = ifelse(r_browser %in% c("Safari"), 1, 0),
         otherbrowsers = ifelse(r_browser %in% c("Samsung", "Vivaldi", "Brave", "Opera", "DuckDuckGo", "Other"), 1, 0))

#Type of Browser Randomly Chosen for AI Questions
aidata <- aidata %>% 
  mutate(r_source = primarybrowsersource)

bar_single_arranged(aidata, r_source, xlab="Type of Browser in AI Qs")
ggsave(file="plot.png", dpi=300, height=5, width=7, bg="white")


#### Browsers -----------------

#Primary Desktop
bar_single_arranged(aidata, q20,xlab="Primay DESKTOP Browser")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")

#Primary Mobile
bar_single_arranged(aidata, q23,xlab="Primay MOBILE Browser")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")


#### Typing Tool -----------------
#Some codes borrowed from Jon's 01-wrangling.R code

#How did user select desktop browser
aidata$brow_choice <- fct_recode(aidata$q4.1,
                                "Don't Own Comp" = "I don’t own/use a laptop/desktop computer",
                                "Pre-Installed" = "I didn’t choose, it's the browser that came with my device",
                                "Deliberate Choice" = "I deliberately chose it",
                                "Someone Else Chose" = "Someone else helped me choose it/set it up for me",
                                "Work/School Requires" = "I am required to use it (by my employer or school)",
                                "Not Sure" = "I’m not sure") %>% 
  fct_relevel("Deliberate Choice", "Came Pre-Installed", "Someone Else Chose","Work/School Requires", "Not Sure", "Don't Own Comp")

bar_single_arranged(aidata, brow_choice, xlab = NULL)
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")

aidata <- aidata %>%
  mutate(desktop_browser_chooser = if_else(brow_choice == "Deliberate Choice", 1, 0),
          desktop_browser_chooser = replace_na(desktop_browser_chooser, 0))

#Level of user agreement with statements about privacy concern and confidence  
aidata <- agree_relevel_idv(aidata, var_name = "q7_1", new_var_name = privacy_confident)
aidata <- agree_relevel_idv_flip(aidata, var_name = "q7_2", new_var_name = privacy_concern_R)
aidata <- agree_relevel_idv(aidata, var_name = "q7_3", new_var_name = ads_free)
aidata <- agree_relevel_idv(aidata, var_name = "q7_4", new_var_name = privacy_proactive)

bar_single(aidata, privacy_confident, xlab = "Level of Agreement")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")

bar_single(aidata, privacy_concern_R, xlab = "Level of Agreement")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")

bar_single(aidata, ads_free, xlab = "Level of Agreement")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")

bar_single(aidata, privacy_proactive, xlab = "Level of Agreement")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")


#Time online
aidata <- freq_media(aidata, var_name = "q6_1", new_var_name = time_online_entertainment)
aidata <- freq_media(aidata, var_name = "q6_2", new_var_name = time_online_work)
aidata <- freq_media(aidata, var_name = "q6_3", new_var_name = time_online_socialmedia)
aidata <- freq_media(aidata, var_name = "q6_4", new_var_name = time_online_info)

bar_single(aidata, time_online_entertainment, xlab = "Frequency")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")

bar_single(aidata, time_online_work, xlab = "Frequency")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")

bar_single(aidata, time_online_socialmedia, xlab = "Frequency")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")

bar_single(aidata, time_online_info, xlab = "Frequency")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")

aidata <- weekly_conversion(aidata, time_online_entertainment_num, time_online_entertainment)
aidata <- weekly_conversion(aidata, time_online_work_num, time_online_work)
aidata <- weekly_conversion(aidata, time_online_socialmedia_num, time_online_socialmedia)
aidata <- weekly_conversion(aidata, time_online_info_num, time_online_info)


#Degree to which user thinks browser reflects personal values
aidata$browser_values <- fct_recode(aidata$q5_1,
                                     "<< Agree Much More: Doesn't Reflect Values" = "1",
                                     "< Agree a Little" = "2",
                                     "Neutral" = "3",
                                     "> Agree a Little" = "4",
                                     ">> Agree Much More: Reflects My Values" = "5") %>% 
  fct_relevel("<< Agree Much More: Doesn't Reflect Values", "< Agree a Little", "Neutral",
              "> Agree a Little", ">> Agree Much More: Reflects My Values")

bar_single(aidata, browser_values, xlab = NULL)
ggsave(file="plot.png", dpi=300, height=7, width=12, bg="white")

#Interest in switching browsers
aidata$browser_switch <- fct_recode(aidata$q5_2,
                                 "<< Agree Much More: Not Interested in Switch" = "1",
                                 "< Agree a Little" = "2",
                                 "Neutral" = "3",
                                 "> Agree a Little" = "4",
                                 ">> Agree Much More: Willing to Switch" = "5") %>% 
  fct_relevel("<< Agree Much More: Not Interested in Switch", "< Agree a Little", "Neutral",
              "> Agree a Little", ">> Agree Much More: Willing to Switch")

bar_single(aidata, browser_switch, xlab = NULL)
ggsave(file="plot.png", dpi=300, height=7, width=12, bg="white")


#User perceptions of companies having access to data in exchange for personalization
aidata$data_experience <- fct_recode(aidata$q9_1,
                                    "<< Agree Much More: Less Access & Worse P13n" = "1",
                                    "< Agree a Little" = "2",
                                    "Neutral" = "3",
                                    "> Agree a Little" = "4",
                                    ">> Agree Much More: More Access & Better P13n" = "5") %>% 
  fct_relevel("<< Agree Much More: Less Access & Worse P13n", "< Agree a Little", "Neutral",
              "> Agree a Little", ">> Agree Much More: More Access & Better P13n")

bar_single(aidata, data_experience, xlab = NULL)
ggsave(file="plot.png", dpi=300, height=7, width=12, bg="white")


#Desktop vs. mobile device orientation
aidata$mobile_vs_desktop <- fct_recode(aidata$q1,
                                     "Only use smartphone/tablet" = "I only use a smartphone/tablet",
                                     "Mostly use smartphone/tablet" = "I use a smartphone/tablet for most of my internet needs but still use a laptop/desktop computer some of the time",
                                     "Use both evenly" = "I use a smartphone/tablet and a laptop/desktop computer for roughly an equal amount of time",
                                     "Mostly use computer" = "I use a laptop/desktop for most of my internet needs but still use a smartphone/tablet some of the time",
                                     "Only use computer" = "I only use a laptop/desktop computer") %>% 
  fct_relevel("Only use computer", "Mostly use computer", "Use both evenly", "Mostly use smartphone/tablet", "Only use smartphone/tablet")

bar_single(aidata, mobile_vs_desktop, xlab = NULL)
ggsave(file="plot.png", dpi=300, height=7, width=11, bg="white")


#Willingness to pay for digital products
aidata$wtp_general <- fct_recode(aidata$q11,
                                       "Not at all" = "Not at all willing to pay",
                                       "Slightly" = "Slightly willing to pay",
                                       "Somewhat" = "Somewhat willing to pay",
                                       "Very" = "Very willing to pay",
                                       "Extremely" = "Extremely willing to pay") %>% 
  fct_relevel("Not at all", "Slightly", "Somewhat", "Very", "Extremely")

bar_single(aidata, wtp_general, xlab = "Willingness to Pay")
ggsave(file="plot.png", dpi=300, height=7, width=11, bg="white")

#Desktop browser counts
aidata <- aidata %>%
  mutate(across(all_of(c("q3_54", "q3_55", "q3_56", "q3_57", "q3_59", "q3_60", "q3_61", "q3_62", "q3_64")),as.numeric)) %>%
  mutate(across(all_of(c("q3_54", "q3_55", "q3_56", "q3_57", "q3_59", "q3_60", "q3_61", "q3_62", "q3_64")), ~replace_na(.x, 0))) %>% 
  mutate(desktop_browsers_num = q3_54 + q3_55 + q3_56 + q3_57 + q3_59 + q3_60 + q3_61 + q3_62 + q3_64)

#Convert any non-numeric variables to numeric for segment calculations
aidata <- aidata %>%
  mutate(across(all_of(c("privacy_confident", "privacy_concern_R", "ads_free", "privacy_proactive",
                         "browser_switch", "browser_values", "data_experience", "mobile_vs_desktop", "wtp_general")),as.numeric))



#Calculate segments
###### typing tool 1 (pasted in from cycle 1 handover code for typing tool)
# K-MEANS R2 - 5 SEGMENTS

aidata <- aidata %>%
  mutate(segment_1 = -47.625416 + (4.2841365 * mobile_vs_desktop) + (2.1605497 * browser_values) + (2.0252121 * data_experience) + (0.1245733 * time_online_entertainment_num) + (2.0272987 * wtp_general) + (3.0258398 * privacy_concern_R) + (3.1106823 * ads_free) + (0.1416866 * time_online_info_num) + (3.3889422 * desktop_browser_chooser) + (1.2081142 * time_online_work_num) + (4.101708 * privacy_proactive) + (1.6273992 * browser_switch) + (1.5752868 * desktop_browsers_num) + (4.1043393 * privacy_confident) + (0.1587867 * time_online_socialmedia_num))

aidata <- aidata %>%
  mutate(segment_2 = -49.700332 + (4.035968 * mobile_vs_desktop) + (1.57754 * browser_values) + (1.70934 * data_experience) + (2.88342 * time_online_entertainment_num) + (1.885415 * wtp_general) + (3.703124 * privacy_concern_R) + (2.843079 * ads_free) + (3.085644 * time_online_info_num) + (4.061696 * desktop_browser_chooser) + (3.71554 * time_online_work_num) + (4.173904 * privacy_proactive) + (1.419669 * browser_switch) + (1.591375 * desktop_browsers_num) + (4.03407 * privacy_confident) + (1.810713 * time_online_socialmedia_num))

aidata <- aidata %>%
  mutate(segment_3 = -37.20273262 + (3.06333663 * mobile_vs_desktop) + (1.65457579 * browser_values) + (1.48189311 * data_experience) + (-0.01622301 * time_online_entertainment_num) + (1.04947813 * wtp_general) + (3.74191824 * privacy_concern_R) + (2.35466436 * ads_free) + (1.15428426 * time_online_info_num) + (2.77026883 * desktop_browser_chooser) + (1.29477468 * time_online_work_num) + (4.24237239 * privacy_proactive) + (1.35571717 * browser_switch) + (1.27247867 * desktop_browsers_num) + (3.55988339 * privacy_confident) + (-0.01090164 * time_online_socialmedia_num))

aidata <- aidata %>%
  mutate(segment_4 = -67.73333 + (4.050163 * mobile_vs_desktop) + (2.599856 * browser_values) + (2.644205 * data_experience) + (2.939592 * time_online_entertainment_num) + (2.893881 * wtp_general) + (3.419231 * privacy_concern_R) + (3.30567 * ads_free) + (2.421413 * time_online_info_num) + (3.807773 * desktop_browser_chooser) + (4.542104 * time_online_work_num) + (4.762146 * privacy_proactive) + (2.07951 * browser_switch) + (1.986113 * desktop_browsers_num) + (4.599955 * privacy_confident) + (2.327396 * time_online_socialmedia_num))

aidata <- aidata %>%
  mutate(segment_5 = -40.491698 + (4.815806 * mobile_vs_desktop) + (1.530761 * browser_values) + (1.830794 * data_experience) + (1.435068 * time_online_entertainment_num) + (1.301866 * wtp_general) + (3.542746 * privacy_concern_R) + (2.852665 * ads_free) + (1.553387 * time_online_info_num) + (1.616936 * desktop_browser_chooser) + (1.314038 * time_online_work_num) + (3.106629 * privacy_proactive) + (1.374147 * browser_switch) + (1.015737 * desktop_browsers_num) + (3.397849 * privacy_confident) + (1.512922 * time_online_socialmedia_num))

aidata %>% select(starts_with("segment_")) %>% summary()

# recode the segments
aidata <- aidata %>%
  rename(
    "Social & Comfortable" = segment_1,
    "Confident Learners" = segment_2,
    "Concerned Traditionalists" = segment_3,
    "Principled Trailblazers" = segment_4,
    "Accepting & Mobile" = segment_5,
  )

# create a single column with the highest segment score as that respondents segment assignment
aidata$top_segment <- names(aidata)[134:138][max.col(aidata[134:138])] # update to whatever numbers the 5 segment calculated columns are 

# turn it into a categorical variable and reorder
aidata$segment <- fct_recode(aidata$top_segment) %>% 
  fct_relevel("Confident Learners", "Social & Comfortable", "Concerned Traditionalists", "Principled Trailblazers", "Accepting & Mobile")

bar_single(aidata, segment, xlab="Mozilla Segment")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")

aidata <- aidata %>%
  mutate(sc = ifelse(segment == "Social & Comfortable", 1, 0),
         cl = ifelse(segment == "Confident Learners", 1, 0),
         ct = ifelse(segment == "Concerned Traditionalists", 1, 0),
         pt = ifelse(segment == "Principled Trailblazers", 1, 0),
         am = ifelse(segment == "Accepting & Mobile", 1, 0))


#### Demos -----------------

#Gender
aidata$gender <- fct_recode(aidata$q1.1,  
                          "Don't describe me" = "These don't describe me",
                          "PNTS" = "Prefer not to say") %>% 
  fct_relevel("Don't describe me", "PNTS")

bar_single_arranged(aidata, gender, xlab="Gender")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")

aidata <- aidata %>%
  mutate(women = ifelse(gender == "Woman", 1, 0),
         men = ifelse(gender == "Man", 1, 0))

#Age
aidata <- aidata %>% 
  mutate(age = 2024-q4)
hist_func(aidata, age, xlab="Age")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")

#Generation
aidata <- aidata %>%
  mutate(generation = case_when(
      age >= 18 & age <= 28 ~ "Gen Z",
      age >= 27 & age <= 42 ~ "Millennial",
      age >= 43 & age <= 58 ~ "Gen X",
      age >= 59 & age <= 75 ~ "Boomer",
      age >= 76 ~ "Silent",
      TRUE ~ NA_character_),
    generation = factor(generation, levels = c("Gen Z", "Millennial", "Gen X", "Boomer", "Silent")))

bar_single(aidata, generation, xlab="Generational Cohort")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")


#Education
aidata$educ <- fct_recode(aidata$q3.1,
                          "No degree" ="No certificate, diploma, or degree",
                          "HS" = "High school diploma or equivalent",
                          "Trade" = "Trade, technical, or vocational training",
                          "Some college" = "Some college",
                          "Associate's" = "Associate’s or two-year degree",
                          "Bachelor's" = "Bachelor’s degree",
                          "Master's" = "Master’s degree",
                          "PHD, MD, DDS, JD" = "Doctorate / Ph.D.",
                          "PHD, MD, DDS, JD" = "Professional degree (MD, DDS, JD)",
                          "Other" = "Other") %>%
  fct_relevel("No degree", "HS", "Trade", "Some college", "Associate's", "Bachelor's", "Master's", "PHD, MD, DDS, JD", "Other")

bar_single(aidata, educ, xlab="Education")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")

aidata <- aidata %>%
  mutate(hs = ifelse(educ %in% c("No degree", "HS"), 1, 0),
         postsec = ifelse(educ %in% c("Trade", "Some college", "Associate's"), 1, 0),
         bachelors = ifelse(educ %in% c("Bachelor's"), 1, 0),
         postgrad = ifelse(educ %in% c("Master's", "PhD, MD, DDS, JD"), 1, 0))


#Income
aidata$hhinc <- fct_recode(aidata$q5.1, 
                           "<$25k" = "Less than $25,000", 
                           "$25,000 - $49,999" = "$25,000 - $49,999",
                           "$50,000 - $99,999" = "$50,000 - $99,999",
                           "$100k - $200k" = "$100,000 - $200,000",
                           ">$200k" = "More than $200,000",
                           "PNTS" = "Prefer not to say") %>% 
  fct_relevel("<$25k", "$25,000 - $49,999", "$50,000 - $99,999", "$100k - $200k", ">$200k")

bar_single(aidata, hhinc, xlab="HH Income")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")

#Tried to use dplyr to recode this, but wouldn't work in regression
aidata$income <- as.numeric(aidata$hhinc)
aidata$income[aidata$hhinc == 1] <- 12500   
aidata$income[aidata$hhinc == 2] <- 37500        
aidata$income[aidata$hhinc == 3] <- 75000         
aidata$income[aidata$hhinc == 4] <- 150000        
aidata$income[aidata$hhinc == 5] <- 250000        
aidata$income[aidata$hhinc == 6] <- NA     

#Occupation
aidata$occ <- fct_recode(aidata$q4.2,
                         "Browser job" ="Employed or self-employed at a job where I use a browser",
                         "Non-browser job" = "Employed or self-employed at a job where I don't use a browser",
                         "Student" = "Student",
                         "Homemaker/caregiver" = "Homemaker / Caregiver",
                         "Unemployed" = "Unemployed",
                         "Retired" = "Retired",
                         "Disabled" = "Unable to work / disabled",
                         "Gig worker" = "Gig worker") %>%
  fct_relevel("Browser job", "Non-browser job", "Student", "Homemaker/caregiver", "Unemployed", "Retired", "Disabled", "Gig worker")

bar_single_arranged(aidata, occ, xlab="Occupation")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")

aidata <- aidata %>%
  mutate(browjob = ifelse(occ %in% c("Browser job"), 1, 0),
         student = ifelse(occ %in% c("Student"), 1, 0))

#Kids
aidata$kids <- fct_recode(aidata$q6.1, 
                           "PNTS" = "Prefer not to say") %>% 
  fct_relevel("Yes", "No", "PNTS")

bar_single_arranged(aidata, kids, xlab="Children")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")

aidata <- aidata %>%
  mutate(child = ifelse(kids %in% c("Yes"), 1, 0))

#Area
bar_single_arranged(aidata, q7.1, xlab="Area of Residence")
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")

aidata <- aidata %>%
  mutate(city = ifelse(q7.1 %in% c("Large city"), 1, 0))

#Country
bar_single_arranged(aidata, q3,xlab=NULL)
ggsave(file="plot.png", dpi=300, height=7, width=9, bg="white")

aidata <- aidata %>%
  mutate(usa = ifelse(q3 %in% c("United States"), 1, 0),
         uk = ifelse(q3 %in% c("United Kingdom"), 1, 0),
         france = ifelse(q3 %in% c("France"), 1, 0),
         germany = ifelse(q3 %in% c("Germany"), 1, 0),
         japan = ifelse(q3 %in% c("Japan"), 1, 0),
         india = ifelse(q3 %in% c("India"), 1, 0))



#### Generic AI Qs -----------------------

#Interest in AI Tools/Apps
aidata <- interest_relevel(aidata, "q5") 
bar_single(aidata, q5,xlab=NULL)
ggsave(file="plot.png", dpi=300, height=8, width=12,  bg="white")

#Awareness of AI Tools/Apps
aidata$aiknow <- fct_recode(aidata$q6, 
                            "Nothing" = "I know nothing about them",
                            "Heard of them" = "I’ve heard of them, but don’t understand them",
                            "Know a little" = "I know a little",
                            "Good understanding" = "I have a good understanding",
                            "A lot" = "I know a lot and can explain them well") %>% 
  fct_relevel("Nothing", "Heard of them", "Know a little", "Good understanding", "A lot")

bar_single(aidata, aiknow, xlab=NULL)
ggsave(file="plot.png", dpi=300, height=5, width=7, bg="white")

bar_single(aidata, aiknow, r_browser=="Firefox", xlab=NULL)
ggsave(file="plot.png", dpi=300, height=5, width=7, bg="white")

bar_single(aidata, aiknow, r_browser=="Chrome", xlab=NULL)
ggsave(file="plot.png", dpi=300, height=5, width=7, bg="white")

#Trust AI Tools/Apps
aidata$aitrust <- fct_recode(aidata$q7, 
                            "A great deal" = "A great deal",
                            "Somewhat" = "Somewhat",
                            "A little" = "A little",
                            "Not at all" = "Not at all") %>% 
  fct_relevel("Not at all", "A little", "Somewhat", "A great deal")

bar_single(aidata, aitrust, xlab="Trust of AI Tools and Apps")
ggsave(file="plot.png", dpi=300, height=5, width=7, bg="white")

#Past 7 days, used which AI Tool/App
aidata <- recode_yes_no(aidata, q8_1, q8_4, q8_6)

bar_single(aidata, q8_6, xlab="Apps like YouTube and Instagram that use AI behind the scenes")
ggsave(file="plot.png", dpi=300, height=8, width=9, bg="white")

bar_single(aidata, q8_1, xlab="Standalone AI tools like ChatGPT")
ggsave(file="plot.png", dpi=300, height=8, width=9, bg="white")

bar_single(aidata, q8_4, xlab="AI software like MS Word's 'Draft with Copilot'")
ggsave(file="plot.png", dpi=300, height=8, width=9, bg="white")

generate_custom_venn(aidata, "q8_1", "q8_4", "q8_6")


#### Browser-Specific AI Qs -----------------------

#Interest in Browser Including AI Features
aidata <- interest_relevel(aidata, "q25") 
bar_single(aidata, q25, q25 != "Not sure / no opinion", xlab=NULL)
ggsave(file="plot.png", dpi=300, height=8, width=12,  bg="white")

#Specific Features - Selected or Not as Interesting to Use
aidata <- aidata %>%
  mutate(across(starts_with("q26_"), 
                ~ str_remove(., " [-‐–—]+ .*")))

aidata <- aidata %>%
  mutate(across(starts_with("q26_"), 
                ~ case_when(
                  !is.na(.) ~ paste(., "selected"), 
                  is.na(.) ~ paste("Not selected") )))



#Specific Features - Top Feature
aidata$aifeat <- str_remove(aidata$q27, " [-‐–—]+ .*")
bar_single_arranged_flipped(aidata, aifeat, xlab="Interest")
ggsave(file="plot.png", dpi=300, height=9, width=7, bg="white")

bar_single_arranged_flipped(aidata, aifeat, r_browser=="Firefox",  xlab=NULL)
ggsave(file="plot.png", dpi=300, height=9, width=7, bg="white")

bar_single_arranged_flipped(aidata, aifeat, r_browser=="Chrome", xlab=NULL)
ggsave(file="plot.png", dpi=300, height=9, width=7, bg="white")

#Comfort 
aidata$aicomf <- fct_recode(aidata$q28) %>% 
  fct_relevel("Very uncomfortable", "Somewhat uncomfortable", "Somewhat comfortable", "Very comfortable", "Not sure / no opinion")

bar_single(aidata, aicomf, xlab=NULL)
ggsave(file="plot.png", dpi=300, height=6, width=10, bg="white")


#Opt-in Vs. Opt-out
aidata$aidefault <- fct_recode(aidata$q24, 
                            "AI features OFF by default" = "AI features turned ON BY DEFAULT, with the option to turn them off later in the settings menu",
                            "AI features ON by default" = "AI features turned OFF BY DEFAULT,  with the option to turn them on later in the settings menu",
                            "Neither" = "Neither",
                            "Not sure / no opinion" = "Not sure / no opinion") %>% 
  fct_relevel("AI features OFF by default", "AI features ON by default", "Neither", "Not sure / no opinion")

bar_single(aidata, aidefault, xlab=NULL)
ggsave(file="plot.png", dpi=300, height=8, width=12, bg="white")


#Switching 
aidata$aiswitch <- fct_recode(aidata$q34, 
                               "Much more likely to switch" = "Much more likely to switch browsers",
                               "Slightly more likely to switch" = "Slightly more likely to switch",
                               "Neutral / no change" = "Neutral / no change",
                               "Slightly more likely to keep using" = "Slightly more likely to keep using",
                               "Much more likely to keep using" = "Much more likely to keep using this browser") %>% 
  fct_relevel("Much more likely to switch", "Slightly more likely to switch", "Neutral / no change", "Slightly more likely to keep using", "Much more likely to keep using")

bar_single(aidata, aiswitch, xlab=NULL)
ggsave(file="plot.png", dpi=300, height=6, width=10, bg="white")

#GenAI Assistant vs. Basket of Features
aidata$genspec <- fct_recode(aidata$q45, 
                              "Prefer single AI assisant" = "Prefer an AI assistant that helps me with a variety of tasks",
                              "Prefer different AI Features" = "Prefer different AI features that help me with specific tasks",
                              "Neither" = "Neither",
                              "Not sure / no opinion" = "Not sure / no opinion") %>% 
  fct_relevel("Prefer different AI Features", "Prefer single AI assisant", "Neither", "Not sure / no opinion")

bar_single(aidata, genspec, xlab=NULL)
ggsave(file="plot.png", dpi=300, height=8, width=12, bg="white")




#Importance / Labeling
aidata <- importance_relevel(aidata, c("q29_1", "q29_2", "q29_3", "q29_4", "q29_5", "q29_6"))

aidata <- aidata %>%
  rename(accuracy = q29_1,
         datasell = q29_2,
         privacy = q29_3,
         labeling = q29_4,
         energy = q29_5,
         content = q29_6)

bar_single(aidata, accuracy,  xlab=NULL)
ggsave(file="plot.png", dpi=300, height=7, width=8, bg="white")

bar_single(aidata, datasell,  xlab=NULL)
ggsave(file="plot.png", dpi=300, height=7, width=8, bg="white")

bar_single(aidata, privacy,  xlab=NULL)
ggsave(file="plot.png", dpi=300, height=7, width=8, bg="white")

bar_single(aidata, labeling,  xlab=NULL)
ggsave(file="plot.png", dpi=300, height=7, width=8, bg="white")

bar_single(aidata, energy,  xlab=NULL)
ggsave(file="plot.png", dpi=300, height=7, width=8, bg="white")

bar_single(aidata, content,  xlab=NULL)
ggsave(file="plot.png", dpi=300, height=7, width=8, bg="white")


bar_multiple(aidata, 
             cat_vars = c("accuracy", "datasell", "privacy", 
                          "labeling", "energy", "content"), 
             response = "Very", 
             xlab = NULL)

ggsave(file="plot.png", dpi=300, height=7, width=11, bg="white")


#Data Sharing
aidata$aishare <- fct_recode(aidata$q30, 
                              "Not sure / no opinion" = "Not sure / no opinion",
                              "Any" = "Any data, so long as it's secure",
                              "Some" = "Some data, if it improves my experience",
                              "Minimal" = "Minimal data, only if it's needed for basic use",
                              "None" = "None at all") %>% 
  fct_relevel("None", "Minimal", "Some", "Any", "Not sure / no opinion")

bar_single(aidata, aishare, xlab=NULL)
ggsave(file="plot.png", dpi=300, height=11, width=7, bg="white")

#Updates to Data Sharing Policy
aidata$aiupdate <- fct_recode(aidata$q31, 
                             "Not sure / no opinion" = "Not sure / no opinion",
                             "Daily" = "Daily, each time a change is made",
                             "Monthly" = "Monthly, or occasionally as needed",
                             "Annually" = "Annually, or only when major changes occur",
                             "Never" = "Never") %>% 
  fct_relevel("Never", "Annually", "Monthly", "Daily", "Not sure / no opinion")

bar_single(aidata, aiupdate, xlab=NULL)
ggsave(file="plot.png", dpi=300, height=8, width=10, bg="white")


#Location of Data Storage
aidata$ailoc <- fct_recode(aidata$q32, 
                              "On my device" = "On my device - more private, but uses harddrive space and provides slower, more generic results",
                              "In the cloud" = "In the cloud - Less private, but uses no harddrive space and provides faster, highly personalized results",
                              "Not sure / no opinion" = "Not sure / no opinion") %>% 
  fct_relevel("On my device", "In the cloud", "Not sure / no opinion")

bar_single(aidata, ailoc, xlab=NULL)
ggsave(file="plot.png", dpi=300, height=7, width=11, bg="white")




#### Analysis -----------------------

#install.packages("ordinal")
library(ordinal)
library(nnet)
#install.packages("pscl")
library(pscl)
#install.packages("car")
library(car)

#Set IVs
predictors <- c("women", "age", "postsec", "bachelors", "postgrad", "income", "browjob", "student", "child", "city", "uk", "france", "germany", "japan", "india", "firefox", "edge", "safari", "otherbrowsers", "cl", "pt", "ct", "sc")
summary(aidata[predictors])

#Test for multi-collinearity
#aidata$test<-as.numeric(aidata$q25)

#model <- lm(test ~ women + age + postsec + bachelors + postgrad + income + browjob + 
#         student + child + city + uk + france + germany + japan + india + firefox + 
#            edge + safari + otherbrowsers + cl + pt + ct + sc, data = aidata)
#summary(model)
#vif(model)

#Interest in browser having AI tools
ologit(data = aidata, dependent_var = "q25", predictors = predictors)

#Specific feature interest
mlogit(data = aidata, dependent_var = "aifeat", predictors = predictors)

#AI features on next update
ologit(data = aidata, dependent_var = "aicomf", predictors = predictors)

#Opt-in vs. opt-out
mlogit(data = aidata, dependent_var = "aidefault", predictors = predictors)

#Switching
ologit(data = aidata, dependent_var = "aiswitch", predictors = predictors)

#GenAI in browser vs. separate features
mlogit(data = aidata, dependent_var = "genspec", predictors = predictors)

#Importance of labeling AI feature outputs
ologit(data = aidata, dependent_var = "labeling", predictors = predictors)

#Data collection levels
ologit(data = aidata, dependent_var = "aishare", predictors = predictors)

#Frequency of notifications about data collection
ologit(data = aidata, dependent_var = "aiupdate", predictors = predictors)

#Data location
mlogit(data = aidata, dependent_var = "ailoc", predictors = predictors)

#Frequency of notifications about data collection
ologit(data = aidata, dependent_var = "aiswitch", predictors = predictors)


#### Comparisons -----------------------

##### Interest -------------------
#Interest in AI feature integration by primary browser
bar_double(data=aidata, cat_var1=r_browser_simple, cat_var2 = q25, xlab=NULL)
ggsave(file="plot.png", dpi=300, height=10, width=11, bg="white")

#Interest in AI Features by Age
boxplot_with_jitter(aidata, "age", "q25", xlab=NULL, ylab="Age")
ggsave(file="plot.png", dpi=300, height=11, width=9, bg="white")

#Interest by gender
bar_double(data=aidata, cat_var1=gender, cat_var2 = q25, gender=="Man" | gender=="Woman", xlab=NULL)
ggsave(file="plot.png", dpi=300, height=10, width=11, bg="white")

#Interest by area
bar_double(data=aidata, cat_var1=q7.1, cat_var2 = q25, xlab=NULL)
ggsave(file="plot.png", dpi=300, height=9, width=14, bg="white")

#Interest by occupation
bar_double(data=aidata, cat_var1=occ, cat_var2 = q25, xlab=NULL)
ggsave("plot.png", dpi=300, height=5, width=12, bg="white")

#Interest by generation
bar_double(data=aidata, cat_var1=generation, cat_var2 = q25, generation!="Silent", xlab=NULL)
ggsave(file="plot.png", dpi=300, height=6, width=10, bg="white")

#Interest by country
bar_double(data=aidata, cat_var1=q3, cat_var2 = q25, xlab=NULL)
ggsave(file="plot.png", dpi=300, height=9, width=14, bg="white")

#Interest by segment
bar_single_breakout_segment(data = aidata, cat_var = q25, breakdown_var = segment, xlab = NULL)
ggsave(file="plot.png", dpi=300, height=12, width=9, bg="white")

ggsave(file="plot.png", dpi=300, height=8, width=12, bg="white")

#GenAI or specialzied AI features by primary browser
bar_double(data=aidata, cat_var1=r_browser_simple, cat_var2 =  genspec)
ggsave(file="plot.png", dpi=300, height=8, width=12, bg="white")

#GenAI or specialzied AI features by primary browser
bar_double(data=aidata, cat_var1=occ, cat_var2 = aifeat, occ== "Browser job" | occ== "Non-browser job", xlab=NULL)
ggsave(file="plot.png", dpi=300, height=8, width=12, bg="white")


##### Privacy -------------------------
#Level of comfort with AI features being integrated on next update, by primary browser
bar_double(data=aidata, cat_var1=r_browser_simple, cat_var2 = aicomf, xlab=NULL)
ggsave(file="plot.png", dpi=300, height=7, width=11, bg="white")

#Opting-in vs. opting-out by primary browser
bar_double(data=aidata, cat_var1=r_browser_simple, cat_var2 = aidefault, aidefault!="Neither", xlab=NULL)
ggsave(file="plot.png", dpi=300, height=8, width=13, bg="white")

#Interest in AI features in browser, by tolerance for data collection
bar_double(data=aidata, cat_var1=q25, cat_var2 = aishare, xlab=NULL)
ggsave(file="plot.png", dpi=300, height=6, width=9, bg="white")

#Where do users prefer data to be stored and processed, by primary browser
bar_double(data=aidata, cat_var1=r_browser_simple, cat_var2 = ailoc, xlab=NULL)
ggsave(file="plot.png", dpi=300, height=6, width=11, bg="white")


##### Transparency --------------------
#Labeling AI outputs by primary browser
bar_double(data=aidata, cat_var1=r_browser_simple, cat_var2 = labeling, xlab=NULL)
ggsave(file="plot.png", dpi=300, height=6, width=10, bg="white")

#Labeling AI outputs by segment
bar_single_breakout_segment(data = aidata, cat_var = labeling, breakdown_var = segment, xlab = NULL)
ggsave(file="plot.png", dpi=300, height=12, width=9, bg="white")


##### Outcomes ------------------------
#Switching browser by occupation
bar_double(data=aidata, cat_var1=occ, cat_var2 = aiswitch, occ=="Browser job" | occ=="Non-browser job" | occ=="Student", xlab=NULL)
ggsave(file="plot.png", dpi=300, height=7, width=10, bg="white")

#Switching by browser
bar_double(data=aidata, cat_var1=r_browser_simple, cat_var2 = aiswitch, xlab=NULL)
ggsave(file="plot.png", dpi=300, height=7, width=10, bg="white")


#Labeling AI outputs by segment
bar_single_breakout_segment(data = aidata, cat_var = aiupdate, breakdown_var = segment, xlab = NULL)
ggsave(file="plot.png", dpi=300, height=12, width=9, bg="white")

bar_single_breakout_segment(data = aidata, cat_var = gender, breakdown_var = segment, gender!= "Don't describe me", xlab = NULL)
ggsave(file="plot.png", dpi=300, height=12, width=9, bg="transparent")

### Other Plots -----------------------

#install.packages("maps")
library(maps)
world_map <- map_data("world")

country_scores <- aidata %>%
  group_by(q3) %>%  
  summarise(score = mean(as.numeric(q25), na.rm = TRUE)) 

country_scores <- country_scores %>%
  mutate(q3 = fct_recode(as.factor(q3),
                         "USA" = "United States",
                         "UK" = "United Kingdom"))

countries <- c("USA", "UK", "France", "Germany", "India", "Japan")

map_data <- map_data("world") %>%
  filter(region %in% countries & !(subregion %in% c("Alaska", "Hawaii"))) %>%
  left_join(country_scores, by = c("region" = "q3"))

average_scores <- map_data %>%
  group_by(region) %>%
  summarise(
    avg_score = mean(score, na.rm = TRUE),
    long = mean(range(long)),
    lat = mean(range(lat))
  )

ggplot(map_data, aes(x = long, y = lat, group = group, fill = score)) +
  geom_polygon(color = "black", size = 0.2) +
  geom_text(
    data = average_scores,
    aes(x = long, y = lat, label = sprintf("%.1f", avg_score)),
    inherit.aes = FALSE, 
    color = "red", 
    fontface = "bold", 
    size = 8, 
    vjust = -1,
    hjust = 0.5
  ) +
  facet_wrap(~region, scales = "free", ncol=2) +
  scale_fill_viridis_c(option = "B", direction = 1, na.value = "gray90") +
  theme_minimal() +
  labs(fill = "Interest in AI Browser Features") +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1, "lines"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(file="plot.png", dpi=300, height=14, width=9, bg="white")
