#Chance York--------------------
#March 11, 2025-----------------
#Early Journeys Survey----------

####1: Import ----
#source functions
source("/users/chance/desktop/nur/functions.R")

#merge telemetry and clean
nur <- pii("desktop_orig.csv", "desktop.csv")
nur <- process(nur, "mobile.csv")

#parse vars 
nur <- parse(nur, datetime_col = "date_submitted", 
             version_col = "firefox_version", 
             engine_col = "search_engine")

#drop obs with dates prior to first day in field
nur <- nur %>%
  filter(date_submitted >= "2025-03-27")

#cleans activity dates & calculates active hours on specific days and rolling averages across weeks
nur <- timevars(nur) 
nur <- rolling(nur)


####2: QC ----

#click-to-complete rate
completion(nur, response_id, email)

#start-to-complete rate
completion(nur, past_use_fx, email)

#falloff 
falloff(nur, bg="white",
        "Intro" = "response_id",
        "Page 2" = "past_use_fx",
        "Page 3" = "down_loc",
        "Page 4" = "why_fx",
        "Page 5" = "why_fx_main",
        "Page 6" = "intent_fx",
        "Page 7" = "onboard_complete",
        "Page 8" = "customize_fx",
        "Page 9" = "moz_accts",
        "Demos" = "email")

#survey mode
nur <- nur %>% 
  mutate(app = case_when(
    app == "abc123" ~ NA_character_,
    app == "android" ~ "Android",
    app == "ios" ~ "iOS",
    tolower(app) == "desktop" | is.na(app) ~ "Desktop",
    TRUE~app))
bar(nur, app, xlab="Survey Platform")

#survey duration
hist(nur, survey_duration, xlab="Survey Duration (in seconds)")

ggplot(nur, aes(x = survey_duration)) +
  geom_boxplot() +
  scale_x_continuous(labels = scales::comma)

nur <- nur %>% 
  filter(survey_duration >= 20 & survey_duration <= 7200)


#page timer
hist(nur, page_timer, xlab="Page Timer (in seconds)")

ggplot(nur, aes(x = page_timer)) +
  geom_boxplot() +
  scale_x_continuous(labels = scales::comma)

nur <- nur %>% 
  filter(survey_duration >= 3)


#duplicate ids
nur <- nur %>%
  mutate(duplicate_id = duplicated(shield_id) | 
           duplicated(shield_id, fromLast = TRUE))




####3: Descriptives ----

##### Demos ------------
#gender
bar(nur, gender, xlab="Gender", order = 'desc')

#age
nur <- nur %>%
  mutate(
    age = if_else(
      as.numeric(dob) < 1930 | as.numeric(dob) > 2010, 
      NA_real_, 2025 - as.numeric(dob)))
hist(nur, age, xlab="Age (in years)")

#generation
nur <- nur %>%
  mutate(
    gen = case_when(
      age >= 77 ~ "Silent",       
      age >= 59 & age <= 76 ~ "Boomer", 
      age >= 43 & age <= 58 ~ "Gen X",     
      age >= 27 & age <= 42 ~ "Millennial",   
      age >= 11 & age <= 26 ~ "Gen Z",       
      TRUE ~ NA_character_) %>% 
      factor(levels = c("Silent", "Boomer", "Gen X", "Millennial", "Gen Z")))
bar(nur, gen, xlab="Generational Cohort", order="original")

#occupation
bar(nur, occupation, xlab="Occupation")

#kids
bar(nur, children, xlab="Kids in Household")

#country
nur <- nur %>%
  mutate(
    geo = case_when(
      is.na(country) ~ NA_character_,
      country == "United States" ~ "USA",
      country == "France" ~ "France",
      country == "Germany" ~ "Germany",
      country == "Japan" ~ "Japan",
      TRUE ~ "Other"),
    geo = factor(geo, levels = c("USA", "Germany", "France", "Japan", "Other")))
bar(nur, geo, xlab = "Geographic Location", order = "original")

#email
bar(nur, email, xlab="Provided Email Address", order="asc")

#locale
bar(nur, locale, xlab="locale", order="desc")

#survey language
bar(nur, language, xlab="locale", order="desc")


##### Mediators ------------

###### behavioral -----------
nur<-nur %>% 
  mutate(default_browser = case_when(
    default_browser == "abc123" ~ NA,
    default_browser == "1" ~ "Yes",
    default_browser == "0" ~ "No"))
bar(nur, default_browser, xlab="Telemetry: Firefox is Default Browser on Device", order="desc")

#fx version
bar(nur, firefox_version, xlab="Telemetry: Version", order="asc")

#fx sync
nur<-nur %>% 
  mutate(sync_setup = case_when(
    sync_setup == "abc123" ~ NA,
    sync_setup == "1" ~ "Yes",
    sync_setup == "0" ~ "No"))
bar(nur, sync_setup, xlab="Telemetry: Sync", order="asc")

#search engine
bar(nur, search_engine, search_engine!="Yahoo" & search_engine!="Other", xlab="Telemetry: Search Engines", order="desc")


###### survey -----------

#user type
bar(nur, past_use_fx, xlab="Type of User")

#[returning users] time since last used Fx
nur<-nur %>% mutate(
  last_use_fx=factor(last_use_fx, levels=
                       c("Less than a year",
                         "1 or 2 years", 
                         "3 or 4 years",
                         "5 years or more",
                         "Not sure")))
bar(nur, last_use_fx, xlab="[Returning Users] Time Since Last Used Firefox", order="original")


#where downloaded Fx
nur <- nur %>% 
  mutate(down_loc = factor(down_loc, levels = c(
    "App store", 
    "3rd-party site", 
    "Device setup (DMA)", 
    "Didn't install", 
    "Employer link", 
    "Firefox site", 
    "Not sure", 
    "Somewhere else"
  )))
bar(nur, down_loc, xlab="Firefox Download Location")


#motivations for Fx install 
multi(nur, why_fx, xlab="Reasons for Installing Firefox on Current Device")
corr(nur,  why_fx, title="Reasons for Installing Firefox on Current Device", method="tetra")


#main motivation [from multi-select]
nur <- nur %>%
  mutate(why_fx_main = str_replace_all(why_fx_main, "_", " "))
bar(nur, why_fx_main, xlab="Primary Reason Why User Downloaded/Installed Firefox on Device")

##### Outcomes ------------

###### day 0 -----------

#plan to use Fx type
nur<-nur %>% 
  mutate(prim_vs_sec = case_when(
    prim_vs_sec == "Main" ~ "Main browser",
    prim_vs_sec == "Secondary" ~ "Secondary browser",
    prim_vs_sec == "Not sure yet" ~ "Not sure yet"),
    prim_vs_sec=factor(prim_vs_sec, levels=c("Main browser",
                                "Secondary browser", 
                                "Not sure yet")))
bar(nur, prim_vs_sec, xlab="How User Plans to Use Firefox", order="original")

#behavioral intent to use Fx
nur<-nur %>% 
  mutate(intent_fx=factor(intent_fx, levels=c(
      "Very unlikely", "Unlikely", "Neutral / not sure", "Likely", "Very likely")))
bar(nur, 
    intent_fx, 
    xlab="Perceived Likelihood of Using Firefox 30 Days from Survey Date", 
    order="original")


###### onboarding -----------
#completion
bar(nur, onboard_complete, app=="Desktop", xlab="First Run Onboarding Completion")

#attention to screens to onboarding screens [users who completed onboarding]
nur<-nur %>% 
  mutate(onboard_attn =factor(onboard_attn, 
                         levels = c("No attention at all", "Some attention",
                                    "Moderate attention", "Close attention")))
bar(nur, onboard_attn, app=="Desktop", xlab="[Of Completed] Attention to Onboarding Screens", order="original")


#reasons for skipping
multi(nur, onboard_skip, app== "Desktop", xlab = "[Of Skipped] Reasons Users Skipped First-Run Onboarding")
corr(nur, onboard_skip, app=="Desktop", title="Correlations of Reasons Users Skipped First-Run Onboarding", method="tetra")


###### p13n -----------

#early user customization
bar(nur, customize_fx, xlab="Early Journey Customization")

#types of early journey customization
multi(nur, custom, xlab="Types of Early Journey Firefox Customization")
corr(nur, custom, title="Correlations of Early Journey Customization Behaviors", method="tetra")

#frequency of accepting app defaults
nur<-nur %>% 
  mutate(keep_defaults =factor(keep_defaults, 
                              levels = c("Never",
                                         "Rarely",
                                         "Sometimes",
                                         "Often",
                                         "Always")))
bar(nur, keep_defaults, xlab= "Frequency of Keeping New App Defaults", order="original")


###### accounts -----------

#signed into accounts
nur<-nur %>% 
  mutate(accts = case_when(
    moz_accts == "Yes, I am" ~ "Signed-in",
    moz_accts == "No, I'm not" ~ "Signed-out",
    moz_accts == "Not sure" ~ "Not sure"),
    moz_accts=factor(accts, levels=c("Signed-in",
                                             "Signed-out", 
                                             "Not sure")))
bar(nur, accts, xlab="User's Mozilla Account Sign-in Status")


#specific non-Firefox Mozilla products used
multi(nur, moz_prods, app=="Desktop", xlab="Mozilla Products Used in Addition to Firefox")
corr(nur, moz_prods,  app=="Desktop", title="Correlations of Mozilla Products Used", method="tetra")
nur<-count(nur, moz_prods)
multi(nur, moz_prods, app=="Desktop", xlab="Mozilla Products Used in Addition to Firefox", height=9, width=7)
nur<-factormulti(nur, moz_prods)

####4:Analysis  ---------


#code some telemtry flags as binaries
nur <- flags(nur, fxa_configured)
nur <- flags(nur, sync_configured)
nur <- flags(nur, is_default_browser)

nur <- nur %>%
  mutate(
    google = case_when(
      search_engine == "Google" ~ 1,
      TRUE ~ 0
    )
  )


nur <- nur %>%
  mutate(
    medianage = case_when(
      age < 42 ~ "Under 42",
      age >= 42 ~ "42 or over",
      TRUE ~ NA_character_ ) )


nur <- nur %>%
  mutate(os_recode = case_when(
    os == "Windows_NT" ~ "Windows",
    os == "Darwin"     ~ "Mac",
    os == "Linux"      ~ "Linux",
    TRUE               ~ NA_character_
  ))


#set model predictors and excluded reference groups
preds_all<- c("gender", "age", "children",
              "is_default_browser")


ref_complete <- list(
  gender = "Man",
  children = "Has kids"
  )


#### Survival -----------
#install.packages("survminer")
#install.packages("forestplot")

#use survival package to clean data for analysis
nur_long <- survival(nur, churn_window = 3)

nur_long %>% 
  group_by(is_default_browser) %>% 
  tally()

#plot overall survival rates
plot_survival_km(nur_long, palette = mountain, height=7, width=10)

#fit a cox model and plot hazard ratios with a forest plot
fit_cox_model(nur_long, preds_all, ref_complete, height=9, width=6)

#check & plot specific group differences
plot_survival_by_group(nur_long, group_var = "gender", group_levels = c("Man", "Woman"), height=7, width=10)
plot_survival_by_group(nur_long, group_var = "geo", group_levels = c("USA", "Japan"), height=7, width=10)
plot_survival_by_group(nur_long, group_var = "children", group_levels = c("Has kids", "No kids"), height=7, width=10)
plot_survival_by_group(nur_long, group_var = "prim_vs_sec", group_levels = c("Main browser", "Secondary browser", "Not sure yet"), height=7, width=10)
plot_survival_by_group(nur_long, group_var = "why_fx_main", group_levels = c("Fixing", "Unhappy brow"), height=7, width=10)
plot_survival_by_group(nur_long, group_var = "down_loc", group_levels = c("Firefox site", "Device setup (DMA)"), height=7, width=10)
plot_survival_by_group(nur_long, group_var = "customize_fx", group_levels = c("Yes, I customized it right away", "No, I kept the default settings"), height=7, width=10)



#### Regressions --------

preds_key<- c("gender", "age", "occupation", "children", "geo", 
              "moz_prods_factor")

ref_key <- list(
  gender = "Man",
  occupation = "Browser job",
  children = "Has kids",
  geo = "USA",
  moz_prods_factor = "None")



#what predicts dropping at certain time period, e.g., present_day2, present_week2, present_week4
model <- logit(data = nur, dependent_var = "present_day2", predictors = preds_key, ref_groups = ref_key)
plot_probs(model$predicted_probs_table, moz_prods_factor, None, x_label = "Mozilla Products Used", width = 10, height = 6)


model <- ols(data = nur, dependent_var = "active_day2", predictors = preds_all, ref_groups = ref_complete)
plot_pr(model$summary_table, prim_vs_sec, "Secondary browser", width = 10, height = 6)




#### Paths --------

#set model predictors and reference groups
nur <- nur %>%
  mutate(
    completed_onboard = ifelse(onboard_complete == "Yes, I completed it", 1, 0),
    new_user = ifelse(past_use_fx == "New user", 1, 0),
    returning_user = ifelse(past_use_fx == "Returning user", 1, 0),
    onboard_new = completed_onboard * new_user,
    onboard_return = completed_onboard * returning_user)


nur <- nur %>%
  mutate(
    onboard_attn = factor(onboard_attn, levels = c(
      "No attention at all", "Some attention", "Moderate attention", "Close attention"
    )),
    onboard_level = as.numeric(onboard_attn)
  )


preds_key<- c("gender", "age", "occupation", "children", "geo", 
              "onboard_level")

ref_key <- list(
  gender = "Man",
  occupation = "Browser job",
  children = "Has kids",
  geo = "USA")


#####Onboarding ----

fit <- path(
  df = nur,
  preds_key = c(),
  ref_key = list(),
  outcome = "present_week2",
  mediators = c("present_day2"),
  primary_iv = c("completed_onboard", "new_user", "returning_user"),
  mediation_paths = c("completed_onboard ~ new_user",
                      "completed_onboard ~ returning_user",
                      "present_day2 ~ completed_onboard",
                      "present_day2 ~ new_user",
                      "present_day2 ~ returning_user",
                      "present_week2 ~ present_day2",
                      "present_week2 ~ completed_onboard",
                      "present_week2 ~ completed_onboard",
                      "present_week2 ~ new_user"))


fit <- path(
  df = nur,
  preds_key = c(),
  ref_key = list(),
  outcome = "present_week2",
  mediators = c("present_day2"),
  primary_iv = c("completed_onboard", "new_user", "returning_user"),
  mediation_paths = c("completed_onboard ~ new_user",
                      "completed_onboard ~ returning_user",
                      "present_day2 ~ completed_onboard",
                      "present_day2 ~ new_user",
                      "present_day2 ~ returning_user",
                      "present_week2 ~ present_day2",
                      "present_week2 ~ completed_onboard",
                      "present_week2 ~ completed_onboard",
                      "present_week2 ~ new_user"),
  bootstrap = TRUE,
  n_bootstrap = 1000)

#alternate specification
fit <- path(
  df = nur,
  preds_key = c("gender", "age", "occupation", "children", "geo"),
  ref_key = list("Man", "Browser job", "Has kids", "USA"),
  outcome = "present_week2",
  mediators = c("present_day2"),
  primary_iv = "completed_onboard",
  group_by = "past_use_fx",
  mediation_paths = c("present_day2 ~ completed_onboard", 
                      "present_week2 ~ present_day2",
                      "present_week2 ~ completed_onboard"))


fit <- path(
  df = nur,
  preds_key = c(),
  ref_key = list(),
  outcome = "present_week2",
  mediators = c("present_day2"),
  primary_iv = c("onboard_level", "new_user", "returning_user"),
  mediation_paths = c("onboard_level ~ new_user",
                      "onboard_level ~ returning_user",
                      "present_day2 ~ onboard_level",
                      "present_day2 ~ new_user",
                      "present_day2 ~ returning_user",
                      "present_week2 ~ present_day2",
                      "present_week2 ~ onboard_level",
                      "present_week2 ~ onboard_level",
                      "present_week2 ~ new_user"))


fit <- path(
  df = nur,
  preds_key = c(),
  ref_key = list(),
  outcome = "present_week2",
  mediators = c("present_day2"),
  primary_iv = c("onboard_level", "new_user", "returning_user"),
  mediation_paths = c("onboard_level ~ new_user",
                      "onboard_level ~ returning_user",
                      "present_day2 ~ onboard_level",
                      "present_day2 ~ new_user",
                      "present_day2 ~ returning_user",
                      "present_week2 ~ present_day2",
                      "present_week2 ~ onboard_level",
                      "present_week2 ~ onboard_level",
                      "present_week2 ~ new_user"),
  bootstrap = TRUE,
  n_bootstrap = 1000)




###### Plots ----------

bars(nur, past_use_fx, app, height = 6, width = 10, order = "desc", xlab = "Type of User by Firefox App")

bars(nur, why_fx_main, app, height = 6, width = 10, order = "desc", xlab = "Main Reason Why User Downloaded/Installed Firefox on Device by Firefox App")

bars(nur, down_loc, app, xlab="Firefox Download Location")

#reason by user type
multi(nur, why_fx_main, past_use_fx=="Continuing user",  height = 7, width = 4, order = "desc", xlab = "Main Reason Why CONTINUING User Downloaded/Installed Fx")
multi(nur, why_fx_main, past_use_fx=="New user",  height = 7, width = 4, order = "desc", xlab = "Main Reason Why NEW User Downloaded/Installed Fx")
multi(nur, why_fx_main, past_use_fx=="Returning user", height = 7, width = 4, order = "desc", xlab = "Main Reason Why RETURNING User Downloaded/Installed Fx")

#Reasons for creating new profile, by platform and user type
multi(nur, why_fx_main, app=="Desktop" & past_use_fx=="Continuing user", height = 7, width = 4, order = "desc", xlab = "Main Reason Why User Downloaded/Installed Fx on DESKTOP")
multi(nur, why_fx_main, app=="Desktop" & past_use_fx=="Returning user", height = 7, width = 4, order = "desc", xlab = "Main Reason Why User Downloaded/Installed Fx on DESKTOP")
multi(nur, why_fx_main, app=="Desktop" & past_use_fx=="New user", height = 7, width = 4, order = "desc", xlab = "Main Reason Why User Downloaded/Installed Fx on DESKTOP")

multi(nur, why_fx_main, app=="Android" & past_use_fx=="Continuing user", height = 7, width = 4, order = "desc", xlab = "Main Reason Why User Downloaded/Installed Fx on ANDROID")
multi(nur, why_fx_main, app=="Android" & past_use_fx=="Returning user", height = 7, width = 4, order = "desc", xlab = "Main Reason Why User Downloaded/Installed Fx on ANDROID")

multi(nur, why_fx_main, app=="iOS" & past_use_fx=="Continuing user", height = 7, width = 4, order = "desc", xlab = "Main Reason Why User Downloaded/Installed Fx on IOS")
multi(nur, why_fx_main, app=="iOS" & past_use_fx=="Returning user", height = 7, width = 4, order = "desc", xlab = "Main Reason Why User Downloaded/Installed Fx on IOS")
multi(nur, why_fx_main, app=="iOS" & past_use_fx=="New user", height = 7, width = 4, order = "desc", xlab = "Main Reason Why User Downloaded/Installed Fx on IOS")

bars(nur, why_fx_main, app, past_use_fx=="Continuing user", xlab="[Continuing Users] Main Reason for Installing Fx by Device Type")
