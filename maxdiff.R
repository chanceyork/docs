### Feature MaxDiff Project 
### Written by Chance York 
### July 24, 2025 

####Functions ----
source("/Users/chance/Desktop/maxdiff/maxdiff_functions.R")

#### Import ----
survey_cint<-read.csv2("/Users/chance/Desktop/maxdiff/datasets/survey_cint.csv", sep = ",", na.strings = "")
maxdiff_cint_desktop<-read.csv2("/Users/chance/Desktop/maxdiff/datasets/maxdiff_cint_desktop.csv", sep = ",", na.strings = "")
maxdiff_cint_mobile<-read.csv2("/Users/chance/Desktop/maxdiff/datasets/maxdiff_cint_mobile.csv", sep = ",", na.strings = "")
survey_desktop <-read.csv2("/Users/chance/Desktop/maxdiff/datasets/survey_desktop.csv", sep = ",", na.strings = "")
maxdiff_desktop<-read.csv2("/Users/chance/Desktop/maxdiff/datasets/maxdiff_desktop.csv", sep = ",", na.strings = "")
survey_mobile<-read.csv2("/Users/chance/Desktop/maxdiff/datasets/survey_mobile.csv", sep = ",", na.strings = "")
maxdiff_mobile<-read.csv2("/Users/chance/Desktop/maxdiff/datasets/maxdiff_mobile.csv", sep = ",", na.strings = "")

#### Setup ----
today.date <- Sys.Date() 

colors <- rep(c("#FCE762", "#FFFDED", "#FFB17A", "#4F4789", "#201355"), 10)
palette(colors)

source_colors <- c(
  "General Browser Users" = "#FCE762", 
  "Firefox Desktop" = "#4F4789", 
  "Firefox Mobile" = "#FFB17A" )

#### Clean ----
#Clean surveys

survey_cint <- clean.survey(survey_cint)
survey_desktop <- clean.survey(survey_desktop)
survey_mobile <- clean.survey(survey_mobile)

#Clean MaxDiffs
maxdiff_cint_desktop <- clean.maxdiff(maxdiff_cint_desktop)
maxdiff_cint_mobile <- clean.maxdiff(maxdiff_cint_mobile)
maxdiff_desktop <- clean.maxdiff(maxdiff_desktop)
maxdiff_mobile <- clean.maxdiff(maxdiff_mobile)

#Add a 'source' column to each survey df
survey_cint <- survey_cint %>% mutate(source = "General Browser Users")
survey_desktop <- survey_desktop %>% mutate(source = "Firefox Desktop")
survey_mobile <- survey_mobile %>% mutate(source = "Firefox Mobile")

#### Cint IDs ---------------------
source("/Users/chance/Desktop/maxdiff/maxdiff_completes.R")


### Telemetry ----
source("/Users/chance/Desktop/maxdiff/clean_telemetry.R")

### Joins ----
survey_all <- bind_rows(
  survey_desktop %>% mutate(source = "Firefox Desktop"),
  survey_mobile  %>% mutate(source = "Firefox Mobile"),
  survey_cint    %>% mutate(source = "General Browser Users"))

#Integrate dates in single column
#survey_all <- survey_all %>%
 # mutate(date_submitted = coalesce(extracted_date, date_submitted))

#Reformat all date types
survey_all <- survey_all %>%
  mutate(date_submitted = parse_date_time(date_submitted, orders = c("ymd", "mdy HM", "mdy HMS p"), tz = "UTC"),
    date_submitted = format(date_submitted, format = "%b %d, %Y"))

#Add a 'source' column to each maxdiff df
maxdiff_cint_desktop <- maxdiff_cint_desktop %>% mutate(source = "General Browser Users")
maxdiff_cint_mobile <- maxdiff_cint_mobile %>% mutate(source = "General Browser Users")
maxdiff_desktop <- maxdiff_desktop %>% mutate(source = "Firefox Desktop")
maxdiff_mobile <- maxdiff_mobile %>% mutate(source = "Firefox Mobile")

#Join Maxdiffs
maxdiff_all <- bind_rows(maxdiff_cint_desktop, maxdiff_desktop, maxdiff_cint_mobile, maxdiff_mobile)

### Recodes ----
source("/Users/chance/Desktop/maxdiff/maxdiff_recodes.R")

### Drops ----
source("/Users/chance/Desktop/maxdiff/maxdiff_drops.R")


### Descriptives ----
#### Demos ----
##Add filter variable as needed to each call, eg: barplot(survey_all source, country=="Germany")

#Survey Data Sources
bar(survey_all, source, xlab="Survey Audience", width=9, height=5)

# Gender
bar(survey_all, gender, xlab="Gender", source=="General Browser Users", width=5)
bar(survey_all, gender, xlab="Gender", source=="Firefox Desktop", width=5)
bar(survey_all, gender, xlab="Gender", source=="Firefox Mobile", width=5)

# Age
hist(survey_all, age, xlab = "Age in Years", source == "General Browser Users", width=5)
hist(survey_all, age, xlab = "Age in Years", source == "Firefox Desktop", width=5)
hist(survey_all, age, xlab = "Age in Years", source == "Firefox Mobile", width=5)

# Generation
bar(survey_all, generations, xlab="Generation", source=="General Browser Users", order="original", width=5)
bar(survey_all, generations, xlab="Generation", source=="Firefox Desktop", order="original", width=5)
bar(survey_all, generations, xlab="Generation", source=="Firefox Mobile", order="original", width=5)

# Occupation
bar(survey_all, occ, xlab="Occupation", source=="General Browser Users", flip_coords = TRUE, order="asc", width=4)
bar(survey_all, occ, xlab="Occupation", source=="Firefox Desktop", flip_coords = TRUE, order="asc", width=4)
bar(survey_all, occ, xlab="Occupation", source=="Firefox Mobile", flip_coords = TRUE, order="asc", width=4)

# Kids
bar(survey_all, kids, xlab="Children", source=="General Browser Users", width=5)
bar(survey_all, kids, xlab="Children", source=="Firefox Desktop", width=5)
bar(survey_all, kids, xlab="Children", source=="Firefox Mobile", width=5)

# Area
bar(survey_all, area, xlab="Area of Residence", source=="General Browser Users", width=5)
bar(survey_all, area, xlab="Area of Residence", source=="Firefox Desktop", width=5)
bar(survey_all, area, xlab="Area of Residence", source=="Firefox Mobile", width=5)

# Country
bar(survey_all, country, xlab="Country of Residence", source=="General Browser Users", width=5)
bar(survey_all, country, xlab="Country of Residence", source=="Firefox Desktop", width=5)
bar(survey_all, country, xlab="Country of Residence", source=="Firefox Mobile", width=5)


#### Psychos ---
#Data Privacy / P13n tradeoff
bar(survey_all, comfort, order="original", source=="General Browser Users", xlab="Level of Agreement", width=6)
bar(survey_all, comfort, order="original", source=="Firefox Desktop", xlab="Level of Agreement", width=6)
bar(survey_all, comfort, order="original", source=="Firefox Mobile", xlab="Level of Agreement", width=6)

#Openness to Experience
bar(survey_all, vivid, order="original", xlab="Level of Agreement", width=5)
bar(survey_all, variety, order="original", xlab="Level of Agreement", width=5)
 #combined numeric measure of vivid+variety
hist(survey_all, openness, xlab="Average Openness to Experience Score", width = 6)

#Maximizing Personality
bar(survey_all, standards, order="original", xlab="Level of Agreement", width =5)
bar(survey_all, surf, order="original", xlab="Level of Agreement", width = 5)
bar(survey_all, gift, order="original", xlab="Level of Agreement", width = 5)


#### Devices ---
#Frequency of use
bar(survey_all, lap_freq, order="original", xlab="Frequency of using LAPTOP computer")

bar(survey_all, desk_freq, order="original", xlab="Frequency of using DESKTOP computer")

bar(survey_all, mob_freq, order="original", xlab="Frequency of using SMARTPHONE")

bar(survey_all, tab_freq,order="original", xlab="Frequency of using TABLET")

#Mobile-Desktop Orientation
bar(survey_all, device_usage, order="original", device_usage!="Other or infrequent user")


#### Desktop ---
#Desktop OS
bar(survey_all, deskos, xlab="Self-Reported DESKTOP OS", source=="General Browser Users", width=5)
bar(survey_all, deskos, xlab="Self-Reported DESKTOP OS", source=="Firefox Desktop", width=5)

#Desktop Primary Browser
bar(survey_all, deskbrow, source=="General Browser Users", xlab="Primary DESKTOP Browser", width=5)
bar(survey_all, deskbrow, source=="Firefox Desktop", xlab="Primary DESKTOP Browser", width=5)

#Desktop Browser Frequency
bar(survey_all, freqdesk, order="original", xlab="Frequency of Using DESKTOP Browser")

#Ever Use Firefox Desktop
bar(survey_all, everdesk, order="original", xlab= "Ever use Firefox desktop", width=6.5)


#### Mobile ---
#Mobile OS
bar(survey_all, mobos, xlab="Self-Reported MOBILE OS", source=="General Browser Users", width=5)
bar(survey_all, mobos, xlab="Self-Reported MOBILE OS", source=="Firefox Mobile", width=5)

#Mobile Primary Browser
bar(survey_all, mobbrow, source=="General Browser Users", xlab="Primary MOBILE Browser", width=5)
bar(survey_all, mobbrow, source=="Firefox Mobile", xlab="Primary MOBILE Browser", width=5)

#Mobile Browser frequency
bar(survey_all, freqmob, order="original", xlab="Frequency of Using MOBILE Browser")

#Ever use Firefox mobile
bar(survey_all, evermob, order="original", xlab= "Ever use Firefox mobile", width=6.5)


#### Demo Comparisons ----
#Descriptive differences among sample demos
point_double(survey_all, gender, source)

point_double(survey_all, genz, source)

point_double(survey_all, occ, source)

point_double(survey_all, deskos, source)

point_double(survey_all, mobos, source)

point_double(survey_all, deskbrow, source)

point_double(survey_all, mobbrow, source)


### MaxDiff Analysis  ----
maxdiff_scores<- maxdiff_props(maxdiff_all)
maxdiff_summary<-maxdiff_sum(maxdiff_scores)
maxdiff_between <- maxdiff_anova(maxdiff_all)
maxdiff_within <- maxdiff_pairwise(maxdiff_scores)

#### Feature Responses ---
max_stacked(maxdiff_summary, n_top=30, height=11, width=7)
max_stacked(maxdiff_summary, selected_source = "General Browser Users", n_top=30, height=11, width=7)
max_stacked(maxdiff_summary, selected_source = "Firefox Desktop", n_top=30, height=11, width=7)
max_stacked(maxdiff_summary, selected_source = "Firefox Mobile", n_top=30, height=11, width=7)

#### Raw Counts ---
max_raw(maxdiff_scores, n_top = 30, height=11, width=7.5)
max_raw(maxdiff_scores, selected_source = "General Browser Users", n_top = 30, height=11, width=7)
max_raw(maxdiff_scores, selected_source = "Firefox Desktop", n_top = 30, height=11, width=7)
max_raw(maxdiff_scores, selected_source = "Firefox Mobile", n_top = 30, height=11, width=7)

#### Most Wanted Features ---
max_percent(maxdiff_summary, metric = "pct_best", n_top=10)

max_percent(maxdiff_summary, metric = "pct_best", source_filter = "Firefox Mobile", n_top=10)

#### Least Wanted Features ---
max_percent(maxdiff_summary, metric = "pct_worst", source_filter = "General Browser Users", n_top= 8)

#### Diverging bars ---
max_diverging(maxdiff_summary, score_limits = c(-0, .5), order="desc", n_top = 12, height=9, width=6)

max_diverging(maxdiff_summary, sources = c("General Browser Users"), score_limits = c(-0.1, .5), order="desc", n_top = 15, height=9, width=6)
max_diverging(maxdiff_summary, sources = c("Firefox Desktop"), score_limits = c(-0.1, .5), order="desc", n_top = 15, height=9, width=6)
max_diverging(maxdiff_summary, sources = c("Firefox Mobile"), score_limits = c(-0.1, .5), order="desc",  n_top = 15, height=9, width=6)

max_diverging(maxdiff_summary, sources = c("General Browser Users", "Firefox Desktop", "Firefox Mobile"), score_limits = c(-.6, .6), n_top = 30)


max_diverging(maxdiff_summary, 
              sources = c("General Browser Users", "Firefox Desktop", "Firefox Mobile"), 
              attributes=c("Hotkey Customization", "Live Crop", "Setup Sharing"), 
              score_limits = c(-.2,.2), height=11, width=9, n_top = 30, legend=T, order="asc")

max_diverging(maxdiff_summary, 
              sources = c("General Browser Users", "Firefox Desktop", "Firefox Mobile"), 
              attributes=c("Answer Engine", "AI Do-It-For-You", "AI Mode", "Second Opinion", "Smart Compare"), 
              score_limits = c(-.4,.4), height=11, width=9, n_top = 30, legend=T, order="asc")

max_diverging(maxdiff_summary, score_limits = c(-.6, .6), order="desc", height=11, width=6)
max_diverging(maxdiff_summary, sources = c("General Browser Users"), order="desc", score_limits = c(-.6, .6), height=11, width=6)
max_diverging(maxdiff_summary, sources = c("Firefox Desktop"), order="desc", score_limits = c(-.6, .6), height=11, width=6)
max_diverging(maxdiff_summary, sources = c("Firefox Mobile"), order="desc", score_limits = c(-.6, .6), height=11, width=6)

max_diverging(maxdiff_summary,score_limits = c(-.6, 0), sources = c("General Browser Users"), n_bottom=10, order="desc", height=9, width=6)
max_diverging(maxdiff_summary,score_limits = c(-.6, 0), sources = c("Firefox Desktop"), n_bottom=10, order="desc", height=9, width=6)
max_diverging(maxdiff_summary,score_limits = c(-.6, 0), sources = c("Firefox Mobile"), n_bottom=10, order="desc", height=9, width=6)
max_diverging(maxdiff_summary,score_limits = c(-.6, 0), n_bottom=10, order="desc", height=9, width=6)


max_diverging(maxdiff_summary, sources = c("General Browser Users", "Firefox Desktop"), legend=T, order="desc", score_limits = c(-.6, .6), n_top = 30)
max_diverging(maxdiff_summary, sources = c("General Browser Users", "Firefox Mobile"), legend=T,  order="desc", score_limits = c(-.6, .6), n_top = 30)


#### Scatterplots to Compare Scores ---
max_compare(maxdiff_scores, sources = c("General Browser Users", "Firefox Desktop"), attributes=c("Adaptive Performance", "Auto-On Public VPN"), width=6, height=6, color="#a1fbdd")
max_compare(maxdiff_scores, sources = c("General Browser Users", "Firefox Mobile"), attributes=c("Adaptive Performance", "Auto-On Public VPN"), width=6, height=6, color="#aad6ff")

max_compare(maxdiff_scores, sources = c("General Browser Users", "Firefox Desktop"), attributes=c("Mini Games", "Password Quick Share", "Kid Mode", "Shake to Summarize"), width=6, height=6, color="#a1fbdd")
max_compare(maxdiff_scores, sources = c("General Browser Users", "Firefox Mobile"), attributes=c("Mini Games", "Password Quick Share", "Kid Mode", "Shake to Summarize"), width=6, height=6, color="#aad6ff")

max_compare(maxdiff_scores, sources = c("General Browser Users", "Firefox Desktop"), attributes=c("Hotkey Customization", "Live Crop"), width=6, height=6, color="#a1fbdd")
max_compare(maxdiff_scores, sources = c("General Browser Users", "Firefox Mobile"), attributes=c("Mini Games", "Password Quick Share", "Kid Mode"), width=6, height=6, color="#aad6ff")



max_compare(maxdiff_scores, sources = c("General Browser Users", "Firefox Desktop"), color="#a1fbdd", n_top = 30)
max_compare(maxdiff_scores, sources = c("General Browser Users", "Firefox Mobile"), color="#aad6ff", n_top = 30)
max_compare(maxdiff_scores, sources = c("Firefox Desktop", "Firefox Mobile"), color="#D26059", n_top = 30)

####Dumbbell plots ---
max_dumbbell(maxdiff_summary, "General Browser Users", "Firefox Desktop", 
             n_top=30, height=10, width=7, scale_limits = c(-0.4, 0.4),
             attributes=c("Answer Engine", "AI Mode", "AI Do-It-For-You", "Second Opinion", "Smart Compare"))

max_dumbbell(maxdiff_summary, "General Browser Users", "Firefox Mobile", 
             n_top=30, height=10, width=7, scale_limits = c(-0.4, 0.4),
             attributes=c("Answer Engine", "AI Mode", "AI Do-It-For-You", "Second Opinion", "Smart Compare"))

max_dumbbell(maxdiff_summary, "General Browser Users", "Firefox Desktop", 
             n_top=30, height=12, width=7, scale_limits = c(-0.5, 0.5))

max_dumbbell(maxdiff_summary, "General Browser Users", "Firefox Mobile", 
             n_top=30, height=12, width=7, scale_limits = c(-0.5, 0.5))

#### Anchor---
multi(survey_all, "deskfeat", source=="General Browser Users", desk_map = desk_map)

multi(survey_all, "mobfeat", source=="General Browser Users", mob_map = mob_map)





### Demo Effects ----
#Explore specific demographic effects

#Primary Desktop Browser
max_effect(maxdiff_scores, survey_all,
            demographic = "deskbrow",
            demographic_values = c("Firefox", "Chrome", "Safari", "Edge"),
            n_top =30,
            width=6,
            height=10)

max_effect(maxdiff_scores, survey_all,
           demographic = "deskbrow",
           demographic_values = c("Firefox", "Chrome", "Safari", "Edge"),
           attributes = c("AI Mode", "AI Do It For You", "Smart Compare", "Answer Engine", "Second Opinion"),
           width=10,
           height=6)



#Primary Mobile Browser
max_effect(maxdiff_scores, survey_all,
            demographic = "mobbrow",
            demographic_values = c("Samsung", "Firefox", "Chrome", "Safari"),
            source = "General Browser Users",
            n_top =10,
            width=6,
            height=10)


#Generational Cohort
max_effect(maxdiff_scores, survey_all,
            demographic = "generations",
            demographic_values = c("Gen Z", "Millennials", "Gen X", "Boomers"), 
            n_top =30,
            width=6.5,
            height=10)

max_effect(maxdiff_scores, survey_all,
           demographic = "generations",
           demographic_values = c("Gen Z", "Millennials", "Gen X", "Boomers"), 
           attributes=c("AI Mode", "Smart Compare", "Answer Engine", "Second Opinion", "AI Do It For You"),
           width=8,
           height=6)

max_effect(maxdiff_scores, survey_all,
           demographic = "generations",
           demographic_values = c("Gen Z", "Millennials", "Gen X", "Boomers"), 
           source="Firefox Desktop",
           title="Firefox Desktop",
           attributes=c("AI Mode", "Smart Compare", "Answer Engine", "Second Opinion", "AI Do It For You"),
           width=7,
           height=5)


#GenZ
max_effect(maxdiff_scores, survey_all,
            demographic = "genz",
            demographic_values = c("Gen Z", "Older Generations"), 
            n_top =30,
            width=6.5,
            height=10)

max_effect(maxdiff_scores, survey_all,
           demographic = "genz",
           demographic_values = c("Gen Z", "Older Generations"), 
           attributes = c("AI Mode", "AI Do It For You", "Smart Compare", "Second Opinion", "Answer Engine"),
           n_top =30,
           width=8,
           height=6)

max_effect(maxdiff_scores, survey_all,
           demographic = "genz",
           demographic_values = c("Gen Z", "Older Generations"), 
           attributes = c("AI Mode", "AI Do It For You", "Smart Compare", "Second Opinion", "Answer Engine"),
           source="General Browser Users",
           title = "General Browser Users",
           width=6,
           height=6, y_limits=c(-.6, .6))

max_effect(maxdiff_scores, survey_all,
           demographic = "genz",
           demographic_values = c("Gen Z", "Older Generations"), 
           attributes = c("AI Mode", "AI Do It For You", "Smart Compare", "Second Opinion", "Answer Engine"),
           source="Firefox Desktop",
           title = "Firefox Desktop",
           width=6,
           height=6,  y_limits=c(-.6, .6))

max_effect(maxdiff_scores, survey_all,
           demographic = "genz",
           demographic_values = c("Gen Z", "Older Generations"), 
           attributes = c("AI Mode", "AI Do It For You", "Smart Compare", "Second Opinion", "Answer Engine"),
           source="Firefox Mobile",
           title = "Firefox Mobile",
           width=6,
           height=6,  y_limits=c(-.6, .6))


max_effect(maxdiff_scores, survey_all,
           demographic = "genz",
           demographic_values = c("Gen Z", "Older Generations"), 
           attributes=c("Living Wallpapers", "Community Notes", "Time Insights",
                        "Green Mode", "Setup Sharing", "Themeable Spaces", "Hotkey Customization"),
           width=8,
           height=6)


#Country
max_effect(maxdiff_scores, survey_all,
           demographic = "country",
           demographic_values = c("United States", "Germany", "Japan"),
           n_top = 30,
           width=8,
           height=6)


max_effect(maxdiff_scores, survey_all,
            demographic = "country",
            demographic_values = c("United States", "Germany", "Japan"),
            attributes=c("Adaptive Performance", "Auto On Public VPN"),
            width=8,
            height=6)

max_effect(maxdiff_scores, survey_all,
           demographic = "country",
           demographic_values = c("United States", "Germany", "Japan"),
           attributes=c("AI Mode", "Smart Compare", "Answer Engine", "Second Opinion"),
           width=8,
           height=6)

max_effect(maxdiff_scores, survey_all,
           demographic = "country",
           demographic_values = c("Germany",  "Japan"),
           attributes=c("Kid Mode", "Smart Offline Reader", "Green Mode", "Snapshot Translate"),
           width=8,
           height=6)


#Occupation
max_effect(maxdiff_scores, survey_all,
            demographic = "occ",
            demographic_values = c("Browser job", "Job w/o browser"),
            n_top =30,
            width=6,
            height=10)

max_effect(maxdiff_scores, survey_all,
           demographic = "occ",
           demographic_values = c("Browser job", "Student"),
           n_top =30,
           width=6,
           height=10)

max_effect(maxdiff_scores, survey_all,
           demographic = "occ",
           demographic_values = c("Browser job", "Job w/o browser"),
           attributes=c("Clean Workspace", "Auto On Public VPN", "Adaptive Performance"),
           width=8,
           height=6)



#Gender
max_effect(maxdiff_scores, survey_all,
            demographic = "gender",
            demographic_values = c("Man", "Woman"),
            n_top =30,
            width=14,
            height=7)

max_effect(maxdiff_scores, survey_all,
           demographic = "gender",
           demographic_values = c("Man", "Woman"),
           attributes=c("Mini Games", "Green Mode", "Hotkey Customization", "Smart Offline Reader", "Snapshot Translate"),
           width=14,
           height=7)

#Device usage
max_effect(maxdiff_scores, survey_all,
           demographic = "device_usage",
           demographic_values = c("Mobile-heavy user", "Multi-device user"),
           n_top =30,
           width=6,
           height=10)

max_effect(maxdiff_scores, survey_all,
            demographic = "device_usage",
            demographic_values = c("Mobile-heavy user", "Multi-device user"),
            n_top =2,
            width=6,
            height=10)


#Primary Mobile OS
max_effect(maxdiff_scores, survey_all,
           demographic = "app",
           demographic_values = c("android", "ios"),
           title="Firefox Users",
           n_top =30,
           width=8,
           height=6)

#Kids in HH
max_effect(maxdiff_scores, survey_all,
           demographic = "kids",
           demographic_values = c("Has Kids", "Doesn't Have Kids"),
           n_top=30,
           width=14,
           height=8)

max_effect(maxdiff_scores, survey_all,
           demographic = "kids",
           demographic_values = c("Has Kids", "Doesn't Have Kids"),
           attributes = c("AI Mode", "Smart Compare", "Second Opinion", "AI Do It For You", "Answer Engine"),
           width=8,
           height=6)


max_effect(maxdiff_scores, survey_all,
           demographic = "kids",
           demographic_values = c("Has Kids", "Doesn't Have Kids"),
           attributes = c("Kid Mode"),
           width=8,
           height=6)


#Tagged SAP Search
max_effect(maxdiff_scores, survey_desktop,
           demographic = "tagged_sap_bin",
           demographic_values = c("Low Tagged SAP Search", "High Tagged SAP Search"),
           n_top =30,
           width=6,
           height=10)

max_effect(maxdiff_scores, survey_desktop,
           demographic = "tagged_sap_bin",
           demographic_values = c("Low Tagged SAP Search", "High Tagged SAP Search"),
           attributes = c("AI Mode", "Smart Compare", "Second Opinion", "AI Do It For You", "Answer Engine"),
           width=8,
           height=6)

max_effect(maxdiff_scores, survey_desktop,
           demographic = "tagged_sap_bin",
           demographic_values = c("Low Tagged SAP Search", "High Tagged SAP Search"),
           attributes = c("Themeable Spaces", "Community Notes"),
           width=8,
           height=6)


#Privacy vs. P13n
max_effect(maxdiff_scores, survey_all,
           demographic = "comfortfolded",
           demographic_values = c("Disagree", "Agree"),
           n_top=30,
           width=8,
           height=6)

max_effect(maxdiff_scores, survey_all,
            demographic = "comfortfolded",
            demographic_values = c("Disagree", "Agree"),
            attributes = c("AI Mode", "Smart Compare", "Second Opinion", "AI Do It For You", "Answer Engine"),
            width=8,
            height=6)

max_effect(maxdiff_scores, survey_all,
           demographic = "comfortfolded",
           demographic_values = c("Disagree", "Agree"),
           source="Firefox Desktop",
           title="Firefox Desktop",
           attributes = c("AI Mode", "Smart Compare", "Second Opinion", "AI Do It For You", "Answer Engine"),
           width=8,
           height=6)

max_effect(maxdiff_scores, survey_all,
           demographic = "comfortfolded",
           demographic_values = c("Disagree", "Agree"),
           source="Firefox Mobile",
           title="Firefox Mobile",
           attributes = c("AI Mode", "Smart Compare", "Second Opinion", "AI Do It For You", "Answer Engine"),
           width=8,
           height=6)

max_effect(maxdiff_scores, survey_all,
           demographic = "comfortfolded",
           demographic_values = c("Disagree", "Agree"),
           n_top=30,
           width=8,
           height=6)