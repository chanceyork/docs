### Functions
#Bar Plot for Categorical Variable
bar_single <- function(data, cat_var, xlab = NULL, ...) {
  filters <- enquos(...)
  
  for (filter in filters) {
    data <- data %>%
      filter(!!filter)}
  
  bardata <- data %>%
    filter(!is.na({{cat_var}})) %>%
    filter({{cat_var}} != "NA") %>%
    filter({{cat_var}} != "Prefer not to answer") %>%
    group_by({{cat_var}}) %>%
    summarize(n = n(), .groups = 'drop') %>%
    mutate(
      total = sum(n), 
      prop = n / total, 
      perc = prop * 100,
      sd = sd(perc, na.rm = TRUE), 
      se = sd / sqrt(n),
      t = qt((1 + 0.95) / 2, n - 1), 
      lci = perc - t * se, 
      uci = perc + t * se,
      id = row_number(), 
      labs = paste0({{cat_var}}, '\n(n=', n, ')'))
  
  print(bardata)
  
  bardata %>%
    ggplot(aes(x = fct_reorder(labs, id), y = perc, fill = {{cat_var}})) +
    geom_col() +
    geom_errorbar(aes(y = perc, ymin = lci, ymax = uci), width = 0.15) +
    scale_fill_manual(values = colors) + 
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
    geom_text(size = 3.5, aes(label = paste0(round(perc), "%"), y = uci), vjust = -0.5) +
    ylab("Percent of Sample") +
    xlab(xlab) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(caption = paste('Firefox Quant UR //', 'Updated on', Sys.Date(), sep = " "))}


#Bar Plot for Categorical Variable that Breaks Out by a Second
bar_single_breakout <- function(data, cat_var, breakdown_var, xlab = NULL, ...) {
  filters <- enquos(...)
  
  for (filter in filters) {
    data <- data %>%
      filter(!!filter)}
  
  bardata <- data %>%
    filter(!is.na({{cat_var}})) %>%
    filter({{cat_var}} != "NA") %>%
    filter({{cat_var}} != "PNTS") %>%
    group_by({{cat_var}}, {{breakdown_var}}) %>%
    summarize(n = n(), .groups = 'drop') %>%
    group_by({{cat_var}}) %>%
    mutate(
      total = sum(n),
      prop = n / total,
      perc = prop * 100,
      labs = paste0({{cat_var}}, '\n(n=', total, ')')) %>%
    ungroup() %>%
    mutate(id = row_number()) 
  
  print(bardata)
  
  bardata %>%
    ggplot(aes(
      x = fct_reorder(labs, id), 
      y = perc,
      fill = {{breakdown_var}})) +
    geom_col() +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, NA)) +    
    geom_text(
      aes(label = paste0(round(perc), "%")),
      position = position_stack(vjust = 0.5),
      size = 3
    ) +
    ylab("Percent of Sample") +
    xlab(xlab) +
    theme_minimal() +
    theme(legend.position = "right", plot.title.position = "plot") +
    labs(
      caption = paste('Firefox Quant UR //', 'Updated on', Sys.Date(), sep = " ")
    )
}

#Bar Plot for Categorical Variable that Breaks Out by Segment
bar_single_breakout_segment <- function(data, cat_var, breakdown_var, xlab = NULL, ...) {
  filters <- enquos(...)
  
  for (filter in filters) {
    data <- data %>%
      filter(!!filter)}
  
  bardata <- data %>%
    filter(!is.na({{cat_var}})) %>%
    filter({{cat_var}} != "NA") %>%
    filter({{cat_var}} != "PNTS") %>% 
    filter({{cat_var}} != "Not sure / no opinion") %>%
    group_by({{cat_var}}, {{breakdown_var}}) %>%
    summarize(n = n(), .groups = 'drop') %>%
    group_by({{cat_var}}) %>%
    mutate(
      total = sum(n),
      prop = n / total,
      perc = prop * 100,
      labs = paste0({{cat_var}}, '\n(n=', total, ')')) %>%
    ungroup() %>%
    mutate(id = row_number()) 
  
  print(bardata)
  
  bardata %>%
    ggplot(aes(
      x = fct_reorder(labs, id), 
      y = perc,
      fill = {{breakdown_var}})) +
    geom_col() +
    scale_fill_manual(values = segment_colors) +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, NA)) +    
    geom_text(
      aes(label = paste0(round(perc), "%")),
      position = position_stack(vjust = 0.5),
      size = 3) +
    ylab("Percent of Sample") +
    xlab(xlab) +
    theme_minimal() +
    theme(legend.position = "right", plot.title.position = "plot") +
    labs(
      caption = paste('Firefox Quant UR //', 'Updated on', Sys.Date(), sep = " "))}


#Sankey plot for categorical breakouts by a segmenter variable
alluvial_flow <- function(data, cat_var1, cat_var2, xlab = NULL, ylab = "Count", ...) {
  
  filters <- enquos(...)
  for (filter in filters) {
    data <- data %>%
      filter(!!filter)}
  
  alluvial_data <- data %>%
    filter(
      !is.na({{cat_var1}}), 
      !is.na({{cat_var2}}), 
      {{cat_var1}} != "Not sure / no opinion",
      {{cat_var2}} != "Not sure / no opinion",
      {{cat_var1}} != "PNTS",
      {{cat_var2}} != "PNTS") %>%  
    count({{cat_var1}}, {{cat_var2}}) %>% 
    rename(cat1 = {{cat_var1}}, cat2 = {{cat_var2}}) 
  
  print(alluvial_data)
  
  ggplot(alluvial_data, aes(
    axis1 = cat1,
    axis2 = cat2,
    y = n)) +
    geom_alluvium(aes(fill = cat1), alpha = 0.8) +  
    geom_stratum(width = 0.25, fill = "gray80", color = "black") +  
    geom_text(
      stat = "stratum",
      aes(label = after_stat(stratum)),
      size = 3,
      color = "black") +
    scale_x_discrete(
      expand = c(0.1, 0.1),
      labels = c(as_label(enquo(cat_var2)), as_label(enquo(cat_var1)))) +
    scale_fill_manual(values = colors) +  
    labs(x = xlab, 
         y = ylab, 
         caption = paste('Firefox Quant UR //', 'Updated on', Sys.Date(), sep = " ")) +
    theme_minimal() +
    theme(legend.position = "none",
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10))}


#Bar Plot for Categorical Variable that's Arranged High to Low
bar_single_arranged <- function(data, cat_var, xlab = NULL, ...) {
  filters <- enquos(...)
  
  for (filter in filters) {
    data <- data %>%
      filter(!!filter)}
  
  bararr <- data %>%
    filter(!is.na({{cat_var}})) %>%
    filter({{cat_var}} != "NA") %>%
    filter({{cat_var}} != "Prefer not to answer") %>%
    group_by({{cat_var}}) %>%
    summarize(n = n()) %>%
    mutate(
      total = sum(n),
      prop = n / total,
      perc = prop * 100,
      sd = sd(perc),
      se = sd / sqrt(n),
      t = qt((1 + 0.95) / 2, n - 1),
      lci = perc - t * se,
      uci = perc + t * se,
      id = row_number(),
      labs = paste0({{cat_var}}, '\n(n=', n, ')'))
  
  print(bararr)
  
  bararr %>%
    arrange(desc(perc)) %>%
    ggplot(aes(x = fct_reorder(labs, perc, .desc = TRUE), y = perc, fill = {{cat_var}})) +
    geom_col() +
    geom_errorbar(aes(y = perc, ymin = lci, ymax = uci), width = 0.15) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
    geom_text(size = 3.5, aes(label = paste0(round(perc), "%"), y = uci), vjust = -0.5) +
    xlab(xlab) +
    ylab("Percent of Sample") +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(caption = paste('Firefox Quant UR //', 'Updated on', Sys.Date(), sep = " "))}


#Coord Flipped Bar Plot for Categorical Variable that's Arranged High to Low
bar_single_arranged_flipped <- function(data, cat_var, xlab = NULL, ...) {
  filters <- enquos(...)
  
  for (filter in filters) {
    data <- data %>%
      filter(!!filter)}
  
  bararr <- data %>%
    filter(!is.na({{cat_var}})) %>%
    filter({{cat_var}} != "NA") %>%
    filter({{cat_var}} != "Prefer not to answer") %>%
    group_by({{cat_var}}) %>%
    summarize(n = n()) %>%
    mutate(
      total = sum(n),
      prop = n / total,
      perc = prop * 100,
      sd = sd(perc),
      se = sd / sqrt(n),
      t = qt((1 + 0.95) / 2, n - 1),
      lci = perc - t * se,
      uci = perc + t * se,
      id = row_number(),
      labs = paste0({{cat_var}}, '\n(n=', n, ')'))
  
  print(bararr)
  
  bararr %>%
    arrange(desc(perc)) %>%
    ggplot(aes(x = fct_reorder(labs, perc), y = perc, fill = {{cat_var}})) +
    geom_col() +
    geom_errorbar(aes(y = perc, ymin = lci, ymax = uci), width = 0.15) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
    geom_text(size = 3.5, aes(label = paste0(round(perc), "%"), y = uci), hjust = -0.2) +
    xlab(xlab) +
    ylab("Percent of Sample") +
    coord_flip() + 
    theme_minimal() +
    theme(legend.position = "none") +
    labs(caption = paste('Firefox Quant UR //', 'Updated on', Sys.Date(), sep = " "))}




#Bar Plot for Two Categorical Variables
bar_double <- function(data, cat_var1, cat_var2, xlab = NULL, ...) {
  filters <- enquos(...)
  
  for (filter in filters) {
    data <- data %>%
      filter(eval_tidy(filter, data))}
  
  bardub <- data %>%
    filter(!is.na({{cat_var1}}), !is.na({{cat_var2}}), 
           {{cat_var1}} != "PNTS", 
           {{cat_var2}} != "PNTS",
           {{cat_var1}} != "Not sure / no opinion", 
           {{cat_var2}} != "Not sure / no opinion") %>% 
    count({{cat_var1}}, {{cat_var2}}) %>% 
    group_by({{cat_var1}}) %>% 
    mutate(cattotal = sum(n), prop = n / cattotal, perc = prop * 100,
           sd = sd(perc), se = sd / sqrt(n),
           t = qt((1 + 0.95) / 2, n - 1), lci = perc - t * se, uci = perc + t * se,
           id = row_number(), labs = paste0({{cat_var1}}, '\n(n=', cattotal, ')')) 
  
  print(bardub, n = 100)
  
  bardub %>% 
    ggplot(aes(x = factor(labs, levels = unique(labs)), y = perc, fill = {{cat_var2}})) + 
    geom_col(position = "dodge") +
    geom_errorbar(aes(ymin = lci, ymax = uci), position = position_dodge(width = 0.9), width = 0.25) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
    geom_text(aes(label = paste0(round(perc), "%"),
                  y = uci + 0.8),  
              position = position_dodge(width = 0.9), 
              vjust = -0.5, size = 3.5) +
    labs(x = xlab,      
      y = "Percent of Subsample",
      fill = NULL,   
      caption = paste('Firefox Quant UR //', 'Updated on', today.date, sep = " ")) +
    theme_minimal() +
    theme(legend.position = "bottom",
      legend.text = element_text(color = "black"))}

#Bar Plot for Two Categorical Variables, Retain Segment Colors
bar_double_seg <- function(data, cat_var1, cat_var2, ...) {
  filters <- enquos(...)
  
  for (filter in filters) {
    data <- data %>%
      filter(eval_tidy(filter, data)) }
  
  bardub <- data %>%
    filter(!is.na({{cat_var1}}), !is.na({{cat_var2}}), 
           {{cat_var1}} != "PNTS", 
           {{cat_var2}} != "PNTS",
           {{cat_var1}} != "Not sure / no opinion", 
           {{cat_var2}} != "Not sure / no opinion") %>% 
    count({{cat_var1}}, {{cat_var2}}) %>% 
    group_by({{cat_var1}}) %>% 
    mutate(cattotal = sum(n), prop = n / cattotal, perc = prop * 100,
           sd = sd(perc), se = sd / sqrt(n),
           t = qt((1 + 0.95) / 2, n - 1), lci = perc - t * se, uci = perc + t * se,
           id = row_number(),labs = paste0({{cat_var1}},'\n(n=', cattotal, ')')) 
  
  print(bardub, n = 100)
  
  bardub %>% 
    ggplot(aes(x = factor(labs, levels=unique(labs)), y = perc, fill = {{cat_var2}})) + 
    geom_col(position = "dodge") +
    geom_errorbar(aes(ymin = lci, ymax = uci), position = position_dodge(width = 0.9), width = 0.25) +
    scale_fill_manual(values = segment_colors) +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
    geom_text(aes(label = paste0(round(perc), "%"),
                  y = uci + 0.8),  
              position = position_dodge(width = 0.9), 
              vjust = -0.5, size = 3.5) +
    xlab(xlab)+
    ylab("Percent of Subsample") +
    theme_minimal() +
    theme(legend.position = "bottom", legend.text = element_text(color = "black"),
          axis.title.x = element_blank()) +
    labs(caption = paste('Firefox Quant UR //', 'Updated on', today.date, sep = " ")) }


#Bar Plot for Two Categorical Variables, Faceted by One
bar_double_faceted <- function(data, cat_var1, cat_var2, xlab = NULL, ...) {
  filters <- enquos(...)
  
  for (filter in filters) {
    data <- data %>%
      filter(eval_tidy(filter, data))}
  
  bardub <- data %>%
    filter(!is.na({{cat_var1}}), !is.na({{cat_var2}}), 
           {{cat_var1}} != "PNTS", 
           {{cat_var2}} != "PNTS",
           {{cat_var1}} != "Not sure / no opinion", 
           {{cat_var2}} != "Not sure / no opinion") %>% 
    count({{cat_var1}}, {{cat_var2}}) %>% 
    group_by({{cat_var2}}) %>% 
    mutate(cattotal = sum(n), prop = n / cattotal, perc = prop * 100,
           sd = sd(perc), se = sd / sqrt(n),
           t = qt((1 + 0.95) / 2, n - 1), lci = perc - t * se, uci = perc + t * se,
           id = row_number(), labs = paste0({{cat_var2}}, '\n(n=', cattotal, ')'))
  
  print(bardub, n = 100)
  
  bardub %>% 
    arrange(desc(perc)) %>% 
    ggplot(aes(x = labs, y = perc, fill = {{cat_var1}})) +
    geom_col() +
    geom_errorbar(aes(y = perc, ymin = lci, ymax = uci), width = 0.15) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
    geom_text(size = 3.5, aes(label = paste0(round(perc), "%"), y = uci), vjust = -0.5) +
    xlab(NULL) +
    ylab("Percent of Subsample") +
    facet_wrap(vars({{cat_var1}}), scales = "free", nrow = 1) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.text = element_text(color = "black"),
          axis.title.x = element_blank()) +
    labs(caption = paste('Firefox Quant UR //', 'Updated on', Sys.Date(), sep = " "))}


#Bar Plot for Multiple Factor Variables with Commonly Filtered Response Option
bar_multiple <- function(data, cat_vars, response, xlab = NULL, ...) {
  filters <- enquos(...)
  
  for (filter in filters) {
    data <- data %>%
      filter(!!filter)
  }
  
  n_totals <- data %>%
    select(all_of(cat_vars)) %>%
    summarise(across(everything(), ~ sum(!is.na(.)))) %>%
    pivot_longer(cols = everything(), names_to = "question", values_to = "n_total")
  
  bardata <- data %>%
    select(all_of(cat_vars)) %>%
    pivot_longer(cols = everything(), names_to = "question", values_to = "response") %>%
    filter(response == !!response) %>%
    group_by(question) %>%
    summarize(
      n_response = n(),
      .groups = "drop"
    ) %>%
    left_join(n_totals, by = "question") %>%
    mutate(
      perc = (n_response / n_total) * 100,
      labs = paste0(question, '\n(n=', n_response, ')'),
      se = sqrt((perc / 100) * (1 - perc / 100) / n_total),
      ci = 1.96 * se,
      lci = perc - ci * 100,
      uci = perc + ci * 100
    ) %>%
    ungroup() %>%
    mutate(
      id = row_number(),
      labs = stringr::str_to_title(labs) # Capitalize the labels
    )
  
  bardata <- bardata %>%
    arrange(desc(perc)) %>%
    mutate(labs = factor(labs, levels = labs))
  
  print(bardata)
  
  bardata %>%
    ggplot(aes(
      x = fct_reorder(labs, -perc),
      y = perc)) +
    geom_col(aes(fill = question)) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
    geom_errorbar(
      aes(ymin = lci, ymax = uci),
      width = 0.2) +
    geom_text(
      aes(label = paste0(round(perc), "%")),
      position = position_nudge(y = 3),  
      size = 3) +
    ylab(paste0("Percent of '", response, "' Responses")) +
    xlab(xlab) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(
      caption = paste('Firefox Quant UR //', 'Updated on', Sys.Date(), sep = " "))
}






#Histogram for Continuous Variable
hist_func <- function(data, num_var, xlab = NULL, ...) {
  filters <- enquos(...)
  
  for (filter in filters) {
    data <- data %>%
      filter(eval_tidy(filter, data))}
  
  summary_stats <- data %>%
    summarise(mean = mean({{num_var}}, na.rm = TRUE),
              sd = sd({{num_var}}, na.rm = TRUE),
              min = min({{num_var}}, na.rm = TRUE),
              max = max({{num_var}}, na.rm = TRUE))
  
  print(summary_stats)
  
  mean_data <- data.frame(mean = summary_stats$mean)
  
  data %>%
    filter(!is.na({{num_var}})) %>%
    ggplot(aes(x = {{num_var}})) +
    geom_histogram(aes(fill = after_stat(count)), bins = 30) +
    scale_fill_gradientn(colors = colors) + 
    geom_vline(data = mean_data, aes(xintercept = mean), 
               lwd = 0.5, color = "black", linetype = "longdash") +
    geom_text(data = mean_data, aes(x = mean, y = Inf, 
                                    label = paste("Mean =", round(mean))), 
              color = "black", angle = 90, vjust = -1.5, hjust = 1.1) +
    xlab(xlab) +  
    ylab("Number of Users in Sample") +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(caption = paste('Firefox Quant UR //', 'Updated on', Sys.Date(), sep = " "))}



reorder_freq <- function(data, var) {
  data %>%
    mutate({{var}} := factor({{var}},
                             levels = c("Never", "Rarely", "Sometimes", "Often"),
                             ordered = TRUE))  
}

recode_yes_no <- function(data, ...) {
  data %>% mutate(across(.cols = c(...), .fns = ~ case_when(is.na(.) ~ "No", TRUE ~ "Yes") %>% factor(levels = c("Yes", "No"))))}

importance_relevel <- function(data, var_names) {
  data <- data %>% mutate(across(all_of(var_names), ~ fct_recode(.,
                                                  "Not at all" = "Not at all important",
                                                  "A little" = "A little",
                                                  "Somewhat" = "Somewhat",
                                                  "Very" = "Very important",
                                                  "Not sure / no opinion" = "Not sure / no opinion") %>% 
      fct_relevel("Not at all",  "A little", "Somewhat", "Very", "Not sure / no opinion")))
  
  return(data)}

interest_relevel <- function(data, var_name) {
  data <- data %>%
    mutate(across(all_of(var_name), ~ fct_recode(.,
                                                 "Not at all" = "Not at all interested",
                                                 "Not very" = "Not very interested",
                                                 "Somewhat" = "Somewhat interested",
                                                 "Very" = "Very interested") %>% 
                    fct_relevel("Not at all", "Not very", "Somewhat", "Very")))
  
  return(data)}


agree_relevel <- function(data, var_name) {
  data <- data %>%
    mutate(across(all_of(var_name), ~ fct_recode(.,
                                                 "Strongly disagree" = "Strongly disagree",
                                                 "Disagree" = "Disagree",
                                                 "Neither agree nor disagree" = "Neither agree nor disagree",
                                                 "Agree" = "Agree",
                                                 "Strongly agree" = "Strongly agree") %>%
                    fct_relevel("Disagree", "Strongly disagree", "Neither agree nor disagree", "Agree", "Strongly agree")))
  
  return(data)}

agree_relevel_idv <- function(data, var_name, new_var_name) {
  data <- data %>%
    mutate(
      {{ new_var_name }} := fct_recode(.data[[var_name]],
                                       "Strongly disagree" = "Strongly disagree",
                                       "Disagree" = "Disagree",
                                       "Neither agree nor disagree" = "Neither agree nor disagree",
                                       "Agree" = "Agree",
                                       "Strongly agree" = "Strongly agree") %>%
        fct_relevel("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))
  
  return(data)}

agree_relevel_idv_flip <- function(data, var_name, new_var_name) {
  data <- data %>%
    mutate(
      {{ new_var_name }} := fct_recode(.data[[var_name]],
                                       "Strongly disagree" = "Strongly agree",
                                       "Disagree" = "Agree",
                                       "Neither agree nor disagree" = "Neither agree nor disagree",
                                       "Agree" = "Disagree",
                                       "Strongly agree" = "Strongly disagree") %>%
        fct_relevel("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))
  
  return(data)}

freq_media <- function(data, var_name, new_var_name) {
  data <- data %>%
    mutate(
      {{ new_var_name }} := fct_recode(.data[[var_name]],
                                       "Never or infrequently" = "Never or less than once a month",
                                       "Less than once a week" = "Less than once a week",
                                       "Once a week" = "Once a week",
                                       "Several times a week" = "Several times a week",
                                       "Every day" = "Every day",
                                       "Multiple times a day" = "Multiple times a day") %>%
        fct_relevel("Never or infrequently", "Less than once a week", "Once a week", "Several times a week", "Every day", "Multiple times a day"))
  
  return(data)}

weekly_conversion <- function(df, new_var, old_var){
  df %>%
    mutate({{new_var}} := case_when(
      {{old_var}} == "Never or infrequently" ~ 0,
      {{old_var}} == "Less than once a week" ~ 0,
      {{old_var}} == "Once a week" ~ 0,
      {{old_var}} == "Several times a week" ~ 0,
      {{old_var}} == "Every day" ~ 1,
      {{old_var}} == "Multiple times a day" ~ 1))}


boxplot_with_jitter <- function(data, y_var, x_var, jitter_width = 0.2, xlab = "X Axis Label", ylab = "Y Axis Label") {
  # Filter out 'Not sure / no opinion' responses
  data_filtered <- data %>% 
    filter(get(x_var) != "Not sure / no opinion")
  
  avg_ages <- data_filtered %>%
    group_by(.data[[x_var]]) %>%
    summarise(avg_age = mean(.data[[y_var]], na.rm = TRUE))
  
  ggplot(data_filtered, aes_string(x = x_var, y = y_var, fill = x_var)) +
    geom_boxplot(outlier.shape = NA, color = "black", alpha = 0.7) +  
    geom_jitter(width = jitter_width, color = "#FF6B6B", size = 1, alpha = 0.6) +  
    scale_fill_manual(values = colors) +  
    labs(x = xlab, y = ylab) + 
    geom_text(data = avg_ages, aes(x = .data[[x_var]], y = avg_age, label = round(avg_age, 1)), 
              color = "black", fontface = "bold", size = 7, 
              vjust = -1.5, hjust = 1, nudge_x = 0.25) + 
    coord_flip()+
    theme_minimal() +
    theme(
      plot.title = element_blank(), 
      legend.position = "none"  
    )
}




#Venn diagram
generate_custom_venn <- function(aidata, q8_1, q8_4, q8_6) {
  
  genai <- !is.na(aidata[[q8_1]]) & aidata[[q8_1]] == "Yes"
  aisoft <- !is.na(aidata[[q8_4]]) & aidata[[q8_4]] == "Yes"
  aiapp <- !is.na(aidata[[q8_6]]) & aidata[[q8_6]] == "Yes"
  
  total_count <- nrow(aidata)
  
  intersection_counts <- c(
    genai_only = sum(genai & !aisoft & !aiapp),
    aisoft_only = sum(!genai & aisoft & !aiapp),
    aiapp_only = sum(!genai & !aisoft & aiapp),
    genai_aisoft = sum(genai & aisoft & !aiapp),
    genai_aiapp = sum(genai & !aisoft & aiapp),
    aisoft_aiapp = sum(!genai & aisoft & aiapp),
    all_three = sum(genai & aisoft & aiapp))
  
  intersection_percentages <- (intersection_counts / total_count) * 100
  
  custom_labels <- c(
    sprintf("%d\n(%.1f%%)", intersection_counts[1], intersection_percentages[1]),
    sprintf("%d\n(%.1f%%)", intersection_counts[2], intersection_percentages[2]),
    sprintf("%d\n(%.1f%%)", intersection_counts[3], intersection_percentages[3]),
    sprintf("%d\n(%.1f%%)", intersection_counts[4], intersection_percentages[4]),
    sprintf("%d\n(%.1f%%)", intersection_counts[5], intersection_percentages[5]),
    sprintf("%d\n(%.1f%%)", intersection_counts[6], intersection_percentages[6]),
    sprintf("%d\n(%.1f%%)", intersection_counts[7], intersection_percentages[7]))
  
  grid.newpage()
  
  pushViewport(viewport(width = 1, height = 1))
  
  grid.circle(x = 0.37, y = 0.64, r = 0.25, gp = gpar(fill = "#D705F2", alpha = 0.5))
  grid.circle(x = 0.63, y = 0.64, r = 0.25, gp = gpar(fill = "#A66641", alpha = 0.5))
  grid.circle(x = 0.5, y = 0.35, r = 0.25, gp = gpar(fill = "#A5F828", alpha = 0.5))
  
  grid.text("GenAI Tools like ChatGPT", x = 0.26, y = 0.92, gp = gpar(fontsize = 18, fontface = "bold"))
  grid.text("AI-Powered Software", x = 0.74, y = 0.92, gp = gpar(fontsize = 18, fontface = "bold"))
  grid.text("Apps that Use AI", x = 0.5, y = 0.04, gp = gpar(fontsize = 18, fontface = "bold"))
  
  grid.text(custom_labels[1], x = 0.29, y = 0.70, gp = gpar(fontsize = 15, col = "black"))
  grid.text(custom_labels[2], x = 0.70, y = 0.70, gp = gpar(fontsize = 15, col = "black"))
  grid.text(custom_labels[3], x = 0.50, y = 0.2, gp = gpar(fontsize = 15, col = "black"))
  grid.text(custom_labels[4], x = 0.4, y = 0.49, gp = gpar(fontsize = 15, col = "black"))
  grid.text(custom_labels[5], x = 0.6, y = 0.49, gp = gpar(fontsize = 15, col = "black"))
  grid.text(custom_labels[6], x = 0.50, y = 0.69, gp = gpar(fontsize = 15, col = "black"))
  grid.text(custom_labels[7], x = 0.50, y = 0.56, gp = gpar(fontsize = 15, col = "black"))
  
  grid.text(label = sprintf("Firefox Quant UR // Updated on %s", today.date),
    x = 0.85, y = 0.01, gp = gpar(fontsize = 10, col = "black"))}



#Logit for binary outcomes
logit <- function(data, dependent_var, predictors) {
  data <- data[!data[[dependent_var]] %in% c("Neither", "Not sure / no opinion"), ]
  data[[dependent_var]] <- factor(data[[dependent_var]], levels = unique(data[[dependent_var]]))
  if (length(levels(data[[dependent_var]])) != 2) {
    stop("Dependent variable must have exactly two levels after filtering.") }
  formula <- as.formula(paste(dependent_var, "~", paste(predictors, collapse = " + ")))
  model <- glm(formula, data = data, family = binomial)
  assign("last_model", model, envir = .GlobalEnv)
  null_model <- glm(as.formula(paste(dependent_var, "~ 1")), data = data, family = binomial)
  mc_fadden_r2 <- 1 - (logLik(model) / logLik(null_model))
  cat("Number of observations (nobs):", nobs(model), "\n")
  cat("Dependent variable levels:\n")
  cat("High/1:", levels(data[[dependent_var]])[2], "\n")
  cat("Low/0:", levels(data[[dependent_var]])[1], "\n")
  cat("McFadden's R-squared:", mc_fadden_r2, "\n")
  return(summary(model))}


# Ologit for ordered categorical outcomes
ologit <- function(data, dependent_var, predictors) {
  data[[dependent_var]] <- ifelse(data[[dependent_var]] == "Not sure / no opinion", NA, data[[dependent_var]])
  data[[dependent_var]] <- factor(data[[dependent_var]], ordered = TRUE)
  formula <- as.formula(paste(dependent_var, "~", paste(predictors, collapse = " + ")))
  model <- clm(formula, data = data)
  assign("last_model", model, envir = .GlobalEnv)
  library(pscl)
  mc_fadden_r2 <- pR2(last_model)["McFadden"]
  cat("McFadden's R-squared:", mc_fadden_r2, "\n")
  return(summary(model))}


#Multinomial logit for unordered categorical outcome
mlogit <- function(data, dependent_var, predictors) {
  options(max.print = 10000)  
  data[[dependent_var]] <- factor(data[[dependent_var]])

  formula <- as.formula(paste(dependent_var, "~", paste(predictors, collapse = " + ")))
  model <- multinom(formula, data = data)
  assign("last_model", model, envir = .GlobalEnv)
  
  mc_fadden_r2 <- pR2(last_model)["McFadden"]
  cat("McFadden's R-squared:", mc_fadden_r2, "\n")
  
  reference_group <- levels(data[[dependent_var]])[1]
  cat("Baseline/Reference group:", reference_group, "\n")
  
  coefs <- summary(model)$coefficients
  ses <- summary(model)$standard.errors
  z_vals <- coefs / ses
  p_vals <- 2 * (1 - pnorm(abs(z_vals)))
  results <- data.frame(
    Outcome = rep(rownames(coefs), each = ncol(coefs)),
    Predictor = rep(colnames(coefs), times = nrow(coefs)),
    Coefficient = as.vector(coefs),
    SE = as.vector(ses),
    Z = as.vector(z_vals),
    P_value = as.vector(p_vals))
  results$Significance <- cut(
    results$P_value,
    breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
    labels = c("***", "**", "*", ".", ""),
    include.lowest = TRUE)
  
  print(results)
  return(model)}
