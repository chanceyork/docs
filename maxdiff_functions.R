### Functions

## Load Packages ----
library(tidyverse)
library(ggrepel)
library(rlang)
library(broom)
library(lubridate)
library(rlang)
library(emmeans)
library(scales)
library(bayesm)

#Data Cleaning----

#Reorder a frequency variable
reorder_freq <- function(data, var) {
  data %>%
    mutate({{var}} := factor({{var}},
                             levels = c("Never", "Rarely", "Sometimes", "Often"),
                             ordered = TRUE))  }

reorder_agreement <- function(data, var) {
  data %>%
    mutate({{var}} := factor({{var}},
                             levels = c("Strongly disagree", "Disagree", "Neither agree or disagree", "Agree", "Strongly agree"),
                             ordered = TRUE))}

reorder_custom <- function(data, var) {
  data %>%
    mutate({{var}} := factor({{var}},
                             levels = c("No, never",
                                        "Yes, within the past year",
                                        "Yes, about 1-4 years ago",
                                        "Yes, 5 or more years ago",
                                        "Not sure"),
                             labels = c("No, never",
                                        "Yes, past year",
                                        "Yes, 1–4 years ago",
                                        "Yes, 5+ years ago",
                                        "Not sure"),
                             ordered = TRUE))}




#Clean surveys
clean.survey <- function(x) {x %>%  
    setNames(tolower(names(.))) %>% 
    setNames(gsub("\\.", "_", names(.))) %>%  
    select(-any_of(unique(c(
      "status", "time_started", "referer", "sessionid", "legacy_comments", 
      "comments", "user_agent", "tags", "url_variable__slanguage",
      "url_variable__action", "url_variable__controller",
      "url_variable__id", "url_variable__module", 
      "url_variable__link_id", "url_variable__preview_frame",
      "url_variable__sglocale", "url_variable__rid",
      "url_variable__snc", "url_variable__utm_campaign",
      "url_variable__utm_medium", "url_variable__updatechannel",
      "url_variable__type", "url_variable__surveyversion",
      "url_variable__utm_source","url_variable__source",
      "url_variable__app", "url_variable__userid",
      "capture_comma_separated_list_of_best_options", "branch",
      "complete","new_javascript")))) %>% 
    select(-any_of(c("url_variable__slanguage"))) %>% 
    rename(duration = survey_timer, lang = language) %>% 
    mutate_at(vars(c("duration", "yob", contains(".timer"))), as.numeric) %>% 
    mutate_if(is.character, as.factor)}



#Clean maxdiffs
clean.maxdiff <- function(x) {x %>%  
    setNames(tolower(names(.))) %>% 
    setNames(gsub("\\.", "_", names(.))) %>%  
    mutate_if(is.character, as.factor)}


#Bar Plots----

#bar plot
bar <- function(data, cat_var, filter_expr = NULL, xlab = NULL, height = 5, width = 9, bg = "transparent", order = "desc", flip_coords = FALSE,...) {
  cat_var <- rlang::enquo(cat_var)
  filter_expr <- rlang::enquo(filter_expr)
  
  if (!rlang::quo_is_missing(filter_expr) && !rlang::is_null(rlang::get_expr(filter_expr))) {
    data <- data %>% filter(!!filter_expr) }
  
  if (!missing(...)) {
    data <- data %>% filter(...)}
  
  data_processed <- data %>%
    filter(
      !is.na(!!cat_var),
      !(!!cat_var %in% c("PNTS", "Prefer not to say"))) %>%
    count(!!cat_var, name = "n") %>%
    mutate(
      total = sum(n),
      prop = n / total,  
      se = sqrt(prop * (1 - prop) / n),
      t = ifelse(n > 1, qt(0.975, df = n - 1), NA_real_), 
      perc = prop * 100, 
      lci = (prop - t * se) * 100, 
      uci = (prop + t * se) * 100, 
      labs = paste0(as.character(!!cat_var), "\n(n=", n, ")")) %>%
    mutate(
      x = if (order == "original") {
        factor(labs, levels = labs)} else {
          fct_reorder(labs, n, .desc = (order == "desc")) })
  
  print(data_processed)
  
  plot <- data_processed %>%
    ggplot(aes(x = x, y = perc, fill = !!cat_var)) +
    geom_col() +
    geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.15) +
    scale_fill_manual(values = colors) + 
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
    geom_text(aes(label = paste0(round(perc), "%"), y = uci), vjust = -0.5) +
    ylab("Percent of Sample") +
    xlab(xlab) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      legend.position = "none",
      axis.title.x = element_text(margin = margin(t = 15))) +
    labs(caption = paste('Firefox Quant UR // Updated on', Sys.Date()))
  
  if (flip_coords) {
    plot <- plot + coord_flip()
  }
  
  ggsave("plot.png", plot = plot, height = height, width = width, bg = bg, dpi = 300)
  
  return(plot)}


#bar plot with proportions of one categorical variable over another
bars <- function(data, cat_var, group_var, filter_expr = NULL, 
                 xlab = NULL, height = 5, width = 9, bg = "transparent", 
                 order = "desc", ...) {
  
  cat_var <- rlang::enquo(cat_var)
  group_var <- rlang::enquo(group_var)
  filter_expr <- rlang::enquo(filter_expr)
  
  if (!rlang::quo_is_missing(filter_expr) && !rlang::is_null(rlang::get_expr(filter_expr))) {
    data <- data %>% filter(!!filter_expr)}
  
  if (!missing(...)) {
    data <- data %>% filter(...) }
  
  data_processed <- data %>%
    filter( !is.na(!!cat_var), !is.na(!!group_var),
      !(!!cat_var %in% c("PNTS", "Prefer not to say"))) %>%    
  count(!!group_var, !!cat_var, name = "n") %>%
    group_by(!!group_var) %>%
    mutate(
      total = sum(n),
      prop = n / total,
      se = sqrt(prop * (1 - prop) / n),
      t = ifelse(n > 1, qt(0.975, df = n - 1), NA_real_),
      perc = prop * 100,
      lci = (prop - t * se) * 100,
      uci = (prop + t * se) * 100,
      labs = paste0(as.character(!!group_var), "\n(n=", total, ")")) %>%
    mutate(
      x = if (order == "original") {
        factor(labs, levels = labs)  } else {
          fct_reorder(labs, total, .desc = (order == "desc")) } )
  
  print(data_processed)
  
  plot <- data_processed %>%
    ggplot(aes(x = x, y = perc, fill = !!cat_var)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_errorbar(
      aes(ymin = lci, ymax = uci),
      position = position_dodge(width = 0.8),
      width = 0.15) +
    geom_text(
      aes(label = paste0(round(perc), "%"), y = uci),
      vjust = -0.5,
      position = position_dodge(width = 0.8),
      size = 3) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
    ylab("Percent of Sample") +
    xlab(xlab) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      legend.position ='bottom',
      axis.title.x = element_text(margin = margin(t = 15)),
      legend.title = element_blank()) +
    labs(caption = paste('Firefox Quant UR // Updated on', Sys.Date()))
  
  
  ggsave("plot.png", plot = plot, height = height, width = width, bg = bg, dpi = 300)
  
  return(plot)}



#histogram
hist <- function(data, num_var, xlab = NULL, ..., height = 5, width = 9, bg = "transparent") {
  filters <- enquos(...)
  
  for (filter in filters) {
    data <- data %>%
      filter(eval_tidy(filter, data))
  }
  
  summary_stats <- data %>%
    summarise(mean = mean({{num_var}}, na.rm = TRUE),
              sd = sd({{num_var}}, na.rm = TRUE),
              median = median({{num_var}}, na.rm = TRUE),
              min = min({{num_var}}, na.rm = TRUE),
              max = max({{num_var}}, na.rm = TRUE))
  
  print(summary_stats)
  
  mean_data <- data.frame(mean = summary_stats$mean)
  
  plot <- data %>%
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
    labs(caption = paste('Firefox Quant UR //', 'Updated on', Sys.Date(), sep = " "))
  
  ggsave(filename = "plot.png", plot = plot, dpi = 300, height = height, width = width, bg = bg)
  
  return(plot)}



#Bar Plot for Two Categorical Variables, Faceted by One
bar_double <- function(data, cat_var1, cat_var2, xlab = NULL, title = NULL, ...) {
  filters <- enquos(...)
  
  for (filter in filters) {
    data <- data %>%
      filter(eval_tidy(filter, data))}
  
  bardub <- data %>%
    filter(!is.na({{cat_var1}}), !is.na({{cat_var2}}), 
           {{cat_var1}} != "Prefer not to answer", 
           {{cat_var2}} != "Prefer not to answer") %>% 
    count({{cat_var1}}, {{cat_var2}}) %>% 
    group_by({{cat_var2}}) %>% 
    mutate(cattotal = sum(n), prop =n/cattotal, perc = prop*100,
           sd= sd(perc), se = sd/sqrt(n),
           t = qt((1 + 0.95)/2, n-1), lci = perc - t * se, uci = perc + t * se,
           id = row_number(),labs = paste0({{cat_var2}},'\n(n=', cattotal, ')')) 
  
  print(bardub, n = 100)
  
  bardub %>% 
    arrange(desc(perc)) %>% 
    slice(1:10) %>% 
    ggplot(aes(x=labs, y=perc, fill={{cat_var1}})) +
    geom_col() +
    geom_errorbar(aes(y=perc, ymin=lci, ymax=uci), width=0.15) +
    scale_fill_manual(values=colors) +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits=c(0, 100)) +
    geom_text(size = 3.5, aes(label = paste0(round(perc), "%"), y = uci), vjust = -0.5) +
    xlab(NULL) +
    ylab("Percent of Sample") +
    facet_wrap(vars(({{cat_var1}})), scales="free", nrow = 1) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.text = element_text(color = "white"),
          axis.title.x = element_blank()) +
    labs(caption = paste('Firefox Quant UR //', 'Updated on', today.date, sep = " "), title=title) }



#Descriptive Plots ----
point_double <- function(data, cat_var1, cat_var2, ...) {
  filters <- enquos(...)
  bardub <- data %>%
    filter(!is.na({{cat_var1}}), !is.na({{cat_var2}})) %>%
    count({{cat_var1}}, {{cat_var2}}) %>%
    group_by({{cat_var2}}) %>%
    mutate(total = sum(n), prop = n / total, perc = prop * 100, 
           se = sqrt((prop * (1 - prop)) / n), 
           ci = qt(0.975, df = n - 1) * se, 
           lci = (prop - ci) * 100, uci = (prop + ci) * 100, 
           labs = paste0("(n=", n, ")")) %>%
    filter({{cat_var1}} != "Prefer not to say", 
           {{cat_var2}} != "Prefer not to say", 
           {{cat_var1}} != "These don't describe me", 
           {{cat_var1}} != "Homemaker/caregiver", 
           {{cat_var1}} != "Gig worker", 
           {{cat_var1}} != "Retired", 
           {{cat_var1}} != "Student", 
           {{cat_var1}} != "Unemployed", 
           {{cat_var1}} != "Disabled", 
           {{cat_var1}} != "NA", 
           {{cat_var1}} != "Not sure", 
           {{cat_var1}} != "Other")
  print(bardub, n = 100)
  
  source_shapes <- c(16, 17, 15) 
  
  p <- bardub %>%
    ggplot(aes(x = factor({{cat_var1}}), y = perc, color = {{cat_var2}}, 
               group = {{cat_var2}}, shape = {{cat_var2}})) +  # Add shape aesthetic
    geom_pointrange(aes(ymin = lci, ymax = uci), position = position_dodge(width = 0.9), 
                    size = 1, fatten = 3, color = "black") +  # Change CI color to black
    geom_point(aes(color = {{cat_var2}}), position = position_dodge(width = 0.9), size = 5) +  # Increase point size
    geom_text(aes(label = labs, y = 0), position = position_dodge(width = 0.9), 
              vjust = 1.5, size = 2.5, color = "black") +  
    scale_color_manual(values = source_colors) +  # Use existing color palette
    scale_shape_manual(values = source_shapes) +  # Assign distinct shapes (circle, triangle, square) to sources
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
    geom_text(aes(label = paste0(round(perc), "%"), y = pmax(uci + 1, 0.5)),  
              position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5, color = "black") +  
    xlab(NULL) + ylab("Percent of Sample") + 
    theme_minimal() + 
    theme(legend.position = "bottom", legend.title = element_blank(), 
          axis.title.x = element_blank(), 
          legend.text = element_text(color = "black")) +  
    labs(caption = paste('Firefox Quant UR //', 'Updated on', today.date, sep = " "))
  print(p)
}


#multi-selects
multi <- function(data, suffix, ..., xlab = NULL, order = "desc", height = 8, width = 6, bg = "transparent",
                  desk_map = NULL, mob_map = NULL) {
  
  if (missing(suffix) || !is.character(suffix) || length(suffix) != 1) {
    stop("Please provide a single suffix string, e.g. 'deskfeat'")
  }
  
  dots <- enquos(...)
  if (length(dots)) {
    data <- data %>% filter(!!!dots)
  }
  
  data_long <- data %>%
    select(response_id = 1, ends_with(suffix)) %>%
    pivot_longer(-response_id, names_to = "feature", values_to = "response") %>%
    mutate(
      feature = str_replace(feature, paste0("_?", suffix, "$"), ""),
      response = factor(response)
    ) %>%
    filter(!is.na(response))
  
  total_respondents <- n_distinct(data_long$response_id)
  
  summary_df <- data_long %>%
    group_by(feature) %>%
    summarize(
      n_would = sum(response == "WOULD make me switch"),
      n_total = n(),
      perc_would = 100 * n_would / n_total,
      p = perc_would / 100,
      se = sqrt(p * (1 - p) / n_total),
      t = qt(0.975, n_total - 1),
      lci = perc_would - t * se * 100,
      uci = perc_would + t * se * 100,
      .groups = "drop"
    )
  
  label_map <- switch(suffix,
                      "deskfeat" = desk_map,
                      "mobfeat" = mob_map,
                      NULL)
  
  summary_df <- summary_df %>%
    mutate(
      feature_label = if (!is.null(label_map)) {
        map_chr(feature, ~ label_map[[.x]] %||% str_to_sentence(.x))
      } else {
        str_to_sentence(feature)},
      feature_label = paste0(feature_label, "\n(n=", n_total, ")"),
      feature_label = if (order == "desc") fct_rev(fct_reorder(feature_label, perc_would, .desc = TRUE)) else feature_label)
  
  plot <- ggplot(summary_df, aes(x = feature_label, y = perc_would, fill = feature_label)) +
    geom_col() +
    geom_errorbar(aes(ymin = pmax(0, lci), ymax = pmin(100, uci)), width = 0.15) +
    geom_text(aes(label = paste0(round(perc_would, 1), "%"), y = uci + 2), hjust = 0) +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 110)) +
    scale_fill_manual(values = rep(colors, length.out = nrow(summary_df))) +
    coord_flip() +
    labs(
      x = "Feature",
      y = "Percent 'WOULD make me switch'"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      axis.title.x = element_text(margin = margin(t = 15))
    )
  
  ggsave("plot.png", height = height, width = width, bg = bg, dpi = 300)
  
  print(plot)
  
  return(summary_df)
}





#MaxDiff  ----

maxdiff_props <- function(data) {
  data %>%
    pivot_longer(cols = starts_with("attribute"), names_to = "attribute_type", values_to = "attribute") %>%
    group_by(response_id, source, combination, attribute) %>%
    summarise(
      best_count = sum(best == attribute, na.rm = TRUE),
      worst_count = sum(worst == attribute, na.rm = TRUE),
      unselected_count = sum(!(best == attribute | worst == attribute), na.rm = TRUE),
      total_count = n(),
      .groups = "drop"
    ) %>%
    group_by(response_id, source, attribute) %>%
    summarise(
      total_best = sum(best_count),
      total_worst = sum(worst_count),
      total_unselected = sum(unselected_count),
      total_items = sum(total_count),
      proportional_score = (total_best - total_worst) / total_items,
      .groups = "drop"
    )
}



maxdiff_sum <- function(data) {
  by_source <- data %>%
    group_by(source, attribute) %>%
    summarise(
      avg_proportional_score = mean(proportional_score, na.rm = TRUE),
      std_error = sd(proportional_score, na.rm = TRUE) / sqrt(n()),
      lower_ci = avg_proportional_score - 1.96 * std_error,
      upper_ci = avg_proportional_score + 1.96 * std_error,
      
      pct_best = sum(total_best, na.rm = TRUE) / sum(total_items, na.rm = TRUE) * 100,
      pct_worst = sum(total_worst, na.rm = TRUE) / sum(total_items, na.rm = TRUE) * 100,
      pct_unselected = sum(total_unselected, na.rm = TRUE) / sum(total_items, na.rm = TRUE) * 100,
      
      se_best = sd(total_best / total_items * 100, na.rm = TRUE) / sqrt(n()),
      lower_best = pct_best - 1.96 * se_best,
      upper_best = pct_best + 1.96 * se_best,
      
      se_worst = sd(total_worst / total_items * 100, na.rm = TRUE) / sqrt(n()),
      lower_worst = pct_worst - 1.96 * se_worst,
      upper_worst = pct_worst + 1.96 * se_worst,
      
      se_unselected = sd(total_unselected / total_items * 100, na.rm = TRUE) / sqrt(n()),
      lower_unselected = pct_unselected - 1.96 * se_unselected,
      upper_unselected = pct_unselected + 1.96 * se_unselected,
      .groups = "drop"
    )
  
  full_sample <- data %>%
    group_by(attribute) %>%
    summarise(
      source = "Full Sample",
      avg_proportional_score = mean(proportional_score, na.rm = TRUE),
      std_error = sd(proportional_score, na.rm = TRUE) / sqrt(n()),
      lower_ci = avg_proportional_score - 1.96 * std_error,
      upper_ci = avg_proportional_score + 1.96 * std_error,
      
      pct_best = sum(total_best, na.rm = TRUE) / sum(total_items, na.rm = TRUE) * 100,
      pct_worst = sum(total_worst, na.rm = TRUE) / sum(total_items, na.rm = TRUE) * 100,
      pct_unselected = sum(total_unselected, na.rm = TRUE) / sum(total_items, na.rm = TRUE) * 100,
      
      se_best = sd(total_best / total_items * 100, na.rm = TRUE) / sqrt(n()),
      lower_best = pct_best - 1.96 * se_best,
      upper_best = pct_best + 1.96 * se_best,
      
      se_worst = sd(total_worst / total_items * 100, na.rm = TRUE) / sqrt(n()),
      lower_worst = pct_worst - 1.96 * se_worst,
      upper_worst = pct_worst + 1.96 * se_worst,
      
      se_unselected = sd(total_unselected / total_items * 100, na.rm = TRUE) / sqrt(n()),
      lower_unselected = pct_unselected - 1.96 * se_unselected,
      upper_unselected = pct_unselected + 1.96 * se_unselected,
      .groups = "drop"
    )
  
  bind_rows(by_source, full_sample)
}



maxdiff_pairwise <- function(data) {
  sources <- unique(data$source)
  results <- list()
  
  for (s in sources) {
    subset <- data %>%
      filter(source == s, !is.na(proportional_score))
    
    attributes <- unique(subset$attribute)
    pair_results <- list()
    
    for (i in 1:(length(attributes) - 1)) {
      for (j in (i + 1):length(attributes)) {
        attr1 <- attributes[i]
        attr2 <- attributes[j]
        
        group1 <- subset %>% filter(attribute == attr1) %>% pull(proportional_score)
        group2 <- subset %>% filter(attribute == attr2) %>% pull(proportional_score)
        
        t_res <- t.test(group1, group2)
        
        pair_results[[length(pair_results) + 1]] <- tibble::tibble(
          attribute_1 = attr1,
          attribute_2 = attr2,
          mean_1 = mean(group1, na.rm = TRUE),
          mean_2 = mean(group2, na.rm = TRUE),
          mean_diff = mean(group1, na.rm = TRUE) - mean(group2, na.rm = TRUE),
          t_statistic = unname(t_res$statistic),
          p_value = t_res$p.value,
          source = s,
          significantly_different = ifelse(t_res$p.value < 0.001, "yes", "no")
        )
      }
    }
    
    results[[s]] <- dplyr::bind_rows(pair_results)
  }
  
  subset_all <- data %>%
    filter(!is.na(proportional_score))
  
  attributes <- unique(subset_all$attribute)
  pair_results_all <- list()
  
  for (i in 1:(length(attributes) - 1)) {
    for (j in (i + 1):length(attributes)) {
      attr1 <- attributes[i]
      attr2 <- attributes[j]
      
      group1 <- subset_all %>% filter(attribute == attr1) %>% pull(proportional_score)
      group2 <- subset_all %>% filter(attribute == attr2) %>% pull(proportional_score)
      
      t_res <- t.test(group1, group2)
      
      pair_results_all[[length(pair_results_all) + 1]] <- tibble::tibble(
        attribute_1 = attr1,
        attribute_2 = attr2,
        mean_1 = mean(group1, na.rm = TRUE),
        mean_2 = mean(group2, na.rm = TRUE),
        mean_diff = mean(group1, na.rm = TRUE) - mean(group2, na.rm = TRUE),
        t_statistic = unname(t_res$statistic),
        p_value = t_res$p.value,
        source = "All",
        significantly_different = ifelse(t_res$p.value < 0.001, "yes", "no")
      )
    }
  }
  
  results[["All"]] <- dplyr::bind_rows(pair_results_all)
  
  dplyr::bind_rows(results)
}


maxdiff_anova <- function(data) {
  maxdiff_scores_df <- maxdiff_props(data)
  
  anova_results <- aov(proportional_score ~ source * attribute, data = maxdiff_scores_df)
  
  posthoc_results <- emmeans(anova_results, pairwise ~ source | attribute)
  
  summary(posthoc_results$contrasts) %>%
    dplyr::mutate(
      significantly_different = ifelse(p.value < 0.001, "yes", "no"),
      contrast = gsub("\\(|\\)", "", contrast)
    ) %>%
    tidyr::separate(contrast, into = c("source1", "source2"), sep = " - ") %>%
    dplyr::mutate(
      lower_ci = estimate - 1.96 * SE,
      upper_ci = estimate + 1.96 * SE
    ) %>%
    dplyr::select(
      attribute,
      source1,
      source2,
      estimate,
      SE,
      t.ratio,
      lower_ci,
      upper_ci,
      p.value,
      significantly_different
    )
}


max_percent <- function(data, metric = "pct_best", source_filter = NULL, n_top = 10,
                        width = 6, height = 9, bg = "transparent") {
  if (!(metric %in% c("pct_best", "pct_worst"))) stop("Invalid metric specified. Choose either 'pct_best' or 'pct_worst'.")
  ci_lower <- ifelse(metric == "pct_best", "lower_best", "lower_worst")
  ci_upper <- ifelse(metric == "pct_best", "upper_best", "upper_worst")
  
  filter_source <- ifelse(is.null(source_filter), "Full Sample", source_filter)
  
  data_filtered <- data %>%
    filter(source == filter_source) %>%
    slice_max(order_by = !!sym(metric), n = n_top)
  
  fill_colors <- if (filter_source == "Full Sample") {
    c("Full Sample" = "#A3C9F9")
  } else {
    source_colors
  }
  
  p <- ggplot(data_filtered, aes(x = reorder(attribute, !!sym(metric)), y = !!sym(metric), fill = source)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = !!sym(ci_lower), ymax = !!sym(ci_upper)), width = 0.2, position = position_dodge(width = 0.9)) +
    geom_text(aes(label = paste0(round(!!sym(metric), 1), "%"),
                  y = ifelse(!!sym(metric) > 0, !!sym(ci_upper), !!sym(ci_lower)),
                  hjust = ifelse(!!sym(metric) > 0, -0.2, 1.2)), size = 5, position = position_dodge(width = 0.9)) +
    scale_fill_manual(values = fill_colors) +
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal() +
    labs(
      title = bquote("Top Features by" ~ bold(.(filter_source))),
      x = "Browser Feature", y = "Percentage",
      caption = paste('Firefox Quant UR // Updated on', Sys.Date())
    ) +
    theme(legend.position = "none", axis.text.y = element_text(size = 13)) +
    coord_flip()    
  
  ggsave("plot.png", plot = p, dpi = 300, width = width, height = height, bg = bg)
  
  return(p)
}


max_stacked <- function(data, selected_source = NULL, n_top = NULL, 
                        width = 10, height = 6, bg = "transparent") {
  color_mapping <- list(
    "General Browser Users" = c("#FCE762", "#FEF29D", "#FEFBD8"),
    "Firefox Desktop" = c("#A59ED6", "#CBBEFA", "#F1ECFF"),
    "Firefox Mobile" = c("#FFB17A", "#FFCCA9", "#FFE2D3"),
    "Full Sample" = c("#A3C9F9", "#C6D9FF", "#E7F0FF")  # Your blue shades
  )
  
  colors <- if (!is.null(selected_source)) {
    color_mapping[[selected_source]]
  } else {
    color_mapping[["Full Sample"]]
  }
  
  plot_title <- ifelse(is.null(selected_source), "Full Sample", selected_source)
  
  if (!is.null(selected_source)) {
    data <- data %>% filter(source == selected_source)
  } else {
    data <- data %>% filter(source == "Full Sample")
  }
  
  
  if (!is.null(n_top)) {
    data <- data %>%
      arrange(desc(pct_best)) %>%
      slice_head(n = n_top)
  }
  
  data_long <- data %>%
    pivot_longer(cols = c(pct_best, pct_worst, pct_unselected), 
                 names_to = "selection_type", 
                 values_to = "percentage") %>%
    mutate(selection_type = factor(selection_type, levels = c("pct_best", "pct_worst", "pct_unselected"))) %>%
    group_by(attribute) %>%
    mutate(order = max(ifelse(selection_type == "pct_best", percentage, 0))) %>%
    ungroup() %>%
    arrange(order) %>%
    group_by(attribute) %>%
    mutate(cum_percentage = cumsum(percentage)) %>%
    ungroup()
  
  plot <- ggplot(data_long, aes(x = reorder(attribute, order), y = percentage, fill = selection_type)) +
    geom_bar(stat = "identity", width = 0.85, position = position_stack(reverse = TRUE)) +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 102)) +
    scale_fill_manual(values = colors, labels = c("pct_best" = "Want Most", "pct_worst" = "Want Least", "pct_unselected" = "Unselected")) +
    theme_minimal() + 
    coord_flip() +
    labs(title = bquote("MaxDiff Responses - " ~ bold(.(plot_title))), 
         x = "Browser Feature", y = "Percentage", fill = NULL) +
    theme(axis.text.y = element_text(size = 10),
          legend.position = "bottom",
          legend.text = element_text(size = 12)) +
    geom_text(aes(label = paste0(round(percentage, 1), "%"), y = cum_percentage - (percentage / 2)), 
              color = "black", size = 4.5) +
    labs(caption = paste('Firefox Quant UR // Updated on', Sys.Date()))
  
  print(plot)
  
  ggsave("plot.png", plot = plot, width = width, height = height, dpi = 300, bg = bg)
}


max_diverging <- function(data, 
                          sources = NULL,  
                          attributes = NULL,
                          n_top = NULL, 
                          n_bottom = NULL,
                          score_limits = NULL,
                          legend = FALSE,
                          order = "desc",
                          flip_coords = TRUE,
                          width = 6, 
                          height = 9, 
                          bg = "transparent") {
  
  clean_attribute <- function(x) x %>% str_replace_all("&", "and") %>% str_replace_all("-", " ") %>% str_squish() %>% str_trim()
  
  sorted_data <- if (!is.null(sources)) {
    if (!all(sources %in% unique(data$source))) stop("Some sources specified are not in the data.")
    data %>% filter(source %in% sources)
  } else data %>% filter(source == "Full Sample")
  
  sorted_data <- sorted_data %>% mutate(attribute_clean = clean_attribute(attribute))
  
  if (!is.null(attributes)) sorted_data <- sorted_data %>% filter(attribute_clean %in% clean_attribute(attributes))
  
  if (!is.null(n_top) | !is.null(n_bottom)) {
    score_summary <- sorted_data %>% group_by(attribute_clean) %>% summarise(avg_score = mean(avg_proportional_score, na.rm = TRUE), .groups = "drop")
    top_attr <- if (!is.null(n_top)) score_summary %>% arrange(desc(avg_score)) %>% slice_head(n = n_top) %>% pull(attribute_clean) else character(0)
    bottom_attr <- if (!is.null(n_bottom)) score_summary %>% arrange(avg_score) %>% slice_head(n = n_bottom) %>% pull(attribute_clean) else character(0)
    sorted_data <- sorted_data %>% filter(attribute_clean %in% union(top_attr, bottom_attr))
  }
  
  sorted_data <- sorted_data %>% mutate(attribute_clean = factor(attribute_clean, levels = {
    score_summary <- sorted_data %>% group_by(attribute_clean) %>% summarise(score = mean(avg_proportional_score, na.rm = TRUE))
    if (order == "asc") rev(arrange(score_summary, score) %>% pull(attribute_clean))
    else if (order == "desc") rev(arrange(score_summary, desc(score)) %>% pull(attribute_clean))
    else if (order == "abs") rev(arrange(score_summary, desc(abs(score))) %>% pull(attribute_clean))
    else rev(unique(sorted_data$attribute_clean))
  }))
  
  p <- ggplot(sorted_data, aes(x = attribute_clean, y = avg_proportional_score, fill = source)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.95)) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(width = 0.95)) +
    geom_text(aes(label = round(avg_proportional_score, 2),
                  y = if (flip_coords) ifelse(avg_proportional_score > 0, upper_ci, lower_ci) else ifelse(avg_proportional_score > 0, upper_ci + 0.02, lower_ci - 0.02),
                  hjust = if (flip_coords) ifelse(avg_proportional_score > 0, -0.2, 1.2) else 0.5),
              size = 3.5, position = position_dodge(width = 0.95)) +
    scale_fill_manual(values = if (is.null(sources)) c("Full Sample" = "#A3C9F9") else source_colors) +
    theme_minimal() +
    labs(title = if (is.null(sources)) bquote("MaxDiff Preference Scores - " * bold("Full Sample"))
         else bquote("MaxDiff Preference Scores - " * bold(.(paste(sources, collapse = " vs. ")))),
         x = "Browser Feature", y = "Average MaxDiff Score", fill = NULL,
         caption = paste('Firefox Quant UR // Updated on', Sys.Date())) +
    theme(legend.position = if (legend) "bottom" else "none", axis.text.y = element_text(size = 9, margin = margin(b = 20)))
  
  if (flip_coords) p <- p + coord_flip()
  if (!is.null(score_limits)) p <- p + ylim(score_limits[1], score_limits[2])
  
  ggsave("plot.png", plot = p, dpi = 300, width = width, height = height, bg = bg)
  p
}





max_dumbbell <- function(data, source1, source2,
                         n_top = 10,
                         attributes = NULL,
                         scale_limits = NULL,
                         width = 8, height = 10, bg = "transparent") {
  
  if (!all(c(source1, source2) %in% unique(data$source))) {
    stop("One or both sources specified are not in the data.")
  }
  
  comparison_data <- data %>%
    select(attribute, source, avg_proportional_score) %>%
    pivot_wider(names_from = source, values_from = avg_proportional_score) %>%
    mutate(gap = round(abs(.data[[source1]] - .data[[source2]]), 2))
  
  if (!is.null(attributes)) {
    comparison_data <- comparison_data %>%
      filter(attribute %in% attributes)
  } else {
    comparison_data <- comparison_data %>%
      slice_max(order_by = gap, n = n_top)
  }
  
  comparison_data <- comparison_data %>% arrange(desc(gap)) %>%
    filter(!is.na(.data[[source1]]) & !is.na(.data[[source2]]))
  
  avg_gap <- mean(comparison_data$gap)
  print(paste("Average absolute difference in MaxDiff scores:", round(avg_gap, 3)))
  
  p <- ggplot(comparison_data, aes(y = reorder(attribute, gap))) +
    geom_segment(aes(x = .data[[source1]], xend = .data[[source2]], yend = attribute),
                 color = "lightgray", linewidth = 3) +
    geom_vline(xintercept = 0, color = "black", linewidth = .5, linetype="dashed") +
    geom_point(aes(x = .data[[source1]], color = source1), size = 3.5) +
    geom_point(aes(x = .data[[source2]], color = source2), size = 3.5) +
    geom_text(aes(x = (.data[[source1]] + .data[[source2]]) / 2, label = paste("Δ", gap)),
              color = "black", size = 3.5, vjust = -0.8) +
    scale_color_manual(values = source_colors) +
    scale_y_discrete(expand = expansion(add = c(0.5, 0.5))) +
    labs(
      title = bquote("Preference Gaps: " ~ bold(.(source1)) ~ "vs." ~ bold(.(source2))),
      x = "MaxDiff Score", y = "Attribute",
      caption = paste('Firefox Quant UR // Updated on', Sys.Date())) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_blank()
    )
  
  if (!is.null(scale_limits)) {
    p <- p + scale_x_continuous(limits = scale_limits, 
                                breaks = pretty(scale_limits, n = 5), 
                                labels = scales::number_format(accuracy = 0.1))
  } else {
    p <- p + scale_x_continuous(limits = c(-0.6, 0.6), 
                                breaks = seq(-0.6, 0.6, by = 0.2), 
                                labels = scales::number_format(accuracy = 0.1))
  }
  
  ggsave("plot.png", plot = p, dpi = 300, width = width, height = height, bg = bg, units = "in")
  
  return(p)
}


  



#counts plot
max_raw <- function(data, selected_source = NULL, n_top = NULL, 
                    width = 10, height = 6, bg = "transparent") {
  
  color_mapping <- list(
    "General Browser Users" = c("#FCE762", "#FEF29D", "#FEFBD8"),
    "Firefox Desktop" = c("#A59ED6", "#CBBEFA", "#F1ECFF"),
    "Firefox Mobile" = c("#FFB17A", "#FFCCA9", "#FFE2D3"),
    "Full Sample" = c("#A3C9F9", "#C8DCFA", "#E3EEFD")  )
  
  if (!is.null(selected_source)) {
    data <- data %>% filter(source == selected_source)
    colors <- color_mapping[[selected_source]]
    plot_title <- bquote("MaxDiff Response Counts -" ~ bold(.(selected_source)))
  } else {
    colors <- color_mapping[["Full Sample"]]
    plot_title <- bquote("MaxDiff Response Counts -" ~ bold("Full Sample"))  }
  
  data_sum <- data %>%
    group_by(attribute) %>%
    summarise(
      count_best = sum(total_best, na.rm = TRUE),
      count_worst = sum(total_worst, na.rm = TRUE),
      count_unselected = sum(total_unselected, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (!is.null(n_top)) {
    data_sum <- data_sum %>%
      arrange(desc(count_best)) %>%
      slice_head(n = n_top)
  }
  
  data_long <- data_sum %>%
    pivot_longer(cols = starts_with("count_"),
                 names_to = "selection_type",
                 values_to = "count") %>%
    mutate(selection_type = factor(selection_type, 
                                   levels = c("count_best", "count_worst", "count_unselected"))) %>%
    group_by(attribute) %>%
    mutate(order = max(ifelse(selection_type == "count_best", count, 0))) %>%
    ungroup() %>%
    arrange(order) %>%
    group_by(attribute) %>%
    mutate(cum_count = cumsum(count)) %>%
    ungroup()
  
  plot <- ggplot(data_long, aes(x = reorder(attribute, order), y = count, fill = selection_type)) +
    geom_bar(stat = "identity", width = 0.85, position = position_stack(reverse = TRUE)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(values = colors,
                      labels = c("count_best" = "Want Most", "count_worst" = "Want Least", "count_unselected" = "Unselected")) +
    theme_minimal() +
    coord_flip() +
    labs(
      title = plot_title,
      x = "Browser Feature",
      y = "Response Count",
      fill = NULL,
      caption = paste('Firefox Quant UR // Updated on', Sys.Date())
    ) +
    theme(
      axis.text.y = element_text(size = 10),
      legend.position = "bottom",
      legend.text = element_text(size = 12)
    ) +
    geom_text(aes(label = count, y = cum_count - (count / 2)), 
              color = "black", size = 4.5)
  
  print(plot)
  
  ggsave("plot.png", plot = plot, width = width, height = height, dpi = 300, bg = bg)
}


#compare scores on a scatter plot
max_compare <- function(data, 
                        sources = c("General Browser Users", "Firefox Desktop"), 
                        n_top = 10, 
                        attributes = NULL, 
                        color = NULL,  # Now takes a hex code like "#FF6B6B"
                        width = 9, 
                        height = 9, 
                        bg = "transparent") {
  
  if (length(sources) != 2) {
    stop("Please provide exactly two sources for comparison.")
  }
  
  score_summary <- data %>%
    group_by(source, attribute) %>%
    summarise(
      avg_score = mean(proportional_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(source %in% sources)
  
  if (!is.null(attributes)) {
    top_attrs <- attributes
  } else {
    top_attrs <- score_summary %>%
      filter(source == sources[1]) %>%
      arrange(desc(avg_score)) %>%
      slice_head(n = n_top) %>%
      pull(attribute)
  }
  
  compare_data <- score_summary %>%
    filter(attribute %in% top_attrs) %>%
    pivot_wider(names_from = source, values_from = avg_score)
  
  colnames(compare_data)[2:3] <- c("score_1", "score_2")
  compare_data <- compare_data %>% drop_na()
  
  compare_data <- compare_data %>%
    mutate(
      label = paste0(attribute, " (", round(score_1, 2), ", ", round(score_2, 2), ")"),
      color = if (!is.null(color)) color else rep(colors, length.out = n())
    )
  
  plot <- ggplot(compare_data, aes(x = score_2, y = score_1)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_point(aes(color = color), size = 3, show.legend = FALSE) +
    geom_text_repel(aes(label = label), size = 3, max.overlaps = Inf) +
    scale_color_identity() +
    labs(
      x = paste("Preference Score -", sources[1]),
      y = paste("Preference Score -", sources[2]),
      caption = paste('Firefox Quant UR // Updated on', Sys.Date())
    ) +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      panel.grid = element_blank()
    ) +
    coord_equal(xlim = c(-0.6, 0.6), ylim = c(-0.6, 0.6))
  
  ggsave("plot.png", plot = plot, width = width, height = height, dpi = 300, bg = bg)
  print(plot)
}




#Plot effects of demo variables on maxdiff scores
max_effect <- function(maxdiff_scores, survey_all,
                       demographic = "genz",
                       demographic_values = c("Gen Z", "Older Generations"),
                       source = NULL,
                       n_top = 10,
                       attributes = NULL,
                       height = 6,
                       width = 8,
                       bg = "transparent",
                       filename = "plot.png",
                       flip_coords = FALSE,
                       title = NULL,
                       y_limits = NULL) {  # new argument
  
  demo_sym <- rlang::sym(demographic)
  
  survey_demo <- survey_all %>%
    select(response_id, source, !!demo_sym)
  
  maxdiff_scores_demo <- maxdiff_scores %>%
    left_join(survey_demo, by = c("response_id", "source"))
  
  if (!is.null(source)) {
    maxdiff_scores_demo <- maxdiff_scores_demo %>%
      filter(source == !!source)
  } else {
    maxdiff_scores_demo <- maxdiff_scores_demo %>%
      mutate(source = "Full Sample")
  }
  
  if (!is.null(title)) {
    title_label <- switch(tolower(title),
                          "firefox" = "Firefox Users",
                          "general" = "General Users",
                          "firefox desktop" = "Firefox Desktop",
                          "firefox mobile" = "Firefox Mobile",
                          "full sample" = "Full Sample",
                          title)
  } else {
    survey_name <- deparse(substitute(survey_all))
    title_label <- switch(survey_name,
                          "survey_desktop" = "Firefox Desktop",
                          "survey_mobile" = "Firefox Mobile",
                          "survey_all" = "Full Sample",
                          "Full Sample")
  }
  
  filtered <- maxdiff_scores_demo %>%
    filter(!is.na(!!demo_sym), !!demo_sym %in% demographic_values) %>%
    mutate(attribute_clean = attribute %>%
             str_replace_all("&", "and") %>%
             str_replace_all("-", " ") %>%
             str_squish() %>%
             str_trim())
  
  if (!is.null(attributes)) {
    top_attributes <- attributes
  } else {
    top_attributes <- filtered %>%
      group_by(attribute_clean) %>%
      summarise(mean_score = mean(proportional_score, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(mean_score)) %>%
      slice_head(n = n_top) %>%
      pull(attribute_clean)
  }
  
  filtered_top <- filtered %>%
    filter(attribute_clean %in% top_attributes)
  
  cat("\nDifference in Means Tests (", demographic, "):\n", sep = "")
  
  diff_tests <- lapply(unique(filtered_top$attribute_clean), function(att) {
    att_data <- filtered_top %>% filter(attribute_clean == att)
    group1 <- att_data %>% filter(!!demo_sym == demographic_values[1]) %>% pull(proportional_score)
    group2 <- att_data %>% filter(!!demo_sym == demographic_values[2]) %>% pull(proportional_score)
    
    if (length(group1) < 2 || length(group2) < 2 ||
        sd(group1, na.rm = TRUE) == 0 || sd(group2, na.rm = TRUE) == 0) {
      cat("\nAttribute:", att, "\n",
          "t-test = Not computed (insufficient data/variation)\n",
          "p value = NA\n",
          "Significant? = No\n", sep = "")
      return(tibble(
        attribute_clean = att,
        mean_diff = NA_real_,
        t_value = NA_real_,
        p_value = NA_real_,
        significant = "No"
      ))
    }
    
    test <- t.test(group1, group2)
    mean_diff <- mean(group1, na.rm = TRUE) - mean(group2, na.rm = TRUE)
    sig_flag <- ifelse(test$p.value < 0.001, "Yes", "No")
    
    cat(sprintf(
      "Attributes being compared: %s\nMean difference = %.3f\nt-test = %.3f\np value = %.4g\nSignificant? = %s\n\n",
      att,
      mean_diff,
      test$statistic,
      test$p.value,
      sig_flag
    ))
    
    tibble(
      attribute_clean = att,
      mean_diff = mean_diff,
      t_value = unname(test$statistic),
      p_value = test$p.value,
      significant = sig_flag
    )
  }) %>% bind_rows()
  
  summary <- filtered_top %>%
    group_by(attribute_clean, !!demo_sym) %>%
    summarise(
      avg_proportional_score = mean(proportional_score, na.rm = TRUE),
      std_error = sd(proportional_score, na.rm = TRUE) / sqrt(n()),
      lower_ci = avg_proportional_score - 1.96 * std_error,
      upper_ci = avg_proportional_score + 1.96 * std_error,
      .groups = "drop"
    )
  
  palette <- setNames(c("#563263", "#FFB17A", "#C2435F", "#DB7093"), demographic_values)
  
  title_text <- bquote("MaxDiff Preference Scores -" ~ bold(.(title_label)))
  
  p <- ggplot(summary, aes(x = reorder(attribute_clean, avg_proportional_score),
                           y = avg_proportional_score,
                           fill = !!demo_sym)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                  width = 0.2, position = position_dodge(width = 0.8)) +
    geom_text(
      aes(
        label = round(avg_proportional_score, 2),
        y = if (flip_coords) {
          ifelse(avg_proportional_score > 0, upper_ci, lower_ci)
        } else {
          ifelse(avg_proportional_score > 0, upper_ci + 0.02, lower_ci - 0.02)
        },
        hjust = if (flip_coords) {
          ifelse(avg_proportional_score > 0, -0.2, 1.2)
        } else {
          0.5
        }
      ),
      size = 3, position = position_dodge(width = 0.8)
    ) +
    scale_fill_manual(values = palette) +
    theme_minimal() +
    labs(title = title_text,
         x = "Browser Feature", y = "Average MaxDiff Score", fill = NULL,
         caption = paste('Firefox Quant UR // Updated on', Sys.Date())) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  if (!is.null(y_limits)) {
    if (flip_coords) {
      p <- p + xlim(y_limits)  # when flipped, x-axis is the score axis
    } else {
      p <- p + ylim(y_limits)
    }
  }
  
  if (flip_coords) p <- p + coord_flip()
  
  ggsave(filename, plot = p, dpi = 300, width = width, height = height, bg = bg)
  
  return(p)
}
