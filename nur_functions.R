require(tidyverse)

#palettes
##viridis <- rep(c("#FDE725", "#B8DE29", "#5DC863", "#21918C", "#3B528B", "#440A6D"), 20)
##cividis <- rep(c("#FFD700", "#FFEA00", "#FFE200", "#B8B8B8", "#808080", "#404040"), 20)
##temps <- rep(c("#FFE200", "#f9b641ff", "#f68f46ff","#de7065ff", "#b8627dff", "#7e4e90ff", "#593d9cff"), 20)
##neutral <- rep(c("#66545e",  "#a39193", "#aa6f73","#d59682", "#eea990","#f1c8a2", "#f6e0b5"), 20)
mountain <- rep(c("#ffa361ff",  "#ffe4ccff", "#352e44ff", "#d9c9b0ff", "#bf7c82ff"), 20)
mountain2 <- rep(c( "#352e44ff",  "#d9c9b0ff", "#ffa361ff", "#8d8d8d", "#ffe4ccff", "#bf7c82ff"), 20)

palette(mountain)

#date
today.date <- Sys.Date() 


#merge back in pii after telemetry join
pii <- function(desktop_orig_file, desktop_bq_file) {
  df_orig <- read.csv2(desktop_orig_file, sep = ",", stringsAsFactors = FALSE)
  df_bq <- read.csv2(desktop_bq_file, sep = ",", stringsAsFactors = FALSE)
  
  df_orig <- df_orig %>% rename_with(~ tolower(.) %>% str_replace_all("\\.|\\s+", "_"))
  df_bq <- df_bq %>% rename_with(~ tolower(.) %>% str_replace_all("\\.|\\s+", "_"))
  
  merged_df <- left_join(df_orig, df_bq, by = "response_id")
  
  return(merged_df)}



#merge two dfs with common columns
process <- function(df1_or_file, file2) {
  if (!is.data.frame(df1_or_file)) {
    df1 <- read.csv2(df1_or_file, sep = ",", stringsAsFactors = FALSE)
  } else {
    df1 <- df1_or_file}
  
  df2 <- read.csv2(file2, sep = ",", stringsAsFactors = FALSE)
  
  df1 <- df1 %>% rename_with(~ tolower(.) %>% str_replace_all("\\.|\\s+", "_"))
  df2 <- df2 %>% rename_with(~ tolower(.) %>% str_replace_all("\\.|\\s+", "_"))
  
  df1 <- df1 %>% select(-any_of("response_id"))
  df2 <- df2 %>% select(-any_of("response_id"))
  
  combined_df <- bind_rows(df1, df2) %>%  
    mutate(across(where(is.character), ~ replace(., . == "", NA))) %>% 
    select(-any_of(c("comments",
                     "legacy_comments",
                     "tags",
                     "branch",
                     "release_version",
                     "user_agent",
                     "status",
                     "referer",
                     "sessionid",
                     "time_started"))) %>% 
    ungroup() %>% 
    mutate(response_id = row_number())
  
  return(combined_df)}



#clean day-by-day hours active columns 
timevars <- function(df) {
  active_cols <- grep("^active_2025", names(df), value = TRUE)
  
  if (length(active_cols) == 0) return(df)
  
  active_dates <- as.Date(sub("active_", "", active_cols), format = "%Y_%m_%d")
  
  df %>%
    rowwise() %>%
    mutate(
      has_non_na = any(!is.na(c_across(all_of(active_cols)))),
      across(all_of(active_cols), 
             .names = "{.col}",
             .fns = function(val, col_name = cur_column()) {
               val_num <- suppressWarnings(as.numeric(val))
               this_date <- as.Date(sub("active_", "", col_name), format = "%Y_%m_%d")
               survey_date <- as.Date(date_submitted)
               
               if (!has_non_na) {
                 # All NA row: leave as is
                 return(val_num)
               } else {
                 # Has some non-NA: fill NAs with 0 if on/after date_submitted, else leave as NA
                 if (is.na(val_num)) {
                   if (!is.na(survey_date) && !is.na(this_date) && this_date >= survey_date) {
                     return(0)
                   } else {
                     return(NA_real_)
                   }
                 } else {
                   return(round(val_num, 5))
                 }
               }
             })
    ) %>%
    select(-has_non_na) %>%
    ungroup()}



#calculate rolling averages and retention on specific days calculated
rolling <- function(df) {
  active_cols <- grep("^active_2025", names(df), value = TRUE)
  
  if (length(active_cols) == 0) return(df)
  
  df$date_submitted <- as.Date(df$date_submitted)
  
  df <- df %>%
    rowwise() %>%
    mutate(
      active_day1_col = paste0("active_2025_", format(date_submitted, "%m_%d")),
      active_day2_col = paste0("active_2025_", format(date_submitted + 1, "%m_%d")),
      active_day1 = if (active_day1_col %in% active_cols) get(active_day1_col) else NA_real_,
      active_day2 = if (active_day2_col %in% active_cols) get(active_day2_col) else NA_real_
    ) %>%
    ungroup()
  
  # Function to get the relevant week columns
  week_sums <- function(submit_date, week_num) {
    week_start <- submit_date + (7 * (week_num - 1))
    week_days <- seq(week_start, week_start + 6, by = "day")
    week_days <- week_days[week_days >= submit_date]
    cols <- paste0("active_2025_", format(week_days, "%m_%d"))
    cols[cols %in% active_cols]
  }
  
  # Checking if any user has missing values across all active_2025_* columns
  df <- df %>%
    mutate(
      missing_active_2025 = rowSums(is.na(select(., starts_with("active_2025")))) == length(active_cols)
    )
  
  active_week1 <- numeric(nrow(df))
  active_week2 <- numeric(nrow(df))
  active_week3 <- numeric(nrow(df))
  active_week4 <- numeric(nrow(df))
  active_week5 <- numeric(nrow(df))
  
  # Loop through each row and compute weekly sums
  for (i in seq_len(nrow(df))) {
    submit_date <- df$date_submitted[i]
    
    week1_cols <- week_sums(submit_date, 1)
    week2_cols <- week_sums(submit_date, 2)
    week3_cols <- week_sums(submit_date, 3)
    week4_cols <- week_sums(submit_date, 4)
    week5_cols <- week_sums(submit_date, 5)
    
    active_week1[i] <- if (length(week1_cols) > 0) sum(as.numeric(df[i, week1_cols]), na.rm = TRUE) else NA_real_
    active_week2[i] <- if (length(week2_cols) > 0) sum(as.numeric(df[i, week2_cols]), na.rm = TRUE) else NA_real_
    active_week3[i] <- if (length(week3_cols) > 0) sum(as.numeric(df[i, week3_cols]), na.rm = TRUE) else NA_real_
    active_week4[i] <- if (length(week4_cols) > 0) sum(as.numeric(df[i, week4_cols]), na.rm = TRUE) else NA_real_
    active_week5[i] <- if (length(week5_cols) > 0) sum(as.numeric(df[i, week5_cols]), na.rm = TRUE) else NA_real_
  }
  
  # Assign the calculated values to the new columns
  df$active_week1 <- active_week1
  df$active_week2 <- active_week2
  df$active_week3 <- active_week3
  df$active_week4 <- active_week4
  df$active_week5 <- active_week5
  
  # Update columns to include missing logic for active_week columns
  df <- df %>%
    mutate(
      active_week1 = ifelse(missing_active_2025, NA_real_, active_week1),
      active_week2 = ifelse(missing_active_2025, NA_real_, active_week2),
      active_week3 = ifelse(missing_active_2025, NA_real_, active_week3),
      active_week4 = ifelse(missing_active_2025, NA_real_, active_week4),
      active_week5 = ifelse(missing_active_2025, NA_real_, active_week5),
      
      present_day1 = case_when(
        !is.na(active_day1) & active_day1 > 0 ~ 1,
        !is.na(active_day1) & active_day1 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      present_day2 = case_when(
        !is.na(active_day2) & active_day2 > 0 ~ 1,
        !is.na(active_day2) & active_day2 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      present_week2 = case_when(
        !is.na(active_week2) & active_week2 > 0 ~ 1,
        !is.na(active_week2) & active_week2 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      present_week4 = case_when(
        !is.na(active_week4) & active_week4 > 0 ~ 1,
        !is.na(active_week4) & active_week4 == 0 ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  return(df)
}


#convert to binary
binary <- function(data, var) {
  var <- rlang::ensym(var)  
  
  data %>%
    mutate(!!var := recode(!!var, "Yes" = 1, "No" = 0)) }

flags <- function(data, var) {
  var <- rlang::ensym(var)  
  
  data %>%
    mutate(!!var := recode(!!var, "true" = 1, "false" = 0)) }


#count items separated by semi-colons on a single column multi-select 
count <- function(data, column) {
  col_sym <- rlang::ensym(column)  
  new_col_name <- paste0(as.character(col_sym), "_count")
  
  none_strings <- tolower(c("None", "None of these", "None of the above"))
  
  data[[new_col_name]] <- sapply(
    strsplit(as.character(data[[as.character(col_sym)]]), ";"),
    function(x) {
      x_clean <- trimws(x[x != ""])  
      x_lower <- tolower(x_clean)
      
      if (length(x_clean) == 1 && x_lower %in% none_strings) {
        return(0)
      } else {
        return(length(unique(x_clean)))
      }
    }
  )
  
  return(data)
}


factormulti <- function(data, column) {
  col_sym <- rlang::ensym(column)
  col_name <- as.character(col_sym)
  new_col <- paste0(col_name, "_factor")
  
  data %>%
    mutate(
      # Split each value by semicolon
      !!new_col := str_split(!!col_sym, ";")
    ) %>%
    rowwise() %>%
    mutate(
      !!new_col := list({
        # Flatten the list, trim whitespace, and filter out empty values
        cleaned <- str_trim(unlist(!!rlang::sym(new_col)))
        cleaned <- cleaned[cleaned != "" & !is.na(cleaned)]
        
        # If "None" is present, set to "None"
        if (any(str_to_lower(cleaned) %in% c("none", "none of these", "none of the above"))) {
          "None"
        } else {
          cleaned <- sort(cleaned)  # Ensure consistent order
        }
        
        cleaned  # Return cleaned, sorted options
      })
    ) %>%
    ungroup() %>%
    mutate(
      !!new_col := sapply(!!rlang::sym(new_col), function(x) paste(x, collapse = "; ")),
      !!new_col := factor(
        !!rlang::sym(new_col),
        levels = {
          all_levels <- unique(unlist(.[[new_col]]))  # Flatten list to a vector
          all_levels <- sort(all_levels[all_levels != "None"])
          c("None", all_levels)  # Add "None" as first level
        }
      )
    )
}




#simple completion rate
completion <- function(df, first_var, last_var) {
  
  first_col <- deparse(substitute(first_var))
  last_col <- deparse(substitute(last_var))
  
  started <- sum(!is.na(df[[first_col]]))
  
  completed <- sum(!is.na(df[[last_col]]))
  
  completion_rate <- completed / started
  
  cat(sprintf("Completion Rate: %.1f%% (%d of %d respondents)\n",
              completion_rate * 100, completed, started))}


#falloff
falloff <- function(data, bg = "transparent", ...) {
  
  page_items <- c(...)
  
  falloff_data <- tibble(page = names(page_items),
                         item = page_items) %>%
    rowwise() %>%
    mutate(vars = strsplit(item, "\\|"),
           reached = list(
             data %>%
               mutate(reached = if_else(
                 rowSums(!is.na(across(all_of(vars)))) > 0, 1, 0)) %>%
               summarize(
                 n_reached = sum(reached),
                 n_total = n(),
                 pct_reached = n_reached / n_total,
                 se = sqrt(pct_reached * (1 - pct_reached) / n_total),
                 lower = pmax(0, pct_reached - 1.96 * se),
                 upper = pmin(1, pct_reached + 1.96 * se)))) %>%
    unnest_wider(reached) %>%
    mutate(falloff_rate = 1 - pct_reached,
           page_factor = factor(page, levels = page),
           labs = paste0(page, "\n(n=", n_reached, ")"))
  
  print(falloff_data)
  
  plot<-ggplot(falloff_data, aes(x = page_factor, y = pct_reached, fill = page_factor)) +
    geom_col() +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    geom_text(aes(y = upper +.02, label = scales::percent(pct_reached, accuracy = 0.1)),size = 4) +
    geom_line(aes(y = falloff_rate, group = 1),  linetype = "dashed") +
    geom_point(aes(y = falloff_rate), size=4, shape=1) +
    geom_text(aes(y = falloff_rate-0.02, label = scales::percent(falloff_rate, accuracy = 0.1)), size = 4, vjust=1) +
    scale_x_discrete(labels = falloff_data$labs) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       sec.axis = sec_axis(~ 1 - ., name = "Falloff Rate (%)", labels = scales::percent_format(accuracy = 1))) +
    scale_fill_manual(values=mountain) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
      legend.position = "none",
      axis.title.x = element_text(margin = margin(t = 15))) +
    labs(x = "Survey Page",
      y = "Completion Rate (%)",
      fill = "Page",
      caption = paste('Firefox Quant UR // Updated on', Sys.Date()))
  
  print(plot)
  
  ggsave(file = "plot.png", plot = plot, dpi = 300, height = 5, width = 9, bg = bg)}


#parse datetime, version, search engines
parse <- function(data, datetime_col = NULL, version_col = NULL, engine_col = NULL) {
  
  if (!is.null(datetime_col)) {
    datetime_col <- ensym(datetime_col)  
    
    data <- data %>%
      mutate(
        datetime_parsed = mdy_hms(!!datetime_col, tz = "UTC"),
        date_submitted = as.Date(datetime_parsed),
        time_submitted = format(datetime_parsed, "%I:%M %p") %>% tolower()) %>%
      select(-datetime_parsed) }
  
  if (!is.null(version_col)) {
    version_col <- ensym(version_col) 
    
    data <- data %>%
      mutate(
        firefox_version = !!version_col %>%
          str_extract("^\\d+") %>%  
          as.numeric() %>%  
          as.factor()  )  }
  
  if (!is.null(engine_col)) {
    engine_col <- ensym(engine_col)  
    
    data <- data %>%
      mutate(
        search_engine := case_when(
          str_detect(!!engine_col, regex("google", ignore_case = TRUE)) ~ "Google",
          str_detect(!!engine_col, regex("bing", ignore_case = TRUE)) ~ "Bing",
          str_detect(!!engine_col, regex("yahoo", ignore_case = TRUE)) ~ "Yahoo",
          str_detect(!!engine_col, regex("duckduckgo|ddg", ignore_case = TRUE)) ~ "DuckDuckGo",
          str_detect(!!engine_col, regex("baidu", ignore_case = TRUE)) ~ "Baidu",
          str_detect(!!engine_col, regex("yandex", ignore_case = TRUE)) ~ "Yandex",
          str_detect(!!engine_col, regex("qwant", ignore_case = TRUE)) ~ "Qwant",
          str_detect(!!engine_col, regex("ecosia", ignore_case = TRUE)) ~ "Ecosia",
          str_detect(!!engine_col, regex("startpage", ignore_case = TRUE)) ~ "Startpage",
          str_detect(!!engine_col, regex("seznam", ignore_case = TRUE)) ~ "Seznam",
          str_detect(!!engine_col, regex("naver", ignore_case = TRUE)) ~ "Naver",
          str_detect(!!engine_col, regex("sogou", ignore_case = TRUE)) ~ "Sogou",
          str_detect(!!engine_col, regex("aol", ignore_case = TRUE)) ~ "AOL",
          !!engine_col == "abc123" ~ NA_character_,
          !!engine_col == "null" ~ NA_character_,
          is.na(!!engine_col) | str_trim(!!engine_col) == "" ~ NA_character_,
          TRUE ~ "Other"),
        search_engine = factor(
          search_engine,
          levels = c("Google", "Bing", "Yahoo", "DuckDuckGo", "Qwant", "Ecosia", "Startpage",
                     "Naver", "Baidu", "Yandex", "Seznam", "AOL", "Other")) )}
  
  return(data)}




#bar plot
bar <- function(data, cat_var, filter_expr = NULL, xlab = NULL, height = 5, width = 9, bg = "transparent", order = "desc", ...) {
  cat_var <- rlang::enquo(cat_var)
  filter_expr <- rlang::enquo(filter_expr)
  
  if (!rlang::quo_is_missing(filter_expr) && !rlang::is_null(rlang::get_expr(filter_expr))) {
    data <- data %>% filter(!!filter_expr) }
  
  if (!missing(...)) {
    data <- data %>% filter(...)}
  
  data_processed <- data %>%
    filter(!is.na(!!cat_var), !!cat_var != "PNTS") %>%
    group_by(!!cat_var) %>% summarise(n = n(), .groups = "drop") %>% 
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
    scale_fill_manual(values = mountain) + 
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
    filter(!is.na(!!cat_var), !is.na(!!group_var), !!cat_var != "PNTS") %>%
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
    scale_fill_manual(values = mountain2) +
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
hist <- function(data, num_var, xlab = NULL, title = NULL, xlim_range = NULL, filter_expr = NULL, ...) {
  filters <- enquos(...)
  
  for (filter in filters) {
    data <- data %>%
      filter(eval_tidy(filter, data)) }
  
  if (!is.null(filter_expr)) {
    data <- data %>% filter(!!filter_expr)}
  
  summary_stats <- data %>%
    summarise(mean = mean({{num_var}}, na.rm = TRUE),
              sd = sd({{num_var}}, na.rm = TRUE),
              min = min({{num_var}}, na.rm = TRUE),
              max = max({{num_var}}, na.rm = TRUE))
  
  print(summary_stats)
  
  mean_data <- data.frame(mean = summary_stats$mean)
  
  plot <- data %>%
    filter(!is.na({{num_var}})) %>%
    ggplot(aes(x = {{num_var}})) +
    geom_histogram(aes(fill = ..count..), bins = 30) +
    scale_fill_gradientn(colors = mountain) +
    geom_vline(data = mean_data, aes(xintercept = mean), lwd = 0.5, linetype = "longdash") +
    geom_text(data = mean_data, aes(x = mean, y = Inf, label = paste("Mean =", round(mean))), angle = 90, vjust = -1.5, hjust = 1.1) +
    xlab(xlab) +
    ylab("Number of Users in Sample") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          legend.position = 'none') +
    labs(caption = paste('Firefox Quant UR //', 'Updated on', Sys.Date()),
         title = title)
  
  if (!is.null(xlim_range)) {
    plot <- plot + scale_x_continuous(limits = xlim_range)}
  
  ggsave(file = "plot.png", plot = plot, dpi = 300, height = 5, width = 9, bg = "transparent")
  
  return(plot)}



point <- function(data, cat_var, group_var, filter_expr, height = 5, width = 9, bg = "transparent", order = "original", xlab_text = NULL) {
  cat_var <- enquo(cat_var)
  group_var <- enquo(group_var)
  
  if (!missing(filter_expr)) {
    data <- data %>% filter({{ filter_expr }}) }
  
  data_processed <- data %>%
    filter(!is.na(!!cat_var), !is.na(!!group_var)) %>%
    count(!!group_var, !!cat_var) %>%
    group_by(!!group_var) %>%
    mutate(
      perc = n / sum(n) * 100,
      lci = perc - 1.96 * sqrt((perc * (100 - perc)) / sum(n)),
      uci = perc + 1.96 * sqrt((perc * (100 - perc)) / sum(n)),
      labs = paste0(as.character(!!group_var), "\n(n=", sum(n), ")") ) %>%
    ungroup()
  
  if (order == "asc") {
    data_processed <- data_processed %>%
      mutate(labs = factor(labs, levels = sort(unique(labs))))
  } else if (order == "desc") {
    data_processed <- data_processed %>%
      mutate(labs = factor(labs, levels = sort(unique(labs), decreasing = TRUE)))
  } else {
    data_processed <- data_processed %>%
      mutate(labs = factor(labs, levels = unique(labs)))}
  
  plot <- ggplot(data_processed, aes(x = labs, y = perc, color = !!cat_var)) +
    geom_point(size = 3, position = position_dodge(width = 0.7)) +
    geom_errorbar(
      aes(ymin = lci, ymax = uci),
      width = 0.1,
      position = position_dodge(width = 0.7)) +
    geom_text(
      aes(
        y = uci,
        label = paste0(round(perc, 1), "%"),
        group = !!cat_var),
      position = position_dodge2(width = 0.7, preserve = "single"),
      color = "black",
      size = 3,
      vjust = -2) +
    scale_y_continuous(
      labels = function(x) paste0(round(x, 1), "%"),
      limits = c(0, 100) ) +
    scale_color_manual(values = mountain) +
    ylab("Percent of Group") +
    xlab(xlab_text) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      axis.text.y = element_text(size = 10)) +
    labs(caption = paste("Firefox Quant UR // Updated on", Sys.Date()))
  
  ggsave("plot.png", plot = plot, height = height, width = width, bg = bg, dpi = 300)
  
  return(plot)}


#multi-selects
multi <- function(data, col, filter_condition = NULL, xlab = NULL, order = "original", height = 5, width = 9, bg = "transparent") {
  col_quo <- rlang::enquo(col) 
  
   if (!rlang::quo_is_null(rlang::enquo(filter_condition))) {
    data <- data %>% filter(!!rlang::enquo(filter_condition)) }
  
  col_name <- rlang::as_name(col_quo)
  count_col_name <- paste0(col_name, "_count")
  data[[count_col_name]] <- ifelse(
    is.na(data[[col_name]]) | data[[col_name]] == "na",
    0,
    stringr::str_count(data[[col_name]], ";") + 1 )
  
  respondents_answered <- data %>%
    select(response_id, !!col_quo) %>%
    filter(if_any(-response_id, ~ !is.na(.) & . != "na")) %>%
    distinct(response_id)
  
  total_respondents <- nrow(respondents_answered)
  
  data_processed <- data %>%
    select(response_id, !!col_quo) %>%
    pivot_longer(cols = !!col_quo, names_to = "variable", values_to = "value") %>% 
    filter(!is.na(value), value != "na") %>%
    separate_rows(value, sep = ";") %>%
    group_by(value) %>%
    summarize(n = n(), 
              total = total_respondents,
              perc = (n / total) * 100, p = perc / 100, 
              se = sqrt(p * (1 - p) / n),
              t = qt(0.975, n - 1),
              lci = perc - t * se * 100,
              uci = perc + t * se * 100) %>%
    ungroup() %>%
    mutate(cat_label = gsub("_", " ", value), 
           cat_label = str_to_sentence(cat_label), 
           cat_label = paste0(cat_label, "\n(n=", n, ")"))
  
  plot <- ggplot(data_processed,  aes(
    x = fct_reorder(cat_label, perc, .desc = (order =="asc")), 
    y = perc, fill = cat_label)) +
    geom_col() + 
    geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.15) + 
    scale_fill_manual(values = mountain2) +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
    geom_text(aes(label = paste0(round(perc), "%"), y = uci), hjust = -0.5) + 
    ylab("Percent of Sample") + 
    xlab(xlab) + 
    theme_minimal() + 
    theme(legend.position = "none", axis.title.x = element_text(margin = margin(t = 15)), axis.text.y = element_text(size = 9)) +
    coord_flip() +
    labs(caption = paste("Firefox Quant UR // Updated on", Sys.Date()))
  
  print(plot)
  
  ggsave(file = "plot.png", dpi = 300, height = height, width = width, bg = bg)
  
  return(data)  }



#correlation heatmaps
corr <- function(data, col, filter_condition = NULL, order = "original", title=NULL, height = 7, width = 7, bg = "transparent", method = "pearson") {
  require(reshape2)
  require(psych)
  
  col_quo <- rlang::enquo(col)  
  
  if (!rlang::quo_is_null(rlang::enquo(filter_condition))) {
    data <- data %>% filter(!!rlang::enquo(filter_condition)) }
  
  col_str <- rlang::as_label(col_quo)
  
  data_long <- data %>%
    select(response_id, !!col_quo) %>%
    filter(!is.na(.data[[col_str]]), .data[[col_str]] != "na") %>%
    separate_rows(!!col_quo, sep = ";") %>%
    mutate(value = str_trim(.data[[col_str]])) %>%
    group_by(response_id) %>%
    filter(!any(str_detect(value, regex("None|Unsure", ignore_case = TRUE)))) %>%
    ungroup() %>%
    distinct(response_id, value) %>%
    mutate(value = str_replace_all(value, "_", " "),
           value = str_to_sentence(value)) %>%
    mutate(dummy = 1) %>%
    pivot_wider(names_from = value, values_from = dummy, values_fill = list(dummy = 0))
  
  data_for_corr <- data_long %>% select(-response_id)
  
  if (method == "tetra") {
    corr_obj <- psych::tetrachoric(data_for_corr)
    corr_mat <- corr_obj$rho
    n_mat <- corr_obj$n
    cat("Sample sizes (N for each pair):\n")
    print(n_mat)
  } else {
    corr_mat <- cor(data_for_corr, method = method, use = "pairwise.complete.obs")
    n_mat <- outer(
      colSums(!is.na(data_for_corr)),
      colSums(!is.na(data_for_corr)),
      pmin)
    cat("Sample sizes (approximate pairwise N):\n")
    print(n_mat) }
  
  cat("Correlation matrix:\n")
  print(round(corr_mat, 3))
  
  corr_melt <- reshape2::melt(corr_mat)
  
  plot <- ggplot(corr_melt, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "#ffa361ff", mid = "white", high =  "#352e44ff", midpoint = 0, limit = c(-1,1), name = "Correlation") +
    geom_text(aes(label = round(value, 2)), size = 3, color = "black") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 9),
          axis.text.y = element_text(size = 9),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          legend.position = "right") +
    coord_fixed() +
    labs(title = title, caption = paste("Firefox Quant UR // Updated on", Sys.Date()))
  
  print(plot)

  ggsave(file = "plot.png", dpi = 300, height = height, width = width, bg = bg)}



ols <- function(data, dependent_var, predictors, ref_groups) {
  require(stats)
  require(broom)
  
  for (var in names(ref_groups)) {
    if (!is.factor(data[[var]])) {
      data[[var]] <- factor(data[[var]]) }
    data[[var]] <- relevel(data[[var]], ref = ref_groups[[var]]) }
  
  formula <- as.formula(paste(dependent_var, "~", paste(predictors, collapse = " + ")))
  
  model <- lm(formula, data = data)
  
  cat("Ordinary Least Squares Regression Summary:\n\n")
  print(summary(model))
  
  coefs <- summary(model)$coefficients
  ci <- confint(model)
  t <- coefs[, "Estimate"] / coefs[, "Std. Error"]
  p <- 2 * (1 - pt(abs(t), df = model$df.residual))
  
  stars <- cut(p,
               breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
               labels = c("***", "**", "*", ".", ""))
  
  result <- data.frame(
    term = rownames(coefs),
    Beta = round(coefs[, "Estimate"], 3),
    LCI = round(ci[, 1], 3),
    UCI = round(ci[, 2], 3),
    t = round(t, 3),
    p = round(p, 3),
    sig = stars)
  
  baseline <- lapply(predictors, function(var) {
    if (is.factor(data[[var]])) {
      ref_groups[[var]]
    } else {
      mean(data[[var]], na.rm = TRUE)
    }
  })
  names(baseline) <- predictors
  baseline_df <- as.data.frame(baseline)
  
  pred_baseline <- predict(model, newdata = baseline_df)
  
  pred_list <- lapply(result$term, function(term) {
    scenario <- baseline_df  # copy baseline
    
    if (term == "(Intercept)") {
      pred_changed <- pred_baseline  
    } else {
      matched <- FALSE
      for (var in predictors) {
        if (is.factor(data[[var]]) && startsWith(term, paste0(var))) {
          level_name <- sub(paste0(var), "", term)
          level_name <- sub("^", "", level_name)
          level_name <- trimws(level_name)
          level_name <- gsub("^\\(|\\)$", "", level_name)  
          scenario[[var]] <- level_name
          matched <- TRUE
          break
        }
        if (!is.factor(data[[var]]) && term == var) {
          scenario[[var]] <- scenario[[var]] + 1
          matched <- TRUE
          break
        }
      }
      pred_changed <- predict(model, newdata = scenario) }
    
    data.frame(
      term = term,
      baseline_pred = pred_baseline,
      changed_pred = pred_changed,
      predicted_change = pred_changed - pred_baseline  ) })
  
  predicted_values <- do.call(rbind, pred_list)
  
  final_table <- merge(result, predicted_values, by = "term", all.x = TRUE)
  
  cat("\nRegression Table with Predicted Values:\n\n")
  print(final_table)
  
  return(list(model = model, summary_table = final_table))
}




#logit
logit <- function(data, dependent_var, predictors, ref_groups) {
  for (var in names(ref_groups)) {
    if (!is.factor(data[[var]])) data[[var]] <- factor(data[[var]])
    data[[var]] <- relevel(data[[var]], ref = ref_groups[[var]]) }
  
  formula <- as.formula(paste(dependent_var, "~", paste(predictors, collapse = " + ")))
  model   <- glm(formula, data = data, family = binomial)
  
  cat("\nValid N for model:", nobs(model), "\n")
  
  coefs <- summary(model)$coefficients
  or    <- exp(coefs[, "Estimate"])
  ci    <- exp(confint.default(model))
  z     <- coefs[, "Estimate"] / coefs[, "Std. Error"]
  p     <- 2 * (1 - pnorm(abs(z)))
  stars <- cut(p, breaks = c(-Inf,0.001,0.01,0.05,0.1,Inf), labels = c("***","**","*",".",""))
  
  result <- data.frame(
    OR  = round(or,   3),
    LCI = round(ci[,1],3),
    UCI = round(ci[,2],3),
    z   = round(z,    3),
    p   = round(p,    3),
    sig = stars)
  
  cat("\nOdds Ratios with 95% CIs and Significance:\n"); print(result)
  
  ref_row <- data.frame(lapply(data[predictors], function(x)
    if(is.factor(x)) factor(levels(x)[1], levels=levels(x))
    else if(is.numeric(x)) mean(x, na.rm=TRUE)
    else x[1]), stringsAsFactors=FALSE)
  
  factor_level_probs <- data.frame()
  
  for(var in predictors) {
    if(is.factor(data[[var]])) {
      for(level in levels(data[[var]])) {
        tmp <- ref_row
        tmp[[var]] <- factor(level, levels=levels(data[[var]]))
        pr <- predict(model, newdata=tmp, type="response")
        rp <- predict(model, newdata=ref_row, type="response")
        se <- predict(model, newdata=tmp, type="response", se.fit=TRUE)$se.fit
        
        pp  <- pr * 100
        pc  <- (pr - rp) * 100
        lci <- (pr - 1.96*se) * 100
        uci <- (pr + 1.96*se) * 100
        
        factor_level_probs <- rbind(factor_level_probs, data.frame(
          variable              = var,
          level                 = level,
          predicted_probability = round(pp, 1),
          prob_change           = round(pc, 1),
          lower_ci              = round(lci, 1),
          upper_ci              = round(uci, 1) )) }
    }
    else if(is.numeric(data[[var]])) {
      tmp <- ref_row
      tmp[[var]] <- tmp[[var]] + 1
      pr <- predict(model, newdata=tmp, type="response")
      rp <- predict(model, newdata=ref_row, type="response")
      se <- predict(model, newdata=tmp, type="response", se.fit=TRUE)$se.fit
      
      pp  <- pr * 100
      pc  <- (pr - rp) * 100
      lci <- (pr - 1.96*se) * 100
      uci <- (pr + 1.96*se) * 100
      
      factor_level_probs <- rbind(factor_level_probs, data.frame(
        variable              = var,
        level                 = paste(var, "one-unit increase"),
        predicted_probability = round(pp, 1),
        prob_change           = round(pc, 1),
        lower_ci              = round(lci, 1),
        upper_ci              = round(uci, 1)
      )) }}
  
  cat("\nPredicted Probabilities and Change from Reference (percent):\n")
  print(factor_level_probs, row.names=FALSE)
  
  return(list(
    model                = model,
    odds_ratios          = result,
    predicted_probs_table= factor_level_probs,
    data_with_probs      = data
  ))}









#ordered logit
ologit <- function(data, dependent_var, predictors, ref_groups) {
  require(ordinal)
  require(broom)
  
  for (var in names(ref_groups)) {
    if (!is.factor(data[[var]])) {
      data[[var]] <- factor(data[[var]]) }
    data[[var]] <- relevel(data[[var]], ref = ref_groups[[var]]) }
  
  formula <- as.formula(paste(dependent_var, "~", paste(predictors, collapse = " + ")))
  
  model <- clm(formula, data = data)
  
  cat("Ordered Logistic Regression Summary:\n\n")
  print(summary(model))
  
  coefs <- summary(model)$coefficients
  or <- exp(coefs[, "Estimate"])
  ci <- exp(confint(model))
  z <- coefs[, "Estimate"] / coefs[, "Std. Error"]
  p <- 2 * (1 - pnorm(abs(z)))
  
  stars <- cut(p,
               breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
               labels = c("***", "**", "*", ".", ""))
  
  result <- data.frame(
    OR = round(or, 3),
    LCI = round(ci[, 1], 3),
    UCI = round(ci[, 2], 3),
    z = round(z, 3),
    p = round(p, 3),
    sig = stars)
  
  cat("\nOdds Ratios with 95% Confidence Intervals and Significance:\n\n")
  print(result)
  
  invisible(model)}



#multinomial logit 
mlogit <- function(data, dependent_var, predictors, ref_groups) {
  require(nnet)
  
  for (var in names(ref_groups)) {
    if (!is.factor(data[[var]])) {
      data[[var]] <- factor(data[[var]]) }
    data[[var]] <- relevel(data[[var]], ref = ref_groups[[var]]) }
  
  formula <- as.formula(paste(dependent_var, "~", paste(predictors, collapse = " + ")))
  
  model <- multinom(formula, data = data)
  coefs <- summary(model)$coefficients
  ses <- summary(model)$standard.errors
  
  outcome_levels <- rownames(coefs)
  for (i in seq_along(outcome_levels)) {
    level <- outcome_levels[i]
    est <- coefs[level, ]
    se <- ses[level, ]
    z <- est / se
    p <- 2 * (1 - pnorm(abs(z)))
    or <- exp(est)
    lci <- exp(est - 1.96 * se)
    uci <- exp(est + 1.96 * se)
    stars <- cut(p,
                 breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                 labels = c("***", "**", "*", ".", ""))
    
    results <- data.frame(
      Variable = names(est),
      OR = round(or, 3),
      LCI = round(lci, 3),
      UCI = round(uci, 3),
      z = round(z, 3),
      p = round(p, 3),
      sig = stars )
    
    cat("\nMultinomial Logistic Regression Results for outcome level:", level, "\n\n")
    print(results)}
  
  invisible(model)}



#plot predicted values from OLS regression
plot_pr <- function(result_table, variable, reference_level, width = 9, height = 5, bg = "transparent") {
  
  variable        <- deparse(substitute(variable))
  reference_level <- deparse(substitute(reference_level))
  
  var_effects <- result_table %>%
    filter(grepl(paste0("^", variable), term)) %>%
    mutate(level = trimws(sub(paste0("^", variable), "", term)))
  
  baseline_pred <- unique(result_table$baseline_pred)
  
  var_effects <- bind_rows(
    var_effects,
    data.frame(
      term            = paste0(variable, reference_level),
      Beta            = NA, LCI = NA, UCI = NA, t = NA, p = NA, sig = "",
      baseline_pred   = baseline_pred,
      changed_pred    = baseline_pred,
      predicted_change= 0,
      level           = reference_level,
      stringsAsFactors= FALSE))
  
  var_effects <- var_effects %>%
    mutate(
      predicted_value = baseline_pred + predicted_change,
      LCI_pred        = predicted_value + LCI,
      UCI_pred        = predicted_value + UCI)
  
  y_max  <- max(var_effects$UCI_pred, na.rm = TRUE) + 10
  offset <- 0.02 * y_max
  
  plot <- ggplot(var_effects, aes(x = reorder(level, predicted_value), y = predicted_value, fill = level)) +
    geom_col() +
    geom_errorbar(aes(ymin = LCI_pred, ymax = UCI_pred), width = 0.2) +
    geom_text(aes(
      y = ifelse(predicted_value == baseline_pred,
                 predicted_value + offset,
                 UCI_pred + offset),
      label = round(predicted_value, 1)), size = 4) +
    geom_hline(yintercept = baseline_pred, linetype = "dashed") +
    scale_fill_manual(values = mountain) +
    scale_y_continuous(limits = c(0, y_max + 5)) +
    labs(
      x = str_to_title(variable),
      y = "Predicted Value",
      caption = paste('Firefox Quant UR // Updated on', Sys.Date())) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 9),
          axis.title.x = element_text(margin = margin(t = 10)))
  
  ggsave("plot.png", plot = plot, height = height, width = width, bg = bg, dpi = 300)
  return(plot)}



#plot predicted probabilities from a logit regression
plot_probs <- function(result_table, variable, reference_level, x_label = NULL, width = 9, height = 5, bg = "transparent") {
  
  variable        <- deparse(substitute(variable))
  reference_level <- deparse(substitute(reference_level))
  
  # Subset the data for the given variable
  var_effects <- result_table %>%
    filter(variable == !!variable)
  
  # Find the full reference level based on the first word
  full_ref_match <- var_effects %>%
    filter(str_starts(level, reference_level)) %>%
    pull(level) %>%
    unique()
  
  if (length(full_ref_match) == 0) stop("Reference level not found.")
  if (length(full_ref_match) > 1) warning("Multiple levels match the reference_level; using the first one.")
  
  full_ref <- full_ref_match[1]
  
  baseline_prob <- var_effects %>%
    filter(level == full_ref) %>%
    pull(predicted_probability) %>%
    unique()
  
  if (! full_ref %in% var_effects$level) {
    var_effects <- bind_rows(
      var_effects,
      data.frame(variable              = variable,
                 level                 = full_ref,
                 predicted_probability = baseline_prob,
                 prob_change           = 0,
                 lower_ci              = baseline_prob,
                 upper_ci              = baseline_prob,
                 stringsAsFactors      = FALSE) )}
  
  y_max  <- max(var_effects$upper_ci, na.rm=TRUE) + 10
  offset <- 0.02 * y_max   
  
  plot <- ggplot(var_effects, aes(x = reorder(level, predicted_probability), 
                                  y = predicted_probability, fill = level)) +
    geom_col() +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
    geom_text(aes(y = upper_ci + offset,
                  label = paste0(round(predicted_probability, 1), "%")),
              size = 4, vjust = -0.5) +
    geom_hline(yintercept = baseline_prob, linetype = "dashed") +
    scale_fill_manual(values = mountain) +
    scale_y_continuous(limits = c(0, y_max + 5)) +
    labs(x = ifelse(!is.null(x_label), x_label, str_to_title(variable)),
         y = "Predicted Probability (%)",
         caption = paste('Firefox Quant UR // Updated on', Sys.Date())) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 9),
          axis.title.x = element_text(margin = margin(t = 10)))
  
  ggsave("plot.png", plot = plot, height = height, width = width, bg = bg, dpi = 300)
  
  return(plot)
}




#path modeling




#path model estimation and plots
path <- function(df, preds_key, ref_key, outcome, mediators, primary_iv, group_by = NULL, 
                 mediation_paths = NULL, var_label_cleanup = NULL, bootstrap = FALSE, n_bootstrap = 1000) {
  
  require(lavaan)
  require(dplyr)
  require(rlang)
  
  required_vars <- c(outcome, mediators, primary_iv)
  if (!is.null(group_by)) {
    required_vars <- c(required_vars, group_by)
  }
  all_vars <- c(required_vars, preds_key)
  
  if (!all(all_vars %in% names(df))) {
    stop("Not all required variables are in the dataframe")
  }
  
  df_clean <- df %>%
    filter(!is.na(!!sym(outcome)))
  
  for (iv in primary_iv) {
    df_clean <- df_clean %>%
      filter(!is.na(!!sym(iv)))
  }
  
  if (!is.null(group_by)) {
    df_clean <- df_clean %>%
      filter(!is.na(!!sym(group_by)))
  }
  
  for (mediator in mediators) {
    df_clean <- df_clean %>%
      filter(!is.na(!!sym(mediator)))
  }
  
  for (var in names(ref_key)) {
    if (is.factor(df_clean[[var]])) {
      ref_level <- ref_key[[var]]
      if (!(ref_level %in% levels(df_clean[[var]]))) {
        stop(paste("Reference level", ref_level, "not found in", var))
      }
      df_clean[[var]] <- relevel(df_clean[[var]], ref = ref_level)
    }
  }
  
  iv_formula <- paste(primary_iv, collapse = " + ")
  mediator_formula <- paste(mediators, collapse = " + ")
  
  outcome_formula <- paste(outcome, "~", iv_formula, 
                           if (length(mediators) > 0) paste("+", mediator_formula) else "")
  
  full_model <- outcome_formula
  if (!is.null(mediation_paths)) {
    full_model <- paste(c(outcome_formula, mediation_paths), collapse = "\n")
  }
  
  cat("\nModel Specification:\n", full_model, "\n")
  
  # Modify the model fitting to include bootstrapping if specified
  if (!is.null(group_by)) {
    if (bootstrap) {
      fit <- sem(full_model, data = df_clean, group = group_by, se = "bootstrap", bootstrap = n_bootstrap)
    } else {
      fit <- sem(full_model, data = df_clean, group = group_by)
    }
  } else {
    if (bootstrap) {
      fit <- sem(full_model, data = df_clean, se = "bootstrap", bootstrap = n_bootstrap)
    } else {
      fit <- sem(full_model, data = df_clean)
    }
  }
  
  # Print full summary
  print(summary(fit, standardized = TRUE, fit.measures = TRUE))
  
  # Standardized effect sizes for the regression paths (~)
  cat("\nEffect Sizes (Standardized Estimates):\n")
  
  effects_df <- parameterEstimates(fit, standardized = TRUE) %>%
    filter(op == "~")
  
  if ("group" %in% colnames(effects_df)) {
    effects <- effects_df %>%
      select(group, lhs, op, rhs, est, se, z, pvalue, std.all)
  } else {
    effects <- effects_df %>%
      select(lhs, op, rhs, est, se, z, pvalue, std.all)
  }
  
  print(effects)
  
  cat("\nStandardized Solution Table (full standardized paths):\n")
  
  effects_table <- as.data.frame(standardizedSolution(fit))
  
  if (nrow(effects_table) > 0) {
    print(effects_table)
  } else {
    cat("No standardized solution available.\n")
  }
  
  return(list(fit = fit, effects = effects, effects_table = effects_table))
}



#create long df with churn calculations for survival analysis
survival <- function(data, activity_prefix = "active_2025_", churn_window = 3) {
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(survival)
  
  activity_cols <- grep(paste0("^", activity_prefix), names(data), value = TRUE)
  
  missing_activity_ids <- data %>%
    filter(rowSums(is.na(select(., all_of(activity_cols)))) == length(activity_cols)) %>%
    pull(response_id)
  
  long_data <- data %>%
    select(response_id, all_of(activity_cols)) %>%
    pivot_longer(
      cols = starts_with(activity_prefix),
      names_to = "date_str",
      values_to = "active_hours"
    ) %>%
    mutate(date = as.Date(str_remove(date_str, "active_"), format = "%Y_%m_%d")) %>%
    arrange(response_id, date)
  
  setDT(long_data)
  long_data[, zero := active_hours == 0]
  
  long_data[, zero_run := rleid(zero), by = response_id]
  long_data[, run_length := .N, by = .(response_id, zero_run)]
  
  churn_dates <- long_data[zero == TRUE & run_length >= churn_window,
                           .(churn_date = min(date, na.rm = TRUE)), by = response_id]
  
  start_dates <- long_data[!is.na(active_hours), .(start_date = min(date)), by = response_id]
  
  last_active_dates <- long_data[!is.na(active_hours), .(last_active_date = max(date)), by = response_id]
  
  survival_data <- data %>%
    select(response_id) %>%
    left_join(start_dates, by = "response_id") %>%
    left_join(churn_dates, by = "response_id") %>%
    left_join(last_active_dates, by = "response_id") %>%
    mutate(
      event = !is.na(churn_date),
      observed_until = if_else(event, churn_date, last_active_date),
      days_observed = as.integer(observed_until - start_date)
    )
  
  survival_data <- survival_data %>%
    mutate(
      churn_date = ifelse(response_id %in% missing_activity_ids, NA, churn_date),
      start_date = ifelse(response_id %in% missing_activity_ids, NA, start_date),
      observed_until = ifelse(response_id %in% missing_activity_ids, NA, observed_until),
      days_observed = ifelse(response_id %in% missing_activity_ids, NA, days_observed),
      event = ifelse(response_id %in% missing_activity_ids, NA, event)
    )
  
  survival_data <- survival_data %>%
    filter(!response_id %in% missing_activity_ids)
  
  extra_vars <- data %>%
    select(-any_of(activity_cols))  
  
  survival_data <- left_join(survival_data, extra_vars, by = "response_id")
  
  survival_data$churn_date <- as.Date(survival_data$churn_date, origin = "1970-01-01")
  survival_data$start_date <- as.Date(survival_data$start_date, origin = "1970-01-01")
  survival_data$observed_until <- as.Date(survival_data$observed_until, origin = "1970-01-01")
  
  survival_data$surv_obj <- with(survival_data, Surv(days_observed, event))
  
  survival_data <- survival_data %>%
    mutate(
      time = surv_obj[, "time"],
      status = surv_obj[, "status"]
    )
  
  return(survival_data)
}


plot_survival_km <- function(survival_data,
                             output_path = "plot.png",
                             palette = "#FF6B6B",
                             bg = "transparent",
                             width = 8,
                             height = 7) {
  library(survival)
  library(survminer)
  library(gridExtra)
  library(grid)
  library(ggplot2)
  
  # Fit survival model
  fit <- survfit(surv_obj ~ 1, data = survival_data)
  
  # Extract the survival time and survival probabilities
  surv_summary <- summary(fit)
  times <- surv_summary$time
  surv_probs <- surv_summary$surv
  
  # Extract the median survival time
  median_time <- times[which.min(abs(surv_probs - 0.5))]
  
  # Calculate the differences between consecutive survival probabilities
  surv_diff <- diff(surv_probs)
  time_diff <- diff(times)
  
  # Steepest slope (largest absolute difference in survival probabilities)
  steepest_slope_index <- which.max(abs(surv_diff / time_diff))  # Max rate of change
  steepest_slope_time <- times[steepest_slope_index]  # Corresponding time
  
  # Level-off point (when slope change becomes minimal)
  level_off_time <- times[which.min(abs(surv_diff))]  # When slope becomes minimal
  
  # Print out key metrics
  cat("Median survival time:", round(median_time, 0), "days\n")
  cat("Steepest slope occurs at day:", round(steepest_slope_time, 0), "\n")
  cat("Slope levels off at day:", round(level_off_time, 0), "\n")
  
  # Create the survival plot with risk table
  ggsurv <- ggsurvplot(
    fit,
    data = survival_data,
    risk.table = TRUE,
    risk.table.height = 0.25,
    risk.table.y.text.col = TRUE,
    risk.table.y.text = FALSE,
    conf.int = TRUE,
    palette = palette,
    legend = "none",
    ggtheme = theme_minimal(),
    surv.median.line = "hv",
    xlab = "Days Since Firefox Profile Created",
    ylab = "Survival Probability"
  )
  
  # Add median time annotation to the plot
  ggsurv$plot <- ggsurv$plot + 
    annotate("text", 
             x = median_time, 
             y = 0.5, 
             label = paste("Median =", round(median_time, 0), "days"), 
             hjust = -0.1, 
             vjust = -1.5, 
             color = "black", 
             size = 4)
  
  # Caption in bottom right
  caption_grob <- textGrob(
    paste("Firefox Quant UR // Updated on", Sys.Date()),
    gp = gpar(fontsize = 10, fontface = "italic"),
    x = 1, hjust = 1,  # Right-aligned
    y = 0, vjust = 0   # Bottom-aligned
  )
  
  # Arrange the combined plot
  final_plot <- grid.arrange(
    ggsurv$plot,
    ggsurv$table,
    caption_grob,
    ncol = 1,
    heights = c(3.5, 1.5, 0.3)
  )
  
  # Save the plot with specified dimensions
  ggsave(output_path,
         plot = final_plot,
         width = width,
         height = height,
         dpi = 300,
         bg = bg)
  
  # Print the plot to the current graphics device
  print(final_plot)
}

plot_survival_by_group <- function(data,
                                   group_var,
                                   group_levels = NULL,
                                   time_var = "time",
                                   event_var = "event",
                                   output_path = "~/Desktop/plot.png",
                                   bg = "transparent",
                                   width = 8,
                                   height = 7) {
  library(survival)
  library(survminer)
  library(dplyr)
  library(ggplot2)
  library(gridExtra)
  library(grid)  # Needed for caption textGrob
  
  # Filter and prepare group variable
  data <- data %>%
    filter(!is.na(.data[[group_var]])) %>%
    mutate(group = as.character(.data[[group_var]]))
  
  if (!is.null(group_levels)) {
    data <- data %>%
      filter(group %in% group_levels) %>%
      mutate(group = factor(group, levels = group_levels))
  } else {
    data <- data %>%
      mutate(group = factor(group))
    group_levels <- levels(data$group)
  }
  
  # Fit survival curve
  surv_obj <- Surv(data[[time_var]], data[[event_var]])
  surv_fit <- survfit(surv_obj ~ group, data = data)
  surv_diff <- survdiff(surv_obj ~ group, data = data)
  
  # Extract summary
  surv_summary <- summary(surv_fit)
  medians <- summary(surv_fit)$table[, "median"]
  
  # Initialize vectors
  steepest_drop <- numeric(length(group_levels))
  level_off <- numeric(length(group_levels))
  
  # Calculate steepest drop and level-off day
  for (i in seq_along(group_levels)) {
    grp <- group_levels[i]
    sel <- surv_summary$strata == paste0("group=", grp)
    times <- surv_summary$time[sel]
    survs <- surv_summary$surv[sel]
    
    drops <- survs[-length(survs)] - survs[-1]
    if (length(drops) > 0) {
      idx <- which.max(drops)
      steepest_drop[i] <- times[idx + 1]
    } else {
      steepest_drop[i] <- NA
    }
    
    level_off[i] <- max(times, na.rm = TRUE)
  }
  
  # Print summary
  cat("Churn Summary by Group:\n")
  for (i in seq_along(group_levels)) {
    cat(sprintf("Group: %s\n", group_levels[i]))
    cat(sprintf("- Median churn day: %s\n", ifelse(is.na(medians[i]), "NA", medians[i])))
    cat(sprintf("- Steepest churn day: %s\n", ifelse(is.na(steepest_drop[i]), "NA", steepest_drop[i])))
    cat(sprintf("- Level-off day: %s\n\n", ifelse(is.na(level_off[i]), "NA", level_off[i])))
  }
  
  # Color palette
  if (exists("mountain2")) {
    colors <- mountain2
  } else {
    colors <- c("#FF6F61", "#6B8E23", "#8A2BE2", "#FFA500", "#20B2AA")
  }
  
  # Survival plot
  g <- ggsurvplot(
    surv_fit, data = data,
    risk.table = TRUE,
    legend.title = group_var,
    legend.labs = group_levels,
    palette = colors,
    conf.int = TRUE,
    ggtheme = theme_minimal(),
    risk.table.y.text.col = TRUE,
    risk.table.height = 0.25
  )
  
  # Ensure color match
  g$plot <- g$plot +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors)
  
  # Add p-value annotation
  chisq <- surv_diff$chisq
  df <- length(surv_diff$n) - 1
  p_val <- 1 - pchisq(chisq, df)
  p_label <- paste0("p = ", format.pval(p_val, digits = 3, eps = 0.001))
  
  g$plot <- g$plot + 
    annotate("text", x = Inf, y = Inf, label = p_label,
             hjust = 1.1, vjust = 1.5, size = 4.5, fontface = "italic")
  
  # Add caption
  caption_grob <- textGrob(
    label = paste("Firefox Quant UR // Updated on", Sys.Date()),
    x = unit(1, "npc"), hjust = 1,
    gp = gpar(fontsize = 8, fontface = "italic")
  )
  
  # Assemble final plot
  combined_plot <- arrangeGrob(g$plot, g$table, ncol = 1, heights = c(3, 1))
  final_plot <- arrangeGrob(combined_plot, bottom = caption_grob)
  
  # Print and save
  grid.newpage()
  grid.draw(final_plot)
  ggsave(output_path, plot = final_plot, width = width, height = height, dpi = 300, bg = bg)
  
  return(list(survdiff = surv_diff, survfit = surv_fit, plot = final_plot))
}





fit_cox_model <- function(data, preds_all, ref_complete, height = 6, width = 8, bg = "transparent") {
  library(survival)
  library(ggplot2)
  
  # Ensure 'surv_obj' exists in the dataset
  if (!"surv_obj" %in% names(data)) {
    stop("'surv_obj' variable is missing in the data. Please ensure the survival object is present.")
  }
  
  # Ensure all categorical variables are factors and check for any that only have one level
  for (var in names(ref_complete)) {
    # Check if the variable is character and convert it to a factor
    if (is.character(data[[var]])) {
      data[[var]] <- factor(data[[var]])
    }
    
    # Recode categorical variables to factors with reference levels as defined
    data[[var]] <- factor(data[[var]], levels = c(ref_complete[[var]], setdiff(levels(data[[var]]), ref_complete[[var]])))
    
    # Check if there is only one level in the factor
    if (length(unique(data[[var]])) == 1) {
      warning(paste("The variable", var, "only has one level in the dataset and will be excluded from the model."))
      preds_all <- preds_all[preds_all != var]  # Remove the variable from preds_all
    }
  }
  
  # If no valid predictors left, stop the function
  if (length(preds_all) == 0) {
    stop("No valid predictor variables left after checking levels.")
  }
  
  # Fit the Cox proportional hazards model with the specified predictors
  formula_str <- paste("surv_obj ~", paste(preds_all, collapse = " + "))
  cox_model <- coxph(as.formula(formula_str), data = data)
  
  # Print the summary of the model
  print(summary(cox_model))
  
  # Extract the coefficients and standard errors from the model
  coefs <- summary(cox_model)$coefficients
  
  # Use confint() to get the confidence intervals for the coefficients
  conf_intervals <- confint(cox_model)
  
  # Prepare a data frame for the forest plot
  plot_data <- data.frame(
    variable = rownames(coefs),
    coef = coefs[, "coef"],
    exp_coef = exp(coefs[, "coef"]),
    lower_ci = exp(conf_intervals[, 1]),  # Use exp() to get HR for CI
    upper_ci = exp(conf_intervals[, 2]),  # Use exp() to get HR for CI
    p_value = coefs[, "Pr(>|z|)"]
  )
  
  # Optional: Filter for significant variables if needed
  plot_data <- plot_data[!is.na(plot_data$p_value), ]  # Remove NA p-values
  
  # Filter out extreme estimates and CIs
  plot_data <- plot_data[plot_data$exp_coef <= 3 & plot_data$lower_ci <= 3 & plot_data$upper_ci <= 3, ]
  
  # Filter out "PNTS" levels from the plot data
  plot_data <- plot_data[!grepl("PNTS", plot_data$variable), ]
  
  # Modify the y-axis labels (extract levels and handle "Other" and "Not sure")
  plot_data$variable <- sapply(plot_data$variable, function(var) {
    # Extract the factor variable and its levels
    parts <- strsplit(var, "(?<=.)(?=[A-Z])", perl = TRUE)[[1]]
    
    # Get the main level (e.g., "Man" from "genderMan")
    main_level <- parts[length(parts)]
    
    # Check if the main level is "Other" or "Not sure"
    if (main_level %in% c("Other", "Not sure")) {
      # Append the abbreviated variable name to ensure uniqueness
      var_name <- gsub("(Other|Not sure)", "", var)
      main_level <- paste(main_level, gsub("_", "", var_name), sep = "_")
    }
    
    return(main_level)
  })
  
  # Plotting the forest plot
  forest_plot <- ggplot(plot_data, aes(y = reorder(variable, coef))) +
    # Plot points (exp(coef)) on the x-axis
    geom_point(aes(x = exp_coef, color = p_value < 0.05), size = 3) +  # Highlight significant coefficients
    # Plot error bars for confidence intervals on the x-axis
    geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2) +
    scale_color_manual(values = c("black", "#ffa361ff")) +  # Red for significant
    theme_minimal() +
    labs(
      x = "Hazard Ratio (exp(coef))",
      y = NULL,
      color = "Significance",
      caption = paste('Firefox Quant UR // Updated on', Sys.Date())  # Add the caption
    ) +
    theme(axis.text.y = element_text(size = 8), legend.position = "top") +
    # Adjust the x-axis with fewer breaks and format labels
    scale_x_continuous(
      breaks = seq(0, 2, by = 0.2),  # Adjusted breaks between 0 and 2
      labels = scales::number_format(scale = 1)  # Simple formatting without scientific notation
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels to avoid overlap
      panel.grid.major = element_blank(),  # Remove major gridlines
      panel.grid.minor = element_blank()   # Remove minor gridlines
    ) +
    # Zoom in on the x-axis by setting limits, adjusted to center around 1
    xlim(c(0, 2.5)) +  # Set limits to range from 0 to 2 for a more focused view
    # Add a dashed vertical line at x = 1
    geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 0.5)  # Dashed line at 1.0
  
  # Print the plot to the console
  print(forest_plot)
  
  # Save the plot as a PNG file
  ggsave("plot.png", plot = forest_plot, height = height, width = width, bg = bg, dpi = 300)
  
  # Return the Cox model object for further use
  return(cox_model)
}

