# List of libraries
libraries <- c("boot", "plyr", "ggplot2", "tidyr", "broom",
               "stringr", "dplyr", "ggpubr", "magrittr",
                "gdata", "purrr", "multiplex", "GGally",
               "parallel", "foreach", "doParallel", "tictoc", "zoo",
               "ggrepel", "forcats")



# Install and load libraries
#install.packages(libraries, dependencies = TRUE)
libs = lapply(libraries, require, character.only = TRUE)

#upload functions from other project
bootstrap_data = function(data, boot_var) { # boot_var - variable for which to bootstrap
  boot_var= sym(boot_var)
  boot_vars = data %>% pull(!!boot_var) %>% unique()
  boot_vars_boot = tibble(!!boot_var := sample(boot_vars, size = length(boot_vars), replace = TRUE)) %>%
    count(!!boot_var) %>%
    mutate(NBOOT = TRUE)

  data %<>% repeat_rows(boot_vars_boot)

  return(data)
}

repeat_rows = function(data, count_table) {
  data %>%
    left_join(count_table) %>%
    filter(!is.na(NBOOT)) %>%
    uncount(n) %>% #easy way to duplicate rows
    select(!NBOOT)
}

get_CI = function(data, level, var = "slope", p_control = 0, ONLY_i = FALSE) {
  if (!"n" %in% names(data)) data$n = data$i
  data$x = data[[var]]
  if(nrow(data %>% filter(i==0&n==0)) > 0) estimate = data %>% filter(i==0&n==0) %>% pull(!!sym(var)) else estimate = NA
  if (length(level) == 1) {
    level = c((1-level)/2,  level + (1 - level)/2)
  }
  if (ONLY_i) data %<>% filter(i!=0 & n==0) else data %<>% filter(i!=0 & n!=0)

  data %<>%  summarise(median = median(x, na.rm = TRUE), CI_min = quantile(x, probs = level[1], na.rm = TRUE) %>% as.numeric(),
                    CI_max = quantile(x , probs =level[2], na.rm = TRUE) %>% as.numeric(),
                    p_value_one_sided = sum(x < p_control, na.rm = TRUE)/sum(!is.na(x)),  x = estimate)

  data[[var]] =  data$x
  data %>% select(!x)
}

path = "./data_studies/Lange/"
files = list.files(path, pattern = "*.csv", full.names = TRUE)
#combine all files in one data frame adding column with file name
data = do.call(rbind, lapply(files, function(x) {
  df = read.csv(x)
  df$File = x
  return(df)
}))


#retreave the epoch from the file name - delete ./data_studies/Lange//cp_pm_neuro_data_ and .csv
data %<>% mutate(Epoch = str_replace(File, "./data_studies/Lange//cp_pm_neuro_data_", ""),
                   Epoch = str_replace(Epoch, ".csv", ""))

#change unit_id for the epoch that include 3 to 100*unit_id
data %<>% mutate(unit_id = ifelse(str_detect(Epoch, "3"), unit_id*100, unit_id))

#now replace 3 in epoch to 2 combining two arrays in one epoch for Appolo
data %<>% mutate(Epoch = str_replace(Epoch, "3", "2"),
                 Epoch = str_replace(Epoch, "AB", ifelse (task == "Cardinal", "A","B")))

data %>% count(subject, task, Epoch) %>% View()
data %>% group_by(subject, task, Epoch) %>% summarise(n = unique(session) %>% length())

#set up parallel backend to use many processors

mean_CP_boot = foreach(i = 1:100, .combine = rbind) %do% {
  #parallelize by subject, task and epoch
  data %>% ddply(.variables = c("subject", "task", "Epoch"), .fun = function(df) {
        df_boot = df %>%
          bootstrap_data("session") %>%
          bootstrap_data("unit_id")
        data.frame(i = i, mean_CP = mean(df_boot$cp))
  })
}

mean_CP = mean_CP_boot %>%
  group_by(subject, task, Epoch) %>%
  get_CI(level = 0.95, var = "mean_CP", p_control = 0.5, ONLY_i = FALSE)

#get the text as median(CI_min - CI_max)
mean_CP %<>% mutate(text = paste0(round(median, 3), " (", round(CI_min, 3), " - ", round(CI_max, 3), ")"))
mean_CP %<>% arrange(desc(subject))

####NP ratio#########

data %<>% mutate(DP_monkey = sqrt(2*pi)*pm_slope, #here assumption that no lapses
                 NP_ratio = DP_monkey/(neuro_dp/sqrt(2))
                 )

DP_monkey_median = data %>% select(subject, session, task, DP_monkey) %>% unique() %>%
  group_by(subject, task) %>% summarise(DP_monkey_epoch = median(DP_monkey, na.rm = TRUE))

data %<>% left_join(DP_monkey_median)
data %<>% mutate(NP_ratio_epoch = DP_monkey_epoch/(neuro_dp/sqrt(2)))

NP_ratio_epoch = data %>%
        group_by(subject, task, session) %>%
        summarise(NP_ratio_epoch = exp(mean(log(NP_ratio), na.rm = TRUE)),
                  N= n()) %>%
       group_by(subject, task) %>%
        summarise(NP_ratio_epoch = exp(mean(log(NP_ratio_epoch), na.rm = TRUE)),
                  N = sum(N))

write.csv(NP_ratio_epoch, "./data_studies/Lange/NP_ratio_epoch.csv", row.names = FALSE)
###now CP - PN slopes
data %<>% mutate(PN_mean = 1/NP_ratio_epoch)

lm_CP_PN = data %>%
      filter(!is.na(PN_mean) & !is.na(cp)) %>%
      ddply(.(subject, task), function(x) {
        model = lm(cp~ PN_mean, data = x)
        tidy_model = tidy(model, conf.int = TRUE)

        df = data.frame(slope = tidy_model$estimate[2], slope_min = tidy_model$conf.low[2], slope_max = tidy_model$conf.high[2],
                   intercept = tidy_model$estimate[1], intercept_min = tidy_model$conf.low[1], intercept_max = tidy_model$conf.high[1],
                   R_squared = summary(model)$r.squared) %>%
          mutate(slope_text = paste0(round(slope, 4), " (", round(slope_min, 4), "; ", round(slope_max, 4), ")"),
                   intercept_text = paste0(round(intercept, 3), " (", round(intercept_min, 3), "; ", round(intercept_max, 3), ")"))

      return(df)
      })

write.csv(lm_CP_PN, "./data_studies/Lange/CP_PN_slopes.csv", row.names = FALSE)