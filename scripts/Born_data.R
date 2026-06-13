# List of libraries
libraries <- c("boot", "R.matlab", "plyr", "ggplot2", "tidyr",
               "stringr", "dplyr", "ggpubr", "magrittr",
                "gdata", "purrr", "multiplex", "GGally",
               "parallel", "foreach", "doParallel", "tictoc", "zoo",
               "ggrepel", "forcats")

# Install and load libraries
#install.packages(libraries, dependencies = TRUE)
libs = lapply(libraries, require, character.only = TRUE)

#load functions from other project
source("/Users/apletenev/Desktop/Phd/Projects/probinf/CP_DP_Analysis/scripts/functions/functions for CP_bootstrap.R")

registerDoParallel(cores = detectCores()-1)

data = read.csv("bornlab_CP_data.csv") %>%
  mutate(monkey = case_when(grepl("2016", Session) ~ "U",
                            grepl("2017", Session) ~ "A17",
                            grepl("2019", Session) ~ "A19") %>%
                  factor(levels = c("U", "A17", "A19")),
         task_context = case_when(grepl("[A]", Task, fixed = TRUE) ~ "A",
                                  grepl("[B]", Task, fixed = TRUE) ~ "B",
                                  TRUE ~ Task)) %>%
  rename(CH = Electrode.., Lower = Lower...025.., Upper = Upper...975..) %>%
  mutate(SE = (Upper - Lower)/(2*1.96))


#count sessions
N_sessions = data %>%
  select(Session, Task, monkey, task_context) %>%
  unique() %>%
  count(monkey, task_context)

#count neurons
N_neurons = data %>%
  count(monkey, task_context)

N = N_sessions  %>%
  rename(n_ses = n) %>%
  left_join(N_neurons %>% rename(n_neur = n)) %>%
  mutate(result = paste0(n_neur, "(", n_ses, ")"))


#calculate mean CP
#repeat the same data.frame 1001 times to replicate the method but without taking into account trial uncertanty
data_boot = foreach(i = 1:1000, .combine = rbind) %dopar% {
  data %>% mutate(i = i)}

#now sample from normal distribution with mean = CP and SE = SE
data_boot %<>%
  mutate(CP = rnorm(n(), mean = CP, sd = SE)) %>%
  rbind(data %>% mutate(i = 0))



CP_boot =  data_boot %>%
  ddply(c("monkey","task_context", "i"), .parallel = TRUE, .paropts = list(.packages = c("dplyr","magrittr")),
            function(s) {
              if (s$i[1] != 0) {
                s %<>% boostrap_data(boot_var = "CH") %>%
                  boostrap_data(boot_var = "Session")
              }
              s
            })

CP_avg  = CP_boot %>%
    group_by(monkey,task_context, i) %>%
    summarise(CP_avg = mean(CP, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(n = i) %>%
    ddply(c("monkey","task_context"), get_CI, level = 0.95, var = "CP_avg", p_control = 0.5)

CP_avg %<>% mutate(across(c(median,CI_min, CI_max), ~round(.x,3)), result = paste0(median, "(", CI_min, "-", CI_max, ")"))

write.csv(CP_avg, "Mean_CP_results_Born.csv")

write.csv(N, "N_results_Born.csv")

