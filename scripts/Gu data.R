# List of libraries
libraries <- c("boot", "plyr", "ggplot2", "tidyr", "broom",
               "stringr", "dplyr", "ggpubr", "magrittr",
                "gdata", "purrr", "multiplex", "GGally",
               "parallel", "foreach", "doParallel", "tictoc", "zoo",
               "ggrepel", "forcats")

# Install and load libraries
#install.packages(libraries, dependencies = TRUE)
libs = lapply(libraries, require, character.only = TRUE)

#load data from csv
data = read.csv("./data_studies/Gu data.csv")
names(data)
psych_thresh = data.frame(Monkey = c(rep("A",3), rep("C", 3)), Condition = c("Combined",   "Visual",  "Vestibular", "Combined",   "Visual", "Vestibular"),
                          Psich.threshold = c(0.9,1.3, 1.2,2.1,3.2,3.1))

data %<>% left_join(psych_thresh)
data %<>%
  mutate(PN = Psich.threshold/Neural.Threshold, NP = Neural.Threshold/Psich.threshold)

data %>% count(Monkey, Condition)

#now foe each monkey and condition calculate the regression slope CP - PN and ouput the slope, p-value of slope, intercept, pvalue of intercept
CP_PN_reg = data %>% ddply(c("Monkey", "Condition"), function(s) {
                 lm(CP ~ PN, data = s) %>%
                 summary() %>%
                   tidy()
                    }) %>%
  mutate(Sign = case_when(p.value < 0.001 ~ "***",
                            p.value < 0.01 ~ "**",
                            p.value < 0.05 ~ "*",
                            TRUE ~ "ns"))

#slopes
CP_PN_reg %>%
  filter(term == "PN") %>%
  pull(estimate)

#intercepts
CP_PN_reg %>%
  filter(term == "(Intercept)") %>%
  pull(estimate)

#now plot CP - PN regression for each monkey and condition
data %>%
  mutate(MonkeyCondition = paste(Monkey, Condition, sep = "_")) %>%
  ggplot(aes(x = PN, y = CP, color = MonkeyCondition)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(se = FALSE) +
    facet_wrap(~ Monkey + Condition, scales = "free") +
  #add 0.5 horizontal line
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  labs(title = "CP vs PN by Monkey and Condition") +
  theme_minimal() +
  theme(legend.position = "bottom")

#now the same but CP - Neural.Threshold
data %>%
    mutate(MonkeyCondition = paste(Monkey, Condition, sep = "_")) %>%
    ggplot(aes(x = Neural.Threshold, y = CP, color = MonkeyCondition)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(se = FALSE) +
    facet_wrap(~ Monkey + Condition, scales = "free") +
    #add 0.5 horizontal line
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
    labs(title = "CP vs Neural Threshold by Monkey and Condition") +
    theme_minimal() +
    theme(legend.position = "bottom")


#now histogram of PN  for each monkey and condition
data %>%
  mutate(MonkeyCondition = paste(Monkey, Condition, sep = "_")) %>%
  ggplot(aes(x = PN, fill = MonkeyCondition)) +
  geom_histogram(binwidth = 0.1, position = "dodge") +
  facet_wrap(~ Monkey + Condition, scales = "free") +
  labs(title = "Histogram of PN by Monkey and Condition") +
  theme_minimal() +
  theme(legend.position = "bottom")

#now NP ratio
data %>%
  mutate(MonkeyCondition = paste(Monkey, Condition, sep = "_")) %>%
  ggplot(aes(x = NP, fill = MonkeyCondition)) +
  geom_histogram(binwidth = 0.1, position = "dodge") +
  facet_wrap(~ Monkey + Condition, scales = "free") +
  labs(title = "Histogram of NP by Monkey and Condition") +
  theme_minimal() +
  theme(legend.position = "bottom")



data_slope = data %>%
    group_by(Monkey, Condition) %>%
    summarise(meanCP = mean(CP, na.rm = TRUE), SEM = sd(CP, na.rm = TRUE)/sqrt(n()),
              meanPN = mean(PN, na.rm = TRUE),
              meanNP = mean(NP, na.rm = TRUE),
              meanNeuralThreshold = mean(Neural.Threshold, na.rm = TRUE),
              meanPsychThreshold = mean(Psich.threshold, na.rm = TRUE),
              meanPmeanN =   meanPsychThreshold/meanNeuralThreshold,
              inverse_meanNP = 1/meanNP
      ) %>%
  mutate(Papers = "Gu et al. (2008)", Brain.areas = "MST/MSTd",
         Epoch = case_when(
                    Condition == "Combined" ~ "1(combined cues, all neurons)",
                    Condition == "Visual" ~ "4(only visual, all neurons)",
                    Condition == "Vestibular" ~ "1(all neurons, vestibular stimulus)"
                  )) %>%
  mutate(Papers = ifelse(Condition == "Vestibular", "Gu et al.  (2007)", Papers) ) %>%
  left_join(CP_PN_reg %>% filter(term == "PN") ) %>%
  rename(Slope = estimate, std.error_slope = std.error, statistic_slope = statistic, p.value_slope = p.value, Sign_slope = Sign) %>%
  left_join(CP_PN_reg %>% filter(term == "(Intercept)") %>% select(!term)) %>%
  rename(Intercept = estimate)



#now add clery and nienborg papers
CP_PN_from_CC_CC = function(CC_CC) {
  #invert it as they do predicted - measured, but we want measured - predicted
  CC_CC = 1/CC_CC
  #convert CC - CC slope to CP - PN slope
  CP_PN = sqrt(2)/pi * CC_CC
  return(CP_PN)
}


#computing slopes from reported data
#Nienborg
CC_CC_N = CP_PN_from_CC_CC(2.5)
CC_CC_N_max = CP_PN_from_CC_CC(1.73)
CC_CC_N_min = CP_PN_from_CC_CC(4.1)
print(paste(round(CC_CC_N, 3), " (", round(CC_CC_N_min, 3),  "-", round(CC_CC_N_max, 3), ")", sep = ""))


#Clery
CC_CC_C = CP_PN_from_CC_CC(7.36)
CC_CC_C_max = CP_PN_from_CC_CC(2.69)
CC_CC_C_min = CP_PN_from_CC_CC(28.94)
print(paste(round(CC_CC_C, 3), " (", round(CC_CC_C_min, 3),  "-", round(CC_CC_C_max, 3), ")", sep = ""))

data_slope %<>%
  bind_rows(data.frame(Slope = c(CC_CC_C, CP_PN_N), Brain.areas = "V2", Monkey = c("two combined", "two combined"),
                       Epoch = c("1(all neurons)"), Papers = c("Clery et al. (2017)","Nienborg & Cumming (2006)")))

#save RDS
saveRDS(data_slope, "./data_studies/data_slope.rds")
