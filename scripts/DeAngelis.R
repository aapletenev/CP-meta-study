# List of libraries
libraries <- c("boot", "plyr", "ggplot2", "tidyr", "broom",
               "stringr", "dplyr", "ggpubr", "magrittr",
                "gdata", "purrr", "multiplex", "GGally",
               "parallel", "foreach", "doParallel", "tictoc", "zoo",
               "ggrepel", "forcats", 'ellipsis')



# Install and load libraries
#install.packages(libraries, dependencies = TRUE)
libs = lapply(libraries, require, character.only = TRUE)

data = read.csv("./data_studies/DeAngelis.csv")

#for is.na(Monkey) populate it with two first characters (could be numbers or letters) of the FILENAME
data %<>% mutate(Monkey = ifelse(is.na(Monkey), str_extract(FILENAME, "^[A-Za-z0-9]{2}"), Monkey))

#substitute  Nthresh==500 to NA
data %<>% mutate(Nthresh = ifelse(Nthresh == 500, NA, Nthresh))

#for each combination of Monkey*Study compute mean CP and itsSEM and geometric mean Nthresh/Pthresh
mean_CP = data %>% group_by(Study, Monkey) %>%
  summarise(mean_CP = mean(Cpgrand, na.rm = TRUE),
            SEM_CP = sd(Cpgrand, na.rm = TRUE)/sqrt(sum(!is.na(Cpgrand))),
            meanNthresh = median(Nthresh, na.rm = TRUE),
            meanPthresh = median(Pthresh, na.rm = TRUE),
            #geometric mean of the ratio of Nthresh and Pthresh
            mean_NP =  exp(mean(log(Nthresh/Pthresh), na.rm = TRUE)),
            N = n()) %>%
  mutate(text = paste0(round(mean_CP, 3), "(SEM=", round(SEM_CP, 4), ")"))

#now do the same but calculate spearman coeficient between Cpgrand and Nthresh for each Monkey*Study combination and the its p value
cor_CP_NP = data %>% group_by(Study, Monkey) %>%
    summarise(cor_CP_NP = cor(Cpgrand, Nthresh, method = "spearman", use = "complete.obs"),
                p_value = cor.test(Cpgrand, Nthresh, method = "spearman", use = "complete.obs")$p.value) %>%
    mutate(text = paste0(round(cor_CP_NP, 3), "(p=", round(p_value, 4), ")"))

#now do the same but calculate slope and intercept of the linear regression between Cpgrand and Pthresh/Nthresh for each Monkey*Study combination and the its 95% confidence intervals
 data %<>%
   left_join(mean_CP %>% select(Study, Monkey, meanNthresh, meanPthresh), by = c("Study", "Monkey")) %>%
   mutate(PN = Pthresh/Nthresh,
          PN_mean = meanPthresh/Nthresh)


# now do the same but with PN_mean instead of PN
lm_CP_NP = data %>%
  ddply(.(Study, Monkey), function(x) {
    model = lm(Cpgrand ~ PN_mean, data = x)
    tidy_model = tidy(model, conf.int = TRUE)

    df = data.frame(slope = tidy_model$estimate[2], slope_min = tidy_model$conf.low[2], slope_max = tidy_model$conf.high[2],
               intercept = tidy_model$estimate[1], intercept_min = tidy_model$conf.low[1], intercept_max = tidy_model$conf.high[1],
               R_squared = summary(model)$r.squared) %>%
      mutate(slope_text = paste0(round(slope, 4), " (", round(slope_min, 4), "; ", round(slope_max, 4), ")"),
               intercept_text = paste0(round(intercept, 3), " (", round(intercept_min, 3), "; ", round(intercept_max, 3), ")"))
    names(df) = paste(names(df), "mean", sep = "_")

    model = lm(Cpgrand ~ PN, data = x)
    tidy_model = tidy(model, conf.int = TRUE)

    df %<>% cbind(
      data.frame(slope = tidy_model$estimate[2], slope_min = tidy_model$conf.low[2], slope_max = tidy_model$conf.high[2],
               intercept = tidy_model$estimate[1], intercept_min = tidy_model$conf.low[1], intercept_max = tidy_model$conf.high[1],
               R_squared = summary(model)$r.squared) %>%
      mutate(slope_text = paste0(round(slope, 4), " (", round(slope_min, 4), "; ", round(slope_max, 4), ")"),
               intercept_text = paste0(round(intercept, 3), " (", round(intercept_min, 3), "; ", round(intercept_max, 3), ")"))
    )
  return(df)
  })



write.csv(lm_CP_NP, "./data_studies/DeAngelis_CP_NP.csv", row.names = FALSE)

#Now do scatter plot of Cpgrand vs PN_maen with linear regression line for each Monkey*Study combination, the color is study, the shape is Monkey
plot1_mean = ggplot(data, aes(x = PN_mean, y = Cpgrand, color = Study, shape = Monkey, group = paste(Study, Monkey))) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  labs(x = "P(median)/N ratio", y = "CP", title = "Based on median (over sessions) psychometric thresholds" ) +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") + theme(legend.position = "none")



hist(data$PN - data$PN_mean)

plot2_raw = ggplot(data, aes(x = PN, y = Cpgrand, color = Study, shape = Monkey, group = paste(Study, Monkey))) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  labs(x = "P/N ratio", y = "CP", title = "Based on session-based psychometric thresholds") +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black")


#now Pthreh vs Nthresh
plot_thresh = ggplot(data, aes(x = Pthresh, y = Nthresh, color = Study, shape = Monkey, group = paste(Study, Monkey))) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  labs(x = "Psych. thresh", y = "Neuro. thresh.") +
  theme(legend.position = "bottom") +
  #identity line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  #free scales
  facet_wrap(~Study, scales = "free")

ggplot(data, aes(x = Pthresh, y = Cpgrand, color = Study, shape = Monkey, group = paste(Study, Monkey))) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  labs(x = "Psych. thresh", y = "CP") +
  theme(legend.position = "bottom") +
  #identity line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  #free scales
  facet_wrap(~Study, scales = "free")





# scatter plot of slopes with horizontal and vertical errorbars based on lm_CP_NP mean and raw
plot_slopes = ggplot(lm_CP_NP, aes(x = slope_mean, y = slope, color = Study, shape = Monkey)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = slope_min, ymax = slope_max), width = 0.02) +
  geom_errorbarh(aes(xmin = slope_min_mean, xmax = slope_max_mean), height = 0.02) +
  theme_classic() +
  labs(x = "based on median (over sessions) psych. thresh.", y = "based on session-based psych. thresh.",
       title = "Slopes of regression between CP and P/N ratio (95%CI)") +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  #add identity line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  #make aspect ratio 1
  coord_equal() + theme(legend.position = "none")

#now do the same with R squared but without error bars
plot_R_squared = ggplot(lm_CP_NP, aes(x = R_squared_mean, y = R_squared, color = Study, shape = Monkey)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "based on median (over sessions) psych. thresh.", y = "based on session-based psych. thresh.", title = "R squared of linear regression between CP and P/N ratio") +
  theme(legend.position = "bottom") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  coord_equal() + theme(legend.position = "none")

library(patchwork)
library(cowplot)

PN_all <- (plot2_raw | plot1_mean) / ((plot_slopes|plot_R_squared) + plot_layout(nrow = 1, widths = c(1,1)))
# Add annotation and collect legends
PN_all <- PN_all +
  plot_layout(
    guides = 'collect'
  ) &
  theme(legend.position = "bottom",
        plot.margin = margin(t = 0, b = 10)) # Adds 20pt space above and below every plot)


print(PN_all)

ggsave("./data_studies/DeAngelis_CP_PN.pdf", PN_all, width = 12, height = 10)

lm_CP_NP_rest = data %>%
  ddply(.(Study, Monkey), function(x) {
    model = lm(Cpgrand ~ PN_mean, data = x)
    tidy_model = tidy(model, conf.int = TRUE)

    df = data.frame(slope = tidy_model$estimate[2], slope_min = tidy_model$conf.low[2], slope_max = tidy_model$conf.high[2],
               intercept = tidy_model$estimate[1], intercept_min = tidy_model$conf.low[1], intercept_max = tidy_model$conf.high[1],
               R_squared = summary(model)$r.squared) %>%
      mutate(slope_text = paste0(round(slope, 4), " (", round(slope_min, 4), "; ", round(slope_max, 4), ")"),
               intercept_text = paste0(round(intercept, 3), " (", round(intercept_min, 3), "; ", round(intercept_max, 3), ")"))
    names(df) = paste(names(df), "mean", sep = "_")

    model = lm(Cpgrand ~ PN, data = x)
    tidy_model = tidy(model, conf.int = TRUE)

    df %<>% cbind(
      data.frame(slope = tidy_model$estimate[2], slope_min = tidy_model$conf.low[2], slope_max = tidy_model$conf.high[2],
               intercept = tidy_model$estimate[1], intercept_min = tidy_model$conf.low[1], intercept_max = tidy_model$conf.high[1],
               R_squared = summary(model)$r.squared) %>%
      mutate(slope_text = paste0(round(slope, 4), " (", round(slope_min, 4), "; ", round(slope_max, 4), ")"),
               intercept_text = paste0(round(intercept, 3), " (", round(intercept_min, 3), "; ", round(intercept_max, 3), ")"))
    )

    model = lm(Cpgrand ~ PN + Pthresh, data = x)
    tidy_model = tidy(model, conf.int = TRUE)

    add = data.frame(slope = tidy_model$estimate[2], slope_min = tidy_model$conf.low[2], slope_max = tidy_model$conf.high[2],
                     slopeP = tidy_model$estimate[3], slopeP_min = tidy_model$conf.low[3], slopeP_max = tidy_model$conf.high[3],
               intercept = tidy_model$estimate[1], intercept_min = tidy_model$conf.low[1], intercept_max = tidy_model$conf.high[1],
               R_squared = summary(model)$r.squared) %>%
      mutate(slope_text = paste0(round(slope, 4), " (", round(slope_min, 4), "; ", round(slope_max, 4), ")"),
               intercept_text = paste0(round(intercept, 3), " (", round(intercept_min, 3), "; ", round(intercept_max, 3), ")"))

    names(add) = paste(names(add), "P", sep = "_")
    df %<>% cbind(add)
    return(df)
  })
