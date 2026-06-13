# List of libraries
libraries <- c("boot", "plyr", "ggplot2", "tidyr", "betareg",
               "stringr", "dplyr", "ggpubr", "magrittr",
               "lme4", "gdata", "purrr", "multiplex", "GGally", "grid",
               "parallel", "foreach", "zoo", "tidyverse", "stringr", "ggrepel",
               "ggbeeswarm", "FactoMineR", "Gifi", "missMDA", "Hmisc", "metafor", "patchwork", "car", "broom")

# Install and load libraries
#install.packages(libraries, dependencies = TRUE)
libs = lapply(libraries, library, character.only = TRUE)
#source functions
source("scripts/functions.R")




dir_out = "/Users/apletenev/Dropbox/Apps/Overleaf/Choice Probabilities Review/images/"

font_labels = 12
font_axis = 11
font_legend = 11
w = 7
h = 3


#######START###############
#options(contrasts = c("contr.sum", "contr.poly")) #Setting contr.sum ensures that categorical variables are tested relative to the overall mean rather than a reference group.
data = read.csv("CP data - technical.csv")

#remove empty rows, by conditioning on Main_epoch columns are empty ("" or NA)
data %<>% filter(!(is.na(Main.epoch)|Main.epoch == ""))

##populate Paper names
data %<>%
  mutate(Papers = na_if(Papers, "")) %>% # Convert empty strings to NA
  fill(Papers, .direction = "down") # Fill NA with preceding non-NA values

#change Paper names
data %<>% mutate(Papers = case_when(Papers == "Cook & Maunsell (2002) (mean DP  from Herrington et al. (2009))" ~ "Cook & Maunsell (2002)",
                                 Papers == "Nienborg & Cumming (2006) (partly reanalysed in Nienborg & Cumming (2007))" ~ "Nienborg & Cumming (2006)",
                                  Papers == "Chang et al. (2020), Doudlah et al.  (2022)" ~ "Chang et al. (2021)",  #average year of two papers as data from both of them
                                 TRUE ~ Papers))


#exclude non sensory areas VIP, LIP, AIP areas, but not CIP
data %<>% filter(!Brain.areas %in% c("VIP", "LIP", "AIP"))
data$Brain.areas %>% unique()
#data %>% filter(Brain.areas == "MT/MST") %>% View()
#data %>% filter(Brain.areas == "MT/MST") %>% View()
#data %>% filter(Brain.areas == "MST") %>% View()

#change  "V3a" area to "V3/V3A"; "MSTd" and "MST" to "MST/MSTd", "IT(TE)" to "IT"
data %<>% mutate(Brain.areas = case_when(Brain.areas %in% c("V3A", "V3/V3a") ~ "V3/V3A",
                                         Brain.areas == "MSTd" ~ "MST/MSTd",
                                         Brain.areas == "MST" ~ "MST/MSTd",
                                         Brain.areas == "IT(TE)" ~ "IT",
                                         TRUE ~ Brain.areas))

#replace "MT/MST" to "MT" but only for paper "Price & Born (2010)" as they are mostly consisted of MT neurons, and "V1/V2" to "V1" for paper "Goris et al. (2017)"
data %<>% mutate(Brain.areas = case_when(Papers == "Price & Born (2010)" & Brain.areas == "MT/MST" ~ "MT",
                                         Papers == "Goris et al. (2017)" & Brain.areas == "V1/V2" ~ "V1",
                                         TRUE ~ Brain.areas))
data$Brain.areas %>% unique()



#make longer table with one CP fieled
data %<>%
  pivot_longer(cols = c("Grand.CP..random.seeds", "CP..zero.signal...random.seeds", "Grand.CP..frozen.seeds", "CP..zero.signal...frozen.seeds"),
                             names_to = "method", values_to = "CP") %>%
  filter(CP != "") %>%
  mutate(method = gsub("...", ".", method, fixed = TRUE)) %>%
  mutate(method = gsub("..", ".", method, fixed = TRUE)) %>%
  mutate(Brain.areas = factor(Brain.areas, levels = c("V1","V2", "V4", "IT", "V3/V3A", "MT","MST/MSTd", "CIP")))

data %<>% mutate(method = factor(method, levels = c("Grand.CP.random.seeds", "Grand.CP.frozen.seeds", "CP.zero.signal.random.seeds", "CP.zero.signal.frozen.seeds")))

# #colors for brain areas
# color_brain = c(
#   "V1"       = "#E69F00",  # Orange
#   "V2"       = "#56B4E9",  # Sky Blue
#   "V3/V3A"   = "#009E73",  # Bluish Green
#   "MT"       = "#F0E442",  # Yellow
#   "MST/MSTd" = "#0072B2",  # Blue (distinct from Sky Blue)
#   "V4"       = "#D55E00",  # Vermilion
#   "IT"       = "#CC79A7",  # Reddish Purple
#   "CIP"      = "gray30"    # Dark Grey (Neutral but visible)
# )

color_brain = c(
  # Primary Visual Cortex (Source of both streams)
  "V1"       = "gray55",         # Neutral grey to anchor the hierarchy

  # Ventral / Form & Color (Cool Colors)
  "V2"       = "dodgerblue",     # Shifted your original blue here to start the stream
  "V4"       = "blueviolet",     # Shift to purple
  "IT"       = "darkmagenta",    # Deep purple/pink (End of stream)

  # Dorsal / Motion & Space (Warm/Mixed Colors)
  "V3/V3A"   = "seagreen3",      # Distinct Green
  "MT"       = "gold3",          # Dark Yellow/Gold
  "MST/MSTd" = "darkorange2",    # Orange
  "CIP"      = "firebrick"       # Dark Red (distinct from V2's blues)
)

shape_brain = c(
  # Ventral / Early
  "V1"       = 21,  # Circle
  "V2"       = 22,  # Square
  "V4"       = 23,  # Diamond
  "IT"       = 24,  # Triangle point up

  # Dorsal / Motion
  "V3/V3A"   = 22,  # Square
  "MT"       = 23,  # Diamond
  "MST/MSTd" = 24,  # Triangle point up
  "CIP"      = 25   # Triangle point down
)


#for CP split significance and find whether significant
data %<>% split_find_significance(var = "CP")

#check if coversion did not fail for some values
data %>% filter(!is.na(Sign) & is.na(SIGN)) %>% View()

#for Purushothaman & Bradley (2005) recaculate CP based on simple average of two epochs (neurons with upward and downward components), because they picked upward component due to the higher CP which biased estimate and have biased data so we cannot weight by number of neurons
data_P_B = data %>% filter(Papers == "Purushothaman & Bradley (2005)")
data_P_B %>% pull(Epoch)
data_P_B_main = data[which(data$Papers == "Purushothaman & Bradley (2005)" & data$Epoch == "1(test period, neurons with upward component)" & data$method == "Grand.CP.random.seeds"), ] %>%
                    mutate(Epoch = "0(test period, all neurons)", CP = 0.5*(data_P_B$CP[which(data_P_B$Epoch == "1(test period, neurons with upward component)" & data_P_B$method == "Grand.CP.random.seeds")] +
                                                                    data_P_B$CP[which(data_P_B$Epoch == "3(test period, neurons with downward component)")]),
                      Sign = NA,SIGN = NA, "Number.of.neurons..number.of.sessions." = 240 + 41)

data %<>% rbind(data_P_B_main)
data$Main.epoch[data$Papers == "Purushothaman & Bradley (2005)" & data$Epoch != "0(test period, all neurons)"] = 0


####add unique markers for each paper
N_papers = data$Papers %>% unique() %>% length()
custom_markers <- c(letters, LETTERS, as.character(1:(N_papers - 52))) # unique markers for each paper
#add shape parameter for each paper
data %<>% left_join(data.frame(Papers = data$Papers %>% unique()) %>% mutate(symbol = custom_markers, order = 1:N_papers))

###add new variables

#########Create all new variables##############
data %<>%
  rename(Non_task_parm = Non.task.parameters.of.stimulus.tailored.to.neurons, Task_parm = Task.parameters.of.stimulus.tailored.to.neurons, Stimulus_size = Stimulus.size.tailored.to.neurons..RF,
            St_duration = "Stimulus.duration.ms.", Time_window = "Time.window.for.CP.calc..ms.", Predict_targets = "Predictibility.of.choice.targets",
            Stimulus_pos = "Location.of.stimulus..deg.visual.angle.", Target1_pos = "Location.of.target.1.deg.visual.angle.", Target2_pos = "Location.of.target.2.deg.visual.angle.",
            method_pref = "How.neurons..preferences.were.estimated", RT_task = "Reaction.Time..RT..Task",
            Learning_before = Perceptual.learning.exposure.duration.before.data.collection, Learning_during = Perceptual.learning.exposure.duration.during.data.collection,
            CP_sensitivity_var = CP.sensitivity..variable.for.sensitivity, CP_sensitivity_meth = CP.sensitivity..how.measured, CP_sensitivity_value = CP.sensitivity..result..pvalue.,
            PN_slope = CP.P.N.ratio.slope, PN_intercept = CP.P.N.ratio.intercept) %>%
  mutate(Papers = gsub(x = Papers, pattern = "in prep", replacement = "2025"),
         #retrieve year of publication from Papers (all numbers)
    Year = gsub(x = Papers, pattern = "[^0-9]", replacement = "") %>% as.numeric(),
   RT_task = case_when(RT_task == "No" ~ "fixed time",
                                          RT_task == "Yes" ~ "RT") %>% factor(levels = c("fixed time", "RT"), ordered = TRUE),
    Recording= case_when (grepl("single", Recording.technique) ~ "single-electrode",
                                            grepl("Utah|Linear", Recording.technique) ~ "multi-electrode")
                                             %>% factor(levels = c("single-electrode", "multi-electrode")),
    Task_tailor = case_when(
                        Task_parm == "3 (not tailored)" & Non_task_parm == "3 (not tailored)" ~ "not fit / not fit",
                        Task_parm == "3 (not tailored)" & Non_task_parm == "4(opposite to preferences))" ~ "not fit / not fit", #opposite to preferences we will consider as not fit as too small number of points
                        Task_parm == "3 (not tailored)" & Non_task_parm == "2 (population of neurons)" ~ "not fit / population",
                        Task_parm == "2 (population of neurons)" & Non_task_parm == "3 (not tailored)" ~ "population / not fit",
                        Task_parm == "2 (population of neurons)" & Non_task_parm == "2 (population of neurons)" ~ "population / population",
                        Task_parm == "3 (not tailored)" & Non_task_parm == "1 (individual neuron)"  ~ "not fit / single neuron",
                        Task_parm == "1 (individual neuron)" & Non_task_parm == "3 (not tailored)" ~ "single neuron / not fit",
                        Task_parm == "1 (individual neuron)" & Non_task_parm == "2 (population of neurons)" ~ "single neuron / population",
                        Task_parm == "1 (individual neuron)" & (Non_task_parm == "1 (individual neuron)"| Non_task_parm == "")  ~ "single neuron / single neuron") %>%
                      factor(levels = c("not fit / not fit", "not fit / population", "population / not fit", "population / population", "not fit / single neuron", "single neuron / not fit", "single neuron / population", "single neuron / single neuron")),
    Task_parm = case_when(
      Task_parm == "3 (not tailored)" ~ "not fit",
      Task_parm == "2 (population of neurons)" ~ "population",
      Task_parm == "1 (individual neuron)" ~ "single neuron") %>% factor(levels = c("not fit", "population", "single neuron")),
    Non_task_parm = case_when(
      Non_task_parm %in% c("3 (not tailored)", "4(opposite to preferences))") ~ "not fit",
      Non_task_parm == "2 (population of neurons)" ~ "population",
      Non_task_parm == "1 (individual neuron)" ~ "single neuron") %>% factor(levels = c("not fit", "population", "single neuron")),
    Stimulus_size = case_when(Stimulus_size == "1 (individual neuron RF)" ~ "fit to RF",
                                                        Stimulus_size == "2 (population of neurons)" ~ "fit to population",
                                                        Stimulus_size == "3 (smaller than single neuron RF)" ~ "smaller than RF",
                                                        Stimulus_size == "not tailored/not applicable" ~ "not fit") %>%
                            factor(levels = c("not fit", "fit to population", "fit to RF", "smaller than RF")),
    Lapse.rate = Lapse.rate %>% gsub(pattern = "\\%|~|around",replacement = "") %>% delete_spaces() %>% as.numeric(),
    Task = case_when(Stimulus.type == "2 (bi stable)" ~ "bistable",
                                                        Task.type %in% c("1 (fine discrimination)")  ~ "fine-discrimination",
                                                        Task.type == "2 (coarse discrimination)" ~ "coarse-discrimination",
                                                        Task.type %in% c("3 (change detection)", "4 (detection)") ~ "detection") %>%
                      factor(levels = c("coarse-discrimination", "fine-discrimination", "detection", "bistable")),
    Task_var = case_when(grepl("coherence", Task.parameter) ~ "coherence",
                                                  grepl("orientation", Task.parameter) ~ "orientation",
                                                  grepl("direction", Task.parameter) ~ "direction",
                                                  grepl("near", Task.parameter) ~ "depth",
                                                  TRUE ~ "other") %>% factor(levels = c("direction", "depth", "orientation", "coherence", "other")),
    St_type = case_when(grepl("grating", Description.of.the.stimulus) ~ "grating",
                                                  grepl("stereogram", Description.of.the.stimulus) ~ "stereogram",
                                                  grepl("dot", Description.of.the.stimulus) ~ "moving dots",
                                                  TRUE ~ "other") %>% factor(levels = c("moving dots", "stereogram", "grating", "other")),
    Predict_targets = case_when( !Choice.made.by %in% c("Saccades", "not applicable") ~ "not saccades",
                                                      Predict_targets %in% c("not applicable")  ~ NA,
                                                      Predict_targets == "totally unpredictable " ~ "unpredictable",
                                                      Predict_targets %in% c("predictable but varied trial by trial", "varied block by block")  ~ "varied within session",
                                                      Predict_targets %in% c("varied session by session", "varied across multiple sessions") ~ "varied between sessions",
                                                      Predict_targets %in% c("Fixed for the whole period") ~ "fixed") %>%
                          factor(levels = c("not saccades", "unpredictable", "varied within session", "varied between sessions", "fixed")),
    method_pref = case_when(method_pref == "not applicable" ~ NA,
                                                method_pref =="from separate sessions" ~ "passive viewing",
                                                method_pref =="from task data" ~ "task itself") %>%
                                        factor(levels = c("passive viewing", "task itself")),
          N_neurons = before_bracket(Number.of.neurons..number.of.sessions.) %>% as.numeric(),
         Period = cut(Year, breaks = c(1995, 2005, 2015, 2024),
                      labels = c("1995-2005", "2006-2015", "2016-2024"))
)

#retrieve SEM or make its aproximation based on the assumption that CP is Gausian
data %<>%
  mutate(SEM = case_when(Sign_method == "SEM" ~ gsub(x = Sign, pattern = "SEM| |=", replacement = "") %>% as.numeric(),
                         Sign_method == "p-value" & grepl(x = Sign, pattern = "=") & CP != 0.5 ~
                           gsub(x = Sign, pattern = "p| |=|", replacement = "")  %>% as.numeric(), # when CP equals 0.5, SEM cannot be estimated from p-value
                         #Sign_method == "p-value" & grepl(x = Sign, pattern = "<") ~ gsub(x = Sign, pattern = "p| |<|", replacement = "") %>% as.numeric(), #this approximation give very rough estimate
                         Sign_method == "CI" ~ gsub(x = Sign, pattern = " ", replacement = "") %>%
                           str_split("-", simplify = TRUE)  %>%
                           apply(1, function(x) as.numeric(x[2]) - as.numeric(x[1])) %>% .[]/3.92 #CI is 95% so 1.96*SEM = CI, so SEM = CI/1.96
        ),
         #SEM = ifelse(Sign_method == "p-value" & Sign_method == "p-value" & grepl(x = Sign, pattern = "<") & SEM > 0.001, NA, SEM),
         SEM = ifelse( Sign_method == "p-value", abs(CP-0.5)/qnorm(1- SEM/2) , SEM)
  )

#split and find significance for CP sensitivity if exists
data %<>% split_find_significance(var = "CP_sensitivity_value", baseline = 0)


##Stimulus duraton

data %<>%
  mutate(St_duration = St_duration %>% delete_spaces() %>% as.numeric(),
         St_duration_raw = St_duration, #keep raw values for reference
         Time_window = Time_window %>% delete_spaces(),
         Time_window = ifelse(Time_window == "16-64", (16+64)/2, Time_window %>% as.numeric()),
         St_duration = pmax(Time_window, St_duration), #choose maximum between Time_window and St_duration
         St_duration = St_duration/1000
  )

#Task exposure
data %<>%
  mutate(across(c(Learning_before, Learning_during), ~  gsub(.x, pattern = "\\%|~|>",replacement = "") %>% delete_spaces() %>% str_split( "-", simplify = TRUE) %>% 
                                                                                                              apply( 2, as.numeric) %>% rowMeans(na.rm = TRUE))) %>%
  mutate(Learning = case_when(!is.na(Learning_before) & !is.na(Learning_during)  ~ Learning_before + Learning_during/2,
                                                    !is.na(Learning_before) & is.na(Learning_during)  ~ Learning_before,
                                                    is.na(Learning_before)  ~ NA))


#change it a bit as some categories are too wide and some are incorrect
data %<>% mutate(Task_var = case_when(
  Task.parameter == "direction of rotation" ~ "depth",
  Task.parameter == "direction of self motion" ~ "self motion",
  Task.parameter == "coherent motion" ~ "coherence",
  Task.parameter %in% c("slant ", "tilt") ~ "3D orientation",
  Task_var == "orientation" ~ "2D orientation",
  Task_var == "direction" ~ "2D motion direction",
  TRUE ~ Task_var) %>%
  factor(levels = c("2D motion direction", "depth", "2D orientation",  "self motion", "coherence", "3D orientation","other")))


#change the stimulus type to less subjective: moving dots, static dots, drifting gratings, static gratings, other
data %<>% mutate(St_type = case_when(
  grepl("moving dots", Description.of.the.stimulus, fixed = TRUE) ~ "moving dots",
  grepl("starfield", Description.of.the.stimulus, fixed = TRUE) ~ "moving dots",
  Description.of.the.stimulus == "moving dots stereogram" ~ "moving dots",
  grepl("drifting", Description.of.the.stimulus, fixed = TRUE) & Description.of.the.stimulus!= "drifting plaids with texture" ~ "drifting gratings",
  St_type == "grating" ~ "static gratings",
  St_type == "stereogram" ~ "static dots",
  TRUE ~ St_type) %>% factor(levels = c("moving dots", "static dots", "drifting gratings", "static gratings", "other")))


#retrieve only stimulus positions that start with "("
x = data$Stimulus_pos

data %<>% mutate(Stimulus_ecc = Stimulus_pos %>% get_eccentricity())

#add unique row ID
data %<>% mutate(ID = paste0("ID", 1:nrow(data)))


####CP-sensitivity slope within studies
data %<>%
  split_find_significance(var = "PN_slope", baseline = 0, CI_sep = ";") %>%
  split_find_significance(var = "PN_intercept", baseline = 0.5, CI_sep = ";")




####MAIN#####
#filter epochs for main analysis
data_main = data %>% filter(Main.epoch == 1)
data_main %<>% filter_best_method()
data_main %<>% arrange(Papers, Monkey, Brain.areas, Epoch)


#papers with data from monkeys combined
data_main %>%
  filter(grepl("combined", Monkey, fixed = TRUE)) %>% pull(Papers) %>% unique() %>% length()

data_main %>% count(Papers) %>% pull(n) %>% summary()

###exclude combined data
data_monkey = data_main  %>%
  filter(!Monkey %in% c("two combined", "two combined (L and N)", "two combined (L and O)", "three combined", "E and J"))  %>%
  group_by(Papers, Brain.areas, Epoch) %>%
  #filter all groups that have fewer than 2 monkeys
  filter(n_distinct(Monkey) > 1) %>%
  mutate(CP_monkey = CP - min(CP)) %>%  # normalize CP to the lowest value between monkeys
  arrange(CP_monkey) %>%
  #get rid of the first monkey
  slice(-1) %>%
  ungroup()

##########add P/N ratio
#find the data which could be added to the main data when no NP ratio is present in main data
data %>%  filter(N.P.ratio != "" & Main.epoch == 2) %>% View()

##adding data from Nienborg and Cumming (2006) and Kang & Maunsell (2020) as they have NP ratio but not in the main dataset
data_add = data %>%
  filter(N.P.ratio != "" & Main.epoch == 2 & Papers %in% c("Nienborg & Cumming (2006)", "Kang & Maunsell (2020)")) %>%
  filter_best_method()

data_NP = data_main %>%
  filter(N.P.ratio != "") %>%
  bind_rows(data_add) %>%
  rename(NP.ratio = N.P.ratio)

data_NP$NP.ratio %>% unique()
data_NP %<>% mutate(NP.ratio = ifelse(NP.ratio == ">>1", NA, NP.ratio) %>% delete_spaces() %>% before_bracket()) %>% filter(!is.na(NP.ratio)) #warning due to "N/A"

#now convert all NP.ratio to neuron*anti-neuron * sqrt(2)
#find all where Neurometric.threshold.method include sqrt(2)
data_NP  %<>% mutate(NP.ratio = ifelse(grepl("sqrt", Neurometric.threshold.method), NP.ratio, NP.ratio*sqrt(2)))

#add PN ratio as it is with it we expect linear relationship
data_NP %<>% mutate(PN.ratio = 1/NP.ratio, CC = (pi/sqrt(2)) * (CP - 0.5))

####CP -sensitivity correlation
data_CP_sens = data_main %>% bind_rows(data %>% filter(Main.epoch == 4)) #4 is for CP vs sensitivity within study when it absent for main epoch == 1


###Cohen and Newsome 2009 - need to take only one point!!!
data_CP_sens %<>%
  bind_rows(data %>% filter(Main.epoch == 2 & Papers %in% c("Britten et al. (1996)", "Kim et al.  (2015)","Elmore et al.  (2019)"))) #add data from #check that only one point is added and 2 for Elmore et al.  (2019)

#filter only with CP sensitivity
data_CP_sens %<>%
  filter(!is.na(CP_sensitivity_value)| !is.na(SIGN_CP_sensitivity_value)) %>%
  filter_best_method()


#now the value itself
#flipp CP sensitivity value if the var is "threshold"
data_CP_sens %<>%
  mutate(CP_sensitivity_value_flipped = case_when(CP_sensitivity_var == "threshold" ~ -CP_sensitivity_value,
                                         CP_sensitivity_var == "sensitivity" ~ CP_sensitivity_value,
                                         TRUE ~ CP_sensitivity_value),
         CP_sensitivity_val_higher_0 = CP_sensitivity_value_flipped > 0
  )

#for Lange et al. (2023) add CP_sensitivity_val_lower_0 - true for p< 0.5 and false for p>0.5, For Pletenev all are good
data_CP_sens %<>%
  mutate(CP_sensitivity_val_higher_0 = case_when(Papers == "Lange et al. (2023)"  ~ gsub("p| |=", "", Sign_CP_sensitivity_value)  %>% as.numeric() < 0.5,
                                                #Papers == "Liu et al. (2026)" ~ TRUE,
                                                TRUE ~ CP_sensitivity_val_higher_0
  ))



#########within study slopes
data_PN_slope = data_main %>% filter(!is.na(PN_slope))

data_add = data %>%
  filter(!is.na(PN_slope) & Main.epoch != 1 & Papers %in% c("Nienborg & Cumming (2006)", "Clery et al. (2017)")) %>%
  filter_best_method()

data_PN_slope %<>% bind_rows(data_add)

#calculate PN_slope SEM
data_PN_slope %<>%
  mutate(PN_slope_SEM = Sign_PN_slope %>% delete_spaces() %>%
    str_split(";", simplify = TRUE)  %>%
    apply(1, function(x) as.numeric(x[2]) - as.numeric(x[1])) %>% .[]/3.92
  )

#standartize slope
data_PN_slope %<>%
  mutate(PN_slope = ifelse(grepl("sqrt", Neurometric.threshold.method), PN_slope, PN_slope*sqrt(2)),
         PN_slope_SEM = ifelse(grepl("sqrt", Neurometric.threshold.method), PN_slope_SEM, PN_slope_SEM*sqrt(2)))


#now add slopes to data
data_NP_slope = data_NP %>% select(!c(PN_slope)) %>%
  left_join(data_PN_slope %>% dplyr::select(Papers, Epoch, Monkey, PN_slope, Brain.areas)) %>%
  mutate(x = PN.ratio, y = CP)

#for Clery  - slope is for two monkeys - so x is average of two monkeys and y = average CP
data_Clery = data_NP_slope %>% filter(Papers == "Clery et al. (2017)") %>%
  mutate(x = mean(x), y = mean(y)) %>%
  slice(1) %>%
  mutate(PN_slope = data_PN_slope %>% filter(Papers == "Clery et al. (2017)") %>% pull(PN_slope),
         Monkey = "two combined")

data_NP_slope %<>% filter(Papers != "Clery et al. (2017)") %>%
  rbind(data_Clery)


#compute scaling factor for the slope line
asp_x_y = diff(range(data_NP_slope$PN.ratio))/ diff(range(data_NP_slope$CP)) #scaling factor for x and y axis
data_NP_slope %<>% get_segments(slope_var = PN_slope, asp = asp_x_y)

#####CP methods
#add extended data where different CP method data was provided
data_CP = data %>%
  filter(Main.epoch == 1) %>%
  rbind(data %>% filter(Main.epoch == 2 & Papers %in% c("Britten et al. (1996)", "Dodd et al. (2001)", "Kim et al.  (2015)", "Elmore et al.  (2019)", "Palmer et al.  (2007)")))

#Palmer need to change epoch name to combine with zero signal
data_CP$Epoch[data_CP$Papers == "Palmer et al.  (2007)"] = "1(SU and MU)"


#Grand vs Zero signal
data_Grand = data_CP %>%
                  mutate(method_combined = case_when(method %in% c("Grand.CP.random.seeds", "Grand.CP.frozen.seeds") ~ "Grand CP",
                                            method %in% c("CP.zero.signal.random.seeds", "CP.zero.signal.frozen.seeds") ~ "Zero signal") %>%
                                              factor(levels = c("Zero signal", "Grand CP"), ordered = TRUE)) %>%
                  #if two methods for 1 combined, choose random seeds
                  group_by(Papers, Monkey, Brain.areas, Epoch, method_combined) %>%
                  arrange(method) %>%
                  slice(1) %>%
                  ungroup() %>%
                  group_by(Papers, Monkey, Brain.areas, Epoch) %>%
                  #filter only where more than 1 row
                  filter(n() > 1) %>%
                  ungroup()




###random seed vs frozen seeds
data_seeds = data_CP %>%
  mutate(method_combined2 = case_when(method %in% c("Grand.CP.random.seeds", "CP.zero.signal.random.seeds") ~ "Random seeds",
                                      method %in% c("Grand.CP.frozen.seeds", "CP.zero.signal.frozen.seeds") ~ "Frozen seeds") %>%
    factor(levels = c("Frozen seeds", "Random seeds"), ordered = TRUE)) %>%
  #if two methods for 1 combined, choose Grand CP
  group_by(Papers, Monkey, Brain.areas, Epoch, method_combined2) %>%
  arrange(method) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(Papers, Monkey, Brain.areas, Epoch) %>%
  #filter only where more than 1 row
  filter(n() > 1) %>%
  ungroup()







##################TEXT########################
####final regressions#####
model_all_PN_task_brain_st =  lm(CP ~ PN.ratio + Brain.areas + St_duration + Task, data = data_NP )
summary(model_all_PN_task_brain_st)

model_all_task_tail =  lm(CP ~ Task_parm + Non_task_parm + Stimulus_size + Brain.areas + St_duration  + Task, data = data_main)
summary(model_all_task_tail) #

#number of points in the joint regression
data_main %>% filter(!is.na(Task_parm) & !is.na(Non_task_parm) & !is.na(Stimulus_size) & !is.na(Brain.areas) & !is.na(St_duration) & !is.na(Task)) %>% nrow()
data_NP %>% filter(!is.na(PN.ratio) & !is.na(Brain.areas) & !is.na(St_duration) & !is.na(Task)) %>% nrow()

####Introduction and Data overview#####
nrow(data_main)
data_main$Papers %>% unique() %>% length()
data_main %>% count(method) %>% mutate(perc = n/sum(n)*100)


##summary statistics
data.frame(mean = mean(data_main$CP, na.rm = TRUE),
                         median = median(data_main$CP, na.rm = TRUE),
                         sd = sd(data_main$CP, na.rm = TRUE),
                         IQR_lower = quantile(data_main$CP, probs = 0.25, na.rm = TRUE),
                         IQR_upper = quantile(data_main$CP, probs = 0.75, na.rm = TRUE),
                         #95% CI
                        CI_lower = quantile(data_main$CP, probs = 0.025, na.rm = TRUE),
                        CI_upper = quantile(data_main$CP, probs = 0.975, na.rm = TRUE),
                         N = nrow(data_main))



##number of significant data points, CP > 0.5, CP < 0.5
#find papers that do not report significance of the CP best method
Papers_no_SIGN = data_main %>% filter(is.na(SIGN)) %>% pull(Papers) %>% unique()
Papers_yes_SIGN = data_main %>% filter(!is.na(SIGN)) %>% pull(Papers) %>% unique()
Papers_no_SIGN = Papers_no_SIGN[!Papers_no_SIGN %in% c(Papers_yes_SIGN, "Purushothaman & Bradley (2005)")] #exclude "Purushothaman & Bradley (2005)" as we combined the values and realyy do not know the significance




data_main %>% bind_rows(data %>%  filter(Main.epoch == 2 & Papers %in% c("Bondy et al. (2018)", "Nienborg & Cumming (2006)", "Kang & Maunsell (2020)") & !is.na(SIGN))) %>% # add second epoch for these papers that report significance on combined data
  bind_rows(data %>% filter((Papers == "Goris et al. (2017)" & Main.epoch == 1) & !is.na(SIGN))) %>% # add data from Goris et al. (2017) from zero signal trials as only there significance is reported
  filter(!is.na(SIGN)) %>% summarise(N = n(),
                                                N_sign = sum(SIGN == TRUE),
                                                N_not_sign = sum(SIGN == FALSE),
                                                N_lower_0.5 = sum(CP < 0.5 & SIGN == TRUE),
                                                N_higher_sign_0.5 = sum(CP > 0.5 & SIGN == TRUE),
                                                P_sign_higher = N_higher_sign_0.5/N
  )

#Papers that have negative significant CP
data_main %>% bind_rows(data %>%  filter(Main.epoch == 2 & Papers %in% c("Bondy et al. (2018)", "Nienborg & Cumming (2006)", "Kang & Maunsell (2020)") & !is.na(SIGN))) %>% # add second epoch for these papers that report significance on combined data
  bind_rows(data %>% filter((Papers == "Goris et al. (2017)" & Main.epoch == 1) & !is.na(SIGN))) %>%
  filter(!is.na(SIGN) & CP < 0.5 & SIGN == TRUE) %>% View()


#####Between monkeys comparison

mean_monkey_diff = mean(data_monkey$CP_monkey)
mean_monkey_diff
data_monkey$CP_monkey %>% summary()

#now mean diff between all CP values
# Number of values
n <- length(data_main$CP)
# Compute average absolute difference across all unique pairs
up_tri =  upper.tri(diag(nrow(data_main)))
mean_pairwise_abs_diff <- mean(abs(outer(data_main$CP, data_main$CP, "-")[up_tri]))

#now compute ratio
100*mean_monkey_diff / mean_pairwise_abs_diff



###year
lm(CP ~  Year, data = data_main) %>% summary()

#full model tailoring
all_tailor_year = lm(CP ~ Year + Task + Brain.areas + St_duration + Task_parm + Non_task_parm + Stimulus_size, data = data_main)
summary(all_tailor_year)
# with PN.ratio
lm(CP ~  Year + PN.ratio, data = data_NP) %>% summary()
lm(CP ~  Year + PN.ratio + Task + Brain.areas + St_duration, data = data_NP) %>% summary()





####Sensitivity######

###sensitivity measure
model_all_PN_tail =  lm(PN.ratio ~ Task_parm + Non_task_parm + Stimulus_size, data = data_NP)
summary(model_all_PN_tail)

####within study CP-sensitivity correlation

data_CP_sens %>% filter(!is.na(SIGN_CP_sensitivity_value)) %>%
  summarise(N = n(),
            N_papers = n_distinct(Papers),
            N_sign_pos = sum(SIGN_CP_sensitivity_value == TRUE & CP_sensitivity_val_higher_0 == TRUE, na.rm = TRUE),
            N_sign_neg = sum(SIGN_CP_sensitivity_value == TRUE & CP_sensitivity_val_higher_0 == FALSE, na.rm = TRUE),
            N_sign_NA = sum(SIGN_CP_sensitivity_value == TRUE & is.na(CP_sensitivity_val_higher_0)),
            N_not_sign = sum(SIGN_CP_sensitivity_value == FALSE)
  ) %>% 
    mutate(N_sign_pos = N_sign_pos+N_sign_NA, P_sign = (N_sign_pos+N_sign_NA)/N)

data_CP_sens %>% filter(CP_sensitivity_meth == "Spearman correlation") %>%   pull(CP_sensitivity_value_flipped) %>% summary()
data_CP_sens %>% filter(CP_sensitivity_meth == "Spearman correlation" &!is.na(CP_sensitivity_value_flipped)) %>%  pull(CP_sensitivity_value_flipped) %>% length()

lm(CP~CP_sensitivity_value_flipped, data = data_CP_sens %>%
  filter(CP_sensitivity_meth == "Spearman correlation")) %>% summary()

#####within study slope
nrow(data_PN_slope)
data_PN_slope$Papers %>% unique() %>% length()
data_PN_slope$PN_slope %>%  summary()
data_PN_slope$PN_intercept %>% summary()

#find % significantly positive slopes, significantly negative and non-significant
data_PN_slope %>% filter(!is.na(SIGN_PN_slope)) %>%
  summarise(N = n(),
            N_papers = n_distinct(Papers),
            N_sign_pos = sum(SIGN_PN_slope == TRUE & PN_slope > 0, na.rm = TRUE),
            N_sign_neg = sum(SIGN_PN_slope == TRUE & PN_slope < 0, na.rm = TRUE),
            P_sign_pos = N_sign_pos/N
  )

#papers that significantly negative
data_PN_slope %>% filter(!is.na(SIGN_PN_slope) & SIGN_PN_slope == TRUE & PN_slope < 0) %>% pull(Papers) %>% unique()


###across studies slope
model_all_NP4 =  lm(CP ~ PN.ratio, data = data_NP)
model_all_NP4 %>% summary()
nrow(data_NP)
#get 95% confidence interval for the slope PN.ratio
slope_CI = model_all_NP4 %>% confint() %>% as.data.frame() %>% rownames_to_column(var = "term") %>% filter(term == "PN.ratio") %>% select(-term) %>% as.numeric()

##try with PN.ratio and tailoring
# model_all_PN_tail =  lm(CP ~ PN.ratio + Task_parm + Non_task_parm + Stimulus_size + Brain.areas + St_duration + Task, data = data_NP)
# model_all_PN_tail %>% summary()


#now compare with within study slopes
#split based on ; min and max
data_PN_slope_CI = data_PN_slope$Sign_PN_slope %>% strsplit(split = ";") %>% lapply(function(x) as.numeric(x)) %>% do.call(rbind, .) %>% as.data.frame() %>%
  rename(PN_slope_CI_lower = V1, PN_slope_CI_upper = V2)

data_PN_slope_CI %>%
  #compare to the slope CI from across studies - count how many significantly lower or higher
    mutate(PN_slope_sign = case_when(PN_slope_CI_upper < slope_CI[1] ~ "significantly lower",
                                     PN_slope_CI_lower > slope_CI[2] ~ "significantly higher",
                                     TRUE ~ "not significantly different")) %>%
    count(PN_slope_sign) %>% mutate(percent = n/sum(n)*100)

#do the same with final regression
slope_CI_f = model_all_PN_task_brain_st %>% confint() %>% as.data.frame() %>% rownames_to_column(var = "term") %>% filter(term == "PN.ratio") %>% select(-term) %>% as.numeric()

data_PN_slope_CI %>%
  #compare to the slope CI from across studies - count how many significantly lower or higher
    mutate(PN_slope_sign = case_when(PN_slope_CI_upper < slope_CI_f[1] ~ "significantly lower",
                                     PN_slope_CI_lower > slope_CI_f[2] ~ "significantly higher",
                                     TRUE ~ "not significantly different")) %>%
    count(PN_slope_sign) %>% mutate(percent = n/sum(n)*100)




model_all_PN_task_st_MT =  lm(CP ~ PN.ratio + Task + St_duration, data = data_NP %>% filter(Brain.areas == "MT"))
summary(model_all_PN_task_st_MT)

#tailoring
model_all_NP4 =  lm(CP ~ Task_parm + Non_task_parm + Stimulus_size, data = data_main)
model_all_NP4 %>% summary() #only Task_parm
Anova(model_all_NP4, type="II") #this is the best, only Task_param

#model without intercept
model_all_NP4_no_intercept =   lm(delta_CP ~ PN.ratio+0, data = data_NP %>% mutate(delta_CP = CP - 0.5))
summary(model_all_NP4_no_intercept)

#final model without intercept
model_all_PN_task_brain_st_no_intercept =  lm(delta_CP ~ PN.ratio + Brain.areas + St_duration + Task + 0, data = data_NP %>% mutate(delta_CP = CP - 0.5))
summary(model_all_PN_task_brain_st_no_intercept)


####Brain area###
data_main %>% count(Brain.areas)

####Stimulus duration####
lm(CP ~ St_duration, data = data_main %>% filter(!is.na(St_duration))) %>% summary()

#proportion of RT task
data_main %>% count(RT_task) %>% mutate(percent = n/sum(n)*100)

#histogram between st_duration and time window
data_main %>%
  mutate(St_wind_dif = St_duration_raw - Time_window ) %>%
  #filter(St_wind_dif < - 10) %>% pull(Papers)
  filter(!is.na(St_wind_dif)) %>%
  mutate(St_wind_dif_cat = case_when (abs(St_wind_dif) < 10 ~ "Equal",
                                      St_wind_dif > 10 ~ "St_higher",
  St_wind_dif < - 10 ~ "St_lower")) %>%
  count(St_wind_dif_cat) %>%
  mutate(percent = n/sum(n)*100)

##only in MT
lm(CP ~ St_duration, data = data_main %>% filter(!is.na(St_duration), Brain.areas == "MT")) %>% summary()
data_main %>% filter(!is.na(St_duration), Brain.areas == "MT") %>% nrow()
#only in V2
lm(CP ~ St_duration, data = data_main %>% filter(!is.na(St_duration), Brain.areas == "V2")) %>% summary()
data_main %>% filter(!is.na(St_duration), Brain.areas == "V2") %>% nrow()

##now exclude RT task
lm(CP ~ St_duration, data = data_main %>% filter(!is.na(St_duration), RT_task == "fixed time" )  ) %>% summary()
lm(CP ~ PN.ratio + St_duration + Brain.areas, data = data_NP  %>% filter(!is.na(St_duration), RT_task == "fixed time" ) ) %>% summary()
lm(CP ~ St_duration + Brain.areas + Task_parm + Non_task_parm + Stimulus_size, data = data_main %>% filter(!is.na(St_duration), RT_task == "fixed time" )  ) %>% summary()

#now only RT task
lm(CP ~ St_duration, data = data_main %>% filter(!is.na(St_duration), RT_task == "RT" ) %>% mutate(St_duration = St_duration/1000) ) %>% summary()
data_main %>% filter(!is.na(St_duration), RT_task == "RT" ) %>% nrow()

######Task type##############
data_main %>% count(Task) %>% mutate(percent = n/sum(n)*100) %>% arrange(desc(percent))
data_main %>% count(Task, Recording) %>%  group_by(Task) %>% mutate(percent = n/sum(n)*100)


#compare the median PN.ratio for different tasks
data_NP %>%
  filter(!is.na(Task)) %>%
  group_by(Task) %>%
  summarise(median_PN = median(PN.ratio, na.rm = TRUE), N = n())

data_NP %>% count(Task, Task_parm) %>%  group_by(Task) %>% mutate(percent = n/sum(n)*100)
data_NP %>% count(Task, Non_task_parm) %>%  group_by(Task) %>% mutate(percent = n/sum(n)*100)
data_NP %>% count(Task, Stimulus_size) %>%  group_by(Task) %>% mutate(percent = n/sum(n)*100)

model_all_PN_task_tail_brain =  lm(PN.ratio ~ Task + Brain.areas + Task_parm + Non_task_parm + Stimulus_size, data = data_NP)
summary(model_all_PN_task_tail_brain)

data_main %>% count(Task, Recording) %>%  group_by(Task) %>% mutate(percent = n/sum(n)*100) %>% View()
data_main %>% count(Task, Brain.areas) %>%  group_by(Task) %>% mutate(percent = n/sum(n)*100) %>% View()

data_main %>% count(Task,RT_task) %>%  group_by(Task) %>% mutate(percent = n/sum(n)*100)
data_main %>% count(RT_task, Task) %>%  group_by(RT_task) %>% mutate(percent = n/sum(n)*100)


#percent of different stimulus in bistable
data_main %>% filter(Task == "bistable") %>%
  count(Description.of.the.stimulus) %>%
  mutate(percent = n/sum(n)*100) %>%
  arrange(desc(percent))

data_main %>% filter(Task == "bistable", Description.of.the.stimulus == "dot rotating cylinder", Brain.areas == "MT") %>% pull(CP) %>% summary()
data_main %>% filter(Task == "bistable", Description.of.the.stimulus != "dot rotating cylinder", Brain.areas == "MT") %>% pull(CP) %>% summary()
data_main %>% filter(Task == "coarse-discrimination", Brain.areas == "MT") %>% pull(CP) %>% summary()

#exclude rotating stimulus
model_all_PN_task_tail_brain_rot =  lm(CP ~ Task + Brain.areas + Task_parm + Non_task_parm + Stimulus_size + St_duration,
                                       data = data_main %>% filter(Description.of.the.stimulus != "dot rotating cylinder"))
summary(model_all_PN_task_tail_brain_rot)





####Other variables####
###Task exposure
data_main %>% count(!is.na(Learning))
lm(CP ~  Learning, data = data_main) %>% summary()

#with other variables
lm(CP ~  Learning+ PN.ratio + Task + Brain.areas + St_duration, data = data_NP) %>% summary()
data_NP %>% filter(!is.na(Learning)& !is.na(PN.ratio) & !is.na(Task) & !is.na(Brain.areas) & !is.na(St_duration)) %>% nrow()

#with tailoring
lm(CP ~ Learning  + Task + Brain.areas + St_duration + Task_parm + Non_task_parm + Stimulus_size, data = data_main) %>% summary()
data_main %>% filter(!is.na(Learning) & !is.na(Task) & !is.na(Brain.areas) & !is.na(St_duration) & !is.na(Task_parm) & !is.na(Non_task_parm) & !is.na(Stimulus_size)) %>% nrow()
####Lapse rate
data_main %>% count(!is.na(Lapse.rate))
lm(CP ~  Lapse.rate, data = data_main) %>% summary()
lm(CP ~  Lapse.rate + PN.ratio + Task + Brain.areas + St_duration, data = data_NP) %>% summary()
data_NP %>% filter(!is.na(Lapse.rate) & !is.na(PN.ratio) & !is.na(Task) & !is.na(Brain.areas) & !is.na(St_duration)) %>% nrow()
lm(CP ~ Lapse.rate + Task + Brain.areas + St_duration + Task_parm + Non_task_parm + Stimulus_size, data = data_main) %>% summary()
data_main %>% filter(!is.na(Lapse.rate) & !is.na(Task) & !is.na(Brain.areas) & !is.na(St_duration) & !is.na(Task_parm) & !is.na(Non_task_parm) & !is.na(Stimulus_size)) %>% nrow()

##Predictability of targets
data_main %>% filter(!is.na(Predict_targets)) %>% count(Predict_targets)

###CP methods
data_seeds %>% nrow() %>% .[]/2
wilcox.test(data_seeds$CP[data_seeds$method_combined2 == "Frozen seeds"], data_seeds$CP[data_seeds$method_combined2 == "Random seeds"], paired = TRUE)

data_Grand %>% nrow() %>% .[]/2
wilcox.test(data_Grand$CP[data_Grand$method_combined == "Zero signal"], data_Grand$CP[data_Grand$method_combined == "Grand CP"], paired = TRUE) #p = 0.42

###Preference estimation
data_main %>% filter(!is.na(method_pref)) %>% count(method_pref) %>% mutate(percent = n/sum(n)*100) %>% arrange(desc(percent))

lm(CP ~ method_pref, data = data_main %>% filter(!is.na(method_pref))) %>% summary()
lm(CP ~ method_pref + Recording, data = data_main %>% filter(!is.na(method_pref))) %>% summary()
lm(CP ~ method_pref + PN.ratio + Brain.areas + St_duration + Task, data = data_NP ) %>% summary()
lm(CP ~ method_pref + Task_parm + Non_task_parm + Stimulus_size + Brain.areas + St_duration  + Task, data = data_main) %>% summary()

###Task parameter
##task parameter
data_main %>% count(Task_var) %>% mutate(percent = n/sum(n)*100) %>% arrange(desc(percent))

###Task_var
lm(CP ~  Task_var, data = data_main) %>% summary() #R = 0.30
##only MT
lm(CP ~  Task_var, data = data_main %>% filter(Brain.areas == "MT")) %>% summary()
lm(CP ~ Task_var + PN.ratio + Task  + St_duration,
   data = data_NP %>% filter(Brain.areas == "MT")) %>% summary()
lm(CP ~ Task_var + Task + St_duration + Task_parm + Non_task_parm + Stimulus_size, data = data_main %>% filter(Brain.areas == "MT")) %>% summary() #R^2 = 0.45
#exclude rotating cylinder
lm(CP ~  Task_var, data = data_main %>% filter(Task.parameter!= "direction of rotation" & Brain.areas == "MT")) %>% summary() #R = 0.30
lm(CP ~  Task_var + PN.ratio + Task  + St_duration,
   data = data_NP %>% filter(Task.parameter!= "direction of rotation" & Brain.areas == "MT")) %>% summary() #R^2 = 0.45
lm(CP ~ Task_var + Task + St_duration + Task_parm + Non_task_parm + Stimulus_size,
   data = data_main %>% filter(Task.parameter!= "direction of rotation" & Brain.areas == "MT")) %>% summary() #R^2 = 0.45

#####outliers########
sds = 2
reg_main_NP = model_all_PN_task_brain_st
reg_main_tail = model_all_task_tail

data_NP_res = data_NP %>% filter(!is.na(St_duration)) %>% mutate(residuals = reg_main_NP$residuals, res_sd = residuals/ sd(residuals)) %>%
  select(CP, Number.of.neurons..number.of.sessions.,Papers, Monkey, PN.ratio, Task, Brain.areas, St_duration, residuals, res_sd)


data_main_res = data_main %>% filter(!is.na(St_duration)& !is.na(Stimulus_size)) %>%
  mutate(residuals = reg_main_tail$residuals, res_sd = residuals/ sd(residuals)) %>%
  select(CP, Papers, Monkey, Task, Brain.areas, St_duration, Task_parm, Non_task_parm, Stimulus_size, residuals, res_sd)

#show outliers
data_NP_res %>% filter(abs(residuals) > sds*sd(residuals)) %>% View()
data_main_res %>% filter(abs(residuals) > sds*sd(residuals)) %>% View()

data_NP_res %>% filter(Papers %in% c("Uka et al. (2012)", "Britten et al. (1996)", "Kumano & Uka (2014) ", "Goris et al. (2017)", "Doudlah et al.  (2022)")) %>% View()


data_main %>% filter(!is.na(St_duration)& !is.na(Stimulus_size)) %>%
   mutate(residuals = reg_main_tail$residuals, res_sd = residuals/ sd(residuals)) %>%
   filter(Papers %in% c("Goris et al. (2017)", "Grunewald et al. (2002)", "Doudlah et al.  (2022)")) %>%
   select(CP, Papers, Monkey, Number.of.neurons..number.of.sessions.,Task, Brain.areas, St_duration, Task_parm, Non_task_parm, Stimulus_size, residuals, res_sd) %>% View()


data_main %>% filter(!is.na(St_duration)& !is.na(Stimulus_size)) %>%
  mutate(residuals = reg_main_tail$residuals, res_sd = residuals/ sd(residuals)) %>%
  filter(Task == "bistable") %>%
  select(CP, Papers, Monkey, Task, Brain.areas, St_duration, Task_parm, Non_task_parm, Stimulus_size, residuals, res_sd) %>%
  View()

data_NP_res %>%  filter(Task == "bistable") %>% View()




#regression without st size
reg_main_tail_no_st = lm(CP ~ Task + Brain.areas + St_duration + Task_parm + Non_task_parm,
                          data = data_main %>% filter(!is.na(St_duration)) ) %>% summary()


data_main %>% filter(!is.na(St_duration)) %>%
    mutate(residuals = reg_main_tail_no_st$residuals) %>%
    filter(abs(residuals) > sds*sd(residuals)) %>%
    select(CP, Papers, Monkey, Task, Brain.areas, St_duration, Task_parm, Non_task_parm, residuals) %>% View()


#now without stimulus duration
reg_main_NP_without_st_dur = lm(CP ~  PN.ratio + Task + Brain.areas, data = data_NP %>% filter(!is.na(Stimulus_size))) %>% summary()
data_NP %>% filter(!is.na(Stimulus_size)) %>%
  mutate(residuals = reg_main_NP_without_st_dur$residuals, res_sd = residuals/ sd(residuals)) %>%
  filter(abs(residuals) > sds*sd(residuals)) %>%
  select(CP, Papers, Monkey, PN.ratio, Task, Brain.areas, Stimulus_size, residuals, res_sd) %>% View()

reg_main_tail_no_st_dur = lm(CP ~ Task + Brain.areas + Task_parm + Non_task_parm + Stimulus_size,
                          data = data_main %>% filter(!is.na(Stimulus_size)) ) %>% summary()
data_main %>% filter(!is.na(Stimulus_size)) %>%
    mutate(residuals = reg_main_tail_no_st_dur$residuals, res_sd = residuals/ sd(residuals)) %>%
    filter(abs(residuals) > sds*sd(residuals)) %>%
    select(CP, Papers, Monkey, Task, Brain.areas, Task_parm, Non_task_parm, Stimulus_size, residuals, res_sd) %>% View()

###Methods####
Anova(model_all_PN_task_brain_st, type="II")
Anova(model_all_task_tail, type="II")

Anova(lm(CP ~ PN.ratio + Brain + St_duration + Task, data = data_NP %>% mutate(Brain = Brain.areas == "V1" ) ))

###Suplements####



####Recording technique
lm(CP ~  Recording, data = data_main) %>% summary()
#lm(CP ~  Recording + PN.ratio + Task + Brain.areas + St_duration, data = data_NP) %>% summary() #does not work, because of nearly all studies  with NP ratio are single electrode
data_NP %>% count(Recording) #only single electrode has NP ratio
lm(CP ~ Recording + Task_parm + Non_task_parm + Stimulus_size, data = data_main) %>% summary()
lm(CP ~ Recording + Task + Brain.areas + St_duration + Task_parm + Non_task_parm + Stimulus_size, data = data_main) %>% summary()




##stat power
##number of neurons
lm(CP ~  N_neurons, data = data_main %>% filter(Recording == "single-electrode")) %>% summary()
data_main %>% filter(Recording == "single-electrode", !is.na(N_neurons)) %>% nrow()


#for multi-electrode
lm(CP ~  N_neurons, data = data_main %>% filter(Recording == "multi-electrode")) %>% summary()
data_main %>% filter(Recording == "multi-electrode", !is.na(N_neurons)) %>% nrow()

model_all_PN_task_brain_st_N =  lm(CP ~ N_neurons+PN.ratio + Brain.areas + St_duration + Task, data = data_NP %>% filter(Recording == "single-electrode") )
summary(model_all_PN_task_brain_st_N)

model_all_task_tail_N =  lm(CP ~ N_neurons+ Task_parm + Non_task_parm + Stimulus_size + Brain.areas + St_duration  + Task, data = data_main %>% filter(Recording == "single-electrode"))
summary(model_all_task_tail_N) #


#eccentricity
data_main %>% count(!is.na(Stimulus_ecc) )
lm(CP ~  Stimulus_ecc, data = data_main) %>% summary()
lm(CP ~  Stimulus_ecc + PN.ratio + Task + Brain.areas + St_duration, data = data_NP) %>% summary()
data_NP %>% count(!is.na(Stimulus_ecc) )
lm(CP ~ Stimulus_ecc + Task + Brain.areas + St_duration + Task_parm + Non_task_parm + Stimulus_size, data = data_main) %>% summary() #R^2 = 0.45



####END TEXT####







####CP vs year of publication####

data_main$Papers %>% unique()


##plot CP vs year of publication with Brain areas colored
plot_year = data_main %>%
  plot_data("Year", "CP", ADD_MEDIAN = FALSE,
                                    xlab = "", left_margin = 10, title = "Simple regression") +
  #align title to center and make it bold font
  theme(plot.title = element_text(hjust = 0.5,  face = "bold")) +
  annotate("text", x = min(data_main$Year, na.rm = TRUE) + 3, y = max(data_main$CP, na.rm = TRUE) - 0.05,
             label = get_pval_string(data_main, "CP", "Year"),
             size = 4)


print(plot_year)
ggsave(file = paste0(dir_out, "year.pdf"), plot_year,  width = 0.8*(2*w/3), height = h)


plot_year_legend_bottom = plot_legend(data, legend_position = "bottom") +
  theme(
    legend.title = element_blank(),
    legend.background = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  # Adding byrow = TRUE fills Row 1 completely, then Row 2
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    fill = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE)
  )
ggsave(file = paste0(dir_out, "year_legend_bottom.pdf"), plot_year_legend_bottom,  width = 1*(2*w/3)+2, height = h+1)

plot_year_legend_bottom_no_lines = plot_legend(data, legend_position = "bottom",ADD_LINES =FALSE) +
    theme(
        legend.title = element_blank(),
        legend.background = element_rect(color = "black", fill = NA, linewidth = 0.5)
    ) +
    # Adding byrow = TRUE fills Row 1 completely, then Row 2
    guides(
        color = guide_legend(nrow = 2, byrow = TRUE),
        fill = guide_legend(nrow = 2, byrow = TRUE),
        shape = guide_legend(nrow = 2, byrow = TRUE)
    )
ggsave(file = paste0(dir_out, "year_legend_bottom_no_lines.pdf"), plot_year_legend_bottom_no_lines,  width = 1*(2*w/3)+2, height = h+1)

#with lines and legend on the right - one column with box
plot_year_legend_right = plot_legend(data, legend_position = "right") +
      theme(
          legend.title = element_blank(),
          legend.background = element_rect(color = "black", fill = NA, linewidth = 0.5)
        ) +
        guides(color = guide_legend(ncol = 1), fill = guide_legend(ncol = 1), shape = guide_legend(ncol = 1))

ggsave(file = paste0(dir_out, "year_legend_right.pdf"), plot_year_legend_right,  width = 1*(2*w/3)+2, height = h+1)

#now legend only in two cols
plot_year_legend_bottom_2cols= plot_legend(data, legend_position = "right") +
      theme(
          legend.title = element_blank(),
          legend.background = element_rect(color = "black", fill = NA, linewidth = 0.5)
        ) +
        # This forces the color legend to have 2 rows
        guides(color = guide_legend(ncol = 2), fill = guide_legend(ncol = 2), shape = guide_legend(ncol = 2))


plot_year_legend_bottom_2rows= plot_legend(data, legend_position = "bottom") +
      theme(
          legend.title = element_blank(),
          legend.background = element_rect(color = "black", fill = NA, linewidth = 0.5)
        ) +
        # This forces the color legend to have 2 rows
        guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2), shape = guide_legend(nrow = 2))
ggsave(file = paste0(dir_out, "year_legend_bottom_2rows.pdf"), plot_year_legend_bottom_2rows,  width = 1*(2*w/3)+2, height = h+1)
# ###for presentations
# #without lines
# ggsave(file = paste0(dir_out, "year_1.pdf"), data_main %>% plot_data("Year", "CP", ADD_MEDIAN = FALSE, xlab = "Year of publication", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE), width = w, height = h+1)
# ggsave(file = paste0(dir_out, "year_2.pdf"), data_main %>% plot_data("Year", "CP", ADD_MEDIAN = FALSE, xlab = "Year of publication", REGRESSION_ALL = TRUE, REGRESSION_GROUP = FALSE), width = w, height = h+1)
# ggsave(file = paste0(dir_out, "year_3.pdf"), data_main %>% plot_data("Year", "CP", ADD_MEDIAN = FALSE, xlab = "Year of publication"), width = w, height = h+1)


#calculate the the number of single electrode vs multielectrode recordings (Recording) for three periods 1995-2005, 2006-2015, 2016-2025 and do the barplot
plot_rec_period = data_main %>%
  mutate(Period = cut(Year, breaks = c(1995, 2005, 2015, 2025), labels = c("1995-2005", "2006-2015", "2016-2025"))) %>%
  count(Period, Recording, .drop = FALSE) %>%
  ggplot(aes(x = Period, y = n, group = Recording, fill = Recording)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_classic() +
    labs(x = "Period of publication", y = "# data points") +
    theme(
      #text = element_text(size = grid::unit(font_labels, "pt")),
      axis.text = element_text(size = grid::unit(font_axis, "pt")),
      legend.text = element_text(size = grid::unit(font_legend, "pt")),
      legend.title = element_text(size = grid::unit(font_legend, "pt")),
      #make x axis readable
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.margin = margin(10, 10, 10, 40), # Increase the left margin
      legend.position = "bottom"
    ) +
  guides(fill = guide_legend(nrow = 2))
print(plot_rec_period)
ggsave(file = paste0(dir_out, "recording_period.pdf"), plot_rec_period, width = 3.5, height = 3)

#now plot vs year with grouping by Recording

plot_year_rec = data_main %>%
  filter(Recording == "single-electrode") %>%
  plot_data("Year", "CP", ADD_MEDIAN = FALSE, xlab = "Year of publication",
                                        stroke = 0.7,  REGRESSION_ALL = TRUE, REGRESSION_GROUP = FALSE,
                                        left_margin = 10, point_size = 1.8)

#now add the multi-electrode data points with dashed regression lines and new shapes
plot_year_rec = plot_year_rec +
  geom_quasirandom(data = data_main %>% filter(Recording == "multi-electrode"),
                   width = 0.5, size = 1.8, stroke = 0.7, alpha = 0.5, fill = "white") +
  geom_smooth(data = data_main %>% filter(Recording == "multi-electrode"), aes(group = NA),method = "lm", size = 1.5, se = TRUE, color = "black", linetype = "dashed")

print(plot_year_rec)
ggplot2::ggsave(file = paste0(dir_out, "year_recording.pdf"), plot_year_rec,  width = 0.8*(2*w/3), height = 0.8*h)

#with legend
plot_year_rec_legend = data_main %>%
  plot_data("Year", "CP", group = "Recording", ADD_MEDIAN = FALSE, xlab = "Year of publication", shape = c("single-electrode" = 21, "multi-electrode" = 1),
            stroke = 0.7,  REGRESSION_ALL = FALSE, SE_REG_GROUP = TRUE, color_reg_group = "black",
            line_type = c("solid","dashed"), left_margin = 10, point_size = 1.8, legend_position = "bottom")
print(plot_year_rec_legend)
ggsave(file = paste0(dir_out, "year_recording_legend.pdf"), plot_year_rec_legend,  width = 0.8*(2*w/3)+2, height = h+1)



plot_year_single = data_main %>% filter(Recording == "single-electrode") %>% plot_data("Year", "CP", ADD_MEDIAN = FALSE)
print(plot_year_single)
ggsave(file = paste0(dir_out, "year_single.pdf"), plot_year_single, width = w, height = h)


#Stimulus duration vs Year
plot_year_duration = data_main %>%
  plot_data("Year", "St_duration", ADD_MEDIAN = FALSE,
            xlab = "Year of publication", left_margin = 10, ylab = "Stimulus duration (ms)", legend_position = "right", font_scale = 0.9,
            yline = 0)
print(plot_year_duration)
ggsave(file = paste0(dir_out, "year_duration.pdf"), plot_year_duration, width = (2*w/3), height = 0.9*h)



data_year_brain = data_main %>%
  group_by(Period, Brain.areas) %>%
  summarise(N = n(), .groups = "drop") %>%
  #add proportion
  group_by(Period) %>%
  mutate(Prop = 100*N/sum(N))

#now plot the stacked bar plot of the proportion for each Brain area in each period
plot_year_brain = data_year_brain  %>%
  ggplot(aes(x = Period, y = Prop, fill = Brain.areas)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_manual(values = color_brain, guide = guide_legend(title = "Brain areas")) +
    theme_classic() +
    labs(x = "Period of publication", y = "Proportion of data points") +
    theme(
      text = element_text(size = grid::unit(0.9*font_labels, "pt")),
      axis.text = element_text(size = grid::unit(0.9*font_axis, "pt")),
      legend.text = element_text(size = grid::unit(0.9*font_legend, "pt")),
      legend.title = element_text(size = grid::unit(0.9*font_legend, "pt")),
      #make x axis readable
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.margin = margin(10, 10, 10, 40), # Increase the left margin
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(nrow = 2))

print(plot_year_brain)

ggsave(file = paste0(dir_out, "year_brain.pdf"), plot_year_brain, width =1.1*2/3*w, height = h)

#do the same thing but with task type
data_year_Task = data_main %>%
  group_by(Period, Task) %>%
  summarise(N = n(), .groups = "drop") %>%
  #add proportion
  group_by(Period) %>%
  mutate(Prop = 100*N/sum(N))

plot_year_Task = data_year_Task %>%
  ggplot(aes(x = Period, y = Prop, fill = Task)) +
    geom_bar(stat = "identity", position = "fill") +
    theme_classic() +
    labs(x = "Period of publication", y = "Proportion of data points") +
    theme(
      #text = element_text(size = grid::unit(font_labels, "pt")),
      axis.text = element_text(size = grid::unit(font_axis, "pt")),
      legend.text = element_text(size = grid::unit(font_legend, "pt")),
      legend.title = element_text(size = grid::unit(font_legend, "pt")),
      #make x axis readable
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.margin = margin(10, 10, 10, 40), # Increase the left margin
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(nrow = 2))

print(plot_year_Task)


data_main %>% count(Recording)

plot_recordings = data_main %>%
  plot_data("Recording", "CP", jitter = 0.2,
            REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, STAT_COMPARISON = TRUE)
print(plot_recordings)
ggsave(file = paste0(dir_out,"recording.pdf"), plot_recordings, width = 0.8*(2*w/3), height = h)

lm(CP ~ Recording, data = data_main) %>% summary()
lm(CP ~ Recording, data = data_main %>% filter(Brain.areas == "MT")) %>% summary()


###Between monkey comparison#####
plot_density = function(data,x, LABELS = FALSE, font_scale = 1){
  plot = data %>%
    ggplot(aes(x = !!sym(x))) +
        geom_density(size = 1.5, color = "black", show.legend = FALSE) +
        theme_classic() +
         labs(x = "difference in CP between monkeys", y = "density")+
        theme(
          text = element_text(size = grid::unit(font_scale*font_axis, "pt")),
          axis.text = element_text(size = grid::unit(font_scale*font_axis, "pt")),
          legend.text = element_text(size = grid::unit(font_scale*font_legend, "pt")),
          legend.title = element_text(size = grid::unit(font_scale*font_legend, "pt")),
          #make x axis readable
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          plot.margin = margin(10, 10, 10, 10)
        )
  if (LABELS) {plot = plot +
                        geom_text(aes(y = 0, label = symbol, color = Brain.areas), position = position_jitter(width = 0, height = 2), size = font_labels) +
                        scale_color_manual(values = color_brain, guide = guide_legend(title = "Brain areas"))
  } else   plot = plot +
                geom_jitter(aes(y = 5, fill = Brain.areas, shape = Brain.areas), width = 0, height = 4, size = 2.5, stroke = 0.2, color = "white") +
                scale_fill_manual(values = color_brain, guide = guide_legend(title = "Brain areas")) +
                scale_shape_manual(values = shape_brain, guide = guide_legend(title = "Brain areas"))    
  return(plot)
}

data_main  %>% pull(Monkey) %>% unique()


plot_Monkey = data_monkey %>%
  plot_density("CP_monkey", font_scale = 0.9)


print(plot_Monkey)
ggsave(file = paste0(dir_out, "monkey.pdf"), plot_Monkey, width = (2*w/3), height = 0.85*h)

plot_Monkey_sup = data_monkey %>%
  plot_density("CP_monkey", LABELS = TRUE)
print(plot_Monkey_sup)
ggsave (file = paste0(dir_out, "monkey_sup.pdf"), plot_Monkey_sup, width = w, height = h)



####Funnel plot############
#plot CP vs the number of neurons
plot_CP_N = data_main %>%
  filter(Recording == "single-electrode") %>%
  plot_data("N_neurons", "CP", ADD_MEDIAN = FALSE, xlab = "# neurons", left_margin = 10,
            angle = 0, hjust = NULL, jitter = 0)
print(plot_CP_N)

ggsave(file = paste0(dir_out, "CP_N_neurons.pdf"), plot_CP_N, width = 0.8*(2*w/3), height = 0.9*h)

#now for multi-electrode
plot_CP_N_mu = data_main %>%
  filter(Recording == "multi-electrode") %>%
  plot_data("N_neurons", "CP", ADD_MEDIAN = FALSE, xlab = "# neurons", left_margin = 10,
            angle = 0, hjust = NULL, jitter = 0)
print(plot_CP_N_mu)
ggsave(file = paste0(dir_out, "CP_N_neurons_multi.pdf"), plot_CP_N_mu, width = 0.8*(2*w/3), height = 0.9*h)


#plot Number of neurons vs Year
data_main %>%
  filter(Recording == "single-electrode") %>%
  plot_data("Year", "N_neurons", ADD_MEDIAN = FALSE, xlab = "Year of publication")


data_main %>% select(Papers, Monkey, Brain.areas, Epoch, CP, Sign, SIGN, Sign_method, SEM) %>% View()



#plot SEM with Year
plot_SEM_year = data_main %>%
  filter(!is.na(SEM) & Recording == "single-electrode") %>%
  plot_data("Year", "SEM", ADD_MEDIAN = FALSE, xlab = "Year of publication",
            ylab = "Standard Error of CP", yline = 0)
print(plot_SEM_year)

####Funnel plots
data_SEM = data_main %>%
  #add Goris et al. (2017) data from zero signal trials as only there SEM is reported
  bind_rows(data %>% filter((Papers == "Goris et al. (2017)" & Main.epoch == 1) & !is.na(SEM))) %>%
  filter(Recording == "single-electrode")

#statistic SEM - method
data_SEM %>% filter(!is.na(SEM)) %>% count(Sign_method) %>% mutate(perc = n/sum(n)*100)

# Create the funnel plot
df <- data_SEM %>%
  select(CP, SEM, Brain.areas) %>% na.omit() %>%
  rename(yi = CP, sei = SEM)

nrow(df) #number of points with SEM

#now the same for muti-electrode
data_SEM_mu = data_main %>%
  #add Goris et al. (2017) data from zero signal trials as only there SEM is reported
  bind_rows(data %>% filter((Papers == "Goris et al. (2017)" & Main.epoch == 1) & !is.na(SEM))) %>%
  filter(Recording == "multi-electrode")

#percentage of different Sign_method
bind_rows(data_SEM, data_SEM_mu) %>% filter(!is.na(Sign_method)&!is.na(SEM)) %>% count(Sign_method) %>% mutate(perc = n/sum(n))

df_mu <- data_SEM_mu %>%
  select(CP, SEM, Brain.areas) %>% na.omit() %>%
  rename(yi = CP, sei = SEM)
nrow(df_mu) #number of points with SEM

# Estimate meta-analytic mean
meta_mean <- weighted.mean(df$yi, 1 / df$sei^2)

# Create funnel cone boundaries
se_range <- seq(0, max(df$sei) * 1.05, length.out = 100)
funnel_df <- data.frame(
  sei = c(se_range, rev(se_range)),
  yi  = c(meta_mean - 1.96 * se_range, rev(meta_mean + 1.96 * se_range))
)

# Plot
plot_funnel = function(df, legend_position = "none", font_scale = 1){

  ggplot(df, aes(x = yi, y = sei, color = Brain.areas,fill = Brain.areas, shape = Brain.areas)) +
    geom_point(size = 2, alpha = 0.5) +
      scale_color_manual(values = color_brain, guide = guide_legend(title = "Brain areas")) +
  scale_shape_manual(values = shape_brain, guide = guide_legend(title = "Brain areas")) +
    scale_fill_manual(values = color_brain, guide = guide_legend(title = "Brain areas")) +
  geom_polygon(data = funnel_df, aes(x = yi, y = sei),
               fill = "lightgray", color = NA, alpha = 0.5, inherit.aes = FALSE) +
  geom_vline(xintercept = meta_mean, linetype = "solid") +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "black", size = 0.5) +


  scale_y_reverse() +  # Invert y-axis: low SE (high precision) at top
  labs(x = "mean CP", y = "Standard Error") +
  theme_classic() +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "black", size = 0.5) +
  theme(text = element_text(size = grid::unit(font_scale*font_axis, "pt")),
        axis.text = element_text(size = grid::unit(font_scale*font_axis, "pt")),
        legend.text = element_text(size = grid::unit(font_scale*font_legend, "pt")),
        legend.title = element_text(size = grid::unit(font_scale*font_legend, "pt")),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.margin = margin(0, 5, 5, 5),
        legend.position = legend_position
      )+
  #make breaks in x axis at 0.4, 0.5, 0.6
  scale_x_continuous(breaks = seq(0.3, 0.7, by = 0.1))
}
plot_funnel_single <- plot_funnel(df)
print(plot_funnel_single)
ggsave(file = paste0(dir_out, "funnel_plot.pdf"), plot_funnel_single, width = 0.92*h, height = 0.85*h)

res <- rma(yi = df$yi, sei = df$sei, method = "FE")
regtest(res, model = "lm")  # Egger’s test
nrow(df)
#now do Ecker test using lm
lm(st_y ~ precision, data = data_SEM %>% mutate(precision = 1/SEM, st_y = (CP - mean(CP))/SEM)) %>% summary()

#now do the Ecker test with covariates for Tailoring
lm(st_y ~ precision + Task_parm + Non_task_parm + Stimulus_size,
  data = data_SEM %>% mutate(precision = 1/SEM, st_y = (CP - mean(CP))/SEM)) %>% summary()


lm(st_y ~ precision + Task_parm + Non_task_parm + Stimulus_size + Brain.areas + Task + St_duration,
  data = data_SEM %>% mutate(precision = 1/SEM, st_y = (CP - mean(CP))/SEM)) %>% summary()

#plot Standard Error vs Tailor
plot_SEM_tailor = data_SEM %>%
  filter(!is.na(SEM) & Recording == "single-electrode") %>%
  plot_data("Task_tailor", "SEM", ADD_MEDIAN = FALSE, xlab = "Tailoring", ylab = "Standard Error of CP", yline = 0)
print(plot_SEM_tailor)

##now do funnel plot for multi-electrode
meta_mean_mu <- weighted.mean(df_mu$yi, 1 / df_mu$sei^2)
# Create funnel cone boundaries
se_range_mu <- seq(0, max(df_mu$sei) * 1.05, length.out = 100)
funnel_df_mu <- data.frame(
  sei = c(se_range_mu, rev(se_range_mu)),
  yi  = c(meta_mean_mu - 1.96 * se_range_mu, rev(meta_mean_mu + 1.96 * se_range_mu))
)
# Plot
plot_funnel_mu = plot_funnel(df_mu)
print(plot_funnel_mu)
ggsave(file = paste0(dir_out, "funnel_plot_multi.pdf"), plot_funnel_mu, width = 0.92*h, height = 0.85*h)
# Egger’s test
res_mu <- rma(yi = df_mu$yi, sei = df_mu$sei, method = "FE")
regtest(res_mu, model = "lm")  # Egger’s test
nrow(df_mu)

#now do the Ecker test with covariates for Tailoring combined single and multi electrode
lm(st_y ~ precision + Task_parm + Non_task_parm + Stimulus_size,
  data = rbind(data_SEM, data_SEM_mu) %>% mutate(precision = 1/SEM, st_y = (CP - mean(CP))/SEM)) %>% summary()


#now the same but adding Brain area, Task, St_duration
lm(st_y ~ precision + Task_parm + Non_task_parm + Stimulus_size + Brain.areas + Task + St_duration,
  data = rbind(data_SEM, data_SEM_mu) %>% mutate(precision = 1/SEM, st_y = (CP - mean(CP))/SEM)) %>% summary()




###CP-sensitivity within studies Correlation coeficient####


#studies that does not have CP sensitivity input at all
Papers_not = data_CP_sens %>% group_by(Papers) %>%
  summarise(N = n(),
            N_no_sens = sum(is.na(CP_sensitivity_value)& is.na(SIGN_CP_sensitivity_value))) %>%
  filter(N_no_sens == N) %>%
  pull(Papers) %>%
  unique()

#studies that have CP sensitivity for some data points in the main dataset
Papers_yes = data %>% filter(!is.na(CP_sensitivity_value) | !is.na(SIGN_CP_sensitivity_value)) %>%
  pull(Papers) %>%
    unique()

#Papers that have CP sensitivity for some data points but not in the main dataset
print( Papers_yes[Papers_yes %in% Papers_not])


#the same for CP_sensitivity_var
data_CP_sens %>% filter(!is.na(CP_sensitivity_var)) %>% count(CP_sensitivity_var) %>% mutate(perc = n/sum(n))

data_CP_sens %>% filter(!is.na(CP_sensitivity_var)) %>% pull(Papers) %>% unique() %>% length()

data_CP_sens %>% filter(!is.na(CP_sensitivity_var)) %>%
  select(Papers, CP_sensitivity_var) %>% unique() %>% count (CP_sensitivity_var) %>% mutate(perc = n/sum(n))



#number of significant positive vs negative correlations
data_CP_sens %>% filter(!is.na(CP_sensitivity_val_higher_0)) %>% count(CP_sensitivity_val_higher_0) %>% mutate(perc = n/sum(n))



data_CP_sens %>% count(CP_sensitivity_meth)



##plot CP sensitivity value flipped vs CP
plot_CP_sens = data_CP_sens %>%
  filter(CP_sensitivity_meth == "Spearman correlation") %>%
  plot_data("CP_sensitivity_value_flipped", "CP", ADD_MEDIAN = FALSE, ylim = c(NA, 0.69),
            xlab = "CP - sensitivity correlation coeficient", yline = 0.5, xline = 0, jitter = 0,
                      regression_width = 1.5, left_margin = 10, angle = 0, hjust = NULL)

print(plot_CP_sens)






####CP-sensitivity slope within studies######

#find % significantly positive slopes, significantly negative and non-significant
data_PN_slope %>% filter(!is.na(SIGN_PN_slope)) %>%
  summarise(N = n(),
            N_papers = n_distinct(Papers),
            N_sign_pos = sum(SIGN_PN_slope == TRUE & PN_slope > 0, na.rm = TRUE),
            N_sign_neg = sum(SIGN_PN_slope == TRUE & PN_slope < 0, na.rm = TRUE),
            P_sign_pos = N_sign_pos/N
  )



#now calculate weighted mean of the slope using SEM as weights
weighted_mean_slope = weighted.mean(data_PN_slope$PN_slope, w = 1/data_PN_slope$PN_slope_SEM^2, na.rm = TRUE)


data_PN_slope$CP %>% summary()




plot_PN_within = data_NP_slope %>%
            #for Clery et al. (2017) change CP for y and PN.ratio for x
            mutate(PN.ratio = ifelse(Papers == "Clery et al. (2017)", x, PN.ratio),
                   CP = ifelse(Papers == "Clery et al. (2017)", y, CP)) %>%
            plot_data("PN.ratio", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE,
                      jitter = 0,   xlab = "Normalized sensitivity [inverse mean N/P ratio]", ADD_MEDIAN = FALSE,
                      regression_width = 1.5, left_margin = 10, angle = 0, hjust = NULL)+
  coord_cartesian(xlim = c(0, NA)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)))+
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               size = 0.7, show.legend = FALSE)

print(plot_PN_within)
ggsave(file = paste0(dir_out, "PN_ratio_within.pdf"), plot_PN_within, width = 0.8*(2*w/3), height = h)

######P/N ratio###############

data_NP %>% count(Recording,NP.ratio) %>% View()
data_NP$NP.ratio %>% summary()

#median CP per brain area
data_NP %>% group_by( Brain.areas) %>% summarise(CP_median = median(CP, na.rm = TRUE))


plot_PN = data_NP %>%
            plot_data("PN.ratio", "CP", REGRESSION_ALL = TRUE, REGRESSION_GROUP = TRUE, jitter = 0,  xlab = "Normalized sensitivity [inverse mean N/P ratio]", ADD_MEDIAN = FALSE,
                      regression_width = 1.5, left_margin = 10, angle = 0, hjust = NULL) +
  coord_cartesian(xlim = c(0, NA)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)))

# #add crosses to bistable stimuli
# plot_PN = plot_PN +
#   geom_point(data = data_NP %>% filter(Task == "bistable"), shape = 4, size = 1, color = "black", stroke = 1)


print(plot_PN)
ggsave(file = paste0(dir_out, "PN_ratio.pdf"), plot_PN, width = 0.8*(2*w/3), height = h)



# #now plot only for some papers for presentation purpurses
# plot_NP_MT = data_NP %>%
#   plot_data("PN.ratio", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, jitter = 0, xlab = "Psychometric/Neurometric threshold ratio", ADD_MEDIAN = FALSE, regression_width = 1.5, alpha_points = 0)
#
# plot_NP_MT = plot_NP_MT +
#   geom_point(data = data_NP %>% filter(Brain.areas == "MT", Papers %in% c("Law & Gold (2008)",  "Clark & Bradley (2022)",  "Kang & Maunsell (2020)", "Purushothaman & Bradley (2005)")),
#              color = "darkolivegreen4" , size = 2.5)
#
# print(plot_NP_MT)
# ggplot2::ggsave(file = paste0(dir_out, "NP_ratio_MT_2papers.pdf"), plot_NP_MT, width = w, height = h+1)



##now plot CP - PN grouped by task type

plot_NP_task_group = data_NP %>%
    filter(Task.type %in% c("2 (coarse discrimination)", "1 (fine discrimination)")) %>%
    mutate(Task.type = factor(ifelse(Task.type=="2 (coarse discrimination)", "coarse discrimination", "fine discrimination"),
                              levels = c("coarse discrimination", "fine discrimination"))) %>%
    plot_data("PN.ratio", "CP", group = "Task.type", ADD_MEDIAN = FALSE, xlab = "Normalized sensitivity",
              shape = c("coarse discrimination" = 16, "fine discrimination" = 1),
              stroke = 0.7,  REGRESSION_ALL = FALSE, SE_REG_GROUP = TRUE, color_reg_group = "black",  line_type = c("solid", "dashed"),
               legend_position = "bottom", color_legend = guide_legend(position = "right", title = "Brain areas"), font_scale = 0.9,
              test_cut = 0.01)

plot_NP_task_group = plot_NP_task_group + guides(fill = "none", shape = guide_legend(title = "Task type"))
print(plot_NP_task_group)

ggplot2::ggsave(file = paste0(dir_out, "PN_task_group.pdf"), plot_NP_task_group , width = 0.85*w, height = 1.07*h)

lm(CP ~ PN.ratio, data = data_NP %>% filter(Task.type %in% c("2 (coarse discrimination)"))) %>% summary()
lm(CP ~ PN.ratio, data = data_NP %>% filter(Task.type %in% c("1 (fine discrimination)"))) %>% summary()
lm(CP ~ PN.ratio * Task.type, data = data_NP %>% filter(Task.type %in% c("2 (coarse discrimination)", "1 (fine discrimination)"))) %>% summary()

##now exclude those where Task_parm was not fit
plot_PN_exl_not_fit = data_NP %>%
  filter(Task_parm != "not fit") %>%
  plot_data("PN.ratio", "CP", REGRESSION_ALL = TRUE, REGRESSION_GROUP = TRUE, jitter = 0, xlab = "Psychometric/Neurometric threshold ratio", ADD_MEDIAN = FALSE, regression_width = 1.5, title = "Excluding studies with not fit task parameters")
print(plot_PN_exl_not_fit)

#now only not fit
plot_PN_not_fit = data_NP %>%
  filter(Task_parm == "not fit") %>%
  plot_data("PN.ratio", "CP", REGRESSION_ALL = TRUE, REGRESSION_GROUP = TRUE, jitter = 0, xlab = "Psychometric/Neurometric threshold ratio", ADD_MEDIAN = FALSE, regression_width = 1.5, title = "Studies with not fit task parameters") +
  xlim(0,1.25)+
  ylim(0.45,0.7)
print(plot_PN_not_fit)



###now CC vs PN
plot_CC_PN = data_NP %>%
  plot_data("PN.ratio", "CC", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, jitter = 0,
            xlab = "Psychometric/Neurometric threshold ratio", ADD_MEDIAN = FALSE, regression_width = 1.5, yline = 0, ylab = "mean Choice Correlations")
#add identity line
plot_CC_PN = plot_CC_PN + geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue", size = 1.5) +
                geom_smooth(data = data_NP %>% filter(PN.ratio <=1 &Brain.areas %in% c("V1", "V2", "MT", "MST/MSTd")), method = "lm", se = FALSE, size = 0.7, show.legend = FALSE) +
                geom_smooth(aes(group = NA), data_NP %>% filter(PN.ratio <=1), method = "lm", color = "black", size = 1.5, show.legend = FALSE)


print(plot_CC_PN)

ggsave(file = paste0(dir_out, "CC_PN_ratio.pdf"), plot_CC_PN, width = w, height = h+1)
##Now between monkeys for the same Paper, Area, epoch
data_NP %>% pull(Monkey) %>% unique()

plot_NP_M = data_NP %>%
  filter(!Monkey %in% c("two combined", "three combined")) %>%
  mutate(Monkey = Monkey %>% delete_spaces()) %>%
  plot_data("PN.ratio", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, CONNECT_PER_PARER = TRUE, CONNECT_REGRESSION = FALSE, jitter = 0, ADD_MEDIAN = FALSE,
            group_connect = c("Papers", "Brain.areas", "Epoch"), xlab = "Psychometric/Neurometric threshold ratio")
print(plot_NP_M)

ggsave(file = paste0(dir_out, "NP_ratio_monkey.pdf"), plot_NP_M, width = w, height = h+1)

###now the same but for neurometric threshold

plot_N_M = data_NP %>%
  filter(Neurometric.threshold != "") %>%
  mutate(Neurometric.threshold = Neurometric.threshold %>% delete_spaces() %>% before_bracket()) %>%
  filter(!Monkey %in% c("two combined", "three combined")) %>%
  mutate(Monkey = Monkey %>% delete_spaces()) %>%
  group_by(Papers, Brain.areas, Epoch) %>%
  mutate(N_threshold = 100 * Neurometric.threshold/min(Neurometric.threshold)) %>%
  ungroup() %>%
  plot_data("N_threshold", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, CONNECT_PER_PARER = TRUE, CONNECT_REGRESSION = FALSE, jitter = 0,ADD_MEDIAN = FALSE,
            group_connect = c("Papers", "Brain.areas", "Epoch"), xlab = "Neurometric threshold, 100% is the lowest bewteen monkeys")

print(plot_N_M)
ggsave(file = paste0(dir_out, "N_threshold_monkey.pdf"), plot_N_M, width = w, height = h+1)

####sensitive vs non-sensitive neurons
data_Sense = data %>%  filter(Sensitive.neurons != "" ) %>% filter_best_method()
#need to mark Zaidel visual and vestibular epoch
data_Sense %<>% mutate(Monkey = case_when(Papers == "Zaidel et al. (2017)" & grepl("visual", Epoch) ~ paste0(Monkey, " visual"),
                                          Papers == "Zaidel et al. (2017)" & grepl("vestibular", Epoch) ~ paste0(Monkey, " vestibular"),
                                          TRUE ~ Monkey))

data_Sense %<>% mutate(Sensitive = case_when(Sensitive.neurons == 0 ~ "less sensitive",
                                              Sensitive.neurons == 1 ~ "more sensitive") %>% factor(levels = c("less sensitive", "more sensitive"), ordered = TRUE))

plot_sense = data_Sense %>% plot_data("Sensitive", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE,  STAT_COMPARISON = TRUE, PAIRED = TRUE,
                                      CONNECT_PER_PARER = TRUE, jitter = 0, group_connect = c("Papers", "Monkey", "Brain.areas"), xlab = "Neurons subset",
                                      MEAN_NUDGE = TRUE, width_error = 0.03)

print(plot_sense)
ggsave(file = paste0(dir_out, "sensitive_neurons.pdf"), plot_sense, width = w, height = h+1)

wilcox.test(data_Sense$CP[data_Sense$Sensitive == "less sensitive"], data_Sense$CP[data_Sense$Sensitive == "more sensitive"], paired = TRUE) # p = 0.08


###Now PN.ratio vs Tailoring
plot_NP_tailor = data_NP %>% filter(!is.na(Task_tailor)) %>%
  plot_data(y  = "PN.ratio", x = "Task_tailor", jitter = 0.2, REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, ADD_MEDIAN = TRUE, ylab = "Normalized sensitivity",
            STAT_COMPARISON = TRUE, test_cut = 0.01, yline = 0, right_margin = 0)
# plot_NP_tailor = plot_NP_tailor + coord_cartesian(clip = "off") +
#     annotation_custom(grob = textGrob("Tailor:\ntask / non-task\nparams", x = 0.87, y = -0.3, rot = 35, just = "left", gp = gpar(fontsize = font_axis, fontface = "bold", lineheight = 0.8)))


print(plot_NP_tailor)

ggsave(file = paste0(dir_out, "PN_ratio_tailor.pdf"), plot_NP_tailor, width = 0.9*2/3*w, height = h)

#only for MT
plot_NP_tailor_MT = data_NP %>% filter(!is.na(Task_tailor) & Brain.areas == "MT") %>%
  plot_data(y  = "PN.ratio", x = "Task_tailor", jitter = 0.2, REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, ADD_MEDIAN = TRUE, ylab = "Psychometric/Neurometric threshold ratio",
            STAT_COMPARISON = TRUE, test_cut = 0.01, yline = 0)
print(plot_NP_tailor_MT)
ggsave(file = paste0(dir_out, "NP_ratio_tailor_MT.pdf"), plot_NP_tailor_MT, width = w, height = h+3)

#Now PN.ratio vs Stimulus size
plot_NP_size = data_NP %>% filter(!is.na(Stimulus_size)) %>%
  plot_data(y  = "PN.ratio", x = "Stimulus_size", jitter = 0.2, REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, ADD_MEDIAN = TRUE, ylab = "Normalized sensitivity",
            STAT_COMPARISON = TRUE, yline = 0)
print(plot_NP_size)
ggsave(file = paste0(dir_out, "PN_ratio_size.pdf"), plot_NP_size, width = 0.7*2/3*w, height = h)

#only MT
plot_NP_size_MT = data_NP %>% filter(!is.na(Stimulus_size) & Brain.areas == "MT") %>%
  plot_data(y  = "PN.ratio", x = "Stimulus_size", jitter = 0.2, REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, ADD_MEDIAN = TRUE, ylab = "Psychometric/Neurometric threshold ratio",
            STAT_COMPARISON = TRUE, yline = 0)
print(plot_NP_size_MT)
ggsave(file = paste0(dir_out, "NP_ratio_size_MT.pdf"), plot_NP_size_MT, width = w-1, height = h+1)


###now PN.ratio vs Task
plot_NP_task = data_NP %>% filter(!is.na(Task)) %>%
  plot_data(y  = "PN.ratio", x = "Task", jitter = 0.2, REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, ADD_MEDIAN = TRUE, ylab = "Normalized sensitivity",
            STAT_COMPARISON = TRUE, yline = 0, alpha_points = 0.5, left_margin = 5, mean_size = 1,
            bottom_margin = 5, hjust = 0.7, vjust = 0.8, angle = 30, test_cut = 0.01)
print(plot_NP_task)





#check tailoring, only for NP data
data_NP %>%
  count(Task, Task_parm) %>%
  #percentage within each task
  group_by(Task) %>%
  mutate(perc = n/sum(n))

#all data
data_main %>%
  count(Task, Task_parm) %>%
  #percentage within each task
  group_by(Task) %>%
  mutate(perc = n/sum(n))

data_NP %>%
  count(Task, Non_task_parm) %>%
  #percentage within each task
  group_by(Task) %>%
  mutate(perc = n/sum(n))

#stimulus size
data_NP %>%
  count(Task, Stimulus_size) %>%
  #percentage within each task
  group_by(Task) %>%
  mutate(perc = n/sum(n))



##PN.ratio vs Brain.areas
plot_NP_area = data_NP %>% plot_data(y  = "PN.ratio", x = "Brain.areas", jitter = 0.2, REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, ADD_MEDIAN = TRUE, ylab = "Normalized sensitivity",
                                     STAT_COMPARISON = TRUE, test_cut = 0.01, yline = 0, font_scale = 0.9)
print(plot_NP_area)
ggsave(file = paste0(dir_out, "PN_ratio_area.pdf"), plot_NP_area, width = 2/3* w, height = h)

##PN.ratio vs Task_var
plot_NP_task_var = data_NP %>%
  plot_data(y  = "PN.ratio", x = "Task_var", jitter = 0.2, REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, ADD_MEDIAN = TRUE, ylab = "Psychometric/Neurometric threshold ratio",
            STAT_COMPARISON = TRUE, yline = 0)
print(plot_NP_task_var)
ggsave(file = paste0(dir_out, "NP_ratio_task_var.pdf"), plot_NP_task_var, width = w, height = h+1)

#vs stimulus size
plot_NP_size = data_NP %>% filter(!is.na(Stimulus_size)) %>%
  plot_data(y  = "PN.ratio", x = "Stimulus_size", jitter = 0.2, REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, ADD_MEDIAN = TRUE, ylab = "Psychometric/Neurometric threshold ratio",
            STAT_COMPARISON = TRUE, yline = 0)
print(plot_NP_size)

#vs Year
plot_NP_year = data_NP %>%
  plot_data(y  = "PN.ratio", x = "Year", jitter = 0.2, REGRESSION_ALL = TRUE, REGRESSION_GROUP = TRUE, ADD_MEDIAN = FALSE,
            ylab = "Normalized sensitivity", yline = 0, font_scale = 0.9, xlab = "Year of publication")+
            scale_x_continuous(breaks = c(2000, 2010, 2020)) +
            annotate("text", x = min(data_NP$Year, na.rm = TRUE) + 3, y = max(data_NP$PN.ratio, na.rm = TRUE),
             label = get_pval_string(data_NP, "PN.ratio", "Year"),
             size = 4)
print(plot_NP_year)
ggsave(file = paste0(dir_out, "PN_ratio_year.pdf"), plot_NP_year, width = 0.8*2/3*w, height = h)
lm(PN.ratio ~ Year, data = data_NP) %>% summary()

#vs Number of neurons
plot_NP_N = data_NP %>%
  filter(Recording == "single-electrode") %>%
  plot_data(y  = "PN.ratio", x = "N_neurons", jitter = 0.2, REGRESSION_ALL = TRUE, REGRESSION_GROUP = TRUE, ADD_MEDIAN = FALSE,
            ylab = "Normalized sensitivity", yline = 0, font_scale = 0.9, xlab = "# neurons")
print(plot_NP_N)


###now do CP vs Task_parm but only for NP data
plot_Task_parm_subset_NP = data_NP %>%
  plot_data("Task_parm", "CP", jitter = 0.2, REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, STAT_COMPARISON = TRUE)
print(plot_Task_parm_subset_NP)



######CP-sensitivity simulation plots####
###1
#simullate 100 datapoints CP - P/N hypothetic relationship with P/N ratios with mean = 0.5 and sd = 0.2 but not lower than 0 and CP with the mean of 0.55 and sd = 0.05
set.seed(145)
data_sim = tibble(PN.ratio = c(rnorm(100, mean = 0.7, sd = 0.3) %>% pmax(0), runif(10, min = 0.02, max = 0.3)),
                  CP = 0.5 + 0.2*PN.ratio  + rnorm(110, mean = 0, sd = 0.05) + 0.01)

data_sim %<>% filter(PN.ratio > 0.05 & CP < 0.85 & CP > 0.45)

#plot it
plot_sim = data_sim %>%
  ggplot(aes(x = PN.ratio, y = CP)) +
    geom_point(size = 1, color = "blue", shape = 1) +
    geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
    #geom_smooth(se = FALSE, size = 1, color = "black", linetype = "dashed") +
    geom_point(aes(x = mean(PN.ratio), y = mean(CP)), size = 3, color = "black", fill = "blue", shape = 21, stroke = 0.5) +
    theme_classic() +
    #add horizontal line at 0.5
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", size = 0.5) +
    labs(x = "Sensitivity", y = "") +
    theme(
          axis.text = element_text(size = grid::unit(font_axis, "pt")),
          legend.text = element_text(size = grid::unit(font_legend, "pt")),
          legend.title = element_text(size = grid::unit(font_legend, "pt")),
      #make x axis readable
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      axis.text.y = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    ) +
  xlim(0, max(data_sim$PN.ratio) + 0.1) +
  ylim(0.45, max(data_sim$CP)) +
  coord_cartesian(xlim = c(0, NA)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)))
print(plot_sim)

#add neurometric threshold
data_sim %<>% mutate(Neurometric.threshold = 5/PN.ratio) # assuming that psychometric threshold = 13%

#plot neurometric threshold vs CP
plot_sim_N = data_sim %>%
  ggplot(aes(x = Neurometric.threshold, y = CP)) +
    geom_point(size = 1, color = "blue", shape = 1) +
    geom_smooth(formula = y ~ I(1 / x), method = "lm", se = FALSE, size = 1, color = "black") +
    #geom_smooth(se = FALSE, size = 1, color = "black", linetype = "dashed") +
    #add horizontal line at 0.5
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", size = 0.5) +
    theme_classic() +
    labs(x = "Threshold",y = "Choice Probability" ) +
    theme(
       axis.text = element_text(size = grid::unit(font_axis, "pt")),
       legend.text = element_text(size = grid::unit(font_legend, "pt")),
       legend.title = element_text(size = grid::unit(font_legend, "pt")),
      #make x axis readable
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.margin = margin(10, 10, 10, 10)
    ) +
  ylim(0.45, max(data_sim$CP) ) +
  coord_cartesian(xlim = c(0, NA)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)))

print(plot_sim_N)



#now stack them side by side using ggarrange
plot_sim_combined = ggarrange(plot_sim_N, plot_sim, ncol = 2, nrow = 1, labels = NULL, common.legend = TRUE, legend = "bottom", align = c ("hv"))
print(plot_sim_combined)
ggsave(file = paste0(dir_out, "simulated_CP_PN.pdf"), plot_sim_combined, width = 3.5, height = 2.0)


#####2 now illustrate the consequance of different sampling
set.seed(43)
#PN.ratio is sampled from exponantial distribution

data_sim1 = tibble(PN.ratio = rexp(3000, rate = 4) %>% ifelse(. > 1.5, 1.4 + rnorm(n=1, 0, 0.1), .),
                  CP = 0.5 + 0.2*PN.ratio  + rnorm(2000, mean = 0, sd = 0.05))
data_sim1$PN.ratio %>% cut(breaks = seq(0, 1.6, by = 0.2)) %>% table()

data_sim1 %<>% mutate(PN_cut = cut(PN.ratio, breaks = seq(0, 1.6, by = 0.2)))

#now simulate single electrode sampling by generating 100 random samples from normal distribution and finding the closest PN.ratio in data_sim1
PN_single = rnorm(100, mean = 0.8, sd = 0.2)

#now multi higher than 0.05

PN_multi = c(rnorm(200, 0.2, 0.2) %>% pmax(0.05))

data_sim1 %<>% rowwise() %>%
  mutate(Dist_to_single = min(abs(PN_single - PN.ratio)),
         Dist_to_multi = min(abs(PN_multi - PN.ratio)),
         ) %>%
  ungroup()


#create new columns marking sampled points
data_sim1 %<>% mutate(Sample  = case_when(Dist_to_single < 0.00085 ~ "tailored",
                                     Dist_to_multi < 0.000085 ~ "not tailored",
                                     TRUE ~ "not sampled") %>%
  factor(levels = c("tailored", "not tailored", "not sampled"))
)


#add mean points per sample
data_sim1 %<>%
  group_by(Sample) %>%
    mutate(PN_mean = ifelse(Sample != "not sampled", mean(PN.ratio), NA),
           CP_mean = ifelse(Sample != "not sampled", mean(CP), NA)) %>%
    ungroup()

data_sim1 %<>% mutate(CP_mean =  ifelse(Sample == "not tailored", CP_mean+0.001, CP_mean))

data_sim1 %<>% filter(PN.ratio < 1.2)

# Define colors
sample_colors <- c(
  "tailored" = "red",
  "not tailored" = "blue",
  "not sampled" = "grey70"
)

# MAIN PLOT (Scatter + regression)
p_main <- ggplot(data_sim1, aes(x = PN.ratio, y = CP,  fill = Sample, color = Sample, alpha = Sample != "not sampled")) +
  geom_point(data = data_sim1 %>% filter(Sample == "not sampled"),
           aes(x = PN.ratio, y = CP),
           shape = 1, size = 1, alpha = 0.3, color = "grey70") +
  geom_point(data = data_sim1 %>% filter(Sample != "not sampled"),
           aes(x = PN.ratio, y = CP, color = Sample),
           shape = 1, size = 1, alpha = 0.8) +
  geom_smooth(data = filter(data_sim1, Sample != "not sampled"),
              method = "lm", se = FALSE, size = 0.7, color = "black") +
  geom_point(aes(x = PN_mean, y = CP_mean), size = 3,
             color = "black", shape = 21, stroke = 0.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", size = 0.5) +
  scale_alpha_manual(values = c(0.3, 1)) +
  scale_color_manual(values = sample_colors) +
  scale_fill_manual(values = sample_colors) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Normalized sensitivity", y = "Choice Probability") +
  coord_cartesian(xlim = c(0, NA)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  ylim(0.4,0.8)

# DENSITY TOP
p_top <- ggplot(data_sim1, aes(x = PN.ratio, fill = Sample, color = Sample)) +
  geom_density(alpha = 0.4, size = 0.5) +
  scale_fill_manual(values = sample_colors) +
  scale_color_manual(values = sample_colors) +
  theme_void() +
  theme(legend.position = "none")  +
  coord_cartesian(xlim = c(0, NA)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)))

# DENSITY RIGHT
p_right <- ggplot(data_sim1, aes(x = CP, fill = Sample, color = Sample)) +
  geom_density(alpha = 0.4, size = 0.5) +
  scale_fill_manual(values = sample_colors) +
  scale_color_manual(values = sample_colors) +
  coord_flip() +
  theme_void() +
  theme(legend.position = "none")  +
  xlim(0.35, 0.85)

# Layout: patchwork (2x2), with empty placeholder in top-right
layout_plot <- p_top + plot_spacer() +
               p_main + p_right +
  plot_layout(ncol = 2, nrow = 2, widths = c(4, 1.2), heights = c(1.2, 4))

layout_plot

ggsave(file = paste0(dir_out, "simulated_CP_sampling.pdf"), layout_plot, width = 0.8*(2*w/3), height = h)

#now save the p_main but with the legend
p_main_legend <- p_main +
  theme(legend.position = "right",
        legend.title = element_text(size = grid::unit(font_legend, "pt")),
        legend.text = element_text(size = grid::unit(font_legend, "pt"))
        ) +
  guides(alpha = "none") +
  labs(fill = "Stimulus:", color = "Stimulus:")

ggsave(file = paste0(dir_out, "simulated_CP_sampling_legend.pdf"), p_main_legend, width = 0.8*(2*w/3), height = h)
############now mean + slope
set.seed(16)
data_sim2 = tibble(x = runif(10, 0.05, 1.1), #x is normalized sensitivity
                   y = 0.5 + 0.2*x + rnorm(10, mean = 0, sd = 0.05) - 0.022,
                   Slope = runif(10, 0.15, 0.25))

data_sim2 %<>% mutate(y = ifelse (y < 0.5, 0.52, y ))

data_sim2 %<>% get_segments(asp = diff(range(data_sim2$x))/ diff(range(data_sim2$y)), slope_var = Slope, L = 0.3)

plot_sc1 = data_sim2 %>%
  ggplot(aes(x = x, y = y)) +
      #wnat only SE, no regression line
      geom_smooth(aes(group = NA), method = "lm", se = TRUE, size = 1.5, color = NA, show.legend = FALSE) +
      geom_point(size = 3, color = "black", fill = "blue", shape = 21, stroke = 0.5) +
      geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end), size = 0.7, show.legend = FALSE, color = "black") +

      theme_classic() +
      #add horizontal line at 0.5
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", size = 0.5) +
      labs(x = "Normalized sensitivity", y = "mean CP") +
      theme(
        text = element_text(size = grid::unit(font_labels, "pt")),
        axis.text = element_text(size = grid::unit(font_axis, "pt")),
        legend.text = element_text(size = grid::unit(font_legend, "pt")),
        legend.title = element_text(size = grid::unit(font_legend, "pt")),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(size = grid::unit(font_labels, "pt")),
        axis.text.y = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
      ) +
   coord_cartesian(xlim = c(0, NA)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 1, by = 0.5))
print(plot_sc1)

set.seed(138)
data_sim3 = data_sim2 %>% mutate(x = sample(x))
#data_sim3 %<>% mutate(x = ifelse (y > 0.7, x - 0.35, x ))
data_sim3 %<>% get_segments(asp = diff(range(data_sim3$x))/ diff(range(data_sim3$y)), slope_var = Slope, L = 0.3)
plot_sc2 = data_sim3 %>%
  ggplot(aes(x = x, y = y)) +
      geom_smooth(aes(group = NA), method = "lm", se = TRUE, size = 1.5, color = NA, show.legend = FALSE) +
      geom_point(size = 3, color = "black", fill = "blue", shape = 21, stroke = 0.5) +
      geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end), size = 0.7, show.legend = FALSE, color = "black") +

      theme_classic() +
      #add horizontal line at 0.5
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", size = 0.5) +
      labs(x = "Normalized sensitivity", y = "") +
    theme(
      text = element_text(size = grid::unit(font_labels, "pt")),
      axis.text = element_text(size = grid::unit(font_axis, "pt")),
      legend.text = element_text(size = grid::unit(font_legend, "pt")),
      legend.title = element_text(size = grid::unit(font_legend, "pt")),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.title = element_text(size = grid::unit(font_labels, "pt")),
      axis.text.y = element_blank(),
      plot.margin = margin(10, 0, 10, 20)
    ) +
  coord_cartesian(xlim = c(0, NA)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 1, by = 0.5))
print(plot_sc2)


######Tailoring tasks######
data_main %>% count(Task_parm, Non_task_parm)

#for single electrode recordings,  percentage that tailored to task parameters
data_main %>%
  filter(Recording.technique == "single electrode") %>%
  count(Task_parm) %>%
  mutate(perc = n/sum(n))

#now stimulus size
data_main %>%
  filter(Recording.technique == "single electrode") %>%
  count(Stimulus_size) %>%
  mutate(perc = n/sum(n))

data_main %>% count(Task_tailor)
data_main %>% count(Recording.technique, Task_tailor)
data_main %>% count(Brain.areas, Task_tailor)

#plot per Tailoring
plot_tailor = data_main %>% plot_data("Task_tailor", "CP", ADD_MEDIAN = TRUE, jitter = 0.3, REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, STAT_COMPARISON = TRUE, test_cut = 0.01)
# plot_tailor = plot_tailor + coord_cartesian(clip = "off") +
#                     annotation_custom(grob = textGrob("Tailor:\ntask / non-task\nparams", x = 0.87, y = -0.3, rot = 35, just = "left", gp = gpar(fontsize = font_axis, fontface = "bold", lineheight = 0.8)))

print(plot_tailor)
ggsave(file = paste0(dir_out,"tailoring.pdf"), plot_tailor, width = 0.93*2/3*w, height = h+1)

#Suplement
plot_tailor_sup = data_main %>% plot_data("Task_tailor", "CP", ADD_MEDIAN = FALSE, cex_jitter = 2, hjust = 0.7, vjust = 0.7, LABELS = TRUE, REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE)
plot_tailor_sup = plot_tailor_sup + coord_cartesian(clip = "off") +
                    annotation_custom(grob = textGrob("Tailor:\ntask / non-task\nparams", x = -0.17, y = -0.17, rot = 35, just = "left", gp = gpar(fontsize = font_axis, fontface = "bold", lineheight = 0.8)))

print(plot_tailor_sup)
ggsave(file = paste0(dir_out,"tailoring_sup.pdf"), plot_tailor_sup, width = w, height = h+3)



#now only tailoring task parameter
data_main %>% count(Task_parm, Non_task_parm, Task_tailor)

plot_tailor_task = data_main %>% plot_data("Task_parm", "CP", jitter = 0.2,  REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, STAT_COMPARISON = TRUE)
plot_tailor_task = plot_tailor_task + coord_cartesian(clip = "off") +
                    annotation_custom(grob = textGrob("Tailor:\ntask params", x = -0.17, y = -0.17, rot = 35, just = "left", gp = gpar(fontsize = font_axis, fontface = "bold", lineheight = 0.8)))

print(plot_tailor_task)
ggsave(file = paste0(dir_out,"tailoring_task.pdf"), plot_tailor_task, width = w, height = h+1)

plot_tailor_non_task = data_main %>% plot_data("Non_task_parm", "CP", jitter = 0.2,  REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, STAT_COMPARISON = TRUE)
plot_tailor_non_task = plot_tailor_non_task + coord_cartesian(clip = "off") +
                    annotation_custom(grob = textGrob("Tailor:\nnon-task\nparams", x = -0.17, y = -0.17, rot = 35, just = "left", gp = gpar(fontsize = font_axis, fontface = "bold", lineheight = 0.8)))
print(plot_tailor_non_task)
ggsave(file = paste0(dir_out,"tailoring_non_task.pdf"), plot_tailor_non_task, width = w, height = h+1)

lm(CP ~ Task_tailor, data = data_main) %>% summary()
lm(CP ~ Task_tailor, data = data_main %>% filter(Brain.areas== "MT")) %>% summary()
lm(CP ~ Task_parm + Non_task_parm, data = data_main) %>% summary()
aov(CP ~ Task_parm + Non_task_parm, data = data_main) %>% summary()
aov(CP ~ Non_task_parm + Task_parm, data = data_main) %>% summary()




aov(CP ~ Brain.areas + Recording + Task_tailor, data = data_main) %>% summary()


#plot vs Tailoring but conditioned on single electrode recording
plot_tailor_recording = data_main %>% filter(Recording == "single-electrode") %>%
  plot_data("Task_tailor", "CP", jitter = 0.2, ADD_MEDIAN = TRUE, REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, STAT_COMPARISON = TRUE, test_cut = 0.01)
print(plot_tailor_recording)
ggsave(file = paste0(dir_out,"tailoring_recording.pdf"), plot_tailor_recording, width = w, height = h+3)
lm(CP ~ Task_tailor, data = data_main %>% filter( Recording.technique == "single electrode")) %>% summary()
lm(CP ~ Task_tailor, data = data_main %>% filter(Brain.areas == "MT", Recording.technique == "single electrode")) %>% summary()

#now multiple electrode recordings
plot_tailor_recording_multi = data_main %>% filter(Recording == "multi-electrode") %>%
  plot_data("Task_tailor", "CP", jitter = 0.2, ADD_MEDIAN = TRUE, REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, STAT_COMPARISON = TRUE, test_cut = 0.01)
print(plot_tailor_recording_multi)
ggsave(file = paste0(dir_out,"tailoring_recording_multi.pdf"), plot_tailor_recording_multi, width = w, height = h+3)

##for single electrode recordings and MT
plot_tailor_recording_single_MT = data_main %>% filter(Recording == "single-electrode" & Brain.areas == "MT") %>%
  plot_data("Task_tailor", "CP", jitter = 0.2, ADD_MEDIAN = TRUE, REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, STAT_COMPARISON = TRUE, test_cut = 0.01)
print(plot_tailor_recording_single_MT)
ggsave(file = paste0(dir_out,"tailoring_recording_single_MT.pdf"), plot_tailor_recording_single_MT, width = w, height = h+3)

#####Stimulus size####
data_main %>% count(Stimulus_size)

data_main %>%  filter(Recording == "single-electrode") %>% count(Stimulus_size)
data_main %>% count(Papers, Monkey, Brain.areas, Stimulus_size) %>% View()


plot_Stimulus_size = data_main %>%
                          filter(!is.na(Stimulus_size)) %>%
                          plot_data("Stimulus_size", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, STAT_COMPARISON = TRUE, jitter = 0.2)
print(plot_Stimulus_size)
ggsave(file = paste0(dir_out, "stimulus_size.pdf"), plot_Stimulus_size, width = 0.7*2/3*w, height = h)

plot_Stimulus_size_single = data_main %>% filter(!is.na(Stimulus_size) & Recording == "single-electrode") %>%
  plot_data("Stimulus_size", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, STAT_COMPARISON = TRUE, jitter = 0.2)
print(plot_Stimulus_size_single)
ggsave(file = paste0(dir_out, "stimulus_size_single.pdf"), plot_Stimulus_size_single, width = w, height = h+1)

#now only MT and single electrode recordings
plot_Stimulus_size_single_MT = data_main %>% filter(!is.na(Stimulus_size) & Brain.areas == "MT" & Recording == "single-electrode") %>%
  plot_data("Stimulus_size", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, STAT_COMPARISON = TRUE, jitter = 0.2)
print(plot_Stimulus_size_single_MT)
ggsave(file = paste0(dir_out, "stimulus_size_single_MT.pdf"), plot_Stimulus_size_single_MT, width = w, height = h+1)



test_btw_size = test_pairs(data_main %>% filter(!is.na(Stimulus_size)), var = "Stimulus_size")
test_btw_size %>% filter(p < 0.05)
test_btw_size_MT = test_pairs(data_main %>% filter(!is.na(Stimulus_size) & Brain.areas == "MT"), var = "Stimulus_size")
test_btw_size_MT %>% filter(p < 0.05)

test_btw_size_single = test_pairs(data_main %>% filter(!is.na(Stimulus_size) & Recording == "single-electrode"), var = "Stimulus_size")
test_btw_size_single %>% filter(p < 0.05)
test_btw_size_single_MT = test_pairs(data_main %>% filter(!is.na(Stimulus_size) & Brain.areas == "MT" & Recording == "single-electrode"), var ="Stimulus_size")
test_btw_size_single_MT %>% filter(p < 0.05)


aov(CP ~ Brain.areas + Recording + Stimulus_size, data = data_main) %>% summary()
aov(CP ~ Brain.areas + Recording + Stimulus_size + Task_tailor, data = data_main) %>% summary()
aov(CP ~ Brain.areas + Recording + Task_tailor + Stimulus_size, data = data_main) %>% summary()


###combine in two categories
data_main %<>% mutate(Stimulus_size2 = case_when(Stimulus_size %in%   c("fit to RF","smaller than RF") ~ "< 2RF",
                                                        Stimulus_size %in%   c("fit to population","not fit") ~  "> 2RF") %>%
    factor(levels = c("> 2RF", "< 2RF"), ordered = TRUE))


plot_Stimulus_size2 = data_main %>%
  filter(!is.na(Stimulus_size2)) %>%
  plot_data("Stimulus_size2", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, jitter = 0.2, STAT_COMPARISON = TRUE)
print(plot_Stimulus_size2)
ggsave(file = paste0(dir_out, "stimulus_size2.pdf"), plot_Stimulus_size2, width = w, height = h)

test_btw_size2 = test_pairs(data_main %>% filter(!is.na(Stimulus_size2)), var = "Stimulus_size2")
test_btw_size2 %>% filter(p < 0.05)

test_btw_size2_MT = test_pairs(data_main %>% filter(!is.na(Stimulus_size2) & Brain.areas == "MT"), var = "Stimulus_size2")
test_btw_size2_MT %>% filter(p < 0.05)

test_btw_size2_single = test_pairs(data_main %>% filter(!is.na(Stimulus_size2) & Recording == "single-electrode"), var = "Stimulus_size2")
test_btw_size2_single %>% filter(p < 0.05)

test_btw_size2_single_MT = test_pairs(data_main %>% filter(!is.na(Stimulus_size2) & Brain.areas == "MT" & Recording == "single-electrode"), var = "Stimulus_size2")
test_btw_size2_single_MT %>% filter(p < 0.05)
aov(CP ~ Brain.areas + Recording + Stimulus_size2, data = data_main) %>% summary()
aov(CP ~ Brain.areas + Recording + Stimulus_size2 + Task_tailor, data = data_main) %>% summary()
aov(CP ~ Brain.areas + Recording + Task_tailor + Stimulus_size2, data = data_main) %>% summary()


#########Stimulus size ratio
#find the data which could be added to the main data when no Stimulus size ratio is present in main data
data %>%  filter(Ratio.of.stimulus.size.to.RF.size != "" & Main.epoch == 2) %>% View()
#only Nienborg and Cumming, Dodd data has per monkey for stimulus ratio RF data
data_add = data %>%  filter(Ratio.of.stimulus.size.to.RF.size != "" & Main.epoch == 2 & Papers == c("Nienborg & Cumming (2006)") & method == "Grand.CP.random.seeds")
data_st_size_ratio = data_main %>%
  filter(Ratio.of.stimulus.size.to.RF.size != "") %>%
  bind_rows(data_add)


data_st_size_ratio %<>% rename(St_size_ratio = Ratio.of.stimulus.size.to.RF.size)
data_st_size_ratio %>% pull(St_size_ratio) %>% unique()

data_st_size_ratio %<>% mutate(St_size_ratio = St_size_ratio %>% delete_spaces())
data_st_size_ratio %<>%  separate(St_size_ratio, into = c("St_size_ratio", "St_size_ratio_range"), sep = "\\(", extra = "merge", fill = "right")

#if - in  St_size_ratio, take simple average
data_st_size_ratio %<>% mutate(St_size_ratio = case_when(St_size_ratio == '' ~ NA,
  grepl("-", St_size_ratio) ~ ((str_split(St_size_ratio, "-", simplify = TRUE) %>% .[,1] %>% as.numeric()) + (str_split(St_size_ratio, "-", simplify = TRUE) %>% .[,2] %>% as.numeric())) / 2,
  TRUE ~ St_size_ratio %>% as.numeric()) )

data_st_size_ratio %>% count(St_size_ratio)
plot_St_size_ratio =  data_st_size_ratio %>%
  plot_data("St_size_ratio", "CP", REGRESSION_ALL = TRUE, REGRESSION_GROUP = FALSE, jitter = 0.2, xlab = "Stimulus size/RF ratio", ADD_MEDIAN = FALSE)
print(plot_St_size_ratio)

ggsave(file = paste0(dir_out, "stimulus_size_ratio.pdf"), plot_St_size_ratio, width = w, height = h)



#####Brain area#####
plot_area = data_main  %>% plot_data("Brain.areas", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, CONNECT_PER_PARER = FALSE, jitter = 0.3,
                                     ADD_MEDIAN = TRUE, STAT_COMPARISON = TRUE, angle = 30, hjust = 1, xlab = "", test_cut = 0.01, alpha_points = 0.5, left_margin = 5, mean_size = 1, bottom_margin = 5)
# plot_area = plot_area +
#   scale_x_discrete(labels = paste0(levels(data_main$Brain.areas), "\n", c(1:4, rep("", 4)), "\n", c(1,2, "", "", 3:6))) +
#   coord_cartesian(clip = "off") +
#   annotation_custom(grob = textGrob("Levels:\nventral\ndorsal", x = -0.1, y = -0.07, just = "left", gp = gpar(fontsize = font_axis, fontface = "bold", lineheight = 0.8)))

print(plot_area)
ggsave(file = paste0(dir_out,"brain_areas.pdf"), plot_area, width = 0.8*(2*w/3), height = 0.85*h)

#plot for single electrode recordings only
plot_area_single = data_main %>% filter(Recording == "single-electrode") %>% plot_data("Brain.areas", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, CONNECT_PER_PARER = FALSE, jitter = 0.3,
                                     ADD_MEDIAN = TRUE, STAT_COMPARISON = TRUE, angle = 30, hjust = 1, xlab = "", test_cut = 0.01, font_scale = 0.9)
print(plot_area_single)
ggsave(file = paste0(dir_out,"brain_areas_single.pdf"), plot_area_single, width = 0.8*2/3*w, height = 0.9*h)





##now the data for papers that only have 2 areas for comparison
#add the papers that have comparable data only for two monkeys combined
data_area_2 = data_main %>%
  bind_rows(data %>% filter(Main.epoch == 2& Papers %in% c("Nienborg & Cumming (2006)", "Kang & Maunsell (2020)")) %>% filter_best_method())

data_area_2 = data_area_2 %>%
  mutate(group = paste(Papers, Monkey, Epoch)) %>%
  count(group) %>%
  filter(n > 1) %>%
  left_join(data_area_2 %>% mutate(group = paste(Papers, Monkey, Epoch)))

plot_area_2 = data_area_2  %>% plot_data("Brain.areas", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, CONNECT_PER_PARER = TRUE, jitter = 0,
                                     ADD_MEDIAN = FALSE, STAT_COMPARISON = FALSE, font_scale = 0.9)

plot_area_2 = plot_area_2 + scale_x_discrete(drop = FALSE)
print(plot_area_2)
ggsave(file = paste0(dir_out,"brain_areas_2.pdf"), plot_area_2, width = 0.8*2/3*w, height = 0.9*h)

Papers = data_area_2 %>% select(Papers, Brain.areas) %>% unique() %>% count(Papers) %>% arrange(-n) %>% filter(n == 2) %>% pull(Papers)

data_area_2  %<>% mutate(N.P.ratio = N.P.ratio %>% as.numeric(), PN.ratio = 1/ N.P.ratio)

data_area_2  %>%  plot_data("Brain.areas", "PN.ratio", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, CONNECT_PER_PARER = TRUE, jitter = 0,
            ADD_MEDIAN = FALSE, STAT_COMPARISON = FALSE, angle = 0, hjust = 0.5, xlab = "Brain areas", ylab = "Psychometric/Neurometric threshold ratio", yline = 0)

data_area_2 %>% filter(!is.na(PN.ratio)) %>% select(Papers, Monkey, Brain.areas, Epoch, CP, PN.ratio) %>% View()

###paired test between areas


##Supplements
plot_area_sup = data_main  %>% plot_data("Brain.areas", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, jitter = 0.3,
                                      STAT_COMPARISON = FALSE, angle = 0, hjust = 0.5, LABELS = TRUE, ADD_MEDIAN = FALSE)

print(plot_area_sup)
ggsave(file = paste0(dir_out,"brain_areas_sup.pdf"), plot_area_sup, width = w, height = h+1)


data_main %>% count(Brain.areas)
data_main %>% count(Brain.areas, SIGN, CP>0.5)

data_main %>% dlply("Brain.areas", function(s) if(nrow(s) > 2) wilcox.test(s$CP, mu = 0.5))

#make a table with results of wilcox.test between areas

test_btw_areas = test_pairs(data_main)
test_btw_areas %>% filter(p < 0.05)


#plot N_neurons vs Brain area
plot_N_neurons = data_main %>%
  filter(!is.na(N_neurons),  Recording == "single-electrode") %>%
  plot_data(y = "N_neurons", x = "Brain.areas", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, CONNECT_PER_PARER = FALSE, jitter = 0.3,
            ADD_MEDIAN = TRUE, STAT_COMPARISON = TRUE, angle = 30, hjust = 1,  test_cut = 0.01, alpha_points = 0.5, left_margin = 5,
            mean_size = 1, bottom_margin = 5, yline = 0, ylab = "N neurons", xlab = "Brain areas")

print(plot_N_neurons)


#####simulation plots Brain area#######
data_sim_brain_1 = data.frame(areas = c("V1", "V2", "V4", "IT") %>% factor(levels =  c("V1", "V2", "V4", "IT")), CP = rep(0.55, 4))

plot_simulation_simple = function(data, x, y, xlab, ylab, breaks=c(0.5)) {
  data %>%
    ggplot(aes(x = {{x}}, y = {{y}})) +
    geom_point(size = 3, color = "black", fill = "blue", shape = 21, stroke = 0.5) +
    theme_classic() +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", size = 0.5) +
    labs(x = xlab, y = ylab) +
    theme(
      text = element_text(size = grid::unit(8, "pt")),
      axis.text = element_text(size = grid::unit(0.8*font_axis, "pt")),
      legend.text = element_text(size = grid::unit(font_legend, "pt")),
      legend.title = element_text(size = grid::unit(font_legend, "pt")),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.margin = margin(10, 0, 10, 20)
    ) +
    scale_y_continuous(breaks = breaks, limits = c(0.5, 0.61), expand = expansion(mult = c(NA, 0)))
}

plot_sim_brain_1 = plot_simulation_simple(data_sim_brain_1, areas, CP, xlab = "", ylab = "mean CP")

print(plot_sim_brain_1)
ggsave(file = paste0(dir_out, "simulated_brain_areas_1.pdf"), plot_sim_brain_1, width = 2, height = 1.7)

data_sim_brain_2 = data.frame(areas = c("V1", "V2", "V4", "IT") %>% factor(levels =  c("V1", "V2", "V4", "IT")), CP =c(0.53, 0.55, 0.57, 0.6))

plot_sim_brain_2 = plot_simulation_simple(data_sim_brain_2, areas, CP, xlab = "", ylab = "mean CP")

print(plot_sim_brain_2)
ggsave(file = paste0(dir_out, "simulated_brain_areas_2.pdf"), plot_sim_brain_2, width = 2, height = 1.7)



#######Stimulus duration##########
data_main %>%  filter(!is.na(St_duration)) %>% count(St_duration) %>% mutate(percent = n/sum(n)*100) %>% arrange(desc(percent))




plot_St_duration = data_main %>% plot_data("St_duration", "CP", REGRESSION_ALL = TRUE, REGRESSION_GROUP = TRUE, jitter = 20/1000, xlab = "Stimulus duration, s", ADD_MEDIAN = FALSE,
                                           regression_width = 1.5, left_margin = 10, angle = 0, hjust = NULL)
print(plot_St_duration)
ggsave(file = paste0(dir_out, "stimulus_duration.pdf"), plot_St_duration, width = 0.8*(2*w/3), height = h)

#now only for single electrode recordings
plot_St_duration_single = data_main %>% filter(Recording == "single-electrode") %>% plot_data("St_duration", "CP", REGRESSION_ALL = TRUE, REGRESSION_GROUP = TRUE, jitter = 20/1000, xlab = "Stimulus duration, s", ADD_MEDIAN = FALSE)
print(plot_St_duration_single)
ggsave(file = paste0(dir_out, "stimulus_duration_single.pdf"), plot_St_duration_single, width = w, height = h+1)

#now single electrode and MT
plot_St_duration_single_MT = data_main %>% filter(Recording == "single-electrode" & Brain.areas == "MT") %>% plot_data("St_duration", "CP", REGRESSION_ALL = TRUE, REGRESSION_GROUP = TRUE, jitter = 20/1000, xlab = "Stimulus duration, s", ADD_MEDIAN = FALSE)
print(plot_St_duration_single_MT)
ggsave(file = paste0(dir_out, "stimulus_duration_single_MT.pdf"), plot_St_duration_single_MT, width = w, height = h+1)


#now only RT tasks
plot_St_duration_RT = data_main %>% filter(RT_task == "RT") %>% plot_data("St_duration", "CP", REGRESSION_ALL = TRUE, REGRESSION_GROUP = TRUE, 
                                                                          jitter = 20/1000, xlab = "Stimulus duration, s", ADD_MEDIAN = FALSE, legend_position = "right" )
print(plot_St_duration_RT)
ggsave(file = paste0(dir_out, "stimulus_duration_RT.pdf"), plot_St_duration_RT, width = 0.85*w, height = h)



##compare only random dot and MT
data_main %>% filter(Description.of.the.stimulus == "random dots", Task.type == "2 (coarse discrimination)", Brain.areas == "MT", Monkey != "E and J" ) %>%
plot_data("St_duration", "CP", REGRESSION_ALL = TRUE, REGRESSION_GROUP = TRUE, jitter = 20/1000, xlab = "Stimulus duration, s", ADD_MEDIAN = FALSE)




#####Simulation plots for stimulus duration
data_sim_dur = data.frame(St_duration = c(0.5, 1, 1.5), CP = rev(c(0.52, 0.54, 0.58)), model = "FF") %>%
    bind_rows( data.frame(St_duration = c(0.5, 1, 1.5), CP = c(0.52, 0.54, 0.58), model = "FB") ) %>%
   bind_rows( data.frame(St_duration = c(0.5, 1, 1.5), CP = c(0.545, 0.545, 0.545), model = "FF2") )



plot_sim_dur = ggplot(data_sim_dur, aes(x = St_duration, y = CP, color = model, group = model)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE)+
  theme_classic() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", size = 0.5) +
  # Adding Direct Labels
  annotate("text", x = 1.20, y = 0.535, label = "Feedforward",
           color = "blue", hjust = 0, size = font_labels/2.845) +
  annotate("text", x = 1.15, y = 0.58, label = "Feedback",
           color = "red", hjust = 0, size = font_labels/2.845) +
  labs(x = "Stimulus duration, s", y = "mean CP", title = "Across-study relationship") +
  theme(
    text = element_text(size = grid::unit(font_labels, "pt")),
    axis.text = element_text(size = grid::unit(font_axis, "pt")),
    legend.text = element_text(size = grid::unit(font_legend, "pt")),
    legend.title = element_text(size = grid::unit(font_legend, "pt")),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 0, 10, 20),
    legend.position = "none",
    plot.title = element_text(size = grid::unit(font_labels, "pt"), face = "bold")
  ) +
  scale_x_continuous(breaks = c(0.5, 1, 1.5), limits = c(0.4, 1.6), expand = expansion(mult = c(NA, 0))) +
  scale_color_manual(values = c("FF"= "blue", "FB"= "red", "FF2"= "blue"), name = "Model")
print(plot_sim_dur)




####Task types####
#data_main %>% count(Stimulus.type,Task.type)

#data_main %>% count(Task,Stimulus.type,Task.type)


test_pairs(data_main, var = "Task")

test_pairs(data_main %>% filter(Recording == "single-electrode"), var = "Task")






data_main %>% filter(Task == "coarse-discrimination") %>% count(Task, Difficulty.parameter)

data_main %>% filter(Task == "detection") %>% View()



plot_task = data_main %>% plot_data("Task", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, jitter = 0.4, xlab = "", STAT_COMPARISON = TRUE, alpha_points = 0.5,
                                    left_margin = 5, mean_size = 1, bottom_margin = 5, hjust = 0.7, vjust = 0.8, angle = 30, test_cut = 0.01)
print(plot_task)
ggsave (file = paste0(dir_out, "task.pdf"), plot_task, width = 0.8*(2*w/3), height = 0.95*h)

test_btw_tasks = test_pairs(data_main, "Task")
test_btw_tasks %>% filter(p < 0.05)

lm(CP ~ Task +Recording+Brain.areas+Task_tailor, data = data_main) %>% summary()
lm(CP ~ Task +Recording+Brain.areas+Task_tailor, data = data_main %>% filter(Task != "bistable")) %>% summary()

#only single electrode recordings
plot_task_single = data_main %>% filter(Recording == "single-electrode") %>%
  plot_data("Task", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, jitter = 0.2, xlab = "", STAT_COMPARISON = TRUE,
            alpha_points = 0.5, left_margin = 5, mean_size = 1, bottom_margin = 5, hjust = 0.7, vjust = 0.8, angle = 30,
  legend_position = "right", test_cut = 0.01)
print(plot_task_single)
ggsave(file = paste0(dir_out, "task_single.pdf"), plot_task_single, width = 0.9*w, height = 1.07*h)

#only single and MT
plot_task_single_MT = data_main %>% filter(Recording == "single-electrode" & Brain.areas == "MT") %>% plot_data("Task", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, jitter = 0.2, xlab = "", STAT_COMPARISON = TRUE)
print(plot_task_single_MT)
ggsave(file = paste0(dir_out, "task_single_MT.pdf"), plot_task_single_MT, width = w, height = h+1)

###fixed time vs reaction time task
#count % of RT task
data_main %>% count(RT_task) %>% mutate(percent = n/sum(n)*100)


data_main %>% count(RT_task, Task)
plot_RT = data_main %>%
  mutate(Not_Detection = ifelse (Task != "detection", "Not detection task", "Detection task") %>% factor(levels = c("Not detection task", "Detection task"))) %>%
  plot_data("RT_task", "CP", REGRESSION_ALL = FALSE,
            REGRESSION_GROUP = FALSE, jitter = 0.2, xlab = "", STAT_COMPARISON = TRUE , shape = c("Detection task" = 1, "Not detection task" = 16),
            group = "Not_Detection", stroke = 1, legend_position = "bottom", color_legend = guide_legend(position = "right", title = "Brain areas"), font_scale = 0.9,
  angle = 0, hjust = 0.5)

plot_RT = plot_RT + guides(fill = "none", shape = guide_legend(title = ""))
print(plot_RT)
ggsave(file = paste0(dir_out, "RT_task.pdf"), plot_RT, width = 2/3*w, height = 1.07*h)

lm(CP ~ RT_task, data = data_main %>% filter(Task == "coarse-discrimination")) %>% summary()
lm(CP ~ RT_task +Task, data = data_main) %>% summary()


## plot stumulus duration vs task
plot_st_duration_task = data_main %>% filter(!is.na(St_duration)) %>%
  plot_data("Task", "St_duration", REGRESSION_ALL = FALSE,
            REGRESSION_GROUP = FALSE, jitter = 0.2, xlab = "", STAT_COMPARISON = TRUE, ylab = "Stimulus duration, s",
    legend_position = "right", font_scale = 0.9, test_cut = 0.01)
print(plot_st_duration_task)
ggsave(file = paste0(dir_out, "st_duration_task.pdf"), plot_st_duration_task, width = 0.9*w, height = 1.08*h)

##number of neurons vs task
plot_N_neurons_task = data_main %>%
  filter(!is.na(N_neurons),  Recording == "single-electrode") %>%
  plot_data(y = "N_neurons", x = "Task", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, CONNECT_PER_PARER = FALSE, jitter = 0.3,
            ADD_MEDIAN = TRUE, STAT_COMPARISON = TRUE, angle = 30, hjust = 1, alpha_points = 0.5, left_margin = 5,
            mean_size = 1, bottom_margin = 5, yline = 0, ylab = "N neurons", xlab = "Task")


#######Lapse rate###############
data_main$Lapse.rate %>% unique()

plot_Lapse = data_main %>%
  plot_data("Lapse.rate", "CP", REGRESSION_ALL = TRUE, REGRESSION_GROUP = TRUE,  jitter = 0,
            xlab = "Lapse rate, %", ADD_MEDIAN = FALSE, font_scale = 0.9)
print(plot_Lapse)


lm(CP ~ Lapse.rate, data = data_main) %>% summary()



####Task exposure#######


plot_learning = data_main %>%
  plot_data("Learning", "CP", REGRESSION_ALL = TRUE, REGRESSION_GROUP = TRUE,
            jitter = 0.5, xlab = "Task exposure, months", ADD_MEDIAN = FALSE,
  font_scale = 0.9)
print(plot_learning)





#####Saccade targets vs stimulus location#####
plot_targets = data_main %>% filter(!is.na(Predict_targets)) %>%
  plot_data("Predict_targets", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, jitter = 0.2, xlab = "Saccade targets",
            STAT_COMPARISON = TRUE, legend_position = "right", font_scale = 0.9, test_cut = 0.01)
print(plot_targets)
ggsave(file = paste0(dir_out, "saccade_targets.pdf"), plot_targets, width = 0.8*w, height = h)



data_main %<>% mutate(St_pos = Stimulus_pos, T1_pos = Target1_pos , T2_pos = Target2_pos)
#apply to St_pos, T1_pos, T2_pos: ~ %>% delete_spaces() %>% str_split("\\(", simplify = TRUE)[,2]
data_main %<>% mutate(across(c(St_pos, T1_pos, T2_pos), ~ .x %>% delete_spaces() %>% get_inside_bracket() ))
data_main %>% filter(!is.na(St_pos) & !is.na(T1_pos) & !is.na(T2_pos)) %>% count(St_pos, T1_pos, T2_pos) #only 4 points - 1 study, also Stimulus (0,0) cannot be used as fully symetrical

# get_z = function(x) {
#   x = x %>% str_split(",", simplify = TRUE) %>% apply(2, as.numeric)
#   x = complex(real = x[,1], imaginary = x[,2])
#   return(x)
# }
#
# #now we need to split each position into two values x and y and then calculate a complex z = x+i*y
# data_main %<>% mutate(across(c(St_pos, T1_pos, T2_pos), ~ .x %>% get_z()))
#
# data_main$St_pos %>% str_split(",", simplify = TRUE) %>% apply(2, as.numeric) %>% data.frame() %>% mutate(z = complex(real = X1, imaginary = X2))


#####Method of CP calulation###########
data_main %>% count(method) %>% mutate(percent = n/sum(n)*100) %>% arrange(desc(percent))



data_CP %>% count(Papers, Monkey, Brain.areas, Epoch) %>% View()


data_Grand$Papers %>% unique() #12 studies


plot_Grand = data_Grand %>%
    plot_data("method_combined", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, CONNECT_PER_PARER = TRUE, group_connect = c("Papers", "Monkey", "Brain.areas", "Epoch"),
                CONNECT_REGRESSION = FALSE, jitter = 0, xlab = "", STAT_COMPARISON = TRUE, PAIRED = TRUE, ADD_MEDIAN = TRUE, MEAN_NUDGE = TRUE,
              width_error = 0.03, legend_position = "right", font_scale = 0.9)

print(plot_Grand)
ggsave(file = paste0(dir_out, "Grand_vs_Zero.pdf"), plot_Grand, width = 2/3*w, height = h)

#do paired wilcox test
wilcox.test(data_Grand$CP[data_Grand$method_combined == "Zero signal"], data_Grand$CP[data_Grand$method_combined == "Grand CP"], paired = TRUE) #p = 0.42

###random seed vs frozen seeds
data_seeds$Papers %>% unique() #only 5 studies
#number of points
data_seeds %>% nrow() %>% .[]/2

plot_seeds = data_seeds %>%
  plot_data("method_combined2", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, CONNECT_PER_PARER = TRUE, group_connect = c("Papers", "Monkey", "Brain.areas", "Epoch"),
            CONNECT_REGRESSION = FALSE, jitter = 0, xlab = "", STAT_COMPARISON = TRUE, PAIRED = TRUE, ADD_MEDIAN = TRUE, MEAN_NUDGE = TRUE,
            width_error = 0.03, legend_position = "right", font_scale = 0.9)
print(plot_seeds)
ggsave(file = paste0(dir_out, "seeds.pdf"), plot_seeds, width = 2/3*w, height = 0.9*h)

#do paired wilcox test
wilcox.test(data_seeds$CP[data_seeds$method_combined2 == "Frozen seeds"], data_seeds$CP[data_seeds$method_combined2 == "Random seeds"], paired = TRUE)


####Method of estimating the preferences of a neuron##########

#proportion of studies that used different methods

data_main %>% filter(!is.na(method_pref)) %>% count(method_pref, Recording) %>% group_by(method_pref) %>% mutate(percent = n/sum(n)*100)

plot_pref = data_main %>% filter(!is.na(method_pref)) %>%
  plot_data("method_pref", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, jitter = 0.2,
            STAT_COMPARISON = TRUE, xlab = "Neuron preference estimation from trials of", width_error = 0.03,
            legend_position = "right", font_scale = 0.9)
print(plot_pref)
ggsave(file = paste0(dir_out, "preferences.pdf"), plot_pref, width = 2/3*w, height = h)

#wilcox.test only for MT
wilcox.test(data_main$CP[data_main$method_pref == "passive viewing" & data_main$Brain.areas == "MT"], data_main$CP[data_main$method_pref == "task itself" & data_main$Brain.areas == "MT"]) #p = 0.27
#wilcox.test only for MT and single neurons recoordings
wilcox.test(data_main$CP[data_main$method_pref == "passive viewing" & data_main$Brain.areas == "MT" & data_main$Recording.technique == "single electrode"],
             data_main$CP[data_main$method_pref == "task itself" & data_main$Brain.areas == "MT" & data_main$Recording.technique == "single electrode"]) #p = 0.03

plot_pref_MT_single = data_main %>% filter(!is.na(method_pref)& Brain.areas == "MT" & Recording.technique == "single electrode") %>%
  plot_data("method_pref", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, jitter = 0.2,  STAT_COMPARISON = TRUE, xlab = "Neuron preference estimation from", width_error = 0.03)
print(plot_pref_MT_single )
ggsave(file = paste0(dir_out, "preferences_MT_single.pdf"), plot_pref_MT_single , width = w, height = h)
#now lm model that takes into account all factors
lm(CP ~ method_pref+Recording+Brain.areas+Task_tailor+Task, data = data_main) %>% summary() #0.05

#####type of the stimulus and task#####
##task parameter
data_main %>% count(Task_var, Task.parameter) %>% View()



data_main %>% count(Task_var) %>% mutate(percent = n/sum(n)*100) %>% arrange(desc(percent))

plot_task_var = data_main %>%
    plot_data("Task_var", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, jitter = 0.2,
              STAT_COMPARISON = TRUE, xlab = "Task parameter", test_cut = 0.01,  font_scale = 0.9)
print(plot_task_var)


#now plot only MT and single electrode
plot_task_var_MT = data_main %>% filter(Brain.areas == "MT") %>%
  plot_data("Task_var", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, jitter = 0.2,
            STAT_COMPARISON = TRUE, xlab = "Task parameter", font_scale = 0.9)
print(plot_task_var_MT)
ggsave(file = paste0(dir_out, "task_parameter_MT.pdf"), plot_task_var_MT, width = 0.5*w, height = 0.8*h)

#now lm model that takes into account all factors
lm(CP ~ Task_var+Recording+Brain.areas+Task_tailor+Task, data = data_main) %>% summary() #depth is higher than direction

###stimulus parameter
data_main %>%
  count(Task_var, Description.of.the.stimulus) %>% View()

data_main %>% count(St_type, Description.of.the.stimulus) %>% View()


data_main %>% count(St_type)

plot_St_type = data_main %>%
  plot_data("St_type", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, jitter = 0.2, STAT_COMPARISON = TRUE, xlab = "Stimulus type")

print(plot_St_type)
ggsave(file = paste0(dir_out, "stimulus_type.pdf"), plot_St_type, width = w, height = h+1)

#now plot only MT
plot_St_type_MT = data_main %>% filter(Brain.areas == "MT") %>%
  plot_data("St_type", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, jitter = 0.2, STAT_COMPARISON = TRUE, xlab = "Stimulus type")
print(plot_St_type_MT)
ggsave(file = paste0(dir_out, "stimulus_type_MT.pdf"), plot_St_type_MT, width = w, height = h+1)

lm(CP ~ St_type + Task_var, data = data_main) %>% summary()
lm(CP ~ St_type + Task_var+Recording+Brain.areas+Task_tailor+Task, data = data_main) %>% summary() #depth is higher than direction

##Plot PN ratio vs Task_var for MT
data_NP %>% filter(Brain.areas == "MT") %>%
  plot_data("Task_var", "PN.ratio", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, jitter = 0.2, STAT_COMPARISON = TRUE, xlab = "Task parameter", ylab = "Psychometric/Neurometric threshold ratio")

#Plot PN ratio vs St_type for MT
data_NP %>% filter(Brain.areas == "MT") %>%
  plot_data("St_type", "PN.ratio", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, jitter = 0.2, STAT_COMPARISON = TRUE, xlab = "Stimulus type", ylab = "Psychometric/Neurometric threshold ratio")


#####Stimulus eccentricity######


#plot CP vs Stimulus_ecc
plot_ecc = data_main %>%
  plot_data("Stimulus_ecc", "CP", REGRESSION_ALL = TRUE, REGRESSION_GROUP = TRUE, jitter = 0.2,
            xlab = "Stimulus eccentricity, deg", ADD_MEDIAN = FALSE, font_scale = 0.9)
print(plot_ecc)





#######Regressions for paper###################



data_NP = data_NP

vars_reg_NP_var = c("PN.ratio", "Brain.areas", "Task_parm", "Non_task_parm", "Stimulus_size", "St_duration", "Task", "Recording", "Task_var", "St_type" , "Predict_targets")
data_NP_var = data_NP %>% dplyr::select(all_of(vars_reg_NP_var))

##year
glm(CP ~ Year, data = data_main) %>% summary()
glm(CP ~ Year, data = data_main %>% filter(Recording == "single-electrode")) %>% summary()
glm(CP ~ Year, data = data_main %>% filter(Recording == "multi-electrode")) %>% summary()

#number of neurons
glm(CP ~ N_neurons, data = data_main %>% filter(Recording == "single-electrode")) %>% summary()

##Sensitivity


#check sqrt(PN.ratio) and log(PN.ratio)
model_all_NP4_sqrt =  lm(CP ~ PN.ratio_sqrt, data = data_NP %>% mutate(PN.ratio_sqrt = sqrt(PN.ratio)))
model_all_NP4_sqrt %>% summary()


model_all_NP4_log =  lm(CP ~ PN.ratio_log, data = data_NP %>% mutate(PN.ratio_log = log(PN.ratio)))
model_all_NP4_log %>% summary()

model_all_NP4_qrrt =  lm(CP ~ PN.ratio_qrrt, data = data_NP %>% mutate(PN.ratio_qrrt = (PN.ratio)^(1/4)))
model_all_NP4_qrrt %>% summary()

#now the same but without intercept
model_all_NP4_no_intercept =  lm(CP ~ 0 + PN.ratio, data = data_NP)
model_all_NP4_no_intercept %>% summary()


##Tailoring
model_all_NP_var =  lm(PN.ratio ~ Task_parm + Non_task_parm + Stimulus_size, data = data_NP_var)
model_all_NP_var %>% summary()
Anova(model_all_NP_var, type="II") #this is the best

#mean CP vs PN and tailoring
model_all_NP_tail =  lm(CP ~ PN.ratio + Task_parm + Non_task_parm + Stimulus_size, data = data_NP)
model_all_NP_tail %>% summary() #only Task_parm and PN.ratio
Anova(model_all_NP_tail, type="II") #this is the best, only Task_param and PN.ratio


#now all data mean CP vs : task,nontask, stimulus size


model_all_NP5 =  lm(CP ~ Task_parm, data = data_main)
model_all_NP5 %>% summary() #only Task_parm

###Brain area
model_brain =  lm(CP ~ Brain.areas, data = data_main)
model_brain %>% summary()

#contrling for sensitivity and other covariates
model_brain_PN =  lm(CP ~ Brain.areas + PN.ratio + Task +  St_duration, data = data_NP)
model_brain_PN %>% summary()


get_coefs = function(model, exclude_vars = "PN.ratio", str_remove = "Brain.areas", levels) {
     model %>% summary() %>% tidy() %>%
       mutate(term =  term %>% str_remove(str_remove) %>% factor(levels = levels)) %>%
       filter(!is.na(term)) %>%
       rename(coef = estimate, se = std.error) %>%
       #add significance *,**,***
       mutate(signif = case_when(p.value < 0.001 ~ "***",
                                    p.value < 0.01 ~ "**",
                                    p.value < 0.05 ~ "*",
                                    TRUE ~ ""))


}

#combine into one data frame
coef_brain_PN = get_coefs(model_brain_PN, levels =  levels(data_main$Brain.areas))

#retrieve the areas that have lower than 4 points and asign NA to coef and se
areas_exclude = data_NP %>% count(Brain.areas) %>% filter(n < 4) %>% pull(Brain.areas) %>% unique()
coef_brain_PN %<>% mutate(coef = ifelse(term %in% areas_exclude, NA, coef),
                            se = ifelse(term %in% areas_exclude, NA, se),
                          signif= ifelse(term %in% areas_exclude, NA, signif))

#now controlling for sensitivity using tailoring
model_brain_tailor =  lm(CP ~ Brain.areas + Task +  St_duration +  Task_parm + Non_task_parm + Stimulus_size, data = data_main)
model_brain_tailor %>% summary()
coef_brain_tailor = get_coefs(model_brain_tailor, levels =  levels(data_main$Brain.areas)) %>% filter(!is.na(vars))

coef_brain = rbind(coef_brain_PN %>% mutate(control = "inversed N/P ratio"), coef_brain_tailor %>% mutate(control = "tailoring"))


#now plot on x - axis brain areas, y - axis coefficient with errorbars, control  is for grouping and plot side by side
plot_brain = ggplot(coef_brain, aes(x = term, y = coef, color = term, group = control, shape = control))+
    guides(color = "none") +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = coef - se, ymax = coef + se), width = 0.2, position = position_dodge(width = 0.5)) +
  #scale shape solid and empty circles
    scale_shape_manual(values = c(16, 1), guide = guide_legend(title = "Control for sensitivity")) +
    #scale_linetype_manual(values = c("solid", "dashed"), guide = guide_legend(title = "Control for sensitivity")) +
    scale_color_manual(values = color_brain,
                         guide = guide_legend(title = ""))+
    geom_hline(yintercept = 0, linetype = "dashed") +
   #add significance stars just above error bars
    geom_text(aes(y = coef + se + 0.1*se, label = signif), position = position_dodge(width = 0.5), size = 5, color = "black") +
    theme_classic() +
    xlab("") +
    ylab("Mean CP difference \nto V1") +
    theme(axis.text = element_text(size = grid::unit(font_axis, "pt")),
           axis.text.x = element_text(angle = 30, hjust = 1),
            legend.text = element_text(size = grid::unit(font_legend, "pt")),
            legend.title = element_text(size = grid::unit(font_legend, "pt")),
            legend.position = "none",
     plot.margin = margin(10, 5, 10, 10))


print(plot_brain)
ggsave(file = paste0(dir_out, "brain_areas_sensitivity_controled.pdf"), plot_brain, width = 0.75*(2*w/3), height = 0.8*h)

ggsave(file = paste0(dir_out, "brain_areas_sensitivity_controled_legend.pdf"), plot_brain + theme(legend.position = "right"), width = 0.8*(2*w/3), height = 0.8*h)

#### Stimulus duration

lm(CP ~ log(St_duration), data = data_main %>% filter(!is.na(St_duration))) %>% summary()
lm(CP ~ sqrt(St_duration), data = data_main %>% filter(!is.na(St_duration))) %>% summary()
lm(CP ~ St_duration_sq, data = data_main %>% filter(!is.na(St_duration)) %>% mutate(St_duration_sq = St_duration^2)) %>% summary()
lm(CP ~ St_duration_cub, data = data_main %>% filter(!is.na(St_duration)) %>% mutate(St_duration_cub = St_duration^3)) %>% summary()


#control for sensitivity
model_all_NP4 =  lm(CP ~ PN.ratio + St_duration, data = data_NP %>% mutate(St_duration = St_duration/1000))
summary(model_all_NP4) #
Anova(model_all_NP4, type="II")
#check stimulus duration and PN.ratio
model_St_dur =  glm(St_duration ~ PN.ratio, data = data_NP)
summary(model_St_dur) #there is no correlation

#add Brain area as well
model_all_NP4 =  lm(CP ~ PN.ratio + St_duration + Brain.areas, data = data_NP %>% mutate(St_duration = St_duration/1000))
summary(model_all_NP4) #

#the same but stimulus is squared
model_all_st_area_sq =  lm(CP ~ PN.ratio + St_duration_sq + Brain.areas, data = data_NP %>% mutate(St_duration_sq = St_duration^2))
summary(model_all_st_area_sq) #St_duration_sq is significant

#the same but stimulus is sqrt
model_all_st_area_sqrt =  lm(CP ~ PN.ratio + St_duration_sqrt + Brain.areas, data = data_NP %>% mutate(St_duration_sqrt = sqrt(St_duration)))
summary(model_all_st_area_sqrt) #St_duration_sqrt is significant

#th2 same but stimulus is log
model_all_st_area_log =  lm(CP ~ PN.ratio + St_duration_log + Brain.areas, data = data_NP %>% mutate(St_duration_log = log(St_duration)))
summary(model_all_st_area_log) #St_duration_log is significant

##now all dataset with tailoring as control
model_all_st_tail =  lm(CP ~ St_duration + Brain.areas + Task_parm + Non_task_parm + Stimulus_size, data = data_main %>% mutate(St_duration = St_duration/1000))
summary(model_all_st_tail) #St_duration is significant


lm(CP ~ St_duration, data = data_main %>% filter(!is.na(St_duration), RT_task == "RT", Brain.areas == "MT" ) %>% mutate(St_duration = St_duration/1000) ) %>% summary()
lm(CP ~ PN.ratio + St_duration + Brain.areas, data = data_NP  %>% filter(!is.na(St_duration), RT_task == "RT" )  %>% mutate(St_duration = St_duration/1000)) %>% summary()
lm(CP ~ St_duration + Brain.areas + Task_parm + Non_task_parm + Stimulus_size, data = data_main %>% filter(!is.na(St_duration), RT_task == "RT" )  %>% mutate(St_duration = St_duration/1000)) %>% summary()

###now exclude 500, 1000, 1500, 2000 ms
lm(CP ~ St_duration, data = data_main %>% filter(!is.na(St_duration)) %>% mutate(St_duration = St_duration/1000) %>% filter(!St_duration %in% c(1.5, 2))) %>% summary()
lm(CP ~ St_duration, data = data_main %>% filter(!is.na(St_duration)) %>% mutate(St_duration = St_duration/1000) %>% filter(!St_duration %in% c(0.5, 1, 1.5, 2))) %>% summary()


lm(CP ~ PN.ratio + St_duration + Brain.areas, data = data_NP  %>% filter(!is.na(St_duration)) %>% mutate(St_duration = St_duration/1000) %>% filter(!St_duration %in% c(1.5, 2))) %>% summary() #N = 38
lm(CP ~ St_duration + Brain.areas + Task_parm + Non_task_parm + Stimulus_size , data = data_main %>% filter(!is.na(St_duration)) %>% mutate(St_duration = St_duration/1000) %>% filter(!St_duration %in% c(1.5, 2))) %>% summary()


####Task types
model_all_PN_task =  lm(CP ~ PN.ratio + Task, data = data_NP)
summary(model_all_PN_task)

#add brain areas
model_all_PN_task_brain =  lm(CP ~ PN.ratio + Task + Brain.areas, data = data_NP)
summary(model_all_PN_task_brain)



####Final regressions#######
#add stimulus duration, this is final regression for significant variables
model_all_PN_task_brain_st =  lm(CP ~ PN.ratio + Brain.areas + St_duration + Task, data = data_NP )
summary(model_all_PN_task_brain_st)
Anova(model_all_PN_task_brain_st, type="II") #PN.ratio, Task, Brain areas
#the sample size of the model
nrow(data_NP %>% select(all_of(c("CP", "PN.ratio", "Task", "Brain.areas", "St_duration"))) %>%
na.omit())


#without Task
Anova(lm(CP ~ PN.ratio + Brain.areas + St_duration, data = data_NP), type="II") #PN.ratio, Brain areas

#without Brain areas
Anova(lm(CP ~ PN.ratio + Task + St_duration, data = data_NP), type="II") #PN.ratio, Task

##only tailoring
model_all_task_only_tail =  lm(CP ~ Task + Task_parm + Non_task_parm + Stimulus_size, data = data_main)
summary(model_all_task_only_tail)


##now tailoring with full dataset, this is the final
model_all_task_tail =  lm(CP ~ Task_parm + Non_task_parm + Stimulus_size + Brain.areas + St_duration  + Task, data = data_main)
summary(model_all_task_tail) #
Anova(model_all_task_tail, type="II") #Task, Brain areas, Task_parm
#the sample size of the model
nrow(data_main %>% select(all_of(c("CP", "Task_parm", "Non_task_parm", "Stimulus_size", "Brain.areas", "St_duration", "Task"))) %>%
        na.omit())


#only task parm
# model_all_task_tail =  lm(CP ~ Task + St_duration + Brain.areas + Task_parm, data = data_main %>% mutate(St_duration = St_duration/1000))
# summary(model_all_task_tail) #

linearHypothesis(model_all_task_tail, "Taskfine-discrimination = Taskdetection") # p = 0.006

#plot coeffs
coef_taskPN = get_coefs(model_all_PN_task_brain_st, levels = levels(data_NP$Task),  str_remove = "Task")
coef_task_tailor = get_coefs(model_all_task_tail, levels = levels(data_NP$Task),  str_remove = "Task")
coef_task = rbind(coef_taskPN %>% mutate(control = "P/N threshold ratio"), coef_task_tailor %>% mutate(control = "tailoring"))

plot_coef_task = ggplot(coef_task, aes(x = term, y = coef, group = control, shape = control))+
    geom_point(size = 3, position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = coef - se, ymax = coef + se), width = 0.2, position = position_dodge(width = 0.3)) +
  #scale shape solid and empty circles
    scale_shape_manual(values = c(16, 1), guide = guide_legend(title = "Control for sensitivity")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
   #add significance stars just above error bars
    geom_text(aes(y = coef + se + 0.1*se, label = signif), position = position_dodge(width = 0.3), size = 5, color = "black") +
    theme_classic() +
    xlab("") +
    ylab("Mean CP difference \nto coarse-discrimin.")+
    theme(axis.text = element_text(size = grid::unit(font_axis, "pt")),
          axis.text.x = element_text(angle = 30, hjust = 0.7, vjust = 0.8),
            legend.text = element_text(size = grid::unit(font_legend, "pt")),
            legend.title = element_text(size = grid::unit(font_legend, "pt")),
            legend.position = "none")


print(plot_coef_task)
ggsave(file = paste0(dir_out, "task_types_sensitivity_controled.pdf"), plot_coef_task, width = 0.75*(2*w/3), height = 0.85*h)


##now within only coarse discrimination task
CP_PN_coarse = lm(CP ~ PN.ratio, data = data_NP %>% filter(Task == "coarse-discrimination"))
summary(CP_PN_coarse)
CP_PN_all_coarse = lm(CP ~ PN.ratio + Brain.areas + St_duration, data = data_NP %>% filter(Task == "coarse-discrimination"))
summary(CP_PN_all_coarse)

#now fine discrimination
CP_PN_fine = lm(CP ~ PN.ratio, data = data_NP %>% filter(Task == "fine-discrimination"))
summary(CP_PN_fine)
CP_PN_all_fine = lm(CP ~ PN.ratio + Brain.areas + St_duration, data = data_NP %>% filter(Task == "fine-discrimination"))
summary(CP_PN_all_fine)

#test difference between slopes and intercepts  CP_PN_coarse vs CP_PN_fine
data_coarse_fine = data_NP %>% filter(Task %in% c("coarse-discrimination", "fine-discrimination")) %>%
  mutate(Task = factor(Task, levels = c("coarse-discrimination", "fine-discrimination")))

lm(CP ~ PN.ratio * Task, data = data_coarse_fine) %>% summary()

#now P/N ratio vs Task and tailoring
lm(PN.ratio ~ Task, data = data_NP) %>% summary()
model_all_PN_task_tail =  lm(PN.ratio ~ Task + Task_parm + Non_task_parm + Stimulus_size, data = data_NP)
summary(model_all_PN_task_tail)

#add brain areas


#check MT vs MST
model_all_PN_task_brain =  lm(PN.ratio ~ Task + Brain.areas, data = data_NP)
summary(model_all_PN_task_brain)



# now with Pn , all variables and tailoring
model_all_PN_task_brain_st_tail =  lm(CP ~ PN.ratio + Task + Brain.areas + St_duration + Task_parm + Non_task_parm + Stimulus_size , data = data_NP %>% mutate(St_duration = St_duration/1000))
summary(model_all_PN_task_brain_st_tail) # it seem that tailoring is correlated with all them


#only for MT
model_PN_MT =  lm(CP ~ PN.ratio, data = data_NP %>% filter(Brain.areas == "MT") %>% mutate(St_duration = St_duration/1000))
summary(model_PN_MT)




#only V1
model_PN_V1 =  lm(CP ~ PN.ratio, data = data_NP %>% filter(Brain.areas == "V1") %>% mutate(St_duration = St_duration/1000))
summary(model_PN_V1)

model_all_PN_task_st_V1 =  lm(CP ~ PN.ratio + Task + St_duration, data = data_NP %>% filter(Brain.areas == "V1") %>% mutate(St_duration = St_duration/1000))
summary(model_all_PN_task_st_V1)

#only MST
model_all_PN_task_st_MST =  lm(CP ~ PN.ratio + Task + St_duration, data = data_NP %>% filter(Brain.areas == "MST/MSTd") %>% mutate(St_duration = St_duration/1000))
summary(model_all_PN_task_st_MST)

#V2
model_all_PN_task_st_V2 =  lm(CP ~ PN.ratio + Task + St_duration, data = data_NP %>% filter(Brain.areas == "V2") %>% mutate(St_duration = St_duration/1000))
summary(model_all_PN_task_st_V2)


##now do glm with all variables and beta distribution of residuals
#PN
model_PN_beta <- betareg(CP ~ PN.ratio + Task + Brain.areas + St_duration,
                      data = data_NP %>% mutate(St_duration = St_duration / 1000))

model_PN_beta %>% summary()

#Tailoring
model_tail_beta <- betareg(CP ~ Task + Brain.areas + St_duration + Task_parm + Non_task_parm + Stimulus_size,
                        data = data_main %>% mutate(St_duration = St_duration / 1000))
model_tail_beta %>% summary()


#Predict targets
data_main %<>%
  mutate(Predict_targets = factor(Predict_targets, levels = c("fixed","varied between sessions",
                                                              "varied within session", "unpredictable", "not saccades")))
lm(CP ~  Predict_targets, data = data_main) %>% summary()


###method of CP calculation
lm(CP ~  method, data = data_main) %>% summary()
lm(CP ~  method + PN.ratio + Task + Brain.areas + St_duration, data = data_NP) %>% summary() #not significant
lm(CP ~ method + Task + Brain.areas + St_duration + Task_parm + Non_task_parm + Stimulus_size, data = data_main) %>% summary() #strange results - should be some confound

#####Method of preference calculation
lm(CP ~  method_pref, data = data_main) %>% summary()
lm(CP ~  method_pref, data = data_main %>% filter(Brain.areas == "MT")) %>% summary()
lm(CP ~  method_pref + Recording, data = data_main) %>% summary()
lm(CP ~  method_pref + PN.ratio + Task + Brain.areas + St_duration, data = data_NP) %>% summary()
lm(CP ~ method_pref + Task + Brain.areas + St_duration + Task_parm + Non_task_parm + Stimulus_size, data = data_main) %>% summary()



lm(CP ~ Recording + Task_parm + Non_task_parm + Stimulus_size, data = data_main) %>% summary()
lm(CP ~ Recording + Task + Brain.areas + St_duration + Task_parm + Non_task_parm + Stimulus_size, data = data_main) %>% summary()




# PN.ratio
lm(CP ~  Year + PN.ratio, data = data_NP) %>% summary()
lm(CP ~  Year + PN.ratio + Task + Brain.areas + St_duration, data = data_NP) %>% summary()

# tailoring
lm(CP ~  Year + Task_parm + Non_task_parm + Stimulus_size, data = data_main) %>% summary()
lm(CP ~  Year +  Brain.areas  + Task_parm + Non_task_parm + Stimulus_size, data = data_main) %>% summary()
lm(CP ~  Year + Task + Brain.areas +  Task_parm + Non_task_parm + Stimulus_size, data = data_main) %>% summary()

# Fit the full model
all_tailor_year = lm(CP ~ Year + Task + Brain.areas + St_duration + Task_parm + Non_task_parm + Stimulus_size, data = data_main)

# Get year coefficient and its standard error
year_coef = summary(all_tailor_year)$coefficients["Year", "Estimate"]
year_se = summary(all_tailor_year)$coefficients["Year", "Std. Error"]

# Compute mean values
mean_year = mean(data_main$Year, na.rm = TRUE)
mean_cp = mean(data_main$CP, na.rm = TRUE)

# Compute intercept so line passes through (mean_x, mean_y)
intercept_year = mean_cp - year_coef * mean_year

# Degrees of freedom for t-distribution
df = df.residual(all_tailor_year)
t_crit = qt(0.975, df = df)

# Create a grid of x values for Year
x_vals = seq(min(data_main$Year, na.rm = TRUE), max(data_main$Year, na.rm = TRUE), length.out = 200)

# Predicted CP values based on adjusted line
y_vals = mean_cp + year_coef * (x_vals - mean_year)

# Standard error of line = SE(beta_x) * |x - mean_x|
ci_width = t_crit * year_se * abs(x_vals - mean_year)

# Dataframe for plotting
df_line = data.frame(
  Year = x_vals,
  CP = y_vals,
  CP_lower = y_vals - ci_width,
  CP_upper = y_vals + ci_width,
  Brain.areas = "MT"
)

# Your plot with added regression line and CI band
plot_year_reg = data_main %>%
  plot_data("Year", "CP", ADD_MEDIAN = FALSE, xlab = "Year of publication", left_margin = 10,
            REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, title = "Controlled regression") +
  geom_ribbon(data = df_line, aes(x = Year, ymin = CP_lower, ymax = CP_upper), fill = "#999999", alpha = 0.4, color = NA) +
  geom_line(data = df_line, aes(x = Year, y = CP), color = "black", size = 1.5)+
  theme(plot.title = element_text(hjust = 0.5,  face = "bold")) +
  #add p-value text
    annotate("text", x = min(data_main$Year, na.rm = TRUE) + 3, y = max(data_main$CP, na.rm = TRUE) - 0.05,
             label = paste0("p = ", signif(summary(all_tailor_year)$coefficients["Year", "Pr(>|t|)"], 2)),
             size = 4)


print(plot_year_reg )
ggsave(file = paste0(dir_out, "year_controlled.pdf"), plot_year_reg, width = 0.8*(2*w/3), height = 0.9*h)





#only PN.ratio
lm(CP ~  N_neurons + PN.ratio, data = data_NP %>% filter(Recording == "single-electrode")) %>% summary()
lm(CP ~  N_neurons + PN.ratio + Task + Brain.areas + St_duration, data = data_NP %>% filter(Recording == "single-electrode")) %>% summary()
#only tailoring
lm(CP ~  N_neurons + Task_parm + Non_task_parm + Stimulus_size, data = data_main %>% filter(Recording == "single-electrode")) %>% summary()
lm(CP ~  N_neurons +  Task + Task_parm + Non_task_parm + Stimulus_size,
   data = data_main %>% filter(Recording == "single-electrode")) %>% summary()
lm(CP ~ N_neurons + Task + Brain.areas + St_duration + Task_parm + Non_task_parm + Stimulus_size,
   data = data_main %>% filter(Recording == "single-electrode")) %>% summary()

###Task_var
lm(CP ~  Task_var, data = data_main) %>% summary() #R = 0.30
#only MT
lm(CP ~  Task_var, data = data_main %>% filter(Brain.areas == "MT")) %>% summary()
#exclude Task parameter - direction of rotation
lm(CP ~  Task_var, data = data_main %>% filter(Task.parameter!= "direction of rotation" & Brain.areas == "MT")) %>% summary() #R = 0.30


lm(CP ~  Task_var + PN.ratio + Task + Brain.areas + St_duration, data = data_NP) %>% summary() #R^2 = 0.48
#within MT
lm(CP ~ Task_var + PN.ratio + Task  + St_duration,
   data = data_NP %>% filter(Brain.areas == "MT")) %>% summary() #R^2 = 0.48



#exclude Task parameter - direction of rotation
lm(CP ~  Task_var + PN.ratio + Task  + St_duration,
   data = data_NP %>% filter(Task.parameter!= "direction of rotation" & Brain.areas == "MT")) %>% summary() #R^2 = 0.45


lm(CP ~ Task_var + Task + Brain.areas + St_duration + Task_parm + Non_task_parm + Stimulus_size, data = data_main) %>% summary() #R^2 = 0.45
#within MT
lm(CP ~ Task_var + Task + St_duration + Task_parm + Non_task_parm + Stimulus_size, data = data_main %>% filter(Brain.areas == "MT")) %>% summary() #R^2 = 0.45

#exclude Task parameter - direction of rotation
lm(CP ~ Task_var + Task + St_duration + Task_parm + Non_task_parm + Stimulus_size,
   data = data_main %>% filter(Task.parameter!= "direction of rotation" & Brain.areas == "MT")) %>% summary() #R^2 = 0.45



########Outliers##############
sds = 2

reg_main_NP = lm(CP ~  PN.ratio + Task + Brain.areas + St_duration, data = data_NP %>% filter(!is.na(St_duration))) %>% summary()
reg_main_tail = lm(CP ~ Task + Brain.areas + St_duration + Task_parm + Non_task_parm + Stimulus_size,
                   data = data_main %>% filter(!is.na(St_duration) & !is.na(Stimulus_size)) ) %>% summary()
data_NP_res = data_NP %>% filter(!is.na(St_duration)) %>% mutate(residuals = reg_main_NP$residuals, res_sd = residuals/ sd(residuals)) %>%
  select(CP, Number.of.neurons..number.of.sessions.,Papers, Monkey, PN.ratio, Task, Brain.areas, St_duration, residuals, res_sd)
#show outliers - more than 3sigma of residuals
data_NP_res %>% filter(abs(residuals) > sds*sd(residuals)) %>% View()


data_NP_res %>% filter(Papers %in% c("Uka et al. (2012)", "Britten et al. (1996)", "Kumano & Uka (2014) ", "Goris et al. (2017)", "Doudlah et al.  (2022)")) %>% View()

data_main %>% filter(is.na(St_duration) | is.na(Stimulus_size)) %>%
  select(CP, Papers, Monkey, Task, Brain.areas, St_duration, Task_parm,  Non_task_parm, Stimulus_size) %>% View()

data_main %>% filter(!is.na(St_duration)& !is.na(Stimulus_size)) %>%
  mutate(residuals = reg_main_tail$residuals, res_sd = residuals/ sd(residuals)) %>%
  filter(abs(residuals) > sds*sd(residuals)) %>%
  select(CP, Papers, Monkey, Task, Brain.areas, St_duration, Task_parm, Non_task_parm, Stimulus_size, residuals, res_sd) %>%
  View()

data_main %>% filter(!is.na(St_duration)& !is.na(Stimulus_size)) %>%
  mutate(residuals = reg_main_tail$residuals, res_sd = residuals/ sd(residuals)) %>%
  filter(Task == "bistable") %>%
  select(CP, Papers, Monkey, Task, Brain.areas, St_duration, Task_parm, Non_task_parm, Stimulus_size, residuals, res_sd) %>%
  View()

data_NP_res %>%  filter(Task == "bistable") %>% View()



data_main %>% filter(!is.na(St_duration)& !is.na(Stimulus_size)) %>%
   mutate(residuals = reg_main_tail$residuals, res_sd = residuals/ sd(residuals)) %>%
   filter(Papers %in% c("Goris et al. (2017)", "Grunewald et al. (2002)", "Doudlah et al.  (2022)")) %>%
   select(CP, Papers, Monkey, Number.of.neurons..number.of.sessions.,Task, Brain.areas, St_duration, Task_parm, Non_task_parm, Stimulus_size, residuals, res_sd) %>% View()

#regression without st size
reg_main_tail_no_st = lm(CP ~ Task + Brain.areas + St_duration + Task_parm + Non_task_parm,
                          data = data_main %>% filter(!is.na(St_duration)) ) %>% summary()


data_main %>% filter(!is.na(St_duration)) %>%
    mutate(residuals = reg_main_tail_no_st$residuals) %>%
    filter(abs(residuals) > sds*sd(residuals)) %>%
    select(CP, Papers, Monkey, Task, Brain.areas, St_duration, Task_parm, Non_task_parm, residuals) %>% View()


#now without stimulus duration
reg_main_NP_without_st_dur = lm(CP ~  PN.ratio + Task + Brain.areas, data = data_NP %>% filter(!is.na(Stimulus_size))) %>% summary()
data_NP %>% filter(!is.na(Stimulus_size)) %>%
  mutate(residuals = reg_main_NP_without_st_dur$residuals, res_sd = residuals/ sd(residuals)) %>%
  filter(abs(residuals) > sds*sd(residuals)) %>%
  select(CP, Papers, Monkey, PN.ratio, Task, Brain.areas, Stimulus_size, residuals, res_sd) %>% View()

reg_main_tail_no_st_dur = lm(CP ~ Task + Brain.areas + Task_parm + Non_task_parm + Stimulus_size,
                          data = data_main %>% filter(!is.na(Stimulus_size)) ) %>% summary()
data_main %>% filter(!is.na(Stimulus_size)) %>%
    mutate(residuals = reg_main_tail_no_st_dur$residuals, res_sd = residuals/ sd(residuals)) %>%
    filter(abs(residuals) > sds*sd(residuals)) %>%
    select(CP, Papers, Monkey, Task, Brain.areas, Task_parm, Non_task_parm, Stimulus_size, residuals, res_sd) %>% View()








#####Number of papers that mention choice probability in title, key words or abstract
library("rentrez")

# Search for "choice probability" in PubMed
search_term <- '"choice probability"[All Fields]'
results <- entrez_search(db = "pubmed", term = search_term, retmax = 1000)

# Fetch summaries (metadata)
summaries <- entrez_summary(db = "pubmed", id = results$ids)

records <- ldply(summaries, function(x) {
  data.frame(
    title = x$title,
    authors = paste(x$authors[,1],  collapse = ", "),
    year = as.numeric(substr(x$pubdate, 1, 4)),
    stringsAsFactors = FALSE
  )
}) %>% arrange(year)


years <- sapply(summaries, function(x) x$pubdate)
years <- as.numeric(substring(years, 1, 4))

# Count publications per year
df <- data.frame(Year = years) %>%
  count(Year)


####table of papers####

table_papers = data_main %>%
  mutate(COMBINED = grepl("combined|and", Monkey),
         Monkey_combined = ifelse(COMBINED, case_when(
                                                  grepl("two|and|And|Two", Monkey) ~ 2,
                                                  grepl("three|Three", Monkey) ~ 3)
                            ,NA),
         Task = case_when(Task == "coarse-discrimination" ~ "coarse",
                          Task == "fine-discrimination" ~ "fine",
                          TRUE ~ Task),
         NP.ratio = ifelse(N.P.ratio=="", "-", '+')
  ) %>% #select(Papers, Monkey, Monkey_combined, COMBINED) %>% View()
  group_by(Papers) %>%
  summarise(Brain.areas = paste(Brain.areas %>% unique() %>%
                                  factor(levels = data$Brain.areas %>% levels()) %>% sort(), collapse = "; "),
            N = n(),
            N_combined_rows = sum(COMBINED, na.rm = TRUE),
            N_monkeys = n_distinct(Monkey),
            N_combined_m = Monkey_combined %>% na.omit() %>% max() %>% pmax(0),
            N_tasks = n_distinct(Epoch),
            Task = paste(Task %>% unique() %>%
                           factor(levels = c("coarse", "fine", "detection", "bistable")) %>% sort(), collapse = "; "),
            St_duration = paste(St_duration %>% unique() %>% sort(), collapse = "; "),
            NP.ratio = paste(NP.ratio %>% unique() %>% factor(levels = c("+", "-")) %>% sort(), collapse = "; "),
            Year = Year[1]
    ) %>%
  arrange(Year) %>%
  mutate(N_monkeys_final = case_when(N_combined_m == 0 ~  N_monkeys,
                                     (N - N_combined_rows) == 0 ~ N_combined_m,
                                     TRUE ~ pmax(N_monkeys - 1, N_combined_m)) %>% as.integer())

#change paper Chang et al. (2021) to "Chang et al. (2020); Doudlah et al. (2022)"
table_papers %<>% mutate(Papers = ifelse(Papers == "Chang et al. (2021)", "Chang et al. (2020); Doudlah et al. (2022)", Papers),
                         Papers = ifelse(Papers == "Uka & DeAngelis (2006)", "Uka & DeAngelis (2006); Gazzaniga (2009) Chapter 33", Papers))

table_papers %<>% mutate(Brain.areas = ifelse(Brain.areas == "V3/V3A; CIP", "V3A; CIP",  Brain.areas))


table_papers %>% select(Papers, N, N_combined_rows, N_monkeys_final, N_tasks, NP.ratio, Brain.areas, St_duration, Task) %>%
  rename("Paper" = Papers,
         "points" = N,
         "combined" = N_combined_rows,
          "monkeys" = N_monkeys_final,
         "conditions" = N_tasks,
         "NP" = NP.ratio,
         "area" = Brain.areas,
         "duration" = St_duration,
         "Task" = Task) %>%
  mutate(Paper = gsub("&", "and", Paper),
         Paper = gsub("  ", " ", Paper)) %>%
  readr::write_csv(paste0(dir_out, "table_of_papers.csv"))


######Conceptual figure###########


library(ggforce)


data_cons = data.frame(
    labels = c("single\nelectrode", "linear\nprobe", "2D array", "Neuropixels", "our meta-study", "future meta-studies\n(based on raw data)"),
    task_diversity = c(rep(3.9, 4), 16, 16), 
    data_richness = c(2, 6, 10, 18, 2, 18),
    width = c(3, 3, 3, 3, 7, 7),     
    height = c(2.5, 3, 3.5, 5, 1.5, 6),        
    alpha = c(rep(0.3, 5), 0.05),
    # 1. Add a column for text color. The first 5 are black, the last is light gray.
    text_color = c(rep("black", 5), "darkgray") 
)

plot_conceptual = ggplot(data_cons) +
    geom_ellipse(aes(x0 = task_diversity, y0 = data_richness,
                     a = width, b = height, angle = 0, fill = labels, alpha = alpha),
                 color = NA) +
    scale_alpha_identity() +
    
    # 2. Add 'color = text_color' inside the aes() wrapper
    geom_text(aes(x = task_diversity, y = data_richness, label = labels, color = text_color),
              vjust = 0.5, size = font_axis/2.845, lineheight = 0.7) +
    
    # 3. Add scale_color_identity() to force ggplot to use your exact color names
    scale_color_identity() +
    
    xlab("Number of tasks. Number of subjects.") +
    ylab("Amount/resolution of data") +
    theme_classic() +
    theme(
        text = element_text(size = grid::unit(font_axis, "pt")),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none" 
    ) +
    xlim(c(0, 24)) + 
    ylim(c(-1, 24))

print(plot_conceptual)
ggsave(file = paste0(dir_out, "conceptual_b.pdf"), plot_conceptual, width = 0.8*(2*w/3), height = h)


###############Plots for paper############################
library(patchwork)
library(cowplot)

# Define the layout
# "|" places plots side-by-side
# "/" places the next plot underneath
# legend_only <- cowplot::get_legend(plot_year_legend_bottom)

#####year
all_guides <- get_plot_component(plot_year_legend_bottom, "guide-box", return_all = TRUE)
legend_only <- all_guides[[3]]

plot_year     <- plot_year + labs(tag = "a)")
plot_year_reg <- plot_year_reg + labs(tag = "b)")
plot_NP_year  <- plot_NP_year + labs(tag = "c)")

year_all <- (plot_year / plot_year_reg) | (plot_spacer()/plot_NP_year/legend_only + plot_layout(ncol = 1, heights = c(0.5, 1, 0.5)))
# Add annotation and collect legends
year_all <- year_all +
  plot_layout(guides = 'collect') &
  theme(
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
    plot.tag = element_text(face = "bold", size = grid::unit(font_axis+2, "pt"))
  )

# Save the final combined plot to PDF
ggsave(paste0(dir_out, "year_all.pdf"), year_all, width = 1.06*w, height = 1.95*h)

########CP - sensitivity

# Create an empty plot with centered, bold text
scenarios_text = ggplot() +
  annotate("text", x = 0.5, y = 0.1, label = "Scenarios:", fontface = "bold", size = 3) +
  theme_void()

vs_text = ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "VS", fontface = "bold", size = font_labels/2.845) +
  theme_void()+
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))



#add the slope (rounded 2 digits) and intercept (rounded 3 digits) to as labels in topleft + * if p-value is less than 0.05, ** if p-value is less than 0.01, *** if p-value is less than 0.001
model_all_NP4 =  lm(CP ~ PN.ratio, data = data_NP)
p_value_NP4 = summary(model_all_NP4)$coefficients["PN.ratio", "Pr(>|t|)"]
p_value_label_NP4 = ifelse(p_value_NP4 < 0.001, "***", ifelse(p_value_NP4 < 0.01, "**", ifelse(p_value_NP4 < 0.05, "*", "")))
p_value_intercept_NP4 = summary(model_all_NP4)$coefficients["(Intercept)", "Pr(>|t|)"]
p_value_label_intercept_NP4 = ifelse(p_value_intercept_NP4 < 0.001, "***", ifelse(p_value_intercept_NP4 < 0.01, "**", ifelse(p_value_intercept_NP4 < 0.05, "*", "")))
coef_NP4 = summary(model_all_NP4)$coefficients["PN.ratio", "Estimate"]
intercept_NP4 = summary(model_all_NP4)$coefficients["(Intercept)", "Estimate"]

plot_PN = plot_PN +
  annotate("text", x = 0.05, y = 0.655,
           label = paste0("slope = ", round(coef_NP4, 2),p_value_label_NP4, "\nintercept = ", round(intercept_NP4, 3),p_value_label_intercept_NP4), hjust = 0, vjust = 0,
           size = font_labels/2.845)+
  labs(title = "Across-study slope", tag = "c)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = grid::unit(font_labels, "pt")),
        axis.title.x = element_blank())

#now create label = median (1Q-3Q) for data_NP_slope$PN_slope


median_within = data_NP_slope$PN_slope %>% median(na.rm = TRUE) %>% round(2)
min_within = data_NP_slope$PN_slope %>% quantile(0.25, na.rm = TRUE) %>% round(2)
max_within = data_NP_slope$PN_slope %>% quantile(0.75, na.rm = TRUE) %>% round(2)



plot_PN_within = plot_PN_within +
    annotate("text", x = 0.05, y = 0.66,
             label = paste0("slopes = ", median_within, " (", min_within, "-", max_within, ")"), hjust = 0, vjust = 0,
             size = font_labels/2.845) +
  labs(title = "Within-study slopes", tag = "b)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = grid::unit(font_labels, "pt")),
        axis.title.x = element_blank())

plot_sc1  = plot_sc1 +
  labs(title = "Aligned slopes", tag = "a)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = grid::unit(font_labels, "pt")))
plot_sc2  = plot_sc2 + labs(title = "Uncorrelated Means") + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = grid::unit(font_labels, "pt")))


shared_x_title <- wrap_elements(panel = textGrob("Normalized sensitivity [inverse mean N/P ratio]", gp = gpar(fontsize = font_labels)))

legend_2col = get_plot_component(plot_year_legend_bottom_2cols, "guide-box", return_all = TRUE)[[1]]

CP_sens_all = (
    # Top Row: Stacking a spacer on top of the legend pushes it to the bottom
    (plot_spacer()|plot_sc1 | vs_text | plot_sc2 | (plot_spacer() / plot_spacer()/ plot_spacer() /legend_2col)) +
    plot_layout(nrow = 1, widths = c(0.02,0.5, 0.08, 0.5, 0.6))
  ) /
  # Inserted vertical spacer
  plot_spacer() /
  # Bottom Row
  (plot_PN_within| plot_PN) /
  # Shared x-axis title
  shared_x_title +
  # Main layout
  plot_layout(guides = 'collect', heights = c(0.7, 0.1, 1, 0.1)) &
  theme(
    plot.margin = margin(t = 5, r = 10, b = 0, l = 5, unit = "pt"),
    plot.tag = element_text(face = "bold", size = grid::unit(font_axis+2, "pt"))
  )


print(CP_sens_all)

ggsave(paste0(dir_out, "sensitivity.pdf"), CP_sens_all, width = 1.06*w, height = 1.95*h)

###CP duration

CP_dur_all = (
  (plot_sim_dur + labs(tag = "a)")|plot_spacer()) 
  /
    ((plot_St_duration + labs(tag = "b)")|legend_2col) +plot_layout(nrow = 1, widths = c(0.7, 0.3)))
  ) +
    plot_layout(nrow = 2, heights =  c(0.4, 0.6))

print(CP_dur_all)
ggsave(paste0(dir_out, "stimulus_duration_final.pdf"), CP_dur_all, width = 1.06*w, height = 1.95*h)


#####supplementary figures
legend_2rows = get_plot_component(plot_year_legend_bottom_2rows, "guide-box", return_all = TRUE)[[1]]


recordings_all = plot_recordings + labs(tag = "a)") |plot_year_rec + labs(tag = "b)")
ggsave(paste0(dir_out, "recordings_all.pdf"), recordings_all, width = 1.06*w, height = h)




plot_sup= function(plot, file_name, width = 1.06*w, height = 0.9*h, layout = c(0.7, 0.3,0.2)) {
  plot_new = (plot|legend_2col|plot_spacer()) + plot_layout(nrow = 1, widths = layout)
  ggsave(file = paste0(dir_out, file_name), plot_new, width = width, height = height)
}

plot_sup(plot_CP_sens, "CP_sensitivity_within_study.pdf")
#plot_sup(plot_St_duration_RT, "stimulus_duration_RT.pdf")
plot_sup(plot_NP_task, "PN_ratio_task.pdf", width = 0.9*w, height = 1.07*h, layout = c(0.6, 0.3,0.1))
plot_sup(plot_learning, "learning.pdf")
plot_sup(plot_Lapse, "lapse_rate.pdf")
plot_sup(plot_task_var, "task_parameter.pdf",  height = 1.07*h)
plot_sup(plot_ecc, "stimulus_eccentricity.pdf")
plot_sup(plot_NP_N, "PN_ratio_N_neurons.pdf")

#############END######################


# ######agnostic approach##################
# vars_search = c("Task_parm", "Non_task_parm", "Stimulus_size", "Brain.areas",  "St_duration", "Task",
#                 "Recording", "Year",
#                 "Predict_targets", "method_pref", "method",
#                 "Task_var",
#                 "CP")
#
# data_search = data_main %>% dplyr::select(all_of(vars_search)) %>% na.omit() %>% mutate(Year = Year - 1996)
#
# full_model <- lm(CP ~ ., data = data_search )
# null_model <- lm(CP ~ 1, data = data_search)  # Only intercept
# step_forward <- step(null_model, scope = formula(full_model), direction = "forward", trace = 0)
# summary(step_forward)
# step_backward <- step(full_model, direction = "backward", trace = 0)
# summary(step_backward)
# step_both <- step(full_model, direction = "both", trace = 0)
# summary(step_both)
# AIC(step_forward, step_backward, step_both) #all models are the same: Brain.areas, Non-task (population <), Stimulus_size (population >), St_duration, Task, Predict_targetsfixed  (>, but insignificant)
#
#
# ####PCA########
# vars_PCA = c("Brain.areas", "Task_parm", "Non_task_parm", "Stimulus_size", "St_duration", "Task", "Recording", "Task_var",  "CP")
# data_PCA = data_main %>% select(all_of(vars_PCA))
#
# ###FAMD - Factor Analysis for Mixed Data - categorical(not ordinal) and numerical
# result_FAMD = FAMD(data_PCA %>% na.omit(), ncp = 3, graph = TRUE, sup.var = ncol(data_PCA):ncol(data_PCA))
# result_FAMD$var$contrib
#
# data_FAMD = result_FAMD$ind$coord %>% cbind(data_PCA %>% na.omit())
#
# glm (data = data_FAMD, formula = CP ~ Dim.1 + Dim.2 + Dim.3) %>% summary()
#
#
# ###princals - Principal Component Analysis for Mixed Data - numerical and categorical
# result_principals = princals(data = data_PCA %>% na.omit() %>% select(!CP) %>% as.data.frame(), ndim = 3,  levels = c("nominal",  "ordinal", "ordinal",  "ordinal", "metric", "nominal", "nominal", "nominal"))
# plot(result_principals, "biplot", main = "Biplot of Principal Components")
# plot(result_principals, "jointplot", main = "Biplot of Principal Components")
#
# summary(result_principals)
# result_principals$quantifications
# result_principals$loadings
#
# data_principal = result_principals$objectscores %>% cbind(data_PCA %>% na.omit())
# glm (data = data_principal, formula = CP ~ D1 + D2 + D3) %>% summary()
#
# ##homals - Multiple Correspondence Analysis  - tryin to find the relationship between categorical variables
# result_homals = homals(data = data_PCA %>% na.omit() %>% select(!CP) %>% as.data.frame(), ndim = 3,  levels = c("nominal",  "ordinal", "ordinal",  "ordinal", "metric", "nominal", "nominal", "nominal"))
# summary(result_homals)
# result_homals$loadings
# result_homals$quantifications
# plot(result_homals, plot.type = "transplot", main = "Category Quantifications")
# plot(result_homals, plot.type = "biplot", main = "Object Scores (Homals Biplot)")
# data_homals = result_homals$objectscores %>% cbind(data_PCA %>% na.omit())
# glm (data = data_homals, formula = CP ~ D1 + D2 + D3) %>% summary()
#
#
# ####Regressions for all variables#########
# hist(data_main$CP)
#
#
#
#
# ##########with PN.ratio###############
#
#
# model_all_NP =  glm(CP ~ ., data = data_NP)
# model_all_NP %>% summary()
# Anova(model_all_NP, type="II") #this is the best
#
#
# ##just PN.ratio
# data_NP$PN.ratio %>% hist()
#
# plot(model_all_NP4)
#
# # test heteroscedacity
# library(lmtest)
# library(sandwich)
# bptest(model_all_NP4) #there is heteroscedacity
# coeftest(model_all_NP4, vcov = vcovHC(model_all_NP4, type = "HC3")) #robust standard errors, become even more significant
#
# #spearman correlation
# cor.test(data_NP$CP, data_NP$PN.ratio, method = "spearman") #rank correlation
# #pearson correlation
# cor.test(data_NP$CP, data_NP$PN.ratio, method = "pearson")
#
# 1- model_all_NP4$deviance/ model_all_NP4$null.deviance
# 1- var(model_all_NP4$residuals)/var(data_NP$CP) #R^2
#
#
# #only for PN>0.5
# model_all_NP4_part_higher =  glm(CP ~ PN.ratio, data = data_NP %>% filter(PN.ratio > 0.25))
# model_all_NP4_part_higher %>% summary()
#
# #only for PN<0.5
# model_all_NP4_part_lower =  glm(CP ~ PN.ratio, data = data_NP %>% filter(PN.ratio < 0.25))
# model_all_NP4_part_lower %>% summary()
#
# #just binary variable
# model_all_NP_binary =  glm(CP ~ as.factor(PN.ratio > 0.25), data = data_NP)
# model_all_NP_binary %>% summary()
#
# AIC(model_all_NP4,model_all_NP_binary)
#
#
# #segmented regression
# library(segmented)
# model_all_NP4 =  glm(CP ~ PN.ratio, data = data_NP)
#
# seg_model <- segmented(model_all_NP4,  psi = 0.25)  # Initial guess: breakpoint at x=5
# summary(seg_model)
#
#
#
# #sqrt
# model_all_NP4 =  glm(CP ~ sqrt(PN.ratio), data = data_NP)
# model_all_NP4 %>% summary()
# plot(model_all_NP4)
#
#
# #log
# model_all_NP4 =  glm(CP ~ log(PN.ratio), data = data_NP)
# model_all_NP4 %>% summary()
# plot(model_all_NP4)
#
# #1/, i.e. NP.ratio
# model_all_NP4 =  glm(CP ~ NP.ratio, data = data_NP %>% mutate(NP.ratio = 1/PN.ratio))
# model_all_NP4 %>% summary()
# plot(model_all_NP4)
#
# #now use regression taking into account CP is a proportion
#
# library(betareg)
# beta_model <- betareg(CP ~ PN.ratio, data = data_NP)
# summary(beta_model)
# plot(beta_model)
# bptest(beta_model) #there is heteroscedacity
#
#
# #compare beta model with linear model
# AIC(model_all_NP4,beta_model)
#
#
# ##+Tailoring
# model_all_NP4 =  glm(CP ~ PN.ratio + Task_parm + Non_task_parm + Stimulus_size, data = data_NP)
# summary(model_all_NP4) #Task_parm_single neuron decreases CP if you controlled for P/N ratio
# Anova(model_all_NP4, type="II") #this is the best
#
# model_all_NP4 =  glm(CP ~ PN.ratio + Task_parm, data = data_NP)
# summary(model_all_NP4) #Task_parm_single neuron decreases CP if you controlled for P/N ratio
# Anova(model_all_NP4, type="II") #this is the best
#
# ##+Brain.areas
# model_all_NP4 =  glm(CP ~ PN.ratio + Brain.areas, data = data_NP)
# summary(model_all_NP4) #MT > V1, CIP>V1
# Anova(model_all_NP4, type="II")
#
# #+Task
# model_all_NP4 =  glm(CP ~ PN.ratio + Task, data = data_NP)
# summary(model_all_NP4) #bistable >
# Anova(model_all_NP4, type="II")
#
# #Stimulus duration
# model_all_NP4 =  glm(CP ~ PN.ratio + St_duration, data = data_NP)
# summary(model_all_NP4) #
# Anova(model_all_NP4, type="II")
# #check stimulus duration and PN.ratio
# model_St_dur =  glm(St_duration ~ PN.ratio, data = data_NP)
# summary(model_St_dur) #there is no correlation
#
#
#
# #Task_var
# model_all_NP4 =  glm(CP ~ PN.ratio + Task_var, data = data_NP)
# summary(model_all_NP4) #depth >, orientation<. But it may be confounded by brain area
# Anova(model_all_NP4, type="II")
#
# #Stimulus type
# model_all_NP4 =  glm(CP ~ PN.ratio + St_type, data = data_NP)
# summary(model_all_NP4) #grating <
# Anova(model_all_NP4, type="II")
#
# #Predict_targets
# model_all_NP4 =  glm(CP ~ PN.ratio + Predict_targets, data = data_NP)
# summary(model_all_NP4) #not significant
# Anova(model_all_NP4, type="II")
#
# ####now three variables
# model_all_NP4 =  glm(CP ~ PN.ratio + Brain.areas + Task, data = data_NP)
# summary(model_all_NP4) #brain is insignificant
# Anova(model_all_NP4, type="II")
#
# model_all_NP4 =  glm(CP ~ PN.ratio + Brain.areas + Task_parm, data = data_NP)
# summary(model_all_NP4) # all three are significant
# Anova(model_all_NP4, type="II")
#
# model_all_NP4 =  glm(CP ~ PN.ratio + Task + Task_parm, data = data_NP)
# summary(model_all_NP4) #task parm is insignificant
# Anova(model_all_NP4, type="II")
#
# #with Stimulus duraion
# model_all_3_Brain =  glm(CP ~ PN.ratio + Brain.areas + St_duration, data = data_NP)
# summary(model_all_3_Brain) # all significant
# Anova(model_all_3_Brain, type="II")
#
# model_all_3_Task =  glm(CP ~ PN.ratio + Task + St_duration, data = data_NP)
# summary(model_all_3_Task) # all significant, PN on the verge of significance p = 0.052
# Anova(model_all_3_Task, type="II")
#
# AIC(model_all_3_Brain,model_all_3_Task) #better is brain
#
# model_4 = glm(CP ~ PN.ratio + Brain.areas + Task + St_duration, data = data_NP)
# summary(model_4) #Task is not significant, but p = 0.06 for bistable
# Anova(model_4, type="II")
#
# AIC(model_all_3_Brain,model_4) #three is better
#
# ##add 5 variables and see
# data_5_NP = data_NP %>% dplyr::select(CP, PN.ratio, Brain.areas, Task, St_duration) %>% na.omit()
# full_model = lm(CP ~ ., data = data_5_NP)  # Full model with all predictors
# null_model = lm(CP ~ 1, data = data_5_NP)  # Only intercept
# step_forward = step(null_model, scope = formula(full_model), direction = "forward", trace = 0)
# summary(step_forward)
# step_backward = step(full_model, direction = "backward", trace = 0)
# summary(step_backward)
# step_both = step(full_model, direction = "both", trace = 0)
# summary(step_both)
# AIC(model_all_3_Brain, step_forward, step_backward, step_both)
#
#
#
# ###forward
# full_model <- lm(CP ~ ., data = data_NP %>% filter(!is.na(St_duration)))  # Full model with all predictors
# null_model <- lm(CP ~ 1, data = data_NP %>% filter(!is.na(St_duration)))  # Only intercept
#
# step_forward <- step(null_model, scope = formula(full_model), direction = "forward", trace = 0)
# summary(step_forward)
#
# ###backward
# step_backward <- step(full_model, direction = "backward", trace = 0)
# summary(step_backward)
#
# #both
# step_both <- step(full_model, direction = "both", trace = 0)
# summary(step_both)
#
# AIC(model_all_3_Brain, step_forward, step_backward, step_both)
#
# ###now the same but without Task_var and St_type
# full_model2 <- lm(CP ~ ., data = data_NP %>% dplyr::select(!c("Task_var", "St_type")) %>% filter(!is.na(St_duration)))  # Full model with all predictors
# null_model2 <- lm(CP ~ 1, data = data_NP %>% dplyr::select(-Task_var, -St_type) %>% filter(!is.na(St_duration)))  # Only intercept
# step_forward2 <- step(null_model2, scope = formula(full_model2), direction = "forward", trace = 0)
# summary(step_forward2)
# step_backward2 <- step(full_model2, direction = "backward", trace = 0)
# summary(step_backward2)
# step_both2 <- step(full_model2, direction = "both", trace = 0)
# summary(step_both2)
# AIC(model_all_3_Brain, step_forward2, step_backward2, step_both2)
#
# ##cross validation
# # Define LOOCV control
# library(caret)
# train_control <- trainControl(method = "LOOCV")
#
# # Fit the model using LOOCV
# loocv_full_model <- train(
#   CP ~ PN.ratio + Brain.areas + Stimulus_size + St_duration + Predict_targets,
#   data = data_NP %>% dplyr::select(!c("Task_var", "St_type")) %>% filter(!is.na(St_duration)),
#   method = "lm",
#   trControl = train_control
# )
#
# # Print results
# print(loocv_full_model)
#
# # Fit the model using LOOCV
# loocv_model_3 <- train(
#   CP ~ PN.ratio + Brain.areas + St_duration,
#   data = data_NP %>% dplyr::select(!c("Task_var", "St_type")) %>% filter(!is.na(St_duration)),
#   method = "lm",
#   trControl = train_control
# )
#
# print(loocv_model_3) #so this is the best for three variables, it is better for BIC compare to full model
#
# AIC(loocv_full_model$finalModel, loocv_model_3$finalModel)
# BIC(loocv_full_model$finalModel, loocv_model_3$finalModel)
#
#
#
# ## now do the same with Task_parm
# model_all_NP4 =  glm(CP ~ Task_parm, data = data_NP)
# model_all_NP4 %>% summary() #not significant
# #now all tailoring: task,nontask, stimulus size
# model_all_NP4 =  glm(CP ~ Task_parm + Non_task_parm + Stimulus_size, data = data_NP)
# model_all_NP4 %>% summary() #only stimulus size fit to RF
#
#
# ##so the proxy will be Task_parm and  Stimulus_size
# #+Brain area
# model_all_NP4 =  glm(CP ~ Task_parm  + Stimulus_size + Brain.areas, data = data_NP)
# model_all_NP4 %>% summary() #only stimulus size fit to RF
# Anova(model_all_NP4, type="II") #this is the best
#
# #+Stimulus duration
# model_all_NP4 =  glm(CP ~ Stimulus_size + St_duration, data = data_NP)
# model_all_NP4 %>% summary() #only stimulus size fit to RF
# Anova(model_all_NP4, type="II") #this is the best
#
# #+Task
# model_all_NP4 =  glm(CP ~ Task_parm  + Task + Stimulus_size, data = data_NP)
# model_all_NP4 %>% summary() #only stimulus size fit to RF
# Anova(model_all_NP4, type="II") #this is the best
#
#
#
#
#
# ###now model PN.ratio
#
#
# #+stimulus duration
# model_all_NP_var =  glm(PN.ratio ~  Task_parm + Non_task_parm + Stimulus_size + St_duration, data = data_NP_var)
# model_all_NP_var %>% summary()
# Anova(model_all_NP_var, type="II") #this is the best
#
#
# model_all_NP_var =  glm(PN.ratio ~ ., data = data_NP_var)
# model_all_NP_var %>% summary()
# Anova(model_all_NP_var, type="II") #this is the best
#
#
# model_all_NP_var2 =  glm(PN.ratio ~ Brain.areas + Task_parm + Stimulus_size + Task, data = data_NP_var)
# model_all_NP_var2 %>% summary()
# Anova(model_all_NP_var2, type="II") #this is the best
#
# model_all_NP_var3=  glm(PN.ratio ~ Task_parm + Task, data = data_NP_var)
# model_all_NP_var3 %>% summary()
# Anova(model_all_NP_var3, type="II") #this is the best
#
# ### now without nature of the task
# vars_reg2 = c("Brain.areas", "Task_parm", "Non_task_parm", "Stimulus_size", "St_duration", "Task", "Recording",  "CP", "Predict_targets", "method_pref")
# data_reg2 = data_main %>% select(all_of(vars_reg2))
#
# model_all2 =  glm(CP ~ ., data = data_reg2)
# model_all2 %>% summary()
# Anova(model_all2, type="II") #this is the best
#
# ###the same but without task type
# vars_reg3 = c("Brain.areas", "Task_parm", "Non_task_parm", "Stimulus_size", "St_duration", "Task_var", "Recording",  "CP", "Predict_targets", "method_pref")
# data_reg3 = data_main %>% select(all_of(vars_reg3))
# model_all3 =  glm(CP ~ .+ Task_parm*Non_task_parm, data = data_reg3)
# model_all3 %>% summary()
# Anova(model_all3, type="II") #this is the best
#
#
#
# ####all dataset without PN.ratio########
# vars_reg = c("Brain.areas", "Task_parm", "Non_task_parm", "Stimulus_size", "St_duration", "Task", "Recording", "Task_var", "St_type" ,"CP", "Predict_targets", "method_pref")
# data_reg = data_main %>% select(all_of(vars_reg))
# str(data_reg)
# ##only tailoring
# model_all =  glm(CP ~ Task_parm  + Stimulus_size, data = data_reg)
# model_all %>% summary() #Task_parmpopulation <: it takes into account that population recordings have lower P/N ratio, Stimulus_sizefit to RF  >
# Anova(model_all, type="II") #this is the best
#
# ##+Brain area
# model_all =  glm(CP ~ Task_parm  + Stimulus_size + Brain.areas, data = data_reg)
# model_all %>% summary() #Brain area, and all are significant
# Anova(model_all, type="II") #this is the best
#
# ##+Stimulus duration
# model_all =  glm(CP ~ Task_parm  + Stimulus_size + St_duration, data = data_reg)
# model_all %>% summary() #Stimulus size is insignificant
# Anova(model_all, type="II") #this is the best
#
# #check how stimulus duration is dependent on Stimulus size and Task_parm
# model_all =  glm(St_duration ~ Task_parm  + Stimulus_size, data = data_reg)
# model_all %>% summary() #both are significant, Task_parm_single have lower duration, Stimulus_sizefit to RF is longer
# Anova(model_all, type="II") #this is the best
#
# ##+Task
# model_all =  glm(CP ~ Task_parm  + Stimulus_size + Task, data = data_reg)
# model_all %>% summary() #all are significant, but direction of coeficients of stimulus size and task_parm become meaningless
# Anova(model_all, type="II") #this is the best
#
# #now all 5 varibles and do forward, backward and both
# data_5 = data_reg %>% dplyr::select(Brain.areas, Task_parm, Non_task_parm, Stimulus_size, St_duration, Task, CP) %>% na.omit()
# full_model <- lm(CP ~ Task_parm + Stimulus_size + St_duration + Task + Brain.areas, data = data_5)
# null_model <- lm(CP ~ 1, data = data_5)  # Only intercept
# step_forward <- step(null_model, scope = formula(full_model), direction = "forward", trace = 0)
# summary(step_forward)
# step_backward <- step(full_model, direction = "backward", trace = 0)
# summary(step_backward)
# step_both <- step(full_model, direction = "both", trace = 0)
# summary(step_both)
# AIC(step_forward, step_backward, step_both) #all models are the same : Task_parm (population<), Duration, Task, Brain.areas
#
# ##now all varibales "Task_var", "St_type"
# data_all = data_reg %>% dplyr::select(-Task_var, -St_type, - Predict_targets) %>% na.omit()
# full_model <- lm(CP ~ ., data = data_all)
# null_model <- lm(CP ~ 1, data = data_all)  # Only intercept
# step_forward <- step(null_model, scope = formula(full_model), direction = "forward", trace = 0)
# summary(step_forward)
# step_backward <- step(full_model, direction = "backward", trace = 0)
# summary(step_backward)
# step_both <- step(full_model, direction = "both", trace = 0)
# summary(step_both)
# AIC(step_forward, step_backward, step_both) #all models are the same: Brain.areas, Non-task (population <), Stimulus_size (population >), St_duration, Task, Predict_targetsfixed  (>, but insignificant)
#
#
#
#
# model_all =  glm(CP ~ . + Task_parm*Non_task_parm, data = data_reg)
# model_all %>% summary()
# # linearHypothesis(model_all, "Brain.areas1 + Brain.areas2 + Brain.areas3 + Brain.areas4 + Brain.areas5 + Brain.areas6 + Brain.areas7 = 0")
# # linearHypothesis(model_all, "Task1 + Task2 + Task3 = 0")
# # linearHypothesis(model_all, "St_type1 + St_type2 + St_type3 = 0")
# plot(model_all)
# str(data_PCA)
#
# #ANOVA
# library(car)
# Anova(model_all, type="II") #this is the best
#
# ##after excluding non-significant variables
# model_all2 =  glm(CP ~ Brain.areas +Task_var + St_duration + Task + Task_parm*Non_task_parm, data = data_reg)
# summary(model_all2)
# Anova(model_all2, type="II") #this is the best
#
#
# ##now interactions
# # model_int_param =  glm(CP ~ . + Task_parm * Non_task_parm, data = data_reg)
# # summary(model_int_param)
# # Anova(model_int_param, type="III")
#
# ##investigate why MT>MST but not when all variables are included
#
# data_main %>% filter(Brain.areas %in% c("MT", "MST/MSTd")) %>% count(Brain.areas, St_duration, Task_parm, Task, Task_var) %>% View()
#
# data_main %>% filter(Brain.areas %in% c("MT", "MST/MSTd")) %>%
#   group_by (Brain.areas) %>%
#   summarise(St_duration = mean(St_duration, na.rm = TRUE), Task_parm = sum(Task_parm == "single neuron", na.rm = TRUE)/n(), Task_bi = sum(Task == "bistable", na.rm = TRUE)/n(),
#             Task_det = sum(Task == "detection", na.rm = TRUE)/n(),  Task_var_depth = sum(Task_var == "depth", na.rm = TRUE)/n(), Task_var_dir = sum(Task_var == "direction", na.rm = TRUE)/n())
#
# #calculate the proportion of missing data for each of columns Learning, Task, St_duration, PN.ratio, St_size_ratio, Stimulus_size, Task_tailor, Recording, Brain.areas
#
#
# ###Do CP - , but only with the dataset that has Np.ratio
# data_reg_cut = data_NP %>% select(all_of(vars_reg))
# model_all_cut =  glm(CP ~ . + Task_parm*Non_task_parm, data = data_reg_cut)
# summary(model_all_cut)
# Anova(model_all_cut, type="II") #this is the best
#
# model_all_cut = glm(CP ~ Brain.areas + Stimulus_size + Task_var  + Task +St_type, data = data_reg_cut)
# summary(model_all_cut)
# Anova(model_all_cut, type="II") #this is the best

#check how different the CP papers that report NP.ratio vs not reporting them

# data_check_NP = data_main %>%
#   filter( !(Papers %in% c("Nienborg & Cumming (2006)", "Kang & Maunsell (2020)") & Monkey !="two combined")) %>%
#    bind_rows(data_add)  %>%
#    rename(NP.ratio = N.P.ratio) %>%
#    mutate(NP.ratio = ifelse(NP.ratio == ">>1", NA, NP.ratio) %>% delete_spaces() %>% before_bracket(), PN.ratio = 1/NP.ratio, HAVE_NP = factor(!is.na(NP.ratio)))
#
#
#
#
#
# plot_have_NP = data_check_NP %>%
#   plot_data("HAVE_NP", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, jitter = 0.2, xlab = "Have NP ratio", ADD_MEDIAN = TRUE, STAT_COMPARISON = TRUE)
#
# print(plot_have_NP)
#
# #now with single electrode recordings only
# plot_have_NP_single = data_check_NP %>% filter(Recording == "single-electrode") %>%
#   plot_data("HAVE_NP", "CP", REGRESSION_ALL = FALSE, REGRESSION_GROUP = FALSE, jitter = 0.2, xlab = "Have NP ratio", ADD_MEDIAN = TRUE, STAT_COMPARISON = TRUE)
# print(plot_have_NP_single)

