####################
## Set up session ##
####################

# Load packages and variables
source("0_global.R")

# Save file path
dat_path <- "/Users/hollyzaharchuk/Mirror/dissertation/12_behavioral"

######################
## Set up contrasts ##
######################

# Set up factor levels
lev_exp <- c("no training", "variant similar", "invariant similar", 
             "variant dissimilar", "invariant dissimilar")
lab_exp <- c("no\ntraining", "variant\n/ptk/ (direct)", "invariant\n/ptk/ (direct)", 
             "variant\n/bdg/ (indirect)", "invariant\n/bdg/ (indirect)")
lev_match <- c("identity", "competitor", "unrelated")
lab_match <- c("park-park\n(identity)", "park-bark\n(competitor)", "park-wand\n(unrelated)")

# Set up factor table: Exp 1A
tab_exp_1a <- tibble(cond_exp = c(lev_exp, "variant control", "invariant control"), 
                     cond_exp_fac = factor(cond_exp, 
                                           levels = c(lev_exp, "variant control", "invariant control"), 
                                           labels = c(lev_exp, "variant control", "invariant control")))

# Factors for other experiments
tab_exp <- tibble(cond_exp = lev_exp, 
                  cond_exp_fac = factor(cond_exp, levels = lev_exp, labels = lev_exp),
                  cond_exp_lab = factor(cond_exp, levels = lev_exp, labels = lab_exp))

# Match type
tab_match <- tibble(match_type = lev_match, 
                    match_type_fac = factor(match_type, 
                                            levels = lev_match,
                                            labels = lev_match),
                    match_type_lab = factor(match_type, 
                                            levels = lev_match,
                                            labels = lab_match))

# Create contrast codes: Variability
contrast_var <- matrix(c(1/2, -1/2), 
                       dimnames = list(c("variant", "invariant"), 
                                       c("var_invar")))

# Create contrast codes: Similarity
contrast_sim_1a <- matrix(c(1/2, -1/2, 0,
                            -1/3, -1/3, 2/3), 
                          ncol = 2,
                          dimnames = list(c("direct", "indirect", "control"), 
                                          c("dir_ind", "vot_aff")))

# Create contrast codes: Similarity
contrast_sim <- matrix(c(1/2, -1/2), 
                       dimnames = list(c("direct", "indirect"), 
                                       c("dir_ind")))

# Create contrast codes: Training (individual)
contrast_train_1a <- matrix(c(-1/7, 6/7, -1/7, -1/7, -1/7, -1/7, -1/7,
                              -1/7, -1/7, 6/7, -1/7, -1/7, -1/7, -1/7,
                              -1/7, -1/7, -1/7, 6/7, -1/7, -1/7, -1/7,
                              -1/7, -1/7, -1/7, -1/7, 6/7, -1/7, -1/7,
                              -1/7, -1/7, -1/7, -1/7, -1/7, 6/7, -1/7,
                              -1/7, -1/7, -1/7, -1/7, -1/7, -1/7, 6/7),
                            ncol = 6,
                            dimnames = list(c("no training", 
                                              "variant direct", 
                                              "invariant direct", 
                                              "variant indirect", 
                                              "invariant indirect",
                                              "variant control", 
                                              "invariant control"), 
                                            c("no_var_dir", "no_invar_dir", 
                                              "no_var_ind", "no_invar_ind",
                                              "no_var_cont", "no_invar_cont")))

# Create contrast codes: Training (individual)
contrast_train <- matrix(c(-1/5, 4/5, -1/5, -1/5, -1/5,
                           -1/5, -1/5, 4/5, -1/5, -1/5,
                           -1/5, -1/5, -1/5, 4/5, -1/5,
                           -1/5, -1/5, -1/5, -1/5, 4/5), 
                         ncol = 4,
                         dimnames = list(c("no training", 
                                           "variant direct", 
                                           "invariant direct", 
                                           "variant indirect", 
                                           "invariant indirect"), 
                                         c("no_var_dir", "no_invar_dir", 
                                           "no_var_ind", "no_invar_ind")))

# Create contrast codes: Match type
contrast_match_1 <- matrix(c(1/2, -1/2, 0,
                             -1/3, -1/3, 2/3), 
                           ncol = 2,
                           dimnames = list(c("unrelated", "competitor", "identity"), 
                                           c("unrel_comp", "mis_match")))

# Create contrast codes: Match type
contrast_match_2 <- matrix(c(1/2, -1/2, 0,
                             -1/3, -1/3, 2/3), 
                           ncol = 2,
                           dimnames = list(c("identity", "competitor", "unrelated"), 
                                           c("id_comp", "rel_unrel")))

# Create contrast codes: Exposure stimuli
contrast_stim <- matrix(c(1/2, -1/2), 
                        dimnames = list(c("critical", "filler"), 
                                        c("critical_fill")))

# Create contrast codes: Exposure word type
contrast_type <- matrix(c(1/2, -1/2), 
                        dimnames = list(c("real", "nonword"), 
                                        c("real_non")))

###############
## Load data ##
###############

# Load exposure data: acc
dat_exp_filt <- read.csv(paste(dat_path, "data/filtered/dat_exp_filt.csv", sep = "/")) %>%
  dplyr::filter(stim_cond != "filler" & type == "real") %>%
  replace_na(list(vot = 0)) %>%
  mutate(cond_sim = case_when(
    cond_sim == "similar" ~ "direct",
    cond_sim == "dissimilar" ~ "indirect",
    TRUE ~ cond_sim
  ))

# Load exposure data: rt
dat_exp_rt <- read.csv(paste(dat_path, "data/filtered/dat_exp_rt.csv", sep = "/")) %>%
  dplyr::filter(stim_cond != "filler" & type == "real") %>%
  replace_na(list(vot = 0)) %>%
  mutate(cond_sim = case_when(
    cond_sim == "similar" ~ "direct",
    cond_sim == "dissimilar" ~ "indirect",
    TRUE ~ cond_sim
  ))

# Load test data: acc
dat_test_filt <- read.csv(paste(dat_path, "data/filtered/dat_test_filt.csv", sep = "/")) %>%
  mutate(cond_sim = case_when(
    cond_sim == "similar" ~ "direct",
    cond_sim == "dissimilar" ~ "indirect",
    TRUE ~ cond_sim
  ),
  match_type = if_else(match_type == "control", "unrelated", match_type))

# Load test data: rt
dat_test_rt <- read.csv(paste(dat_path, "data/filtered/dat_test_rt.csv", sep = "/")) %>%
  mutate(cond_sim = case_when(
    cond_sim == "similar" ~ "direct",
    cond_sim == "dissimilar" ~ "indirect",
    TRUE ~ cond_sim
  ),
  match_type = if_else(match_type == "control", "unrelated", match_type))
