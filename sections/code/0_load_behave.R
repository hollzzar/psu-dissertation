####################
## Set up session ##
####################

# Load packages and variables
source("/Users/hollyzaharchuk/Mirror/dissertation/16_jml/sections/code/0_global.R")

# Save file path
dat_path <- "/Users/hollyzaharchuk/Mirror/dissertation/12_behavioral"

####################
## Set up factors ##
####################

# Set up factor levels
lev_var <- c("variant", "invariant")
lab_var <- str_to_title(lev_var)
lev_sim_1a <- c("direct", "indirect", "control")
lab_sim_1a <- str_to_title(lev_sim_1a)
lev_sim <- c("direct", "indirect")
lab_sim <- str_to_title(lev_sim)
lev_exp_1a <- c("variant similar", "invariant similar",
                "variant dissimilar", "invariant dissimilar",
                "variant control", "invariant control")
lab_exp_1a <- c("Direct-Variant", "Direct-Invariant",
                "Indirect-Variant", "Indirect-Invariant",
                "Control-Variant", "Control-Invariant")
lev_exp <- lev_exp_1a[1:4]
lab_exp <- lab_exp_1a[1:4]
lev_match <- c("identity", "competitor", "unrelated")
lab_match <- str_to_title(lev_match)

# Set up factor table: Exp 1A
tab_exp_1a <- tibble(cond_exp = c("no training", lev_exp_1a),
                     cond_exp_fac = factor(cond_exp,
                                           levels = c("no training", lev_exp_1a),
                                           labels = c("Test-only", lab_exp_1a)))

# Factor table with Variabilty x Similarity: Exp 1A
tab_factor_1a <- tibble(cond_var = rep(lev_var, 3),
                        cond_sim = sort(rep(lev_sim_1a, 2)),
                        cond_var_fac = factor(cond_var, levels = lev_var,
                                              labels = lab_var),
                        cond_sim_fac = factor(cond_sim, levels = lev_sim_1a,
                                              labels = lab_sim_1a)) %>%
  mutate(cond_exp_fac = paste(cond_sim_fac, cond_var_fac, sep = "-"),
         cond_exp_fac = factor(cond_exp_fac, levels = lab_exp_1a))

# Factor table for other experiments
tab_exp <- tibble(cond_exp = c("no training", lev_exp),
                  cond_exp_fac = factor(cond_exp,
                                        levels = c("no training", lev_exp),
                                        labels = c("Test-only", lab_exp)))

# Factor table with Variability x Similarity for other experiments
tab_factor <- tibble(cond_var = rep(lev_var, 2),
                     cond_sim = sort(rep(lev_sim, 2)),
                     cond_var_fac = factor(cond_var, levels = lev_var,
                                           labels = lab_var),
                     cond_sim_fac = factor(cond_sim, levels = lev_sim,
                                           labels = lab_sim)) %>%
  mutate(cond_exp_fac = paste(cond_sim_fac, cond_var_fac, sep = "-"),
         cond_exp_fac = factor(cond_exp_fac, levels = lab_exp))

# Contrast tables: Variability
tab_contrast_var <- tibble(contrast = rep(c("control - direct", 
                                         "control - indirect",
                                         "direct - indirect"), 2),
                        cond_var = sort(rep(lev_var, 3)),
                        x_1a = c(1.7, 1.95, 1.7,
                              0.7, 0.95, 0.7),
                        xend_1a = c(2.3, 2.3, 2.05,
                                 1.3, 1.3, 1.05),
                        x = x_1a + 0.05,
                        xend = xend_1a + 0.2) %>%
  rowwise() %>%
  mutate(lab_pos_1a = mean(c(x_1a, xend_1a)),
         lab_pos = mean(c(x, xend)))

# Contrast tables: Similarity
tab_contrast_sim <- tibble(contrast = rep(c("invariant - variant"), 3),
                           cond_sim = lev_sim_1a,
                           x_1a = c(0.75, 1.75, 2.75),
                           xend_1a = c(1.25, 2.25, 3.25)) %>%
  rowwise() %>%
  mutate(lab_pos_1a = mean(c(x_1a, xend_1a)))

tab_contrast_exp <- tibble(contrast = c("Direct-Variant - Direct-Invariant",
                                        "Direct-Variant - Indirect-Variant",
                                        "Direct-Variant - Indirect-Invariant",
                                        "Direct-Invariant - Indirect-Variant",
                                        "Direct-Invariant - Indirect-Invariant",
                                        "Indirect-Variant - Indirect-Invariant"),
                           match_type = "competitor",
                           x = c(2, 2, 2, 3, 3, 4),
                           xend = c(3, 4, 5, 4, 5, 5)) %>%
  rowwise() %>%
  mutate(lab_x = mean(c(x, xend)),
         y = case_when(
           xend - x == 1 ~ 1.01,
           xend - x == 2 ~ 1.06,
           xend - x == 3 ~ 1.11,
           xend - x == 4 ~ 1.16
         ),
         lab_y = y + 0.01,
         x = x - 0.05,
         xend = xend + 0.05)

# Match type
tab_match <- tibble(match_type = lev_match, 
                    match_type_fac = factor(match_type, 
                                            levels = lev_match,
                                            labels = lab_match))

# Word type
tab_type <- tibble(type = c("real", "nonword"),
                   type_fac = factor(type, levels = c("real", "nonword"),
                                     labels = c("Real words", "Pseudowords")))

######################
## Set up contrasts ##
######################

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

# Create contrast codes: Test task
contrast_task <- matrix(c(1/2, -1/2), 
                        dimnames = list(c("match", "prime"), 
                                        c("match_prime")))

# Create contrast codes: Training
contrast_wout <- matrix(c(1/2, -1/2), 
                        dimnames = list(c("with", "without"), 
                                        c("with_without")))

# Create contrast codes: Order
contrast_talk <- matrix(c(3/4, -1/4, -1/4, -1/4,
                          0, 2/3, -1/3, -1/3,
                          0, 0, 1/2, -1/2),
                        ncol = 3,
                        dimnames = list(c("order1", "order2", "order3", "order4"),
                                        c("s6_v_all", "s5_v_s4s3", "s4_v_s3")))

###############
## Load data ##
###############

# Load exposure data: acc
dat_exp_filt <- read.csv(paste(dat_path, "data/filtered/dat_exp_filt.csv", sep = "/")) %>%
  replace_na(list(vot = 0)) %>%
  dplyr::filter(stim_cond != "filler") %>%
  mutate(cond_sim = case_when(
    cond_sim == "similar" ~ "direct",
    cond_sim == "dissimilar" ~ "indirect",
    TRUE ~ cond_sim
  )) 

# Load exposure data: rt
dat_exp_rt <- read.csv(paste(dat_path, "data/filtered/dat_exp_rt.csv", sep = "/")) %>%
  dplyr::filter(stim_cond != "filler") %>%
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
