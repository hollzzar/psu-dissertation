####################
## Set up session ##
####################

# Load packages and variables
source("0_global.R")

# Save file path
eeg_path <- "/Users/hollyzaharchuk/Mirror/dissertation/13_eeg"

######################
## Set up contrasts ##
######################

# Create contrast codes: Match type
contrast_match <- matrix(c(1/2, -1/2, 0,
                           -1/3, -1/3, 2/3), 
                         ncol = 2,
                         dimnames = list(c("control", "competitor", "identity"), 
                                         c("cont_comp", "mis_match")))

# Create contrast codes: Target type
contrast_target <- matrix(c(1/2, -1/2), 
                          ncol = 1,
                          dimnames = list(c("test", "filler"), 
                                          c("test_fill")))

#######################
## Load offline data ##
#######################

# Load language background (keep separate)
eeg_lhq <- read.csv(paste(eeg_path, "output/clean_lhq.csv", sep = "/")) %>%
  replace_na(list(lang_eng_dialect = "Not provided")) %>%
  mutate(par_accent_type = gsub("\\[|\\]|\"", "\\1", par_accent_type)) %>%
  replace_na(list(par_accent_strength = 0)) %>%
  mutate(par_accent_strength = if_else(id == 118, NA_integer_, par_accent_strength))

# Get contrast code for dialect experience
lhq_dat <- eeg_lhq %>%
  mutate(cc_eng = case_when(
    care_eng_l1 == "Yes" ~ -1,
    care_eng_l1 == "No, neither one" ~ 1,
    care_eng_l1 == "No, only one" ~ 0),
    cc_bilingual = case_when(
      par_bilingual == "Yes" ~ 1,
      par_bilingual == "No" ~ 0),
    cc_dia = case_when(
      lang_eng_dialect == "American" ~ 0, 
      lang_eng_dialect == "Not provided" ~ NA_integer_,
      TRUE ~ 1)) %>%
  select(id, starts_with("cc_"), par_accent_type, par_accent_strength)

# Load offline measures
behave_dat <- read.csv(paste(eeg_path, "output/clean_axcpt.csv", sep = "/")) %>%
  left_join(read.csv(paste(eeg_path, "output/clean_offline.csv", sep = "/")), by = "id") %>%
  left_join(lhq_dat, by = "id") %>%
  rowwise() %>%
  mutate(par_accent_type = case_when(
    is.na(par_accent_type) ~ list("x"),
    TRUE ~ str_split(par_accent_type, ",")),
    spk_accent_type = case_when(
      is.na(spk_accent_type) ~ list("y"),
      TRUE ~ str_split(spk_accent_type, ",")),
    comp_accent_type = list(par_accent_type[par_accent_type %in% spk_accent_type]),
    comp_accent_type = length(comp_accent_type),
    comp_accent_pct = comp_accent_type/length(spk_accent_type),
    spk_fluency = case_when(
      spk_lang_1 == "English" ~ spk_fluency_1,
      spk_lang_2 == "English" ~ spk_fluency_2,
      TRUE ~ NA_integer_
    ),
    acc_lang = case_when(
      spk_lang_1 == "Spanish" ~ 5,
      spk_lang_2 == "Spanish" ~ 4.5,
      spk_lang_1 == "Portuguese" ~ 4,
      spk_lang_2 == "Portuguese" ~ 3.5,
      spk_lang_1 == "French" ~ 3,
      spk_lang_2 == "French" ~ 2.5,
      spk_lang_1 %in% c("Urdu", "Hindi", "Bengali", "Russian") ~ 2,
      spk_lang_1 %in% c("Mandarin", "Standard Arabic", "None of these") ~ 1,
      spk_lang_1 %in% "English" ~ 0))

# Subset measures
behave_dat_sub <- behave_dat %>%
  mutate(id = as.character(id)) %>%
  select(-c(spk_accent_type, spk_lang_1, spk_lang_2, 
            spk_region, spk_subregion, spk_country,
            par_accent_type, comp_accent_type, 
            spk_fluency_1, spk_fluency_2))

##########################
## Load behavioral data ##
##########################

# Load table with trigger and condition information
code_tab <- read.csv(paste(eeg_path, "input/code_tab.csv", sep = "/")) %>%
  mutate(prime_cond = factor(prime_cond, 
                             levels = c("identity", "competitor", "control"),
                             labels = c("identity", "competitor", "unrelated")))

# Load stimulus info
word_info <- read.csv("~/Mirror/dissertation/4_stims/output/selections/final_exp.csv") %>%
  bind_rows(read.csv("~/Mirror/dissertation/4_stims/output/selections/final_test.csv"))

# Load in behavioral data
all_trial_dat <- read.csv(paste(eeg_path, "output/clean_eprime.csv", sep = "/")) %>% 
  left_join(code_tab) %>%
  mutate(vot = T2OnsetTime - T1OnsetTime,
         target = gsub("\\.wav", "", Target)) %>%
  rename(prime = Prime) %>%
  left_join(word_info, by = c("target" = "stim")) %>%
  select(trial, id, prime, target, true_rt, acc, prime_cond, target_cond, 
         vot, target, stim_onset, FreqZipfUS) %>%
  group_by(id) %>% 
  mutate(exp_amount = cumsum(target_cond == "exposure")) %>%
  ungroup()

# Add contrast codes
contrasts(all_trial_dat$prime_cond) <- contrast_match

##########################
## Behavioral: accuracy ##
##########################

# Pull test and filler trials
behave_trials_all <- all_trial_dat %>%
  dplyr::filter(target_cond == "test" | target_cond == "filler")

# Clean
behave_trials_clean <- behave_trials_all %>%
  dplyr::filter(acc == 1 | true_rt > 250) %>%
  mutate(across(.cols = c(id, target, target_cond, prime),
                as.factor),
         exp_cent = exp_amount - mean(exp_amount),
         vot_cent = vot - mean(vot),
         freq_cent = FreqZipfUS - mean(FreqZipfUS))

# Set contrasts
contrasts(behave_trials_clean$target_cond) <- contrast_target

######################
## Behavioral: test ##
######################

# Pull test trials
test_trial_dat <- all_trial_dat %>%
  dplyr::filter(target_cond == "test")

# Prep data
test_trial_dat_clean <- test_trial_dat %>%
  dplyr::filter(acc == 1 | true_rt > 250) %>%
  mutate(across(.cols = c(id, target, prime),
                as.factor),
         trial_cent = trial - mean(trial),
         vot_cent = vot - mean(vot),
         freq_cent = FreqZipfUS - mean(FreqZipfUS),
         exp_cent = exp_amount - mean(exp_amount))

###################
## Load ERP data ##
###################

# Load in N1
erp_n1_all <- read.csv(paste(eeg_path, "output/erp_n1.csv", sep = "/")) %>% 
  rename(prime = Prime,
         target = Target) %>%
  mutate(target = gsub("\\.wav", "", target)) %>%
  left_join(all_trial_dat, by = join_by(id, trial, prime, target))

# Load in P2
erp_p2_all <- read.csv(paste(eeg_path, "output/erp_p2.csv", sep = "/")) %>% 
  rename(prime = Prime,
         target = Target) %>%
  mutate(target = gsub("\\.wav", "", target)) %>%
  left_join(all_trial_dat, by = join_by(id, trial, prime, target))

# Load in N400
erp_n4_all <- read.csv(paste(eeg_path, "output/erp_n4.csv", sep = "/")) %>% 
  rename(prime = Prime,
         target = Target) %>%
  mutate(target = gsub("\\.wav", "", target)) %>%
  left_join(all_trial_dat, by = join_by(id, trial, prime, target))

##################
## Get channels ##
##################

# Pivot
erp_n1_long <- erp_n1_all %>%
  select(-T1Label) %>%
  pivot_longer(F7:P8, names_to = "chan", values_to = "volt")

# N1
chans_n1 <- erp_n1_long %>%
  group_by(chan) %>%
  summarise(mean_eff = mean(volt)) %>%
  dplyr::filter(mean_eff < -1) %>%
  pull(chan)

# Pivot
erp_p2_long <- erp_p2_all %>%
  select(-T1Label) %>%
  pivot_longer(F7:P8, names_to = "chan", values_to = "volt")

# P2
chans_p2 <- erp_p2_long %>%
  group_by(chan) %>%
  summarise(mean_eff = mean(volt)) %>%
  dplyr::filter(mean_eff > 1) %>%
  pull(chan)

# Pivot
erp_n4_long <- erp_n4_all %>%
  select(-T1Label) %>%
  pivot_longer(F7:P8, names_to = "chan", values_to = "volt")

# N400
chans_n4 <- erp_n4_long %>%
  group_by(chan) %>%
  summarise(mean_eff = mean(volt)) %>%
  dplyr::filter(mean_eff < -1) %>%
  pull(chan)

###############
## Prep data ##
###############

# N1
erp_n1 <- erp_n1_long %>%
  dplyr::filter(chan %in% chans_n1) %>%
  mutate(across(.cols = c(chan, id, target, prime),
                as.factor),
         trial_cent = trial - mean(trial),
         vot_cent = vot - mean(vot),
         freq_cent = FreqZipfUS - mean(FreqZipfUS),
         exp_cent = exp_amount - mean(exp_amount),
         stim_onset_fac = factor(stim_onset, levels = c("p", "t", "k")))

# P2
erp_p2 <- erp_p2_long %>%
  dplyr::filter(chan %in% chans_p2) %>%
  mutate(across(.cols = c(chan, id, target, prime),
                as.factor),
         trial_cent = trial - mean(trial),
         vot_cent = vot - mean(vot),
         freq_cent = FreqZipfUS - mean(FreqZipfUS),
         exp_cent = exp_amount - mean(exp_amount),
         stim_onset_fac = factor(stim_onset, levels = c("p", "t", "k")))

# N400
erp_n4 <- erp_n4_long %>%
  dplyr::filter(chan %in% chans_n4) %>%
  mutate(across(.cols = c(chan, id, target, prime),
                as.factor),
         trial_cent = trial - mean(trial),
         vot_cent = vot - mean(vot),
         freq_cent = FreqZipfUS - mean(FreqZipfUS),
         exp_cent = exp_amount - mean(exp_amount),
         stim_onset_fac = factor(stim_onset, levels = c("p", "t", "k")))

##################
## Correlations ##
##################

# Mean N1
mean_n1 <- erp_n1 %>%
  group_by(id, prime_cond) %>%
  summarise(mean = mean(volt), .groups = "keep") %>%
  pivot_wider(id_cols = id, names_from = prime_cond, values_from = mean) %>%
  mutate(mismatch = mean(competitor, unrelated),
         mean_n1 = abs(identity - mismatch))

# Mean P2
mean_p2 <- erp_p2 %>%
  group_by(id, prime_cond) %>%
  summarise(mean = mean(volt), .groups = "keep") %>%
  pivot_wider(id_cols = id, names_from = prime_cond, values_from = mean) %>%
  mutate(mismatch = mean(competitor, unrelated),
         mean_p2 = abs(identity - mismatch))

# Mean N400
mean_n4 <- erp_n4 %>%
  group_by(id, prime_cond) %>%
  summarise(mean = mean(volt), .groups = "keep") %>%
  pivot_wider(id_cols = id, names_from = prime_cond, values_from = mean) %>%
  mutate(mismatch = mean(competitor, unrelated),
         mean_n4 = abs(identity - mismatch))

# Accuracy
mean_acc <- test_trial_dat_clean %>%
  group_by(id, prime_cond) %>%
  summarise(mean = mean(acc), .groups = "keep") %>%
  pivot_wider(id_cols = id, names_from = prime_cond, values_from = mean) %>%
  mutate(mismatch = mean(competitor, unrelated),
         mean_acc = identity - mismatch)

# Combine
corr_dat <- behave_dat_sub %>%
  left_join(mean_n1 %>% select(id, mean_n1), by = "id") %>%
  left_join(mean_p2 %>% select(id, mean_p2), by = "id") %>%
  left_join(mean_n4 %>% select(id, mean_n4), by = "id") %>%
  left_join(mean_acc %>% select(id, mean_acc), by = "id") %>%
  select(mean_n1, mean_p2, mean_n4, mean_acc, 
         proact, react, lextale_score, mean_att,
         spk_accent_strength, spk_ease, spk_percep, spk_fluency,
         acc_lang, acc_sub_region, acc_accent, par_accent_strength, 
         cc_bilingual, cc_dia, cc_eng)
