####################
## Set up session ##
####################

# Load packages and variables
source("0_global.R")

# Load table with trigger and condition information
code_tab <- read.csv(paste(eeg_path, "input/code_tab.csv", sep = "/")) %>%
  mutate(prime_cond = factor(prime_cond, 
                             levels = c("identity", "competitor", "control"),
                             labels = c("identity", "competitor", "unrelated")))

# ROI
roi <- c("C3","C4","CP1","CP2","Cz","F3","F4","FC1","FC2",
         "FC5","FC6","Fz","P3","P4","P7","P8","Pz")

# Microvolts
v_max <- 2.5
v_min <- -5

###############
## Load data ##
###############

# Load EEG data for correct trials
sub_dat_corr <- read.csv("~/Mirror/dissertation/13_eeg/output/2_sub_eeg/sub_dat_corr.csv")

# Get values in desired channels
viz_dat <- sub_dat_corr %>%
  left_join(code_tab %>% select(prime_cond, T1Label)) %>%
  select(all_of(roi), id, time, prime_cond, trial) %>%
  pivot_longer(cols = all_of(roi), names_to = "chan", values_to = "volt") %>%
  group_by(id, time, prime_cond, trial) %>%
  summarise(erp = mean(volt), .groups = "keep") %>% 
  ungroup() %>% 
  mutate(prime_cond = str_to_title(prime_cond),
         prime_cond = factor(prime_cond, levels = c("Identity", "Competitor", "Unrelated")))

##########
## Plot ##
##########

# Grand mean across participants and trials
viz_plot <- ggplot(viz_dat,
                   aes(x = time, y = erp, color = prime_cond)) +
  annotate(geom = "rect", xmin = 175, xmax = 250, ymin = v_min, ymax = v_max, 
           fill = "#bbbbbb", color = "#bbbbbb", alpha = 0.4) +
  annotate(geom = "rect", xmin = 250, xmax = 375, ymin = v_min, ymax = v_max,
           fill = "#bbbbbb", color = "#bbbbbb", alpha = 0.4) +
  annotate(geom = "rect", xmin = 450, xmax = 650, ymin = v_min, ymax = v_max,
           fill = "#bbbbbb", color = "#bbbbbb", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = "mean", geom = "line", size = 1.75) +
  scale_color_manual(values = col_test, name = "Prime type") +
  scale_x_continuous(breaks = seq(-200, 800, 100)) +
  scale_y_continuous(breaks = seq(v_min, v_max, 0.5)) +
  ylab("Amplitude (μV)") +
  xlab("Time (ms)") +
  theme_classic() +
  theme(legend.position = "bottom",
        text = element_text(family = font_fam, size = text_size),
        axis.ticks = element_blank())

# Save plot and crop to remove white space
ggsave("outputs/grand_plot.png", viz_plot, 
       width = 4200, height = 2100, unit = "px", dpi = 300)
knitr::plot_crop("outputs/grand_plot.png")
