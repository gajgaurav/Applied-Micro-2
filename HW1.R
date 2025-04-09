# Clean Environment
rm(list = ls(all = TRUE))

options(stringsAsFactors = FALSE)

# load packages
library(tidyverse) 
library(stargazer)
library(openxlsx)
library(haven)
library(lfe)
library(broom)

# file paths, change as required
data_path <- 'Desktop/NYU coursework/Applied Microeconomics 2/' 
output_path <- 'Desktop/NYU coursework/Applied Microeconomics 2/' 

hiv <- read.csv(paste0(data_path, 'hiv_data.csv'))

# 4
fig3a_rep <- hiv %>%
  group_by(any) %>%
  summarise(avg_got = mean(got, na.rm = TRUE)) %>%
  filter(any %in% c(0, 1)) %>%
  mutate(any = factor(any, levels = c(0, 1), labels = c("No Incentive", "Incentive")))

ggplot(fig3a_rep, aes(x = factor(any), y = avg_got)) +
  geom_bar(stat = "identity", fill = "darkgrey", width = 0.5) +
  labs(x = "", y = 'Fraction learning HIV results', title = "") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(size = 20)) +  
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))


ggsave(paste0(output_path, 'HW1_fig3a_rep.png'), width = 15, height = 10)

tinc_binned <- cut(hiv$tinc, 
                 breaks = c(-Inf, 0.000, 0.2, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, Inf),
                 labels = c("0", "0.1-0.2", "0.3-0.5", "0.5-1.0", "1-1.5", "1.5-2.0", "2.0-2.5", "2.5-3.0", ">3.0"),
                 right = TRUE) 
  

hiv$tinc_binned <- tinc_binned

fig3b_rep <- hiv %>%
  group_by(tinc_binned) %>%
  summarise(avg_got = mean(got, na.rm = TRUE)) %>%
  filter(!is.na(tinc_binned))

ggplot(fig3b_rep, aes(x = factor(tinc_binned), y = avg_got)) +
  geom_bar(stat = "identity", fill = "darkgrey", width = 0.5) +
  labs(x = "", y = 'Fraction learning HIV results', title = "") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(size = 18)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))

ggsave(paste0(output_path, 'HW1_fig3b_rep.png'), width = 15, height = 10)

# 5
dist_binned <- cut(hiv$distvct, 
                   breaks = c(-Inf, median(hiv$distvct), Inf),
                   labels = c(0, 1),
                   right = TRUE) 

hiv$far <- dist_binned

q5_table <- hiv %>%
  group_by(far) %>%
  summarise(
    avg_dist = mean(distvct, na.rm = TRUE),
    frac_know = mean(got, na.rm = TRUE)
  ) %>%
  filter(!is.na(far))

# 6 
monetary_value <- q5_table %>%
  mutate(across(c(avg_dist, frac_know), ~ . - lag(.))) %>%
  slice(2)

effective_value <- 100 * monetary_value$frac_know / monetary_value$avg_dist

# 7 

q7_table <- hiv %>%
  group_by(any) %>%
  summarise(
    avg_learning = mean(got, na.rm = TRUE),
    avg_condom_use =  mean(anycond, na.rm = TRUE)
    ) %>%
  filter(!is.na(any))

# 8 
q8_pre_table <- hiv %>%
  filter(hiv2004 == 1) %>%
  group_by(any) %>%
  summarise(
    avg_learning = mean(got, na.rm = TRUE),
    avg_condom_use =  mean(anycond, na.rm = TRUE)
  ) %>%
  filter(!is.na(any))
  
q8_table <- q8_pre_table %>%
  mutate(across(c(avg_learning, avg_condom_use), ~ . - lag(.))) %>%
  slice(2)

iv_estimate1 <- q8_table$avg_condom_use / q8_table$avg_learning

# 9
q9_table1 <- hiv %>%
  group_by(far) %>%
  summarise(
    avg_learning = mean(got, na.rm = TRUE),
    avg_condom_use =  mean(anycond, na.rm = TRUE)
  ) %>%
  filter(!is.na(far))

q9_pre_table <- hiv %>%
  filter(hiv2004 == 1) %>%
  group_by(far) %>%
  summarise(
    avg_learning = mean(got, na.rm = TRUE),
    avg_condom_use =  mean(anycond, na.rm = TRUE)
  ) %>%
  filter(!is.na(far))

q9_table3 <- q9_pre_table %>%
  mutate(across(c(avg_learning, avg_condom_use), ~ . - lag(.))) %>%
  slice(2)

iv_estimate2 <- q9_table3$avg_condom_use / q9_table3$avg_learning

# 10 and 11
hiv$random <- rbinom(n = nrow(hiv), size = 1, prob = 0.75)

mean_diff <- hiv %>%
  group_by(random) %>%
  summarise(mean_got = mean(got, na.rm = TRUE)) %>%
  summarise(diff = mean_got[random == 1] - mean_got[random == 0])

print(paste("Mean difference in learning HIV status:", round(mean_diff$diff, 4)))

# 12
set.seed(11220) 

randomize_diff <- function(data) {
  data$random <- rbinom(n = nrow(data), size = 1, prob = 0.75)
  mean_diff <- tapply(data$got, data$random, mean, na.rm = TRUE)
  return(mean_diff[2] - mean_diff[1])
}

results <- replicate(1000, randomize_diff(hiv))

# 13
ggplot(data.frame(differences = results), aes(x = differences)) +
   geom_density(fill = "steelblue", alpha = 0.7) +
   labs(title = "",
        x = "Difference in Learning HIV Status",
        y = "Density") +
   theme_minimal() +
   theme(plot.title = element_text(hjust = 0.5),
         text = element_text(size = 18))

ggsave(paste0(output_path, 'HW1_randomization.png'), width = 15, height = 10)

# 14 
print(mean(results > 0.451))
