# Clean Environment
rm(list = ls(all = TRUE))

# Set options
options(stringsAsFactors = FALSE)

# Function to check and install packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# Load required packages
install_if_missing('tidyverse')
install_if_missing('dplyr')
install_if_missing('lfe')
install_if_missing('Synth')
install_if_missing('ggplot2')
install_if_missing('stargazer')

# Define the directory path
data_path <- '/Users/gauravchoudhary/Desktop/NYU coursework/Applied Microeconomics 2/AM2_final_gc3402/'
output_path <- '/Users/gauravchoudhary/Desktop/NYU coursework/Applied Microeconomics 2/AM2_final_gc3402/' 





##### 0. Constructing the master dataset #####
# Unzipping and loading
zip_file <- file.path(data_path, 'BACI_combined.zip')
extract_path <- file.path(data_path) 
unzip(zip_file, exdir = extract_path)
baci_df <- read.csv(paste0(data_path, 'BACI_combined.csv'))

# Matching on country codes
country_codes <- read_csv(paste0(data_path, 'country_codes_V202501.csv'))

master <- left_join(baci_df, country_codes, by = c('exporter' = 'country_code')) %>%
          select(-country_iso2, -country_iso3) %>%  
          rename(exporter_name = country_name)  

master <- left_join(master, country_codes, by = c('importer' = 'country_code')) %>%
          select(-country_iso2, -country_iso3) %>%  
          rename(importer_name = country_name) 





##### 4. Origin by year analysis #####
# Constructing the dataset. Taking log(1 + x) here to avoid negative values since
# there are trade values < 1 unit (one thousand dollars) as well. In the aggregate,
# these don't change the results much.
dfq4 <- master %>%
        group_by(exporter_name, exporter, year) %>%
        summarise(total_value = sum(value, na.rm = TRUE),
                  total_quantity = sum(quantity, na.rm = TRUE)) %>%
        mutate(log_total_value = log(1 + total_value),
               log_total_quantity = log(1 + total_quantity))

treated_country <- 'Viet Nam'
treatment_year <- 2001

all_years <- sort(unique(dfq4$year))

# Filter out exporters that do not have observations for every year for balanced panel
dfq4 <- dfq4 %>%
  group_by(exporter) %>%
  filter(length(unique(year)) == length(all_years)) %>%
  ungroup()

dfq4$exporter <- as.numeric(dfq4$exporter)
dfq4 <- as.data.frame(dfq4)

# ID for Viet Nam
treated_id <- unique(dfq4 %>% 
                       filter(exporter_name == treated_country) %>% 
                       pull(exporter))

# ID for non- Viet Nam countries
control_ids <- unique(dfq4 %>% 
                        filter(exporter_name != treated_country) %>% 
                        pull(exporter))

# Synthetic control analysis
dataprep_obj <- dataprep(
  foo = dfq4,
  predictors = "log_total_quantity",
  predictors.op = "mean",
  dependent = "log_total_value",
  unit.variable = "exporter",
  time.variable = "year",
  treatment.identifier = treated_id,
  controls.identifier = control_ids,
  time.predictors.prior = seq(min(dfq4$year), treatment_year - 1),
  time.optimize.ssr = seq(min(dfq4$year), treatment_year - 1),
  unit.names.variable = "exporter_name",
  time.plot = seq(min(dfq4$year), max(dfq4$year))
)

synth_res <- synth(dataprep_obj)

# Visualize outcome trajectories 
png(filename = paste0(output_path, "q4_graph.png"), width = 1500, height = 1000, res = 150)
path.plot(
  synth.res = synth_res,
  dataprep.res = dataprep_obj,
  tr.intake = treatment_year,
  Ylab = "Log of Trade Value",
  Xlab = "Year",
  Legend = c("Treated", "Synthetic"),
  Legend.position = "topright"
)
dev.off()

# Regression estimate
Y_treated <- dataprep_obj$Y1plot     
Y_synth   <- dataprep_obj$Y0plot %*% synth_res$solution.w         

gap <- Y_treated - Y_synth

time_periods <- dataprep_obj$tag$time.plot
gap_df <- data.frame(year = time_periods, gap = gap)

gap_df_post <- gap_df %>% filter(year >= treatment_year)

reg_gap <- felm(gap ~ 1, data = gap_df_post)
estimate_q4 <- reg_gap$coefficients





##### 5. Origin - destination by year analysis #####
dfq5 <- master %>%
        group_by(exporter_name, importer_name, exporter, importer, year) %>%
        summarise(total_value = sum(value, na.rm = TRUE),
                  total_quantity = sum(quantity, na.rm = TRUE)) %>%
        mutate(log_total_value = log(1 + total_value),
               log_total_quantity = log(1 + total_quantity))

dfq51 <- dfq5 %>% filter(exporter_name == 'Viet Nam')

all_years <- sort(unique(dfq51$year))
dfq51 <- dfq51 %>% 
         group_by(importer, exporter) %>% 
         filter(length(unique(year)) == length(all_years)) %>%
         ungroup()

dfq51$importer <- as.numeric(dfq51$importer)
dfq51 <- as.data.frame(dfq51)

treated_id <- unique(dfq51 %>% 
                       filter(importer_name == 'USA') %>% 
                       pull(importer))

control_ids <- unique(dfq51 %>% 
                        filter(importer_name != 'USA') %>% 
                        pull(importer))

dataprep_obj <- dataprep(
  foo = dfq51,
  predictors = "log_total_quantity",
  predictors.op = "mean",
  dependent = "log_total_value",
  unit.variable = "importer",
  time.variable = "year",
  treatment.identifier = treated_id,
  controls.identifier = control_ids,
  time.predictors.prior = seq(min(dfq51$year), treatment_year - 1),
  time.optimize.ssr = seq(min(dfq51$year), treatment_year - 1),
  unit.names.variable = "importer_name",
  time.plot = seq(min(dfq51$year), max(dfq51$year))
)

synth_res <- synth(dataprep_obj)

# Visualize outcome trajectories 
png(filename = paste0(output_path, "q51_graph.png"), width = 1500, height = 1000, res = 150)
path.plot(
  synth.res = synth_res,
  dataprep.res = dataprep_obj,
  tr.intake = treatment_year,
  Ylab = "Log of Trade Value",
  Xlab = "Year",
  Legend = c("Treated", "Synthetic"),
  Legend.position = "topright"
)
dev.off()

# Regression estimate
Y_treated <- dataprep_obj$Y1plot     
Y_synth   <- dataprep_obj$Y0plot %*% synth_res$solution.w         

gap <- Y_treated - Y_synth

time_periods <- dataprep_obj$tag$time.plot
gap_df <- data.frame(year = time_periods, gap = gap)

gap_df_post <- gap_df %>% filter(year >= treatment_year)

reg_gap <- felm(gap ~ 1, data = gap_df_post)
estimate_q51 <- reg_gap$coefficients


dfq52 <- dfq5 %>% filter(importer_name == 'USA')

all_years <- sort(unique(dfq52$year))

# Filter out exporters that do not have observations for every year for balanced panel
dfq52 <- dfq52 %>%
  group_by(importer, exporter) %>%
  filter(length(unique(year)) == length(all_years)) %>%
  ungroup()

dfq52$exporter <- as.numeric(dfq52$exporter)
dfq52 <- as.data.frame(dfq52)

treated_id <- unique(dfq52 %>% 
                       filter(exporter_name == treated_country) %>% 
                       pull(exporter))

control_ids <- unique(dfq52 %>% 
                        filter(exporter_name != treated_country) %>% 
                        pull(exporter))

dataprep_obj <- dataprep(
  foo = dfq52,
  predictors = "log_total_quantity",
  predictors.op = "mean",
  dependent = "log_total_value",
  unit.variable = "exporter",
  time.variable = "year",
  treatment.identifier = treated_id,
  controls.identifier = control_ids,
  time.predictors.prior = seq(min(dfq52$year), treatment_year - 1),
  time.optimize.ssr = seq(min(dfq52$year), treatment_year - 1),
  unit.names.variable = "exporter_name",
  time.plot = seq(min(dfq52$year), max(dfq52$year))
)

synth_res <- synth(dataprep_obj)

# Visualize outcome trajectories 
png(filename = paste0(output_path, "q52_graph.png"), width = 1500, height = 1000, res = 150)
path.plot(
  synth.res = synth_res,
  dataprep.res = dataprep_obj,
  tr.intake = treatment_year,
  Ylab = "Log of Trade Value",
  Xlab = "Year",
  Legend = c("Treated", "Synthetic"),
  Legend.position = "topright"
)
dev.off()

# Regression estimate
Y_treated <- dataprep_obj$Y1plot     
Y_synth   <- dataprep_obj$Y0plot %*% synth_res$solution.w         

gap <- Y_treated - Y_synth

time_periods <- dataprep_obj$tag$time.plot
gap_df <- data.frame(year = time_periods, gap = gap)

gap_df_post <- gap_df %>% filter(year >= treatment_year)

reg_gap <- felm(gap ~ 1, data = gap_df_post)
estimate_q52 <- reg_gap$coefficients





###### 6. Placebo analysis #####
vietnam_gap <- dataprep_obj$Y1plot - (dataprep_obj$Y0plot %*% synth_res$solution.w)
years <- seq(min(dfq52$year), max(dfq52$year))
pre_indices <- which(years < treatment_year)
vn_rmspe <- sqrt(mean((dataprep_obj$Y1plot[pre_indices] -
                         (dataprep_obj$Y0plot[pre_indices, ] %*% synth_res$solution.w))^2))

placebo_gaps <- list()
pre_rmspe_list <- list()

donor_pool <- setdiff(unique(dfq52$exporter), treated_id)

for (candidate in donor_pool) {
  
  placebo_dp <- try(
    dataprep(
      foo = dfq52,
      predictors = "log_total_quantity",
      predictors.op = "mean",
      dependent = "log_total_value",
      unit.variable = "exporter",
      time.variable = "year",
      treatment.identifier = candidate,
      controls.identifier = setdiff(unique(dfq52$exporter), candidate),
      time.predictors.prior = seq(min(dfq52$year), treatment_year - 1),
      time.optimize.ssr = seq(min(dfq52$year), treatment_year - 1),
      unit.names.variable = "exporter_name",
      time.plot = seq(min(dfq52$year), max(dfq52$year))
    ),
    silent = TRUE
  )
  
  if (inherits(placebo_dp, "try-error")) next
  
  placebo_synth <- try(synth(placebo_dp), silent = TRUE)
  if (inherits(placebo_synth, "try-error")) next
  
  current_gap <- placebo_dp$Y1plot - (placebo_dp$Y0plot %*% placebo_synth$solution.w)
  
  candidate_rmspe <- sqrt(mean((placebo_dp$Y1plot[pre_indices] -
                                  (placebo_dp$Y0plot[pre_indices, ] %*% placebo_synth$solution.w))^2))
  
  placebo_gaps[[as.character(candidate)]] <- current_gap
  pre_rmspe_list[[as.character(candidate)]] <- candidate_rmspe
}

acceptable_candidates <- names(pre_rmspe_list)[sapply(pre_rmspe_list, function(x) x < 2.5 * vn_rmspe)]

png(filename = paste0(output_path, "q6_graph.png"), width = 1500, height = 1000, res = 150)
plot(years, vietnam_gap, type = "l", lwd = 2, col = "red",
     xlab = "Year", ylab = "Gap (Observed - Synthetic)")
abline(v = treatment_year, lty = 2, col = "black") # Treatment year divider

for (candidate in acceptable_candidates) {
  lines(years, placebo_gaps[[candidate]], col = "grey")
}

lines(years, vietnam_gap, lwd = 2, col = "red")

legend("bottomright", legend = c("Vietnam", "Placebo (Acceptable Fit)"),
       col = c("red", "grey"), lty = 1, lwd = c(2, 1))
dev.off()





##### 7. Weighted average treatment effect #####
dfq7 <- master %>%
  filter(importer_name == 'USA') %>%
  group_by(exporter_name, exporter, chapter, year) %>%
  summarise(total_value = sum(value, na.rm = TRUE),
            total_quantity = sum(quantity, na.rm = TRUE)) %>%
  mutate(log_total_value = log(1 + total_value),
         log_total_quantity = log(1 + total_quantity)) %>%
  ungroup()

# Identify the chapters in which Vietnam exports to the USA
vietnam_chapters <- dfq7 %>%
  filter(exporter_name == treated_country) %>%
  pull(chapter) %>%
  unique()

# Initialize lists to store results from each chapter
chapter_effects <- list()    # Average treatment effect for each chapter (post-treatment gap)
chapter_weights <- list()    # Weight: average pre-treatment trade level for Vietnam in the chapter
chapter_series <- list()     # Time series (years, actual and synthetic outcomes) for Vietnam in each chapter

# Loop over each chapter in which Vietnam exports to USA
for (current_chapter in vietnam_chapters) {
  
  # Convert current chapter to character for use as a name
  current_chapter_char <- as.character(current_chapter)
  
  # Filter data to the given chapter
  df_chapter <- dfq7 %>% filter(chapter == current_chapter)
  
  # Ensure a balanced panel: keep only exporters with data for all years in this chapter
  all_years <- sort(unique(df_chapter$year))
  df_chapter <- df_chapter %>%
    group_by(exporter) %>%
    filter(n_distinct(year) == length(all_years)) %>%
    ungroup()
  df_chapter <- as.data.frame(df_chapter)
  
  treated_id_chapter <- unique(df_chapter %>% filter(exporter_name == treated_country) %>% pull(exporter))
  if (length(treated_id_chapter) == 0) next  # Skip chapter if Vietnam is not present
  
  # Define the donor pool: all other exporters in this chapter
  control_ids_chapter <- unique(df_chapter %>% filter(exporter_name != treated_country) %>% pull(exporter))
  
  # Run synthetic control preparation (dataprep) for this chapter
  dp_chapter <- try(
    dataprep(
      foo = df_chapter,
      predictors = "log_total_quantity",
      predictors.op = "mean",
      dependent = "log_total_value",
      unit.variable = "exporter",
      time.variable = "year",
      treatment.identifier = treated_id_chapter,
      controls.identifier = control_ids_chapter,
      time.predictors.prior = seq(min(df_chapter$year), treatment_year - 1),
      time.optimize.ssr = seq(min(df_chapter$year), treatment_year - 1),
      unit.names.variable = "exporter_name",
      time.plot = seq(min(df_chapter$year), max(df_chapter$year))
    ),
    silent = FALSE
  )
  if (inherits(dp_chapter, "try-error")) next
  
  # Run synthetic control estimation for the chapter
  synth_res_chapter <- try(synth(dp_chapter), silent = TRUE)
  if (inherits(synth_res_chapter, "try-error")) next
  
  # Extract time series (for plotting) and compute average post-treatment gap as the effect
  years_plot <- seq(min(df_chapter$year), max(df_chapter$year))
  post_period <- which(years_plot >= treatment_year)
  
  actual_chapter    <- dp_chapter$Y1plot
  synthetic_chapter <- dp_chapter$Y0plot %*% synth_res_chapter$solution.w
  chapter_gap       <- actual_chapter - synthetic_chapter
  avg_effect        <- mean(chapter_gap[post_period])
  
  # Assign using character keys so the list elements are named
  chapter_effects[[current_chapter_char]] <- avg_effect
  
  # Weight for the chapter: use the average pre-treatment trade level for Vietnam in that chapter
  pre_period <- which(years_plot < treatment_year)
  pre_trade  <- mean(actual_chapter[pre_period])
  chapter_weights[[current_chapter_char]] <- pre_trade
  
  # Save the time series (for later aggregation and plotting)
  chapter_series[[current_chapter_char]] <- data.frame(
    year = years_plot,
    actual = as.vector(actual_chapter),
    synthetic = as.vector(synthetic_chapter),
    chapter = current_chapter
  )
}

# Calculate the overall (weighted average) treatment effect across chapters
valid_chapters <- intersect(names(chapter_effects), names(chapter_weights))
effects_vec <- unlist(chapter_effects[valid_chapters])
weights_vec <- unlist(chapter_weights[valid_chapters])
weighted_avg_effect <- sum(effects_vec * weights_vec) / sum(weights_vec)
print(paste("Weighted Average Treatment Effect (Synthetic Control) across chapters:", round(weighted_avg_effect, 4)))

# Create an aggregated time series by combining the chapter-specific series.
# Here, for each year we weight Vietnam’s actual and synthetic outcomes by the pre-treatment trade weight.
agg_list <- lapply(valid_chapters, function(current_chapter) {
  df <- chapter_series[[current_chapter]][, c("year", "actual", "synthetic")]
  # Multiply each chapter's outcomes by its weight
  df$actual    <- df$actual * chapter_weights[[current_chapter]]
  df$synthetic <- df$synthetic * chapter_weights[[current_chapter]]
  return(df)
})

agg_df <- bind_rows(agg_list)

# Aggregate by year: sum the weighted actual and synthetic values across chapters
agg_df <- agg_df %>%
  group_by(year) %>%
  summarise(
    total_actual    = sum(actual, na.rm = TRUE),
    total_synthetic = sum(synthetic, na.rm = TRUE)
  ) %>%
  ungroup()

# total_weight should be the sum of all chapter weights from your valid chapters
total_weight <- sum(weights_vec)

# Normalize by the total weight
agg_df <- agg_df %>%
  mutate(
    weighted_actual    = total_actual / total_weight,
    weighted_synthetic = total_synthetic / total_weight
  )

# Plot the aggregated time series for Vietnam: actual vs. synthetic control
png(filename = paste0(output_path, "q7_graph.png"), width = 1500, height = 1000, res = 150)
plot(agg_df$year, agg_df$weighted_actual, type = "l", lwd = 2, col = "red",
     xlab = "Year", ylab = "Log of Trade Value")
lines(agg_df$year, agg_df$weighted_synthetic, lwd = 2, col = "blue")
abline(v = treatment_year, lty = 2, col = "black")
legend("bottomright", legend = c("Actual Vietnam", "Synthetic Vietnam"),
       col = c("red", "blue"), lty = 1, lwd = 2)
dev.off()

# Regression estimate
agg_df <- agg_df %>%
  mutate(
    post = if_else(year >= treatment_year, 1, 0),
    gap  = weighted_actual - weighted_synthetic,
    treat_time = post  # treat_time indicates the post-treatment period
  )

reg_model_chapter <- felm(gap ~ treat_time, data = agg_df)
estimates_q7 <- reg_model_chapter$coefficients





##### 8. Bootstrapping standard errors by resampling chapters #####
common_chapters <- valid_chapters[
  sapply(chapter_effects[valid_chapters], function(x) length(x) == 1) &
    sapply(chapter_weights[valid_chapters], function(x) length(x) == 1)
]
effects_vec <- unlist(chapter_effects[common_chapters])
weights_vec <- unlist(chapter_weights[common_chapters])

chapter_boot <- data.frame(
  chapter = common_chapters,
  effect = effects_vec,
  weight = weights_vec
)

# Set the number of bootstrap iterations
n_boot <- 50000
boot_effects <- numeric(n_boot)
set.seed(4309)  # for reproducibility

# Resample chapters (with replacement) and compute the weighted average effect for each bootstrap sample
for (i in 1:n_boot) {
  boot_sample <- chapter_boot[sample(1:nrow(chapter_boot), size = nrow(chapter_boot), replace = TRUE), ]
  boot_effects[i] <- sum(boot_sample$effect * boot_sample$weight) / sum(boot_sample$weight)
}

# Plot the bootstrap distribution of the weighted average treatment effect
png(filename = paste0(output_path, "q8_graph.png"), width = 1500, height = 1000, res = 150)
hist(boot_effects, breaks = 30, xlab = "Weighted Average Treatment Effect",
     main = '', col = "lightblue")
abline(v = weighted_avg_effect, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Point Estimate"), col = "red", lty = 2, lwd = 2)
dev.off()

# Report summary statistics of the bootstrap estimates
boot_mean <- mean(boot_effects)
boot_sd   <- sd(boot_effects)
print(paste("Bootstrap Mean:", round(boot_mean, 4)))
print(paste("Bootstrap Standard Error:", round(boot_sd, 4)))
