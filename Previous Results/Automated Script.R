# Import Necessary Packages
library(netmeta)
library(readxl)
library(dplyr)
library(stringr)
library(meta)
library(grid)
library(NMA)
library(openxlsx)
library(metafor)
library(tidyr)
dir.create("Forest Plots SMD", showWarnings = FALSE)
dir.create("Funnel Plots SMD", showWarnings = FALSE)
dir.create("Leave-One-Out SMD", showWarnings = FALSE)
dir.create("Baujat Plots SMD", showWarnings = FALSE)
dir.create("Influence Diagnostic SMD", showWarnings = FALSE)
dir.create("Significance Tables", showWarnings = FALSE)

# =========================
# Helper function
# =========================
extract_mean_sd <- function(x) {
  x <- str_replace_all(x, ",", ".")
  nums <- str_extract_all(x, "[0-9.]+")[[1]]
  
  if (length(nums) == 1) {
    c(mean = as.numeric(nums[1]), sd = NA)
  } else {
    c(mean = as.numeric(nums[1]), sd = as.numeric(nums[2]))
  }
}

# =========================
# Main analysis function
# =========================
run_analysis <- function(file_path) {
  
  outcome <- tools::file_path_sans_ext(basename(file_path))
  message("Processing: ", outcome)
  
  # -------------------------
  # Read data
  # -------------------------
  data_raw <- read_excel(file_path)
  data <- data_raw %>%
    rowwise() %>%
    mutate(
      mean1 = extract_mean_sd(`Treatment Outcome 1`)[1],
      sd1   = extract_mean_sd(`Treatment Outcome 1`)[2],
      mean2 = extract_mean_sd(`Treatment Outcome 2`)[1],
      sd2   = extract_mean_sd(`Treatment Outcome 2`)[2]
    ) %>%
    ungroup() %>%
    rename(
      Study  = `Study`,
      n1     = `Treatment Sample 1`,
      n2     = `Treatment Sample 2`,
      Treat1 = `Treatment 1`,
      Treat2 = `Treatment 2`,
      Category = `Type of Stroke`
    ) %>%
    filter(!is.na(sd1), !is.na(sd2))
  
  data$Study <- make.unique(data$Study)
  
  # Effect size
  data$TE   <- data$mean1 - data$mean2
  data$seTE <- sqrt((data$sd1^2 / data$n1) + (data$sd2^2 / data$n2))
  
  # -------------------------
  # Pairwise meta-analysis
  # -------------------------
  data_meta <- data.frame(
    study = data$Study,
    treatment = data$Treat1,
    control = data$Treat2,
    mean_int = data$mean1,
    sd_int = data$sd1,
    n_int = data$n1,
    mean_ctrl = data$mean2,
    sd_ctrl = data$sd2,
    n_ctrl = data$n2,
    cat = data$Category
  )
  
  data_meta$subgroup <- ifelse(grepl("chronic", tolower(data_meta$cat)), "Chronic",
                               ifelse(grepl("sub-acute", tolower(data_meta$cat)), "Sub-acute","Other"))
  
  meta_result <- metacont(
    n.e = n_int,
    mean.e = mean_int,
    sd.e = sd_int,
    n.c = n_ctrl,
    mean.c = mean_ctrl,
    sd.c = sd_ctrl,
    studlab = study,
    data = data_meta,
    sm = "SMD",
    method.tau = "REML",
    subgroup = subgroup,
    random = TRUE
  )
  
  overall_df <- data.frame(
    Outcome = outcome,
    Type = "Overall",
    
    # Common effect
    TE_common = meta_result$TE.common,
    lower_common = meta_result$lower.common,
    upper_common = meta_result$upper.common,
    pval_common = meta_result$pval.common,
    
    # Random effect
    TE_random = meta_result$TE.random,
    lower_random = meta_result$lower.random,
    upper_random = meta_result$upper.random,
    pval_random = meta_result$pval.random
  )
  
  subgroup_df <- data.frame(
    Outcome = outcome,
    Type = meta_result$subgroup.levels,
    
    # Common effect
    TE_common = meta_result$TE.common.w,
    lower_common = meta_result$lower.common.w,
    upper_common = meta_result$upper.common.w,
    pval_common = meta_result$pval.common.w,
    
    # Random effect
    TE_random = meta_result$TE.random.w,
    lower_random = meta_result$lower.random.w,
    upper_random = meta_result$upper.random.w,
    pval_random = meta_result$pval.random.w
  )
  
  result_table <- bind_rows(overall_df, subgroup_df)
  
  write.xlsx(
    result_table,
    file = paste0("Significance Tables/", outcome, " summary_effect.xlsx"),
    overwrite = TRUE
  )
  
  pdf(paste0("Forest Plots SMD/forest plot ", outcome, ".pdf"), width = 10, height = 7)
  par(mar = c(0,0,0,0))
  forest(meta_result,
         xlab = paste("Standardized Mean Difference (", outcome, ")", sep = ""),
         subgroup = TRUE,fontsize = 9)
  dev.off()
  
  pdf(paste0("Funnel Plots SMD/funnel plot ", outcome, ".pdf"), 15, 20)
  par(mar = c(0,0,0,0))
  funnel(meta_result, main = paste("Funnel Plot -", outcome))
  if (!is.null(dev.list())) dev.off()
  
  # -------------------------
  # Leave-one-out (Influence analysis)
  # -------------------------
  loo_result <- metainf(meta_result)
  loo_sum <- summary(loo_result)
  
  loo_df <- data.frame(
    Study = loo_sum$studlab,
    TE    = loo_sum$TE,
    seTE = loo_sum$seTE,
    lower = loo_sum$lower,
    upper = loo_sum$upper,
    pval = loo_sum$pval,
    tau2 = loo_sum$tau2,
    tau = loo_sum$tau,
    I2 = loo_sum$I2
  )
  
  write.xlsx(
    loo_df,
    file = paste0("Leave-One-Out SMD/", outcome, " leave1out.xlsx"),
    overwrite = TRUE
  )
  
  pdf(paste0("Leave-One-Out SMD/leave1out plot ", outcome, ".pdf"), width = 10, height = 7)
  par(mar = c(0,0,0,0))
  forest(
    loo_result,
    xlab = paste("SMD after omitting one study"),
    fontsize = 9
  )
  dev.off()
  
  # -------------------------
  # Baujat plot
  # -------------------------
  pdf(paste0("Baujat Plots SMD/baujat plot ", outcome, ".pdf"), 10, 8)
  baujat(meta_result)
  dev.off()
  
  # -------------------------
  # Influence Diagnostic Plots
  # -------------------------
  rma_obj <- rma(
    yi  = meta_result$TE,
    sei = meta_result$seTE,
    method = "REML"
  )
  
  inf <- influence(rma_obj)
  
  png(
    filename = paste0("Influence Diagnostic SMD/influence_plot_", outcome, ".png")
  )
  plot(inf)
  dev.off()
  
}

# =========================
# RUN FOR ALL DATASETS
# =========================
files <- list.files("Datasets", pattern = "\\.xlsx$", full.names = TRUE)
lapply(files, run_analysis)

