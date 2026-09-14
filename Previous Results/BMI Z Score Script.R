# Install and load netmeta package
# install.packages("netmeta")
library(netmeta)
library(readxl)
library(dplyr)
library(stringr)

# Input cleaned and connected dataset
data_raw <- read_excel("BMI Z Score.xlsx")
data_raw

extract_mean_sd <- function(x) {
  x <- str_replace_all(x, ",", ".")
  nums <- str_extract_all(x, "[0-9.]+")[[1]]
  
  if (length(nums) == 1) {
    return(c(mean = as.numeric(nums[1]), sd = NA))
  } else {
    return(c(mean = as.numeric(nums[1]), sd = as.numeric(nums[2])))
  }
}

data <- data_raw %>%
  rowwise() %>%
  mutate(
    mean1 = extract_mean_sd(`Treatment 1 Outcome`)[1],
    sd1   = extract_mean_sd(`Treatment 1 Outcome`)[2],
    mean2 = extract_mean_sd(`Treatment 2 Outcome`)[1],
    sd2   = extract_mean_sd(`Treatment 2 Outcome`)[2]
  ) %>%
  ungroup() %>%
  rename(
    Study = `STUDY ID`,
    n1 = `Treatment 1 sample size`,
    n2 = `Treatment 2 Sample size`,
    Treat1 = `Treatment 1`,
    Treat2 = `Treatment 2`
  ) %>%
  filter(
    !is.na(sd1),
    !is.na(sd2)
  )
data$Study <- make.unique(data$Study)

# Calculate mean difference (TE) and standard error (seTE)
data$TE <- data$mean1 - data$mean2
data$seTE <- sqrt((data$sd1^2 / data$n1) + (data$sd2^2 / data$n2))

pw <- pairwise(
  treat = list(data$Treat1, data$Treat2),
  n = list(data$n1, data$n2),
  mean = list(data$mean1, data$mean2),
  sd = list(data$sd1, data$sd2),
  studlab = data$Study,
  sm = "SMD"
)

# Check network connectivity (optional)
nc <- netconnection(
  TE      = TE,
  seTE    = seTE,
  treat1  = Treat1,
  treat2  = Treat2,
  data    = pw,
  subset  = TRUE
)
nc

# Run network meta-analysis
nma_result <- netmeta(
  TE = data$TE,
  seTE = data$seTE,
  treat1 = data$Treat1,
  treat2 = data$Treat2,
  studlab = data$Study,
  sm = "SMD",
  data = data,
  reference.group = "Control (No Exercise)"
)

# View summary
summary(nma_result)

# Network plot
netgraph(nma_result, plastic = FALSE)

# League table
netleague(nma_result)

#### BMI META ####
# Install and load required libraries
# install.packages(c("meta", "grid"))
library(meta)
library(grid)

# Cleaned BMI dataset (already loaded in `data`)
# Create a new working dataset with proper structure
data_meta <- data.frame(
  study = data$Study,
  treatment = data$Treat1,
  control = data$Treat2,
  mean_int = data$mean1,
  sd_int = data$sd1,
  n_int = data$n1,
  mean_ctrl = data$mean2,
  sd_ctrl = data$sd2,
  n_ctrl = data$n2
)

# Create subgroup variable based on control types
data_meta$subgroup <- ifelse(grepl("No Exercise", data_meta$control), "Non-Exercise Control",
                             ifelse(grepl("MICT", data_meta$control), "MICT",
                                    ifelse(grepl("Low intensity", data_meta$control), "Low Intensity",
                                           ifelse(grepl("Nutrition|L-CIT", data_meta$control), "Nutrition",
                                                  ifelse(grepl("Supra|Sprint|SIIT", data_meta$control), "Sprint Variants",
                                                         "Other")))))

# Run subgroup meta-analysis
meta_bmi_result <- metacont(
  n.e = data_meta$n_int,
  mean.e = data_meta$mean_int,
  sd.e = data_meta$sd_int,
  n.c = data_meta$n_ctrl,
  mean.c = data_meta$mean_ctrl,
  sd.c = data_meta$sd_ctrl,
  data = data_meta,
  studlab = data_meta$study,
  sm = "SMD",
  method.tau = "REML",
  subgroup = data_meta$subgroup,
  random = TRUE
)

# Extract overall Z and p-value for random-effects model
z_value_bmi <- round(meta_bmi_result$zval.random, 2)
p_value_bmi <- format.pval(meta_bmi_result$pval.random, digits = 2, eps = 0.001)

# ----------- PLOT: Forest plot with manual p-values -------------
pdf("forest plot BMI Z Score.pdf", width = 15, height = 20)
forest(meta_bmi_result,
       xlab = "Standardized Mean Difference (BMI Z Score)",
       subgroup = TRUE,
       spacing = 0.5,
       fontsize = 7,
       digits.mean = 2,
       digits.sd = 2,
       digits.n = 0,
       col.square = "black",
       col.square.lines = "black",
       col.diamond = "black",
       col.diamond.lines = "black",
       col.subgroup = "black",
       col.label.left = "black",
       col.label.right = "black",
       colgap.forest.left = "0.5cm")

# ----------- TEXT: Add overall effect result -------------

# Create a separate annotation panel in top-right of plot
grid.text("Effect Summary", x = 0.85, y = 0.95, just = "left", gp = gpar(fontface = "bold", fontsize = 10, col = "black"))

# Overall effect
grid.text("Overall effect: Z = 4.69, p = < 0.001", x = 0.85, y = 0.90, just = "left", gp = gpar(col = "black", fontsize = 9))

dev.off()

# ----------- PLOT: Funnel plot -------------
funnel(meta_bmi_result, main = "Funnel Plot of Standardized Mean Differences (BMI Z Score)", col = "black")

# Add study labels in black on funnel plot
with(meta_bmi_result, 
     text(x = TE, y = seTE, labels = data_meta$study, pos = 4, cex = 0.6, col = "black"))
