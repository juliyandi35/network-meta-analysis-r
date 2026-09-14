# Install and load netmeta package
# install.packages(c("netmeta","readxl","dplyr","stringr","meta","grid"))
library(netmeta)
library(readxl)
library(dplyr)
library(stringr)
library(meta)
library(grid)
library(NMA)

#### BMI ####
# Input cleaned and connected dataset
data_raw <- read_excel("Datasets/BMI.xlsx")
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

# Transitivity
hf_arm <- data %>%
  rename(
    study = Study,
    trt1  = Treat1,
    trt2  = Treat2,
    n1    = n1,
    n2    = n2,
    d1    = mean1,
    d2    = mean2,
    hiit  = `HIIT Treatment Type`,
    age   = `Mean Age`
  ) %>%
  pivot_longer(
    cols = c(trt1, trt2, n1, n2, d1, d2),
    names_to = c(".value", "arm"),
    names_pattern = "([a-z]+)([12])"
  )

hf_nma <- setup(
  study   = study,
  trt     = trt,
  d       = d,
  n       = n,
  z       = c(age, hiit),
  measure = "OR",
  ref     = "Control (No Exercise)",
  data    = hf_arm
)

pdf("Transitivity Graphs/BMI.pdf", width = 15, height = 20)
transitivity(hf_nma, age)
dev.off()

pw <- pairwise(
  treat = list(data$Treat1, data$Treat2),
  n = list(data$n1, data$n2),
  mean = list(data$mean1, data$mean2),
  sd = list(data$sd1, data$sd2),
  studlab = data$Study,
  sm = "SMD"
)

# Check network connectivity (optional)
netconnection(
  TE      = TE,
  seTE    = seTE,
  treat1  = Treat1,
  treat2  = Treat2,
  data    = pw,
  subset  = TRUE
)

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

# Nettable or GRADE
net_table <- nettable(nma_result, digits = 2,
                      path = "Network Tables/BMI nettable.xlsx",overwrite = TRUE)

# Network plot
pdf("Network Graphs/net plot BMI.pdf", width = 15, height = 20)
netgraph(nma_result, plastic = FALSE)
dev.off()

# League table
netleague(nma_result)

## BMI META 
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

# Forest plot
pdf("Forest Plots/forest plot BMI.pdf", width = 15, height = 20)
forest(meta_bmi_result,
       xlab = "Standardized Mean Difference (BMI)",
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

dev.off()

# Funnel plot
pdf("Funnel Plots/funnel plot BMI.pdf", width = 15, height = 20)
funnel(meta_bmi_result, main = "Funnel Plot of Standardized Mean Differences (BMI)", col = "black")

# Add study labels in black on funnel plot
with(meta_bmi_result, 
     text(x = TE, y = seTE, labels = data_meta$study, pos = 4, cex = 0.6, col = "black"))
dev.off()

#### BMI Z Score ####
# Input cleaned and connected dataset
data_raw <- read_excel("Datasets/BMI Z Score.xlsx")
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

# Transitivity
hf_arm <- data %>%
  rename(
    study = Study,
    trt1  = Treat1,
    trt2  = Treat2,
    n1    = n1,
    n2    = n2,
    d1    = mean1,
    d2    = mean2,
    hiit  = `HIIT Treatment Type`,
    age   = `Mean Age`
  ) %>%
  pivot_longer(
    cols = c(trt1, trt2, n1, n2, d1, d2),
    names_to = c(".value", "arm"),
    names_pattern = "([a-z]+)([12])"
  )

hf_nma <- setup(
  study   = study,
  trt     = trt,
  d       = d,
  n       = n,
  z       = c(age, hiit),
  measure = "OR",
  ref     = "Control (No Exercise)",
  data    = hf_arm
)

pdf("Transitivity Graphs/BMI Z Score.pdf", width = 15, height = 20)
transitivity(hf_nma, age)
dev.off()

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

# Nettable or GRADE
net_table <- nettable(nma_result, digits = 2,
                      path = "Network Tables/BMI Z Score nettable.xlsx",overwrite = TRUE)

# Network plot
pdf("Network Graphs/net plot BMI Z Score.pdf", width = 15, height = 20)
netgraph(nma_result, plastic = FALSE)
dev.off()

# League table
netleague(nma_result)

# BMI Z Score META
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
meta_bmi_z_result <- metacont(
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
z_value_bmi_z <- round(meta_bmi_z_result$zval.random, 2)
p_value_bmi_z <- format.pval(meta_bmi_z_result$pval.random, digits = 2, eps = 0.001)

# Forest plot
pdf("Forest Plots/forest plot BMI Z Score.pdf", width = 15, height = 20)
forest(meta_bmi_z_result,
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

dev.off()

# Funnel plot
pdf("Funnel Plots/funnel plot BMI Z Score.pdf", width = 15, height = 20)
funnel(meta_bmi_z_result, main = "Funnel Plot of Standardized Mean Differences (BMI Z Score)", col = "black")

# Add study labels in black on funnel plot
with(meta_bmi_z_result, 
     text(x = TE, y = seTE, labels = data_meta$study, pos = 4, cex = 0.6, col = "black"))
dev.off()

#### Body Fat ####
# Input cleaned and connected dataset
data_raw <- read_excel("Datasets/Body Fat.xlsx")
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

# Transitivity
hf_arm <- data %>%
  rename(
    study = Study,
    trt1  = Treat1,
    trt2  = Treat2,
    n1    = n1,
    n2    = n2,
    d1    = mean1,
    d2    = mean2,
    hiit  = `HIIT Treatment Type`,
    age   = `Mean Age`
  ) %>%
  pivot_longer(
    cols = c(trt1, trt2, n1, n2, d1, d2),
    names_to = c(".value", "arm"),
    names_pattern = "([a-z]+)([12])"
  )

hf_nma <- setup(
  study   = study,
  trt     = trt,
  d       = d,
  n       = n,
  z       = c(age, hiit),
  measure = "OR",
  ref     = "Control (No Exercise)",
  data    = hf_arm
)

pdf("Transitivity Graphs/Body Fat.pdf", width = 15, height = 20)
transitivity(hf_nma, age)
dev.off()

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

# Nettable or GRADE
net_table <- nettable(nma_result, digits = 2,
                      path = "Network Tables/Body Fat nettable.xlsx",overwrite = TRUE)


# Network plot
pdf("Network Graphs/net plot Body Fat.pdf", width = 15, height = 20)
netgraph(nma_result, plastic = FALSE)
dev.off()

# League table
netleague(nma_result)

# Body Fat META
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
meta_body_fat_result <- metacont(
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
z_value_body_fat <- round(meta_body_fat_result$zval.random, 2)
p_value_body_fat <- format.pval(meta_body_fat_result$pval.random, digits = 2, eps = 0.001)

# Forest plot
pdf("Forest Plots/forest plot Body Fat.pdf", width = 15, height = 20)
forest(meta_body_fat_result,
       xlab = "Standardized Mean Difference (Body Fat)",
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

dev.off()

# Funnel plot
pdf("Funnel Plots/funnel plot Body Fat.pdf", width = 15, height = 20)
funnel(meta_body_fat_result, main = "Funnel Plot of Standardized Mean Differences (Body Fat)", col = "black")

# Add study labels in black on funnel plot
with(meta_body_fat_result, 
     text(x = TE, y = seTE, labels = data_meta$study, pos = 4, cex = 0.6, col = "black"))
dev.off()

#### FM ####
# Input cleaned and connected dataset
data_raw <- read_excel("Datasets/FM.xlsx")
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

# Transitivity
hf_arm <- data %>%
  rename(
    study = Study,
    trt1  = Treat1,
    trt2  = Treat2,
    n1    = n1,
    n2    = n2,
    d1    = mean1,
    d2    = mean2,
    hiit  = `HIIT Treatment Type`,
    age   = `Mean Age`
  ) %>%
  pivot_longer(
    cols = c(trt1, trt2, n1, n2, d1, d2),
    names_to = c(".value", "arm"),
    names_pattern = "([a-z]+)([12])"
  )

hf_nma <- setup(
  study   = study,
  trt     = trt,
  d       = d,
  n       = n,
  z       = c(age, hiit),
  measure = "OR",
  ref     = "Control (No Exercise)",
  data    = hf_arm
)

pdf("Transitivity Graphs/FM.pdf", width = 15, height = 20)
transitivity(hf_nma, age)
dev.off()

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

# Nettable or GRADE
net_table <- nettable(nma_result, digits = 2,
                      path = "Network Tables/FM nettable.xlsx",overwrite = TRUE)

# Network plot
pdf("Network Graphs/net plot FM.pdf", width = 15, height = 20)
netgraph(nma_result, plastic = FALSE)
dev.off()

# League table
netleague(nma_result)

# FM META
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
meta_fm_result <- metacont(
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
z_value_fm <- round(meta_fm_result$zval.random, 2)
p_value_fm <- format.pval(meta_fm_result$pval.random, digits = 2, eps = 0.001)

# Forest plot
pdf("Forest Plots/forest plot FM.pdf", width = 15, height = 20)
forest(meta_fm_result,
       xlab = "Standardized Mean Difference (FM)",
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

dev.off()

# Funnel plot
pdf("Funnel Plots/funnel plot FM.pdf", width = 15, height = 20)
funnel(meta_fm_result, main = "Funnel Plot of Standardized Mean Differences (FM)", col = "black")

# Add study labels in black on funnel plot
with(meta_fm_result, 
     text(x = TE, y = seTE, labels = data_meta$study, pos = 4, cex = 0.6, col = "black"))
dev.off()

#### Weight ####
# Input cleaned and connected dataset
data_raw <- read_excel("Datasets/Weight.xlsx")
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

# Transitivity
hf_arm <- data %>%
  rename(
    study = Study,
    trt1  = Treat1,
    trt2  = Treat2,
    n1    = n1,
    n2    = n2,
    d1    = mean1,
    d2    = mean2,
    hiit  = `HIIT Treatment Type`,
    age   = `Mean Age`
  ) %>%
  pivot_longer(
    cols = c(trt1, trt2, n1, n2, d1, d2),
    names_to = c(".value", "arm"),
    names_pattern = "([a-z]+)([12])"
  )

hf_nma <- setup(
  study   = study,
  trt     = trt,
  d       = d,
  n       = n,
  z       = c(age, hiit),
  measure = "OR",
  ref     = "Control (No Exercise)",
  data    = hf_arm
)

pdf("Transitivity Graphs/Weight.pdf", width = 15, height = 20)
transitivity(hf_nma, age)
dev.off()

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

# Nettable or GRADE
net_table <- nettable(nma_result, digits = 2,
                      path = "Network Tables/Weight nettable.xlsx",overwrite = TRUE)

# Network plot
pdf("Network Graphs/net plot Weight.pdf", width = 15, height = 20)
netgraph(nma_result, plastic = FALSE)
dev.off()

# League table
netleague(nma_result)

# Weight META
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
meta_weight_result <- metacont(
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
z_value_weight <- round(meta_weight_result$zval.random, 2)
p_value_weight <- format.pval(meta_weight_result$pval.random, digits = 2, eps = 0.001)

# Forest plot
pdf("Forest Plots/forest plot Weight.pdf", width = 15, height = 20)
forest(meta_weight_result,
       xlab = "Standardized Mean Difference (Weight)",
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

dev.off()

# Funnel plot
pdf("Funnel Plots/funnel plot Weight.pdf", width = 15, height = 20)
funnel(meta_weight_result, main = "Funnel Plot of Standardized Mean Differences (Weight)", col = "black")

# Add study labels in black on funnel plot
with(meta_weight_result, 
     text(x = TE, y = seTE, labels = data_meta$study, pos = 4, cex = 0.6, col = "black"))
dev.off()
