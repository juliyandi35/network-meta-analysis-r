library(meta)
library(dplyr)
library(stringr)
library(readxl)

data_raw <- read_excel("Dataset.xlsx")

extract_mean_sd <- function(x) {
  x <- str_replace_all(x, ",", ".")
  nums <- str_extract_all(x, "[0-9.]+")[[1]]
  
  if (length(nums) == 1) {
    return(c(mean = as.numeric(nums[1]), sd = NA))
  } else {
    return(c(mean = as.numeric(nums[1]), sd = as.numeric(nums[2])))
  }
}

data_clean <- data_raw %>%
  rowwise() %>%
  mutate(
    mean1 = extract_mean_sd(`Treatment 1 Outcome`)[1],
    sd1   = extract_mean_sd(`Treatment 1 Outcome`)[2],
    mean2 = extract_mean_sd(`Treatment 2 Outcome`)[1],
    sd2   = extract_mean_sd(`Treatment 2 Outcome`)[2]
  ) %>%
  ungroup() %>%
  rename(
    study = `STUDY ID`,
    n1 = `Treatment 1 sample size`,
    n2 = `Treatment 2 Sample size`
  ) %>%
  filter(
    !is.na(sd1),
    !is.na(sd2)
  )

data_clean <- data_clean %>%
  rename(
    hiit_type = `HIIT Treatment Type`,
    age = `Mean Age`
  )
data_clean <- data_clean %>%
  filter(hiit_type!="Exercise")
table(data_clean$hiit_type)
data_clean$hiit_type <- factor(data_clean$hiit_type)

meta_main <- metacont(
  n.e = n1,
  mean.e = mean1,
  sd.e = sd1,
  
  n.c = n2,
  mean.c = mean2,
  sd.c = sd2,
  
  studlab = study,
  data = data_clean,
  
  sm = "SMD",
  method.smd = "Hedges",
  method.tau = "REML",
  
  random = TRUE,
  common = FALSE,
  method.random.ci = TRUE
)


summary(meta_main)

dev.new(width = 10, height = 14)
forest(
  meta_main,
  overall = TRUE,
  
  leftcols = c("studlab", "effect", "ci"),
  leftlabs = c("Study", "SMD", "95% CI"),
  
  xlab = "Standardized Mean Difference (Hedges g)",
  smlab = "",
  width = 10,
  height = 10,
  cex = 0.6
)

funnel(
  meta_main,
  xlab = "Standardized Mean Difference",
  ylab = "Standard Error"
)

metabias(
  meta_main,
  method.bias = "Egger"
)

tf <- trimfill(meta_main)
summary(tf)
funnel(tf)

meta_sub <- update(
  meta_main,
  subgroup = hiit_type,
  print.subgroup.name = TRUE
)

pdf("forest_plot_HIIT.pdf", width = 15, height = 20)

forest(
  meta_sub,
  overall = TRUE,
  print.byvar = TRUE,
  cex = 0.7,
  spacing = 0.9
)

dev.off()

meta_sub$Q.between
meta_sub$pval.Q.between

forest(
  meta_sub,
  test.subgroup = TRUE
)

# Network Meta Analysis
# KEEP ONLY FIRST ROW FOR EACH STUDY
data_single <- data_clean %>%
  arrange(study) %>%      # opsional, untuk konsistensi
  group_by(study) %>%
  slice(1) %>%            # ambil baris pertama saja
  ungroup()

# CREATE PAIRWISE DATA
pw <- pairwise(
  treat = list(data_single$treat1, data_single$treat2),
  mean  = list(data_single$mean1, data_single$mean2),
  sd    = list(data_single$sd1, data_single$sd2),
  n     = list(data_single$n1, data_single$n2),
  studlab = data_single$study,
  sm = "SMD"
)

library(netmeta)
pw_df <- as.data.frame(pw)
nc<- netconnection(
  TE      = TE,
  seTE    = seTE,
  treat1  = treat1,
  treat2  = treat2,
  data    = pw_df,
  subset  = TRUE
)

netgraph(
  nc,
  thickness = "number.of.studies",
  col = "black"
)

pw_main <- pw_df %>%
  filter(
    !(treat1 %in% c("hiit+lcit", "lcit") |
        treat2 %in% c("hiit+lcit", "lcit"))
  )

net_main <- netmeta(
  TE, seTE,
  treat1, treat2,
  studlab,
  data = pw_main,
  sm = "SMD",
  random = TRUE,
  reference.group = "control"
)

print(net_main, digits=2)
print(summary(net_main))

netgraph(net_main, start="random", iterate=TRUE, col="darkgray", cex=1.5, multiarm=FALSE, 
         points=TRUE, col.points="green", cex.points=3)

netgraph(net_main, start="circle", iterate=TRUE, col="darkgray", cex=1.5, 
         points=TRUE, col.points="black", cex.points=3, col.multiarm="gray")

summary(net_main, ref="control")

forest(net_main, ref="control", overall=FALSE)
forest(net_main, overall=FALSE, xlim=c(-1.5, 1), ref="control", leftlabs="Contrast to Control", pooled="random")

round(decomp.design(net_main)$Q.decomp, 3)
print(decomp.design(net_main)$Q.het.design, digits=2)
round(decomp.design(net_main)$Q.inc.random, 3)

round(decomp.design(net_main)$Q.inc.design, 2)

set.seed(123)
fe <- net_main$TE.nma.fixed
re <- net_main$TE.nma.random
plot(jitter((fe+re)/2, 5), jitter(fe-re, 5), xlim=c(-1.2, 1.2), 
     ylim=c(-0.25, 0.25), xlab="Mean treatment effect (in fixed effect and random effects model)", ylab="Difference of treatment effect (fixed effect minus random effects model)")
abline(h=0)

summary(net_main$seTE.nma.random / net_main$seTE.nma.fixed)


