library(readxl)
library(dplyr)
library(car)
library(sandwich)
library(lmtest)
library(emmeans)
library(effectsize)
library(broom)
library(tidyr)
library(ggplot2)
library(FSA)


#import files
BN <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/LIWC/LIWC-BN.xlsx")
AN <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/LIWC/LIWC-AN.xlsx")
Control <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/LIWC/LIWC-controllo.xlsx")


--------------------------------------------------------------------------------------------
  ################################# Personal References #####################################

#bind datasets and add corpus as a factor (persons)
AN$corpus <- "AN"
BN$corpus <- "BN"
Control$corpus <- "Control"

data <- rbind(AN, BN, Control)

data$corpus <- factor(data$corpus, levels = c("Control", "AN", "BN"))


#create long dataset

data_long <- data %>%
  pivot_longer(
    cols = c(Mec_Cog,
             Proc_Sen,
             Social,
             Fisico),
    names_to = "variable",
    values_to = "value"
  )

## Kruskal–Wallis test per tutte le categorie (pronomi normalizzati)

vars <- c("Mec_Cog",
          "Proc_Sen",
          "Social",
          "Fisico")

kw_results <- lapply(vars, function(v) {
  
  test <- kruskal.test(as.formula(paste(v, "~ corpus")), data = data)
  
  data.frame(
    Variable = v,
    H = as.numeric(test$statistic),
    p_value = test$p.value
  )
})

kw_results <- do.call(rbind, kw_results)

#apply holm correction
kw_results$p_adjusted <- p.adjust(kw_results$p_value, method = "holm")

kw_results

#effect size (eta sqaured)
k <- length(unique(data$corpus))   # numero gruppi
n <- nrow(data)                    # totale osservazioni

kw_results$eta2 <- (kw_results$H - k + 1) / (n - k)

kw_results


#Dunn tests corrected

dunn_results <- lapply(vars, function(v) {
  
  test <- dunnTest(as.formula(paste(v, "~ corpus")),
                   data = data,
                   method = "holm")
  
  res <- test$res
  res$Variable <- v
  
  return(res)
})

dunn_results <- do.call(rbind, dunn_results)

#effect size for Dunn tests
N <- nrow(data)

dunn_results$r <- dunn_results$Z / sqrt(N)

dunn_results

############################## APA tables #######################################

# Convert to long format (solo variabili normalizzate)
long_data <- data %>%
  pivot_longer(
    cols = c(Mec_Cog,
             Proc_Sen,
             Social,
             Fisico),
    names_to = "variable",
    values_to = "value"
  )

# ---------------------------
# KRUSKAL-WALLIS TESTS
# ---------------------------
library(rstatix)
kw_results <- long_data %>%
  group_by(variable) %>%
  kruskal_test(value ~ corpus)

# Holm correction across the 4 variables
kw_results <- kw_results %>%
  mutate(p_adjusted = p.adjust(p, method = "holm"))

# Compute epsilon squared
N <- nrow(data)
k <- length(unique(data$corpus))

kw_results <- kw_results %>%
  mutate(epsilon2 = (statistic - k + 1) / (N - k))

# ---------------------------
# MEDIANS + IQR
# ---------------------------

descriptives <- long_data %>%
  group_by(variable, corpus) %>%
  summarise(
    median = median(value, na.rm = TRUE),
    IQR = IQR(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(median_IQR = paste0(
    round(median, 2),
    " (", round(IQR, 2), ")"
  )) %>%
  select(variable, corpus, median_IQR) %>%
  pivot_wider(names_from = corpus,
              values_from = median_IQR)

# ---------------------------
# TABLE 1 (Omnibus)
# ---------------------------

table1 <- kw_results %>%
  select(variable, statistic, p_adjusted, epsilon2) %>%
  left_join(descriptives, by = "variable") %>%
  mutate(
    statistic = round(statistic, 2),
    p_adjusted = round(p_adjusted, 3),
    epsilon2 = round(epsilon2, 3)
  )

colnames(table1)[2:4] <- c("H(2)", "p (Holm)", "Epsilon2")

# ---------------------------
# DUNN TESTS
# ---------------------------

dunn_results <- long_data %>%
  group_by(variable) %>%
  dunn_test(value ~ corpus, p.adjust.method = "holm") %>%
  mutate(
    r = statistic / sqrt(n1 + n2),
    statistic = round(statistic, 2),
    p.adj = round(p.adj, 3),
    r = round(r, 3)
  )

# ---------------------------
# TABLE 2 (Post-hoc)
# ---------------------------

table2 <- dunn_results %>%
  select(variable, group1, group2, statistic, p.adj, r)

colnames(table2) <- c("Variable",
                      "Group 1",
                      "Group 2",
                      "Z",
                      "p (Holm)",
                      "Effect size r")

# ---------------------------
# EXPORT TO WORD (APA style)
# ---------------------------

library(flextable)
library(officer)

doc <- read_docx()

# ---------- TABLE 1 ----------
ft1 <- flextable(table1) %>%
  autofit()

doc <- body_add_par(doc, "Table S1", style = "heading 1")
doc <- body_add_par(doc,
                    "Kruskal–Wallis Tests. Cognition, Sociability, Perception and Body",
                    style = "Normal")
doc <- body_add_flextable(doc, ft1)

doc <- body_add_par(doc,
                    "Note. Values are reported as median (IQR). P-values are Holm-corrected across variables. Effect sizes are reported as epsilon squared (ε²).",
                    style = "Normal")

# ---------- TABLE 2 ----------
ft2 <- flextable(table2) %>%
  autofit()

doc <- body_add_par(doc, "\nTable S2", style = "heading 1")
doc <- body_add_par(doc,
                    "Cognition, Sociability, Perception and Body. Dunn Post-Hoc Comparisons with Holm Correction",
                    style = "Normal")
doc <- body_add_flextable(doc, ft2)

doc <- body_add_par(doc,
                    "Note. P-values are Holm-adjusted within each variable. Effect size r = Z / sqrt(n1 + n2).",
                    style = "Normal")

print(doc, target = "/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/paper-ready/tables/Cog_Perc_Social_Body.docx")
#######################################################################################
#Violin plot
###################
# Convert to long format (solo pronomi normalizzati)

vars <- c("Mec_Cog",
          "Proc_Sen",
          "Social",
          "Fisico")

plot_data <- data %>%
  pivot_longer(cols = all_of(vars),
               names_to = "variable",
               values_to = "value")

# Reorder factor levels INSIDE plot_data
plot_data$corpus <- factor(plot_data$corpus,
                           levels = c("AN", "BN", "Control"))


# Etichette più leggibili per la figura
plot_data$variable <- factor(plot_data$variable,
                             levels = c("Mec_Cog",
                                        "Proc_Sen",
                                        "Social",
                                        "Fisico"),
                             labels = c("Cognition",
                                        "Perception",
                                        "Social",
                                        "Body"))

ggplot(plot_data,
       aes(x = corpus,
           y = value,
           fill = corpus)) +
  
  geom_violin(trim = FALSE,
              alpha = .4,
              color = "black",
              linewidth = .3) +
  
  geom_boxplot(width = .12,
               outlier.shape = NA,
               color = "black",
               linewidth = .3) +
  
  facet_wrap(~ variable, scales = "free_y") +
  
  scale_fill_manual(values = c("AN" = "#1b9e77",
                               "BN" = "#d95f02",
                               "Control" = "#7570b3")) +
  
  scale_x_discrete(labels = c("AN" = "AN",
                              "BN" = "BN",
                              "Control" = "Control")) +
  
  theme_classic() +
  
  labs(x = "Corpus",
       y = "Normalized Frequency (%)") +
  
  theme(
    legend.position = "none",
    strip.text = element_text(size = 9),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9),
    text = element_text(size = 6)
  )
