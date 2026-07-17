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
#personal references
bn_persons <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/STANZA output/risultati STANZA BN/output_bulimia_pronomi.xlsx")
an_persons <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/STANZA output/risultati STANZA AN/output_anoressia_pronomi.xlsx")
control_persons <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/STANZA output/risultati STANZA control/output_controllo_pronomi.xlsx")

#temporal references
bn_time <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/STANZA output/risultati STANZA BN/output_bulimia_tempi.xlsx")
an_time <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/STANZA output/risultati STANZA AN/output_anoressia_tempi.xlsx")
control_time <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/STANZA output/risultati STANZA control/output_controllo_tempi.xlsx")

--------------------------------------------------------------------------------------------
################################# Personal References #####################################

#bind datasets and add corpus as a factor (persons)
an_persons$corpus <- "an"
bn_persons$corpus <- "bn"
control_persons$corpus <- "control"

data <- rbind(an_persons, bn_persons, control_persons)

data$corpus <- factor(data$corpus, levels = c("control", "an", "bn"))

names(an_persons)
names(bn_persons)
names(control_persons)

#normalise raw counts by dividing by n_tokens and multiplying by 100
data <- data %>%
  mutate(
    first_sing_norm = (`1st sing.` / n_tokens) * 100,
    first_plur_norm = (`1st plur.` / n_tokens) * 100,
    second_norm     = (`2nd` / n_tokens) * 100,
    third_norm      = (`3rd` / n_tokens) * 100
  )

#check rapido
summary(data$first_sing_norm)
summary(data$first_plur_norm)
summary(data$second_norm)
summary(data$third_norm)

#check divisions by 0
sum(data$n_tokens == 0)

#create long dataset

data_long <- data %>%
  pivot_longer(
    cols = c(first_sing_norm,
             first_plur_norm,
             second_norm,
             third_norm),
    names_to = "variable",
    values_to = "value"
  )

## Kruskal–Wallis test per tutte le categorie (pronomi normalizzati)

vars <- c("first_sing_norm",
          "first_plur_norm",
          "second_norm",
          "third_norm")

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
    cols = c(first_sing_norm,
             first_plur_norm,
             second_norm,
             third_norm),
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
                    "Kruskal–Wallis Tests for Pronoun Categories",
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
                    "Dunn Post-Hoc Comparisons with Holm Correction",
                    style = "Normal")
doc <- body_add_flextable(doc, ft2)

doc <- body_add_par(doc,
                    "Note. P-values are Holm-adjusted within each variable. Effect size r = Z / sqrt(n1 + n2).",
                    style = "Normal")

print(doc, target = "/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/paper-ready/tables/Pronouns_Tables.docx")
#######################################################################################
#Violin plot
###################
# Convert to long format (solo pronomi normalizzati)

vars <- c("first_sing_norm",
          "first_plur_norm",
          "second_norm",
          "third_norm")

plot_data <- data %>%
  pivot_longer(cols = all_of(vars),
               names_to = "variable",
               values_to = "value")

# Reorder factor levels INSIDE plot_data
plot_data$corpus <- factor(plot_data$corpus,
                           levels = c("an", "bn", "control"))


# Etichette più leggibili per la figura
plot_data$variable <- factor(plot_data$variable,
                             levels = c("first_sing_norm",
                                        "first_plur_norm",
                                        "second_norm",
                                        "third_norm"),
                             labels = c("1st Singular",
                                        "1st Plural",
                                        "2nd person",
                                        "3rd person"))

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
  
  scale_fill_manual(values = c("an" = "#1b9e77",
                               "bn" = "#d95f02",
                               "control" = "#7570b3")) +
  
  scale_x_discrete(labels = c("an" = "AN",
                              "bn" = "BN",
                              "control" = "Control")) +
  
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

---------------------------------------------------------------------------------------
################################ Temporal References ###################################
#bind datasets and add corpus as a factor (persons)
an_time$corpus <- "an"
bn_time$corpus <- "bn"
control_time$corpus <- "control"

data <- rbind(an_time, bn_time, control_time)

data$corpus <- factor(data$corpus, levels = c("control", "an", "bn"))

names(an_time)
names(bn_time)
names(control_time)

#normalise raw counts by dividing by n_tokens and multiplying by 100
data <- data %>%
  mutate(
    present_norm = (`present` / n_tokens) * 100,
    past_norm = (`past` / n_tokens) * 100,
    future_norm     = (`future` / n_tokens) * 100,
  )

#check rapido
summary(data$present_norm)
summary(data$past_norm)
summary(data$future_norm)


#check divisions by 0
sum(data$n_tokens == 0)

#create long dataset

data_long <- data %>%
  pivot_longer(
    cols = c(present_norm,
             past_norm,
             future_norm,
             ),
    names_to = "variable",
    values_to = "value"
  )

## Kruskal–Wallis test per tutte le categorie (pronomi normalizzati)

vars <- c("present_norm",
          "past_norm",
          "future_norm"
          )

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
    cols = c(present_norm,
             past_norm,
             future_norm,
             ),
    names_to = "variable",
    values_to = "value"
  )

# ---------------------------
# KRUSKAL-WALLIS TESTS
# ---------------------------

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


doc <- read_docx()

# ---------- TABLE 1 ----------
ft1 <- flextable(table1) %>%
  autofit()

doc <- body_add_par(doc, "Table S1", style = "heading 1")
doc <- body_add_par(doc,
                    "Kruskal–Wallis Tests for Verbal Tense usage",
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
                    "Verbal tense usage. Dunn Post-Hoc Comparisons with Holm Correction",
                    style = "Normal")
doc <- body_add_flextable(doc, ft2)

doc <- body_add_par(doc,
                    "Note. P-values are Holm-adjusted within each variable. Effect size r = Z / sqrt(n1 + n2).",
                    style = "Normal")

print(doc, target = "/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/paper-ready/tables/Tenses_Tables.docx")
########################################################
#Violin plot

# Convert to long format (solo pronomi normalizzati)

vars <- c("present_norm",
          "past_norm",
          "future_norm"
          )

plot_data <- data %>%
  pivot_longer(cols = all_of(vars),
               names_to = "variable",
               values_to = "value")

# Reorder factor levels INSIDE plot_data
plot_data$corpus <- factor(plot_data$corpus,
                           levels = c("an", "bn", "control"))


# Etichette più leggibili per la figura
plot_data$variable <- factor(plot_data$variable,
                             levels = c("present_norm",
                                        "past_norm",
                                        "future_norm"
                                        ),
                             labels = c("Present",
                                        "Past",
                                        "Future"
                                        ))

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
  
  scale_fill_manual(values = c("an" = "#1b9e77",
                               "bn" = "#d95f02",
                               "control" = "#7570b3")) +
  
  scale_x_discrete(labels = c("an" = "AN",
                              "bn" = "BN",
                              "control" = "Control")) +
  
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
###########################################################################################
#POS ANALYSIS
#########################################################################################

bn_POS <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/STANZA output/risultati STANZA BN/output_bulimia_pos.xlsx")
an_POS <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/STANZA output/risultati STANZA AN/output_anoressia_pos.xlsx")
control_POS <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/STANZA output/risultati STANZA control/output_controllo_pos.xlsx")


#bind datasets and add corpus as a factor (persons)
an_POS$corpus <- "an"
bn_POS$corpus <- "bn"
control_POS$corpus <- "control"

data <- rbind(an_POS, bn_POS, control_POS)

data$corpus <- factor(data$corpus, levels = c("control", "an", "bn"))


#normalise raw counts by dividing by n_tokens and multiplying by 100
data <- data %>%
  mutate(
    nouns = (`NOUN` / n_tokens) * 100,
    verbs = (`VERB` / n_tokens) * 100,
    aux    = (`AUX` / n_tokens) * 100,
    adj      = (`ADJ` / n_tokens) * 100,
    punct = (`PUNCT` / n_tokens) * 100,
    adv = (`ADV` / n_tokens) * 100,,
    cconj = (`CCONJ` / n_tokens) * 100,
    det = (`DET` / n_tokens) * 100,
    num = (`NUM` / n_tokens) * 100,
    pron = (`PRON` / n_tokens) * 100,
    sconj = (`SCONJ` / n_tokens) * 100,
  )



#create long dataset
data_long <- data %>%
  pivot_longer(
    cols = c(nouns,
             verbs,
             aux,
             adj,
             punct,
             adv,
             cconj,
             det,
             num,
             pron,
             sconj),
    names_to = "variable",
    values_to = "value"
  )

###Tests
## Kruskal–Wallis test per tutte le categorie (pronomi normalizzati)

vars <- c("nouns",
          "verbs",
          "aux",
          "adj",
          "punct",
          "adv",
          "cconj",
          "det",
          "num",
          "pron",
          "sconj")

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

#Violin plot#####################################################

# Convert to long format (solo pronomi normalizzati)

vars <- c("verbs",
          "nouns"
          
)

plot_data <- data %>%
  pivot_longer(cols = all_of(vars),
               names_to = "variable",
               values_to = "value")

# Reorder factor levels INSIDE plot_data
plot_data$corpus <- factor(plot_data$corpus,
                           levels = c("an", "bn", "control"))


# Etichette più leggibili per la figura
plot_data$variable <- factor(plot_data$variable,
                             levels = c("nouns",
                                        "verbs"
                                        
                             ),
                             labels = c("nouns",
                                        "verbs"
                                        
                             ))

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
  
  scale_fill_manual(values = c("an" = "#1b9e77",
                               "bn" = "#d95f02",
                               "control" = "#7570b3")) +
  
  scale_x_discrete(labels = c("an" = "AN",
                              "bn" = "BN",
                              "control" = "Control")) +
  
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

