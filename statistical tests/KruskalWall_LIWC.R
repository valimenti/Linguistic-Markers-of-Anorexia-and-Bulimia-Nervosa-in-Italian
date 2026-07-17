################## Kruskal Wallis test per lIWC categories#############
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
bn <- read_excel("/Users/.../LIWC-BN.xlsx")
an <- read_excel("/Users/.../LIWC-AN.xlsx")
control <- read_excel("/Users/.../LIWC-controllo.xlsx")


#rename variables in all datasets
an <- an %>%
  rename(
    Positive = Emo_Pos,
    Optimism = Ottimis,
    Negative = Emo_Neg,
    Anxiety = Ansia,
    Anger = Rabbia,
    Sadness = Tristez
  )

bn <- bn %>%
  rename(
    Positive = Emo_Pos,
    Optimism = Ottimis,
    Negative = Emo_Neg,
    Anxiety = Ansia,
    Anger = Rabbia,
    Sadness = Tristez
  )

control <- control %>%
  rename(
    Positive = Emo_Pos,
    Optimism = Ottimis,
    Negative = Emo_Neg,
    Anxiety = Ansia,
    Anger = Rabbia,
    Sadness = Tristez
  )


#bind datasets and add corpus as a factor
an$corpus <- "an"
bn$corpus <- "bn"
control$corpus <- "control"

data <- rbind(an, bn, control)

data$corpus <- factor(data$corpus, levels = c("control", "an", "bn"))


##Kruskal-Wallis test per tutte le categorie
vars <- c("Positive", "Optimism", "Negative",
          "Anxiety", "Anger", "Sadness")

kw_results <- lapply(vars, function(v) {
  test <- kruskal.test(as.formula(paste(v, "~ corpus")), data = data)
  data.frame(
    Variable = v,
    H = as.numeric(test$statistic),
    p_value = test$p.value
  )
})

kw_results <- do.call(rbind, kw_results)

#holm correction for multiple tests
kw_results$p_adjusted <- p.adjust(kw_results$p_value, method = "holm")


#Effect Size (eta squared per Kruskal)
k <- length(unique(data$corpus))
n <- nrow(data)

kw_results$epsilon2 <- sapply(vars, function(v) {
  H <- kw_results$H[kw_results$Variable == v]
  n_var <- sum(!is.na(data[[v]]))
  (H - k + 1) / (n_var - k)
})


View(kw_results)

#Dunn test for all categories with Holm correction
dunn_results <- lapply(vars, function(v) {
  
  test <- dunnTest(as.formula(paste(v, "~ corpus")),
                   data = data,
                   method = "holm")
  
  res <- test$res
  res$Variable <- v
  return(res)
})

dunn_results <- do.call(rbind, dunn_results)

dunn_results

## Crea tabella APA con results of statistical tests + mediana and IQR
vars <- c("Positive", "Optimism", "Negative",
          "Anxiety", "Anger", "Sadness")

N <- nrow(data)
k <- length(unique(data$corpus))



kw_list <- lapply(vars, function(v) {
  
  # Kruskal test
  test <- kruskal.test(as.formula(paste(v, "~ corpus")), data = data)
  
  H <- as.numeric(test$statistic)
  p <- test$p.value
  eta2 <- (H - k + 1) / (N - k)
  
  # Mediana e IQR per gruppo
  desc <- data %>%
    group_by(corpus) %>%
    summarise(
      median = median(.data[[v]], na.rm = TRUE),
      IQR = IQR(.data[[v]], na.rm = TRUE)
    )
  
  data.frame(
    Variable = v,
    Control = paste0(
      round(desc$median[desc$corpus=="control"],2),
      " (",
      round(desc$IQR[desc$corpus=="control"],2),
      ")"
    ),
    AN = paste0(
      round(desc$median[desc$corpus=="an"],2),
      " (",
      round(desc$IQR[desc$corpus=="an"],2),
      ")"
    ),
    BN = paste0(
      round(desc$median[desc$corpus=="bn"],2),
      " (",
      round(desc$IQR[desc$corpus=="bn"],2),
      ")"
    ),
    H = round(H,2),
    p_raw = p,
    eta2 = round(eta2,3)
  )
})

kw_table <- do.call(rbind, kw_list)

# Correzione Holm tra le 6 variabili
kw_table$p_holm <- p.adjust(kw_table$p_raw, method = "holm")

kw_table$p_holm <- round(kw_table$p_holm, 3)
kw_table$p_raw <- round(kw_table$p_raw, 3)

kw_table <- kw_table[, c("Variable","Control","AN","BN","H","p_holm","eta2")]

colnames(kw_table) <- c("Variable",
                        "Control Mdn (IQR)",
                        "AN Mdn (IQR)",
                        "BN Mdn (IQR)",
                        "H(2)",
                        "p (Holm)",
                        "Eta2")


#APA table wuth Dunn tests
dunn_list <- lapply(vars, function(v) {
  
  test <- dunnTest(as.formula(paste(v, "~ corpus")),
                   data = data,
                   method = "holm")
  
  res <- test$res
  
  res$Variable <- v
  
  # Effect size r
  res$r <- res$Z / sqrt(N)
  
  res
})

dunn_table <- do.call(rbind, dunn_list)

dunn_table$Z <- round(dunn_table$Z,2)
dunn_table$P.adj <- round(dunn_table$P.adj,3)
dunn_table$r <- round(dunn_table$r,3)

dunn_table <- dunn_table[, c("Variable","Comparison","Z","P.adj","r")]

colnames(dunn_table) <- c("Variable",
                          "Comparison",
                          "Z",
                          "p (Holm)",
                          "Effect size r")

#export both tables ot word
library(flextable)
library(officer)

doc <- read_docx()

# ---------- TABLE 1 ----------
ft1 <- flextable(kw_table)
ft1 <- autofit(ft1)

doc <- body_add_par(doc, "Table S1", style = "heading 1")
doc <- body_add_par(doc,
                    "Group Differences in Affective Dimensions (Kruskal–Wallis Tests)",
                    style = "Normal")
doc <- body_add_flextable(doc, ft1)

doc <- body_add_par(doc,
                    "Note. Values are reported as median (IQR). P-values are Holm-corrected across affective dimensions. Effect sizes are reported as eta squared (η²).",
                    style = "Normal")

# ---------- TABLE 2 ----------
ft2 <- flextable(dunn_table)
ft2 <- autofit(ft2)

doc <- body_add_par(doc, "\nTable S2", style = "heading 1")
doc <- body_add_par(doc,
                    "Dunn Post-Hoc Comparisons with Holm Correction",
                    style = "Normal")
doc <- body_add_flextable(doc, ft2)

doc <- body_add_par(doc,
                    "Note. P-values are Holm-adjusted within each affective dimension. Effect size r = Z / sqrt(N).",
                    style = "Normal")

print(doc, target = "LIWC_Affective_Results.docx")

########################## Violin Plots #################################
library(ggplot2)

data_long <- data %>%
  pivot_longer(
    cols = c(Positive, Optimism, Negative, Anxiety, Anger, Sadness),
    names_to = "variable",
    values_to = "score"
  )


# Reorder factor levels INSIDE plot_data
data_long$corpus <- factor(data_long$corpus,
                           levels = c("an", "bn", "control"))


#Violin plots
ggplot(data_long,
       aes(x = corpus,
           y = score,
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

