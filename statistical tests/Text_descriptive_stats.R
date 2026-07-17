####Statistical Analisis of text descriptive measures###########################

#install packages
install.packages(c("readxl", "dplyr", "ggplot2", "MVN", "biotools", "car"))
install.packages("mvnormtest")
install.packages(c("rstatix", "FSA"))
install.packages(c("flextable", "officer"))


#load libraries
library(readxl)
library(dplyr)
library(ggplot2)
#library(MVN)
library(biotools)
library(car)
library(mvnormtest)
library(ggplot2)
library(rstatix)
library(FSA)
library(tidyr)
library(flextable)
library(officer)

#import files
AN <- read_excel("/Users/.../AN_textdescriptives.xlsx")
BN <- read_excel("/Users/.../BN_textdescriptives.xlsx")
Control <- read_excel("/Users/.../control_textdescriptives.xlsx")


#add grouping variables as we need a unique long dataset
AN$corpus <- "AN"
BN$corpus <- "BN"
Control$corpus <- "Control"

#convert to factor
AN$corpus <- factor(AN$corpus)
BN$corpus <- factor(BN$corpus)
Control$corpus <- factor(Control$corpus)

#merge the three datasets
data <- bind_rows(AN, BN, Control)
table(data$corpus) #check

#check if all numeric variables are stored as numeric
str(data)


#select variables for analysis 
vars <- c("n_tokens",
          "sentence_length_mean",
          "token_length_mean",
          "syllables_per_token_mean",
          "proportion_unique_tokens",
          "HD-D",
          "n_sentences")

#create analysis dataset
analysis_data <- data[, c("corpus", vars)]


#check normality
by(analysis_data$"HD-D", data$corpus, shapiro.test) #data re not normally distributed


#check normailty visually with QQ plot
ggplot(data, aes(sample = token_length_mean)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ corpus) #data re not normally distributed

#since the data are not normally distributed we are going to Use Kruskal–Wallis tests (one per dependent variable),
#Follow with Dunn post hoc tests,
#Apply multiple comparison correction,
#Report effect sizes for each variable separately.

#make sure corpus is a factor (should already be but just in case)
analysis_data$corpus <- factor(analysis_data$corpus)



# Kruskal-Wallis tests
kruskal_results <- analysis_data %>%
pivot_longer(cols = -corpus,
              names_to = "variable",
              values_to = "value") %>%
group_by(variable) %>%
kruskal_test(value ~ corpus) %>%
ungroup() %>%
mutate(
  p.adj = p.adjust(p, method = "holm")
)

# Add epsilon squared
kruskal_results <- analysis_data %>%
  pivot_longer(cols = -corpus,
               names_to = "variable",
               values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    H = kruskal.test(value ~ corpus)$statistic,
    p = kruskal.test(value ~ corpus)$p.value,
    n = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  mutate(
    k = 3,
    epsilon2 = (H - k + 1) / (n - k),
    p.adj = p.adjust(p, method = "holm")
  )

View(kruskal_results)


#Dunn test for all variables
dunn_results <- analysis_data %>%
  pivot_longer(cols = -corpus,
               names_to = "variable",
               values_to = "value") %>%
  group_by(variable) %>%
  dunn_test(value ~ corpus, p.adjust.method = "holm")

dunn_results

#effect size for dunn tests
dunn_results <- dunn_results %>%
  mutate(r = statistic / sqrt(n1 + n2))

View(dunn_results)

############################# Summary Table with test results and median and IQR #######################################

#recompute everything
library(rstatix)

# Convert to long format
long_data <- analysis_data %>%
  pivot_longer(cols = -corpus,
               names_to = "variable",
               values_to = "value")

# --- Kruskal-Wallis tests ---
kw_results <- long_data %>%
  group_by(variable) %>%
  kruskal_test(value ~ corpus)

# Holm correction across variables
kw_results <- kw_results %>%
  mutate(p_adjusted = p.adjust(p, method = "holm"))

# Compute epsilon squared
N <- nrow(analysis_data)
k <- length(unique(analysis_data$corpus))

#kw_results <- kw_results %>%
  #mutate(epsilon2 = (statistic - k + 1) / (N - k))

kw_results <- long_data %>%
  group_by(variable) %>%
  summarise(
    statistic = kruskal.test(value ~ corpus)$statistic,
    p = kruskal.test(value ~ corpus)$p.value,
    N = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  mutate(
    k = length(unique(analysis_data$corpus)),
    epsilon2 = (statistic - k + 1) / (N - k),
    p_adjusted = p.adjust(p, method = "holm")
  )


#add medians and IQR per group
descriptives <- long_data %>%
  group_by(variable, corpus) %>%
  summarise(
    median = median(value, na.rm = TRUE),
    IQR = IQR(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(median_IQR = paste0(round(median,2),
                             " (", round(IQR,2), ")")) %>%
  select(variable, corpus, median_IQR) %>%
  pivot_wider(names_from = corpus,
              values_from = median_IQR)

#Table 1
table1 <- kw_results %>%
  select(variable, statistic, p_adjusted, epsilon2) %>%
  left_join(descriptives, by = "variable") %>%
  mutate(
    statistic = round(statistic, 2),
    p_adjusted = round(p_adjusted, 3),
    epsilon2 = round(epsilon2, 3)
  )

colnames(table1)[2:4] <- c("H(2)", "p (Holm)", "Epsilon2")

#compute for table 2
dunn_results <- long_data %>%
  group_by(variable) %>%
  dunn_test(value ~ corpus, p.adjust.method = "holm") %>%
  mutate(
    r = statistic / sqrt(n1 + n2),
    statistic = round(statistic, 2),
    p.adj = round(p.adj, 3),
    r = round(r, 3)
  )

#create table 2
table2 <- dunn_results %>%
  select(variable, group1, group2, statistic, p.adj, r)

colnames(table2) <- c("Variable",
                      "Group 1",
                      "Group 2",
                      "Z",
                      "p (Holm)",
                      "Effect size r")

View(table1)
View(table2)

#export both tables to word
library(flextable)
library(officer)

doc <- read_docx()

# ---------- TABLE 1 ----------
ft1 <- flextable(table1) %>%
  autofit()

doc <- body_add_par(doc, "Table S1", style = "heading 1")
doc <- body_add_par(doc,
                    "Kruskal–Wallis Tests Across Variables",
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

print(doc, target = "Nonparametric_Results_Tables.docx")


################################################ Violin Plots ###################################################
ggplot(analysis_data, 
       aes(x = corpus, 
           y = proportion_unique_tokens, 
           fill = corpus)) +
  geom_violin(trim = FALSE, alpha = .4) +
  geom_boxplot(width = .15, outlier.shape = NA) +
  theme_classic() +
  labs(x = "Corpus",
       y = "Proportion of Unique Tokens") +
  theme(legend.position = "none",
        text = element_text(size = 12))

##Single big visualization itegrating all variables#############################

#convert to long format
vars <- c("HD-D",
          "proportion_unique_tokens",
          "sentence_length_mean",
          "syllables_per_token_mean",
          "token_length_mean",
          "n_tokens",
          "n_sentences")

plot_data <- analysis_data %>%
  pivot_longer(cols = all_of(vars),
               names_to = "variable",
               values_to = "value")

#adjust labels for visualization
plot_data$variable <- factor(plot_data$variable,
                             levels = c("HD-D",
                                        "proportion_unique_tokens",
                                        "sentence_length_mean",
                                        "syllables_per_token_mean",
                                        "token_length_mean",
                                        "n_tokens",
                                        "n_sentences"),
                             labels = c("HD-D",
                                        "Proportion of Unique Tokens",
                                        "Mean Sentence Length",
                                        "Mean Syllables per Token",
                                        "Mean Token Length",
                                        "Number of Tokens",
                                        "Number of Sentences")
)

ggplot(plot_data,
       aes(x = corpus,
           y = value,
           fill = corpus)) +
  geom_violin(trim = FALSE, alpha = .4) +
  geom_boxplot(width = .12, outlier.shape = NA) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_manual(values = c("AN" = "#1b9e77",
                               "BN" = "#d95f02",
                               "Control" = "#7570b3")) +
  theme_classic() +
  labs(x = "Corpus",
       y = NULL) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 9),     # facet titles
    axis.text = element_text(size = 8),      # tick labels
    axis.title = element_text(size = 9),     # axis titles
    text = element_text(size = 6)            # overall base size
  )






