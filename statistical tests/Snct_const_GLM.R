##################  Test the hypothesis that the three corpora differ in the distribution of syntactic constraction usage ########

library(readxl)
library(tidyr)
library(dplyr)

AN_data <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/paper-ready/frames_AN.xlsx")
BN_data <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/paper-ready/frames_BN.xlsx")
Control_data <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/paper-ready/frames_Control.xlsx")



#estrai e somma counts for each construction type
# Somma le occorrenze per ciascun tipo di costruzione
AN_counts <- colSums(AN_data[, c("Costruzioni_passive", "Passive_con_agente", "Costruzioni_attive", "Costruzioni_nanticausative", "Impersonali_e_nominali")])
BN_counts <- colSums(BN_data[, c("Costruzioni_passive", "Passive_con_agente", "Costruzioni_attive", "Costruzioni_nanticausative", "Impersonali_e_nominali")])
Control_counts <- colSums(Control_data[, c("Costruzioni_passive", "Passive_con_agente", "Costruzioni_attive", "Costruzioni_nanticausative", "Impersonali_e_nominali")])

# contingency matrix
data_matrix <- rbind(AN_counts, BN_counts, Control_counts)


#chi-squared test
chi_test <- chisq.test(data_matrix)
print(chi_test)


#check criteria 
expected_frequencies <- chi_test$expected
print(expected_frequencies)

# Verifica che tutte le frequenze attese siano >= 5
all(expected_frequencies >= 5)


#confronto corpus per corpus
AN_BN <- rbind(AN_counts, BN_counts)
chisq.test(AN_BN)

AN_Control <- rbind(AN_counts, Control_counts)
chisq.test(AN_Control)

BN_Control <- rbind(BN_counts, Control_counts)
chisq.test(BN_Control)

#apply Holm correction for multiple tests
pvals <- c(
  AN_BN = 0.9039,
  AN_Control = 0.01101,
  BN_Control = 0.005025
)
p.adjust(pvals, method = "holm")

#check residuals to check with constructions play a role
chi_AC <- chisq.test(rbind(AN_counts, Control_counts))
chi_BC <- chisq.test(rbind(BN_counts, Control_counts))

chi_AC$stdres
chi_BC$stdres

#compute percentages
AN_pct <- prop.table(AN_counts) * 100
BN_pct <- prop.table(BN_counts) * 100
Control_pct <- prop.table(Control_counts) * 100

#table
percentages <- rbind(
  AN = round(AN_pct, 1),
  BN = round(BN_pct, 1),
  Control = round(Control_pct, 1)
)

percentages

#table with residuals and percentages
results_table <- data.frame(
  Construction = names(AN_counts),
  AN = paste0(AN_counts, " (", round(100 * AN_counts/sum(AN_counts), 1), "%)"),
  BN = paste0(BN_counts, " (", round(100 * BN_counts/sum(BN_counts), 1), "%)"),
  Control = paste0(Control_counts, " (", round(100 * Control_counts/sum(Control_counts), 1), "%)")
)

results_table

sum(AN_counts)
sum(BN_counts)
sum(Control_counts)

chisq.test(data_matrix)

library(effectsize)
cramers_v(data_matrix)

###################################################################################
#Analisi a livello di testo e non f´di corpus con un modello a regressione multipla (?) poisson o negative n´binomial
######################################################################################
library(glmmTMB)

#unique identifier for each text
AN_data$TextID <-
  paste("AN", AN_data$File, sep="_")

BN_data$TextID <-
  paste("BN", BN_data$File, sep="_")

Control_data$TextID <-
  paste("Control", Control_data$File, sep="_")

#reshape datast
AN_data$Corpus <- "AN"
BN_data$Corpus <- "BN"
Control_data$Corpus <- "Control"

all_data <- bind_rows(AN_data, BN_data, Control_data)

#long format
long_data <- all_data %>%
  pivot_longer(
    cols = c(
      Costruzioni_passive,
      Passive_con_agente,
      Costruzioni_attive,
      Costruzioni_nanticausative,
      Impersonali_e_nominali
    ),
    names_to = "Construction",
    values_to = "Count"
  )




model <- glmmTMB(
  Count ~ Corpus * Construction +
    (1 | TextID),
  family = poisson,
  data = long_data
)

summary(model)

model_no_int <- update(model, . ~ . - Corpus:Construction)
anova(model_no_int, model, test = "Chisq")

library(emmeans)

emm1 <- emmeans(model, ~ Corpus | Construction, type = "response")
pairs(emm1, adjust = "holm")

##############################################
#trx model with total constructions as offset to model the proprtions instead of the raw counts
#############################################



all_data <- bind_rows(AN_data, BN_data, Control_data)

all_data <- all_data %>%
  mutate(
    TotalConstructions =
      Costruzioni_passive +
      Passive_con_agente +
      Costruzioni_attive +
      Costruzioni_nanticausative +
      Impersonali_e_nominali
  )

#reshape to long format
long_data <- all_data %>%
  pivot_longer(
    cols = c(
      Costruzioni_passive,
      Passive_con_agente,
      Costruzioni_attive,
      Costruzioni_nanticausative,
      Impersonali_e_nominali
    ),
    names_to = "Construction",
    values_to = "Count"
  )

#add offset to the model

model_offset <- glmmTMB(
  Count ~ Corpus * Construction +
    offset(log(TotalConstructions)) +
    (1 | TextID),
  family = poisson,
  data = long_data
)

model_offset_no_int <- glmmTMB(
  Count ~ Corpus + Construction +
    offset(log(TotalConstructions)) +
    (1 | TextID),
  family = poisson,
  data = long_data
)

anova(model_offset_no_int, model_offset, test = "Chisq")




emm1 <- emmeans(model_offset, ~ Corpus | Construction, type = "response")
pairs(emm1, adjust = "holm")



############################
#Visualizationn estimated marginal means
##########################################

library(ggplot2)

emm_plot <- emmeans(model_offset, ~ Corpus * Construction, type = "response")

emm_df <- as.data.frame(emm_plot)



# Ordine desiderato delle costruzioni
emm_df$Construction <- factor(
  emm_df$Construction,
  levels = c("Costruzioni_attive",
             "Costruzioni_passive",
             "Passive_con_agente",
             "Costruzioni_nanticausative",
             "Impersonali_e_nominali")
)

# Etichette personalizzate
construction_labels <- c(
  "Costruzioni_attive" = "active",
  "Costruzioni_passive" = "passive",
  "Passive_con_agente" = "passive + agent",
  "Costruzioni_nanticausative" = "anticausative",
  "Impersonali_e_nominali" = "impersonal"
)

ggplot(emm_df, aes(x = Construction, y = rate, fill = Corpus)) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.8),
                width = 0.2,
                linewidth = 0.4) +
  scale_fill_manual(values = c(
    "AN" = "#1b9e77",
    "BN" = "#d95f02",
    "Control" = "#7570b3"
  )) +
  scale_x_discrete(labels = construction_labels) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Construction type",
    y = "Estimated mean count"
  )


