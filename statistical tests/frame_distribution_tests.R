################## Semantic Frame distributions ##############################

library(dplyr)
library(tidyr)
library(readxl)
library(broom)

##########################################################
#GLOBAL TREST: Cho squard with Monte Carlo Distribution

############################################################

#load data
BN <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/LOME data/BN/aggregated_lome_spacy_BN.xlsx")
AN <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/LOME data/AN/aggregated_lome_spacy_anoressia.xlsx")
Control <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/LOME data/CN/aggregated_lome_spacy_cn.xlsx")



#collapse one row per frame per corpus
AN_clean <- AN %>%
  select(SituationKind, Occurrences) %>%
  distinct() %>%
  mutate(Corpus = "AN")

BN_clean <- BN %>%
  select(SituationKind, Occurrences) %>%
  distinct() %>%
  mutate(Corpus = "BN")

Control_clean <- Control %>%
  select(SituationKind, Occurrences) %>%
  distinct() %>%
  mutate(Corpus = "Control")


#merge datasets
all_data <- bind_rows(AN_clean, BN_clean, Control_clean)

#create contingency table
frame_table <- all_data %>%
  pivot_wider(names_from = Corpus,
              values_from = Occurrences,
              values_fill = 0)

#apply frequency threshold
frame_table <- frame_table %>%
  mutate(Total = AN + BN + Control)

#filter
frame_table_filtered <- frame_table %>%
  filter(Total >= 5)

#prepare matrix for test
chi_matrix <- frame_table_filtered %>%
  select(AN, BN, Control) %>%
  as.matrix()

#run test with Monte Carlo Simulation
chi_result_sim <- chisq.test(chi_matrix, simulate.p.value = TRUE, B = 10000)

chi_result_sim







####################################################################################################################
# 1. LOAD DATA
############################################################

AN <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/paper-ready/frames_AN.xlsx") %>%
  mutate(Corpus = "AN",
         File = paste0("AN_", File))

BN <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/paper-ready/frames_BN.xlsx") %>%
  mutate(Corpus = "BN",
         File = paste0("BN_", File))

Control <- read_excel("/Users/valimenti/Documents/TESI- ALIMENTI VALENTINA/paper-ready/frames_Control.xlsx") %>%
  mutate(Corpus = "Control",
         File = paste0("Control_", File))

all_data <- bind_rows(AN, BN, Control)

############################################################
# 2. TOTAL FRAMES PER TEXT (OFFSET BASE)
############################################################

text_totals <- all_data %>%
  group_by(File, Corpus) %>%
  summarise(
    TotalFramesText = sum(Totale_occorrenze_frame),
    .groups = "drop"
  )

############################################################
# 3. TEXT × FRAME MATRIX
############################################################

all_frames <- unique(all_data$Frame)

complete_data <- text_totals %>%
  crossing(Frame = all_frames) %>%
  left_join(all_data,
            by = c("File", "Corpus", "Frame")) %>%
  mutate(
    Totale_occorrenze_frame = ifelse(
      is.na(Totale_occorrenze_frame),
      0,
      Totale_occorrenze_frame
    )
  )

############################################################
# 4. FILTER: frame must appear in ≥ 5 texts per corpus
############################################################

frame_by_corpus <- complete_data %>%
  group_by(Frame, Corpus) %>%
  summarise(
    n_texts = sum(Totale_occorrenze_frame > 0),
    .groups = "drop"
  )

valid_frames <- frame_by_corpus %>%
  group_by(Frame) %>%
  filter(all(n_texts >= 5)) %>%
  pull(Frame) %>%
  unique()

analysis_data <- complete_data %>%
  filter(Frame %in% valid_frames)

############################################################
# 5. OPTIONAL: stability check (highly recommended)
############################################################

frame_stats <- analysis_data %>%
  group_by(Frame) %>%
  summarise(
    n_nonzero = sum(Totale_occorrenze_frame > 0),
    total = sum(Totale_occorrenze_frame),
    .groups = "drop"
  )

valid_frames <- frame_stats %>%
  filter(n_nonzero >= 20) %>%   # stabilità minima per Poisson
  pull(Frame)

analysis_data <- analysis_data %>%
  filter(Frame %in% valid_frames)

############################################################
# 6. POISSON GLM FUNCTION
############################################################

run_poisson <- function(data, ref_level) {
  
  data <- data %>%
    mutate(Corpus = factor(Corpus, levels = ref_level))
  
  results <- data %>%
    group_by(Frame) %>%
    group_map(~{
      
      frame_id <- unique(.y$Frame)
      
      model <- tryCatch(
        glm(
          Totale_occorrenze_frame ~ Corpus +
            offset(log(TotalFramesText)),
          family = poisson(),
          data = .x
        ),
        error = function(e) return(NULL)
        
      )
      
      if (is.null(model)) return(NULL)
      
      coef_table <- summary(model)$coefficients
      
      get_row <- function(term, label) {
        if (!(term %in% rownames(coef_table))) return(NULL)
        
        beta <- coef_table[term, "Estimate"]
        se   <- coef_table[term, "Std. Error"]
        z    <- coef_table[term, "z value"]
        p    <- coef_table[term, "Pr(>|z|)"]
        
        data.frame(
          Frame = frame_id,
          Contrast = label,
          IRR = exp(beta),
          CI_low = exp(beta - 1.96 * se),
          CI_high = exp(beta + 1.96 * se),
          z = z,
          p = p
        )
      }
      
      bind_rows(
        get_row("CorpusBN", "BN vs ref"),
        get_row("CorpusControl", "Control vs ref")
      )
      
    }) %>%
    bind_rows()
  
  return(results)
}

############################################################
# 7. RUN MODELS
############################################################

results_AN <- run_poisson(
  analysis_data,
  ref_level = c("AN", "BN", "Control")
)

results_BN <- run_poisson(
  analysis_data,
  ref_level = c("BN", "AN", "Control")
)

#####rename contrsts
results_AN <- results_AN %>%
  mutate(
    Contrast = case_when(
      Contrast == "BN vs ref" ~ "BN vs AN",
      Contrast == "Control vs ref" ~ "Control vs AN",
      TRUE ~ Contrast
    )
  )

results_BN <- results_BN %>%
  mutate(
    Contrast = case_when(
      Contrast == "Control vs ref" ~ "Control vs BN",
      TRUE ~ Contrast
    )
  )

############################################################
# 8. MERGE RESULTS
############################################################

results_all <- bind_rows(
  results_AN,
  results_BN
)

############################################################
# 9. MULTIPLE TEST CORRECTION
############################################################

results_all <- results_all %>%
  mutate(
    p_FDR = p.adjust(p, method = "bonferroni")
  )

############################################################
# 10. SIGNIFICANT RESULTS
############################################################

results_sig <- results_all %>%
  filter(p_FDR < .05)

############################################################
# 11. QUICK SUMMARY
############################################################

results_all %>%
  group_by(Contrast) %>%
  summarise(
    total = n(),
    significant = sum(p_FDR < 0.05),
    .groups = "drop"
  )



######################################
#Table
###################
table_frames <- results_sig %>%
  mutate(
    IRR = round(IRR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    z = round(z, 2),
    p_FDR = ifelse(
      p_FDR < .001,
      "< .001",
      sprintf("%.3f", p_FDR)
    ),
    `95% CI` = paste0(
      "[",
      CI_low,
      ", ",
      CI_high,
      "]"
    )
  ) %>%
  select(
    Frame,
    Contrast,
    IRR,
    `95% CI`,
    z,
    p_FDR
  ) %>%
  arrange(
    Contrast,
    Frame
  )

###export
library(flextable)
library(officer)

doc <- read_docx()

ft_frames <- flextable(table_frames) %>%
  autofit()

doc <- body_add_par(
  doc,
  "Table S3",
  style = "heading 1"
)

doc <- body_add_par(
  doc,
  "Significant Frame-Level Corpus Contrasts From Poisson Regression Models",
  style = "Normal"
)

doc <- body_add_flextable(
  doc,
  ft_frames
)

doc <- body_add_par(
  doc,
  paste(
    "Note. Only statistically significant contrasts are shown.",
    "Incidence Rate Ratios (IRR) are reported with 95% confidence intervals.",
    "P-values were adjusted using the Benjamini–Hochberg false discovery rate procedure",
    "across all frame-level comparisons."
  ),
  style = "Normal"
)

print(
  doc,
  target = "Frame_Contrasts_APA.docx"
)

########################################################
#Forest Plot
######################################################
#prepare data fro plotting
library(dplyr)
library(ggplot2)
library(forcats)

plot_data <- results_sig %>%
  mutate(
    IRR = as.numeric(IRR),
    CI_low = as.numeric(CI_low),
    CI_high = as.numeric(CI_high),
    logIRR = log(IRR)
  ) %>%
  group_by(Contrast) %>%
  arrange(desc(abs(logIRR)), .by_group = TRUE) %>%
  mutate(Frame_ordered = fct_inorder(Frame)) %>%
  ungroup()



#add percentage interpretation to the plot
plot_data <- plot_data %>%
  mutate(
    percent_change = ifelse(
      IRR > 1,
      (IRR - 1) * 100,
      (1 - IRR) * -100
    ),
    percent_label = paste0(
      ifelse(percent_change > 0, "+", ""),
      round(percent_change, 1),
      "%"
    )
  )


## plot

ggplot(plot_data, aes(x = IRR, y = Frame_ordered)) +
  
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "grey40"
  ) +
  
  geom_errorbarh(
    aes(xmin = CI_low, xmax = CI_high),
    height = 0.2,
    linewidth = 0.6,
    color = "grey30"
  ) +
  
  geom_point(
    color = "darkgreen",
    size = 2.5
  ) +
  
  geom_text(
    aes(label = percent_label),
    hjust = -0.2,
    nudge_y = 0.25,
    size = 3
  ) +
  
  scale_x_log10() +
  
  facet_wrap(
    ~ Contrast,
    scales = "free_y"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  
  labs(
    x = "Incidence Rate Ratio (log scale)",
    y = "Semantic Frame"
  )

##############
plot_data <- plot_data %>%
  mutate(
    label_x = CI_high * 1.05
  )

ggplot(plot_data, aes(x = IRR, y = Frame_ordered)) +
  
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "grey40"
  ) +
  
  geom_errorbarh(
    aes(xmin = CI_low, xmax = CI_high),
    height = 0.2,
    linewidth = 0.6,
    color = "grey30"
  ) +
  
  geom_point(
    color = "darkgreen",
    size = 2.5
  ) +
  
  geom_text(
    aes(
      x = label_x,
      label = percent_label
    ),
    hjust = 0,
    size = 3
  ) +
  
  scale_x_log10(
    expand = expansion(mult = c(0.05, 0.25))
  ) +
  
  facet_wrap(~ Contrast, scales = "free_y") +
  
  theme(
  axis.text.y = element_text(size = 10)
  
  )
