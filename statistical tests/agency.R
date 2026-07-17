
#We want to test the hypothesis that corpus has an effect on the 
#distribution of syntactic constructions on specific frames (namely those related to agency)


library(readxl)
library(dplyr)
library(tidyr)
library(glmmTMB)
library(purrr)

# Load data
AN <- read_excel("/Users/.../frames_AN.xlsx") %>%
  mutate(Corpus = "AN")

BN <- read_excel("/Users/.../frames_BN.xlsx") %>%
  mutate(Corpus = "BN")

Control <- read_excel("/Users/.../frames_Control.xlsx") %>%
  mutate(Corpus = "Control")

# Combine datasets
all_data <- bind_rows(AN, BN, Control)

# Keep only relevant columns
all_data <- all_data %>%
  select(
    File,
    Corpus,
    Frame,
    Totale_occorrenze_frame,
    Costruzioni_passive,
    Passive_con_agente,
    Costruzioni_attive,
    Costruzioni_nanticausative,
    Impersonali_e_nominali
  )

#add column with total occurrences acroos the syntactic constructions i need
all_data <- all_data %>%
  mutate(
    FrameCount_Model =
      Costruzioni_passive +
      Passive_con_agente +
      Costruzioni_attive +
      Costruzioni_nanticausative +
      Impersonali_e_nominali
  )

#selected frames
frames_to_check <- c(
  "Intentionally_act", "Causation", "Deciding", "Capability", "Control", "Addiction", "Successful_action", 
  "Seeking_to_achieve", "Intentionally_affect", "Success_or_failure", "Ingestion", "Ingest_substance",
  "Biological_urge", "Substance", "Opinion", "Exercising",  "Leadership", "Cure", 
  "Possibility", "Certainty", "Level_of_force_exertion", "Cause-harm", 
  "Cause_change", "Cause_change_of_position_on_a_scale", "Dead_or_alive", "Conduct", "Compliance", 
  "Excreting","Purpose", "Choosing", "Intoxication", "Earnings_and_losses", 
  "Body_movement", "Undergo_change", "Avoiding", "Process_start", "Undergoing", 
  "Seeking", "Finish_competition", "Death", "Personal_success", "Escaping", "Intentional_traversing", 
  "Prevarication", "Cause_emotion", "Beat_opponent", 
   "Recovery", "Rewards_and_punishments", "Resolve_problem", 
  "Being_obligated", "Cause_to_perceive", "Experience_bodily_harm", "Accomplishment", 
   "Cause_to_make_progress", "Being_in_control", "Emptying", "Losing",
  "Responsibility", "Agree_or_refuse_to_act", "Atonement", "Dominate_competitor", "Food_gathering", 
   "Destroying", "Interrupt_process", "Fullness"
)

subset_data <- all_data %>%
  filter(Frame %in% frames_to_check)

#diagnostics
frame_diagnostics <- subset_data %>%
  group_by(Frame) %>%
  summarise(
    texts_total = sum(FrameCount_Model > 0),
    texts_AN = sum(FrameCount_Model > 0 & Corpus == "AN"),
    texts_BN = sum(FrameCount_Model > 0 & Corpus == "BN"),
    texts_Control = sum(FrameCount_Model > 0 & Corpus == "Control"),
    total_count = sum(FrameCount_Model),
    count_AN = sum(FrameCount_Model[Corpus == "AN"]),
    count_BN = sum(FrameCount_Model[Corpus == "BN"]),
    count_Control = sum(FrameCount_Model[Corpus == "Control"]),
    .groups = "drop"
  )

#apply inclusion criteria
frame_diagnostics <- frame_diagnostics %>%
  mutate(
    sufficient_AN = texts_AN >= 5,
    sufficient_BN = texts_BN >= 5,
    sufficient_Control = texts_Control >= 5,
    meets_all_criteria =
      
      sufficient_AN &
      sufficient_BN &
      sufficient_Control
  )

View(frame_diagnostics)


#EXTRACT VALID FRAMES TO RUN MODEL
valid_frames <- frame_diagnostics %>%
  filter(meets_all_criteria) %>%
  pull(Frame)

valid_frames

####################################
#Run model
#####################################

run_frame_model <- function(frame_name, data) {
  
  frame_data <- data %>%
    filter(Frame == frame_name & FrameCount_Model > 0)
  
  # Se troppo pochi dati, salta
  #if(nrow(frame_data) < 10) return(NULL)
  
  frame_long <- frame_data %>%
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
  
  frame_long <- frame_long %>%
    group_by(Construction) %>%
    filter(sum(Count) > 0) %>%
    ungroup()
  
  
  model <- tryCatch(
    glmmTMB(
      Count ~ Corpus * Construction +
        offset(log(FrameCount_Model)) +
        (1 | File),
      family = poisson,
      data = frame_long
    ),
    error = function(e) return(NULL)
  )
  
  if(is.null(model)) return(NULL)
  
  model_no_int <- update(model, . ~ . - Corpus:Construction)
  
  lrt <- anova(model_no_int, model, test = "Chisq")
  
  data.frame(
    Frame = frame_name,
    Chi_square = lrt$Chisq[2],
    df = lrt$`Chi Df`[2],
    p_value = lrt$`Pr(>Chisq)`[2]
  )
}

# ----------------------------
# run on all valid frames
# ----------------------------
results_frames <- map_dfr(
  valid_frames,
  run_frame_model,
  data = all_data
)

# ----------------------------
# multiple-test correction
# ----------------------------
results_frames <- results_frames %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")  # FDR più appropriato di Bonferroni
  )


results_frames

########################################################################################
#fit negative binomial models to then compare with poisson and see if something changes
#######################################################################################
run_frame_model_nb <- function(frame_name, data) {
  
  frame_data <- data %>%
    filter(Frame == frame_name & FrameCount_Model > 0)
  
  frame_long <- frame_data %>%
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
  
  frame_long <- frame_long %>%
    group_by(Construction) %>%
    filter(sum(Count) > 0) %>%
    ungroup()
  
  model <- tryCatch(
    glmmTMB(
      Count ~ Corpus * Construction +
        offset(log(FrameCount_Model)) +
        (1 | File),
      family = nbinom2,
      data = frame_long
    ),
    error = function(e) return(NULL)
  )
  
  if(is.null(model)) return(NULL)
  
  model_no_int <- update(model, . ~ . - Corpus:Construction)
  
  lrt <- anova(model_no_int, model, test = "Chisq")
  
  data.frame(
    Frame = frame_name,
    Chi_square = lrt$Chisq[2],
    df = lrt$`Chi Df`[2],
    p_value = lrt$`Pr(>Chisq)`[2]
  )
}

##run on alid_frames
results_frames_nb <- map_dfr(
  valid_frames,
  run_frame_model_nb,
  data = all_data
)

#FDR corrwction
results_frames_nb <- results_frames_nb %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )

##compare poisson and nb
pois <- results_frames %>%
  rename(
    p_pois = p_value,
    p_adj_pois = p_adj
  )

nb <- results_frames_nb %>%
  rename(
    p_nb = p_value,
    p_adj_nb = p_adj
  )

#comparison
comparison <- pois %>%
  select(Frame, p_pois, p_adj_pois) %>%
  left_join(
    nb %>%
      select(Frame, p_nb, p_adj_nb),
    by = "Frame"
  ) %>%
  mutate(
    sig_pois = p_adj_pois < .05,
    sig_nb = p_adj_nb < .05
  )

View(comparison)
