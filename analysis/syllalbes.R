#' title: "Stress Patterns in Intra-word Code-switching"
#' subtitle: "AMP 2025 Proceedings Paper: Final Analysis of Stress Dataset"
#' author: "Moldir Baidildinova"

knitr::opts_chunk$set(echo = TRUE)

#' # Introduction
#' This document presents the data processing and full analysis of syllable duration, intensity, and f0 across different languages (Kazakh, Russian) and modes (Code-Switching) using R.
#' 
#' **Linguistic Phenomenon:** Kazakh-Russian intra-word code-switching: *[[Russian Noun] + [Kazakh Noun suffix]]*
#' 
#' **Working RQ:**	How the stress patterns of two languages interact in word-internal shifts: 
#' whether the addition of Kazakh suffixes to Russian noun stems affects (shifts) the stress 
#' pattern (to the last syllable), consistent with Kazakh phonology.
#' 
#' **Working predictions:**
#' 
#' (A) Stress follows Kazakh rules: Russian words are treated like Kazakh words when suffixed, meaning stress is assigned according to Kazakh stress rules, likely resulting in final-syllable stress.
#' 
#'   - **If this prediction is true:** 
#'       - significant difference in in s1:s2 (reka) of unsuffixed vs s1:s2. (rekalar) of suffixed CS tokens, with suffixed closer to 1. 
#'       - sig diff in s1:s2:s3 (suffixed CS tokens:rekalar) in favor of s3.
#'       - no significant diff in s3(CS):s3(Kazakh).
#' 
#' (B) Stress remains fixed on the root: Russian words maintain the same stress pattern as their 
#' unsuffixed forms. Stress is determined by the root and does not shift when Kazakh suffixes are added.
#' 
#'   - **If this prediction is true:** 
#'       - no significant diff in s1:s2 (reka) of unsuffixed vs s1:s2. (rekalar) of suffixed CS tokens.
#'       - significant diff in s1:s2:s3 (suffixed CS tokens:rekalar) in favor of stress fixed on the root. 
#'       - significant diff in s3(CS):s3(Kazakh) in favor of Kazakh.
#'       
#' (C) A mix of Russian and Kazakh stress: Russian words exhibit characteristics of both languages. The original Russian stress location may remain, but an additional Kazakh-style final stress may also emerge.
#' 
#'   - **If this prediction is true:**
#'       - no significant diff in in s1:s2 (reka) of unsuffixed vs s1:s2 (rekalar) of suffixed CS tokens.
#'       - no significant diff in s3(CS):s3(Kazakh). 
#' 
#' 
#'     
#' (D) Stress follows Russian suffixation rules: Russian words behave as though they have Russian suffixes, 
#' meaning stress is assigned based on Russian stress patterns for the entire word. Some words may shift stress,
#'  while others retain their original placement.
#' 
#'   - **If this prediction is true:** 
#'       - (1) Russian roots with *mobile* stress:
#'         - significant diff in s1:s2 (gorod) of unsuffixed vs s1:s2. (gorodtar) of suffixed CS tokens.
#'         - significant diff in s1:s2:s3 (suffixed CS tokens:gorodtar) in favor of s3.
#'       - (2) for *immobile* roots:
#'         - no significant diff in s1:s2 (kniga) of unsuffixed vs s1:s2. (knigalar) of suffixed CS tokens.
#'         - significant diff in s1:s2:s3 (suffixed CS tokens:knigalar) in favor of root stress (s1, s2).   
#'  
#'   
#' # Data Aggregation

#' ## Dataset Loading and Restructuring

library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(modelr)
library(readr)
library(tidyr)
library(patchwork)
library(lme4)
library(lmerTest)

df_full_sample <- read_csv("/Users/moldir/Documents/Fall_2024_IndStudy/CompletedAnnotations/final_df.csv")

# Update the dataframe by adding new 'Speaker' and 'Gender' columns.
# 'Speaker' extracts the "Speaker_#" part,
# and 'Gender' extracts either "male" or "female"
df_full_sample <- df_full_sample %>%
  mutate(
    Speaker = str_extract(Filename, "^(Speaker_\\d+)"),
    Gender  = str_extract(Filename, "(male|female)")
  )

# Reorganize df
df_full_sample <- df_full_sample %>% select(1:1, Speaker, Gender, everything())
#df_speaker1 <- df_speaker1 %>% rename(Participant = Filename) %>%
#mutate (Participant = substr(Participant, 1,9))


# Separate 'Annotation' column into multiple new columns using '_' as delimiter
# s2_cv_da_stressed
df_full_sample <- df_full_sample %>% 
  separate(Annotation, into = c("SyllPos", "SyllStr", "SyllIPA", "Stress"), sep = "_", fill = "right")

# Create new columns for pause and hg values 
# Create "HasPause" column: TRUE only where Stress == "pause", "" elsewhere
df_full_sample$HasPause <- ifelse(df_full_sample$Stress == "pause", "TRUE", "")

# Create "HasGeminates" column: TRUE only where Stress == "hg", """ elsewhere
df_full_sample$HasGeminates <- ifelse(df_full_sample$Stress == "hg", "TRUE", "")

# Replace "pause" and "hg" with "" in Stress column
df_full_sample$Stress[df_full_sample$Stress %in% c("pause", "hg")] <- ""

view(df_full_sample)
# Update Stress column for Kaz, RUS and CS tokens
# Stress: un/stressed = RUS (all syllables); 
# CS (s1/s2) = un/stressed, CS s3 = NA (bc Kaz suffix); 
# Kaz (all syllables) = NA
# So, different levels of stress for each language

df_full_sample <- df_full_sample %>%
  mutate(
    Stress = case_when(
      # All Kaz tokens should have NA
      Language == "Kaz" ~ NA_character_,
      
      # CS tokens: s1 or s2 - fill NA or "" with "unstressed"
      Language == "CS" & SyllPos %in% c("s1", "s2") & (is.na(Stress) | Stress == "") ~ "unstressed",
      
      # CS tokens: s3 should always be NA
      Language == "CS" & SyllPos == "s3" ~ NA_character_,
      
      # Rus tokens: fill NA or "" with "unstressed"
      Language == "Rus" & (is.na(Stress) | Stress == "") ~ "unstressed",
      
      # Else keep existing value
      TRUE ~ Stress
    )
  )


#' 
#' 
#' ## Normalize Continuous Values (z-score)
#' 
## ----echo = FALSE-------------------------------------------------------------
df_full_sample <- df_full_sample %>%
  mutate(
    Duration_in_ms = as.numeric(Duration_in_ms),
    Mean_dB        = as.numeric(Mean_dB),
    MeanF0         = as.numeric(MeanF0),
    Max_dB         = as.numeric(Max_dB),
    Min_dB         = as.numeric(Min_dB),
    MaxF0Hz        = as.numeric(MaxF0Hz),
    MinF0Hz        = as.numeric(MinF0Hz)
  )

# Then scale the normalized columns
df_full_sample <- df_full_sample %>%
  mutate(
    NormDur      = as.numeric(scale(Duration_in_ms)),
    NormInt      = as.numeric(scale(Mean_dB)),
    NormF0       = as.numeric(scale(MeanF0)),
    NormMax_dB   = as.numeric(scale(Max_dB)),
    NormMin_dB   = as.numeric(scale(Min_dB)),
    NormMaxFOHz  = as.numeric(scale(MaxF0Hz)),
    NormMinFOHz  = as.numeric(scale(MinF0Hz))
  )


#print(df_full_sample)
view(df_full_sample)

#write_csv(df_full_sample,"/Users/aidyn/Documents/Fall_2024_IndStudy/CompletedAnnotations/stress_dataset.csv")
#write.csv(df_full_sample, "stress_dataset.csv", fileEncoding = "UTF-8", row.names = FALSE)


# Classify SyllStr into SyllType
df_full_sample <- df_full_sample %>%
  mutate(SyllType = case_when(
    SyllStr %in% c("cvc", "ccvc", "cvcc") ~ "heavy",
    SyllStr %in% c("cv", "vc", "ccv", "cccv", "v") ~ "non_heavy",
    TRUE ~ NA_character_  
  ))%>%
  drop_na(Duration_in_ms, SyllStr,SyllType, Language)


# Then plot using the new SyllType column
# ggplot(df_full_sample) +
#   geom_boxplot(aes(x = SyllType, y = Duration_in_ms, color = Language), show.legend = TRUE) +
#   #facet_wrap(~ SyllType) +
#   xlab("Syllable Type") +
#   ylab("Duration (ms)") +
#   ggtitle("Syllable Duration by Language and Syllable Type") +
#   theme_classic()

# Save full ds
#write_csv(df_full_sample,"/Users/aidyn/Documents/Fall_2024_IndStudy/CompletedAnnotations/stress_dataset.csv")



#' *OBSERVATION:*
 
#' Russian heavy and non-heavy syllables tend be longer than Kazakh and CS tokens. 
 
#' ## Syllable Duration by Suffix case
# Plotted by colors
ggplot(df_full_sample %>% 
         drop_na(SuffixCase) %>%
         filter(SyllPos == "s3", Language %in% c("Kaz", "CS")))  +
  geom_boxplot(aes(x = SuffixCase, y = Duration_in_ms, color = Language), show.legend = TRUE) +
  #scale_color_manual(values = c(Kaz = "black", CS = "grey40")) +
  #facet_wrap(~Language) +
  xlab("Suffix Case") +
  ylab("Duration (ms)") +
  ggtitle("Syllable Duration by Suffix Case and Language") +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# For the Proceedings paper: black%white only 
ggplot(df_full_sample %>% 
         drop_na(SuffixCase) %>%
         filter(SyllPos == "s3", Language %in% c("Kaz", "CS")),
       aes(x = SuffixCase, y = NormDur)) +
  geom_boxplot(
    aes(fill = Language),
    color = "black",                        # black outlines
    position = position_dodge(width = 0.75),
    width = 0.6,
    outlier.shape = 21,                     # readable outliers
    outlier.color = "black",
    outlier.fill = "white",
    outlier.stroke = 0.3
  ) +
  scale_fill_manual(values = c(Kaz = "white", CS = "grey70")) +
  xlab("Suffix Case") +
  ylab("Duration (z-score)") +
  #ggtitle("Syllable Duration by Suffix Case and Language") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


  
  
#' *OBSERVATION:*
#' There is a notable difference between DAT, GEN and Plural suffix durations in Kazakh and CS tokens. 


#' ## Syllable Duration 
#' ## Kazakh tokens: Compare syllable durations by inflection status


library(forcats)

# Filter Kazakh tokens, exclude NA, recode syllables
df_kazakh <- df_full_sample %>%
  filter(Language == "Kaz") %>%
  drop_na(Duration_in_ms, SyllPos, Language, WordForm) %>%
  mutate(WordForm = fct_relevel(WordForm, "uninflected", "inflected")) 
    # , SyllPos %in% c("s1", "s2", "s3")
  

# Plot
ggplot(df_kazakh, aes(x = SyllPos, y = Duration_in_ms, fill = WordForm)) +
  geom_boxplot(position = position_dodge()) +
 facet_wrap(~ WordForm, scales = "free_x") +
  theme_classic() +
  xlab("Syllable Position") +
  ylab("Duration (ms)") +
  # ggtitle("Kazakh Syllable Duration by WordForm and Syllable Position") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#' ## CS tokens: Syllable duration by stress


df_cs <- df_full_sample %>%
  drop_na(Duration_in_ms, SyllPos, Stress) %>%
  filter(Language == "CS", SyllPos %in% c("s1", "s2")) 

ggplot(df_cs, aes(x = Stress, y = Duration_in_ms, fill = Stress)) +
  geom_boxplot() +
  facet_wrap(~ SyllPos) +
  theme_classic() +
  xlab("Stress") +
  ylab("Duration (ms)") +
  ggtitle("CS Token Duration by Stress and Syllable Position") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#' ### CS tokens:  Stress & WordForm interaction


df_cs_wf <- df_cs %>%
  drop_na(WordForm)

ggplot(df_cs_wf, aes(x = Stress, y = Duration_in_ms, fill = Stress)) +
  geom_boxplot() +
  facet_grid(SyllPos ~ WordForm) +
  theme_classic() +
  xlab("Stress") +
  ylab("Duration (ms)") +
  ggtitle("CS Syllable Duration by Stress and WordForm") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#' ### Compare s3 duration for Kazakh vs CS tokens

df_s3 <- df_full_sample %>%
  drop_na(Duration_in_ms) %>%
  filter(SyllPos == "s3", Language %in% c("Kaz", "CS"))

ggplot(df_s3, aes(x = Language, y = Duration_in_ms, fill = Language)) +
  geom_boxplot() +
  theme_classic() +
  xlab("Language") +
  ylab("Duration (ms)") +
  ggtitle("S3 Duration in Kazakh vs CS Tokens") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#' *OBSERVATION:*
#' S3 has comparable duration despite language context. 
#' 
#' ### Russian tokens: Syllable duration by stress

df_rus <- df_full_sample %>%
  drop_na(Duration_in_ms, Stress) %>%
  filter(Language == "Rus") # %>% , SyllPos %in% c("s1", "s2", "s3")
  # mutate(SyllPos = recode(SyllPos, "s1" = "initial", "s2" = "medial", "s3" = "final"))

ggplot(df_rus, aes(x = Stress, y = Duration_in_ms, fill = Stress)) +
  geom_boxplot() +
  facet_wrap(~ SyllPos) +
  theme_classic() +
  xlab("Stress") +
  ylab("Duration (ms)") +
  ggtitle("Russian Syllable Duration by Stress and Syllable Position") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#' *OBSERVATION:* Russian stressed syllables tend to be longer in all positions. 
 
#' # Statistical Analyses
 

#' # Checking Prior Assumptions (Reports)
 
#' ## Kazakh: is stress default final?


## Plot Kazakh syllable by Position and Word Form
kz_all_syll <- df_full_sample %>%
  filter(Language == "Kaz") %>%
 filter(!is.na(SyllPos))

#kz_all_syll

# Plot with error bars == duration of s1, s2, s3 by WordForm

summary_kz_all <- kz_all_syll %>%
  group_by(SyllPos, WordForm) %>%
  summarise(
    mean_dur = mean(Duration_in_ms, na.rm = TRUE),
    sd_dur = sd(Duration_in_ms, na.rm = TRUE),

    mean_dB = mean(Mean_dB, na.rm = TRUE),
    sd_dB = sd(Mean_dB, na.rm = TRUE),

    mean_f0 = mean(MeanF0, na.rm = TRUE),
    sd_f0 = sd(MeanF0, na.rm = TRUE),

    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se_dur = sd_dur / sqrt(n),
    se_dB = sd_dB / sqrt(n),
    se_f0 = sd_f0 / sqrt(n)
  )


# Fill in missing combinations with NA 
summary_kz_all_complete <- summary_kz_all %>%
  complete(SyllPos, WordForm, fill = list(
    mean_dur = NA,
    se_dur = NA,
    mean_dB = NA,
    se_dB = NA,
    mean_f0 = NA,
    se_f0 = NA
  ))


# Duration
# with color 
kaz_dur <- ggplot(summary_kz_all_complete, aes(x = SyllPos, y = mean_dur, fill = WordForm)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_dur - se_dur, ymax = mean_dur + se_dur),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    x = "Syllable Position",
    y = "Mean Duration (ms)",
    fill = "Word Form"
  ) +
  theme_minimal(base_size = 14)

# black and white

kaz_dur <- ggplot(summary_kz_all_complete,
                  aes(x = SyllPos, y = mean_dur, fill = WordForm)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"          # black outlines
  ) +
  geom_errorbar(
    aes(ymin = mean_dur - se_dur, ymax = mean_dur + se_dur),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  scale_fill_grey(start = 1, end = 0.3) +  # white -> dark grey
  labs(
    x = "Syllable Position",
    y = "Mean Duration (ms)",
    fill = "Word Form"
  ) +
  theme_bw(base_size = 14)

kaz_dur

kaz_dur

# ggsave("kz_duration_plot.png", plot = kaz_dur, width = 8, height = 6, dpi = 300, bg = "white")

# Plot for Mean_dB
# with colors 
kaz_db <- ggplot(summary_kz_all_complete, aes(x = SyllPos, y = mean_dB, fill = WordForm)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
   geom_errorbar(
    aes(ymin = mean_dB - se_dB, ymax = mean_dB + se_dB),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    x = "Syllable Position",
    y = "Mean Intensity (dB)",
    fill = "Word Form"
  ) +
  theme_minimal(base_size = 14) # don't need to put size with ggsave 

# black&white
kaz_db <- ggplot(summary_kz_all_complete,
                 aes(x = SyllPos, y = mean_dB, fill = WordForm)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  geom_errorbar(
    aes(ymin = mean_dB - se_dB, ymax = mean_dB + se_dB),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  scale_fill_grey(start = 1, end = 0.3) +   # white -> dark grey
  labs(
    x = "Syllable Position",
    y = "Mean Intensity (dB)",
    fill = "Word Form"
  ) +
  theme_bw(base_size = 14)

kaz_db


# Save the Mean_dB plot
# ggsave("kz_intensity_plot.png", plot = kaz_db, width = 8, height = 6, dpi = 300, bg = "white")

# Plot for MeanF0
kaz_f0 <- ggplot(summary_kz_all_complete, aes(x = SyllPos, y = mean_f0, fill = WordForm)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_f0 - se_f0, ymax = mean_f0 + se_f0),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    x = "Syllable Position",
    y = "Mean f0 (Hz)",
    fill = "Word Form"
  ) +
  theme_minimal(base_size = 14)

# black&white

kaz_f0 <- ggplot(summary_kz_all_complete,
                 aes(x = SyllPos, y = mean_f0, fill = WordForm)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  geom_errorbar(
    aes(ymin = mean_f0 - se_f0, ymax = mean_f0 + se_f0),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  scale_fill_grey(start = 1, end = 0.3) +  # white -> dark grey
  labs(
    x = "Syllable Position",
    y = "Mean f0 (Hz)",
    fill = "Word Form"
  ) +
  theme_bw(base_size = 14)

kaz_f0


# Save the MeanF0 plot
# ggsave("kz_f0_plot.png", plot = kaz_f0, width = 8, height = 6, dpi = 300, bg = "white")

# Horizontal panel with A, B, C annotations
# Remove x-axis labels from first two plots
kaz_dur_clean <- kaz_dur + theme(axis.title.x = element_blank())
kaz_f0_clean <- kaz_f0 + theme(axis.title.x = element_blank())

# Combine into panel
kaz_panel_horizontal <- kaz_dur_clean + kaz_db +kaz_f0_clean + 
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(tag_levels = 'A')

# Save panel
# ggsave("kaz_panel_horizontal.png", plot = kaz_panel_horizontal, width = 12, height = 5, dpi = 300, bg = "white")
kaz_panel_horizontal

# BW: syllable type
#, WordForm
summary_kz_all <- kz_all_syll %>%
  mutate(
    WordForm = fct_relevel(WordForm, "uninflected", "inflected")  
  ) %>%
  group_by(SyllPos, SyllType) %>%
  summarise(
    mean_dur = mean(Duration_in_ms, na.rm = TRUE),
    sd_dur = sd(Duration_in_ms, na.rm = TRUE),
    
    mean_dB = mean(Mean_dB, na.rm = TRUE),
    sd_dB = sd(Mean_dB, na.rm = TRUE),
    
    mean_f0 = mean(MeanF0, na.rm = TRUE),
    sd_f0 = sd(MeanF0, na.rm = TRUE),
    
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se_dur = sd_dur / sqrt(n),
    se_dB = sd_dB / sqrt(n),
    se_f0 = sd_f0 / sqrt(n)
  )

kaz_dur_type <- ggplot(summary_kz_all,
                  aes(x = SyllPos, y = mean_dur, fill = SyllType)) +
  #facet_wrap(~WordForm) + 
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"          # black outlines
  ) +
  geom_errorbar(
    aes(ymin = mean_dur - se_dur, ymax = mean_dur + se_dur),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  scale_fill_grey(start = 1, end = 0.3) +  # white -> dark grey
  labs(
    x = "Syllable Position",
    y = "Mean Duration (ms)",
    fill = "SyllType"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )


kaz_dur_type 


## Kazakh: Reliable correlates of stress
roots_kaz <- kz_all_syll %>%
  filter(WordForm == "uninflected")

suffixed_kaz <- kz_all_syll %>%
  filter(WordForm == "inflected")

model_roots_dur <- lmer(Duration_in_ms ~ SyllPos + (1 | Speaker) + (1 | Word), data = roots_kaz)
summary(model_roots_dur)

model_suffixed_dur <- lmer(Duration_in_ms ~ SyllPos + (1 | Speaker) + (1 | Word), data = suffixed_kaz)
summary(model_suffixed_dur)

model_roots_int <- lmer(Mean_dB ~ SyllPos + (1 | Speaker) + (1 | Word), data = roots_kaz)
summary(model_roots_int)

model_suffixed_int <- lmer(Mean_dB ~ SyllPos + (1 | Speaker) + (1 | Word), data = suffixed_kaz)
summary(model_suffixed_int)

model_roots_f0 <- lmer(MeanF0 ~ SyllPos + (1 | Speaker) + (1 | Word), data = roots_kaz)
summary(model_roots_f0)

model_suffixed_f0 <- lmer(MeanF0 ~ SyllPos + (1 | Speaker) + (1 | Word), data = suffixed_kaz)
summary(model_suffixed_f0)


# Joint model with root&suffixed forms together
joint_model_dur <- lmer(Duration_in_ms ~ SyllPos*WordForm + (1 | Speaker) + (1 | Word), data =kz_all_syll)
summary(joint_model_dur)

joint_model_int <- lmer(Mean_dB ~ SyllPos*WordForm + (1 | Speaker) + (1 | Word), data =kz_all_syll)
summary(joint_model_int)

joint_model_f0 <- lmer(MeanF0 ~ SyllPos*WordForm + (1 | Speaker) + (1 | Word), data =kz_all_syll)
summary(joint_model_f0)

### New model for Kaz sylltype and pos
model_form_type <- lmer(Duration_in_ms ~ SyllPos*SyllType + (1 | Speaker) + (1 | Word), data = roots_kaz)
summary(model_form_type)

model_form_type_2 <- lmer(Duration_in_ms ~ SyllPos*SyllType + (1 | Speaker) + (1 | Word), data = suffixed_kaz)
summary(model_form_type_2)

model_type_f0 <- lmer(MeanF0 ~ SyllPos*SyllType + (1 | Speaker) + (1 | Word), data = roots_kaz)
summary(model_type_f0)

model_type_f0_2 <- lmer(MeanF0 ~ SyllPos*SyllType + (1 | Speaker) + (1 | Word), data = suffixed_kaz)
summary(model_type_f0_2)

#' **OBSERVATION: Kazakh Stress and its Correlates**
#' 
#' - Reference level - s1: initial syllable in both cases.
#' 
#' 
#' | Dataset      | Acoustic Measure | SyllPos Effect | Estimate | t-value | p-value | Significance |
#' | ------------ | ---------------- | -------------- | -------- | ------- | ------- | ------------ |
#' | **Roots**    | Duration         | s2             | +52.62   | 7.95    | < 0.001 | \*\*\*       |
#' |              | Mean\_dB         | s2             | -0.13    | -0.34   | 0.734   | n.s.         |
#' |              | MeanF0           | s2             | -15.00   | -9.44   | < 0.001 | \*\*\*       |
#' | **Suffixed** | Duration         | s2             | -0.50    | -0.08   | 0.936   | n.s.         |
#' |              | Duration         | s3             | +47.64   | 7.61    | < 0.001 | \*\*\*       |
#' |              | Mean\_dB         | s2             | +1.04    | 2.79    | 0.005   | \*\*         |
#' |              | Mean\_dB         | s3             | +0.25    | 0.67    | 0.506   | n.s.         |
#' |              | MeanF0           | s2             | -1.40    | -1.06   | 0.291   | n.s.         |
#' |              | MeanF0           | s3             | +11.26   | 8.52    | < 0.001 | \*\*\*       |
#' 
#' 
#' 
#' - In uninflected roots, stress appears to fall on the second syllable as shown by increased duration and lower pitch. 
#' 
#' - In inflected forms, stress appears to shift to the suffix, reflected in longer duration and elevated f0 in the final syllable (s3).
#' 
#' - Intensity (Mean_dB) is not a consistent cue across word types and positions, aligning with 
#' previous findings that duration is more robust stress correlate in Kazakh 
#' and the role of pitch needs to be re-assessed.



#' ## Russian: correlates of stress
 

# df_rus
# Filter Russian syllables with valid Stress
rus_all_syll <- df_full_sample %>%
  filter(Language == "Rus", !is.na(Stress))

# Summarize by stress and word form
summary_rus_all <- rus_all_syll %>%
  group_by(Stress, WordForm) %>%
  summarise(
    mean_dur = mean(Duration_in_ms, na.rm = TRUE),
    sd_dur = sd(Duration_in_ms, na.rm = TRUE),
    
    mean_dB = mean(Mean_dB, na.rm = TRUE),
    sd_dB = sd(Mean_dB, na.rm = TRUE),
    
    mean_f0 = mean(MeanF0, na.rm = TRUE),
    sd_f0 = sd(MeanF0, na.rm = TRUE),
    
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se_dur = sd_dur / sqrt(n),
    se_dB = sd_dB / sqrt(n),
    se_f0 = sd_f0 / sqrt(n)
  )

# Prevent NA level from sneaking in
summary_rus_all_complete <- summary_rus_all %>%
  mutate(Stress = as.character(Stress)) %>%
  complete(Stress, WordForm, fill = list(
    mean_dur = NA,
    se_dur = NA,
    mean_dB = NA,
    se_dB = NA,
    mean_f0 = NA,
    se_f0 = NA
  )) %>%
  filter(!is.na(Stress)) %>%
  mutate(Stress = factor(Stress, levels = c("stressed", "unstressed")))  


# Duration Plot
# with color
rus_dur <- ggplot(summary_rus_all_complete, aes(x = Stress, y = mean_dur, fill = WordForm)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_dur - se_dur, ymax = mean_dur + se_dur),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    x = "Stressed Syllable",
    y = "Mean Duration (ms)",
    # fill = "Word Form"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


rus_dur
# black&white 
rus_dur <- ggplot(summary_rus_all_complete,
                  aes(x = Stress, y = mean_dur, fill = Stress)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  geom_errorbar(
    aes(ymin = mean_dur - se_dur, ymax = mean_dur + se_dur),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  scale_fill_grey(start = 1, end = 0.3) +  # white -> dark grey
  labs(
    x = "Stressed Syllable",
    y = "Mean Duration (ms)",
    #fill = "Word Form"
  ) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rus_dur


# ggsave("rus_duration_plot.png", plot = rus_dur, width = 8, height = 6, dpi = 300, bg = "white")

# Intensity Plot
# with color 
rus_db <- ggplot(summary_rus_all_complete, aes(x = Stress, y = mean_dB, fill = WordForm)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_dB - se_dB, ymax = mean_dB + se_dB),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    x = "Stressed Syllable",
    y = "Mean Intensity (dB)",
    fill = "Word Form"
  ) +
  theme_minimal(base_size = 14) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ggsave("rus_intensity_plot.png", plot = rus_db, width = 8, height = 6, dpi = 300, bg = "white")

# black&white 
rus_db <- ggplot(summary_rus_all_complete,
                 aes(x = Stress, y = mean_dB, fill = Stress)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  geom_errorbar(
    aes(ymin = mean_dB - se_dB, ymax = mean_dB + se_dB),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  scale_fill_grey(start = 1, end = 0.3) +  # white -> dark grey
  labs(
    x = "Stressed Syllable",
    y = "Mean Intensity (dB)",
    fill = "Stress"
  ) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rus_db


# F0 Plot
# with color 
rus_f0 <- ggplot(summary_rus_all_complete, aes(x = Stress, y = mean_f0, fill = WordForm)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_f0 - se_f0, ymax = mean_f0 + se_f0),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    x = "Stressed Syllable",
    y = "Mean f0 (Hz)",
    fill = "Word Form"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("rus_f0_plot.png", plot = rus_f0, width = 8, height = 6, dpi = 300, bg = "white")

# black&white 
rus_f0 <- ggplot(summary_rus_all_complete,
                 aes(x = Stress, y = mean_f0, fill = Stress)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  geom_errorbar(
    aes(ymin = mean_f0 - se_f0, ymax = mean_f0 + se_f0),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  scale_fill_grey(start = 1, end = 0.3) +  # white -> dark grey
  labs(
    x = "Stressed Syllable",
    y = "Mean f0 (Hz)",
    fill = "Stress"
  ) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rus_f0


# Horizontal panel with labels A, B, C
rus_dur_clean <- rus_dur + theme(axis.title.x = element_blank())
rus_f0_clean <- rus_f0 + theme(axis.title.x = element_blank())

rus_panel_horizontal <- rus_dur_clean + rus_db + rus_f0_clean +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(tag_levels = 'A')

# ggsave("rus_panel_horizontal.png", plot = rus_panel_horizontal, width = 12, height = 5, dpi = 300, bg = "white")

rus_panel_horizontal 


#' ## Russian: Stress Correlates

## Normalized values
# model_rus_dur <- lmer(NormDur ~ Stress + (1 | Speaker) + (1 | Word), data = rus_all_syll)
# summary(model_rus_dur)
# 
# model_rus_int <- lmer(NormInt ~ Stress + (1 | Speaker) + (1 | Word), data = rus_all_syll)
# summary(model_rus_int)
# 
# model_rus_f0 <- lmer(NormF0 ~ Stress + (1 | Speaker) + (1 | Word), data = rus_all_syll)
# summary(model_rus_f0)

## Raw values for interpretation
model_rus_dur <- lmer(Duration_in_ms ~ Stress + (1 | Speaker) + (1 | Word), data = rus_all_syll)
summary(model_rus_dur)

model_rus_int <- lmer(Mean_dB ~ Stress + (1 | Speaker) + (1 | Word), data = rus_all_syll)
summary(model_rus_int)

model_rus_f0 <- lmer(MeanF0 ~ Stress + (1 | Speaker) + (1 | Word), data = rus_all_syll)
summary(model_rus_f0)

## Interaction between Stress and SyllPos
model_rus_pos_dur <- lmer(Duration_in_ms ~ Stress*SyllPos + (1 | Speaker) + (1 | Word), data = rus_all_syll)
summary(model_rus_pos_dur)

model_rus_pos_int <- lmer(Mean_dB ~ Stress*SyllPos + (1 | Speaker) + (1 | Word), data = rus_all_syll)
summary(model_rus_pos_int)

model_rus_pos_f0 <- lmer(MeanF0 ~ Stress*SyllPos + (1 | Speaker) + (1 | Word), data = rus_all_syll)
summary(model_rus_pos_f0)

## Interaction between Stress and SyllStr
model_rus_str <- lmer(Duration_in_ms ~ Stress*SyllType + (1 | Speaker) + (1 | Word), data = rus_all_syll)
summary(model_rus_str)

model_rus_str_int <- lmer(Mean_dB ~ Stress*SyllType + (1 | Speaker) + (1 | Word), data = rus_all_syll)
summary(model_rus_str_int)

model_rus_str_f0 <- lmer(MeanF0 ~ Stress*SyllType + (1 | Speaker) + (1 | Word), data = rus_all_syll)
summary(model_rus_str_f0)

#' 
#' **OBSERVATION: Russian Stress**
#' 
#' Stress Effects in Russian Words
#' 
#' 
#' | Acoustic Measure | Predictor           | Estimate | t-value | p-value  | Significance |
#' | ---------------- | ------------------- | -------- | ------- | -------- | ------------ |
#' | **Duration**     | Stress (unstressed) | –34.18   | –4.45   |  0.001   | \*\*\*       |
#' | **Intensity**    | Stress (unstressed) | –1.97    | –5.26   |  0.001   | \*\*\*       |
#' | **f0**           | Stress (unstressed) | –8.39    | –3.78   |  0.001   | \*\*\*       |
#' 
#' 
#' 
#' The linear mixed effects models indicate that lexical stress in Russian significantly influences all three acoustic correlates: duration, intensity, and fundamental frequency (f0).
#' 
#' - Duration: Unstressed syllables are, on average, 34.18 ms shorter than stressed ones (p < 0.001), highlighting duration as a robust cue to stress.
#' 
#' - Intensity (Mean dB): Unstressed syllables are 1.97 dB quieter, also statistically significant (p < 0.001), suggesting that loudness is another reliable correlate.
#' 
#' - f0: Unstressed syllables have a significantly lower pitch, 8.39 Hz lower than stressed syllables (p < 0.001), consistent with the expectation that pitch rises under stress.
#' 
#' Together, these results show that duration, intensity, and pitch all significantly differentiate stressed from unstressed syllables in Russian. This supports prior findings that Russian exhibits strong acoustic marking of stress across multiple phonetic dimensions compared to Kazakh. However, these results should be taken by a grain of salt since the participants are not native speakers of Russian despite a high bilingual proficiency. 


# Estimates Marginals Means - model-based predictions rather than raw data averages
library(emmeans)
library(pbkrtest)

### Stress * SyllPos models

## Duration
emm_pos_dur <- emmeans(model_rus_pos_dur, ~ Stress | SyllPos)
pairs(emm_pos_dur, adjust = "tukey")    # stress difference within each SyllPos
emm_pos_dur                             # estimated means

## Intensity
emm_pos_int <- emmeans(model_rus_pos_int, ~ Stress | SyllPos)
pairs(emm_pos_int, adjust = "tukey")
emm_pos_int

## F0
emm_pos_f0 <- emmeans(model_rus_pos_f0, ~ Stress | SyllPos)
pairs(emm_pos_f0, adjust = "tukey")
emm_pos_f0


### Stress * SyllType models

## Duration
# emm_type_dur <- emmeans(model_rus_str, ~ Stress | SyllType)
# pairs(emm_type_dur, adjust = "tukey")
# emm_type_dur
# 
# ## Intensity
# emm_type_int <- emmeans(model_rus_str_int, ~ Stress | SyllType)
# pairs(emm_type_int, adjust = "tukey")
# emm_type_int
# 
# ## F0
# emm_type_f0 <- emmeans(model_rus_str_f0, ~ Stress | SyllType)
# pairs(emm_type_f0, adjust = "tukey")
# emm_type_f0




#' # Hypothesis Testing

#' What this code snippet does:
#' (1) Creates a subset of df for CS and CS&Kazakh tokens.
#' (2) Plots hypotheses A,B,C,D.
#' (3) Runs an lmer() model on the created subsets to check Hs.

#' ## Plot A
 
#' - s1:s2 ratio of uninflected CS tokens vs. s1:s2 ratio of inflected CS tokens.
#' - s1 or s2 is stressed


# Filter for CS tokens with SyllPos s1 or s2
#view(df_full_sample)

# Filter CS words with s1/s2
cs_tokens <- df_full_sample %>%
  filter(Language == "CS", SyllPos %in% c("s1", "s2")) %>%
  group_by(Filename, Word) %>%
  filter(all(c("s1", "s2") %in% SyllPos)) %>%
  ungroup()

# Keep only tokens that appear once per SyllPos (no duplicate s1/s2)
cs_tokens <- cs_tokens %>%
  group_by(Filename, Word, SyllPos) %>%
  filter(n() == 1) %>%
  ungroup()

# Keep these columns untouched
id_cols <- c("Filename", "Speaker", "Gender", "Word", "Word_beg", "Word_end", "Word_dur_ms", 
             "Language", "SuffixCase", "WordForm", "LatinScript", "Gloss", "WordClass", 
             "StressedSyll", "Declension", "NounGender", "StressShift", "ShiftDirect", "AttestedInCS")

# Pivot all columns except for id_cols&SyllPos
pivot_cols <- cs_tokens %>%
  select(-all_of(c(id_cols, "SyllPos"))) %>%
  names()

# Pivot wider
cs_df_wide <- cs_tokens %>%
  pivot_wider(
    id_cols = all_of(id_cols),
    names_from = SyllPos,
    values_from = all_of(pivot_cols),
    names_sep = "_"
  )

# Filter rows where s1 and s2 data are present
cs_df_wide <- cs_df_wide %>%
  filter(
    !is.na(Duration_in_ms_s1) & !is.na(Duration_in_ms_s2),
    !is.na(Mean_dB_s1) & !is.na(Mean_dB_s2),
    !is.na(Max_dB_s1) & !is.na(Max_dB_s2),
    !is.na(MeanF0_s1) & !is.na(MeanF0_s2),
    !is.na(MaxF0Hz_s1) & !is.na(MaxF0Hz_s2)
  )

# Compute ratios
cs_df_wide <- cs_df_wide %>%
  mutate(
    ratio_s1_s2_dur = Duration_in_ms_s1 / Duration_in_ms_s2,
    ratio_mean_int  = Mean_dB_s1 / Mean_dB_s2,
    ratio_max_int   = Max_dB_s1 / Max_dB_s2,
    ratio_mean_f0   = MeanF0_s1 / MeanF0_s2,
    ratio_max_fo    = MaxF0Hz_s1 / MaxF0Hz_s2,
    root            = Word,
    root_stress     = StressedSyll
  )

#view(cs_tokens)
view(cs_df_wide)

# Create the final dataset
cs_roots <- cs_df_wide %>%
  select(Speaker, root, root_stress, WordForm, StressShift, ShiftDirect, ratio_s1_s2_dur, ratio_mean_int, ratio_max_int, ratio_mean_f0, ratio_max_fo)

view(cs_roots)

# Summarize the data
summary_df <- cs_roots %>%
  group_by(root_stress, WordForm) %>%
  summarise(
    mean_ratio_dur  = mean(ratio_s1_s2_dur, na.rm = TRUE),
    sd_ratio_dur    = sd(ratio_s1_s2_dur, na.rm = TRUE),
    
    mean_ratio_int  = mean(ratio_mean_int, na.rm = TRUE),
    sd_ratio_int    = sd(ratio_mean_int, na.rm = TRUE),
    
    mean_ratio_max_int = mean(ratio_max_int, na.rm = TRUE),
    sd_ratio_max_int   = sd(ratio_max_int, na.rm = TRUE),
    
    mean_ratio_f0   = mean(ratio_mean_f0, na.rm = TRUE),
    sd_ratio_f0     = sd(ratio_mean_f0, na.rm = TRUE),
    
    mean_ratio_max_fo = mean(ratio_max_fo, na.rm = TRUE),
    sd_ratio_max_fo   = sd(ratio_max_fo, na.rm = TRUE),
    
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se_ratio_dur  = sd_ratio_dur / sqrt(n),
    se_ratio_int  = sd_ratio_int / sqrt(n),
    se_ratio_max_int = sd_ratio_max_int / sqrt(n),
    se_ratio_f0   = sd_ratio_f0 / sqrt(n),
    se_ratio_max_fo = sd_ratio_max_fo / sqrt(n)
  )
view(summary_df)


# Plot 1: Duration Ratio
cs_ratio_dur <- ggplot(summary_df, aes(x = factor(root_stress), y = mean_ratio_dur, color = WordForm, shape = WordForm)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.4), size = 7) +
  geom_errorbar(
    aes(ymin = mean_ratio_dur - se_ratio_dur, ymax = mean_ratio_dur + se_ratio_dur),
    position = position_dodge(width = 0.4),
    width = 0.2
  ) +
  ylim(.7, 1.3) +
  labs(
    x = "Root Stress",
    y = "Mean Duration Ratio (s1:s2)",
    color = "Word Form",
    shape = "Word Form"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

#print(cs_ratio_dur)
# ggsave("cs_ratio_duration.png", plot = cs_ratio_dur, width = 6, height = 4, dpi = 300, bg = "white")

# Plot 2: Mean Intensity Ratio
cs_ratio_mean_int <- ggplot(summary_df, aes(x = factor(root_stress), y = mean_ratio_int, color = WordForm, shape = WordForm)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.4), size = 7) +
  geom_errorbar(
    aes(ymin = mean_ratio_int - se_ratio_int, ymax = mean_ratio_int + se_ratio_int),
    position = position_dodge(width = 0.4),
    width = 0.2
  ) +
  ylim(.7, 1.3) +
  labs(
    x = "Root Stress Position",
    y = "Mean Intensity Ratio (s1:s2)",
    color = "Word Form",
    shape = "Word Form"
  ) +
#scale_color_manual(values = c("uninflected" = "#2ca02c", "inflected" = "#9467bd")) +
#scale_shape_manual(values = c("uninflected" = 15, "inflected" = 18)) +

  theme_minimal(base_size = 18) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )


#print(cs_ratio_mean_int)
# ggsave("cs_ratio_mean_intensity.png", plot = cs_ratio_mean_int, width = 6, height = 4, dpi = 300, bg = "white")

# Plot 3: Max Intensity Ratio
# cs_ratio_max_int <- ggplot(summary_df, aes(x = factor(root_stress), y = mean_ratio_max_int, color = WordForm, shape = WordForm)) +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
#   geom_point(position = position_dodge(width = 0.4), size = 7) +
#   geom_errorbar(
#     aes(ymin = mean_ratio_max_int - se_ratio_max_int, ymax = mean_ratio_max_int + se_ratio_max_int),
#     position = position_dodge(width = 0.4),
#     width = 0.2
#   ) +
#   ylim(.7, 1.3) +
#   labs(
#     x = "Root Stress Position",
#     y = "Max Intensity Ratio (s1:s2)",
#     color = "Word Form",
#     shape = "Word Form"
#   ) +
#   theme_minimal(base_size = 18) +
#   theme(
#     axis.title = element_text(size = 18),
#     axis.text = element_text(size = 16),
#     legend.title = element_text(size = 16),
#     legend.text = element_text(size = 14)
#   )
# 
# print(cs_ratio_max_int)
# ggsave("cs_ratio_max_intensity.png", plot = cs_ratio_max_int, width = 6, height = 4, dpi = 300)

# Plot 4: Mean F0 Ratio
cs_ratio_mean_f0 <- ggplot(summary_df, aes(x = factor(root_stress), y = mean_ratio_f0, color = WordForm, shape = WordForm)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.4), size = 7) +
  geom_errorbar(
    aes(ymin = mean_ratio_f0 - se_ratio_f0, ymax = mean_ratio_f0 + se_ratio_f0),
    position = position_dodge(width = 0.4),
    width = 0.2
  ) +
  ylim(.7, 1.3) +
  labs(
    x = "Root Stress Position",
    y = "Mean F0 Ratio (s1:s2)",
    color = "Word Form",
    shape = "Word Form"
  ) +
  #scale_color_manual(values = c("uninflected" = "#ff7f0e", "inflected" = "#e377c2")) +
#scale_shape_manual(values = c("uninflected" = 8, "inflected" = 4)) +

  theme_minimal(base_size = 18) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

#print(cs_ratio_mean_f0)
# ggsave("cs_ratio_mean_f0.png", plot = cs_ratio_mean_f0, width = 6, height = 4, dpi = 300, bg = "white")

# Plot 5: Max F0 Ratio
# cs_ratio_max_f0 <- ggplot(summary_df, aes(x = factor(root_stress), y = mean_ratio_max_fo, color = WordForm, shape = WordForm)) +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
#   geom_point(position = position_dodge(width = 0.4), size = 7) +
#   geom_errorbar(
#     aes(ymin = mean_ratio_max_fo - se_ratio_max_fo, ymax = mean_ratio_max_fo + se_ratio_max_fo),
#     position = position_dodge(width = 0.4),
#     width = 0.2
#   ) +
#   ylim(.7, 1.3) +
#   labs(
#     x = "Root Stress Position",
#     y = "Max F0 Ratio (s1:s2)",
#     color = "Word Form",
#     shape = "Word Form"
#   ) +
#   theme_minimal(base_size = 18) +
#   theme(
#     axis.title = element_text(size = 18),
#     axis.text = element_text(size = 16),
#     legend.title = element_text(size = 16),
#     legend.text = element_text(size = 14)
#   )
# 
# print(cs_ratio_max_f0)
# ggsave("cs_ratio_max_f0.png", plot = cs_ratio_max_f0, width = 6, height = 4, dpi = 300)

# Combine into horizontal panel
cs_ratio_dur_clean <- cs_ratio_dur + theme(axis.title.x = element_blank())
cs_ratio_mean_f0_clean <- cs_ratio_mean_f0 + theme(axis.title.x = element_blank())

cs_panel_plotA <- cs_ratio_dur_clean + cs_ratio_mean_int + cs_ratio_mean_f0_clean +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(tag_levels = 'A')

ggsave("Model_A.png", plot = cs_panel_plotA, width = 12, height = 5, dpi = 300, bg = "white")

cs_panel_plotA


#' 
#' ## Model A 
#' 
## -----------------------------------------------------------------------------
### Run lmer() on the subset of dataset

# Test H1: Stress remains fixed on the root
head(cs_roots) 
# The reference level 'uninflected' word forms and against which 'inflected' will be compared in the #. # output. 

# Convert wordform to a factor since there are two cat levels
cs_roots$WordForm <- factor(cs_roots$WordForm)  
# Set ref level
cs_roots$WordForm <- relevel(cs_roots$WordForm, ref = "uninflected")

model_a_dur <- lmer(ratio_s1_s2_dur ~ WordForm*factor(root_stress) + (1|root) +(1|Speaker), data=cs_roots)
summary(model_a_dur)
 
model_a_int <- lmer(ratio_mean_int ~ WordForm*factor(root_stress) + (1|root) +(1|Speaker), data=cs_roots)
summary(model_a_int)

model_a_f0 <- lmer(ratio_mean_f0 ~ WordForm*factor(root_stress) + (1|root) +(1|Speaker), data=cs_roots)
summary(model_a_f0)


# What is the effect of WordForm when root stress =2?
# Relevel root_stress so that 2 becomes the reference level
cs_roots$root_stress <- factor(cs_roots$root_stress, levels = c(2, 1))  # set 2 as the reference

# Refit the model
model_a_stress2 <- lmer(ratio_s1_s2_dur ~ WordForm * root_stress + (1 | root) + (1 | Speaker), data = cs_roots)

# Summarize results
summary(model_a_stress2)


#' 
#' **OBSERVATION:Model_A** 
#' 
#' **Model A: Duration Ratio**
#' 
#' Formula: ratio_s1_s2_dur ~ WordForm * factor(root_stress) + (1|root) + (1|Speaker)
#' 
#' 
#' | Term  | Estimate   | p-value | Interpretation | 
#' |--------------------|---------|--------|----------------------------------------------------------------------|
#' | **(Intercept)**    | **1.095**| < .001 | Baseline duration ratio for **uninflected words with stress on s1** |
#' | `WordForminflected`| 0.092   | .514    | Inflected words show **slightly higher** s1\:s2 duration ratio (NS) |
#' | `factor(root_stress)2`| **–0.283** | .047   | Stress on **s2** results in **lower s1\:s2 duration ratio** (s1 becomes shorter) |
#' | `WordForminflected:factor(root_stress)2` | 0.097  | .625 | No significant interaction effect |
#' 
#' 
#' 
#' **Model A: Mean Intensity Ratio**
#' 
#' Formula: ratio_mean_int ~ WordForm * factor(root_stress) + (1|root) + (1|Speaker)
#' 
#' 
#' | Term    | Estimate   | p-value | Interpretation     |
#' |----------------------|------------|---------|----------------------------------------------------------------------|
#' | **(Intercept)**      | **1.074**  | < .001  | Baseline intensity ratio for **uninflected words with stress on s1** |
#' | `WordForminflected`  | **–0.057** | .015    | Inflected words show **significantly lower** s1\:s2 intensity        |
#' | `factor(root_stress)2`  | **–0.069** | .003    | Stress on **s2** lowers the **s1\:s2 intensity ratio**            |
#' | `WordForminflected:factor(root_stress)2` | 0.044      | .179    | No significant interaction                       |
#' 
#' 
#' **Model A: Mean F0 Ratio**
#' 
#' Formula: ratio_mean_f0 ~ WordForm * factor(root_stress) + (1|root) + (1|Speaker)
#' 
#' 
#' | Term | Estimate   | p-value | Interpretation |
#' |-------------------|---------|------|---------------------------------------------------------------------|
#' | **(Intercept)**   | **1.088**  | < .001  | Baseline F0 ratio for **uninflected words with stress on s1**  |
#' | `WordForminflected` | –0.031     | .078    | Inflected words show **marginally lower** F0 ratio (not quite significant) |
#' | `factor(root_stress)2`  | **–0.042** | .017    | Stress on **s2** lowers the **s1\:s2 F0 ratio**  |
#' | `WordForminflected:factor(root_stress)2` | –0.024     | .321    | No significant interaction effect |
#' 
#' 
#' **Summary**
#' 
#' | Feature                    | Mean Duration | Mean Intensity | Mean F0               |
#' | -------------------------- | ------------- | -------------- | --------------------- |
#' | **Stress position effect** | Significant (decrease in s1:s2 ratio)  | Significant (decrease in s1:s2 ratio)  | Significant (decrease in s1:s2 ratio) |
#' | **Inflection effect**      | Not Significant | Significant (decrease in s1:s2 ratio)  | Not Significant |
#' | **Interaction (Stress*WordForm)** | Not Significant | Not Significant | Not Significant |
#' 
#' 
#' 
#' Since we see a visual difference in Plot A in the s1:s2 duration ratio between root and inflected tokens under root stress condition 2, we examined this effect by refitting the model directly to compare WordForm levels when root_stress == 2. To do this, we re-leveled root_stress so that 2 served as the reference level. In this re-specified model, the main effect of WordForminflected estimates the contrast between inflected and root forms under this stress condition. The model shows a positive but non-significant effect (Estimate = 0.19, p = 0.18), suggesting no strong difference in duration ratio under root stress 2, and the interaction term remains non-significant, indicating that the effect of WordForm does not significantly differ across stress conditions.
#' 
#' 
#' 
#' ## Plot B
#' 
#' - CS tokens by Stress position and WordForm 
#' 
## -----------------------------------------------------------------------------
## Plot B

# Count how many geminates and pause syllables
# df_full_sample %>%
#      filter(Language == "CS", WordForm == "inflected", !is.na(SyllPos)) %>%
#      count(HasGeminates, HasPause)

cs_all_syll <- df_full_sample %>%
  filter(Language == "CS",
         WordForm == "inflected",
         !is.na(SyllPos),
         SyllPos != "c1",
         #(is.na(HasPause) | HasPause != "TRUE"),
         (is.na(HasGeminates) | HasGeminates != "TRUE")) %>%
  mutate(
    Duration_in_ms = as.numeric(Duration_in_ms),
    Mean_dB        = as.numeric(Mean_dB),
    MeanF0         = as.numeric(MeanF0)
  )
         

#view(cs_all_syll)

# New plot for the poster
pd <- position_dodge(width = 0.9)


cs_s1s2s3_plot <- ggplot(cs_all_syll,
       aes(x = factor(StressedSyll), y = Duration_in_ms, fill = SyllPos)) +
  stat_summary(fun = mean, geom = "col", position = pd) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = pd, width = 0.2) +
  labs(x = "Root Stress", y = "Mean Duration (ms)") +
  theme_minimal(base_size = 14) +
  shared_theme
 
print(cs_s1s2s3_plot) 

# Save Plot 
ggsave("cs_s1s2s3_plot.png", plot = cs_s1s2s3_plot, width = 5, height = 5, dpi = 400, bg = "white")






# Plot 2: Intensity
cs_s1s2s3_intensity_plot <- ggplot(cs_all_syll,
                         aes(x = factor(StressedSyll), y = Mean_dB, fill = SyllPos)) +
  stat_summary(fun = mean, geom = "col", position = pd) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = pd, width = 0.2) +
  labs(x = "Root Stress", y = "Mean Intensity (dB)") +
  theme_minimal(base_size = 14) +
  shared_theme

cs_s1s2s3_intensity_plot

# Plot 3: F0 (with legend)
cs_s1s2s3_f0_plot <- ggplot(cs_all_syll,
                            aes(x = factor(StressedSyll), y = MeanF0, fill = SyllPos)) +
  stat_summary(fun = mean, geom = "col", position = pd) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = pd, width = 0.2) +
  labs(x = "Root Stress", y = "Mean f0 (Hz)") +
  theme_minimal(base_size = 14) +
  shared_theme

cs_s1s2s3_f0_plot

# Combine horizontally with shared legend
cs_s1s2s3_plot_clean <- cs_s1s2s3_plot + theme(axis.title.x = element_blank())
cs_s1s2s3_f0_plot_clean <- cs_s1s2s3_f0_plot + theme(axis.title.x = element_blank())

cs_panel_horizontal <- (cs_s1s2s3_plot_clean | cs_s1s2s3_intensity_plot | cs_s1s2s3_f0_plot_clean) +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")

print(cs_panel_horizontal)

# Save output
ggsave("cs_s1s2s3_panel_horizontal.png", plot = cs_panel_horizontal, width = 15, height = 5, dpi = 400, bg = "white")


### Save each plot separately

# Shared theme (legend removed for first 2 plots)
shared_theme <- theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    #legend.position = "none"
  )

# Plot 1: Duration
cs_s1s2s3_plot <- ggplot(summary_cs_all, aes(x = factor(StressedSyll), y = mean_dur, fill = SyllPos)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_dur - se_dur, ymax = mean_dur + se_dur),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  labs(
    x = "Root Stress",
    y = "Mean Duration (ms)"
  ) +
  shared_theme

# Save Plot 1
ggsave("cs_all_duration.png", plot = cs_s1s2s3_plot, width = 5, height = 5, dpi = 400, bg = "white")

# Plot 2: Intensity
cs_s1s2s3_intensity_plot <- ggplot(summary_cs_all, aes(x = factor(StressedSyll), y = mean_dB, fill = SyllPos)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_dB - se_dB, ymax = mean_dB + se_dB),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  labs(
    x = "Root Stress",
    y = "Mean Intensity (dB)"
  ) +
  shared_theme

# Save Plot 2
ggsave("cs_plot_intensity.png", plot = cs_s1s2s3_intensity_plot, width = 5, height = 5, dpi = 400, bg = "white")

# Plot 3: F0 (with legend)
cs_s1s2s3_f0_plot <- ggplot(summary_cs_all, aes(x = factor(StressedSyll), y = mean_f0, fill = SyllPos)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_f0 - se_f0, ymax = mean_f0 + se_f0),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  labs(
    x = "Root Stress",
    y = "Mean F0 (Hz)",
    fill = "Syllable Position"
  ) +
  shared_theme +
  theme(legend.position = "bottom")  # Show legend only here

# Save Plot 3
ggsave("cs_plot_f0.png", plot = cs_s1s2s3_f0_plot, width = 5, height = 5, dpi = 400, bg = "white")

# Combine horizontally
cs_s1s2s3_plot_clean <- cs_s1s2s3_plot + theme(axis.title.x = element_blank())
cs_s1s2s3_f0_plot_clean <- cs_s1s2s3_f0_plot + theme(axis.title.x = element_blank())

cs_panel_horizontal <- (cs_s1s2s3_plot_clean | cs_s1s2s3_intensity_plot | cs_s1s2s3_f0_plot_clean) +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")

print(cs_panel_horizontal)

# Save combined panel
ggsave("cs_s1s2s3_panel_horizontal.png", plot = cs_panel_horizontal, width = 15, height = 5, dpi = 400, bg = "white")


######## Plots for Poster/Proceedings Paper:########

new_df <- read_csv("/Users/moldir/Documents/Fall_2024_IndStudy/CompletedAnnotations/stress_dataset.csv")

cs_roots <- new_df %>%
  filter(Language == "CS",
    WordForm == "uninflected",
    !is.na(SyllPos),
    (is.na(HasPause) | HasPause != "TRUE"),
    (is.na(HasGeminates) | HasGeminates != "TRUE")) %>%
  mutate(
    Duration_in_ms = as.numeric(Duration_in_ms),
    Mean_dB        = as.numeric(Mean_dB),
    MeanF0         = as.numeric(MeanF0))

view(cs_roots)

# define the dodge used by bars and error bars
pd <- position_dodge(width = 0.9)

cs_s1s2 <- ggplot(cs_roots, aes(x = factor(StressedSyll), y = Duration_in_ms, fill = SyllPos)) +
  stat_summary(fun = mean, geom = "col", position = pd) +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = pd, width = 0.2) +
  # match the inflected-plot colors exactly (ggplot default palette):
  scale_fill_manual(values = c("s1" = "#F8766D",   # coral/pink
                               "s2" = "#00BA38"),  # green
                    drop = FALSE, name = NULL) +
  labs(x = "Root Stress", y = "Mean Duration (ms)") +
  shared_theme

print(cs_s1s2)

## For PP

cs_s1s2_bw <- ggplot(cs_roots, aes(x = factor(StressedSyll), y = Duration_in_ms, fill = Stress)) +
  stat_summary(fun = mean, geom = "col", position = pd, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = pd, width = 0.2, color = "black") +
  scale_fill_grey(start = 1, end = 0.3, drop = FALSE, name = NULL) +  # white -> dark grey
  labs(x = "Root Stress", y = "Mean Duration (ms)") +
  shared_theme

print(cs_s1s2_bw)

cs_s1s2_int <- ggplot(cs_roots, aes(x = factor(StressedSyll), y = Mean_dB, fill = Stress)) +
  stat_summary(fun = mean, geom = "col", position = pd, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = pd, width = 0.2, color = "black") +
  scale_fill_grey(start = 1, end = 0.3, drop = FALSE, name = NULL) +  # white -> dark grey
  labs(x = "Root Stress", y = "Mean Intensity (dB)") +
  shared_theme

print(cs_s1s2_int)

cs_s1s2_f0 <- ggplot(cs_roots, aes(x = factor(StressedSyll), y = MeanF0, fill = Stress)) +
  stat_summary(fun = mean, geom = "col", position = pd, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = pd, width = 0.2, color = "black") +
  scale_fill_grey(start = 1, end = 0.3, drop = FALSE, name = NULL) +  # white -> dark grey
  labs(x = "Root Stress", y = "Mean f0 (Hz)") +
  shared_theme

print(cs_s1s2_f0)

# Save Plot 
ggsave("cs_s1s2_bw.png", plot = cs_s1s2, width = 5, height = 5, dpi = 600, bg = "white")



##### BW plots 
cs_s1s2_bw <- ggplot(cs_roots, aes(x = factor(StressedSyll), y = Duration_in_ms, fill = Stress)) +
  stat_summary(fun = mean, geom = "col", position = pd, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = pd, width = 0.2, color = "black") +
  scale_fill_grey(start = 1, end = 0.3, drop = FALSE, name = NULL) +
  labs(x = NULL, y = "Mean Duration (ms)") +   # <- remove x-axis title here
  shared_theme +
  theme(legend.position = "right")

cs_s1s2_int <- ggplot(cs_roots, aes(x = factor(StressedSyll), y = Mean_dB, fill = Stress)) +
  stat_summary(fun = mean, geom = "col", position = pd, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = pd, width = 0.2, color = "black") +
  scale_fill_grey(start = 1, end = 0.3, drop = FALSE, name = NULL) +
  labs(x = "Root Stress", y = "Mean Intensity (dB)") +  # <- keep x-axis title only here
  shared_theme +
  theme(legend.position = "right")

cs_s1s2_f0 <- ggplot(cs_roots, aes(x = factor(StressedSyll), y = MeanF0, fill = Stress)) +
  stat_summary(fun = mean, geom = "col", position = pd, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = pd, width = 0.2, color = "black") +
  scale_fill_grey(start = 1, end = 0.3, drop = FALSE, name = NULL) +
  labs(x = NULL, y = "Mean f0 (Hz)") +      # <- remove x-axis title here
  shared_theme +
  theme(legend.position = "right")

combo_plot <- (cs_s1s2_bw | cs_s1s2_int | cs_s1s2_f0) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

combo_plot


ggsave(
  "/Users/moldir/Documents/Fall_2024_IndStudy/CompletedAnnotations/AMP_2025_Proceedings_Paper/combo.png",
  plot = combo_plot,
  width = 5,
  height = 5,
  dpi = 600,
  bg = "white"
)


#### CS inflected tokens - bw - combined for paper

pd <- position_dodge(width = 0.9)

# 3-level BW/Grey palette for SyllPos (edit greys if you want more/less contrast)
bw3 <- c(s1 = "white", s2 = "grey70", s3 = "grey30")

cs_s1s2s3_plot <- ggplot(cs_all_syll,
                         aes(x = factor(StressedSyll), y = Duration_in_ms, fill = SyllPos)) +
  stat_summary(fun = mean, geom = "col", position = pd, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = pd, width = 0.2, color = "black") +
  scale_fill_manual(values = bw3, drop = FALSE, name = "Syllable position") +
  labs(x = NULL, y = "Mean Duration (ms)") +
  shared_theme +
  theme(legend.position = "right")

cs_s1s2s3_intensity_plot <- ggplot(cs_all_syll,
                                   aes(x = factor(StressedSyll), y = Mean_dB, fill = SyllPos)) +
  stat_summary(fun = mean, geom = "col", position = pd, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = pd, width = 0.2, color = "black") +
  scale_fill_manual(values = bw3, drop = FALSE, name = "Syllable position") +
  labs(x = "Root Stress", y = "Mean Intensity (dB)") +   # only middle gets x label
  shared_theme +
  theme(legend.position = "right")

cs_s1s2s3_f0_plot <- ggplot(cs_all_syll,
                            aes(x = factor(StressedSyll), y = MeanF0, fill = SyllPos)) +
  stat_summary(fun = mean, geom = "col", position = pd, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = pd, width = 0.2, color = "black") +
  scale_fill_manual(values = bw3, drop = FALSE, name = "Syllable position") +
  labs(x = NULL, y = "Mean f0 (Hz)") +
  shared_theme +
  theme(legend.position = "right")

# Combine into one figure: A, B, C tags + single legend on right
combo_plot_all <- (cs_s1s2s3_plot | cs_s1s2s3_intensity_plot | cs_s1s2s3_f0_plot) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

combo_plot_all


### New model that accounts for stress correlates in CS based on Stress and Word Form 


new_model_data <- new_df %>%
  filter(Language == "CS",
         !is.na(SyllPos),
         !is.na(MeanF0),
         (is.na(HasPause) | HasPause != "TRUE"),
         (is.na(HasGeminates) | HasGeminates != "TRUE")) %>%
  mutate(
    Duration_in_ms = as.numeric(Duration_in_ms),
    Mean_dB        = as.numeric(Mean_dB),
    MeanF0         = as.numeric(MeanF0))

view(new_model_data)

new_model_data$WordForm <- factor(new_model_data$WordForm)
new_model_data$WordForm <- relevel(new_model_data$WordForm, ref = "uninflected")


new_model <- lmer(Duration_in_ms ~ Stress + WordForm + (1|Speaker) + (1|Word), data = new_model_data)
summary(new_model)


new_model_int <- lmer(Duration_in_ms ~ Stress * WordForm + (1|Speaker) + (1|Word), data = new_model_data)
summary(new_model_int) 

new_model_loudness <- lmer(Mean_dB ~ Stress * WordForm + (1|Speaker) + (1|Word), data = new_model_data)
summary(new_model_loudness)

new_model_pitch <- lmer(MeanF0 ~ Stress * WordForm + (1|Speaker) + (1|Word), data = new_model_data)
summary(new_model_pitch)

#' ## Model B


# Test H2:Stress follows Kazakh rules
# s3 would have significantly longer duration than s1 and s2 if H2 is true. 
# dataset contains durations of all s1,s2, s3
#head(cs_all_syll)

# Initial model b for comparing s1,s2 and s3 
# model_b <- lmer(Duration_in_ms ~ SyllPos*Stress + (1|Speaker), data=cs_all_syll)
# summary(model_b)

# reference level - s1 stressed vs s3
# reference level - s2 stressed vs s3
# comparing positional difference based on root stress 
# new code below taking into account above comments:

## s1 vs s3
# Recode NA as "no_stress" 
cs_all_syll$Stress <- as.character(cs_all_syll$Stress)
cs_all_syll$Stress[is.na(cs_all_syll$Stress)] <- "no_stress"
cs_all_syll$Stress <- factor(cs_all_syll$Stress, levels = c("stressed", "unstressed", "no_stress"))

# Filter only s1 and s3 rows
cs_s1_s3_new <- cs_all_syll %>%
  filter(SyllPos %in% c("s1", "s3"))

# Identify words where s1 is stressed
words_with_stressed_s1 <- cs_s1_s3_new %>%
  filter(SyllPos == "s1", Stress == "stressed") %>%
  pull(Word) %>% unique()

# Keep s1 and s3 syllables only from those words
cs_s1_s3_stressed_new <- cs_s1_s3_new %>%
  filter(Word %in% words_with_stressed_s1)
cs_s1_s3_stressed_new

# Fit the model (model B_1, stressed s1 vs. s3 no_stress)
model_s1_vs_s3_dur <- lmer(Duration_in_ms ~ factor(SyllPos) + (1 | Speaker) + (1|Word), data = cs_s1_s3_stressed_new)
summary(model_s1_vs_s3_dur)

model_s1_vs_s3_int <- lmer(Mean_dB ~ factor(SyllPos) + (1 | Speaker) + (1|Word), data = cs_s1_s3_stressed_new)
summary(model_s1_vs_s3_int)

model_s1_vs_s3_f0 <- lmer(MeanF0 ~ factor(SyllPos) + (1 | Speaker) + (1|Word), data = cs_s1_s3_stressed_new)
summary(model_s1_vs_s3_f0)


# Filter to keep only rows where Stress is "stressed" AND SyllPos is s2 and s3

# Filter only s1 and s3 rows
cs_s2_s3_new <- cs_all_syll %>%
  filter(SyllPos %in% c("s2", "s3"))

# Identify words where s2 is stressed
words_with_stressed_s2 <- cs_s2_s3_new %>%
  filter(SyllPos == "s2", Stress == "stressed") %>%
  pull(Word) %>% unique()

# Keep s2 and s3 syllables only from those words
cs_s2_s3_stressed_new <- cs_s2_s3_new %>%
  filter(Word %in% words_with_stressed_s2)
cs_s2_s3_stressed_new

# Fit the model (model B_2, stressed s2 vs. s3 no_stress)
model_s2_vs_s3_dur <- lmer(Duration_in_ms ~ factor(SyllPos) + (1 | Speaker) + (1|Word), data = cs_s2_s3_stressed_new)
summary(model_s2_vs_s3_dur)

model_s2_vs_s3_int <- lmer(Mean_dB ~ factor(SyllPos) + (1 | Speaker) + (1|Word), data = cs_s2_s3_stressed_new)
summary(model_s2_vs_s3_int)

model_s2_vs_s3_f0 <- lmer(MeanF0 ~ factor(SyllPos) + (1 | Speaker) + (1|Word), data = cs_s2_s3_stressed_new)
summary(model_s2_vs_s3_f0)


#' 
#' **OBSERVATION: Model_B** 
#' 
#' **Model B: Duration (s1_stressed vs s3)**
#' 
#' Formula: Duration_in_ms ~ factor(SyllPos) + (1 | Speaker)
#' 
#' 
#' | Term                | Estimate   | p-value | Interpretation                                                            |
#' | ------------------- | ---------- | ------- | ------------------------------------------------------------------------- |
#' | **(Intercept)**     | **223** | < .001  | Baseline duration for **stressed s1** is approximately **223 ms**        |
#' | `factor(SyllPos)s3` | -6.372     | 0.543 | No significant duration difference; s3 duration is nearly identical to s1 |
#' 
#' 
#' 
#' **Model B: Intensity (s1_stressed vs s3)**
#' 
#' Formula: Mean_dB ~ factor(SyllPos) + (1 | Speaker)
#' 
#' 
#' | Term                | Estimate  | p-value | Interpretation                                                       |
#' | ------------------- | --------- | ------- | -------------------------------------------------------------------- |
#' | **(Intercept)**     | **69.96** | < .001  | Baseline intensity for **stressed s1** is approximately **70 dB** |
#' | `factor(SyllPos)s3` | **-2.06** | < .01  | s3 syllables are **significantly less intense** than s1 by \~2 dB  |
#' 
#' 
#' 
#' **Model B: F0 (s1_stressed vs s3)**
#' 
#' Formula: MeanF0 ~ factor(SyllPos) + (1 | Speaker)
#' 
#' | Term                | Estimate   | p-value | Interpretation                                                    |
#' | ------------------- | ---------- | ------- | ----------------------------------------------------------------- |
#' | **(Intercept)**     | **160.63** | .013    | Baseline F0 for **stressed s1** is approximately **160 Hz**      |
#' | `factor(SyllPos)s3` | -4.157      | .161  | s3 syllables show **no significant F0 difference** compared to s1 |
#' 
#' 
#' **Summary**
#' 
#' | Feature        | Duration                   | Intensity                  | F0                          |
#' | -------------- | -------------------------- | -------------------------- | --------------------------- |
#' | s3 effect      | Not significant (p = .543) | **Significant** (p < .001) | Not significant (p = .161)  |
#' | Estimate       | -6.372 ms                   | –2 dB                   | –4 Hz                    |
#' | Interpretation | No change from s1          | s3 has **lower intensity** | No meaningful F0 difference |
#' 
#' 
#' ------------------------------------------
#' 
#' **Model B: Duration (s2_stressed vs s3)**
#' 
#' Formula: Duration_in_ms ~ factor(SyllPos) + (1 | Speaker)
#' 
#' 
#' | Term                | Estimate   | p-value | Interpretation                                                     |
#' | ------------------- | ---------- | ------- | ------------------------------------------------------------------ |
#' | **(Intercept)**     | **237.87** | < .001  | Baseline duration for **stressed s2** is approximately **238 ms** |
#' | `factor(SyllPos)s3` | 2.96       | .762    | s3 duration is **not significantly different** from s2             |
#' 
#' 
#' 
#' **Model B: Intensity (s2_stressed vs s3)**
#' 
#' Formula: Mean_dB ~ factor(SyllPos) + (1 | Speaker)
#' 
#' 
#' | Term                | Estimate  | p-value | Interpretation                                                              |
#' | ------------------- | --------- | ------- | --------------------------------------------------------------------------- |
#' | **(Intercept)**     | **70.36** | < .001  | Baseline intensity for **stressed s2** is approximately **70 dB**        |
#' | `factor(SyllPos)s3` | -1.12     | .03     | s3 is **\~1 dB less intense** and this difference is **significant** |
#' 
#' 
#' 
#' **Model B: F0 (s2_stressed vs s3)**
#' 
#' Formula: MeanF0 ~ factor(SyllPos) + (1 | Speaker)
#' 
#' | Term                | Estimate   | p-value | Interpretation                                                            |
#' | ------------------- | ---------- | ------- | ------------------------------------------------------------------------- |
#' | **(Intercept)**     | **154.73** | .017    | Baseline F0 for **stressed s2** is approximately **155 Hz**              |
#' | `factor(SyllPos)s3` | **+9.34**  | < .001  | s3 syllables show a **significantly higher F0** (\~9.4 Hz) compared to s2 |
#' 
#' 
#' 
#' **Summary**
#' 
#' | Feature        | Duration                   | Intensity                  | F0                                  |
#' | -------------- | -------------------------- | -------------------------- | ----------------------------------- |
#' | s3 effect      | Not significant (p = .762) | **Significant** (p = .03) | **Significant** increase (p < .001) |
#' | Estimate       | +2.96 ms                   | –1.12 dB                   | +9.34 Hz                            |
#' | Interpretation | No meaningful change       | Significant change       | s3 has **notably higher pitch**     |
#' 
#' 
#' Within inflected CS tokens, we observe a mixed prosodic pattern across syllable positions. 
#' Duration-wise, both stressed s1 and s2 retain their prominence, while the Kazakh s3 syllable shows slightly longer duration, 
#' although this effect is not statistically significant in either comparison (p = .543 for s1 vs s3; p = .762 for s2 vs s3). 
#' In terms of intensity, stressed Russian syllables (s1 and s2) are consistently more intense than s3, with s3 showing a significant 
#' decrease of ~2 dB and ~1 dB, respectively. However, f0 patterns diverge: while there is no significant f0 difference between s1 and s3, 
#' the comparison with s2 reveals a significant pitch rise on s3 (Estimate = +9.34 Hz, p < .001). This reflects a hybrid prosodic system, 
#' where Russian stress cues persist in early syllables, and intensity is the most robust correlate across syllable positions, while Kazakh
#'  intonational patterns begin to emerge in later positions.




#' ## Plot C
#' 
#' - s3 difference in Kaz and CS tokens 
#' 
## -----------------------------------------------------------------------------
## Plot C == s3 difference in Kaz and CS tokens 

kz_cs_df <- new_df %>%
  filter(Language %in% c("CS", "Kaz"),
         SyllPos == 's3') %>%
 filter(!is.na(SyllPos))
#view(kz_cs_df)

summary_kz_cs <- kz_cs_df %>%
  group_by(Language) %>%
  summarise(
    mean_dur = mean(Duration_in_ms, na.rm = TRUE),
    sd_dur   = sd(Duration_in_ms, na.rm = TRUE),

    mean_dB  = mean(Mean_dB, na.rm = TRUE),
    sd_dB    = sd(Mean_dB, na.rm = TRUE),

    mean_f0  = mean(MeanF0, na.rm = TRUE),
    sd_f0    = sd(MeanF0, na.rm = TRUE),

    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se_dur = sd_dur / sqrt(n),
    se_dB  = sd_dB / sqrt(n),
    se_f0  = sd_f0 / sqrt(n)
  )

# Shared color palette and theme
# fill_colors <- c("Kaz" = "#666666", "CS" = "#a6cee3")


### For the Poster
cs_kaz_s3 <- new_df %>%
  filter(Language %in% c("CS","Kaz"),
    WordForm == "inflected",
    SyllPos == "s3",
    !is.na(SyllPos),
    (is.na(HasPause) | HasPause != "TRUE"),
    (is.na(HasGeminates) | HasGeminates != "TRUE")) %>%
  mutate(
    Duration_in_ms = as.numeric(Duration_in_ms),
    Mean_dB        = as.numeric(Mean_dB),
    MeanF0         = as.numeric(MeanF0))


summary_kz_cs <- cs_kaz_s3 %>%
  group_by(Language) %>%
  summarise(
    mean_dur = mean(Duration_in_ms, na.rm = TRUE),
    sd_dur   = sd(Duration_in_ms, na.rm = TRUE),

    mean_dB  = mean(Mean_dB, na.rm = TRUE),
    sd_dB    = sd(Mean_dB, na.rm = TRUE),

    mean_f0  = mean(MeanF0, na.rm = TRUE),
    sd_f0    = sd(MeanF0, na.rm = TRUE),

    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se_dur = sd_dur / sqrt(n),
    se_dB  = sd_dB / sqrt(n),
    se_f0  = sd_f0 / sqrt(n)
  )


# Duration plot
p_dur <- ggplot(summary_kz_cs, aes(x = Language, y = mean_dur, fill = Language)) +
  geom_bar(stat = "identity", position = position_dodge(0.4), width = 0.4) +
  geom_errorbar(aes(ymin = mean_dur - se_dur, ymax = mean_dur + se_dur),
                width = 0.2, position = position_dodge(0.4)) +
  scale_fill_manual(values = c(CS = "#619CFF", Kaz = "#619CFF")) +  # s3 blue for both
  labs(y = "Mean Duration (ms)", x = NULL) +
  shared_theme

print(p_dur)

# Save Plot 1
# ggsave("s3_dur.png", plot = p_dur, width = 5, height = 5, dpi = 600, bg = "white")

# Intensity plot
p_dB <- ggplot(summary_kz_cs, aes(x = Language, y = mean_dB, fill = Language)) +
  geom_bar(stat = "identity", position = position_dodge(0.4), width = 0.4) +
  geom_errorbar(aes(ymin = mean_dB - se_dB, ymax = mean_dB + se_dB),
                width = 0.2, position = position_dodge(0.4)) +
  # scale_fill_manual(values = fill_colors) +
  labs(y = "Mean Intensity (dB)", x = NULL) +
  shared_theme

# F0 plot
p_f0 <- ggplot(summary_kz_cs, aes(x = Language, y = mean_f0, fill = Language)) +
  geom_bar(stat = "identity", position = position_dodge(0.4), width = 0.4) +
  geom_errorbar(aes(ymin = mean_f0 - se_f0, ymax = mean_f0 + se_f0),
                width = 0.2, position = position_dodge(0.4)) +
  # scale_fill_manual(values = fill_colors) +
  labs(y = "Mean F0 (Hz)", x = NULL) +
  shared_theme

# Horizontal panel with A, B, C annotations
panel_horizontal <- p_dur + p_dB + p_f0 +
  plot_layout(ncol = 3, guides = "collect") & 
  theme(legend.position = "bottom") 
  # plot_annotation(tag_levels = 'A') 
  
panel_horizontal <- panel_horizontal + plot_annotation(
  title = NULL,
  subtitle = NULL,
  caption = "Language"
)

print(panel_horizontal)

# ggsave("kz_cs_panel_horizontal_tagged.png", panel_horizontal, width = 15, height = 5, dpi = 400, bg = "white")


##### For PP 
# bw plots

# Black/white palette 
bw_fills <- c(
  CS  = "grey80",  # light gray
  Kaz = "black"    # black
)

p_dur <- ggplot(summary_kz_cs, aes(x = Language, y = mean_dur, fill = Language)) +
  geom_bar(stat = "identity", position = position_dodge(0.4), width = 0.4, color = "black") +
  geom_errorbar(aes(ymin = mean_dur - se_dur, ymax = mean_dur + se_dur),
                width = 0.2, position = position_dodge(0.4)) +
  scale_fill_manual(values = bw_fills) +
  labs(y = "Mean Duration (ms)", x = NULL, fill = "Language") +
  shared_theme

p_dB <- ggplot(summary_kz_cs, aes(x = Language, y = mean_dB, fill = Language)) +
  geom_bar(stat = "identity", position = position_dodge(0.4), width = 0.4, color = "black") +
  geom_errorbar(aes(ymin = mean_dB - se_dB, ymax = mean_dB + se_dB),
                width = 0.2, position = position_dodge(0.4)) +
  scale_fill_manual(values = bw_fills) +
  labs(y = "Mean Intensity (dB)", x = NULL, fill = "Language") +
  shared_theme

p_f0 <- ggplot(summary_kz_cs, aes(x = Language, y = mean_f0, fill = Language)) +
  geom_bar(stat = "identity", position = position_dodge(0.4), width = 0.4, color = "black") +
  geom_errorbar(aes(ymin = mean_f0 - se_f0, ymax = mean_f0 + se_f0),
                width = 0.2, position = position_dodge(0.4)) +
  scale_fill_manual(values = bw_fills) +
  labs(y = "Mean f0 (Hz)", x = NULL, fill = "Language") +
  shared_theme

# Horizontal panel with legend on the right
panel_horizontal <- (p_dur + p_dB + p_f0) +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "right")

panel_horizontal <- panel_horizontal + plot_annotation(
  title = NULL,
  subtitle = NULL,
  caption = "Language"
)

print(panel_horizontal)


#' ## Model C

# Test H3: A mix of Kazakh and Russian stress 
# dataset contains duration of s3 only for Kaz and CS
# Duration of s3 by Language
head(kz_cs_df)
kz_cs_df <- kz_cs_df %>%
  mutate(MeanF0 = as.numeric(MeanF0))

# Convert wordform to a factor since there are two cat levels
kz_cs_df$Language <- factor(kz_cs_df$Language)  
# Set ref level
kz_cs_df$Language <- relevel(kz_cs_df$Language, ref = "Kaz")

model_c_dur <- lmer(Duration_in_ms ~ factor(Language) + (1|Speaker) + (1|Word), data=kz_cs_df)
summary(model_c_dur)

model_c_int <- lmer(Mean_dB ~ factor(Language) + (1|Speaker) + (1|Word), data=kz_cs_df)
summary(model_c_int)

model_c_f0 <- lmer(MeanF0 ~ factor(Language) + (1|Speaker) + (1|Word), data=kz_cs_df)
summary(model_c_f0)


#' 
#' 
#' **OBSERVATION:Model_C**
#' 
#' **Model C: Duration**
#' 
#' *Formula: Duration_in_ms ~ factor(Language) + (1 | Speaker)*
#' 
#' 
#' | Term                 | Estimate | p-value | Interpretation                                          |
#' | -------------------- | -------- | ------- | ------------------------------------------------------- |
#' | **(Intercept)**      | 243.44   | < .001 | Baseline duration for s3 in Kazakh: \~243 ms           |
#' | `factor(Language)CS` | –10.12   | .092   | CS tokens are \~10 ms shorter, *not significant* |
#' 
#' 
#' 
#' **Model C: Mean Intensity**
#' 
#' *Formula: Mean_dB ~ factor(Language) + (1 | Speaker)*
#' 
#' | Term                 | Estimate | p-value | Interpretation                                             |
#' | -------------------- | -------- | ------- | ---------------------------------------------------------- |
#' | **(Intercept)**      | 68.64    | < .001 | Baseline mean intensity for s3 in Kazakh: \~68.6 dB       |
#' | `factor(Language)CS` | –0.42    | .338   | CS tokens show slightly lower intensity, *not significant* |
#' 
#' 
#' **Model C: Mean F0**
#' 
#' *Formula: MeanF0 ~ factor(Language) + (1 | Speaker)*
#' 
#' | Term                 | Estimate | p-value | Interpretation                                      |
#' | -------------------- | -------- | ------- | --------------------------------------------------- |
#' | **(Intercept)**      | 168.30   | .012   | Baseline mean F0 for s3 in Kazakh: \~168 Hz        |
#' | `factor(Language)CS` | –7.17    | < .001 | CS tokens show **significantly** lower f0 (\~7 Hz drop) |
#' 
#' 
#' **Summary**
#' 
#' | Feature        | Language Effect | Interpretation                                     |
#' | -------------- | --------------- | -------------------------------------------------- |
#' | Duration       | Marginal    | CS tokens tend to be shorter than Kazakh (\~10 ms diff) |
#' | Mean Intensity | Not significant | No notable difference across languages             |
#' | Mean F0        | Significant   | CS tokens show a clear f0 drop (\~7 Hz lower)      |
#' 
#' 
#' Model C examined prosodic differences in the final syllable (s3) between Kazakh and CS tokens. 
#' The results show a marginally significant effect for duration, with CS tokens being approximately 
#' 10 ms shorter than Kazakh tokens (p = .092). No significant difference was found in mean intensity 
#' across languages (p = .338), suggesting that loudness is not a distinguishing cue in this position.
#'  However, mean f0 was significantly lower in CS tokens, with a drop of approximately 7 Hz compared 
#'  to Kazakh (p < .001). This indicates that pitch reduction in CS tokens may serve as a key prosodic 
#'  marker distinguishing them from monolingual Kazakh forms.



#' ## Plot D
#' 
#' - Mobile (forward moving stress) vs immobile CS roots  
#' - Difference in Duration, Intensity, and F0.
#' 
## ----echo = FALSE-------------------------------------------------------------
# Test H4: We need to re-consider this H2 due to variability in stress movement in CS tokens.
# In Cs tokens stress might move to either s1 or s3; so there is no uniform trend in mobile roots.
# StressShift and ShiftDirect columns were added.

# Filter immobile (StressShift =NO) and mobile (StressShift = Yes) and ShiftDirect = forward CS tokens


# Create the final dataset
cs_roots_filtered <- cs_df_wide %>%
  select(Speaker, root, root_stress, WordForm, StressShift, ShiftDirect, ratio_s1_s2_dur, ratio_mean_int, ratio_max_int, ratio_mean_f0, ratio_max_fo)  %>%
  filter(ShiftDirect %in% c("forward", "na")) %>%
    mutate(ShiftDirect = recode(ShiftDirect,
                              "forward" = "mobile",
                             "na" = "fixed"))


#view(cs_df_wide)
# First, filter the data down to the relevant subset
# cs_roots_filtered <- cs_df_wide %>%
#   filter(HasPause_s1 != "TRUE",
#          HasPause_s2 != "TRUE",
#          HasGeminates_s1 != "TRUE",
#          HasGeminates_s2 != "TRUE") %>%
#   select(Speaker, root, root_stress, WordForm, StressShift, ShiftDirect, 
#          ratio_s1_s2_dur, ratio_mean_int, ratio_max_int, ratio_mean_f0, ratio_max_fo) %>%
#   filter(ShiftDirect %in% c("forward", "na")) %>%
#   mutate(ShiftDirect = recode(ShiftDirect,
#                               "forward" = "mobile",
#                               "na" = "fixed"))

# Separate "yes" and randomly sampled "no"
yes_rows <- cs_roots_filtered %>% filter(StressShift == "yes" & root_stress == 1)
no_rows <- cs_roots_filtered %>% filter(StressShift == "no" & root_stress == 1) %>% sample_n(59)

# Combine both
cs_roots_balanced <- bind_rows(yes_rows, no_rows)

# view(cs_roots)

# Summarize the data
summary_df_cs <- cs_roots_balanced %>%
  group_by(root_stress, WordForm, ShiftDirect) %>%
  summarise(
    mean_ratio_dur  = mean(ratio_s1_s2_dur, na.rm = TRUE),
    sd_ratio_dur    = sd(ratio_s1_s2_dur, na.rm = TRUE),
    
    mean_ratio_int  = mean(ratio_mean_int, na.rm = TRUE),
    sd_ratio_int    = sd(ratio_mean_int, na.rm = TRUE),
    
    mean_ratio_max_int = mean(ratio_max_int, na.rm = TRUE),
    sd_ratio_max_int   = sd(ratio_max_int, na.rm = TRUE),
    
    mean_ratio_f0   = mean(ratio_mean_f0, na.rm = TRUE),
    sd_ratio_f0     = sd(ratio_mean_f0, na.rm = TRUE),
    
    mean_ratio_max_fo = mean(ratio_max_fo, na.rm = TRUE),
    sd_ratio_max_fo   = sd(ratio_max_fo, na.rm = TRUE),
    
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se_ratio_dur  = sd_ratio_dur / sqrt(n),
    se_ratio_int  = sd_ratio_int / sqrt(n),
    se_ratio_max_int = sd_ratio_max_int / sqrt(n),
    se_ratio_f0   = sd_ratio_f0 / sqrt(n),
    se_ratio_max_fo = sd_ratio_max_fo / sqrt(n)
  )
view(summary_df_cs)

summary_df_cs_clean <- summary_df_cs %>%
  filter(!is.na(se_ratio_dur),!is.na(se_ratio_int), !is.na(se_ratio_f0))

summary_df_cs_clean 

# Plot 1: Duration Ratio
cs_ratio_dur <- ggplot(summary_df_cs_clean, aes(x = factor(root_stress), y = mean_ratio_dur, color = WordForm, shape = WordForm)) +
  facet_wrap(~ShiftDirect) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.4), size = 7) +
  geom_errorbar(
    aes(ymin = mean_ratio_dur - se_ratio_dur, ymax = mean_ratio_dur + se_ratio_dur),
    position = position_dodge(width = 0.4),
    width = 0.2
  ) +
  
  ylim(.3, 1.5) +
  labs(
    x = "Root Stress Position",
    y = "Mean Duration Ratio (s1:s2)",
    color = "Word Form",
    shape = "Word Form"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

print(cs_ratio_dur)
ggsave("cs_ratio_duration.png", plot = cs_ratio_dur, width = 6, height = 4, dpi = 300, bg = "white")

# Plot 2: Mean Intensity Ratio
cs_ratio_mean_int <- ggplot(summary_df_cs_clean, aes(x = factor(root_stress), y = mean_ratio_int, color = WordForm, shape = WordForm)) +
  facet_wrap(~ShiftDirect) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.4), size = 7) +
  geom_errorbar(
    aes(ymin = mean_ratio_int - se_ratio_int, ymax = mean_ratio_int + se_ratio_int),
    position = position_dodge(width = 0.4),
    width = 0.2
  ) +
  ylim(.9, 1.2) +
 
  labs(
    x = "Root Stress Position",
    y = "Mean Intensity Ratio (s1:s2)",
    color = "Word Form",
    shape = "Word Form"
  ) +
#scale_color_manual(values = c("uninflected" = "#2ca02c", "inflected" = "#9467bd")) +
#scale_shape_manual(values = c("uninflected" = 15, "inflected" = 18)) +

  theme_minimal(base_size = 18) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )


print(cs_ratio_mean_int)
ggsave("cs_ratio_mean_intensity.png", plot = cs_ratio_mean_int, width = 6, height = 4, dpi = 300, bg = "white")


# Plot 4: Mean F0 Ratio
cs_ratio_mean_f0 <- ggplot(summary_df_cs_clean, aes(x = factor(root_stress), y = mean_ratio_f0, color = WordForm, shape = WordForm)) +
  facet_wrap(~ShiftDirect) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.4), size = 7) +
  geom_errorbar(
    aes(ymin = mean_ratio_f0 - se_ratio_f0, ymax = mean_ratio_f0 + se_ratio_f0),
    position = position_dodge(width = 0.4),
    width = 0.2
  ) +
  ylim(.8, 1.3) +
  labs(
    x = "Root Stress Position",
    y = "Mean F0 Ratio (s1:s2)",
    color = "Word Form",
    shape = "Word Form"
  ) +
  #scale_color_manual(values = c("uninflected" = "#ff7f0e", "inflected" = "#e377c2")) +
#scale_shape_manual(values = c("uninflected" = 8, "inflected" = 4)) +

  theme_minimal(base_size = 18) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

print(cs_ratio_mean_f0)
ggsave("cs_ratio_mean_f0.png", plot = cs_ratio_mean_f0, width = 6, height = 4, dpi = 300, bg = "white")



#' ## Plot D 
#' 
#' - fixed and mobile root difference 
#' - duration, intensity, and f0
#' 

cs_all_syll_shift <- df_full_sample %>%
   filter(Language == "CS") %>%
   filter(ShiftDirect %in% c("forward", "na")) %>%
    mutate(ShiftDirect = recode(ShiftDirect,
                              "forward" = "mobile",
                             "na" = "fixed")) %>%
  filter(!is.na(SyllPos))

# Separate "yes" and randomly sampled "no"
yes_rows_new <- cs_all_syll_shift %>% filter(StressShift == "yes" & StressedSyll == 1)
no_rows_new <- cs_all_syll_shift %>% filter(StressShift == "no" & StressedSyll == 1) %>% sample_n(59)

# Combine both
cs_roots_balanced <- bind_rows(yes_rows_new, no_rows_new)

#  filter(StressShift == "no") %>%
#   sample_n(size = 59)
# view(cs_all_syll)

# Plot with error bars (b) == duration of s1, s2, s3 by Stress and WordForm

# Summarize duration, intensity, and F0
summary_cs_all_shift <- cs_roots_balanced %>%
  group_by(StressedSyll, SyllPos,ShiftDirect) %>%
  summarise(
    mean_dur = mean(Duration_in_ms, na.rm = TRUE),
    sd_dur = sd(Duration_in_ms, na.rm = TRUE),

    mean_dB = mean(Mean_dB, na.rm = TRUE),
    sd_dB = sd(Mean_dB, na.rm = TRUE),

    mean_f0 = mean(MeanF0, na.rm = TRUE),
    sd_f0 = sd(MeanF0, na.rm = TRUE),

    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se_dur = sd_dur / sqrt(n),
    se_dB = sd_dB / sqrt(n),
    se_f0 = sd_f0 / sqrt(n)
  )


# Neutral grey palette
#grey_palette <- c("s1" = "#999999", "s2" = "#666666", "s3" = "#333333")

# Shared minimalist theme (legend removed for first 2 plots)
shared_theme <- theme_minimal(base_size = 18) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = "none"
  )

# Plot 1: Duration
cs_s1s2s3_plot <- ggplot(summary_cs_all_shift, aes(x = factor(StressedSyll), y = mean_dur, fill = SyllPos)) +
  facet_wrap(~ShiftDirect) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_dur - se_dur, ymax = mean_dur + se_dur),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  # scale_fill_manual(values = grey_palette) +
  labs(
    x = "Root Stress",
    y = "Duration (ms)"
  ) +
  shared_theme

print(cs_s1s2s3_plot)
ggsave("fixed_mobile_duration.png", plot = cs_s1s2s3_plot, width = 12, height = 5, dpi = 300, bg = "white")

# Plot 2: Intensity
cs_s1s2s3_intensity_plot <- ggplot(summary_cs_all_shift, aes(x = factor(StressedSyll), y = mean_dB, fill = SyllPos)) +
  facet_wrap(~ShiftDirect) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_dB - se_dB, ymax = mean_dB + se_dB),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  # scale_fill_manual(values = grey_palette) +
  labs(
    x = "Root Stress",
    y = "Intensity (dB)"
  ) +
  shared_theme

print(cs_s1s2s3_intensity_plot)
ggsave("fixed_mobile_int.png", plot = cs_s1s2s3_intensity_plot, width = 12, height = 5, dpi = 300, bg = "white")

# Plot 3: F0 (with legend)
cs_s1s2s3_f0_plot <- ggplot(summary_cs_all_shift, aes(x = factor(StressedSyll), y = mean_f0, fill = SyllPos)) +
  facet_wrap(~ShiftDirect) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_f0 - se_f0, ymax = mean_f0 + se_f0),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  # scale_fill_manual(values = grey_palette) +
  labs(
    x = "Root Stress",
    y = "F0 (Hz)",
    fill = "Syllable Position"
  ) +
  shared_theme
print(cs_s1s2s3_f0_plot)
ggsave("fixed_mobile_f0.png", plot = cs_s1s2s3_f0_plot, width = 12, height = 5, dpi = 300, bg = "white")

# Plot 1: Duration
cs_s1s2s3_plot <- ggplot(summary_cs_all_shift, aes(x = factor(StressedSyll), y = mean_dur, fill = SyllPos)) +
  facet_wrap(~ShiftDirect) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_dur - se_dur, ymax = mean_dur + se_dur),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  labs(
    x = "Root Stress",
    y = "Duration (ms)"
  ) +
  shared_theme

# Plot 2: Intensity
cs_s1s2s3_intensity_plot <- ggplot(summary_cs_all_shift, aes(x = factor(StressedSyll), y = mean_dB, fill = SyllPos)) +
  facet_wrap(~ShiftDirect) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_dB - se_dB, ymax = mean_dB + se_dB),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  labs(
    x = "Root Stress",
    y = "Intensity (dB)"
  ) +
  shared_theme

# Plot 3: F0 (with legend)
cs_s1s2s3_f0_plot <- ggplot(summary_cs_all_shift, aes(x = factor(StressedSyll), y = mean_f0, fill = SyllPos)) +
  facet_wrap(~ShiftDirect) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_f0 - se_f0, ymax = mean_f0 + se_f0),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  labs(
    x = "Root Stress",
    y = "F0 (Hz)",
    fill = "Syllable Position"
  ) +
  shared_theme +
  theme(legend.position = "right")  # Keep legend for F0

# Combine all plots horizontally
combined_plot <- cs_s1s2s3_plot + cs_s1s2s3_intensity_plot + cs_s1s2s3_f0_plot + plot_layout(ncol = 3)

# Print combined plot
print(combined_plot)

# Save combined plot
ggsave("fixed_mobile_combined_horizontal.png", plot = combined_plot, width = 20, height = 6, dpi = 300, bg = "white")


#' 
#' ## Model D
#' 
#' - Fixed roots - stress does not move to the s3.
#' - Mobile (forward) shifts - stress does move to the s3. 
#' 
## -----------------------------------------------------------------------------
# view(cs_roots_balanced) 
# # The reference level 'fixed' stress and against which 'mobile' will be compared to. 
# 
# # Convert ShiftDirect to a factor since there are two cat levels
# cs_roots_balanced$WordForm <- factor(cs_roots_balanced$WordForm)  
# # cs_roots_balanced$StressedSyll <- factor(cs_roots_balanced$StressedSyll)  
# # Set ref level
# cs_roots_balanced$WordForm <- relevel(cs_roots_balanced$WordForm, ref = "uninflected")
# 
# cs_roots_balanced_fixed <- cs_roots_balanced %>%
#   filter(ShiftDirect == "fixed")
# 
# cs_roots_balanced_mobile <- cs_roots_balanced %>%
#   filter(ShiftDirect == "mobile")
# 
# 
# # Model predictions for fixed roots
# # Our predictions is the stress remaints on the root
# 
# model_d_dur <- lmer(Duration_in_ms ~ WordForm + (1|Word) +(1|Speaker), data=cs_roots_balanced_fixed)
# summary(model_d_dur)
#  
# model_d_int <- lmer(Mean_dB ~ WordForm + (1|Word) +(1|Speaker), data=cs_roots_balanced_fixed)
# summary(model_d_int)
# 
# model_d_f0 <- lmer(MeanF0 ~ WordForm + (1|Word) +(1|Speaker), data=cs_roots_balanced_fixed)
# summary(model_d_f0)
# 
# # model_d_maxf0 <- lmer(ratio_max_fo ~ ShiftDirect*factor(root_stress) + (1|root) +(1|Speaker), data=cs_roots_balanced)
# # summary(model_d_maxf0)
# 
# # Model predictions for mobile roots
# # Out prediction is the stress shifts to the s3, therefore s1:s2 ratio should decrease
# 
# 
# model_d_dur_m <- lmer(ratio_s1_s2_dur ~ WordForm + (1|root) +(1|Speaker), data=cs_roots_balanced_mobile)
# summary(model_d_dur_m)
#  
# model_d_int_m <- lmer(ratio_mean_int ~ WordForm + (1|root) +(1|Speaker), data=cs_roots_balanced_mobile)
# summary(model_d_int_m)
# 
# model_d_f0_m <- lmer(ratio_mean_f0 ~ WordForm + (1|root) +(1|Speaker), data=cs_roots_balanced_mobile)
# summary(model_d_f0_m)


#### New model testing the effect of StressShift on Stress correlates

#### New model for mobile vs fixed stress

cs_mobile_fixed <- new_df %>%
  filter(Language == "CS", SyllPos %in% c("s1", "s2")) %>%
  select(Speaker, Word, Stress, WordForm, StressShift, ShiftDirect, Duration_in_ms, Mean_dB, MeanF0)  %>%
  filter(ShiftDirect %in% c("forward", "na")) %>%
  mutate(ShiftDirect = recode(ShiftDirect,
                              "forward" = "mobile",
                              "na" = "fixed"))

# Does stress mobility affect realization differently in root vs suffixed forms?
# Better model
mobile_model_3way <- lmer(Duration_in_ms ~ Stress*ShiftDirect*WordForm + (1|Speaker) + (1|Word), data = cs_mobile_fixed)
summary(mobile_model_3way)

mobile_int <- lmer(Mean_dB ~ Stress*ShiftDirect*WordForm + (1|Speaker) + (1|Word), data = cs_mobile_fixed)
summary(mobile_int)
#mobile_model_2way <- lmer(Duration_in_ms ~ Stress*ShiftDirect + Stress*WordForm + (1|Speaker) + (1|Word), data = cs_mobile_fixed)
# summary(mobile_model_2way)

# Include only inflected CS tokens 
# Maybe the roots with mobile stress will exhibit shift toward final s3?

cs_mobile_all <- new_df  %>%
  filter(Language == "CS",
    WordForm == 'inflected')%>%
  select(Speaker, Word, Stress, WordForm, StressShift, ShiftDirect, SyllPos, Duration_in_ms, Mean_dB, MeanF0)  %>%
  filter(ShiftDirect %in% c("forward", "na")) %>%
  mutate(ShiftDirect = recode(ShiftDirect,
                              "forward" = "mobile",
                              "na" = "fixed"))

view(cs_mobile_all)
mobile_inflected <- lmer(Duration_in_ms ~ Stress*ShiftDirect + (1|Speaker) + (1|Word), data = cs_mobile_all)
summary(mobile_inflected)


#' # Conclusion
#' 
#' These results indicate that stress in CS nouns remains on the Russian root, while final syllables exhibit Kazakh-style lengthening, supporting a hybrid prosodic pattern. This outcome suggests that bilinguals represent and coordinate multiple phonological systems even at the word level.
#' 
#' 
#' ## RSession info
## ----echo = FALSE-------------------------------------------------------------
sessionInfo()

#' 
