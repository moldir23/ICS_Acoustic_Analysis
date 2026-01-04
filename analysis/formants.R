# Upload necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(modelr)
library(readr)
library(tidyr)
library(readxl)
library(purrr)
library(stringi)
library(lme4)
library(lmerTest)
library(emmeans)
library(striprtf)

df_1 <- read_csv("/Users/moldir/Documents/Fall_2024_IndStudy/CompletedAnnotations/formants_txt/formants1 copy.csv", locale = locale(encoding = "UTF-16"))
#(df_1)

df_3 <- read_csv("/Users/moldir/Documents/Fall_2024_IndStudy/CompletedAnnotations/formants_txt/formants3 copy.csv", locale = locale(encoding = "UTF-16"))
#view(df_3)

df_2 <- read_csv("/Users/moldir/Documents/Fall_2024_IndStudy/CompletedAnnotations/formants_txt/formants2 copy.csv", locale = locale(encoding = "UTF-16"))
#view(df_2)

df_4 <- read_csv("/Users/moldir/Documents/Fall_2024_IndStudy/CompletedAnnotations/formants_txt/formants4 copy.csv", locale = locale(encoding = "UTF-16"))
#view(df_4)

df_5 <- read_csv("/Users/moldir/Documents/Fall_2024_IndStudy/CompletedAnnotations/formants_txt/formants5 copy.csv", locale = locale(encoding = "UTF-16"))
#view(df_5)

df_6 <- read_csv("/Users/moldir/Documents/Fall_2024_IndStudy/CompletedAnnotations/formants_txt/formants6 copy.csv", locale = locale(encoding = "UTF-16"))
#view(df_6)

df_7 <- read_csv("/Users/moldir/Documents/Fall_2024_IndStudy/CompletedAnnotations/formants_txt/formants7 copy.csv", locale = locale(encoding = "UTF-16"))
#view(df_7)

df_8 <- read_csv("/Users/moldir/Documents/Fall_2024_IndStudy/CompletedAnnotations/formants_txt/formants8 copy.csv", locale = locale(encoding = "UTF-16"))
#view(df_8)



df_combined <- bind_rows(df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8)
#df_combined
 

vowels <- c(
  "ij","ie", "ja",      # multi‐char vowels 
  "i","e","ɛ","a","ə","ɨ","u","o","ʊ","ɔ","y","æ","ɪ","ʏ","œ","ɑ","ɵ","ʌ"
)
# sort by length 
vowels <- vowels[order(nchar(vowels), decreasing = TRUE)]
vowel_pat <- paste0("^(", paste(vowels, collapse = "|"), ")")

# filter to keep only vowels
df_vowels <- df_combined %>%
  filter(
    # strip off any “_stressed” or “_unstressed”  for the purpose of matching
    str_detect(str_remove(phoneme, "_.*$"), vowel_pat)
  )

#view(df_vowels)

df_with_syllables <- df_vowels %>%
  group_by(word,file) %>%                         
  arrange(time, .by_group = TRUE) %>%        
  mutate(syllables = paste0("s", row_number())) %>%  
  ungroup()

# take a look
#view(df_with_syllables)

# Upload metadata wordlist and vowel quality
wordlist <- read_csv("/Users/moldir/Documents/Fall_2024_IndStudy/CompletedAnnotations/metadata_csv/WordList.csv")

wordlist <- wordlist %>%
  select(Word, Language, WordForm, LatinScript, Gloss) %>%
  rename(word = Word)

#wordlist

# combine two dataframes
merged_df_formants <- left_join(df_with_syllables, wordlist, by = "word")

# Split 'phoneme' column into 'vowel' and 'stress' at the underscore
merged_df_formants <- merged_df_formants %>%
  separate(phoneme, into = c("vowel", "stress"), sep = "_")

#view(merged_df_formants)

# Update Stress column for Kaz, RUS and CS tokens
# Stress: un/stressed = RUS (all syllables); 
# CS (s1/s2) = un/stressed, CS s3 = NA (bc Kaz suffix); 
# Kaz (all syllables) = NA
# So, different levels of stress for each language

merged_df_formants <- merged_df_formants %>%
  mutate(
    stress = case_when(
      # All Kaz tokens should have NA
      Language == "Kaz" ~ NA_character_,
      
      # CS tokens: s1 or s2 - fill NA or "" with "unstressed"
      # Language == "CS" & SyllPos %in% c("s1", "s2") & (is.na(Stress) | Stress == "") ~ "unstressed",
      
      # CS tokens: s3 should always be NA
      Language == "CS" & syllables == "s3" ~ NA_character_,
      
      # Rus tokens: fill NA or "" with "unstressed"
      #Language == "Rus" & (is.na(Stress) | Stress == "") ~ "unstressed",
      
      # Else keep existing value
      TRUE ~ stress
    )
  )

#view(merged_df_formants)

merged_df_formants <- merged_df_formants %>%
  mutate(
    stress = case_when(
      stress %in% c("unstressedz", "unstresed", "unstressed`", "unstressedd") ~ "unstressed",
      stress == "stresssed" ~ "stressed",
      TRUE ~ stress
    )
  )

merged_df_formants %>%
  count(Language, stress) %>%
  arrange(Language, desc(n))

# Upload vowel quality files

vq_kaz <- read_csv("/Users/moldir/Documents/Fall_2024_IndStudy/CompletedAnnotations/metadata_csv/VowelQualityKaz.csv")
vq_rus <- read_csv("/Users/moldir/Documents/Fall_2024_IndStudy/CompletedAnnotations/metadata_csv/VowelQualityRus.csv")
  
vq_kaz <- vq_kaz%>%
  select(VowelIPA, VowelHeight, VowelBackness) %>%
  rename(vowel = VowelIPA) %>%
  distinct()
vq_kaz 

vq_rus <- vq_rus %>%
  select(VowelIPA, VowelHeight, VowelBackness) %>%
  rename(vowel = VowelIPA) %>%
  distinct()
vq_rus

# Update df with vowel qualities 

# Filter rows for merging
df_filtered <- merged_df_formants %>%
  filter((Language == "Kaz") | (Language == "CS" & syllables == "s3"))

# Left join only for relevant rows
df_joined <- df_filtered %>%
  left_join(vq_kaz, by = "vowel")

# Filter rows for merging
df_filtered_rus <- merged_df_formants %>%
  filter((Language == "Rus") | (Language == "CS" & syllables == c("s1", "s2")))

# Left join only for relevant rows
df_joined_rus <- df_filtered_rus %>%
  left_join(vq_rus, by = "vowel")

# Combine everything
df_final_formants <- bind_rows(df_joined, df_joined_rus)


# Normalize formant values
df_final_formants <- df_final_formants %>%
  group_by(file)  %>%
  mutate(
    Norm_F1 = as.numeric(scale(F1)),
    Norm_F2 = as.numeric(scale(F2))
  )
  
df_final_formants <- df_final_formants %>%
  mutate(VowelHeight = case_when(
    VowelHeight %in% c("mid", "low") ~ "non-high",
    TRUE ~ VowelHeight
  ))

# Update the dataframe by adding new 'Speaker' and 'Gender' columns.
# 'Speaker' extracts the "Speaker_#" part,
# and 'Gender' extracts either "male" or "female"
df_final_formants <- df_final_formants %>%
  mutate(
    Speaker = str_extract(file, "^(Speaker_\\d+)"),
    Gender  = str_extract(file, "(male|female)")
  )

view(df_final_formants)

# Visualize formants for each language

#### Kazakh vowels ####

# Compute vowel centroids
centroids <- df_final_formants %>%
  filter(Language == "Kaz") %>%
  filter(!vowel %in% c("aj", "e", "a")) %>%
  group_by(vowel) %>%
  summarise(
    mean_F1 = mean(Norm_F1, na.rm = TRUE),
    mean_F2 = mean(Norm_F2, na.rm = TRUE)
  )

# Kazakh Vowel plot with ellipses and labels

df_final_formants %>%
  filter(Language == "Kaz") %>%
  filter(!vowel %in% c("aj", "e", "a")) %>%
  filter(between(Norm_F1, -2.5, 2.5), between(Norm_F2, -2, 2)) %>%
  ggplot(aes(x = Norm_F1, y = Norm_F2, color = vowel)) +
  geom_point(size = 3) +
  stat_ellipse(type = "norm", level = 0.68) +
  geom_text(
    data = centroids,
    aes(x = mean_F2, y = mean_F1, label = vowel),
    color = "black",
    fontface = "bold",
    size = 4,
    inherit.aes = FALSE
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("Vowel backness (Norm_F2)") +
  ylab("Vowel height (Norm_F1)") +
  ggtitle("Kazakh Vowel Dispersion")

# Kazakh vowel dispersion by Speaker

df_final_formants %>%
  filter(Language == "Kaz") %>%
  filter(!vowel %in% c("aj", "e", "a")) %>%
  filter(between(Norm_F1, -2.5, 2.5), between(Norm_F2, -2, 2)) %>%
  ggplot(aes(x = Norm_F2, y = Norm_F1, color = vowel)) +
  facet_wrap(~Speaker) +
  geom_point(size = 3) +
  stat_ellipse(type = "norm", level = 0.68) +
  geom_text(
    data = centroids,
    aes(x = mean_F2, y = mean_F1, label = vowel),
    color = "black",
    fontface = "bold",
    size = 4,
    inherit.aes = FALSE
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("Vowel backness (Norm_F2)") +
  ylab("Vowel height (Norm_F1)") +
  ggtitle("Kazakh Vowel Dispersion by Speaker")


#### Russian Vowels ####

# Compute vowel centroids
centroids <- df_final_formants %>%
  filter(Language == "Rus") %>%
  #filter(!vowel %in% c("aj", "e", "a")) %>%
  group_by(vowel) %>%
  summarise(
    mean_F1 = mean(Norm_F1, na.rm = TRUE),
    mean_F2 = mean(Norm_F2, na.rm = TRUE)
  )


# Vowel plot with ellipses and labels


df_final_formants %>%
  filter(Language == "Rus") %>%
  #filter(!vowel %in% c("aj", "e", "a")) %>%
  filter(between(Norm_F1, -2.5, 2.5), between(Norm_F2, -2, 2)) %>%
  ggplot(aes(x = Norm_F2, y = Norm_F1, color = vowel)) +
  geom_point(size = 3) +
  stat_ellipse(type = "norm", level = 0.68) +
  geom_text(
    data = centroids,
    aes(x = mean_F2, y = mean_F1, label = vowel),
    color = "black",
    fontface = "bold",
    size = 4,
    inherit.aes = FALSE
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("Vowel backness (Norm_F2)") +
  ylab("Vowel height (Norm_F1)") +
  ggtitle("Russian Vowel Dispersion")

# Vowel dispersion by Speaker

df_final_formants %>%
  filter(Language == "Rus") %>%
  #filter(!vowel %in% c("aj", "e", "a")) %>%
  filter(between(Norm_F1, -2.5, 2.5), between(Norm_F2, -2, 2)) %>%
  ggplot(aes(x = Norm_F2, y = Norm_F1, color = vowel)) +
  facet_wrap(~Speaker) +
  geom_point(size = 3) +
  stat_ellipse(type = "norm", level = 0.68) +
  geom_text(
    data = centroids,
    aes(x = mean_F2, y = mean_F1, label = vowel),
    color = "black",
    fontface = "bold",
    size = 4,
    inherit.aes = FALSE
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("Vowel backness (Norm_F2)") +
  ylab("Vowel height (Norm_F1)") +
  ggtitle("Russian Vowel Dispersion by Speaker")


# Stress
df_final_formants %>%
  filter(Language == "Rus") %>%
  drop_na(Norm_F1, Norm_F2, stress)%>%
  filter(between(Norm_F1, -2, 2), between(Norm_F2, -2, 2)) %>%
  ggplot(aes(x = Norm_F2, y = Norm_F1, color = stress, )) +
  geom_point(size = 3) +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("Vowel backness (Norm_F2)") +
  ylab("Vowel height (Norm_F1)") +
  ggtitle("Russian Vowel Dispersion by Stress")



#### F1-F2 by vowel category ####


# Compute centroids (vowel category means by stress)
centroids_rus <- df_final_formants %>%
  filter(Language == "Rus") %>%
  filter(vowel != "ja")%>%
  #filter(between(Norm_F1, -2.5, 2.5), between(Norm_F2, -2, 2)) %>%
  group_by(vowel, stress) %>%
  summarise(
    mean_F1 = mean(Norm_F1, na.rm = TRUE),
    mean_F2 = mean(Norm_F2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(label = ifelse(stress == "stressed", paste0(vowel, "+"), vowel))

# Plot only centroids (no raw data points or ellipses)
ggplot(centroids_rus, aes(x = mean_F2, y = mean_F1)) +
  geom_text(aes(label = label), size = 5, fontface = "bold") +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("F2") +
  ylab("F1") +
  ggtitle("F1 by F2 Russian Vowel Plot") +
  theme_minimal(base_size = 14)


#### CS vowels ####

# CS tokens by stress and syllable position
# df_final_formants %>%
#   filter(Language == "CS" & syllables %in% c("s1", "s2") & stress %in% c("stressed", "unstressed")) %>%
#   drop_na(VowelHeight, VowelBackness, Norm_F1, Norm_F2, stress)%>%
#   #filter(between(Norm_F1, -2.5, 4), between(Norm_F2, -3, 2.5)) %>%
#   ggplot() +
#   geom_point(
#     aes(x = Norm_F2, y = Norm_F1, color = stress), size = 3
#   ) +
#   facet_wrap(~syllables) +
#   scale_x_reverse() +
#   scale_y_reverse() +
#   xlab("Vowel backness (Norm_F2)") +
#   ylab("Vowel height (Norm_F1)") +
#   ggtitle("CS Vowel Dispersion by Stress and Syllable Position")

# by wordform
df_final_formants %>%
  filter(Language == "CS" & syllables %in% c("s1", "s2")) %>%
  filter(stress %in% c("stressed", "unstressed")) %>%
  drop_na(Norm_F1, Norm_F2, stress)%>%
  filter(between(Norm_F1, -2.5, 2.5), between(Norm_F2, -2.5, 2.5)) %>%
  ggplot() +
  geom_point(
    aes(x = Norm_F2, y = Norm_F1, color = stress), size = 3
  ) +
  facet_wrap(~WordForm) +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("Vowel backness (Norm_F2)") +
  ylab("Vowel height (Norm_F1)") +
  ggtitle("CS Vowel Dispersion by Word Form and Stress")



## Boxplot F1, F2 by Stress and Syllable Position

df_final_formants %>%
  filter(Language == "CS" & syllables %in% c("s1", "s2")) %>%
  filter(stress %in% c("stressed", "unstressed")) %>%
  drop_na(Norm_F1, Norm_F2, stress, WordForm)%>%
  filter(between(Norm_F1, -2.5, 2.5), between(Norm_F2, -2.5, 2.5)) %>%
  ggplot() +
  geom_boxplot(
    aes(x = stress, y = Norm_F1, color = stress)
  ) +
  #facet_wrap(~syllables) +
  theme_minimal() +
  xlab("Stress") +
  ylab("Norm_F1") 
  #ggtitle("F1 in CS by Stress")

df_final_formants %>%
  filter(Language == "CS" & syllables %in% c("s1", "s2")) %>%
  filter(stress %in% c("stressed", "unstressed")) %>%
  drop_na(Norm_F1, Norm_F2, stress, WordForm)%>%
  #filter(between(Norm_F1, -2.5, 2.5), between(Norm_F2, -2.5, 2.5)) %>%
  ggplot() +
  geom_boxplot(
    aes(x = stress, y = Norm_F2, color = stress)
  ) +
  #facet_wrap(~syllables) +
  theme_minimal() +
  xlab("Stress") +
  ylab("Norm_F2") 
  #ggtitle("F2 in CS by Stress")

## Boxplot F1, F2 by WordForm
df_final_formants %>%
  filter(Language == "CS" & syllables %in% c("s1", "s2")) %>%
  filter(stress %in% c("stressed", "unstressed")) %>%
  drop_na(Norm_F1, Norm_F2, stress, WordForm)%>%
  filter(between(Norm_F1, -2.5, 2.5), between(Norm_F2, -2.5, 2.5)) %>%
  ggplot() +
  geom_boxplot(
    aes(x = WordForm, y = Norm_F1, color = stress)
  ) +
  theme_minimal() +
  facet_wrap(~syllables) +
  xlab("Word Form") +
  ylab("Norm_F1") #+
  #ggtitle("F1 in CS by Word Form")

df_final_formants %>%
  filter(Language == "CS" & syllables %in% c("s1", "s2")) %>%
  filter(stress %in% c("stressed", "unstressed")) %>%
  drop_na(Norm_F1, Norm_F2, stress, WordForm)%>%
  filter(between(Norm_F1, -2.5, 2.5), between(Norm_F2, -2.5, 2.5)) %>%
  ggplot() +
  geom_boxplot(
    aes(x = WordForm, y = Norm_F2, color = stress)
  ) +
  facet_wrap(~syllables) +
  theme_minimal() +
  xlab("Word Form") +
  ylab("Norm_F2") #+
  #ggtitle("F2 in CS by Word Form")


# F1

# S1 stressed, s2 unstressed
df_final_formants %>%
  filter(Language == "CS") %>%
  filter(stress %in% c("stressed", "unstressed")) %>%
  group_by(word) %>%
  filter(
    any(syllables == "s1" & stress == "stressed") &
      any(syllables == "s2" & stress == "unstressed")
  ) %>%
  ungroup() %>%
  drop_na(Norm_F1, Norm_F2, stress, WordForm) %>%
  filter(between(Norm_F1, -2.5, 2.5), between(Norm_F2, -2.5, 2.5)) %>%
  ggplot() +
  geom_boxplot(
    aes(x = syllables, y = Norm_F1, color = syllables)
  ) +
  facet_wrap(~WordForm) +
  xlab("Syllable Position") +
  ylab("F1") +
  ggtitle("F1 in CS: Word Form Syllable Position Stress")

# By word  F2
# S1 stressed, s2 unstressed

df_final_formants %>%
  filter(Language == "CS") %>%
  filter(stress %in% c("stressed", "unstressed")) %>%
  group_by(word) %>%
  filter(
    any(syllables == "s1" & stress == "stressed") &
      any(syllables == "s2" & stress == "unstressed")
  ) %>%
  ungroup() %>%
  drop_na(Norm_F1, Norm_F2, stress, WordForm) %>%
  filter(between(Norm_F1, -2.5, 2.5), between(Norm_F2, -2.5, 2.5)) %>%
  ggplot() +
  geom_boxplot(
    aes(x = syllables, y = Norm_F2, color = syllables)
  ) +
  facet_wrap(~WordForm) +
  xlab("Syllable Position") +
  ylab("F2") +
  ggtitle("F2 in CS: Word Form × Syllable Position × Stress")


# F1
# S1 unstressed, s2 stressed
df_final_formants %>%
  filter(Language == "CS") %>%
  filter(stress %in% c("stressed", "unstressed")) %>%
  group_by(word) %>%
  filter(
    any(syllables == "s1" & stress == "unstressed") &
      any(syllables == "s2" & stress == "stressed")
  ) %>%
  ungroup() %>%
  drop_na(Norm_F1, Norm_F2, stress, WordForm) %>%
  filter(between(Norm_F1, -2.5, 2.5), between(Norm_F2, -2.5, 2.5)) %>%
  ggplot() +
  geom_boxplot(
    aes(x = syllables, y = Norm_F1, color = syllables)
  ) +
  facet_wrap(~WordForm) +
  xlab("Syllable Position") +
  ylab("F1") +
  ggtitle("F1 in CS: Word Form × Syllable Position × Stress")

# By word  F2
# S1 stressed, s2 unstressed

df_final_formants %>%
  filter(Language == "CS") %>%
  filter(stress %in% c("stressed", "unstressed")) %>%
  group_by(word) %>%
  filter(
    any(syllables == "s1" & stress == "unstressed") &
      any(syllables == "s2" & stress == "stressed")
  ) %>%
  ungroup() %>%
  drop_na(Norm_F1, Norm_F2, stress, WordForm) %>%
  filter(between(Norm_F1, -2.5, 2.5), between(Norm_F2, -2.5, 2.5)) %>%
  ggplot() +
  geom_boxplot(
    aes(x = syllables, y = Norm_F2, color = syllables)
  ) +
  facet_wrap(~WordForm) +
  xlab("Syllable Position") +
  ylab("F2") +
  ggtitle("F2 in CS: Word Form × Syllable Position × Stress")



# Full dataset
cs_tokens <- df_final_formants %>%
  filter(Language == "CS" & syllables %in% c("s1", "s2")) 
  #filter(stress %in% c("stressed", "unstressed")) 
cs_tokens$WordForm <- factor(cs_tokens$WordForm, levels = c("uninflected", "inflected"))
cs_tokens$stress <- factor(cs_tokens$stress, levels = c("stressed", "unstressed"))

lm_f1_3 <- lmer(Norm_F1 ~ stress * WordForm + (1|word), data = cs_tokens)
summary(lm_f1_3)

lm_f2_3 <- lmer(Norm_F2 ~ stress * WordForm + (1|word), data = cs_tokens)
summary(lm_f2_3)

## So lme models didn't yield significant results. 

# Let's try pair-wise test
formant_cs <- cs_tokens %>%
  select(word, vowel, stress, Language, WordForm,syllables, Norm_F1, Norm_F2, Speaker, Gender)
#write_csv(formant_cs,"/Users/aidyn/Documents/Fall_2024_IndStudy/CompletedAnnotations/cs_formants.csv")
view(formant_cs)

# Define the identifier columns (to remain as-is in the wide format)
id_cols <- c("word", "Language", "WordForm", "Speaker", "Gender")

# Define the measurement columns (these will be split across syllables)
pivot_cols <- formant_cs %>%
  select(-all_of(c(id_cols, "syllables"))) %>%
  names()

# Perform pivoting
cs_formant_wide <- formant_cs %>%
  pivot_wider(
    id_cols = all_of(id_cols),
    names_from = syllables,
    values_from = all_of(pivot_cols),
    names_sep = "_"
  )

# View the result
view(cs_formant_wide)




### Formant Analysis 
# Kazakh: does F1 and F2 change by syllable position? since s3 is longer in inflected words,
# s2 is longer in bare roots. lmer(Norm_F1 ~ syllables(s1,s2,s3) + (1|word), data = kazakh )
# two subsets, roots vs. inflected forms

kaz_roots <- df_final_formants %>%
  filter(Language == "Kaz",
         WordForm == "uninflected") 
  
kaz_suffix <- df_final_formants %>%
  filter(Language == "Kaz",
         WordForm == "inflected") 
# Roots 
kazroot_lm_f1 <- lmer(Norm_F1 ~ syllables + (1|word), data = kaz_roots)
summary(kazroot_lm_f1)

kazroot_lm_f2 <- lmer(Norm_F2 ~ syllables + (1|word), data = kaz_roots)
summary(kazroot_lm_f2)

# suffix
kazsuffix_lm_f1 <- lmer(Norm_F1 ~ syllables + (1|word), data = kaz_suffix)
summary(kazsuffix_lm_f1)

kazsuffix_lm_f2 <- lmer(Norm_F2 ~ syllables + (1|word), data = kaz_suffix)
summary(kazsuffix_lm_f2)

# Russian: does F1 and F2 change by Stress placement?
# Stressed vowels should retain full quality while unstressed ones undergo reduction.
# lmer(F1/F2 ~ stress + (1|word), data = russian)

rus_formant <- df_final_formants %>%
  filter(Language == "Rus") %>%
  filter(!is.na(stress))

rus_f1 <- lmer(Norm_F1 ~ stress + (1|word), data = rus_formant)
summary(rus_f1)

rus_f2 <- lmer(Norm_F2 ~ stress + (1|word), data = rus_formant)
summary(rus_f2)

# CS: does F1 and F2 change by Stress and/or Inflection?
# CS tokens, s1 and s2 only
# (1)Stress: lmer(F1/F2 ~ stress + (1|word), data=cs)

stress_cs_f1 <- lmer(Norm_F1 ~ stress + (1|word), data=formant_cs)
summary(stress_cs_f1)

stress_cs_f2 <- lmer(Norm_F2 ~ stress + (1|word), data=formant_cs)
summary(stress_cs_f2)



# Save df
write_csv(df_final_formants,"/Users/aidyn/Documents/Fall_2024_IndStudy/CompletedAnnotations/formants_dataset.csv")



#### New approach ####

####  F1 and F2 in CS by Vowel type ####

## Based on Wright 2003 easy/hard vowel analysis


## Take mean F1 and F2 for each vowel type and plot them by stressed/unstressed pair


df_final_formants <- df_final_formants %>%
  mutate(vowel = case_when(
    vowel == "i" & stress == "unstressed" & Language == "CS" ~ "ɪ",
    TRUE ~ vowel  # keep original value otherwise
  ))


# Compute centroids (vowel category means by stress)
centroids_cs <- df_final_formants %>%
  filter(Language == "CS" & syllables %in% c("s1", "s2")) %>%
  filter(stress %in% c("stressed", "unstressed")) %>%
  drop_na(Norm_F1, Norm_F2, stress, WordForm)%>%
  filter(!vowel %in% c("ja", "ɑ"))%>%
  #filter(between(Norm_F1, -2.5, 2.5), between(Norm_F2, -2, 2)) %>%
  group_by(vowel, stress) %>%
  summarise(
    mean_F1 = mean(Norm_F1, na.rm = TRUE),
    mean_F2 = mean(Norm_F2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(label = ifelse(stress == "stressed", paste0(vowel, "+"), vowel))

# Plot only centroids (no raw data points or ellipses)
ggplot(centroids_cs, aes(x = mean_F2, y = mean_F1)) +
  geom_text(aes(label = label), size = 4, fontface = "bold") +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab(" Mean F2") +
  ylab("Mean F1") +
  #ggtitle("F1 by F2 Vowel Plot (CS, Stressed vs. Unstressed)") +
  theme_minimal(base_size = 12)

view(centroids_cs)

## new visualization attempt:

library(ggrepel)

shrink_factor <- 0.75  # arrow will be 50% shorter

# Compute vowel centroids (F1/F2) by stress
centroids_cs <- df_final_formants %>%
  filter(Language == "CS", syllables %in% c("s1", "s2")) %>%
  filter(stress %in% c("stressed", "unstressed")) %>%
  drop_na(Norm_F1, Norm_F2, stress, WordForm) %>%
  filter(!vowel %in% c("ja", "ɑ")) %>%
  group_by(vowel, stress) %>%
  summarise(
    mean_F1 = mean(Norm_F1, na.rm = TRUE),
    mean_F2 = mean(Norm_F2, na.rm = TRUE),
    .groups = "drop"
  )

# wide format
centroids_wide <- centroids_cs %>%
  pivot_wider(
    names_from = stress,
    values_from = c(mean_F1, mean_F2),
    names_glue = "{.value}_{stress}"
  ) %>%
  filter(!is.na(mean_F1_stressed), !is.na(mean_F1_unstressed)) %>%
  mutate(distance = sqrt((mean_F1_stressed - mean_F1_unstressed)^2 +
                           (mean_F2_stressed - mean_F2_unstressed)^2)) %>%
  filter(distance > 0.01)  # include even small shifts like 'i'

# Compute shortened arrows
centroids_wide <- centroids_wide %>%
  mutate(
    dx = mean_F2_unstressed - mean_F2_stressed,
    dy = mean_F1_unstressed - mean_F1_stressed,
    mean_F2_arrow = mean_F2_stressed + shrink_factor * dx,
    mean_F1_arrow = mean_F1_stressed + shrink_factor * dy
  )

# pecial vowels (a, o, ɨ - schwa)
schwa_centroid <- centroids_cs %>%
  filter(vowel == "ə", stress == "unstressed") %>%
  select(mean_F1_schwa = mean_F1, mean_F2_schwa = mean_F2)

vowels_to_schwa <- centroids_cs %>%
  filter(vowel %in% c("a", "o", "ɨ"), stress == "stressed") %>%
  mutate(
    mean_F1_stressed = mean_F1,
    mean_F2_stressed = mean_F2,
    mean_F1_unstressed = schwa_centroid$mean_F1_schwa,
    mean_F2_unstressed = schwa_centroid$mean_F2_schwa,
    dx = mean_F2_unstressed - mean_F2_stressed,
    dy = mean_F1_unstressed - mean_F1_stressed,
    mean_F2_arrow = mean_F2_stressed + shrink_factor * dx,
    mean_F1_arrow = mean_F1_stressed + shrink_factor * dy
  )

# Combine arrows
arrows_all <- bind_rows(
  centroids_wide %>% select(vowel, mean_F1_stressed, mean_F2_stressed, mean_F1_arrow, mean_F2_arrow),
  vowels_to_schwa %>% select(vowel, mean_F1_stressed, mean_F2_stressed, mean_F1_arrow, mean_F2_arrow)
)

# Plot
ggplot() +
  # Arrows (shortened)
  geom_segment(
    data = arrows_all,
    aes(x = mean_F2_stressed, y = mean_F1_stressed,
        xend = mean_F2_arrow, yend = mean_F1_arrow),
    arrow = arrow(length = unit(0.15, "cm")),
    color = "gray40"
  ) +
  # Vowel labels by stress
  geom_text(
    data = centroids_cs,
    aes(x = mean_F2, y = mean_F1, label = vowel, color = stress),
    size = 4, fontface = "bold"
  ) +
  #scale_color_manual(values = c("stressed" = "firebrick", "unstressed" = "steelblue")) +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("Mean F2") +
  ylab("Mean F1") +
  theme_minimal(base_size = 12)


# Is the dispersion is significant?

lm_mean_f1 <- lm(mean_F1 ~ vowel + stress, data = centroids_cs)
summary(lm_mean_f1)

lm_mean_f2 <- lm(mean_F2 ~ vowel + stress, data = centroids_cs)
summary(lm_mean_f2)

# Run t-test as in the paper 
# o a ɨ when unstressed = ə ???
# i ustressed = ɪ

centroids_cs_wide <- centroids_cs %>%
  select(vowel, stress, mean_F1, mean_F2)%>%
  pivot_wider(
    names_from = stress,
    values_from = c(mean_F1, mean_F2),
    names_sep = "_"
  ) #%>%
#filter(!is.na(mean_F1_stressed), !is.na(mean_F1_unstressed))  # filter to keep only pairs


# extract ɪ's unstressed values
i_unstressed_vals <- centroids_cs_wide %>%
  filter(vowel == "ɪ") %>%
  select(mean_F1_unstressed, mean_F2_unstressed)

# update "i" row with ɪ's unstressed values
centroids_cs_wide <- centroids_cs_wide %>%
  mutate(
    mean_F1_unstressed = if_else(vowel == "i", i_unstressed_vals$mean_F1_unstressed, mean_F1_unstressed),
    mean_F2_unstressed = if_else(vowel == "i", i_unstressed_vals$mean_F2_unstressed, mean_F2_unstressed)
  ) %>%
  filter(vowel != "ɪ")  





#### Compute the Euclidean distance from the center of vowel space for each vowel ####

# Euclidean distance quantifies how far a vowel token is from the center of the vowel space, 
# providing a measure of its articulatory distinctiveness or degree of reduction.

# Euclidean distance from the center of the vowel space:
# distance = sqrt((Norm_F1 - center_F1)^2 + (Norm_F2 - center_F2)^2)


# Filter to CS tokens only
df_cs <- df_final_formants %>%
  filter(Language == "CS" & syllables %in% c("s1", "s2")) %>%
  filter(stress %in% c("stressed", "unstressed")) %>%
  filter(!is.na(Norm_F1), !is.na(Norm_F2))  


#  no grouping before computing the center
df_cs <- df_cs %>% ungroup()

# Compute center F1 and F2 for all vowels in the vowel space
center_F1 <- mean(df_cs$Norm_F1, na.rm = TRUE)
center_F2 <- mean(df_cs$Norm_F2, na.rm = TRUE)

# Check the results
# should say num 0.xxx
str(center_F1) 
str(center_F2)

df_cs_dispersion <- df_cs %>%
  mutate(
    vowel_space_distance = sqrt((Norm_F1 - center_F1)^2 + (Norm_F2 - center_F2)^2)
  ) %>%
  select(Speaker, vowel, stress, WordForm, Language,
         Norm_F1, Norm_F2, vowel_space_distance, everything())

df_cs_dispersion_vc <- df_cs_dispersion %>%
  group_by(vowel, stress) %>%
  summarise(mean_dispersion = mean(vowel_space_distance, na.rm = TRUE))

view(df_cs_dispersion_vc)


view(df_cs_dispersion)

### Visualization
## Bar height = average Euclidean distance from vowel space center
## Error bars = 95% confidence interval around that mean


# Vowel Dispersion by Stress in CS
df_cs_dispersion_vc <- df_cs_dispersion %>%
  filter(!vowel %in% c("ja", "ɑ"))%>%
  group_by(stress) %>%
  summarise(
    mean_dispersion = mean(vowel_space_distance, na.rm = TRUE),
    se = sd(vowel_space_distance, na.rm = TRUE) / sqrt(n()),
    ci95 = 1.96 * se,
    .groups = "drop"
  )

ggplot(df_cs_dispersion_vc, aes(x = stress, y = mean_dispersion, fill = stress)) +
  geom_col() +
  geom_errorbar(
    aes(ymin = mean_dispersion - ci95, ymax = mean_dispersion + ci95),
    width = 0.2,
    color = "black"
  ) +
  ylim(0, NA) +
  #labs(x = "", y = "Mean Dispersion", title = "Vowel Dispersion with 95% CI") +
  theme_minimal(base_size = 12)

## Stressed vowels are more dispersed than unstressed ones in the vowel space. 


# Vowel Dispersion by Vowel Category and Stress in CS
# Compute mean and CI per vowel and stress
df_cs_dispersion_vowel <- df_cs_dispersion %>%
  filter(!vowel %in% c("ja", "ɑ")) %>%
  group_by(vowel, stress) %>%
  summarise(
    mean_dispersion = mean(vowel_space_distance, na.rm = TRUE),
    se = sd(vowel_space_distance, na.rm = TRUE) / sqrt(n()),
    ci95 = 1.96 * se,
    .groups = "drop"
  )

# Plot with grouped bars by stress
ggplot(df_cs_dispersion_vowel, aes(x = vowel, y = mean_dispersion, fill = stress)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = mean_dispersion - ci95, ymax = mean_dispersion + ci95),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  ylim(0, NA) +
  labs(x = "Vowel", y = "Mean Dispersion (z-score formant space)") +
  theme_minimal(base_size = 12)

# there is high variability in some vowel types may be due to speaker variability or limited number of datapoints.


# df_cs_dispersion_vowel

# Check vowel types number 
df_cs_dispersion %>%
  count(vowel) %>%
  arrange(n)

df_cs %>%
  count(vowel) %>%
  arrange(n)

# Filter low freq vowels 

# Filter out low-frequency vowel-stress groups, summarize, reorder, and plot

# unstressed ʌ == o in most cases (5 cases of a in s2)
# o a ɨ when unstressed = ə ???
# i untressed = ɪ
# ɨ = only 2?

# unstressed ʌ == o most cases (5 cases of a in s2)
df_ʌ_tokens <- df_cs_dispersion %>%
  filter(vowel == "ʌ")

df_ʌ_tokens %>%
  pull(word, syllables)


## Dispersion by Vowel pair
df_cs_dispersion %>%
  # Define vowel pairings across stress
  filter(!vowel %in% c("ja", "ɑ")) %>%
  mutate(vowel_pair = case_when(
    vowel == "i" & stress == "stressed" ~ "i/ɪ",
    vowel == "ɪ" & stress == "unstressed" ~ "i/ɪ",
    vowel == "o" & stress == "stressed" ~ "o/ʌ",
    vowel == "ʌ" & stress == "unstressed" ~ "o/ʌ",
    TRUE ~ vowel  # leave others unchanged
  )) %>%
  
  # Filter out low-frequency vowel-stress pairs
  group_by(vowel_pair, stress) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  
  # Summarise by vowel pair and stress
  group_by(vowel_pair, stress) %>%
  summarise(
    mean_dispersion = mean(vowel_space_distance, na.rm = TRUE),
    se = sd(vowel_space_distance, na.rm = TRUE) / sqrt(n()),
    ci95 = 1.96 * se,
    .groups = "drop"
  ) %>%
  
  # Reorder vowel pairs by average dispersion
  group_by(vowel_pair) %>%
  mutate(overall_dispersion = mean(mean_dispersion)) %>%
  ungroup() %>%
  mutate(vowel_pair = fct_reorder(vowel_pair, overall_dispersion)) %>%
  
  # Plot
  ggplot(aes(x = vowel_pair, y = mean_dispersion, fill = stress)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = mean_dispersion - ci95, ymax = mean_dispersion + ci95),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  ylim(0, NA) +
  labs(x = "Vowel Pairs", 
       y = "Mean Dispersion (z-score formant space)") +
  theme_minimal(base_size = 12)

# Stressed vowels like i and e are more dispersed than a and o (similar to findings of Padgett,2005)
# However, overall, stressed vowels are more in periphery than unstressed ones.


## Vowel Dispersion by Speaker and Stress
df_cs_dispersion_speaker <- df_cs_dispersion %>%
  filter(!vowel %in% c("ja", "ɑ")) %>%
  group_by(Speaker, stress) %>%
  summarise(
    mean_dispersion = mean(vowel_space_distance, na.rm = TRUE),
    se = sd(vowel_space_distance, na.rm = TRUE) / sqrt(n()),
    ci95 = 1.96 * se,
    .groups = "drop"
  )

# Plot with grouped bars by stress
ggplot(df_cs_dispersion_speaker, aes(x = Speaker, y = mean_dispersion, fill = stress)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = mean_dispersion - ci95, ymax = mean_dispersion + ci95),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  ylim(0, NA) +
  labs(x = "Speaker", y = "Mean Dispersion (z-score formant space)") +
  theme_minimal(base_size = 12)
## Overall, 3/4 speakers are comparable. There is a slight variation among speakers but not too strong.

## Vowel Dispersion by Gender 
df_cs_dispersion_gender <- df_cs_dispersion %>%
  filter(!vowel %in% c("ja", "ɑ")) %>%
  group_by(Gender, stress) %>%
  summarise(
    mean_dispersion = mean(vowel_space_distance, na.rm = TRUE),
    se = sd(vowel_space_distance, na.rm = TRUE) / sqrt(n()),
    ci95 = 1.96 * se,
    .groups = "drop"
  )

# Plot with grouped bars by stress
ggplot(df_cs_dispersion_gender, aes(x = Gender, y = mean_dispersion, fill = stress)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = mean_dispersion - ci95, ymax = mean_dispersion + ci95),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  ylim(0, NA) +
  labs(x = "Gender", y = "Mean Dispersion (z-score formant space)") +
  theme_minimal(base_size = 12)

# Male speakers tend to have a larger dispersion.

## Vowel Dispersion by WordForm 
df_cs_dispersion_word <- df_cs_dispersion %>%
  filter(!vowel %in% c("ja", "ɑ")) %>%
  group_by(WordForm, stress) %>%
  summarise(
    mean_dispersion = mean(vowel_space_distance, na.rm = TRUE),
    se = sd(vowel_space_distance, na.rm = TRUE) / sqrt(n()),
    ci95 = 1.96 * se,
    .groups = "drop"
  )

# Plot with grouped bars by stress
ggplot(df_cs_dispersion_word, aes(x = WordForm, y = mean_dispersion, fill = stress)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = mean_dispersion - ci95, ymax = mean_dispersion + ci95),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  ylim(0, NA) +
  labs(x = "Word Form", y = "Mean Dispersion (z-score formant space)") +
  theme_minimal(base_size = 12)

# Overall, root CS tokens show slightly larger dispersion than inflected ones. However, the difference
# is very small.


### Statistical Testing 

# How stress, vowel identity, and word form predict the vowel's distance from the center of 
# the vowel space, while accounting for variability across speakers and words.

# Dependent Variable = vowel_space_distance
# Predictors = stress, vowel type, word form
# Random intercept = speaker and word

df_cs_dispersion <- df_cs_dispersion %>%
  filter(!vowel %in% c("ja", "ɑ")) %>%
  mutate(vowel = fct_relevel(vowel, "ə"),
       stress = fct_relevel(stress, "unstressed"))


vowel_disp_ml <- lmer(vowel_space_distance ~ stress + vowel + WordForm + (1|Speaker) + (1|word), data = df_cs_dispersion)
summary(vowel_disp_ml)


# Reference level: a stressed 
# A linear mixed-effects model was fit to predict vowel dispersion (measured as Euclidean 
# distance from the vowel space center) as a function of stress, vowel category, and word form,
# with random intercepts for speaker and word. The model revealed a significant effect of 
# stress, with unstressed vowels being less dispersed than stressed vowels (β = –0.24, 
# p = 0.004), indicating a general pattern of vowel centralization in unstressed positions. 
# Several vowel categories also showed significant deviations from the reference vowel. 
# Notably, the vowel /i/ exhibited the greatest increase in dispersion relative to the 
# reference vowel (β = 0.65, p < 0.001), followed by /ɪ/ (β = 0.43, p = 0.002), suggesting 
# these high vowels are pronounced more peripherally. In contrast, the schwa /ə/ was 
# significantly less dispersed than the reference (β = –0.35, p = 0.007), consistent with 
# its phonetic reduction. No reliable effect was observed for word form (β = –0.006, p = 0.93), 
# nor for several other vowel categories. The random effects structure indicated moderate variance 
# across words (SD = 0.156) and smaller variance across speakers (SD = 0.055), with most variability 
# captured at the residual level (SD = 0.508). These results support the hypothesis that stress enhances
# vowel distinctiveness, particularly for high and peripheral vowels, while unstressed vowels show 
# reduced articulatory dispersion.
# Although word form did not reach significance as a predictor (p = 0.93), the overall pattern
# suggests that inflected CS tokens still exhibit vowel reduction consistent with
# unstressed vowel centralization in Russian.


# Reference level: schwa unstressed 
# The model shows that compared to the baseline (unstressed schwa in inflected words), a number
# of vowels — particularly high front vowels like /i/, /ɪ/, and mid vowels like /e/, /ɛ/ —
# exhibit significantly greater vowel space dispersion, suggesting they occupy more peripheral 
# positions in the vowel space. The effect of stress is also significant, indicating that 
# stressed vowels are generally produced with greater articulatory distinctiveness than 
# unstressed ones. Interestingly, the WordForm effect (inflected vs. uninflected) is not 
# significant, implying that inflection status does not influence vowel dispersion in this 
# dataset. Some vowels (like /æ/, /ɨ/, and /u/) do not significantly differ from schwa in 
# dispersion, which may reflect overlap in articulatory realization or data sparsity.



# Create paired vowel categories

# vowel_pair_data <- df_cs_dispersion %>%
#   filter(!vowel %in% c("ja", "ɑ")) %>%
#   group_by(stress, vowel)%>%
#    filter(n() >= 10) 
#   
#   mutate(vowel = case_when(
#     # Pair "i" (stressed) and "ɪ" (unstressed)
#     vowel == "i" & stress == "stressed" ~ "i/ɪ",
#     vowel == "ɪ" & stress == "unstressed" ~ "i/ɪ",
#     
#     # Pair stressed o, a, ɨ with unstressed ə
#     vowel %in% c("o", "ɨ") & stress == "stressed" ~ "a/o/ɨ-ə",
#     vowel == "ə" & stress == "unstressed" ~ "a/o/ɨ-ə",
#     # Stressed o and unstressed ʌ
#     vowel == "o" & stress == "stressed" ~ "o/ʌ",
#     vowel == "ʌ" & stress == "unstressed" ~ "o/ʌ",
#     # Leave other vowels unchanged
#     TRUE ~ vowel
#   )) %>%
#   mutate(vowel = factor(vowel))
# 
# view(vowel_pair_data)

# Fit model using the updated vowel variable
# vowel_ml <- lmer(vowel_space_distance ~ stress * vowel + (1|Speaker) + (1|word), data = vowel_pair_data)
# summary(vowel_ml)



#### Russian tokens

# Filter to Rus tokens only
df_rus <- df_final_formants %>%
  filter(Language == "Rus") %>%
  filter(stress %in% c("stressed", "unstressed")) %>%
  filter(vowel != "ja") %>%
  filter(!is.na(Norm_F1), !is.na(Norm_F2))  


#  no grouping before computing the center
df_rus<- df_rus %>% ungroup()

# Compute center F1 and F2 for all vowels in the vowel space
center_F1 <- mean(df_rus$Norm_F1, na.rm = TRUE)
center_F2 <- mean(df_rus$Norm_F2, na.rm = TRUE)


df_rus_dispersion <- df_rus %>%
  mutate(
    vowel_space_distance = sqrt((Norm_F1 - center_F1)^2 + (Norm_F2 - center_F2)^2)
  ) %>%
  select(Speaker, vowel, stress, WordForm, Language,
         Norm_F1, Norm_F2, vowel_space_distance, everything())

df_rus_dispersion_vc <- df_rus_dispersion %>%
  group_by(vowel, stress) %>%
  summarise(mean_dispersion = mean(vowel_space_distance, na.rm = TRUE))

view(df_rus_dispersion_vc)
view(df_rus_dispersion)

## Testing
df_rus_dispersion <- df_rus_dispersion %>%
  mutate(vowel = fct_relevel(vowel, "ə"),
         stress = fct_relevel(stress, "unstressed"))

vowel_rus_ml <- lmer(vowel_space_distance ~ stress + vowel + (1|Speaker) + (1|word), data = df_rus_dispersion)
summary(vowel_rus_ml)


# The linear mixed-effects model shows that the unstressed schwa (ə) occupies a central and 
# relatively compact position in the vowel space of Kazakh-Russian bilinguals, with a mean 
# vowel space distance of 0.78. Compared to this baseline, several vowels — including high 
# vowels like /i/ and /u/, and back vowels like /o/ and /ʌ/ — exhibit significantly greater 
# dispersion (p < .05), reflecting their peripheral articulatory positions and possibly 
# greater clarity demands. Additionally, stressed vowels overall are significantly more 
# dispersed than unstressed ones (p = 0.014), indicating that stress enhances articulatory 
# distinctiveness across the vowel space. Notably, some vowels such as /æ/, /ɛ/, and /ɪ/ did 
# not differ significantly from schwa, suggesting potential overlap or neutralization in 
# reduced or unstressed positions.

#############################################################################################


# Prof C. Mayer's feedback: 
#### Reorganize dataset and run models on a new ds ####

## Vowels: stressed vs. unstressed pair (no need to label how unstressed vowels get reduced)
## Context for the lmer model: Russian inflected, Kazakh inflected, and Kazakh roots 
## The second predictor: stress
## Random effects: speaker + word 
## Dependent variable: vowel dispersion from the center of vowel space


# Filter to CS and Rus tokens only
df_rus_cs <- df_formant %>%
  filter(
    (Language == "CS" & syllables %in% c("s1", "s2") & stress %in% c("stressed", "unstressed")) |
      (Language == "Rus")
  ) %>%
  filter(!is.na(Norm_F1), !is.na(Norm_F2))

# view(df_rus_cs)


# Relabel Vowels 

# Upload and clean raw text with vowel labels 
raw_txt <- "борьбаға s1 o unstressed s2 a stressed
кожаға s1 o stressed s2 a unstressed 
жительдің	s1 i stressed s2 e unstressed 
законды	s1 a unstressed  s2 o stressed
землялар	s1 e unstressed  s2 æ stressed
звезды	s1 o stressed  s2 ɨ unstressed 
группы	s1 u stressed s2 ɨ unstressed 
воле	s1 o stressed s2 e unstressed 
окнa	s1 o stressed s2 a unstressed 
небоны	s1 e stressed s2 o unstressed 
вечердің s1 e stressed s2 e unstressed 
винолар	s1 i unstressed s2 o stressed
письмолар	s1 i unstressed s2 o stressed
выходқа	s1 ɨ stressed s2 o unstressed 
книгалар	s1 i stressed s2 a unstressed 
опыту	s1 o stressed s2 ɨ unstressed  s3 u unstressed 
горы	s1 o stressed s2 ɨ unstressed 
помощьты	s1 o stressed s2 o unstressed 
январьдың	s1 jɑ unstressed s2 a stressed
огоньды	s1 o unstressed s2 o stressed
пользы	s1 o stressed s2 ɨ unstressed
группалар	s1 u stressed s2 a unstressed
окноның	s1 o unstressed s2 o stressed
души	s1 u stressed s2 i unstressed 
книги	s1 i stressed s2 i unstressed
личности	s1 i stressed s2 o unstressed s3 i unstressed
народу	s1 a unstressed s2 o stressed s3 u unstressed
стеналар	s1 e unstressed  s2 a  stressed
силы	s1 i stressed s2 ɨ unstressed
рекалар	s1 e unstressed  s2 a stressed
января	s1 ja unstressed  s2 a unstressed s3 æ stressed
воляға	s1 o stressed s2 æ unstressed
реки	s1 e stressed s2 i unstressed
болезни	s1 o unstressed  s2 e stressed s3 i unstressed
душаның	s1 u unstressed  s2 a stressed
лицоны	s1 i unstressed s2 o stressed
городты	s1 o stressed s2 o unstressed
пользаны	s1 o stressed s2 a unstressed 
местолар	s1 e stressed s2 o unstressed
гораның	s1 o unstressed s2 a stressed
пареньді	s1 a stressed s2 e unstressed
правде	s1 a stressed s2 e unstressed
доляға	s1 o stressed s2 æ unstressed
апреля	s1 a unstressed s2 e stressed s3 æ unstressed
вопросқа	s1 o unstressed  s2 o stressed
выходу	s1 ɨ stressed s2 o unstressed  s3 u unstressed
силаны	s1 i stressed s2 a unstressed 
доле	s1 o stressed s2 e unstressed
лошадьты	s1 o stressed s2 a unstressed
страны	s1 a stressed s2 ɨ unstressed
городa	s1 o unstressed s2 o unstressed  s3 a stressed
странаның	s1 a unstressed s2 a stressed
небa	s1 e stressed s2 a unstressed
лошади	s1 o stressed s2 a unstressed s3 i unstressed
парня	s1 a stressed s2 æ unstressed
письмa	s1 i  stressed s2 a unstressed
вины	s1 i stressed s2 ɨ unstressed
правдаға	s1 a stressed s2 a unstressed 
апрельдің	s1 a unstressed  s2 e stressed 
борьбе	s1 o unstressed  s2 e stressed
лицa	s1 i stressed s2 a unstressed
вечерa	s1 e stressed s2 e unstressed  s3 a unstressed
опытқа	s1 o stressed s2 ɨ unstressed
списка	s1 i stressed s2 a unstressed
земли	s1 e stressed s2 i unstressed
селоның	s1 e unstressed  s2 o stressed
коже	s1 o stressed s2 e unstressed
вопросу	s1 o unstressed s2 o stressed s3 u unstressed
звездалар	s1 e unstressed s2 a stressed 
списоктың	s1 i stressed s2 o unstressed
селa	s1 e unstressed s2 a stressed
стены	s1 e stressed s2 ɨ unstressed
болезньдер	s1 o unstressed  s2 e  stressed
помощи	s1 o stressed s2 o unstressed  s3 i unstressed
народқа	s1 a unstressed  s2 o stressed
личностьқа	s1 i stressed s2 o unstressed  
огня	s1 o unstressed  s2 æ stressed
местa	s1 e unstressed  s2 a stressed
законa	s1 a unstressed  s2 o stressed s3 a unstressed
жителя	s1 i stressed s2 e unstressed  s3 æ unstressed
борьба	s1 o unstressed  s2 a stressed	
кожа	s1 o stressed s2 a unstressed
житель	s1 i stressed s2 e unstressed	
закон	s1 a unstressed  s2 o stressed 	
земля	s1 e unstressed s2 æ stressed
небо	s1 e stressed s2 o unstressed
вечер	s1 e stressed s2 e unstressed
вино	s1 i unstressed s2 o stressed	
письмо	s1 i unstressed s2 o stressed	
выход	s1 ɨ stressed s2 o unstressed
книга	s1 i stressed s2 a unstressed
помощь	s1 o stressed s2 o unstressed
январь	s1 jɑ unstressed s2 a stressed
огонь	s1 o unstressed s2 o stressed	
группа	s1 u stressed s2 a unstressed
окно	s1 o unstressed s2 o stressed	
стена	s1 e unstressed s2 a stressed
река	s1 e unstressed s2 a stressed	
воля	s1 o stressed s2 æ unstressed	
душа	s1 u unstressed s2 a stressed	
лицо	s1 i unstressed s2 o stressed	
город	s1 o stressed s2 o unstressed
польза	s1 o stressed s2 a unstressed
место	s1 e stressed s2 o unstressed
гора	s1 o unstressed s2 a stressed	
парень	s1 a stressed s2 e unstressed
доля	s1 o stressed s2 æ unstressed	
вопрос	s1 o unstressed s2 o stressed	
сила	s1 i stressed s2 a unstressed 
лошадь	s1 o stressed s2 a unstressed
страна	s1 a unstressed s2 a stressed	
правда	s1 a stressed s2 a unstressed
апрель	s1 a unstressed s2 e stressed	
опыт	s1 o stressed s2 ɨ unstressed
село	s1 e unstressed s2 o stressed	
звезда	s1 e unstressed s2 a stressed	
список	s1 i stressed s2 o unstressed
болезнь	s1 o unstressed s2 e stressed	
народ	s1 a unstressed s2 o stressed	
личность	s1 i stressed s2 o unstressed"

# Clean tabs and line breaks, trim, and remove empty lines
raw_txt_clean <- gsub("\t", " ", raw_txt)

lines <- str_split(raw_txt_clean, "\n")[[1]] %>%
  str_trim() %>%
  keep(~ .x != "")

split_lines <- str_split(lines, "\\s+")

# Pad lines to equal length
max_len <- max(sapply(split_lines, length))

df_wide <- split_lines %>%
  map(~ { length(.x) <- max_len; .x }) %>%
  do.call(rbind, .) %>%
  as.data.frame(stringsAsFactors = FALSE)

# Name and rename columns
colnames(df_wide) <- c("Word", paste0("Slot_", 1:(max_len - 1)))

df_wide <- df_wide %>%
  rename(
    syllable_1 = Slot_1,
    vowel_1 = Slot_2,
    stress_1 = Slot_3,
    syllable_2 = Slot_4,
    vowel_2 = Slot_5,
    stress_2 = Slot_6,
    syllable_3 = Slot_7,
    vowel_3 = Slot_8,
    stress_3 = Slot_9
  )

# Pivot to long format
df_long <- df_wide %>%
  pivot_longer(
    cols = -Word,
    names_to = c(".value", "syll_num"),
    names_pattern = "(syllable|vowel|stress)_(\\d+)"
  ) %>%
  arrange(Word, syll_num) %>%
  drop_na(syllable, vowel, stress) %>%
  rename(
    word = Word,
    syllables = syllable,
    vowel_new = vowel
  )

# View the result
# View(df_long)

# Attempt anti-join to find non-matching rows
df_rus_cs %>%
  anti_join(df_long %>% select(word, syllables, stress), 
            by = c("word", "syllables", "stress")) %>%
  distinct(word, syllables, stress) %>%
  print(n=36)


df_rus_cs_labeled <- df_rus_cs %>%
  # Join with more constraints to avoid over-merging
  left_join(
    df_long %>%
      select(word, syllables, stress_correct = stress, vowel_new),
    by = c("word",  "syllables")
  ) %>%
  # Replace stress with the trusted one from df_long
  #mutate(
   # stress = coalesce(stress_correct, stress)
  #) %>%
  # Remove temporary column
  select(-stress_correct)


# view(df_rus_cs_labeled)

# Check missing vowel labels (new ones)
df_rus_cs_labeled%>%
    filter(is.na(vowel_new)) %>%
   arrange(word, syllables, stress) %>%
   select(file, word, syllables, vowel, stress, vowel_new) %>%
   print(n = Inf)
## all vowels are labeled!!


#### Russian vowels ####

# Filter to Rus tokens only
df_rus_new <- df_rus_cs_labeled %>%
  filter(Language == "Rus") %>%
  filter(stress %in% c("stressed", "unstressed")) %>%
  filter(vowel_new != "ja") %>%
  filter(!is.na(Norm_F1), !is.na(Norm_F2)) %>%
  group_by(vowel_new, stress) %>%
  mutate(row_num = row_number()) %>%
  filter(vowel_new != "ɨ" | row_num <= 6,
         vowel_new != "u" | row_num <= 6,
         vowel_new != "æ" | row_num <= 8,
         vowel_new != "a" | row_num <= 22,
         vowel_new != "i" | row_num <= 28,
         vowel_new != "o" | row_num <= 33,) %>%
  ungroup()


#  no grouping before computing the center
# df_rus_new <- df_rus_new %>% ungroup()

# Compute center F1 and F2 for all vowels in the vowel space
center_F1 <- mean(df_rus_new$Norm_F1, na.rm = TRUE)
center_F2 <- mean(df_rus_new$Norm_F2, na.rm = TRUE)

# Compute Euclidean distance 
df_rus_dispersion_new <- df_rus_new %>%
  mutate(
    vowel_space_distance = sqrt((Norm_F1 - center_F1)^2 + (Norm_F2 - center_F2)^2)
  ) %>%
  select(Speaker, vowel_new, stress, WordForm, Language,
         Norm_F1, Norm_F2, vowel_space_distance, everything())

df_rus_dispersion_vc <- df_rus_dispersion_new %>%
  group_by(vowel_new, stress) %>%
  summarise(mean_dispersion = mean(vowel_space_distance, na.rm = TRUE))

#view(df_rus_dispersion_vc)
#view(df_rus_dispersion_new)


# Vowel Dispersion by Stress in Rus
# Mean F1-F2 by vowel category

# Compute centroids (mean F1/F2 for each vowel and stress)
centroids_rus_new <- df_rus_dispersion_new %>%
  #filter(Language == "Rus", vowel_new != "ja") %>%
  group_by(vowel_new, stress) %>%
  summarise(
    mean_F1 = mean(Norm_F1, na.rm = TRUE),
    mean_F2 = mean(Norm_F2, na.rm = TRUE),
    .groups = "drop"
  )

# Pivot to wide format for stressed and unstressed comparison
centroids_wide <- centroids_rus_new %>%
  pivot_wider(
    names_from = stress,
    values_from = c(mean_F1, mean_F2),
    names_glue = "{.value}_{stress}"
  ) %>%
  mutate(distance = sqrt((mean_F1_stressed - mean_F1_unstressed)^2 +
                           (mean_F2_stressed - mean_F2_unstressed)^2)) %>%
  filter(!is.na(distance) & distance > 0.1)  # Only meaningful shifts

# Prepare centroids for text plotting
centroids_text <- centroids_rus_new

# Plot vowel

ggplot() +
  # Arrows: stressed -> unstressed
  geom_segment(
    data = centroids_wide,
    aes(x = mean_F2_stressed, y = mean_F1_stressed,
        xend = mean_F2_unstressed, yend = mean_F1_unstressed),
    arrow = arrow(length = unit(0.15, "cm")),  # modest arrows
    color = "gray40"
  ) +
  # Centroid labels
  geom_text(
    data = centroids_text,
    aes(x = mean_F2, y = mean_F1, label = vowel_new, color = stress),
    size = 8, fontface = "bold"
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("F2") +
  ylab("F1") +
  #ggtitle("Russian Vowel Space") +
  theme_minimal(base_size = 12)
## i/i less change
## ɨ/ɨ - unstressed  ɨ becomes more fronted?
## other vowels like e, æ, a, o, u show some degree of reduction. 

###########################
# New combined plot for the poster

library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)       
library(patchwork)  # for combining plots + shared legend

##Russian 

centroids_wide_rus <- centroids_rus_new %>%
  tidyr::pivot_wider(
    names_from  = stress,
    values_from = c(mean_F1, mean_F2),
    names_glue  = "{.value}_{stress}"
  ) %>%
  mutate(
    distance = sqrt((mean_F1_stressed - mean_F1_unstressed)^2 +
                      (mean_F2_stressed - mean_F2_unstressed)^2)
  ) %>%
  filter(!is.na(distance), distance > 0.1)   # optional filter

centroids_text_rus <- centroids_rus_new

## Code-switched 

centroids_cs_new <- df_rus_cs_labeled %>%
  filter(Language == "CS", vowel_new != "jɑ") %>%
  group_by(vowel_new, stress) %>%
  mutate(row_num = dplyr::row_number()) %>%
  # example cap for "ɨ" (keep first 2 rows only) — adjust/remove as needed
  filter(vowel_new != "ɨ" | row_num <= 2) %>%
  ungroup() %>%
  group_by(vowel_new, stress) %>%
  summarise(
    mean_F1 = mean(Norm_F1, na.rm = TRUE),
    mean_F2 = mean(Norm_F2, na.rm = TRUE),
    .groups = "drop"
  )

centroids_wide_cs <- centroids_cs_new %>%
  tidyr::pivot_wider(
    names_from  = stress,
    values_from = c(mean_F1, mean_F2),
    names_glue  = "{.value}_{stress}"
  ) %>%
  mutate(
    distance = sqrt((mean_F1_stressed - mean_F1_unstressed)^2 +
                      (mean_F2_stressed - mean_F2_unstressed)^2)
  ) %>%
  filter(!is.na(distance), distance > 0.1)   # optional filter

centroids_text_cs <- centroids_cs_new

## Common axis limits -

x_all <- c(
  centroids_text_rus$mean_F2,
  centroids_text_cs$mean_F2,
  centroids_wide_rus$mean_F2_stressed,
  centroids_wide_rus$mean_F2_unstressed,
  centroids_wide_cs$mean_F2_stressed,
  centroids_wide_cs$mean_F2_unstressed
)

y_all <- c(
  centroids_text_rus$mean_F1,
  centroids_text_cs$mean_F1,
  centroids_wide_rus$mean_F1_stressed,
  centroids_wide_rus$mean_F1_unstressed,
  centroids_wide_cs$mean_F1_stressed,
  centroids_wide_cs$mean_F1_unstressed
)

x_rng <- range(x_all, na.rm = TRUE)
y_rng <- range(y_all, na.rm = TRUE)

# Reversed axes: limits are c(max, min)
common_x_limits <- c(max(x_rng), min(x_rng))
common_y_limits <- c(max(y_rng), min(y_rng))

## Ensure consistent legend mapping across plots 

stress_levels <- unique(c(centroids_text_rus$stress, centroids_text_cs$stress))
# Optional: enforce specific order if you like
# stress_levels <- c("unstressed","stressed")

shared_theme <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

## Build plots (no titles; clean for poster) 

p_rus <- ggplot() +
  geom_segment(
    data = centroids_wide_rus,
    aes(x = mean_F2_stressed, y = mean_F1_stressed,
        xend = mean_F2_unstressed, yend = mean_F1_unstressed),
    arrow = arrow(length = unit(0.15, "cm")),
    color = "gray40"
  ) +
  geom_text(
    data = centroids_text_rus,
    aes(x = mean_F2, y = mean_F1, label = vowel_new, color = stress),
    size = 8, fontface = "bold"
  ) +
  scale_color_discrete(limits = stress_levels, drop = FALSE) +
  scale_x_reverse(limits = common_x_limits, expand = expansion(mult = 0.05)) +
  scale_y_reverse(limits = common_y_limits, expand = expansion(mult = 0.05)) +
  coord_fixed(ratio = 1) +
  labs(x = "F2", y = "F1", subtitle = "Russian carrier phrase") +
  shared_theme

p_cs <- ggplot() +
  geom_segment(
    data = centroids_wide_cs,
    aes(x = mean_F2_stressed, y = mean_F1_stressed,
        xend = mean_F2_unstressed, yend = mean_F1_unstressed),
    arrow = arrow(length = unit(0.15, "cm")),
    color = "gray40"
  ) +
  geom_text(
    data = centroids_text_cs,
    aes(x = mean_F2, y = mean_F1, label = vowel_new, color = stress),
    size = 8, fontface = "bold"
  ) +
  scale_color_discrete(limits = stress_levels, drop = FALSE) +
  scale_x_reverse(limits = common_x_limits, expand = expansion(mult = 0.05)) +
  scale_y_reverse(limits = common_y_limits, expand = expansion(mult = 0.05)) +
  coord_fixed(ratio = 1) +
  labs(x = "F2", y = "F1", subtitle = "Kazakh carrier phrase") +
  shared_theme

##  Side-by-side with ONE shared legend 

combined <- p_rus + p_cs + plot_layout(guides = "collect") & theme(legend.position = "bottom")

print(combined)

## Plot for PP

bw_stress_colors <- c(
  stressed   = "black",
  unstressed = "grey60"
)

p_rus <- ggplot() +
  geom_segment(
    data = centroids_wide_rus,
    aes(x = mean_F2_stressed, y = mean_F1_stressed,
        xend = mean_F2_unstressed, yend = mean_F1_unstressed),
    arrow = arrow(length = unit(0.15, "cm")),
    color = "gray40"
  ) +
  geom_text(
    data = centroids_text_rus,
    aes(x = mean_F2, y = mean_F1, label = vowel_new, color = stress),
    size = 8, fontface = "bold"
  ) +
  scale_color_manual(
    values = bw_stress_colors,
    limits = stress_levels,
    drop = FALSE
  ) +
  scale_x_reverse(limits = common_x_limits, expand = expansion(mult = 0.05)) +
  scale_y_reverse(limits = common_y_limits, expand = expansion(mult = 0.05)) +
  coord_fixed(ratio = 1) +
  labs(x = "F2", y = "F1", subtitle = "Russian carrier phrase") +
  shared_theme

p_cs <- ggplot() +
  geom_segment(
    data = centroids_wide_cs,
    aes(x = mean_F2_stressed, y = mean_F1_stressed,
        xend = mean_F2_unstressed, yend = mean_F1_unstressed),
    arrow = arrow(length = unit(0.15, "cm")),
    color = "gray40"
  ) +
  geom_text(
    data = centroids_text_cs,
    aes(x = mean_F2, y = mean_F1, label = vowel_new, color = stress),
    size = 8, fontface = "bold"
  ) +
  scale_color_manual(
    values = bw_stress_colors,
    limits = stress_levels,
    drop = FALSE
  ) +
  scale_x_reverse(limits = common_x_limits, expand = expansion(mult = 0.05)) +
  scale_y_reverse(limits = common_y_limits, expand = expansion(mult = 0.05)) +
  coord_fixed(ratio = 1) +
  labs(x = "F2", y = "F1", subtitle = "Kazakh carrier phrase") +
  shared_theme

##  Save poster-ready files 
# PNG —  high DPI for large posters
ggsave("vowel_space_combined.png", combined,
       width = 12, height = 6, dpi = 600, bg = "white")

combined_pp <- p_rus + p_cs + plot_layout(guides = "collect") & theme(legend.position = "bottom")

print(combined_pp)

#########################################################################


# Vowel Dispersion by Stress in Rus

df_rus_vc_new <- df_rus_dispersion_new %>%
  #filter(!vowel %in% c("ja"))%>%
  group_by(stress) %>%
  summarise(
    mean_dispersion = mean(vowel_space_distance, na.rm = TRUE),
    se = sd(vowel_space_distance, na.rm = TRUE) / sqrt(n()),
    ci95 = 1.96 * se,
    .groups = "drop"
  )

ggplot(df_rus_vc_new, aes(x = stress, y = mean_dispersion, fill = stress)) +
  geom_col() +
  geom_errorbar(
    aes(ymin = mean_dispersion - ci95, ymax = mean_dispersion + ci95),
    width = 0.2,
    color = "black"
  ) +
  ylim(0, NA) +
 labs(y = "Mean Dispersion") +
  theme_minimal(base_size = 12)
## Stressed vowels are more dispersed (at the periphery) than unstressed ones.

## Vowel dispersion by Vowel type and stress in RUS

df_rus_voweltype <- df_rus_dispersion_new %>%
  group_by(stress, vowel_new) %>%
  summarise(
    mean_dispersion = mean(vowel_space_distance, na.rm = TRUE),
    se = sd(vowel_space_distance, na.rm = TRUE) / sqrt(n()),
    ci95 = 1.96 * se,
    .groups = "drop"
  )

ggplot(df_rus_voweltype, aes(x = vowel_new, y = mean_dispersion, fill = stress)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = mean_dispersion - ci95, ymax = mean_dispersion + ci95),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  ylim(0, NA) +
  labs(x = "Vowel", y = "Mean Dispersion") +
  theme_minimal(base_size = 12)
## e, æ, a, o - show reduction under stress
## u, ɨ, i - unstressed counterparts are more dispersed than stressed ones?


## Vowel Dispersion by the Speaker
df_rus_speaker <- df_rus_dispersion_new %>%
  group_by(stress, Speaker) %>%
  summarise(
    mean_dispersion = mean(vowel_space_distance, na.rm = TRUE),
    se = sd(vowel_space_distance, na.rm = TRUE) / sqrt(n()),
    ci95 = 1.96 * se,
    .groups = "drop"
  )

ggplot(df_rus_speaker, aes(x = Speaker, y = mean_dispersion, fill = stress)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = mean_dispersion - ci95, ymax = mean_dispersion + ci95),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  ylim(0, NA) +
  labs(y = "Mean Dispersion") +
  theme_minimal(base_size = 12)
# Speaker 1 and 2 there is a small diff between stressed and unstressed vowels.
# Speaker 3 and 4, stressed vowels are more dispersed than unstressed ones.


#### CS vowel dispersion ####

# Compute centroids (mean F1/F2 for each vowel and stress)
# ɨ stress =2 , no stress = 4, filter only 2

centroids_cs_new <- df_rus_cs_labeled %>%
  filter(Language == "CS", vowel_new != "jɑ") %>%
  group_by(vowel_new, stress) %>%
  mutate(row_num = row_number()) %>%
  filter(vowel_new != "ɨ" | row_num <= 2) %>%
  ungroup() %>%
  group_by(vowel_new, stress) %>%
  summarise(
    mean_F1 = mean(Norm_F1, na.rm = TRUE),
    mean_F2 = mean(Norm_F2, na.rm = TRUE),
    .groups = "drop"
  )

# Pivot to wide format for stressed and unstressed comparison
centroids_wide_cs <- centroids_cs_new %>%
  pivot_wider(
    names_from = stress,
    values_from = c(mean_F1, mean_F2),
    names_glue = "{.value}_{stress}"
  ) %>%
  mutate(distance = sqrt((mean_F1_stressed - mean_F1_unstressed)^2 +
                           (mean_F2_stressed - mean_F2_unstressed)^2)) %>%
  filter(!is.na(distance) & distance > 0.1)  # Only meaningful shifts

# Prepare centroids for text plotting
centroids_text_cs <- centroids_cs_new

# Plot vowel 

ggplot() +
  # Arrows: stressed -> unstressed
  geom_segment(
    data = centroids_wide_cs,
    aes(x = mean_F2_stressed, y = mean_F1_stressed,
        xend = mean_F2_unstressed, yend = mean_F1_unstressed),
    arrow = arrow(length = unit(0.15, "cm")),  # modest arrows
    color = "gray40"
  ) +
  # Centroid labels
  geom_text(
    data = centroids_text_cs,
    aes(x = mean_F2, y = mean_F1, label = vowel_new, color = stress),
    size = 8, fontface = "bold"
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  xlab("F2") +
  ylab("F1") +
  #ggtitle("CS Vowel Space") +
  theme_minimal(base_size = 12)

## Vowel Dispersion by Stress in CS

# Filter to Rus tokens only
df_cs_ds <- df_rus_cs_labeled %>%
  filter(Language == "CS", !vowel_new %in% c("jɑ")) %>%
  # filter(stress %in% c("stressed", "unstressed")) %>%
  filter(!is.na(Norm_F1), !is.na(Norm_F2))  

view(df_rus_cs_labeled) 

#  no grouping before computing the center
df_cs_ds <- df_cs_ds %>% ungroup()

# Compute center F1 and F2 for all vowels in the vowel space
center_F1 <- mean(df_cs_ds$Norm_F1, na.rm = TRUE)
center_F2 <- mean(df_cs_ds$Norm_F2, na.rm = TRUE)


# Compute Euclidean distance
df_cs_dispersion_new <- df_cs_ds %>%
  mutate(
    vowel_space_distance = sqrt((Norm_F1 - center_F1)^2 + (Norm_F2 - center_F2)^2)
  ) %>%
  select(Speaker, vowel_new, stress, WordForm, Language,
         Norm_F1, Norm_F2, vowel_space_distance, everything())

# df_rus_dispersion_vc <- df_cs_dispersion_new %>%
#   group_by(vowel_new, stress) %>%
#   summarise(mean_dispersion = mean(vowel_space_distance, na.rm = TRUE))



df_cs_vc_new <- df_cs_dispersion_new %>%
  group_by(stress) %>%
  summarise(
    mean_dispersion = mean(vowel_space_distance, na.rm = TRUE),
    se = sd(vowel_space_distance, na.rm = TRUE) / sqrt(n()),
    ci95 = 1.96 * se,
    .groups = "drop"
  )

ggplot(df_cs_vc_new, aes(x = stress, y = mean_dispersion, fill = stress)) +
  geom_col() +
  geom_errorbar(
    aes(ymin = mean_dispersion - ci95, ymax = mean_dispersion + ci95),
    width = 0.2,
    color = "black"
  ) +
  ylim(0, NA) +
  labs(y = "Mean Dispersion") +
  theme_minimal(base_size = 12)
## CS stressed vowels are more dispersed than unstressed ones. 


## Vowel dispersion by Vowel type and stress
df_cs_voweltype <- df_cs_dispersion_new %>%
  group_by(stress, vowel_new) %>%
  mutate(row_num = row_number()) %>%
  filter(vowel_new != "ɨ" | row_num <= 2) %>%
  summarise(
    mean_dispersion = mean(vowel_space_distance, na.rm = TRUE),
    se = sd(vowel_space_distance, na.rm = TRUE) / sqrt(n()),
    ci95 = 1.96 * se,
    .groups = "drop"
  )

ggplot(df_cs_voweltype, aes(x = vowel_new, y = mean_dispersion, fill = stress)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = mean_dispersion - ci95, ymax = mean_dispersion + ci95),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  ylim(0, NA) +
  labs(x = "Vowel", y = "Mean Dispersion") +
  theme_minimal(base_size = 12)
## all stressed vowels are more dispersed than unstressed ones. 

## Vowel Dispersion by Speaker and Stress

df_cs__speaker_new <- df_cs_dispersion_new %>%
  #filter(!vowel %in% c("ja", "0")) %>%
  group_by(Speaker, stress) %>%
  summarise(
    mean_dispersion = mean(vowel_space_distance, na.rm = TRUE),
    se = sd(vowel_space_distance, na.rm = TRUE) / sqrt(n()),
    ci95 = 1.96 * se,
    .groups = "drop"
  )

# Plot with grouped bars by stress
ggplot(df_cs__speaker_new, aes(x = Speaker, y = mean_dispersion, fill = stress)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = mean_dispersion - ci95, ymax = mean_dispersion + ci95),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  ylim(0, NA) +
  labs(y = "Mean Dispersion") +
  theme_minimal(base_size = 12)
## Speaker 1, 3, and 4 - stressed vowels are more dispersed.
## Speaker 2 - stressed vowels less dispersed.




## Statistical Testing 

# Create a new Context column by collapsing Lang and WordForm columns

df_context_cs <- df_cs_dispersion_new %>%
  mutate(Language_WordForm = paste(Language, WordForm, sep = "_")) %>%
  rename(Context = Language_WordForm)

view(df_context_cs)

df_context_rus <- df_rus_dispersion_new %>%
  mutate(Language_WordForm = paste(Language, WordForm, sep = "_")) %>%
  rename(Context = Language_WordForm)

view(df_context_rus)

# Merge df
df_context_combined <- bind_rows(df_context_rus, df_context_cs)
view(df_context_combined)

## Vowel Dispersion by Context 
df_context_dispersion <- df_context_combined %>%
  #filter(!vowel %in% c("ja")) %>%
  group_by(Context, stress) %>%
  summarise(
    mean_dispersion = mean(vowel_space_distance, na.rm = TRUE),
    se = sd(vowel_space_distance, na.rm = TRUE) / sqrt(n()),
    ci95 = 1.96 * se,
    .groups = "drop"
  )

## Plot with grouped bars by stress
ggplot(df_context_dispersion, aes(x = Context, y = mean_dispersion, fill = stress)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = mean_dispersion - ci95, ymax = mean_dispersion + ci95),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  ylim(0, NA) +
  labs(x = "Context", y = "Mean Dispersion") +
  theme_minimal(base_size = 12)


## Plot for PP

bw_stress_colors <- c(
  stressed   = "black",
  unstressed = "grey60"
)

ggplot(df_context_dispersion, aes(x = Context, y = mean_dispersion, fill = stress)) +
  geom_col(position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(
    aes(ymin = mean_dispersion - ci95, ymax = mean_dispersion + ci95),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  scale_fill_manual(values = bw_stress_colors, limits = stress_levels, drop = FALSE) +
  ylim(0, NA) +
  labs(x = "Context", y = "Mean Dispersion", fill = "Stress") +
  theme_minimal(base_size = 12)


## Uninflected stressed vowels are more dispersed than inflected ones (the diff-s is negligible I think).
## Uninflected unstressed vowels are more reduced than inflected ones (the diff-s is negligible I think).



# Model 1: do context and stress predict the vowel dispersion?
# Df: combined one
# No interaction
context_ml <- lmer(vowel_space_distance ~ Context + stress + (1|word) + (1|Speaker), data = df_context_combined)
summary(context_ml)

# with interaction
context_ml_int <- lmer(vowel_space_distance ~ Context*stress + (1|word) + (1|Speaker), data = df_context_combined)
summary(context_ml_int)


# A linear mixed-effects model was fit to examine how Context and Stress influence vowel space 
# distance in Russian and code-switched tokens. The model included random intercepts for word 
# and Speaker, although the speaker-level variance was estimated to be zero, indicating no 
# detectable variability across speakers. The analysis revealed a significant main effect of 
# stress: unstressed vowels were significantly less dispersed in the vowel space compared to 
# their stressed counterparts (β = –0.29, SE = 0.044, t = –6.45, p < .001). This result is 
# consistent with prior findings that stress is associated with greater articulatory precision 
# and vowel hyperarticulation. In contrast, Context (i.e., whether a token was from a 
# code-switched uninflected form or a Russian inflected form) did not significantly affect 
# vowel dispersion. Neither CS_uninflected (β = 0.015, p = .845) nor Rus_inflected 
# (β = 0.057, p = .404) differed significantly from the reference level (CS_inflected), 
# suggesting that context does not modulate vowel space dispersion beyond the effect of stress.
# Random effects indicated modest variability at the word level (SD = 0.18), but no variance 
# at the Speaker level, possibly due to the small number of speakers or speaker-normalized data.
# These findings suggest that stress is the primary driver of vowel dispersion in the dataset, 
# with contextual differences between code-switched and monolingual tokens exerting no 
# significant effect in this model.

# Which model fits best the data?
anova(context_ml, context_ml_int)
# The model without the interaction term provided a better fit to the data and was preferred 
# over the more complex interaction model (χ²(2) = 3.14, p = .208).


# Model 2: Do stress and vowel type predict dispersion?
# DF: rus and cs separately bs the center of vowel space is defined separately for each lang

# Russian vowels

rus_vowel <- lmer(vowel_space_distance ~ vowel_new + stress + (1|Speaker) + (1|word), data = df_rus_dispersion_new)
summary(rus_vowel)

# The model revealed that vowel identity and stress significantly affect vowel space dispersion 
# in Russian tokens: compared to stressed /a/, stressed /æ/ and /e/ exhibited significantly 
# reduced dispersion (p < .001), /i/ and /u/ showed significantly greater dispersion (p < .05), 
# while /ɨ/ and /o/ did not differ significantly; additionally, unstressed vowels overall were 
# significantly less dispersed than their stressed counterparts (β = –0.22, p < .001).

rus_vowel_int <- lmer(vowel_space_distance ~ vowel_new*stress + (1|Speaker) + (1|word), data = df_rus_dispersion_new)
summary(rus_vowel_int)

anova(rus_vowel, rus_vowel_int)
# the interaction between vowel_new and stress does not significantly improve model fit.
# p=0.1334


# CS vowels

cs_vowel <- lmer(vowel_space_distance ~ vowel_new + stress + (1|Speaker) + (1|word), data = df_cs_dispersion_new)
summary(cs_vowel)

# In the code-switched dataset, stressed vowels /i/ and /e/ exhibited significantly greater 
# vowel space dispersion than stressed /a/, while /æ/, /o/, and /u/ showed no significant 
# differences, and unstressed vowels overall were significantly less dispersed than stressed 
# vowels (β = –0.29, p < .001).

cs_vowel_int <- lmer(vowel_space_distance ~ vowel_new *stress + (1|Speaker) + (1|word), data = df_cs_dispersion_new)
summary(cs_vowel_int)

anova(cs_vowel, cs_vowel_int)
# The model without the interaction term (`cs_vowel`) provides a better fit to the data
# and is preferred over the more complex interaction model, as the addition of the
# vowel × stress interaction does not significantly improve model fit
# (χ²(6) = 6.04, p = .418).


## Russian vs CS vowel dispersion ##

# While both the monolingual Russian and code-switched models revealed significantly
# reduced vowel space dispersion in unstressed syllables (Russian: β = –0.22, p < .001;
# CS: β = –0.29, p < .001), code-switched tokens exhibited lower overall dispersion
# (intercept: 1.27 vs. 1.50) and greater residual variability (σ² = 0.26 vs. 0.18),
# suggesting less precise articulation under mixed-language conditions; additionally,
# vowels such as /e/ and /æ/ showed divergent patterns—/e/ was significantly
# compressed in Russian (β = –0.47, p < .001) but expanded in CS (β = +0.22, p = .011),
# and /æ/ was significantly reduced in Russian (β = –0.50, p < .001) but neutralized in
# CS (p = .63)—indicating altered phonological contrast maintenance and articulatory
# dynamics in code-switched production.

#### Run all predictors in one model ####
sum_ml <- lmer(vowel_space_distance ~ Context + stress + vowel_new + (1|Speaker) + (1|word), data = df_context_combined)
summary(sum_ml)

# The model revealed a significant main effect of stress, with unstressed vowels
# showing reduced dispersion compared to stressed vowels (β = –0.26, p < .001).
# Vowel identity also predicted dispersion: /i/ (β = +0.58, p < .001) and /u/
# (β = +0.41, p = .005) were significantly more dispersed than /a/, while /æ/ was
# significantly less dispersed (β = –0.26, p = .046); other vowels did not differ
# significantly. Context (CS vs. Rus) did not significantly affect vowel space
# distance, suggesting that phonetic realization is stable across monolingual and
# CS tokens. A singular fit warning indicated zero variance for the
# speaker random effect, likely due to speaker-normalized data or limited speaker
# sampling (n = 4).

sum_ml_int <- lmer(vowel_space_distance ~ Context*stress*vowel_new + (1|Speaker) + (1|word), data = df_context_combined)
summary(sum_ml_int)

anova(sum_ml, sum_ml_int)

########################################################################################

# Re-run a model with Bark transformed formant values
# Convert raw F1 and F2 values into a Bark scale (an auditory transform) based on Traunmüller 1997 formula
# Re-run models and try drop1() function to drop insignificant predictors/interactions
# Run: fixed effects + (1 + Context + stress + vowel_new|Speaker)
# fixed effects + (1 + Context|word)


# Convert raw formant values to Bark scale 
# To automatically convert
library(vowels)

# Manual conversion
to_bark <- function(f) 26.81/(1 + 1960/f) - 0.53   # Traunmüller 1997

# [ (26.81*F) / (1960+F) ] - 0.53 where F= freq in Hz

df_context_combined <- df_context_combined %>% 
  mutate(across(c(F1, F2, F3), to_bark, .names = "{.col}_bark"))


view(df_context_combined)

# Filter CS tokens
df_cs <- df_context_combined %>%
  filter(Language == "CS", !vowel_new %in% c("jɑ")) %>%
  # filter(stress %in% c("stressed", "unstressed")) %>%
  filter(!is.na(F1), !is.na(F2))  


#  no grouping before computing the center
df_cs <- df_cs %>% ungroup()


# Compute center F1 and F2 (from a new bark scaled F1 and F2) for all vowels in the vowel space
center_F1 <- mean(df_cs$F1_bark, na.rm = TRUE)
center_F2 <- mean(df_cs$F2_bark, na.rm = TRUE)


# Compute Euclidean distance for CS tokens
df_cs_disp <- df_cs %>%
  mutate(
    vowel_space_distance = sqrt((F1_bark - center_F1)^2 + (F2_bark - center_F2)^2)
  ) %>%
  select(Speaker, vowel_new, stress, WordForm, Language,
         Norm_F1, Norm_F2, vowel_space_distance, everything())

view(df_cs_disp)


# Filter Rus tokens
df_rus <- df_context_combined %>%
  filter(Language == "Rus", !vowel_new %in% c("jɑ")) %>%
  # filter(stress %in% c("stressed", "unstressed")) %>%
  filter(!is.na(F1), !is.na(F2))  


#  no grouping before computing the center
df_rus <- df_rus %>% ungroup()


# Compute center F1 and F2 (from a new bark scaled F1 and F2) for all vowels in the vowel space
center_F1 <- mean(df_rus$F1_bark, na.rm = TRUE)
center_F2 <- mean(df_rus$F2_bark, na.rm = TRUE)


# Compute Euclidean distance for RUS tokens
df_rus_disp <- df_rus %>%
  mutate(
    vowel_space_distance = sqrt((F1_bark - center_F1)^2 + (F2_bark - center_F2)^2)
  ) %>%
  select(Speaker, vowel_new, stress, WordForm, Language,
         Norm_F1, Norm_F2, vowel_space_distance, everything())

view(df_rus_disp)

# Merge two df-s
df_bark <- bind_rows(df_cs_disp, df_rus_disp)

# Run a plain mixed fixed effects model
library(forcats)

df_bark <- df_bark %>%
  mutate(Context = fct_relevel(Context, "Rus_inflected", "CS_uninflected", "CS_inflected"))


sum_bark <- lmer(vowel_space_distance ~ Context + stress + vowel_new + (1|Speaker) + (1|word), data = df_bark)
summary(sum_bark)

# No diff between context, diff in stress and point vowels like e,i, o and u (those vowels are more dispersed than referenced stressed a)


# Run a model with fixed+random effects
library(performance)  # model diagnostics

view(df_bark)
str(df_bark)                     # are Context, stress, vowel_new factors?
levels(df_bark$vowel_new)        # check level ordering
class(df_bark$vowel_new)

#contrasts(df_bark$Context)       
class(df_bark$Context)


### Use the maximal random effects justified by design in LMEMs recommended by Barr et al. 2013
## All fixed effects + random intercepts and all slopes for each predictor by Speaker

mod <- lmer(
  vowel_space_distance ~ Context + stress + vowel_new +
    (1 + Context + stress + vowel_new | Speaker),
  data = df_bark)
# singular fit 


# Summary and fixed-effect tests 
summary(mod)                 # coefficients, random‐effects SDs & correlations
anova(mod, type = 3)         # F-tests (lmerTest)

# Diagnostics 
check_model(mod)             # residual plots, QQ-plot, etc.
# But REsidual plots look fine

##########################################################

## Scaling down
##  “intercept-only” random structure
m0 <- lmer(vowel_space_distance ~ Context + stress + vowel_new +
             (1 | Speaker), data = df_bark, REML = TRUE)
isSingular(m0)        # FALSE, which is a good sign!!

## Add slopes one by one, or in small logical blocks
m1 <- lmer(vowel_space_distance ~ Context + stress + vowel_new +
             (1 + stress | Speaker), data = df_bark, REML = TRUE)

m2 <- lmer(vowel_space_distance ~ Context + stress + vowel_new +
             (1 + stress + Context | Speaker), data = df_bark, REML = TRUE)

m3 <- lmer(vowel_space_distance ~ Context + stress + vowel_new +
             (1 + stress + Context  + vowel_new | Speaker), data = df_bark, REML = TRUE)
# The core issue is too many variance–covariance terms for too few speakers.
# 12 variance terms, 597 data rows and only 4 speakers 

## Model with interaction
## theoretical need for three-way interactions?
m0_int <- lmer(vowel_space_distance ~ Context*stress* vowel_new +
                 (1 | Speaker), data = df_bark, REML = TRUE)
isSingular(m0_int)        # FALSE, good!
summary(m0_int)
# Too complex to interpret!
# But has lower AIC value.


## Compare fits (REML = FALSE for comparison)
anova(update(m0, REML = FALSE),
      update(m1, REML = FALSE),
      update(m2, REML = FALSE),
      update(m3, REML = FALSE),
      update(m0_int, REML = FALSE) )

# Model m0 and m0_int have better fit to the data and have low AIC values.
# Meaning, the simple model without interactions might be a best fit to the given data with 4 speakers.

## Still need all slopes but not the co-variances
m_diag <- lmer(vowel_space_distance ~ Context + stress + vowel_new +
                 (1 + Context + stress + vowel_new || Speaker),
               data = df_bark, REML = TRUE)      # remove co-variances> ||

isSingular(m_diag)    # True!

# Production diff in CS and Rus without Stress effect
m0_no_stress <- lmer(vowel_space_distance ~ Context * vowel_new +
             (1 | Speaker), data = df_bark, REML = TRUE)
summary(m0_no_stress)
# Russian vowels are more dispersed, p=0.02
# Russian vowels such as e,i and u differ from the vowel a in inflected CS tokens





###### Drop insignificant predictors
# start from the fullest *non-singular* random structure
m_full <- lmer(vowel_space_distance ~ Context + stress + vowel_new +
                 (1 | Speaker), data = df_bark, REML = FALSE)

# single-term deletions, χ² tests
drop1(m_full, test = "Chisq")
# Context is not significant?

m_reduced <- update(m_full, . ~ . - Context)
anova(m_full, m_reduced, test = "LRT")    # verify with a likelihood-ratio test
summary(m_reduced)

m_part <- lmer(vowel_space_distance ~ stress + vowel_new +
                 (1 | Speaker), data = df_bark, REML = FALSE)
drop1(m_part, test = "Chisq")

# ### Results
# 
# Mixed‐effects modelling revealed significant main effects of **stress**
# (unstressed vowels were 0.44 Bark less dispersed than stressed vowels,
# *F*(1, 593) = 37.35, *p* < .001) and **vowel category**
# (*F*(6, 593) = 15.82, *p* < .001).  
# The main effect of **Context** (code-switch vs. monolingual) was not
# significant (*F*(2, 593) = 2.22, *p* = .11), and removing it did not impair
# model fit (likelihood-ratio = 4.43, df = 2, *p* = .11).  
# We therefore base interpretation on the reduced model that includes stress
# and vowel but not Context.
# 
# ### Discussion
# 
# The absence of a Context effect may indicate that bilingual speakers maintain
# a consistent stress-to-vowel-dispersion relationship regardless of whether a
# Russian word is fully embedded in a Kazakh sentence or produced in an
# all-Russian context.  
# This supports hypothesis (B) that Russian tokens maintain original stress pattern
# suggesting that even intra-word switches do not disrupt the phonetic
# implementation of lexical stress.
# 
# ### Limitations and Future Work
# 
# Power to detect subtle interactions (e.g., stress × Context or
# vowel × Context) is limited by having only four speakers. Future work with
# a larger sample or with designs that manipulate discourse context
# systematically will be needed to confirm whether the stress–vowel pattern
# is truly invariant across all conversational settings.

# Count palatalized vowels, remove from the analysis?
# REmoved pause and geminates 

# Moderate vowel reduction in immediately pretonic syllables - in our data all unstressed syllables have this feature!!

df_formant <- read_csv("/Users/moldir/Documents/Fall_2024_IndStudy/CompletedAnnotations/formants_dataset.csv")
view(df_formant)
