
# Stress Patterns in Intra-word Code-Switching: Acoustic Analysis

This repository was created as supplementary material for a study conducted by Moldir Baidildinova, Gregory Scontras, and Connor Mayer, which was presented at the Annual Meeting on Phonology 2025. It contains the core datasets and R scripts supporting the study’s findings.

Repository created: May 13, 2025
Corresponding author: Moldir Baidildinova, second-year PhD student, University of California, Irvine (mbaidild@uci.edu). 

---

## Summary

This pilot experiment aims to conduct an acoustic analysis of Kazakh-Russian intra-word code-switching using experimental data and to investigate the phonological implications of word-internal shifts through acoustic correlates of stress, namely, duration, intensity, pitch and vowel reduction. The central question is how the stress patterns of two typologically and genealogically distinct languages interact in word-internal shifts, specifically, whether the phonology aligns with one language’s pattern or exhibits a mixed, hybrid pattern.

---

## Experimental Data

The acoustic data used in this project were collected through a production experiment conducted at the [UCI Speech Science Lab](https://www.langsci.uci.edu/undergrad/courses.php). We tested four Kazakh-Russian bilingual speakers, eliciting disyllabic nouns in a carrier phrase under the following three conditions:

- **Kazakh tokens in a Kazakh context** (non-code-switched), both with and without Kazakh affixes  
- **Russian tokens in a Kazakh context** (code-switched), both with and without Kazakh affixes  
- **Russian tokens in a Russian context**, with Russian suffixes

All audio files were annotated using [Praat](https://www.fon.hum.uva.nl/praat/), and the acoustic analysis was performed using linear mixed-effects models implemented in the [`lme4`](https://cran.r-project.org/package=lme4) package in R.


---

## Repository Structure

ICS_Acoustic_Analysis/
├── analysis/
│ ├── formants.R # R script for vowel-level (F1–F2) acoustic analyses
│ └── syllables.R # R script for syllable-level acoustic analyses (duration, intensity, pitch)
│
├── datasets/
│ ├── formants_dataset.csv # Vowel-level dataset used for formant dispersion analyses
│ └── stress_dataset.csv # Syllable-level dataset with stress and positional information
│
├── AMP_2025_Poster_COPY_UPDATED.key # Keynote file for AMP 2025 poster presentation
├── LICENSE.md # License information for the repository
└── README.md # Project overview and usage instructions


---

## Acknowledgments


Special thanks to [Frankie Boren](https://www.linkedin.com/in/frankieboren), for her diligent work and support on annotation.

---


