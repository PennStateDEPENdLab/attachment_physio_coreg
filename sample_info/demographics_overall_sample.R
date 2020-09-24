library(tidyverse)

couples_clin_wide_inclusion <- read.csv("../data/demo_df.csv")
summary(lm(pdtot_0 ~ clean, couples_clin_wide_inclusion)) #n.s.
summary(lm(pdtot_1 ~ clean, couples_clin_wide_inclusion)) #n.s.

summary(lm(antso_sidp_0 ~ clean, couples_clin_wide_inclusion)) #n.s.
summary(lm(antso_sidp_1 ~ clean, couples_clin_wide_inclusion)) #n.s.

summary(lm(avoid_sidp_0 ~ clean, couples_clin_wide_inclusion)) #n.s.
summary(lm(avoid_sidp_1 ~ clean, couples_clin_wide_inclusion)) #n.s.

summary(lm(bordl_sidp_0 ~ clean, couples_clin_wide_inclusion)) #n.s.
summary(lm(bordl_sidp_1 ~ clean, couples_clin_wide_inclusion)) #n.s.

summary(lm(depen_sidp_0 ~ clean, couples_clin_wide_inclusion)) #.09
summary(lm(depen_sidp_1 ~ clean, couples_clin_wide_inclusion)) #n.s.

summary(lm(histr_sidp_0 ~ clean, couples_clin_wide_inclusion)) #n.s.
summary(lm(histr_sidp_1 ~ clean, couples_clin_wide_inclusion)) #n.s.

summary(lm(narci_sidp_0 ~ clean, couples_clin_wide_inclusion)) #n.s.
summary(lm(narci_sidp_1 ~ clean, couples_clin_wide_inclusion)) #n.s.

summary(lm(obcmp_sidp_0 ~ clean, couples_clin_wide_inclusion)) #n.s.
summary(lm(obcmp_sidp_1 ~ clean, couples_clin_wide_inclusion)) #n.s.

summary(lm(parnd_sidp_0 ~ clean, couples_clin_wide_inclusion)) #.08
summary(lm(parnd_sidp_1 ~ clean, couples_clin_wide_inclusion)) #n.s.

summary(lm(stypl_sidp_0 ~ clean, couples_clin_wide_inclusion)) #n.s.
summary(lm(stypl_sidp_1 ~ clean, couples_clin_wide_inclusion)) #n.s.

summary(lm(szoid_sidp_0 ~ clean, couples_clin_wide_inclusion)) #n.s.
summary(lm(szoid_sidp_1 ~ clean, couples_clin_wide_inclusion)) #n.s.



summary(lm(p_age_0 ~ clean, couples_clin_wide_inclusion)) #.08
summary(lm(p_age_1 ~ clean, couples_clin_wide_inclusion)) #.09

summary(glm(sex_num_1 ~ clean, couples_clin_wide_inclusion, family = "binomial")) #.08
summary(glm(sex_num_0 ~ clean, couples_clin_wide_inclusion, family = "binomial")) #n.s.
summary(glm(race_num_1 ~ clean, couples_clin_wide_inclusion, family = "binomial")) #n.s.
summary(glm(race_num_0 ~ clean, couples_clin_wide_inclusion, family = "binomial")) #n.s.

# Source info and functions --------------------------------------------------------

source("../support_fx.R")


# Load Packages -----------------------------------------------------------

if (!require(pacman)) { install.packages("pacman"); library(pacman) }
p_load(tidyverse, R.matlab,lavaan,lattice, MplusAutomation)



# Load Model Objects ------------------------------------------------------

load("../output/bsem_personalitymod/models_run.RData")
load("../output/bsem_process/model_strings.RData")

# Correlation between SR vs. CR anx/avo
ecravo_mod_avo_mod_df <- models_to_save[["s1_n_ecr_df"]] %>% dplyr::select(PTNUM, ECRavo_1, ECRavo_0) %>% gather(key = "key", value = "ECRavo", ECRavo_0, ECRavo_1) %>% separate(key, into = c("ECR", "DyadID"), sep = "_") %>% dplyr::select(-ECR)
aaravo_mod_avo_mod_df <- models_to_save[["s1_n_aar_df"]]%>% dplyr::select(PTNUM, ECRavo_1, ECRavo_0) %>% gather(key = "key", value = "AARavo", ECRavo_0, ECRavo_1) %>% separate(key, into = c("AAR", "DyadID"), sep = "_") %>% dplyr::select(-AAR)
ecraarav_df <- full_join(ecravo_mod_avo_mod_df, aaravo_mod_avo_mod_df)
cor(ecraarav_df$ECRavo, ecraarav_df$AARavo, use = "complete.obs") # r = .2
summary(lm(ECRavo ~ AARavo, ecraarav_df)) # p = .003
cor(dplyr::filter(ecraarav_df, DyadID == 0) %>% pull(ECRavo), dplyr::filter(ecraarav_df, DyadID == 0) %>% pull(AARavo), use = "complete.obs") # r = .2
cor(dplyr::filter(ecraarav_df, DyadID == 1) %>% pull(ECRavo), dplyr::filter(ecraarav_df, DyadID == 1) %>% pull(AARavo), use = "complete.obs") # r = .22
cor(ecraarav_df$ECRavo, ecraarav_df$AARavo, use = "complete.obs") # r = .2
summary(lm(ECRavo ~ AARavo, dplyr::filter(ecraarav_df, DyadID == 0))) # p = .04
summary(lm(ECRavo ~ AARavo, dplyr::filter(ecraarav_df, DyadID == 1))) # p = .02

ecranx_mod_avo_mod_df <- models_to_save[["s1_n_ecr_df"]] %>% dplyr::select(PTNUM, ECRanx_1, ECRanx_0) %>% gather(key = "key", value = "ECRanx", ECRanx_0, ECRanx_1) %>% separate(key, into = c("ECR", "DyadID"), sep = "_") %>% dplyr::select(-ECR)
aaranx_mod_avo_mod_df <- models_to_save[["s1_n_aar_df"]]%>% dplyr::select(PTNUM, ECRanx_1, ECRanx_0) %>% gather(key = "key", value = "AARanx", ECRanx_0, ECRanx_1) %>% separate(key, into = c("AAR", "DyadID"), sep = "_") %>% dplyr::select(-AAR)
ecraaran_df <- full_join(ecranx_mod_avo_mod_df, aaranx_mod_avo_mod_df)
cor(ecraaran_df$ECRanx, ecraaran_df$AARanx, use = "complete.obs") # r = .35
summary(lm(ECRanx ~ AARanx, ecraaran_df)) # p = < .001

cor(dplyr::filter(ecraaran_df, DyadID == 0) %>% pull(ECRanx), dplyr::filter(ecraaran_df, DyadID == 0) %>% pull(AARanx), use = "complete.obs") # r = .2
cor(dplyr::filter(ecraaran_df, DyadID == 1) %>% pull(ECRanx), dplyr::filter(ecraaran_df, DyadID == 1) %>% pull(AARanx), use = "complete.obs") # r = .45
summary(lm(ECRanx ~ AARanx, dplyr::filter(ecraaran_df, DyadID == 0))) # p = .02
summary(lm(ECRanx ~ AARanx, dplyr::filter(ecraaran_df, DyadID == 1))) # p < .001
