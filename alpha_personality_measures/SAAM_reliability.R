library(tidyverse)
library(psych)
#devtools::install_github("PennStateDEPENdLab/dependlab")
library(dependlab)

saam_post <- read.csv("../data/SAAMPOST.csv") %>% gather(key = "key", value = "value", -PTNUM, -DyadID, -mth) %>% mutate(value = if_else(value == 99, NA_integer_, value)) %>% spread(key = "key", value = "value")
saam_pre <- read.csv("../data/SAAMPRE.csv")%>% gather(key = "key", value = "value", -PTNUM, -DyadID, -mth) %>% mutate(value = if_else(value == 99, NA_integer_, value)) %>% spread(key = "key", value = "value")
posnegint_personality <- read.csv("../data/posnegint_personality.csv")
posnegint_personality_ids <- as.vector(posnegint_personality$PTNUM)
# saam scoring 
# anx items
# I feel a strong need to be unconditionally loved #8
# I really need to feel loved right now #12
# I want to share my feelings with someone #14
# I want to talk with someone who cares for me about things that are worrying me #17
# I wish someone close could seem me now #5
# I wish someone would tell me they really love me #1
# I really need somoeone's emotional support #19
#avo items
#If someone tried to get close to me, I would keep my distance #10
# The idea of being emotionally close makes me nervous #16
# I'm afraid someone will want to get too close to me #9
# I feel alone an dyet don't feel like getting close # 3
# I have mixed feelings about being close to other people #21
# I would be uncomfortable having a good friend or a relationship partner close to me # 2
# I feel like I am loved by others but I really don't care #15

anx_items <- paste0("SAAM", c(8, 12, 14, 17, 5, 1, 19))
avd_items <- paste0("SAAM", c(10, 16, 9, 3, 21, 2, 15))
saam_post_1 <- dplyr::filter(saam_post, DyadID == 1, PTNUM %in% posnegint_personality_ids)
saam_pre_1 <- dplyr::filter(saam_pre, DyadID == 1, PTNUM %in% posnegint_personality_ids)
saam_post_0 <- dplyr::filter(saam_post, DyadID == 0, PTNUM %in% posnegint_personality_ids)
saam_pre_0 <- dplyr::filter(saam_pre, DyadID == 0, PTNUM %in% posnegint_personality_ids)


saam_pre_avd_0 <- dplyr::select(saam_pre_0, avd_items) %>% psych::alpha() #.78
saam_pre_avd_1 <- dplyr::select(saam_pre_1, avd_items) %>% psych::alpha() #.84
saam_pre_anx_0 <- dplyr::select(saam_pre_0, anx_items) %>% psych::alpha() #.82 
saam_pre_anx_1 <- dplyr::select(saam_pre_1, anx_items) %>% psych::alpha() #.81

saam_post_avd_0 <- dplyr::select(saam_post_0, avd_items) %>% psych::alpha() #.86
saam_post_avd_1 <- dplyr::select(saam_post_1, avd_items) %>% psych::alpha() #.88
saam_post_anx_0 <- dplyr::select(saam_post_0, anx_items) %>% psych::alpha() #.87
saam_post_anx_1 <- dplyr::select(saam_post_1, anx_items) %>% psych::alpha() #.88

# Need to mean impute for 8072
saam_pre_mean_imputed <- mean_impute_items(saam_pre, anx_items) 
saam_pre <- saam_pre_mean_imputed %>% mutate(anx = rowSums(dplyr::select(., anx_items)),avo = rowSums(dplyr::select(., avd_items))) # cross validated that this leads to the same results in SAAM pre scored (so no need to re-run analyses related to the inital recoding issue)
