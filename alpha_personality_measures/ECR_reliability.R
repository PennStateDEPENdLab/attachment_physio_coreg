library(haven)
library(dependlab)
library(psych)
library(tidyverse)
# assuming that wd is the folder in which this file lives
ecr <- read_csv("../data/ecr.csv")
View(ecr)
ecr <- dplyr::filter(ecr, mth == 0)
min_value <- 1
max_value <- 7
reverse_keys <- c(2, 3, 5, 11, 15, 17, 19, 22, 25, 27, 29, 
                  31, 33, 35)
reverse_items <- paste0("ECR", reverse_keys)
reverse_items_recode <- sub("$", "r", reverse_items, perl = TRUE)
ecr[, reverse_items_recode] <- lapply(ecr[, reverse_items], 
                                       function(x) {
                                         max_value + min_value - x
                                       })
posnegint_personality <- read.csv("../data/posnegint_personality.csv")
posnegint_personality_ids <- as.vector(posnegint_personality$PTNUM)
ecr_0 <- dplyr::filter(ecr, DyadID == 0, PTNUM %in% posnegint_personality_ids) 
ecr_1 <- dplyr::filter(ecr, DyadID == 1, PTNUM %in% posnegint_personality_ids)


anx_items <- sapply(seq(2, 36, by = 2), function(x) {
  paste0("ECR", x, ifelse(x %in% reverse_keys, "r", 
                                ""))
})
avd_items <- sapply(seq(1, 35, by = 2), function(x) {
  paste0("ECR", x, ifelse(x %in% reverse_keys, "r", 
                                ""))})
ecr_avd_0 <- dplyr::select(ecr_0, avd_items) %>% psych::alpha() # .92
ecr_avd_1 <- dplyr::select(ecr_1, avd_items) %>% psych::alpha() #.91
ecr_anx_0 <- dplyr::select(ecr_0, anx_items) %>% psych::alpha() # .93
ecr_anx_1 <- dplyr::select(ecr_1, anx_items) %>% psych::alpha() #.92