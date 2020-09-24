
# Set up paths ------------------------------------------------------------
## assumes that wd is where this folder is located

bsem_process_res_dir <- "../output/bsem_process"
bsem_personalitymod_res_dir <-"../output/bsem_personalitymod"

# Source info and functions --------------------------------------------------------

source("../support_fx.R")


# Load Packages -----------------------------------------------------------

if (!require(pacman)) { install.packages("pacman"); library(pacman) }
p_load(tidyverse, R.matlab,lavaan,lattice, MplusAutomation)


# Load Data ---------------------------------------------------------------

posnegint_personality <- read.csv("../data/posnegint_personality.csv")

# Load Model String for Simpler Process Models ----------------------------

load( "../output/bsem_process/model_strings.RData")

negint_avo_mod <- syn_model_list_full[["negint_avo_mod"]]
negint_anx_mod <- syn_model_list_full[["negint_anx_mod"]]
negint_sec_mod <- syn_model_list_full[["negint_sec_mod"]]

posint_avo_mod <- syn_model_list_full[["posint_avo_mod"]]
posint_anx_mod <- syn_model_list_full[["posint_anx_mod"]]
posint_sec_mod <- syn_model_list_full[["posint_sec_mod"]]

# Wrangle data for moderation analyses ------------------------------------
center <- function(x) {
  x = x-mean(x, na.rm = TRUE)
  return(x)
}
posnegint_formoderation <- dplyr::select(posnegint_personality, PTNUM, starts_with("p_"), scpt, ccpt, scpr, ccpr, starts_with("ECR"), starts_with("pra"), starts_with("pa"), starts_with("prs"), starts_with("ps")) %>% mutate_at(vars(c(ccpt, ccpr, starts_with("ECR"), starts_with("pra"), starts_with("pa"), starts_with("prs"), starts_with("ps"))), funs(center)) %>% mutate(an0pran0 = ECRanx_0*pranx0, an0pan0 = ECRanx_0*panx0, 
                                                                                                                                                                                                                                                                                                                                                                      an1pran1 = ECRanx_1*pranx1, an1pan1 = ECRanx_1*panx1,
                                                                                                                                                                                                                                                                                                                                                                      an0prav0 = ECRanx_0*pravo0, an0pav0 = ECRanx_0*pavo0, 
                                                                                                                                                                                                                                                                                                                                                                      an1prav1 = ECRanx_1*pravo1, an1pav1 = ECRanx_1*pavo1,
                                                                                                                                                                                                                                                                                                                                                                      an0prse0 = ECRanx_0*prsec0, an0pan0 = ECRanx_0*psec0, 
                                                                                                                                                                                                                                                                                                                                                                      an1prse1 = ECRanx_1*prsec1, an1pan1 = ECRanx_1*psec1,
                                                                                                                                                                                                                                                                                                                                                                      av0pran0 = ECRavo_0*pranx0, av0pan0 = ECRavo_0*panx0, 
                                                                                                                                                                                                                                                                                                                                                                      av1pran1 = ECRavo_1*pranx1, av1pan1 = ECRavo_1*panx1,
                                                                                                                                                                                                                                                                                                                                                                      av0prav0 = ECRavo_0*pravo0, av0pav0 = ECRavo_0*pavo0, 
                                                                                                                                                                                                                                                                                                                                                                      av1prav1 = ECRavo_1*pravo1, av1pav1 = ECRavo_1*pavo1,
                                                                                                                                                                                                                                                                                                                                                                      av0prse0 = ECRavo_0*prsec0, av0pan0 = ECRavo_0*psec0, 
                                                                                                                                                                                                                                                                                                                                                                      av1prse1 = ECRavo_1*prsec1, av1pan1 = ECRavo_1*psec1,
                                                                                                                                                                                                                                                                                                                                                                      an0ccpr0 = ECRanx_0*ccpr, an1ccpt1 = ECRanx_1*ccpt,
                                                                                                                                                                                                                                                                                                                                                                      av0ccpr0 = ECRavo_0*ccpr, av1ccpt1 = ECRanx_1*ccpt,
                                                                                                                                                                                                                                                                                                                                                                      an1ccpr0 = ECRanx_1*ccpr, an0ccpt1 = ECRanx_0*ccpt,
                                                                                                                                                                                                                                                                                                                                                                      av1ccpr0 = ECRavo_1*ccpr, av0ccpt1 = ECRanx_0*ccpt,
                                                                                                                                                                                                                                                                                                                                                                      
                                                                                                                                                                                                                                                                                                                                                                      #partner
                                                                                                                                                                                                                                                                                                                                                                      an1pran0 = ECRanx_1*pranx0, an1pan0 = ECRanx_1*panx0, 
                                                                                                                                                                                                                                                                                                                                                                      an0pran1 = ECRanx_0*pranx1, an0pan1 = ECRanx_0*panx1,
                                                                                                                                                                                                                                                                                                                                                                      an1prav0 = ECRanx_1*pravo0, an1pav0 = ECRanx_1*pavo0, 
                                                                                                                                                                                                                                                                                                                                                                      an0prav1 = ECRanx_0*pravo1, an0pav1 = ECRanx_0*pavo1,
                                                                                                                                                                                                                                                                                                                                                                      an1prse0 = ECRanx_1*prsec0, an1pan0 = ECRanx_1*psec0, 
                                                                                                                                                                                                                                                                                                                                                                      an0prse1 = ECRanx_0*prsec1, an0pan1 = ECRanx_0*psec1,
                                                                                                                                                                                                                                                                                                                                                                      av1pran0 = ECRavo_1*pranx0, av1pan0 = ECRavo_1*panx0, 
                                                                                                                                                                                                                                                                                                                                                                      av0pran1 = ECRavo_0*pranx1, av0pan1 = ECRavo_0*panx1,
                                                                                                                                                                                                                                                                                                                                                                      av1prav0 = ECRavo_1*pravo0, av1pav0 = ECRavo_1*pavo0, 
                                                                                                                                                                                                                                                                                                                                                                      av0prav1 = ECRavo_0*pravo1, av0pav1 = ECRavo_0*pavo1,
                                                                                                                                                                                                                                                                                                                                                                      av1prse0 = ECRavo_1*prsec0, av1pan0 = ECRavo_1*psec0, 
                                                                                                                                                                                                                                                                                                                                                                      av0prse1 = ECRavo_0*prsec1, av0pan1 = ECRavo_0*psec1)


#rescaling for all the models
posnegint_formoderation <- dplyr::mutate_at(posnegint_formoderation, vars(starts_with("p_")), list(~100*.)) %>% dplyr::mutate_at(vars(starts_with("ECR")), list(~10*.))


# Confirm labels correct --------------------------------------------------

left_side <- dplyr::select(posnegint_formoderation, PTNUM, ECRavo_1, ECRavo_0, ECRanx_1, ECRanx_0) 
right_side <- dplyr::select(posnegint_personality, PTNUM, ECRavo_1, ECRavo_0, ECRanx_1, ECRanx_0) %>% rename(ECRavo_1_raw = ECRavo_1,ECRavo_0_raw = ECRavo_0,ECRanx_1_raw = ECRanx_1, ECRnax_0_raw = ECRanx_0)
tocor <- left_join(left_side,right_side) 
cor_ecr <- cor(dplyr::select(tocor, -PTNUM), use = "complete.obs") %>% round(2)
# Initialize Models -------------------------------------------------------

ecravo_allfree_avo_mod_m <-  ecravo_afixed_avo_mod_m <-  ecravo_pfixed_avo_mod_m <- ecravo_apfixed_avo_mod_m <- ecravo_aonly_avo_mod_m <- ecravo_ponly_avo_mod_m <-  ecravo_mod_avo_mod_m <- ""
ecravo_allfree_avo_mod <- ecravo_afixed_avo_mod <- ecravo_pfixed_avo_mod <- ecravo_apfixed_avo_mod <- ecravo_aonly_avo_mod <- ecravo_ponly_avo_mod <-  ecravo_mod_avo_mod <- ""

ecravo_allfree_anx_mod_m <- ecravo_afixed_anx_mod_m  <- ecravo_pfixed_anx_mod_m <- ecravo_apfixed_anx_mod_m <- ecravo_aonly_anx_mod_m <- ecravo_ponly_anx_mod_m <-  ecravo_mod_anx_mod_m <- ""
ecravo_allfree_anx_mod <- ecravo_afixed_anx_mod <- ecravo_pfixed_anx_mod <- ecravo_apfixed_anx_mod <- ecravo_aonly_anx_mod<- ecravo_ponly_anx_mod <- ecravo_mod_anx_mod <- ""

ecravo_allfree_sec_mod_m <- ecravo_afixed_sec_mod_m <- ecravo_pfixed_sec_mod_m <- ecravo_apfixed_sec_mod_m <- ecravo_aonly_sec_mod_m <- ecravo_ponly_sec_mod_m <- ecravo_mod_sec_mod_m <- ""
ecravo_allfree_sec_mod <- ecravo_afixed_sec_mod <- ecravo_pfixed_sec_mod <- ecravo_apfixed_sec_mod <- ecravo_aonly_sec_mod <- ecravo_ponly_sec_mod <-  ecravo_mod_sec_mod <- ""
  

ecranx_allfree_avo_mod_m <- ecranx_afixed_avo_mod_m <- ecranx_pfixed_avo_mod_m <- ecranx_apfixed_avo_mod_m <- ecranx_aonly_avo_mod_m <- ecranx_ponly_avo_mod_m <- ecranx_mod_avo_mod_m <- ""
ecranx_allfree_avo_mod <- ecranx_afixed_avo_mod <- ecranx_pfixed_avo_mod <- ecranx_apfixed_avo_mod <- ecranx_aonly_avo_mod <- ecranx_ponly_avo_mod <-  ecranx_mod_avo_mod <- ""

ecranx_allfree_anx_mod_m <- ecranx_afixed_anx_mod_m <-  ecranx_pfixed_anx_mod_m <- ecranx_apfixed_anx_mod_m <- ecranx_aonly_anx_mod_m <- ecranx_ponly_anx_mod_m <- ecranx_mod_anx_mod_m <- ""
ecranx_allfree_anx_mod <- ecranx_afixed_anx_mod <- ecranx_pfixed_anx_mod <- ecranx_apfixed_anx_mod <- ecranx_aonly_anx_mod <- ecranx_ponly_anx_mod <- ecranx_mod_anx_mod <- ""

ecranx_allfree_sec_mod_m <- ecranx_afixed_sec_mod_m <- ecranx_pfixed_sec_mod_m <- ecranx_apfixed_sec_mod_m <- ecranx_aonly_sec_mod_m <- ecranx_ponly_sec_mod_m <- ecranx_mod_sec_mod_m <- ""
ecranx_allfree_sec_mod <- ecranx_afixed_sec_mod <- ecranx_pfixed_sec_mod <- ecranx_apfixed_sec_mod <- ecranx_aonly_sec_mod <- ecranx_ponly_sec_mod <- ecranx_mod_sec_mod <- ""
# ECR avo gating S1 negint avo ---------------------------------------------


ecravo_allfree_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_allfree_avo_mod_m <- sem(model = ecravo_allfree_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_afixed_avo_mod_m <- sem(model = ecravo_afixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_pfixed_avo_mod_m <- sem(model = ecravo_pfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_apfixed_avo_mod_m <- sem(model = ecravo_apfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_aonly_avo_mod_m <- sem(model = ecravo_aonly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_ponly_avo_mod_m <- sem(model = ecravo_ponly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m)
# Mediation of increases in avoidance mediated by contrarianism. Avoidance in patient also predicted contrarianism in partner during pos interaction. Partner's avoidance gates the extent to which patient avoidance predicts contrarianism. (True in opposite direction with partner). Partner's contrarianism not affected by dispositional ECR.
ecravo_mod_avo_mod<- "

#paths that are not particularly strong
pavo0 ~ 0*ccpr
ccpr ~ 0*pravo0

#strong paths
pavo1 ~  a*ccpt
ccpt ~ c*pravo1 
ccpr ~ c*pravo1
pravo1 ~~ pravo0
pavo1~~pavo0
ccpt ~~ ccpr
pavo1 ~ pravo1
pavo0 ~ pravo0
ac:=a*c
ECRavo_1 ~~ pravo1
ECRavo_0 ~~ pravo0
 ECRavo_1 ~~ 0*av1prav1
ECRavo_1 ~~ 0*av0prav1
ECRavo_0 ~~ 0*av1prav1
ECRavo_0 ~~ 0*av0prav1
av1prav1 ~~ 0*av0prav1
pravo1 ~~ 0*av1prav1
pravo1 ~~ 0*av0prav1
pravo0 ~~ 0*av1prav1
pravo0 ~~ 0*av0prav1 
ccpt ~ l*ECRavo_1

ccpt ~ m*ECRavo_0

ccpr ~ m*ECRavo_1

ccpr ~ l*ECRavo_0 
ccpt ~ av0prav1

ccpt ~ av1prav1

ccpr ~ av0prav1

ccpr ~ av1prav1 

"
ecravo_mod_avo_mod_m <- sem(model = ecravo_mod_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m) 
#Prefers mod

# ECR avo gating S1 negint anx -----------------------------------------
ecravo_allfree_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_allfree_anx_mod_m <- sem(model = ecravo_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_afixed_anx_mod_m <- sem(model = ecravo_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_pfixed_anx_mod_m <- sem(model = ecravo_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_apfixed_anx_mod_m <- sem(model = ecravo_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_aonly_anx_mod_m <- sem(model = ecravo_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_ponly_anx_mod_m <- sem(model = ecravo_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_mod_anx_mod <- "
panx1 ~  f*ccpt
panx0 ~ ccpt
panx0 ~ f*ccpr

ccpt ~ h*pranx1 
ccpt ~ d*pranx0
ccpr ~ d*pranx1
ccpr ~ h*pranx0
pranx1 ~~ pranx0
panx1~~panx0
ccpt ~~ ccpr

panx1~pranx1
panx0 ~ pranx0
ECRavo_1 ~~ pranx1
ECRavo_0 ~~ pranx0
 ECRavo_1 ~~ 0*av1pran1
ECRavo_1 ~~ 0*av1pran0
ECRavo_1 ~~ 0*av0pran1
ECRavo_1 ~~ 0*av0pran0
ECRavo_0 ~~ 0*av1pran1
ECRavo_0 ~~ 0*av1pran0
ECRavo_0 ~~ 0*av0pran1
ECRavo_0 ~~ 0*av0pran0
av1pran1 ~~ 0*av1pran0
av1pran1 ~~ 0*av0pran1
 av1pran1~~ 0*av0pran0
 av1pran0 ~~ 0*av0pran1
 av1pran0 ~~ 0*av0pran0
 av0pran1 ~~ 0*av0pran0
pranx1 ~~ 0*av1pran1
pranx1 ~~ 0*av1pran0
pranx1 ~~ 0*av0pran1
pranx1 ~~ 0*av0pran0
pranx0 ~~ 0*av1pran1
pranx0 ~~ 0*av1pran0
pranx0 ~~ 0*av0pran1
pranx0 ~~ 0*av0pran0 
ccpt ~ ECRavo_1

ccpt ~ 0*ECRavo_0

ccpr ~ 0*ECRavo_1

ccpr ~ ECRavo_0 
ccpt ~ 0*av0pran0

ccpt ~ 0*av0pran1

ccpt ~ 0*av1pran0

ccpt ~ av1pran1

ccpr ~ av0pran0

ccpr ~ 0*av0pran1

ccpr ~ 0*av1pran0

ccpr ~ 0*av1pran1 

"
ecravo_mod_anx_mod_m <- sem(model = ecravo_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)
# Prefers aonly, so set mod to be aonly
ecravo_mod_anx_mod_m <- ecravo_aonly_anx_mod_m 
ecravo_mod_anx_mod <- ecravo_aonly_anx_mod 

# ECR avo gating S1 negint sec --------------------------------------------

ecravo_allfree_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_allfree_sec_mod_m <- sem(model = ecravo_allfree_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_afixed_sec_mod_m <- sem(model = ecravo_afixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_pfixed_sec_mod_m <- sem(model = ecravo_pfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_apfixed_sec_mod_m <- sem(model = ecravo_apfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_aonly_sec_mod_m <- sem(model = ecravo_aonly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_ponly_sec_mod_m <- sem(model = ecravo_ponly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_mod_sec_mod <- "

psec1 ~  f*ccpt
psec1 ~ 0*ccpr
psec0 ~ b*ccpt
psec0 ~ 0*ccpr
ccpt ~ 0*prsec1 
ccpt ~ 0*prsec0
ccpr ~ h*prsec1
ccpr ~ 0*prsec0
prsec1 ~~ prsec0
psec1~~psec0
ccpt ~~ ccpr


psec1~prsec1
psec0 ~ prsec0
 ECRavo_1 ~~ 0*av1prse1 
ECRavo_0 ~~ 0*av1prse1 
ECRavo_1 ~~ 0*av0prse1 
ECRavo_0 ~~ 0*av0prse1 
av1prse1 ~~ av1prse1 
av0prse1 ~~ av0prse1 
prsec1 ~~ 0*av1prse1
prsec1 ~~ 0*av0prse1
prsec0 ~~ 0*av1prse1
prsec0 ~~ 0*av0prse1 
ccpr ~ 0*ECRavo_1

ccpr ~ 0*ECRavo_0 
ccpr ~ b*av1prse1

ccpr ~ 0*av0prse1 
"
ecravo_mod_sec_mod_m <- sem(model = ecravo_mod_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m) # prefers mod

# ECR anx gating S1 negint avo ---------------------------------------------

ecranx_allfree_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecranx_allfree_avo_mod_m <- sem(model = ecranx_allfree_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecranx_afixed_avo_mod_m <- sem(model = ecranx_afixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecranx_pfixed_avo_mod_m <- sem(model = ecranx_pfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecranx_apfixed_avo_mod_m <- sem(model = ecranx_apfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecranx_aonly_avo_mod_m <- sem(model = ecranx_aonly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecranx_ponly_avo_mod_m <- sem(model = ecranx_ponly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m)
#Anxiety does not gate effects of avoidance on coreg, so make mod have the fewest assumptions

ecranx_mod_avo_mod_m <- ecranx_allfree_avo_mod_m
ecranx_mod_avo_mod <- ecranx_allfree_avo_mod


# ECR anx gating S1 negint anx -----------------------------------------

ecranx_allfree_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_allfree_anx_mod_m <- sem(model = ecranx_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_afixed_anx_mod_m <- sem(model = ecranx_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_pfixed_anx_mod_m <- sem(model = ecranx_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_apfixed_anx_mod_m <- sem(model = ecranx_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_aonly_anx_mod_m <- sem(model = ecranx_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_ponly_anx_mod_m <- sem(model = ecranx_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

#Partner baseline anxiety gates effect of patient baseline momentary anxiety on tendency for patient to exhibit contrarianism -- amplifiying the association between baseline anxiety and contrarianism. Partner's avoidance also moderated the effect of partner anxiety on coupling of patient -- however this association is far from significant so probably not worth testing. 
ecranx_mod_anx_mod <- "

panx1 ~  f*ccpt
panx0 ~ ccpt
panx0 ~ f*ccpr

ccpt ~ h*pranx1 
ccpt ~ d*pranx0
ccpr ~ d*pranx1
ccpr ~ h*pranx0
pranx1 ~~ pranx0
panx1~~panx0
ccpt ~~ ccpr

panx1~pranx1
panx0 ~ pranx0
 ECRanx_1 ~~ 0*an1pran1
ECRanx_1 ~~ 0*an1pran0
ECRanx_1 ~~ 0*an0pran1
ECRanx_1 ~~ 0*an0pran0
ECRanx_0 ~~ 0*an1pran1
ECRanx_0 ~~ 0*an1pran0
ECRanx_0 ~~ 0*an0pran1
ECRanx_0 ~~ 0*an0pran0
an1pran1 ~~ 0*an1pran0
an1pran1 ~~ 0*an0pran1
 an1pran1~~ 0*an0pran0
 an1pran0 ~~ 0*an0pran1
 an1pran0 ~~ 0*an0pran0
 an0pran1 ~~ 0*an0pran0
pranx1 ~~ 0*an1pran1
pranx1 ~~ 0*an1pran0
pranx1 ~~ 0*an0pran1
pranx1 ~~ 0*an0pran0
pranx0 ~~ 0*an1pran1
pranx0 ~~ 0*an1pran0
pranx0 ~~ 0*an0pran1
pranx0 ~~ 0*an0pran0
pranx0 ~~ ECRanx_0
pranx1 ~~ ECRanx_1
ccpt ~ 0*ECRanx_1

ccpt ~ ECRanx_0

ccpr ~ ECRanx_1

ccpr ~ 0*ECRanx_0 
ccpt ~ an0pran0

ccpt ~ an0pran1

ccpt ~ 0*an1pran0

ccpt ~ 0*an1pran1

ccpr ~ 0*an0pran0

ccpr ~ 0*an0pran1

ccpr ~ 0*an1pran0

ccpr ~ an1pran1 


"
ecranx_mod_anx_mod_m <- sem(model = ecranx_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)

#Partner baseline anxiety gates effect of patient baseline momentary anxiety on tendency for patient to exhibit contrarianism -- amplifiying the association between baseline anxiety and contrarianism. Partner's avoidance also moderated the effect of partner anxiety on coupling of patient -- however this association is far from significant so probably not worth testing. 





# ECR anx gating S1 negint sec --------------------------------------------


ecranx_allfree_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_allfree_sec_mod_m <- sem(model = ecranx_allfree_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_afixed_sec_mod_m <- sem(model = ecranx_afixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_pfixed_sec_mod_m <- sem(model = ecranx_pfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_apfixed_sec_mod_m <- sem(model = ecranx_apfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_aonly_sec_mod_m <- sem(model = ecranx_aonly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_ponly_sec_mod_m <- sem(model = ecranx_ponly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
ecranx_mod_sec_mod_m <- ecranx_allfree_sec_mod_m
ecranx_mod_sec_mod <- ecranx_allfree_sec_mod
# S1 ECR Negint Save Models  ----------------------------------------------
avav_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m)
avav_syn_model_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod)
df <- mapply(pull_fitmeasures, avav_model_list, avav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_n_ecr_avo_saam_avo"
fulldf <- df

avan_model_list <- c(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)
avan_syn_model_list <- c(ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod, ecravo_mod_anx_mod)
df <- mapply(pull_fitmeasures, avan_model_list, avan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_n_ecr_avo_saam_anx"
fulldf <- bind_rows(fulldf, df)

avse_model_list <- c(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m)
avse_syn_model_list <- c(ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod, ecravo_mod_sec_mod)
df <- mapply(pull_fitmeasures, avse_model_list, avse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_n_ecr_avo_saam_sec"
fulldf <- bind_rows(fulldf, df)


anav_model_list <- c(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m, ecranx_mod_avo_mod_m)
anav_syn_model_list <- c(ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod, ecranx_mod_avo_mod)
df <- mapply(pull_fitmeasures, anav_model_list, anav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_n_ecr_anx_saam_avo"
fulldf <- bind_rows(fulldf, df)

anan_model_list <- c(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)
anan_syn_model_list <- c(ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod, ecranx_mod_anx_mod)
df <- mapply(pull_fitmeasures, anan_model_list, anan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_n_ecr_anx_saam_anx"
fulldf <- bind_rows(fulldf, df)

anse_model_list <- c(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
anse_syn_model_list <- c(ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
df <- mapply(pull_fitmeasures, anse_model_list, anse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_n_ecr_anx_saam_sec"
fulldf <- bind_rows(fulldf, df)


s1_n_ecr_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m,
                       ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m,ecravo_mod_anx_mod_m,
                       ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m,ecravo_mod_sec_mod_m, 
                       ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m,ecranx_mod_avo_mod_m,
                       ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m,  ecranx_mod_anx_mod_m,
                       ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
s1_n_ecr_syn_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod,
                       ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod,ecravo_mod_anx_mod,
                       ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod,ecravo_mod_sec_mod,
                       ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod,ecranx_mod_avo_mod,
                       ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod,  ecranx_mod_anx_mod,
                       ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
s1_n_ecr_df <- posnegint_formoderation
names(s1_n_ecr_model_list) <- c("ecravo_allfree_avo_mod_m", "ecravo_afixed_avo_mod_m", "ecravo_pfixed_avo_mod_m","ecravo_apfixed_avo_mod_m","ecravo_aonly_avo_mod_m","ecravo_ponly_avo_mod_m", "ecravo_mod_avo_mod_m",
                         "ecravo_allfree_anx_mod_m", "ecravo_afixed_anx_mod_m", "ecravo_pfixed_anx_mod_m","ecravo_apfixed_anx_mod_m","ecravo_aonly_anx_mod_m","ecravo_ponly_anx_mod_m","ecravo_mod_anx_mod_m",
                         "ecravo_allfree_sec_mod_m", "ecravo_afixed_sec_mod_m", "ecravo_pfixed_sec_mod_m","ecravo_apfixed_sec_mod_m","ecravo_aonly_sec_mod_m","ecravo_ponly_sec_mod_m","ecravo_mod_sec_mod_m",
                         "ecranx_allfree_avo_mod_m", "ecranx_afixed_avo_mod_m", "ecranx_pfixed_avo_mod_m","ecranx_apfixed_avo_mod_m","ecranx_aonly_avo_mod_m","ecranx_ponly_avo_mod_m","ecranx_mod_avo_mod_m",
                         "ecranx_allfree_anx_mod_m", "ecranx_afixed_anx_mod_m", "ecranx_pfixed_anx_mod_m","ecranx_apfixed_anx_mod_m","ecranx_aonly_anx_mod_m","ecranx_ponly_anx_mod_m",  "ecranx_mod_anx_mod_m",
                         "ecranx_allfree_sec_mod_m", "ecranx_afixed_sec_mod_m", "ecranx_pfixed_sec_mod_m","ecranx_apfixed_sec_mod_m","ecranx_aonly_sec_mod_m","ecranx_ponly_sec_mod_m", "ecranx_mod_sec_mod_m")
names(s1_n_ecr_syn_list) <- c("ecravo_allfree_avo_mod", "ecravo_afixed_avo_mod", "ecravo_pfixed_avo_mod","ecravo_apfixed_avo_mod","ecravo_aonly_avo_mod","ecravo_ponly_avo_mod", "ecravo_mod_avo_mod",
                       "ecravo_allfree_anx_mod", "ecravo_afixed_anx_mod", "ecravo_pfixed_anx_mod","ecravo_apfixed_anx_mod","ecravo_aonly_anx_mod","ecravo_ponly_anx_mod","ecravo_mod_anx_mod",
                       "ecravo_allfree_sec_mod", "ecravo_afixed_sec_mod", "ecravo_pfixed_sec_mod","ecravo_apfixed_sec_mod","ecravo_aonly_sec_mod","ecravo_ponly_sec_mod","ecravo_mod_sec_mod",
                       "ecranx_allfree_avo_mod", "ecranx_afixed_avo_mod", "ecranx_pfixed_avo_mod","ecranx_apfixed_avo_mod","ecranx_aonly_avo_mod","ecranx_ponly_avo_mod","ecranx_mod_avo_mod",
                       "ecranx_allfree_anx_mod", "ecranx_afixed_anx_mod", "ecranx_pfixed_anx_mod","ecranx_apfixed_anx_mod","ecranx_aonly_anx_mod","ecranx_ponly_anx_mod",  "ecranx_mod_anx_mod",
                       "ecranx_allfree_sec_mod", "ecranx_afixed_sec_mod", "ecranx_pfixed_sec_mod","ecranx_apfixed_sec_mod","ecranx_aonly_sec_mod","ecranx_ponly_sec_mod", "ecranx_mod_sec_mod")

# Initialize Models -------------------------------------------------------

ecravo_allfree_avo_mod_m <-  ecravo_afixed_avo_mod_m <-  ecravo_pfixed_avo_mod_m <- ecravo_apfixed_avo_mod_m <- ecravo_aonly_avo_mod_m <- ecravo_ponly_avo_mod_m <-  ecravo_mod_avo_mod_m <- ""
ecravo_allfree_avo_mod <- ecravo_afixed_avo_mod <- ecravo_pfixed_avo_mod <- ecravo_apfixed_avo_mod <- ecravo_aonly_avo_mod <- ecravo_ponly_avo_mod <-  ecravo_mod_avo_mod <- ""

ecravo_allfree_anx_mod_m <- ecravo_afixed_anx_mod_m  <- ecravo_pfixed_anx_mod_m <- ecravo_apfixed_anx_mod_m <- ecravo_aonly_anx_mod_m <- ecravo_ponly_anx_mod_m <-  ecravo_mod_anx_mod_m <- ""
ecravo_allfree_anx_mod <- ecravo_afixed_anx_mod <- ecravo_pfixed_anx_mod <- ecravo_apfixed_anx_mod <- ecravo_aonly_anx_mod<- ecravo_ponly_anx_mod <- ecravo_mod_anx_mod <- ""

ecravo_allfree_sec_mod_m <- ecravo_afixed_sec_mod_m <- ecravo_pfixed_sec_mod_m <- ecravo_apfixed_sec_mod_m <- ecravo_aonly_sec_mod_m <- ecravo_ponly_sec_mod_m <- ecravo_mod_sec_mod_m <- ""
ecravo_allfree_sec_mod <- ecravo_afixed_sec_mod <- ecravo_pfixed_sec_mod <- ecravo_apfixed_sec_mod <- ecravo_aonly_sec_mod <- ecravo_ponly_sec_mod <-  ecravo_mod_sec_mod <- ""

ecranx_allfree_avo_mod_m <- ecranx_afixed_avo_mod_m <- ecranx_pfixed_avo_mod_m <- ecranx_apfixed_avo_mod_m <- ecranx_aonly_avo_mod_m <- ecranx_ponly_avo_mod_m <- ecranx_mod_avo_mod_m <- ""
ecranx_allfree_avo_mod <- ecranx_afixed_avo_mod <- ecranx_pfixed_avo_mod <- ecranx_apfixed_avo_mod <- ecranx_aonly_avo_mod <- ecranx_ponly_avo_mod <-  ecranx_mod_avo_mod <- ""

ecranx_allfree_anx_mod_m <- ecranx_afixed_anx_mod_m <-  ecranx_pfixed_anx_mod_m <- ecranx_apfixed_anx_mod_m <- ecranx_aonly_anx_mod_m <- ecranx_ponly_anx_mod_m <- ecranx_mod_anx_mod_m <- ""
ecranx_allfree_anx_mod <- ecranx_afixed_anx_mod <- ecranx_pfixed_anx_mod <- ecranx_apfixed_anx_mod <- ecranx_aonly_anx_mod <- ecranx_ponly_anx_mod <- ecranx_mod_anx_mod <- ""

ecranx_allfree_sec_mod_m <- ecranx_afixed_sec_mod_m <- ecranx_pfixed_sec_mod_m <- ecranx_apfixed_sec_mod_m <- ecranx_aonly_sec_mod_m <- ecranx_ponly_sec_mod_m <- ecranx_mod_sec_mod_m <- ""
ecranx_allfree_sec_mod <- ecranx_afixed_sec_mod <- ecranx_pfixed_sec_mod <- ecranx_apfixed_sec_mod <- ecranx_aonly_sec_mod <- ecranx_ponly_sec_mod <- ecranx_mod_sec_mod <- ""

# ECR avo gating S2 negint avo ---------------------------------------------
ecravo_allfree_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_allfree_avo_mod_m <- sem(model = ecravo_allfree_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_afixed_avo_mod_m <- sem(model = ecravo_afixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_pfixed_avo_mod_m <- sem(model = ecravo_pfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_apfixed_avo_mod_m <- sem(model = ecravo_apfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_aonly_avo_mod_m <- sem(model = ecravo_aonly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_ponly_avo_mod_m <- sem(model = ecravo_ponly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m) #afixed pfixed and apfixed going to be equivalent because there's no corresonding partner pathways for the partner in the avo model
ecravo_mod_avo_mod_m <- ecravo_allfree_avo_mod_m
ecravo_mod_avo_mod <- ecravo_allfree_avo_mod

# in terms of s2 moderation, no evidence of coupling between contrarianism and increases in avoidance by ECR avoidance -- although direct effect of ECR avoidance on increases in avoidance
# ECR avo gating S2 negint anx -----------------------------------------



ecravo_allfree_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_allfree_anx_mod_m <- sem(model = ecravo_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_afixed_anx_mod_m <- sem(model = ecravo_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_pfixed_anx_mod_m <- sem(model = ecravo_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_apfixed_anx_mod_m <- sem(model = ecravo_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_aonly_anx_mod_m <- sem(model = ecravo_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_ponly_anx_mod_m <- sem(model = ecravo_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m)

ecravo_mod_anx_mod <- "
panx1 ~  f*ccpt
panx0 ~ ccpt
panx0 ~ f*ccpr

ccpt ~ h*pranx1 
ccpt ~ d*pranx0
ccpr ~ d*pranx1
ccpr ~ h*pranx0
pranx1 ~~ pranx0
panx1~~panx0
ccpt ~~ ccpr

panx1~pranx1
panx0 ~ pranx0
 ECRavo_1 ~~ 0*av1ccpt1 
ECRavo_0 ~~ 0*av1ccpt1 
ECRavo_1 ~~ 0*av1ccpr0 
ECRavo_0 ~~ 0*av1ccpr0 
ECRavo_1 ~~ 0*av0ccpt1 
ECRavo_0 ~~ 0*av0ccpt1 
ECRavo_1 ~~ 0*av0ccpr0 
ECRavo_0 ~~ 0*av0ccpr0
av1ccpt1 ~~ av1ccpt1 
av1ccpr0 ~~ av1ccpr0 
av0ccpt1 ~~ av0ccpt1 
av0ccpr0 ~~ av0ccpr0
av1ccpt1 ~~ 0*ccpt 
av1ccpt1 ~~ 0*ccpr 
av1ccpr0 ~~ 0*ccpt 
av1ccpr0 ~~ 0*ccpr 
av0ccpt1 ~~ 0*ccpt 
av0ccpt1 ~~ 0*ccpr 
av0ccpr0 ~~ 0*ccpt 
av0ccpr0 ~~ 0*ccpr 
panx1 ~ 0*ECRavo_1

panx1 ~ 0*ECRavo_0

panx0 ~ 0*ECRavo_1

panx0 ~ ECRavo_0 
panx1 ~ av1ccpt1

panx1 ~ 0*av0ccpt1

panx0 ~ l*av1ccpt1

panx0 ~ av1ccpr0

panx0 ~ l*av0ccpt1

panx0 ~ av0ccpr0 
"
ecravo_mod_anx_mod_m <- sem(model = ecravo_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)




# ECR avo gating S2 negint sec --------------------------------------------

ecravo_allfree_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_allfree_sec_mod_m <- sem(model = ecravo_allfree_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_afixed_sec_mod_m <- sem(model = ecravo_afixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_pfixed_sec_mod_m <- sem(model = ecravo_pfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_apfixed_sec_mod_m <- sem(model = ecravo_apfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_aonly_sec_mod_m <- sem(model = ecravo_aonly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_ponly_sec_mod_m <- sem(model = ecravo_ponly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m)
ecravo_mod_sec_mod <- "
psec1 ~  f*ccpt
psec1 ~ 0*ccpr
psec0 ~ b*ccpt
psec0 ~ 0*ccpr
ccpt ~ 0*prsec1 
ccpt ~ 0*prsec0
ccpr ~ h*prsec1
ccpr ~ 0*prsec0
prsec1 ~~ prsec0
psec1~~psec0
ccpt ~~ ccpr


psec1~prsec1
psec0 ~ prsec0
 ECRavo_1 ~~ 0*av1ccpt1 
ECRavo_0 ~~ 0*av1ccpt1 
ECRavo_1 ~~ 0*av0ccpt1 
ECRavo_0 ~~ 0*av0ccpt1 
av1ccpt1 ~~ av1ccpt1 
av0ccpt1 ~~ av0ccpt1 
av1ccpt1 ~~ 0*ccpt 
av1ccpt1 ~~ 0*ccpr 
av0ccpt1 ~~ 0*ccpt 
av0ccpt1 ~~ 0*ccpr  
psec1 ~ 0*ECRavo_1

psec1 ~ ECRavo_0

psec0 ~ ECRavo_1

psec0 ~ k*ECRavo_0 
psec1 ~ 0*av1ccpt1

psec1 ~ 0*av0ccpt1

psec0 ~ k*av1ccpt1

psec0 ~ av0ccpt1 

"
ecravo_mod_sec_mod_m <- sem(model = ecravo_mod_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m)

# ECR anx gating S2 negint avo ---------------------------------------------


ecranx_allfree_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ),model_syntax =  negint_avo_mod),"\n")
ecranx_allfree_avo_mod_m <- sem(model = ecranx_allfree_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), model_syntax = negint_avo_mod),"\n")
ecranx_afixed_avo_mod_m <- sem(model = ecranx_afixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), model_syntax = negint_avo_mod),"\n")
ecranx_pfixed_avo_mod_m <- sem(model = ecranx_pfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ),model_syntax =  negint_avo_mod),"\n")
ecranx_apfixed_avo_mod_m <- sem(model = ecranx_apfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ),model_syntax =  negint_avo_mod),"\n")
ecranx_aonly_avo_mod_m <- sem(model = ecranx_aonly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_avo_mod <- paste(negint_mod_mod, ECR_moderation("anx","avo", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ),model_syntax =  negint_avo_mod),"\n")
ecranx_ponly_avo_mod_m <- sem(model = ecranx_ponly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m)
ecranx_mod_avo_mod_m <- ecranx_allfree_avo_mod_m 
ecranx_mod_avo_mod <- ecranx_allfree_avo_mod
#No evidence that increases in avoidance related to contrarianism gated by anxiety. Direct effect where increases in avoidance in patient predicted by high levels of anxiety in either patient or partner. 

# ECR anx gating S2 negint anx -----------------------------------------

ecranx_allfree_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_allfree_anx_mod_m <- sem(model = ecranx_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_afixed_anx_mod_m <- sem(model = ecranx_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_pfixed_anx_mod_m <- sem(model = ecranx_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_apfixed_anx_mod_m <- sem(model = ecranx_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_aonly_anx_mod_m <- sem(model = ecranx_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_ponly_anx_mod_m <- sem(model = ecranx_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
ecranx_mod_anx_mod <- "

panx1 ~  f*ccpt
panx0 ~ ccpt
panx0 ~ f*ccpr

ccpt ~ h*pranx1 
ccpt ~ d*pranx0
ccpr ~ d*pranx1
ccpr ~ h*pranx0
pranx1 ~~ pranx0
panx1~~panx0
ccpt ~~ ccpr

panx1~pranx1
panx0 ~ pranx0
 ECRanx_1 ~~ 0*an1ccpt1 
ECRanx_0 ~~ 0*an1ccpt1 
ECRanx_1 ~~ 0*an1ccpr0 
ECRanx_0 ~~ 0*an1ccpr0 
ECRanx_1 ~~ 0*an0ccpt1 
ECRanx_0 ~~ 0*an0ccpt1 
ECRanx_1 ~~ 0*an0ccpr0 
ECRanx_0 ~~ 0*an0ccpr0
an1ccpt1 ~~ an1ccpt1 
an1ccpr0 ~~ an1ccpr0 
an0ccpt1 ~~ an0ccpt1 
an0ccpr0 ~~ an0ccpr0
an1ccpt1 ~~ 0*ccpt 
an1ccpt1 ~~ 0*ccpr 
an1ccpr0 ~~ 0*ccpt 
an1ccpr0 ~~ 0*ccpr 
an0ccpt1 ~~ 0*ccpt 
an0ccpt1 ~~ 0*ccpr 
an0ccpr0 ~~ 0*ccpt 
an0ccpr0 ~~ 0*ccpr 
panx1 ~ 0*ECRanx_1

panx1 ~ 0*ECRanx_0

panx0 ~ 0*ECRanx_1

panx0 ~ ECRanx_0 
panx1 ~ an1ccpt1

panx1 ~ 0*an0ccpt1

panx0 ~ 0*an1ccpt1

panx0 ~ 0*an1ccpr0

panx0 ~ an0ccpt1

panx0 ~ an0ccpr0 
"
ecranx_mod_anx_mod_m <- sem(model = ecranx_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
#Anxiety gates extent to which cross-coupling predicts increases in anxiety, such patients anxiety dampens the effet for patients. Conversely, partner's anxiety amplifies this effect
#Check to see if flipping happening
anova(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)

# ECR anx gating S2 negint sec --------------------------------------------


ecranx_allfree_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_allfree_sec_mod_m <- sem(model = ecranx_allfree_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_afixed_sec_mod_m <- sem(model = ecranx_afixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_pfixed_sec_mod_m <- sem(model = ecranx_pfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_apfixed_sec_mod_m <- sem(model = ecranx_apfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_aonly_sec_mod_m <- sem(model = ecranx_aonly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_ponly_sec_mod_m <- sem(model = ecranx_ponly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m)
ecranx_mod_sec_mod <- "
psec1 ~  f*ccpt
psec1 ~ 0*ccpr
psec0 ~ b*ccpt
psec0 ~ 0*ccpr
ccpt ~ 0*prsec1 
ccpt ~ 0*prsec0
ccpr ~ h*prsec1
ccpr ~ 0*prsec0
prsec1 ~~ prsec0
psec1~~psec0
ccpt ~~ ccpr


psec1~prsec1
psec0 ~ prsec0
 ECRanx_1 ~~ 0*an1ccpt1 
ECRanx_0 ~~ 0*an1ccpt1 
ECRanx_1 ~~ 0*an0ccpt1 
ECRanx_0 ~~ 0*an0ccpt1 
an1ccpt1 ~~ an1ccpt1 
an0ccpt1 ~~ an0ccpt1 
an1ccpt1 ~~ 0*ccpt 
an1ccpt1 ~~ 0*ccpr 
an0ccpt1 ~~ 0*ccpt 
an0ccpt1 ~~ 0*ccpr  
psec1 ~ m*ECRanx_1

psec1 ~ 0*ECRanx_0

psec0 ~ v*ECRanx_1

psec0 ~ v*ECRanx_0 
psec1 ~ 0*an1ccpt1

psec1 ~ 0*an0ccpt1

psec0 ~ m*an1ccpt1

psec0 ~ h*an0ccpt1 

"
ecranx_mod_sec_mod_m <- sem(model = ecranx_mod_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

# S2 ECR Negint Save Models  ----------------------------------------------
avav_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m)
avav_syn_model_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod)
df <- mapply(pull_fitmeasures, avav_model_list, avav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_n_ecr_avo_saam_avo"
fulldf <- df

avan_model_list <- c(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)
avan_syn_model_list <- c(ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod, ecravo_mod_anx_mod)
df <- mapply(pull_fitmeasures, avan_model_list, avan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_n_ecr_avo_saam_anx"
fulldf <- bind_rows(fulldf, df)

avse_model_list <- c(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m)
avse_syn_model_list <- c(ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod, ecravo_mod_sec_mod)
df <- mapply(pull_fitmeasures, avse_model_list, avse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_n_ecr_avo_saam_sec"
fulldf <- bind_rows(fulldf, df)


anav_model_list <- c(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m, ecranx_mod_avo_mod_m)
anav_syn_model_list <- c(ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod, ecranx_mod_avo_mod)
df <- mapply(pull_fitmeasures, anav_model_list, anav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_n_ecr_anx_saam_avo"
fulldf <- bind_rows(fulldf, df)

anan_model_list <- c(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)
anan_syn_model_list <- c(ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod, ecranx_mod_anx_mod)
df <- mapply(pull_fitmeasures, anan_model_list, anan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_n_ecr_anx_saam_anx"
fulldf <- bind_rows(fulldf, df)

anse_model_list <- c(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
anse_syn_model_list <- c(ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
df <- mapply(pull_fitmeasures, anse_model_list, anse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_n_ecr_anx_saam_sec"
fulldf <- bind_rows(fulldf, df)


s2_n_ecr_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m,
                         ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m,ecravo_mod_anx_mod_m,
                         ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m,ecravo_mod_sec_mod_m, 
                         ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m,ecranx_mod_avo_mod_m,
                         ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m,  ecranx_mod_anx_mod_m,
                         ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
s2_n_ecr_syn_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod,
                       ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod,ecravo_mod_anx_mod,
                       ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod,ecravo_mod_sec_mod,
                       ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod,ecranx_mod_avo_mod,
                       ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod,  ecranx_mod_anx_mod,
                       ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
s2_n_ecr_df <- posnegint_formoderation
names(s2_n_ecr_model_list) <- c("ecravo_allfree_avo_mod_m", "ecravo_afixed_avo_mod_m", "ecravo_pfixed_avo_mod_m","ecravo_apfixed_avo_mod_m","ecravo_aonly_avo_mod_m","ecravo_ponly_avo_mod_m", "ecravo_mod_avo_mod_m",
                                "ecravo_allfree_anx_mod_m", "ecravo_afixed_anx_mod_m", "ecravo_pfixed_anx_mod_m","ecravo_apfixed_anx_mod_m","ecravo_aonly_anx_mod_m","ecravo_ponly_anx_mod_m","ecravo_mod_anx_mod_m",
                                "ecravo_allfree_sec_mod_m", "ecravo_afixed_sec_mod_m", "ecravo_pfixed_sec_mod_m","ecravo_apfixed_sec_mod_m","ecravo_aonly_sec_mod_m","ecravo_ponly_sec_mod_m","ecravo_mod_sec_mod_m",
                                "ecranx_allfree_avo_mod_m", "ecranx_afixed_avo_mod_m", "ecranx_pfixed_avo_mod_m","ecranx_apfixed_avo_mod_m","ecranx_aonly_avo_mod_m","ecranx_ponly_avo_mod_m","ecranx_mod_avo_mod_m",
                                "ecranx_allfree_anx_mod_m", "ecranx_afixed_anx_mod_m", "ecranx_pfixed_anx_mod_m","ecranx_apfixed_anx_mod_m","ecranx_aonly_anx_mod_m","ecranx_ponly_anx_mod_m",  "ecranx_mod_anx_mod_m",
                                "ecranx_allfree_sec_mod_m", "ecranx_afixed_sec_mod_m", "ecranx_pfixed_sec_mod_m","ecranx_apfixed_sec_mod_m","ecranx_aonly_sec_mod_m","ecranx_ponly_sec_mod_m", "ecranx_mod_sec_mod_m")
names(s2_n_ecr_syn_list) <- c("ecravo_allfree_avo_mod", "ecravo_afixed_avo_mod", "ecravo_pfixed_avo_mod","ecravo_apfixed_avo_mod","ecravo_aonly_avo_mod","ecravo_ponly_avo_mod", "ecravo_mod_avo_mod",
                              "ecravo_allfree_anx_mod", "ecravo_afixed_anx_mod", "ecravo_pfixed_anx_mod","ecravo_apfixed_anx_mod","ecravo_aonly_anx_mod","ecravo_ponly_anx_mod","ecravo_mod_anx_mod",
                              "ecravo_allfree_sec_mod", "ecravo_afixed_sec_mod", "ecravo_pfixed_sec_mod","ecravo_apfixed_sec_mod","ecravo_aonly_sec_mod","ecravo_ponly_sec_mod","ecravo_mod_sec_mod",
                              "ecranx_allfree_avo_mod", "ecranx_afixed_avo_mod", "ecranx_pfixed_avo_mod","ecranx_apfixed_avo_mod","ecranx_aonly_avo_mod","ecranx_ponly_avo_mod","ecranx_mod_avo_mod",
                              "ecranx_allfree_anx_mod", "ecranx_afixed_anx_mod", "ecranx_pfixed_anx_mod","ecranx_apfixed_anx_mod","ecranx_aonly_anx_mod","ecranx_ponly_anx_mod",  "ecranx_mod_anx_mod",
                              "ecranx_allfree_sec_mod", "ecranx_afixed_sec_mod", "ecranx_pfixed_sec_mod","ecranx_apfixed_sec_mod","ecranx_aonly_sec_mod","ecranx_ponly_sec_mod", "ecranx_mod_sec_mod")

# Initialize Models -------------------------------------------------------

ecravo_allfree_avo_mod_m <-  ecravo_afixed_avo_mod_m <-  ecravo_pfixed_avo_mod_m <- ecravo_apfixed_avo_mod_m <- ecravo_aonly_avo_mod_m <- ecravo_ponly_avo_mod_m <-  ecravo_mod_avo_mod_m <- ""
ecravo_allfree_avo_mod <- ecravo_afixed_avo_mod <- ecravo_pfixed_avo_mod <- ecravo_apfixed_avo_mod <- ecravo_aonly_avo_mod <- ecravo_ponly_avo_mod <-  ecravo_mod_avo_mod <- ""

ecravo_allfree_anx_mod_m <- ecravo_afixed_anx_mod_m  <- ecravo_pfixed_anx_mod_m <- ecravo_apfixed_anx_mod_m <- ecravo_aonly_anx_mod_m <- ecravo_ponly_anx_mod_m <-  ecravo_mod_anx_mod_m <- ""
ecravo_allfree_anx_mod <- ecravo_afixed_anx_mod <- ecravo_pfixed_anx_mod <- ecravo_apfixed_anx_mod <- ecravo_aonly_anx_mod<- ecravo_ponly_anx_mod <- ecravo_mod_anx_mod <- ""

ecravo_allfree_sec_mod_m <- ecravo_afixed_sec_mod_m <- ecravo_pfixed_sec_mod_m <- ecravo_apfixed_sec_mod_m <- ecravo_aonly_sec_mod_m <- ecravo_ponly_sec_mod_m <- ecravo_mod_sec_mod_m <- ""
ecravo_allfree_sec_mod <- ecravo_afixed_sec_mod <- ecravo_pfixed_sec_mod <- ecravo_apfixed_sec_mod <- ecravo_aonly_sec_mod <- ecravo_ponly_sec_mod <-  ecravo_mod_sec_mod <- ""

ecranx_allfree_avo_mod_m <- ecranx_afixed_avo_mod_m <- ecranx_pfixed_avo_mod_m <- ecranx_apfixed_avo_mod_m <- ecranx_aonly_avo_mod_m <- ecranx_ponly_avo_mod_m <- ecranx_mod_avo_mod_m <- ""
ecranx_allfree_avo_mod <- ecranx_afixed_avo_mod <- ecranx_pfixed_avo_mod <- ecranx_apfixed_avo_mod <- ecranx_aonly_avo_mod <- ecranx_ponly_avo_mod <-  ecranx_mod_avo_mod <- ""

ecranx_allfree_anx_mod_m <- ecranx_afixed_anx_mod_m <-  ecranx_pfixed_anx_mod_m <- ecranx_apfixed_anx_mod_m <- ecranx_aonly_anx_mod_m <- ecranx_ponly_anx_mod_m <- ecranx_mod_anx_mod_m <- ""
ecranx_allfree_anx_mod <- ecranx_afixed_anx_mod <- ecranx_pfixed_anx_mod <- ecranx_apfixed_anx_mod <- ecranx_aonly_anx_mod <- ecranx_ponly_anx_mod <- ecranx_mod_anx_mod <- ""

ecranx_allfree_sec_mod_m <- ecranx_afixed_sec_mod_m <- ecranx_pfixed_sec_mod_m <- ecranx_apfixed_sec_mod_m <- ecranx_aonly_sec_mod_m <- ecranx_ponly_sec_mod_m <- ecranx_mod_sec_mod_m <- ""
ecranx_allfree_sec_mod <- ecranx_afixed_sec_mod <- ecranx_pfixed_sec_mod <- ecranx_apfixed_sec_mod <- ecranx_aonly_sec_mod <- ecranx_ponly_sec_mod <- ecranx_mod_sec_mod <- ""



# ECR avo gating S1 posint avo ---------------------------------------------
ecravo_allfree_avo_mod <- paste(posint_avo_mod, ECR_moderation("avo","avo", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecravo_allfree_avo_mod_m <- sem(model = ecravo_allfree_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_avo_mod <- paste(posint_avo_mod, ECR_moderation("avo","avo", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecravo_afixed_avo_mod_m <- sem(model = ecravo_afixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_avo_mod <- paste(posint_avo_mod, ECR_moderation("avo","avo", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecravo_pfixed_avo_mod_m <- sem(model = ecravo_pfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_avo_mod <- paste(posint_avo_mod, ECR_moderation("avo","avo", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecravo_apfixed_avo_mod_m <- sem(model = ecravo_apfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_avo_mod <- paste(posint_avo_mod, ECR_moderation("avo","avo", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecravo_aonly_avo_mod_m <- sem(model = ecravo_aonly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_avo_mod <- paste(posint_avo_mod, ECR_moderation("avo","avo", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecravo_ponly_avo_mod_m <- sem(model = ecravo_ponly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m)
ecravo_mod_avo_mod_m <- ecravo_allfree_avo_mod_m
ecravo_mod_avo_mod <- ecravo_allfree_avo_mod
# No evidence that baseline avoidance on contrarianism in partner is gated by braoder avoidance. Direct effect of braoder avoidance in patient and partner such that avoidance predicts dependency in partner in positive interaction.

# ECR avo gating S1 posint anx -----------------------------------------

ecravo_allfree_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_allfree_anx_mod_m <- sem(model = ecravo_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_afixed_anx_mod_m <- sem(model = ecravo_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_pfixed_anx_mod_m <- sem(model = ecravo_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_apfixed_anx_mod_m <- sem(model = ecravo_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_aonly_anx_mod_m <- sem(model = ecravo_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_ponly_anx_mod_m <- sem(model = ecravo_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m)

ecravo_mod_anx_mod_m <- ecravo_allfree_anx_mod_m
ecravo_mod_anx_mod <- ecravo_allfree_anx_mod

# No evidence that baseline avoidance gates association between baseline anxiety and coupling during positive intearction. Diret effect of avoidance on on partners coupling such that more avoidant partners exhibits more dependency during the positive interaction.
# ECR avo gating S1 posint sec --------------------------------------------

ecravo_allfree_sec_mod <- paste(posint_sec_mod, ECR_moderation("avo","sec", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecravo_allfree_sec_mod_m <- sem(model = ecravo_allfree_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_sec_mod <- paste(posint_sec_mod, ECR_moderation("avo","sec", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecravo_afixed_sec_mod_m <- sem(model = ecravo_afixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_sec_mod <- paste(posint_sec_mod, ECR_moderation("avo","sec", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecravo_pfixed_sec_mod_m <- sem(model = ecravo_pfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_sec_mod <- paste(posint_sec_mod, ECR_moderation("avo","sec", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecravo_apfixed_sec_mod_m <- sem(model = ecravo_apfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_sec_mod <- paste(posint_sec_mod, ECR_moderation("avo","sec", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecravo_aonly_sec_mod_m <- sem(model = ecravo_aonly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_sec_mod <- paste(posint_sec_mod, ECR_moderation("avo","sec", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecravo_ponly_sec_mod_m <- sem(model = ecravo_ponly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m)

ecravo_mod_sec_mod <- "
p_ccpt ~ 0*psec1 #a
p_ccpt ~ 0*prsec1 #a
p_ccpt ~ 0*psec0 #p
p_ccpt ~ prsec0 #p
p_ccpr ~ 0*psec1 #p
p_ccpr ~ prsec1 #p
p_ccpr ~ 0*psec0 # a
p_ccpr ~ k*prsec0 #a

prsec1 ~~ prsec0
psec1~~psec0
p_ccpt ~~ p_ccpr
psec1 ~ prsec1
psec0 ~prsec0

 ECRavo_1 ~~ 0*av1prse1 
ECRavo_0 ~~ 0*av1prse1 
ECRavo_1 ~~ 0*av1prse0 
ECRavo_0 ~~ 0*av1prse0 
ECRavo_1 ~~ 0*av0prse1 
ECRavo_0 ~~ 0*av0prse1 
ECRavo_1 ~~ 0*av0prse0 
ECRavo_0 ~~ 0*av0prse0
av1prse1 ~~ av1prse1 
av1prse0 ~~ av1prse0 
av0prse1 ~~ av0prse1 
av0prse0 ~~ av0prse0
prsec1 ~~ 0*av1prse1
prsec1 ~~ 0*av1prse0
prsec1 ~~ 0*av0prse1
prsec1 ~~ 0*av0prse0
prsec0 ~~ 0*av1prse1
prsec0 ~~ 0*av1prse0
prsec0 ~~ 0*av0prse1
prsec0 ~~ 0*av0prse0 
p_ccpt ~ 0*ECRavo_1

p_ccpt ~ 0*ECRavo_0

p_ccpr ~ 0*ECRavo_1

p_ccpr ~ m*ECRavo_0 
p_ccpt ~ m*av1prse0

p_ccpt ~ 0*av0prse0

p_ccpr ~ 0*av1prse1

p_ccpr ~ 0*av1prse0

p_ccpr ~ k*av0prse1

p_ccpr ~ 0*av0prse0 
"
ecravo_mod_sec_mod_m <- sem(model = ecravo_mod_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m)

# ECR anx gating S1 posint avo ---------------------------------------------

ecranx_allfree_avo_mod <- paste(posint_avo_mod, ECR_moderation("anx","avo", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecranx_allfree_avo_mod_m <- sem(model = ecranx_allfree_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_avo_mod <- paste(posint_avo_mod, ECR_moderation("anx","avo", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecranx_afixed_avo_mod_m <- sem(model = ecranx_afixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_avo_mod <- paste(posint_avo_mod, ECR_moderation("anx","avo", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecranx_pfixed_avo_mod_m <- sem(model = ecranx_pfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_avo_mod <- paste(posint_avo_mod, ECR_moderation("anx","avo", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecranx_apfixed_avo_mod_m <- sem(model = ecranx_apfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_avo_mod <- paste(posint_avo_mod, ECR_moderation("anx","avo", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecranx_aonly_avo_mod_m <- sem(model = ecranx_aonly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_avo_mod <- paste(posint_avo_mod, ECR_moderation("anx","avo", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecranx_ponly_avo_mod_m <- sem(model = ecranx_ponly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m)
ecranx_mod_avo_mod_m <- ecranx_allfree_avo_mod_m
ecranx_mod_avo_mod <- ecranx_allfree_avo_mod

# ECR anx gating S1 posint anx -----------------------------------------

ecranx_allfree_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_allfree_anx_mod_m <- sem(model = ecranx_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_afixed_anx_mod_m <- sem(model = ecranx_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_pfixed_anx_mod_m <- sem(model = ecranx_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_apfixed_anx_mod_m <- sem(model = ecranx_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_aonly_anx_mod_m <- sem(model = ecranx_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_ponly_anx_mod_m <- sem(model = ecranx_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_mod_anx_mod <- "

p_ccpt ~ b*panx1 #a
p_ccpt ~ a*pranx1 #a
p_ccpr ~ b*panx0 # a
p_ccpr ~ a*pranx0 #a


pranx1 ~~ pranx0
panx1~~panx0
p_ccpt ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

 ECRanx_1 ~~ 0*an1pran1 
ECRanx_0 ~~ 0*an1pran1 
ECRanx_1 ~~ 0*an1pran0 
ECRanx_0 ~~ 0*an1pran0 
ECRanx_1 ~~ 0*an0pran1 
ECRanx_0 ~~ 0*an0pran1 
ECRanx_1 ~~ 0*an0pran0 
ECRanx_0 ~~ 0*an0pran0
an1pran1 ~~ an1pran1 
an1pran0 ~~ an1pran0 
an0pran1 ~~ an0pran1 
an0pran0 ~~ an0pran0
pranx1 ~~ 0*an1pran1
pranx1 ~~ 0*an1pran0
pranx1 ~~ 0*an0pran1
pranx1 ~~ 0*an0pran0
pranx0 ~~ 0*an1pran1
pranx0 ~~ 0*an1pran0
pranx0 ~~ 0*an0pran1
pranx0 ~~ 0*an0pran0 
p_ccpt ~ i*ECRanx_1

p_ccpt ~ 0*ECRanx_0

p_ccpr ~ l*ECRanx_1

p_ccpr ~ i*ECRanx_0 
p_ccpt ~ l*an1pran1

p_ccpt ~ l*an0pran1

p_ccpr ~ l*an1pran0

p_ccpr ~ an0pran0 
pranx1 ~~ ECRanx_1
"
ecranx_mod_anx_mod_m <- sem(model = ecranx_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
# mrarginal effect where baseline anxiety on coupling in partner during positive interaction is weakened or made dependent by broader attachment anxiety.  
anova(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)

# ECR anx gating S1 posint sec --------------------------------------------


ecranx_allfree_sec_mod <- paste(posint_sec_mod, ECR_moderation("anx","sec", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecranx_allfree_sec_mod_m <- sem(model = ecranx_allfree_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_sec_mod <- paste(posint_sec_mod, ECR_moderation("anx","sec", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecranx_afixed_sec_mod_m <- sem(model = ecranx_afixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_sec_mod <- paste(posint_sec_mod, ECR_moderation("anx","sec", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecranx_pfixed_sec_mod_m <- sem(model = ecranx_pfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_sec_mod <- paste(posint_sec_mod, ECR_moderation("anx","sec", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecranx_apfixed_sec_mod_m <- sem(model = ecranx_apfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_sec_mod <- paste(posint_sec_mod, ECR_moderation("anx","sec", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecranx_aonly_sec_mod_m <- sem(model = ecranx_aonly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_sec_mod <- paste(posint_sec_mod, ECR_moderation("anx","sec", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecranx_ponly_sec_mod_m <- sem(model = ecranx_ponly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m)
ecranx_mod_sec_mod <- "
p_ccpt ~ 0*psec1 #a
p_ccpt ~ 0*prsec1 #a
p_ccpt ~ 0*psec0 #p
p_ccpt ~ prsec0 #p
p_ccpr ~ 0*psec1 #p
p_ccpr ~ prsec1 #p
p_ccpr ~ 0*psec0 # a
p_ccpr ~ prsec0 #a

prsec1 ~~ prsec0
psec1~~psec0
p_ccpt ~~ p_ccpr
psec1 ~ prsec1
psec0 ~prsec0

 ECRanx_1 ~~ 0*an1prse1 
ECRanx_0 ~~ 0*an1prse1 
ECRanx_1 ~~ 0*an1prse0 
ECRanx_0 ~~ 0*an1prse0 
ECRanx_1 ~~ 0*an0prse1 
ECRanx_0 ~~ 0*an0prse1 
ECRanx_1 ~~ 0*an0prse0 
ECRanx_0 ~~ 0*an0prse0
an1prse1 ~~ an1prse1 
an1prse0 ~~ an1prse0 
an0prse1 ~~ an0prse1 
an0prse0 ~~ an0prse0
prsec1 ~~ 0*an1prse1
prsec1 ~~ 0*an1prse0
prsec1 ~~ 0*an0prse1
prsec1 ~~ 0*an0prse0
prsec0 ~~ 0*an1prse1
prsec0 ~~ 0*an1prse0
prsec0 ~~ 0*an0prse1
prsec0 ~~ 0*an0prse0 
p_ccpt ~ l*ECRanx_1

p_ccpt ~ 0*ECRanx_0

p_ccpr ~ ECRanx_1

p_ccpr ~ 0*ECRanx_0 
p_ccpt ~ l*an1prse0

p_ccpt ~ v*an0prse0

p_ccpr ~ v*an1prse1

p_ccpr ~ v*an1prse0

p_ccpr ~ 0*an0prse1

p_ccpr ~ 0*an0prse0 
"
ecranx_mod_sec_mod_m <- sem(model = ecranx_mod_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)


# S1 ECR Posint Save Models  ----------------------------------------------
avav_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m)
avav_syn_model_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod)
df <- mapply(pull_fitmeasures, avav_model_list, avav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_p_ecr_avo_saam_avo"
fulldf <- df

avan_model_list <- c(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)
avan_syn_model_list <- c(ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod, ecravo_mod_anx_mod)
df <- mapply(pull_fitmeasures, avan_model_list, avan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_p_ecr_avo_saam_anx"
fulldf <- bind_rows(fulldf, df)

avse_model_list <- c(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m)
avse_syn_model_list <- c(ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod, ecravo_mod_sec_mod)
df <- mapply(pull_fitmeasures, avse_model_list, avse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_p_ecr_avo_saam_sec"
fulldf <- bind_rows(fulldf, df)


anav_model_list <- c(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m, ecranx_mod_avo_mod_m)
anav_syn_model_list <- c(ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod, ecranx_mod_avo_mod)
df <- mapply(pull_fitmeasures, anav_model_list, anav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_p_ecr_anx_saam_avo"
fulldf <- bind_rows(fulldf, df)

anan_model_list <- c(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)
anan_syn_model_list <- c(ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod, ecranx_mod_anx_mod)
df <- mapply(pull_fitmeasures, anan_model_list, anan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_p_ecr_anx_saam_anx"
fulldf <- bind_rows(fulldf, df)

anse_model_list <- c(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
anse_syn_model_list <- c(ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
df <- mapply(pull_fitmeasures, anse_model_list, anse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_p_ecr_anx_saam_sec"
fulldf <- bind_rows(fulldf, df)


s1_p_ecr_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m,
                         ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m,ecravo_mod_anx_mod_m,
                         ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m,ecravo_mod_sec_mod_m, 
                         ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m,ecranx_mod_avo_mod_m,
                         ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m,  ecranx_mod_anx_mod_m,
                         ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
s1_p_ecr_syn_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod,
                       ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod,ecravo_mod_anx_mod,
                       ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod,ecravo_mod_sec_mod,
                       ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod,ecranx_mod_avo_mod,
                       ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod,  ecranx_mod_anx_mod,
                       ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
s1_p_ecr_df <- posnegint_formoderation
names(s1_p_ecr_model_list) <- c("ecravo_allfree_avo_mod_m", "ecravo_afixed_avo_mod_m", "ecravo_pfixed_avo_mod_m","ecravo_apfixed_avo_mod_m","ecravo_aonly_avo_mod_m","ecravo_ponly_avo_mod_m", "ecravo_mod_avo_mod_m",
                                "ecravo_allfree_anx_mod_m", "ecravo_afixed_anx_mod_m", "ecravo_pfixed_anx_mod_m","ecravo_apfixed_anx_mod_m","ecravo_aonly_anx_mod_m","ecravo_ponly_anx_mod_m","ecravo_mod_anx_mod_m",
                                "ecravo_allfree_sec_mod_m", "ecravo_afixed_sec_mod_m", "ecravo_pfixed_sec_mod_m","ecravo_apfixed_sec_mod_m","ecravo_aonly_sec_mod_m","ecravo_ponly_sec_mod_m","ecravo_mod_sec_mod_m",
                                "ecranx_allfree_avo_mod_m", "ecranx_afixed_avo_mod_m", "ecranx_pfixed_avo_mod_m","ecranx_apfixed_avo_mod_m","ecranx_aonly_avo_mod_m","ecranx_ponly_avo_mod_m","ecranx_mod_avo_mod_m",
                                "ecranx_allfree_anx_mod_m", "ecranx_afixed_anx_mod_m", "ecranx_pfixed_anx_mod_m","ecranx_apfixed_anx_mod_m","ecranx_aonly_anx_mod_m","ecranx_ponly_anx_mod_m",  "ecranx_mod_anx_mod_m",
                                "ecranx_allfree_sec_mod_m", "ecranx_afixed_sec_mod_m", "ecranx_pfixed_sec_mod_m","ecranx_apfixed_sec_mod_m","ecranx_aonly_sec_mod_m","ecranx_ponly_sec_mod_m", "ecranx_mod_sec_mod_m")
names(s1_p_ecr_syn_list) <- c("ecravo_allfree_avo_mod", "ecravo_afixed_avo_mod", "ecravo_pfixed_avo_mod","ecravo_apfixed_avo_mod","ecravo_aonly_avo_mod","ecravo_ponly_avo_mod", "ecravo_mod_avo_mod",
                              "ecravo_allfree_anx_mod", "ecravo_afixed_anx_mod", "ecravo_pfixed_anx_mod","ecravo_apfixed_anx_mod","ecravo_aonly_anx_mod","ecravo_ponly_anx_mod","ecravo_mod_anx_mod",
                              "ecravo_allfree_sec_mod", "ecravo_afixed_sec_mod", "ecravo_pfixed_sec_mod","ecravo_apfixed_sec_mod","ecravo_aonly_sec_mod","ecravo_ponly_sec_mod","ecravo_mod_sec_mod",
                              "ecranx_allfree_avo_mod", "ecranx_afixed_avo_mod", "ecranx_pfixed_avo_mod","ecranx_apfixed_avo_mod","ecranx_aonly_avo_mod","ecranx_ponly_avo_mod","ecranx_mod_avo_mod",
                              "ecranx_allfree_anx_mod", "ecranx_afixed_anx_mod", "ecranx_pfixed_anx_mod","ecranx_apfixed_anx_mod","ecranx_aonly_anx_mod","ecranx_ponly_anx_mod",  "ecranx_mod_anx_mod",
                              "ecranx_allfree_sec_mod", "ecranx_afixed_sec_mod", "ecranx_pfixed_sec_mod","ecranx_apfixed_sec_mod","ecranx_aonly_sec_mod","ecranx_ponly_sec_mod", "ecranx_mod_sec_mod")

# Initialize Models -------------------------------------------------------

ecravo_allfree_avo_mod_m <-  ecravo_afixed_avo_mod_m <-  ecravo_pfixed_avo_mod_m <- ecravo_apfixed_avo_mod_m <- ecravo_aonly_avo_mod_m <- ecravo_ponly_avo_mod_m <-  ecravo_mod_avo_mod_m <- ""
ecravo_allfree_avo_mod <- ecravo_afixed_avo_mod <- ecravo_pfixed_avo_mod <- ecravo_apfixed_avo_mod <- ecravo_aonly_avo_mod <- ecravo_ponly_avo_mod <-  ecravo_mod_avo_mod <- ""

ecravo_allfree_anx_mod_m <- ecravo_afixed_anx_mod_m  <- ecravo_pfixed_anx_mod_m <- ecravo_apfixed_anx_mod_m <- ecravo_aonly_anx_mod_m <- ecravo_ponly_anx_mod_m <-  ecravo_mod_anx_mod_m <- ""
ecravo_allfree_anx_mod <- ecravo_afixed_anx_mod <- ecravo_pfixed_anx_mod <- ecravo_apfixed_anx_mod <- ecravo_aonly_anx_mod<- ecravo_ponly_anx_mod <- ecravo_mod_anx_mod <- ""

ecravo_allfree_sec_mod_m <- ecravo_afixed_sec_mod_m <- ecravo_pfixed_sec_mod_m <- ecravo_apfixed_sec_mod_m <- ecravo_aonly_sec_mod_m <- ecravo_ponly_sec_mod_m <- ecravo_mod_sec_mod_m <- ""
ecravo_allfree_sec_mod <- ecravo_afixed_sec_mod <- ecravo_pfixed_sec_mod <- ecravo_apfixed_sec_mod <- ecravo_aonly_sec_mod <- ecravo_ponly_sec_mod <-  ecravo_mod_sec_mod <- ""

ecranx_allfree_avo_mod_m <- ecranx_afixed_avo_mod_m <- ecranx_pfixed_avo_mod_m <- ecranx_apfixed_avo_mod_m <- ecranx_aonly_avo_mod_m <- ecranx_ponly_avo_mod_m <- ecranx_mod_avo_mod_m <- ""
ecranx_allfree_avo_mod <- ecranx_afixed_avo_mod <- ecranx_pfixed_avo_mod <- ecranx_apfixed_avo_mod <- ecranx_aonly_avo_mod <- ecranx_ponly_avo_mod <-  ecranx_mod_avo_mod <- ""

ecranx_allfree_anx_mod_m <- ecranx_afixed_anx_mod_m <-  ecranx_pfixed_anx_mod_m <- ecranx_apfixed_anx_mod_m <- ecranx_aonly_anx_mod_m <- ecranx_ponly_anx_mod_m <- ecranx_mod_anx_mod_m <- ""
ecranx_allfree_anx_mod <- ecranx_afixed_anx_mod <- ecranx_pfixed_anx_mod <- ecranx_apfixed_anx_mod <- ecranx_aonly_anx_mod <- ecranx_ponly_anx_mod <- ecranx_mod_anx_mod <- ""

ecranx_allfree_sec_mod_m <- ecranx_afixed_sec_mod_m <- ecranx_pfixed_sec_mod_m <- ecranx_apfixed_sec_mod_m <- ecranx_aonly_sec_mod_m <- ecranx_ponly_sec_mod_m <- ecranx_mod_sec_mod_m <- ""
ecranx_allfree_sec_mod <- ecranx_afixed_sec_mod <- ecranx_pfixed_sec_mod <- ecranx_apfixed_sec_mod <- ecranx_aonly_sec_mod <- ecranx_ponly_sec_mod <- ecranx_mod_sec_mod <- ""



# ECR avo gating S2 posint anx -----------------------------------------



ecravo_allfree_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_allfree_anx_mod_m <- sem(model = ecravo_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_afixed_anx_mod_m <- sem(model = ecravo_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_pfixed_anx_mod_m <- sem(model = ecravo_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_apfixed_anx_mod_m <- sem(model = ecravo_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_aonly_anx_mod_m <- sem(model = ecravo_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_ponly_anx_mod_m <- sem(model = ecravo_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
ecravo_mod_anx_mod <- "
p_ccpt ~ b*panx1 #a
p_ccpt ~ a*pranx1 #a
p_ccpr ~ b*panx0 # a
p_ccpr ~ a*pranx0 #a


pranx1 ~~ pranx0
panx1~~panx0
p_ccpt ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

 ECRavo_1 ~~ 0*av1pan1 
ECRavo_0 ~~ 0*av1pan1 
ECRavo_1 ~~ 0*av1pan0 
ECRavo_0 ~~ 0*av1pan0 
ECRavo_1 ~~ 0*av0pan1 
ECRavo_0 ~~ 0*av0pan1 
ECRavo_1 ~~ 0*av0pan0 
ECRavo_0 ~~ 0*av0pan0
av1pan1 ~~ av1pan1 
av1pan0 ~~ av1pan0 
av0pan1 ~~ av0pan1 
av0pan0 ~~ av0pan0
panx1 ~~ 0*av1pan1
panx1 ~~ 0*av1pan0
panx1 ~~ 0*av0pan1
panx1 ~~ 0*av0pan0
panx0 ~~ 0*av1pan1
panx0 ~~ 0*av1pan0
panx0 ~~ 0*av0pan1
panx0 ~~ 0*av0pan0 
p_ccpt ~ l*ECRavo_1

p_ccpt ~ l*ECRavo_0

p_ccpr ~ f*ECRavo_1

p_ccpr ~ f*ECRavo_0 
p_ccpt ~ av1pan1

p_ccpt ~ av0pan1

p_ccpr ~ av1pan0

p_ccpr ~ av0pan0 
av0pan1 ~~ av0pan0
"
ecravo_mod_anx_mod_m <- sem(model = ecravo_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)




# ECR anx gating S2 posint anx -----------------------------------------

ecranx_allfree_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod), "\n") #,"\nan0pan1 ~~ an0pan0","\nan1pan1 ~~ an1pan0","\nan1pan1 ~~ an0pan0", "\nECRanx_0 ~~ panx0", "\n")
ecranx_allfree_anx_mod_m <- sem(model = ecranx_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_afixed_anx_mod_m <- sem(model = ecranx_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_pfixed_anx_mod_m <- sem(model = ecranx_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_apfixed_anx_mod_m <- sem(model = ecranx_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_aonly_anx_mod_m <- sem(model = ecranx_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_ponly_anx_mod_m <- sem(model = ecranx_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
ecranx_mod_anx_mod <-  "
p_ccpt ~ b*panx1 #a
p_ccpt ~ a*pranx1 #a
p_ccpr ~ b*panx0 # a
p_ccpr ~ a*pranx0 #a


pranx1 ~~ pranx0
panx1~~panx0
p_ccpt ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

 ECRanx_1 ~~ 0*an1pan1 
ECRanx_0 ~~ 0*an1pan1 
ECRanx_1 ~~ 0*an1pan0 
ECRanx_0 ~~ 0*an1pan0 
ECRanx_1 ~~ 0*an0pan1 
ECRanx_0 ~~ 0*an0pan1 
ECRanx_1 ~~ 0*an0pan0 
ECRanx_0 ~~ 0*an0pan0
an1pan1 ~~ an1pan1 
an1pan0 ~~ an1pan0 
an0pan1 ~~ an0pan1 
an0pan0 ~~ an0pan0
panx1 ~~ 0*an1pan1
panx1 ~~ 0*an1pan0
panx1 ~~ 0*an0pan1
panx1 ~~ 0*an0pan0
panx0 ~~ 0*an1pan1
panx0 ~~ 0*an1pan0
panx0 ~~ 0*an0pan1
panx0 ~~ 0*an0pan0 
p_ccpt ~ ECRanx_1

p_ccpt ~ 0*ECRanx_0

p_ccpr ~ 0*ECRanx_1

p_ccpr ~ ECRanx_0 
p_ccpt ~ 0*an1pan1

p_ccpt ~ 0*an0pan1

p_ccpr ~ an1pan0

p_ccpr ~ 0*an0pan0 

"
ecranx_mod_anx_mod_m <- sem(model = ecranx_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)


# S2 ECR Posint Save Models  ----------------------------------------------
avav_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m)
avav_syn_model_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod)
df <- mapply(pull_fitmeasures, avav_model_list, avav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_p_ecr_avo_saam_avo"
fulldf <- df

avan_model_list <- c(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)
avan_syn_model_list <- c(ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod, ecravo_mod_anx_mod)
df <- mapply(pull_fitmeasures, avan_model_list, avan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_p_ecr_avo_saam_anx"
fulldf <- bind_rows(fulldf, df)

avse_model_list <- c(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m)
avse_syn_model_list <- c(ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod, ecravo_mod_sec_mod)
df <- mapply(pull_fitmeasures, avse_model_list, avse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_p_ecr_avo_saam_sec"
fulldf <- bind_rows(fulldf, df)


anav_model_list <- c(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m, ecranx_mod_avo_mod_m)
anav_syn_model_list <- c(ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod, ecranx_mod_avo_mod)
df <- mapply(pull_fitmeasures, anav_model_list, anav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_p_ecr_anx_saam_avo"
fulldf <- bind_rows(fulldf, df)

anan_model_list <- c(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)
anan_syn_model_list <- c(ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod, ecranx_mod_anx_mod)
df <- mapply(pull_fitmeasures, anan_model_list, anan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_p_ecr_anx_saam_anx"
fulldf <- bind_rows(fulldf, df)

anse_model_list <- c(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
anse_syn_model_list <- c(ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
df <- mapply(pull_fitmeasures, anse_model_list, anse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_p_ecr_anx_saam_sec"
fulldf <- bind_rows(fulldf, df)


s2_p_ecr_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m,
                         ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m,ecravo_mod_anx_mod_m,
                         ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m,ecravo_mod_sec_mod_m, 
                         ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m,ecranx_mod_avo_mod_m,
                         ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m,  ecranx_mod_anx_mod_m,
                         ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
s2_p_ecr_syn_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod,
                       ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod,ecravo_mod_anx_mod,
                       ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod,ecravo_mod_sec_mod,
                       ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod,ecranx_mod_avo_mod,
                       ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod,  ecranx_mod_anx_mod,
                       ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
s2_p_ecr_df <- posnegint_formoderation
names(s2_p_ecr_model_list) <- c("ecravo_allfree_avo_mod_m", "ecravo_afixed_avo_mod_m", "ecravo_pfixed_avo_mod_m","ecravo_apfixed_avo_mod_m","ecravo_aonly_avo_mod_m","ecravo_ponly_avo_mod_m", "ecravo_mod_avo_mod_m",
                                "ecravo_allfree_anx_mod_m", "ecravo_afixed_anx_mod_m", "ecravo_pfixed_anx_mod_m","ecravo_apfixed_anx_mod_m","ecravo_aonly_anx_mod_m","ecravo_ponly_anx_mod_m","ecravo_mod_anx_mod_m",
                                "ecravo_allfree_sec_mod_m", "ecravo_afixed_sec_mod_m", "ecravo_pfixed_sec_mod_m","ecravo_apfixed_sec_mod_m","ecravo_aonly_sec_mod_m","ecravo_ponly_sec_mod_m","ecravo_mod_sec_mod_m",
                                "ecranx_allfree_avo_mod_m", "ecranx_afixed_avo_mod_m", "ecranx_pfixed_avo_mod_m","ecranx_apfixed_avo_mod_m","ecranx_aonly_avo_mod_m","ecranx_ponly_avo_mod_m","ecranx_mod_avo_mod_m",
                                "ecranx_allfree_anx_mod_m", "ecranx_afixed_anx_mod_m", "ecranx_pfixed_anx_mod_m","ecranx_apfixed_anx_mod_m","ecranx_aonly_anx_mod_m","ecranx_ponly_anx_mod_m",  "ecranx_mod_anx_mod_m",
                                "ecranx_allfree_sec_mod_m", "ecranx_afixed_sec_mod_m", "ecranx_pfixed_sec_mod_m","ecranx_apfixed_sec_mod_m","ecranx_aonly_sec_mod_m","ecranx_ponly_sec_mod_m", "ecranx_mod_sec_mod_m")
names(s2_p_ecr_syn_list) <- c("ecravo_allfree_avo_mod", "ecravo_afixed_avo_mod", "ecravo_pfixed_avo_mod","ecravo_apfixed_avo_mod","ecravo_aonly_avo_mod","ecravo_ponly_avo_mod", "ecravo_mod_avo_mod",
                              "ecravo_allfree_anx_mod", "ecravo_afixed_anx_mod", "ecravo_pfixed_anx_mod","ecravo_apfixed_anx_mod","ecravo_aonly_anx_mod","ecravo_ponly_anx_mod","ecravo_mod_anx_mod",
                              "ecravo_allfree_sec_mod", "ecravo_afixed_sec_mod", "ecravo_pfixed_sec_mod","ecravo_apfixed_sec_mod","ecravo_aonly_sec_mod","ecravo_ponly_sec_mod","ecravo_mod_sec_mod",
                              "ecranx_allfree_avo_mod", "ecranx_afixed_avo_mod", "ecranx_pfixed_avo_mod","ecranx_apfixed_avo_mod","ecranx_aonly_avo_mod","ecranx_ponly_avo_mod","ecranx_mod_avo_mod",
                              "ecranx_allfree_anx_mod", "ecranx_afixed_anx_mod", "ecranx_pfixed_anx_mod","ecranx_apfixed_anx_mod","ecranx_aonly_anx_mod","ecranx_ponly_anx_mod",  "ecranx_mod_anx_mod",
                              "ecranx_allfree_sec_mod", "ecranx_afixed_sec_mod", "ecranx_pfixed_sec_mod","ecranx_apfixed_sec_mod","ecranx_aonly_sec_mod","ecranx_ponly_sec_mod", "ecranx_mod_sec_mod")

# Load Data for Excessive Dependency ---------------------------------------------------------------
posnegint_personality <- read.csv("../data/posnegint_aapr.csv") # n.b. rather than have the columns be labeled according to the aar sublabels, just named with ECR sublabels. This meant that I could copy and paste the code from above, rather than having to re-implement the code
# anx refers to excessive dependency
# avo refers to defensive separation



# Wrangle data for moderation analyses ------------------------------------
center <- function(x) {
  x = x-mean(x, na.rm = TRUE)
  return(x)
}
posnegint_formoderation <- dplyr::select(posnegint_personality, PTNUM, starts_with("p_"), scpt, ccpt, scpr, ccpr, starts_with("ECR"), starts_with("pra"), starts_with("pa"), starts_with("prs"), starts_with("ps")) %>% mutate_at(vars(c(ccpt, ccpr, starts_with("ECR"), starts_with("pra"), starts_with("pa"), starts_with("prs"), starts_with("ps"))), funs(center)) %>% mutate(an0pran0 = ECRanx_0*pranx0, an0pan0 = ECRanx_0*panx0, 
                                                                                                                                                                                                                                                                                                                                                                                  an1pran1 = ECRanx_1*pranx1, an1pan1 = ECRanx_1*panx1,
                                                                                                                                                                                                                                                                                                                                                                                  an0prav0 = ECRanx_0*pravo0, an0pav0 = ECRanx_0*pavo0, 
                                                                                                                                                                                                                                                                                                                                                                                  an1prav1 = ECRanx_1*pravo1, an1pav1 = ECRanx_1*pavo1,
                                                                                                                                                                                                                                                                                                                                                                                  an0prse0 = ECRanx_0*prsec0, an0pan0 = ECRanx_0*psec0, 
                                                                                                                                                                                                                                                                                                                                                                                  an1prse1 = ECRanx_1*prsec1, an1pan1 = ECRanx_1*psec1,
                                                                                                                                                                                                                                                                                                                                                                                  av0pran0 = ECRavo_0*pranx0, av0pan0 = ECRavo_0*panx0, 
                                                                                                                                                                                                                                                                                                                                                                                  av1pran1 = ECRavo_1*pranx1, av1pan1 = ECRavo_1*panx1,
                                                                                                                                                                                                                                                                                                                                                                                  av0prav0 = ECRavo_0*pravo0, av0pav0 = ECRavo_0*pavo0, 
                                                                                                                                                                                                                                                                                                                                                                                  av1prav1 = ECRavo_1*pravo1, av1pav1 = ECRavo_1*pavo1,
                                                                                                                                                                                                                                                                                                                                                                                  av0prse0 = ECRavo_0*prsec0, av0pan0 = ECRavo_0*psec0, 
                                                                                                                                                                                                                                                                                                                                                                                  av1prse1 = ECRavo_1*prsec1, av1pan1 = ECRavo_1*psec1,
                                                                                                                                                                                                                                                                                                                                                                                  an0ccpr0 = ECRanx_0*ccpr, an1ccpt1 = ECRanx_1*ccpt,
                                                                                                                                                                                                                                                                                                                                                                                  av0ccpr0 = ECRavo_0*ccpr, av1ccpt1 = ECRanx_1*ccpt,
                                                                                                                                                                                                                                                                                                                                                                                  an1ccpr0 = ECRanx_1*ccpr, an0ccpt1 = ECRanx_0*ccpt,
                                                                                                                                                                                                                                                                                                                                                                                  av1ccpr0 = ECRavo_1*ccpr, av0ccpt1 = ECRanx_0*ccpt,
                                                                                                                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                                                                                                                  #partner
                                                                                                                                                                                                                                                                                                                                                                                  an1pran0 = ECRanx_1*pranx0, an1pan0 = ECRanx_1*panx0, 
                                                                                                                                                                                                                                                                                                                                                                                  an0pran1 = ECRanx_0*pranx1, an0pan1 = ECRanx_0*panx1,
                                                                                                                                                                                                                                                                                                                                                                                  an1prav0 = ECRanx_1*pravo0, an1pav0 = ECRanx_1*pavo0, 
                                                                                                                                                                                                                                                                                                                                                                                  an0prav1 = ECRanx_0*pravo1, an0pav1 = ECRanx_0*pavo1,
                                                                                                                                                                                                                                                                                                                                                                                  an1prse0 = ECRanx_1*prsec0, an1pan0 = ECRanx_1*psec0, 
                                                                                                                                                                                                                                                                                                                                                                                  an0prse1 = ECRanx_0*prsec1, an0pan1 = ECRanx_0*psec1,
                                                                                                                                                                                                                                                                                                                                                                                  av1pran0 = ECRavo_1*pranx0, av1pan0 = ECRavo_1*panx0, 
                                                                                                                                                                                                                                                                                                                                                                                  av0pran1 = ECRavo_0*pranx1, av0pan1 = ECRavo_0*panx1,
                                                                                                                                                                                                                                                                                                                                                                                  av1prav0 = ECRavo_1*pravo0, av1pav0 = ECRavo_1*pavo0, 
                                                                                                                                                                                                                                                                                                                                                                                  av0prav1 = ECRavo_0*pravo1, av0pav1 = ECRavo_0*pavo1,
                                                                                                                                                                                                                                                                                                                                                                                  av1prse0 = ECRavo_1*prsec0, av1pan0 = ECRavo_1*psec0, 
                                                                                                                                                                                                                                                                                                                                                                                  av0prse1 = ECRavo_0*prsec1, av0pan1 = ECRavo_0*psec1)


#rescaling for all the models
posnegint_formoderation <- dplyr::mutate_at(posnegint_formoderation, vars(starts_with("p_")), list(~100*.)) %>% dplyr::mutate_at(vars(starts_with("ECR")), list(~10*.))


# Initialize Models -------------------------------------------------------

ecravo_allfree_avo_mod_m <-  ecravo_afixed_avo_mod_m <-  ecravo_pfixed_avo_mod_m <- ecravo_apfixed_avo_mod_m <- ecravo_aonly_avo_mod_m <- ecravo_ponly_avo_mod_m <-  ecravo_mod_avo_mod_m <- ""
ecravo_allfree_avo_mod <- ecravo_afixed_avo_mod <- ecravo_pfixed_avo_mod <- ecravo_apfixed_avo_mod <- ecravo_aonly_avo_mod <- ecravo_ponly_avo_mod <-  ecravo_mod_avo_mod <- ""

ecravo_allfree_anx_mod_m <- ecravo_afixed_anx_mod_m  <- ecravo_pfixed_anx_mod_m <- ecravo_apfixed_anx_mod_m <- ecravo_aonly_anx_mod_m <- ecravo_ponly_anx_mod_m <-  ecravo_mod_anx_mod_m <- ""
ecravo_allfree_anx_mod <- ecravo_afixed_anx_mod <- ecravo_pfixed_anx_mod <- ecravo_apfixed_anx_mod <- ecravo_aonly_anx_mod<- ecravo_ponly_anx_mod <- ecravo_mod_anx_mod <- ""

ecravo_allfree_sec_mod_m <- ecravo_afixed_sec_mod_m <- ecravo_pfixed_sec_mod_m <- ecravo_apfixed_sec_mod_m <- ecravo_aonly_sec_mod_m <- ecravo_ponly_sec_mod_m <- ecravo_mod_sec_mod_m <- ""
ecravo_allfree_sec_mod <- ecravo_afixed_sec_mod <- ecravo_pfixed_sec_mod <- ecravo_apfixed_sec_mod <- ecravo_aonly_sec_mod <- ecravo_ponly_sec_mod <-  ecravo_mod_sec_mod <- ""

ecranx_allfree_avo_mod_m <- ecranx_afixed_avo_mod_m <- ecranx_pfixed_avo_mod_m <- ecranx_apfixed_avo_mod_m <- ecranx_aonly_avo_mod_m <- ecranx_ponly_avo_mod_m <- ecranx_mod_avo_mod_m <- ""
ecranx_allfree_avo_mod <- ecranx_afixed_avo_mod <- ecranx_pfixed_avo_mod <- ecranx_apfixed_avo_mod <- ecranx_aonly_avo_mod <- ecranx_ponly_avo_mod <-  ecranx_mod_avo_mod <- ""

ecranx_allfree_anx_mod_m <- ecranx_afixed_anx_mod_m <-  ecranx_pfixed_anx_mod_m <- ecranx_apfixed_anx_mod_m <- ecranx_aonly_anx_mod_m <- ecranx_ponly_anx_mod_m <- ecranx_mod_anx_mod_m <- ""
ecranx_allfree_anx_mod <- ecranx_afixed_anx_mod <- ecranx_pfixed_anx_mod <- ecranx_apfixed_anx_mod <- ecranx_aonly_anx_mod <- ecranx_ponly_anx_mod <- ecranx_mod_anx_mod <- ""

ecranx_allfree_sec_mod_m <- ecranx_afixed_sec_mod_m <- ecranx_pfixed_sec_mod_m <- ecranx_apfixed_sec_mod_m <- ecranx_aonly_sec_mod_m <- ecranx_ponly_sec_mod_m <- ecranx_mod_sec_mod_m <- ""
ecranx_allfree_sec_mod <- ecranx_afixed_sec_mod <- ecranx_pfixed_sec_mod <- ecranx_apfixed_sec_mod <- ecranx_aonly_sec_mod <- ecranx_ponly_sec_mod <- ecranx_mod_sec_mod <- ""




# AAR avo gating S1 negint avo ---------------------------------------------
# COMPLETE

ecravo_allfree_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_allfree_avo_mod_m <- sem(model = ecravo_allfree_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_afixed_avo_mod_m <- sem(model = ecravo_afixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_pfixed_avo_mod_m <- sem(model = ecravo_pfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_apfixed_avo_mod_m <- sem(model = ecravo_apfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_aonly_avo_mod_m <- sem(model = ecravo_aonly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_ponly_avo_mod_m <- sem(model = ecravo_ponly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m)
# Mediation of increases in avoidance mediated by contrarianism. Avoidance in patient also predicted contrarianism in partner during pos interaction. Partner's avoidance gates the extent to which patient avoidance predicts contrarianism. (True in opposite direction with partner). Partner's contrarianism not affected by dispositional ECR.
ecravo_mod_avo_mod<- "

#paths that are not particularly strong
pavo0 ~ 0*ccpr
ccpr ~ 0*pravo0

#strong paths
pavo1 ~  a*ccpt
ccpt ~ c*pravo1 
ccpr ~ c*pravo1
pravo1 ~~ pravo0
pavo1~~pavo0
ccpt ~~ ccpr
pavo1 ~ pravo1
pavo0 ~pravo0
ac:=a*c

 ECRavo_1 ~~ 0*av1prav1 
ECRavo_0 ~~ 0*av1prav1 
ECRavo_1 ~~ 0*av0prav1 
ECRavo_0 ~~ 0*av0prav1 
av1prav1 ~~ av1prav1 
av0prav1 ~~ av0prav1 
pravo1 ~~ 0*av1prav1
pravo1 ~~ 0*av0prav1
pravo0 ~~ 0*av1prav1
pravo0 ~~ 0*av0prav1 
ccpt ~ 0*ECRavo_1

ccpt ~ 0*ECRavo_0

ccpr ~ 0*ECRavo_1

ccpr ~ 0*ECRavo_0 
ccpt ~ av1prav1

ccpt ~ 0*av0prav1

ccpr ~ av1prav1

ccpr ~ 0*av0prav1
"
ecravo_mod_avo_mod_m <- sem(model = ecravo_mod_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m) 
# AAR avoidance in patient gates the extent to which baseline levels of avoidance predict contrarianism, such that it weakens that association. Yet, for partners who are avoidant, at moments where there partner is avoidant in the moment, the partner's broader avoidance increases the reaction of contrarianism to the patient's momentary avoiance. 
# AAR avo gating S1 negint anx -----------------------------------------
ecravo_allfree_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_allfree_anx_mod_m <- sem(model = ecravo_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_afixed_anx_mod_m <- sem(model = ecravo_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_pfixed_anx_mod_m <- sem(model = ecravo_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_apfixed_anx_mod_m <- sem(model = ecravo_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_aonly_anx_mod_m <- sem(model = ecravo_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_ponly_anx_mod_m <- sem(model = ecravo_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_mod_anx_mod <- "
panx1 ~  f*ccpt
panx0 ~ ccpt
panx0 ~ f*ccpr

ccpt ~ h*pranx1 
ccpt ~ d*pranx0
ccpr ~ d*pranx1
ccpr ~ h*pranx0
pranx1 ~~ pranx0
panx1~~panx0
ccpt ~~ ccpr

panx1~pranx1
panx0 ~ pranx0
 ECRavo_1 ~~ 0*av1pran1 
ECRavo_0 ~~ 0*av1pran1 
ECRavo_1 ~~ 0*av1pran0 
ECRavo_0 ~~ 0*av1pran0 
ECRavo_1 ~~ 0*av0pran1 
ECRavo_0 ~~ 0*av0pran1 
ECRavo_1 ~~ 0*av0pran0 
ECRavo_0 ~~ 0*av0pran0
av1pran1 ~~ av1pran1 
av1pran0 ~~ av1pran0 
av0pran1 ~~ av0pran1 
av0pran0 ~~ av0pran0
pranx1 ~~ 0*av1pran1
pranx1 ~~ 0*av1pran0
pranx1 ~~ 0*av0pran1
pranx1 ~~ 0*av0pran0
pranx0 ~~ 0*av1pran1
pranx0 ~~ 0*av1pran0
pranx0 ~~ 0*av0pran1
pranx0 ~~ 0*av0pran0 
ccpt ~ 0*ECRavo_1

ccpt ~ 0*ECRavo_0

ccpr ~ 0*ECRavo_1

ccpr ~ 0*ECRavo_0 
ccpt ~ av1pran1

ccpt ~ 0*av1pran0

ccpt ~ 0*av0pran1

ccpt ~ aaa*av0pran0

ccpr ~ aaa*av1pran1

ccpr ~ aaa*av1pran0

ccpr ~ 0*av0pran1

ccpr ~ 0*av0pran0 
"

ecravo_mod_anx_mod_m <- sem(model = ecravo_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)
# Mod is prefrred. When patients or partners are high in broader avoidance, extent to which patient momentary avoidance predicts partner contrarianism is greater. When low in broader patient or partner avoidance evokes dependency. Similar effect of partner anxiety onto patient coupling by partner broader avoidance. Whereas patients typically exhibit contrarianism in response to patient momentary anxiety, this is gated by overall levels of patient avoidance. If patient more avoidant broadly, this effect is weakened (and maybe shifts to dependency).
# AAR avo gating S1 negint sec --------------------------------------------

ecravo_allfree_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_allfree_sec_mod_m <- sem(model = ecravo_allfree_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_afixed_sec_mod_m <- sem(model = ecravo_afixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_pfixed_sec_mod_m <- sem(model = ecravo_pfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_apfixed_sec_mod_m <- sem(model = ecravo_apfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_aonly_sec_mod_m <- sem(model = ecravo_aonly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_ponly_sec_mod_m <- sem(model = ecravo_ponly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m)
ecravo_mod_sec_mod_m <- ecravo_allfree_sec_mod_m
ecravo_mod_sec_mod<- ecravo_allfree_sec_mod

# AAR anx gating S1 negint avo ---------------------------------------------

ecranx_allfree_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecranx_allfree_avo_mod_m <- sem(model = ecranx_allfree_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecranx_afixed_avo_mod_m <- sem(model = ecranx_afixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecranx_pfixed_avo_mod_m <- sem(model = ecranx_pfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecranx_apfixed_avo_mod_m <- sem(model = ecranx_apfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecranx_aonly_avo_mod_m <- sem(model = ecranx_aonly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecranx_ponly_avo_mod_m <- sem(model = ecranx_ponly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_mod_avo_mod <- "
#paths that are not particularly strong
pavo0 ~ 0*ccpr
ccpr ~ 0*pravo0

#strong paths
pavo1 ~  a*ccpt
ccpt ~ c*pravo1 
ccpr ~ c*pravo1
pravo1 ~~ pravo0
pavo1~~pavo0
ccpt ~~ ccpr
pavo1 ~ pravo1
pavo0 ~pravo0
ac:=a*c

 ECRanx_1 ~~ 0*an1prav1 
ECRanx_0 ~~ 0*an1prav1 
ECRanx_1 ~~ 0*an0prav1 
ECRanx_0 ~~ 0*an0prav1 
an1prav1 ~~ an1prav1 
an0prav1 ~~ an0prav1 
pravo1 ~~ 0*an1prav1
pravo1 ~~ 0*an0prav1
pravo0 ~~ 0*an1prav1
pravo0 ~~ 0*an0prav1 
ccpt ~ k*ECRanx_1

ccpt ~ 0*ECRanx_0

ccpr ~ j*ECRanx_1

ccpr ~ 0*ECRanx_0 
ccpt ~ k*an1prav1

ccpt ~ k*an0prav1

ccpr ~ j*an1prav1

ccpr ~ j*an0prav1 
"
ecranx_mod_avo_mod_m <- sem(model = ecranx_mod_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
# Typically patient avoidance evokes contrarianim in both patient and partner. This amplified for the patient if patient experiences broader attachment anxiety. This effect is weakened  for partner if patient is high in braoder attachment anxiety. 
anova(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m, ecranx_mod_avo_mod_m)

# AAR anx gating S1 negint anx -----------------------------------------

ecranx_allfree_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_allfree_anx_mod_m <- sem(model = ecranx_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_afixed_anx_mod_m <- sem(model = ecranx_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_pfixed_anx_mod_m <- sem(model = ecranx_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_apfixed_anx_mod_m <- sem(model = ecranx_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_aonly_anx_mod_m <- sem(model = ecranx_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_ponly_anx_mod_m <- sem(model = ecranx_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_mod_anx_mod <-"

panx1 ~  f*ccpt
panx0 ~ ccpt
panx0 ~ f*ccpr

ccpt ~ h*pranx1 
ccpt ~ d*pranx0
ccpr ~ d*pranx1
ccpr ~ h*pranx0
pranx1 ~~ pranx0
panx1~~panx0
ccpt ~~ ccpr

panx1~pranx1
panx0 ~ pranx0
 ECRanx_1 ~~ 0*an1pran1 
ECRanx_0 ~~ 0*an1pran1 
ECRanx_1 ~~ 0*an1pran0 
ECRanx_0 ~~ 0*an1pran0 
ECRanx_1 ~~ 0*an0pran1 
ECRanx_0 ~~ 0*an0pran1 
ECRanx_1 ~~ 0*an0pran0 
ECRanx_0 ~~ 0*an0pran0
an1pran1 ~~ an1pran1 
an1pran0 ~~ an1pran0 
an0pran1 ~~ an0pran1 
an0pran0 ~~ an0pran0
pranx1 ~~ 0*an1pran1
pranx1 ~~ 0*an1pran0
pranx1 ~~ 0*an0pran1
pranx1 ~~ 0*an0pran0
pranx0 ~~ 0*an1pran1
pranx0 ~~ 0*an1pran0
pranx0 ~~ 0*an0pran1
pranx0 ~~ 0*an0pran0 
ccpt ~ k*ECRanx_1

ccpt ~ 0*ECRanx_0

ccpr ~ j*ECRanx_1

ccpr ~ 0*ECRanx_0 
ccpt ~ k*an1pran1

ccpt ~ 0*an1pran0

ccpt ~ 0*an0pran1

ccpt ~ an0pran0

ccpr ~ j*an1pran1

ccpr ~ 0*an1pran0

ccpr ~ j*an0pran1

ccpr ~ 0*an0pran0 
"
ecranx_mod_anx_mod_m <- sem(model = ecranx_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
# Paient anxiety amplifies the extent to which patient anxiety predicts dependency in the partner and patient anxiety attenuates the extent to which which partner baseline anxiety predicts contrarianism. Broader patient anxiety amplifies the extent to which patient anxiety predicts contrarinsim in patients. Braoder partner anxiety amplifies the extent to which momentary partner anxiety elicits dependency in the patient. 

anova(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)


# AAR anx gating S1 negint sec --------------------------------------------


ecranx_allfree_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_allfree_sec_mod_m <- sem(model = ecranx_allfree_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_afixed_sec_mod_m <- sem(model = ecranx_afixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_pfixed_sec_mod_m <- sem(model = ecranx_pfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_apfixed_sec_mod_m <- sem(model = ecranx_apfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_aonly_sec_mod_m <- sem(model = ecranx_aonly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_ponly_sec_mod_m <- sem(model = ecranx_ponly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m, ecranx_ponly_sec_mod_m)

ecranx_mod_sec_mod_m <- ecranx_allfree_sec_mod_m 
ecranx_mod_sec_mod <- ecranx_allfree_sec_mod
# S1 AAR Negint Save Models  ----------------------------------------------
avav_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m)
avav_syn_model_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod)
df <- mapply(pull_fitmeasures, avav_model_list, avav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_n_aar_avo_saam_avo"
fulldf <- df

avan_model_list <- c(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)
avan_syn_model_list <- c(ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod, ecravo_mod_anx_mod)
df <- mapply(pull_fitmeasures, avan_model_list, avan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_n_aar_avo_saam_anx"
fulldf <- bind_rows(fulldf, df)

avse_model_list <- c(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m)
avse_syn_model_list <- c(ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod, ecravo_mod_sec_mod)
df <- mapply(pull_fitmeasures, avse_model_list, avse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_n_aar_avo_saam_sec"
fulldf <- bind_rows(fulldf, df)


anav_model_list <- c(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m, ecranx_mod_avo_mod_m)
anav_syn_model_list <- c(ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod, ecranx_mod_avo_mod)
df <- mapply(pull_fitmeasures, anav_model_list, anav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_n_aar_anx_saam_avo"
fulldf <- bind_rows(fulldf, df)

anan_model_list <- c(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)
anan_syn_model_list <- c(ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod, ecranx_mod_anx_mod)
df <- mapply(pull_fitmeasures, anan_model_list, anan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_n_aar_anx_saam_anx"
fulldf <- bind_rows(fulldf, df)

anse_model_list <- c(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
anse_syn_model_list <- c(ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
df <- mapply(pull_fitmeasures, anse_model_list, anse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_n_aar_anx_saam_sec"
fulldf <- bind_rows(fulldf, df)


s1_n_aar_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m,
                         ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m,ecravo_mod_anx_mod_m,
                         ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m,ecravo_mod_sec_mod_m, 
                         ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m,ecranx_mod_avo_mod_m,
                         ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m,  ecranx_mod_anx_mod_m,
                         ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
s1_n_aar_syn_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod,
                       ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod,ecravo_mod_anx_mod,
                       ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod,ecravo_mod_sec_mod,
                       ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod,ecranx_mod_avo_mod,
                       ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod,  ecranx_mod_anx_mod,
                       ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
s1_n_aar_df <- posnegint_formoderation
names(s1_n_aar_model_list) <- c("ecravo_allfree_avo_mod_m", "ecravo_afixed_avo_mod_m", "ecravo_pfixed_avo_mod_m","ecravo_apfixed_avo_mod_m","ecravo_aonly_avo_mod_m","ecravo_ponly_avo_mod_m", "ecravo_mod_avo_mod_m",
                                "ecravo_allfree_anx_mod_m", "ecravo_afixed_anx_mod_m", "ecravo_pfixed_anx_mod_m","ecravo_apfixed_anx_mod_m","ecravo_aonly_anx_mod_m","ecravo_ponly_anx_mod_m","ecravo_mod_anx_mod_m",
                                "ecravo_allfree_sec_mod_m", "ecravo_afixed_sec_mod_m", "ecravo_pfixed_sec_mod_m","ecravo_apfixed_sec_mod_m","ecravo_aonly_sec_mod_m","ecravo_ponly_sec_mod_m","ecravo_mod_sec_mod_m",
                                "ecranx_allfree_avo_mod_m", "ecranx_afixed_avo_mod_m", "ecranx_pfixed_avo_mod_m","ecranx_apfixed_avo_mod_m","ecranx_aonly_avo_mod_m","ecranx_ponly_avo_mod_m","ecranx_mod_avo_mod_m",
                                "ecranx_allfree_anx_mod_m", "ecranx_afixed_anx_mod_m", "ecranx_pfixed_anx_mod_m","ecranx_apfixed_anx_mod_m","ecranx_aonly_anx_mod_m","ecranx_ponly_anx_mod_m",  "ecranx_mod_anx_mod_m",
                                "ecranx_allfree_sec_mod_m", "ecranx_afixed_sec_mod_m", "ecranx_pfixed_sec_mod_m","ecranx_apfixed_sec_mod_m","ecranx_aonly_sec_mod_m","ecranx_ponly_sec_mod_m", "ecranx_mod_sec_mod_m")
names(s1_n_aar_syn_list) <- c("ecravo_allfree_avo_mod", "ecravo_afixed_avo_mod", "ecravo_pfixed_avo_mod","ecravo_apfixed_avo_mod","ecravo_aonly_avo_mod","ecravo_ponly_avo_mod", "ecravo_mod_avo_mod",
                              "ecravo_allfree_anx_mod", "ecravo_afixed_anx_mod", "ecravo_pfixed_anx_mod","ecravo_apfixed_anx_mod","ecravo_aonly_anx_mod","ecravo_ponly_anx_mod","ecravo_mod_anx_mod",
                              "ecravo_allfree_sec_mod", "ecravo_afixed_sec_mod", "ecravo_pfixed_sec_mod","ecravo_apfixed_sec_mod","ecravo_aonly_sec_mod","ecravo_ponly_sec_mod","ecravo_mod_sec_mod",
                              "ecranx_allfree_avo_mod", "ecranx_afixed_avo_mod", "ecranx_pfixed_avo_mod","ecranx_apfixed_avo_mod","ecranx_aonly_avo_mod","ecranx_ponly_avo_mod","ecranx_mod_avo_mod",
                              "ecranx_allfree_anx_mod", "ecranx_afixed_anx_mod", "ecranx_pfixed_anx_mod","ecranx_apfixed_anx_mod","ecranx_aonly_anx_mod","ecranx_ponly_anx_mod",  "ecranx_mod_anx_mod",
                              "ecranx_allfree_sec_mod", "ecranx_afixed_sec_mod", "ecranx_pfixed_sec_mod","ecranx_apfixed_sec_mod","ecranx_aonly_sec_mod","ecranx_ponly_sec_mod", "ecranx_mod_sec_mod")

# Initialize Models -------------------------------------------------------

ecravo_allfree_avo_mod_m <-  ecravo_afixed_avo_mod_m <-  ecravo_pfixed_avo_mod_m <- ecravo_apfixed_avo_mod_m <- ecravo_aonly_avo_mod_m <- ecravo_ponly_avo_mod_m <-  ecravo_mod_avo_mod_m <- ""
ecravo_allfree_avo_mod <- ecravo_afixed_avo_mod <- ecravo_pfixed_avo_mod <- ecravo_apfixed_avo_mod <- ecravo_aonly_avo_mod <- ecravo_ponly_avo_mod <-  ecravo_mod_avo_mod <- ""

ecravo_allfree_anx_mod_m <- ecravo_afixed_anx_mod_m  <- ecravo_pfixed_anx_mod_m <- ecravo_apfixed_anx_mod_m <- ecravo_aonly_anx_mod_m <- ecravo_ponly_anx_mod_m <-  ecravo_mod_anx_mod_m <- ""
ecravo_allfree_anx_mod <- ecravo_afixed_anx_mod <- ecravo_pfixed_anx_mod <- ecravo_apfixed_anx_mod <- ecravo_aonly_anx_mod<- ecravo_ponly_anx_mod <- ecravo_mod_anx_mod <- ""

ecravo_allfree_sec_mod_m <- ecravo_afixed_sec_mod_m <- ecravo_pfixed_sec_mod_m <- ecravo_apfixed_sec_mod_m <- ecravo_aonly_sec_mod_m <- ecravo_ponly_sec_mod_m <- ecravo_mod_sec_mod_m <- ""
ecravo_allfree_sec_mod <- ecravo_afixed_sec_mod <- ecravo_pfixed_sec_mod <- ecravo_apfixed_sec_mod <- ecravo_aonly_sec_mod <- ecravo_ponly_sec_mod <-  ecravo_mod_sec_mod <- ""

ecranx_allfree_avo_mod_m <- ecranx_afixed_avo_mod_m <- ecranx_pfixed_avo_mod_m <- ecranx_apfixed_avo_mod_m <- ecranx_aonly_avo_mod_m <- ecranx_ponly_avo_mod_m <- ecranx_mod_avo_mod_m <- ""
ecranx_allfree_avo_mod <- ecranx_afixed_avo_mod <- ecranx_pfixed_avo_mod <- ecranx_apfixed_avo_mod <- ecranx_aonly_avo_mod <- ecranx_ponly_avo_mod <-  ecranx_mod_avo_mod <- ""

ecranx_allfree_anx_mod_m <- ecranx_afixed_anx_mod_m <-  ecranx_pfixed_anx_mod_m <- ecranx_apfixed_anx_mod_m <- ecranx_aonly_anx_mod_m <- ecranx_ponly_anx_mod_m <- ecranx_mod_anx_mod_m <- ""
ecranx_allfree_anx_mod <- ecranx_afixed_anx_mod <- ecranx_pfixed_anx_mod <- ecranx_apfixed_anx_mod <- ecranx_aonly_anx_mod <- ecranx_ponly_anx_mod <- ecranx_mod_anx_mod <- ""

ecranx_allfree_sec_mod_m <- ecranx_afixed_sec_mod_m <- ecranx_pfixed_sec_mod_m <- ecranx_apfixed_sec_mod_m <- ecranx_aonly_sec_mod_m <- ecranx_ponly_sec_mod_m <- ecranx_mod_sec_mod_m <- ""
ecranx_allfree_sec_mod <- ecranx_afixed_sec_mod <- ecranx_pfixed_sec_mod <- ecranx_apfixed_sec_mod <- ecranx_aonly_sec_mod <- ecranx_ponly_sec_mod <- ecranx_mod_sec_mod <- ""



# AAR avo gating S2 negint avo ---------------------------------------------
ecravo_allfree_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_allfree_avo_mod_m <- sem(model = ecravo_allfree_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_afixed_avo_mod_m <- sem(model = ecravo_afixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_pfixed_avo_mod_m <- sem(model = ecravo_pfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_apfixed_avo_mod_m <- sem(model = ecravo_apfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_aonly_avo_mod_m <- sem(model = ecravo_aonly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_ponly_avo_mod_m <- sem(model = ecravo_ponly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_mod_avo_mod <-"
#paths that are not particularly strong
pavo0 ~ 0*ccpr
ccpr ~ 0*pravo0

#strong paths
pavo1 ~  a*ccpt
ccpt ~ c*pravo1 
ccpr ~ c*pravo1
pravo1 ~~ pravo0
pavo1~~pavo0
ccpt ~~ ccpr
pavo1 ~ pravo1
pavo0 ~pravo0
ac:=a*c

 ECRavo_1 ~~ 0*av1ccpt1 
ECRavo_0 ~~ 0*av1ccpt1 
ECRavo_1 ~~ 0*av0ccpt1 
ECRavo_0 ~~ 0*av0ccpt1 
av1ccpt1 ~~ av1ccpt1 
av0ccpt1 ~~ av0ccpt1 
av1ccpt1 ~~ 0*ccpt 
av1ccpt1 ~~ 0*ccpr 
av0ccpt1 ~~ 0*ccpt 
av0ccpt1 ~~ 0*ccpr  
pavo1 ~ aaa*ECRavo_1

pavo1 ~ aaa*ECRavo_0 
pavo1 ~ av1ccpt1

pavo1 ~ aaa*av0ccpt1 
"
ecravo_mod_avo_mod_m <- sem(model = ecravo_mod_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
ecravo_mod_avo_mod_m <- ecravo_allfree_avo_mod_m
ecravo_mod_avo_mod <- ecravo_allfree_avo_mod
anova(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m) 
# No sign of s2 moderation for AAR avo on avo process

# AAR avo gating S2 negint anx -----------------------------------------



ecravo_allfree_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_allfree_anx_mod_m <- sem(model = ecravo_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_afixed_anx_mod_m <- sem(model = ecravo_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_pfixed_anx_mod_m <- sem(model = ecravo_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_apfixed_anx_mod_m <- sem(model = ecravo_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_aonly_anx_mod_m <- sem(model = ecravo_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_ponly_anx_mod_m <- sem(model = ecravo_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m)

ecravo_mod_anx_mod <- "

panx1 ~  f*ccpt
panx0 ~ ccpt
panx0 ~ f*ccpr

ccpt ~ h*pranx1 
ccpt ~ d*pranx0
ccpr ~ d*pranx1
ccpr ~ h*pranx0
pranx1 ~~ pranx0
panx1~~panx0
ccpt ~~ ccpr

panx1~pranx1
panx0 ~ pranx0
 ECRavo_1 ~~ 0*av1ccpt1 
ECRavo_0 ~~ 0*av1ccpt1 
ECRavo_1 ~~ 0*av1ccpr0 
ECRavo_0 ~~ 0*av1ccpr0 
ECRavo_1 ~~ 0*av0ccpt1 
ECRavo_0 ~~ 0*av0ccpt1 
ECRavo_1 ~~ 0*av0ccpr0 
ECRavo_0 ~~ 0*av0ccpr0
av1ccpt1 ~~ av1ccpt1 
av1ccpr0 ~~ av1ccpr0 
av0ccpt1 ~~ av0ccpt1 
av0ccpr0 ~~ av0ccpr0
av1ccpt1 ~~ 0*ccpt 
av1ccpt1 ~~ 0*ccpr 
av1ccpr0 ~~ 0*ccpt 
av1ccpr0 ~~ 0*ccpr 
av0ccpt1 ~~ 0*ccpt 
av0ccpt1 ~~ 0*ccpr 
av0ccpr0 ~~ 0*ccpt 
av0ccpr0 ~~ 0*ccpr 
panx1 ~ 0*ECRavo_1

panx1 ~ lll*ECRavo_0

panx0 ~ lll*ECRavo_1

panx0 ~ 0*ECRavo_0 
panx1 ~ 0*av1ccpt1

panx1 ~ lll*av0ccpt1

panx0 ~ 0*av1ccpt1

panx0 ~ lll*av1ccpr0

panx0 ~ 0*av0ccpt1

panx0 ~ av0ccpr0 

"
ecravo_mod_anx_mod_m <- sem(model = ecravo_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)
# Mod preferred. Extent to which patient coupling predicts increases in partner and patient anxiety over course of negative interaction is moderated by patient and partner braoder anxiety (patient and partner for patient's anxiety and partner's anxiety for partner increases in anxiety). 
#Specifically this weakens the tendency for dependency to increase anxiety in patients (and tilts towards contrarianism increasing anxiety).  


# AAR avo gating S2 negint sec --------------------------------------------

ecravo_allfree_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_allfree_sec_mod_m <- sem(model = ecravo_allfree_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_afixed_sec_mod_m <- sem(model = ecravo_afixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_pfixed_sec_mod_m <- sem(model = ecravo_pfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_apfixed_sec_mod_m <- sem(model = ecravo_apfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_aonly_sec_mod_m <- sem(model = ecravo_aonly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_ponly_sec_mod_m <- sem(model = ecravo_ponly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m)
ecravo_mod_sec_mod <- "

psec1 ~  f*ccpt
psec1 ~ 0*ccpr
psec0 ~ b*ccpt
psec0 ~ 0*ccpr
ccpt ~ 0*prsec1 
ccpt ~ 0*prsec0
ccpr ~ h*prsec1
ccpr ~ 0*prsec0
prsec1 ~~ prsec0
psec1~~psec0
ccpt ~~ ccpr


psec1~prsec1
psec0 ~ prsec0
 ECRavo_1 ~~ 0*av1ccpt1 
ECRavo_0 ~~ 0*av1ccpt1 
ECRavo_1 ~~ 0*av0ccpt1 
ECRavo_0 ~~ 0*av0ccpt1 
av1ccpt1 ~~ av1ccpt1 
av0ccpt1 ~~ av0ccpt1 
av1ccpt1 ~~ 0*ccpt 
av1ccpt1 ~~ 0*ccpr 
av0ccpt1 ~~ 0*ccpt 
av0ccpt1 ~~ 0*ccpr  
psec1 ~ 0*ECRavo_1

psec1 ~ v*ECRavo_0

psec0 ~ 0*ECRavo_1

psec0 ~ 0*ECRavo_0 
psec1 ~ 0*av1ccpt1

psec1 ~ v*av0ccpt1

psec0 ~ v*av1ccpt1

psec0 ~ v*av0ccpt1 
"
ecravo_mod_sec_mod_m <- sem(model = ecravo_mod_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m)

# AAR anx gating S2 negint avo ---------------------------------------------


ecranx_allfree_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ),model_syntax =  negint_avo_mod),"\n")
ecranx_allfree_avo_mod_m <- sem(model = ecranx_allfree_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), model_syntax = negint_avo_mod),"\n")
ecranx_afixed_avo_mod_m <- sem(model = ecranx_afixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), model_syntax = negint_avo_mod),"\n")
ecranx_pfixed_avo_mod_m <- sem(model = ecranx_pfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ),model_syntax =  negint_avo_mod),"\n")
ecranx_apfixed_avo_mod_m <- sem(model = ecranx_apfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ),model_syntax =  negint_avo_mod),"\n")
ecranx_aonly_avo_mod_m <- sem(model = ecranx_aonly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ),model_syntax =   negint_avo_mod),"\n")
ecranx_ponly_avo_mod_m <- sem(model = ecranx_ponly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
ecranx_mod_avo_mod <- "

#paths that are not particularly strong
pavo0 ~ 0*ccpr
ccpr ~ 0*pravo0

#strong paths
pavo1 ~  a*ccpt
ccpt ~ c*pravo1 
ccpr ~ c*pravo1
pravo1 ~~ pravo0
pavo1~~pavo0
ccpt ~~ ccpr
pavo1 ~ pravo1
pavo0 ~pravo0
ac:=a*c

 ECRanx_1 ~~ 0*an1ccpt1 
ECRanx_0 ~~ 0*an1ccpt1 
ECRanx_1 ~~ 0*an0ccpt1 
ECRanx_0 ~~ 0*an0ccpt1 
an1ccpt1 ~~ an1ccpt1 
an0ccpt1 ~~ an0ccpt1 
an1ccpt1 ~~ 0*ccpt 
an1ccpt1 ~~ 0*ccpr 
an0ccpt1 ~~ 0*ccpt 
an0ccpt1 ~~ 0*ccpr  
pavo1 ~ 0*ECRanx_1

pavo1 ~ 0*ECRanx_0 
pavo1 ~ an1ccpt1

pavo1 ~ 0*an0ccpt1 
"
ecranx_mod_avo_mod_m <- sem(model = ecranx_mod_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
# no evidence of s2 moderation for avoidance by anxiety
anova(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m, ecranx_mod_avo_mod_m)

# AAR anx gating S2 negint anx -----------------------------------------

ecranx_allfree_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_allfree_anx_mod_m <- sem(model = ecranx_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_afixed_anx_mod_m <- sem(model = ecranx_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_pfixed_anx_mod_m <- sem(model = ecranx_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_apfixed_anx_mod_m <- sem(model = ecranx_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_aonly_anx_mod_m <- sem(model = ecranx_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_ponly_anx_mod_m <- sem(model = ecranx_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
ecranx_mod_anx_mod <- "
panx1 ~  f*ccpt
panx0 ~ ccpt
panx0 ~ f*ccpr

ccpt ~ h*pranx1 
ccpt ~ d*pranx0
ccpr ~ d*pranx1
ccpr ~ h*pranx0
pranx1 ~~ pranx0
panx1~~panx0
ccpt ~~ ccpr

panx1~pranx1
panx0 ~ pranx0
 ECRanx_1 ~~ 0*an1ccpt1 
ECRanx_0 ~~ 0*an1ccpt1 
ECRanx_1 ~~ 0*an1ccpr0 
ECRanx_0 ~~ 0*an1ccpr0 
ECRanx_1 ~~ 0*an0ccpt1 
ECRanx_0 ~~ 0*an0ccpt1 
ECRanx_1 ~~ 0*an0ccpr0 
ECRanx_0 ~~ 0*an0ccpr0
an1ccpt1 ~~ an1ccpt1 
an1ccpr0 ~~ an1ccpr0 
an0ccpt1 ~~ an0ccpt1 
an0ccpr0 ~~ an0ccpr0
an1ccpt1 ~~ 0*ccpt 
an1ccpt1 ~~ 0*ccpr 
an1ccpr0 ~~ 0*ccpt 
an1ccpr0 ~~ 0*ccpr 
an0ccpt1 ~~ 0*ccpt 
an0ccpt1 ~~ 0*ccpr 
an0ccpr0 ~~ 0*ccpt 
an0ccpr0 ~~ 0*ccpr 
panx1 ~ 0*ECRanx_1

panx1 ~ l*ECRanx_0

panx0 ~ l*ECRanx_1

panx0 ~ 0*ECRanx_0 
panx1 ~ l*an1ccpt1

panx1 ~ l*an0ccpt1

panx0 ~ an1ccpt1

panx0 ~ 0*an1ccpr0

panx0 ~ l*an0ccpt1

panx0 ~ 0*an0ccpr0 
"
ecranx_mod_anx_mod_m <- sem(model = ecranx_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
# Mod preferred. Extent to which patients dependency predicts increases in anxiety in patient is gated by patient and partner's broader anxiety whereby this association is weakened or goes the opposite direction when patient or partner is higher in broad anxiety. In partner's, extent to which partner reports increases in anxiety in response to patient's dependency is amplified when patient is high  in anxiety but attenuated when partner is high in attachment anxiety. 
anova(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)




# AAR anx gating S2 negint sec --------------------------------------------


ecranx_allfree_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_allfree_sec_mod_m <- sem(model = ecranx_allfree_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_afixed_sec_mod_m <- sem(model = ecranx_afixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_pfixed_sec_mod_m <- sem(model = ecranx_pfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_apfixed_sec_mod_m <- sem(model = ecranx_apfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_aonly_sec_mod_m <- sem(model = ecranx_aonly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_ponly_sec_mod_m <- sem(model = ecranx_ponly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m) 
ecranx_mod_sec_mod <- "
psec1 ~  f*ccpt
psec1 ~ 0*ccpr
psec0 ~ b*ccpt
psec0 ~ 0*ccpr
ccpt ~ 0*prsec1 
ccpt ~ 0*prsec0
ccpr ~ h*prsec1
ccpr ~ 0*prsec0
prsec1 ~~ prsec0
psec1~~psec0
ccpt ~~ ccpr


psec1~prsec1
psec0 ~ prsec0
 ECRanx_1 ~~ 0*an1ccpt1 
ECRanx_0 ~~ 0*an1ccpt1 
ECRanx_1 ~~ 0*an0ccpt1 
ECRanx_0 ~~ 0*an0ccpt1 
an1ccpt1 ~~ an1ccpt1 
an0ccpt1 ~~ an0ccpt1 
an1ccpt1 ~~ 0*ccpt 
an1ccpt1 ~~ 0*ccpr 
an0ccpt1 ~~ 0*ccpt 
an0ccpt1 ~~ 0*ccpr  
psec1 ~ m*ECRanx_1

psec1 ~ 0*ECRanx_0

psec0 ~ ECRanx_1

psec0 ~ v*ECRanx_0 
psec1 ~ 0*an1ccpt1

psec1 ~ m*an0ccpt1

psec0 ~ v*an1ccpt1

psec0 ~ v*an0ccpt1
"
ecranx_mod_sec_mod_m <- sem(model = ecranx_mod_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m) 

# S2 AAR Negint Save Models  ----------------------------------------------
avav_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m)
avav_syn_model_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod)
df <- mapply(pull_fitmeasures, avav_model_list, avav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_n_aar_avo_saam_avo"
fulldf <- df

avan_model_list <- c(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)
avan_syn_model_list <- c(ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod, ecravo_mod_anx_mod)
df <- mapply(pull_fitmeasures, avan_model_list, avan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_n_aar_avo_saam_anx"
fulldf <- bind_rows(fulldf, df)

avse_model_list <- c(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m)
avse_syn_model_list <- c(ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod, ecravo_mod_sec_mod)
df <- mapply(pull_fitmeasures, avse_model_list, avse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_n_aar_avo_saam_sec"
fulldf <- bind_rows(fulldf, df)


anav_model_list <- c(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m, ecranx_mod_avo_mod_m)
anav_syn_model_list <- c(ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod, ecranx_mod_avo_mod)
df <- mapply(pull_fitmeasures, anav_model_list, anav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_n_aar_anx_saam_avo"
fulldf <- bind_rows(fulldf, df)

anan_model_list <- c(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)
anan_syn_model_list <- c(ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod, ecranx_mod_anx_mod)
df <- mapply(pull_fitmeasures, anan_model_list, anan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_n_aar_anx_saam_anx"
fulldf <- bind_rows(fulldf, df)

anse_model_list <- c(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
anse_syn_model_list <- c(ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
df <- mapply(pull_fitmeasures, anse_model_list, anse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_n_aar_anx_saam_sec"
fulldf <- bind_rows(fulldf, df)


s2_n_aar_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m,
                         ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m,ecravo_mod_anx_mod_m,
                         ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m,ecravo_mod_sec_mod_m, 
                         ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m,ecranx_mod_avo_mod_m,
                         ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m,  ecranx_mod_anx_mod_m,
                         ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
s2_n_aar_syn_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod,
                       ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod,ecravo_mod_anx_mod,
                       ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod,ecravo_mod_sec_mod,
                       ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod,ecranx_mod_avo_mod,
                       ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod,  ecranx_mod_anx_mod,
                       ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
s2_n_aar_df <- posnegint_formoderation
names(s2_n_aar_model_list) <- c("ecravo_allfree_avo_mod_m", "ecravo_afixed_avo_mod_m", "ecravo_pfixed_avo_mod_m","ecravo_apfixed_avo_mod_m","ecravo_aonly_avo_mod_m","ecravo_ponly_avo_mod_m", "ecravo_mod_avo_mod_m",
                                "ecravo_allfree_anx_mod_m", "ecravo_afixed_anx_mod_m", "ecravo_pfixed_anx_mod_m","ecravo_apfixed_anx_mod_m","ecravo_aonly_anx_mod_m","ecravo_ponly_anx_mod_m","ecravo_mod_anx_mod_m",
                                "ecravo_allfree_sec_mod_m", "ecravo_afixed_sec_mod_m", "ecravo_pfixed_sec_mod_m","ecravo_apfixed_sec_mod_m","ecravo_aonly_sec_mod_m","ecravo_ponly_sec_mod_m","ecravo_mod_sec_mod_m",
                                "ecranx_allfree_avo_mod_m", "ecranx_afixed_avo_mod_m", "ecranx_pfixed_avo_mod_m","ecranx_apfixed_avo_mod_m","ecranx_aonly_avo_mod_m","ecranx_ponly_avo_mod_m","ecranx_mod_avo_mod_m",
                                "ecranx_allfree_anx_mod_m", "ecranx_afixed_anx_mod_m", "ecranx_pfixed_anx_mod_m","ecranx_apfixed_anx_mod_m","ecranx_aonly_anx_mod_m","ecranx_ponly_anx_mod_m",  "ecranx_mod_anx_mod_m",
                                "ecranx_allfree_sec_mod_m", "ecranx_afixed_sec_mod_m", "ecranx_pfixed_sec_mod_m","ecranx_apfixed_sec_mod_m","ecranx_aonly_sec_mod_m","ecranx_ponly_sec_mod_m", "ecranx_mod_sec_mod_m")
names(s2_n_aar_syn_list) <- c("ecravo_allfree_avo_mod", "ecravo_afixed_avo_mod", "ecravo_pfixed_avo_mod","ecravo_apfixed_avo_mod","ecravo_aonly_avo_mod","ecravo_ponly_avo_mod", "ecravo_mod_avo_mod",
                              "ecravo_allfree_anx_mod", "ecravo_afixed_anx_mod", "ecravo_pfixed_anx_mod","ecravo_apfixed_anx_mod","ecravo_aonly_anx_mod","ecravo_ponly_anx_mod","ecravo_mod_anx_mod",
                              "ecravo_allfree_sec_mod", "ecravo_afixed_sec_mod", "ecravo_pfixed_sec_mod","ecravo_apfixed_sec_mod","ecravo_aonly_sec_mod","ecravo_ponly_sec_mod","ecravo_mod_sec_mod",
                              "ecranx_allfree_avo_mod", "ecranx_afixed_avo_mod", "ecranx_pfixed_avo_mod","ecranx_apfixed_avo_mod","ecranx_aonly_avo_mod","ecranx_ponly_avo_mod","ecranx_mod_avo_mod",
                              "ecranx_allfree_anx_mod", "ecranx_afixed_anx_mod", "ecranx_pfixed_anx_mod","ecranx_apfixed_anx_mod","ecranx_aonly_anx_mod","ecranx_ponly_anx_mod",  "ecranx_mod_anx_mod",
                              "ecranx_allfree_sec_mod", "ecranx_afixed_sec_mod", "ecranx_pfixed_sec_mod","ecranx_apfixed_sec_mod","ecranx_aonly_sec_mod","ecranx_ponly_sec_mod", "ecranx_mod_sec_mod")

# Initialize Models -------------------------------------------------------

ecravo_allfree_avo_mod_m <-  ecravo_afixed_avo_mod_m <-  ecravo_pfixed_avo_mod_m <- ecravo_apfixed_avo_mod_m <- ecravo_aonly_avo_mod_m <- ecravo_ponly_avo_mod_m <-  ecravo_mod_avo_mod_m <- ""
ecravo_allfree_avo_mod <- ecravo_afixed_avo_mod <- ecravo_pfixed_avo_mod <- ecravo_apfixed_avo_mod <- ecravo_aonly_avo_mod <- ecravo_ponly_avo_mod <-  ecravo_mod_avo_mod <- ""

ecravo_allfree_anx_mod_m <- ecravo_afixed_anx_mod_m  <- ecravo_pfixed_anx_mod_m <- ecravo_apfixed_anx_mod_m <- ecravo_aonly_anx_mod_m <- ecravo_ponly_anx_mod_m <-  ecravo_mod_anx_mod_m <- ""
ecravo_allfree_anx_mod <- ecravo_afixed_anx_mod <- ecravo_pfixed_anx_mod <- ecravo_apfixed_anx_mod <- ecravo_aonly_anx_mod<- ecravo_ponly_anx_mod <- ecravo_mod_anx_mod <- ""

ecravo_allfree_sec_mod_m <- ecravo_afixed_sec_mod_m <- ecravo_pfixed_sec_mod_m <- ecravo_apfixed_sec_mod_m <- ecravo_aonly_sec_mod_m <- ecravo_ponly_sec_mod_m <- ecravo_mod_sec_mod_m <- ""
ecravo_allfree_sec_mod <- ecravo_afixed_sec_mod <- ecravo_pfixed_sec_mod <- ecravo_apfixed_sec_mod <- ecravo_aonly_sec_mod <- ecravo_ponly_sec_mod <-  ecravo_mod_sec_mod <- ""

ecranx_allfree_avo_mod_m <- ecranx_afixed_avo_mod_m <- ecranx_pfixed_avo_mod_m <- ecranx_apfixed_avo_mod_m <- ecranx_aonly_avo_mod_m <- ecranx_ponly_avo_mod_m <- ecranx_mod_avo_mod_m <- ""
ecranx_allfree_avo_mod <- ecranx_afixed_avo_mod <- ecranx_pfixed_avo_mod <- ecranx_apfixed_avo_mod <- ecranx_aonly_avo_mod <- ecranx_ponly_avo_mod <-  ecranx_mod_avo_mod <- ""

ecranx_allfree_anx_mod_m <- ecranx_afixed_anx_mod_m <-  ecranx_pfixed_anx_mod_m <- ecranx_apfixed_anx_mod_m <- ecranx_aonly_anx_mod_m <- ecranx_ponly_anx_mod_m <- ecranx_mod_anx_mod_m <- ""
ecranx_allfree_anx_mod <- ecranx_afixed_anx_mod <- ecranx_pfixed_anx_mod <- ecranx_apfixed_anx_mod <- ecranx_aonly_anx_mod <- ecranx_ponly_anx_mod <- ecranx_mod_anx_mod <- ""

ecranx_allfree_sec_mod_m <- ecranx_afixed_sec_mod_m <- ecranx_pfixed_sec_mod_m <- ecranx_apfixed_sec_mod_m <- ecranx_aonly_sec_mod_m <- ecranx_ponly_sec_mod_m <- ecranx_mod_sec_mod_m <- ""
ecranx_allfree_sec_mod <- ecranx_afixed_sec_mod <- ecranx_pfixed_sec_mod <- ecranx_apfixed_sec_mod <- ecranx_aonly_sec_mod <- ecranx_ponly_sec_mod <- ecranx_mod_sec_mod <- ""





# AAR avo gating S1 posint avo ---------------------------------------------
ecravo_allfree_avo_mod <- paste(posint_avo_mod, ECR_moderation("avo","avo", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecravo_allfree_avo_mod_m <- sem(model = ecravo_allfree_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_avo_mod <- paste(posint_avo_mod, ECR_moderation("avo","avo", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecravo_afixed_avo_mod_m <- sem(model = ecravo_afixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_avo_mod <- paste(posint_avo_mod, ECR_moderation("avo","avo", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecravo_pfixed_avo_mod_m <- sem(model = ecravo_pfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_avo_mod <- paste(posint_avo_mod, ECR_moderation("avo","avo", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecravo_apfixed_avo_mod_m <- sem(model = ecravo_apfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_avo_mod <- paste(posint_avo_mod, ECR_moderation("avo","avo", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecravo_aonly_avo_mod_m <- sem(model = ecravo_aonly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_avo_mod <- paste(posint_avo_mod, ECR_moderation("avo","avo", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecravo_ponly_avo_mod_m <- sem(model = ecravo_ponly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m)
ecravo_mod_avo_mod_m <- ecravo_allfree_avo_mod_m
ecravo_mod_avo_mod <- ecravo_allfree_avo_mod

# AAR avo gating S1 posint anx -----------------------------------------

ecravo_allfree_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_allfree_anx_mod_m <- sem(model = ecravo_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_afixed_anx_mod_m <- sem(model = ecravo_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_pfixed_anx_mod_m <- sem(model = ecravo_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_apfixed_anx_mod_m <- sem(model = ecravo_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_aonly_anx_mod_m <- sem(model = ecravo_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_ponly_anx_mod_m <- sem(model = ecravo_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m)

ecravo_mod_anx_mod <- "
p_ccpt ~ b*panx1 #a
p_ccpt ~ a*pranx1 #a
p_ccpr ~ b*panx0 # a
p_ccpr ~ a*pranx0 #a


pranx1 ~~ pranx0
panx1~~panx0
p_ccpt ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

 ECRavo_1 ~~ 0*av1pran1 
ECRavo_0 ~~ 0*av1pran1 
ECRavo_1 ~~ 0*av1pran0 
ECRavo_0 ~~ 0*av1pran0 
ECRavo_1 ~~ 0*av0pran1 
ECRavo_0 ~~ 0*av0pran1 
ECRavo_1 ~~ 0*av0pran0 
ECRavo_0 ~~ 0*av0pran0
av1pran1 ~~ av1pran1 
av1pran0 ~~ av1pran0 
av0pran1 ~~ av0pran1 
av0pran0 ~~ av0pran0
pranx1 ~~ 0*av1pran1
pranx1 ~~ 0*av1pran0
pranx1 ~~ 0*av0pran1
pranx1 ~~ 0*av0pran0
pranx0 ~~ 0*av1pran1
pranx0 ~~ 0*av1pran0
pranx0 ~~ 0*av0pran1
pranx0 ~~ 0*av0pran0 
p_ccpt ~ 0*ECRavo_1

p_ccpt ~ ECRavo_0

p_ccpr ~ 0*ECRavo_1

p_ccpr ~ ECRavo_0 
p_ccpt ~ av1pran1

p_ccpt ~ 0*av0pran1

p_ccpr ~ 0*av1pran0

p_ccpr ~ 0*av0pran0 

"
ecravo_mod_anx_mod_m <- sem(model = ecravo_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
ecravo_mod_anx_mod_m <- ecravo_allfree_anx_mod_m 
ecravo_mod_anx_mod <- ecravo_allfree_anx_mod

# AAR avo gating S1 posint sec --------------------------------------------

ecravo_allfree_sec_mod <- paste(posint_sec_mod, ECR_moderation("avo","sec", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecravo_allfree_sec_mod_m <- sem(model = ecravo_allfree_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_sec_mod <- paste(posint_sec_mod, ECR_moderation("avo","sec", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecravo_afixed_sec_mod_m <- sem(model = ecravo_afixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_sec_mod <- paste(posint_sec_mod, ECR_moderation("avo","sec", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecravo_pfixed_sec_mod_m <- sem(model = ecravo_pfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_sec_mod <- paste(posint_sec_mod, ECR_moderation("avo","sec", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecravo_apfixed_sec_mod_m <- sem(model = ecravo_apfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_sec_mod <- paste(posint_sec_mod, ECR_moderation("avo","sec", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecravo_aonly_sec_mod_m <- sem(model = ecravo_aonly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_sec_mod <- paste(posint_sec_mod, ECR_moderation("avo","sec", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecravo_ponly_sec_mod_m <- sem(model = ecravo_ponly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m)

ecravo_mod_sec_mod <- "
p_ccpt ~ 0*psec1 #a
p_ccpt ~ 0*prsec1 #a
p_ccpt ~ 0*psec0 #p
p_ccpt ~ m*prsec0 #p
p_ccpr ~ 0*psec1 #p
p_ccpr ~ m*prsec1 #p
p_ccpr ~ 0*psec0 # a
p_ccpr ~ l*prsec0 #a

prsec1 ~~ prsec0
psec1~~psec0
p_ccpt ~~ p_ccpr
psec1 ~ prsec1
psec0 ~prsec0

 ECRavo_1 ~~ 0*av1prse1 
ECRavo_0 ~~ 0*av1prse1 
ECRavo_1 ~~ 0*av1prse0 
ECRavo_0 ~~ 0*av1prse0 
ECRavo_1 ~~ 0*av0prse1 
ECRavo_0 ~~ 0*av0prse1 
ECRavo_1 ~~ 0*av0prse0 
ECRavo_0 ~~ 0*av0prse0
av1prse1 ~~ av1prse1 
av1prse0 ~~ av1prse0 
av0prse1 ~~ av0prse1 
av0prse0 ~~ av0prse0
prsec1 ~~ 0*av1prse1
prsec1 ~~ 0*av1prse0
prsec1 ~~ 0*av0prse1
prsec1 ~~ 0*av0prse0
prsec0 ~~ 0*av1prse1
prsec0 ~~ 0*av1prse0
prsec0 ~~ 0*av0prse1
prsec0 ~~ 0*av0prse0 
p_ccpt ~ 0*ECRavo_1

p_ccpt ~ 0*ECRavo_0

p_ccpr ~ 0*ECRavo_1

p_ccpr ~ 0*ECRavo_0 
p_ccpt ~ m*av1prse0

p_ccpt ~ l*av0prse0

p_ccpr ~ m*av1prse1

p_ccpr ~ 0*av1prse0

p_ccpr ~ l*av0prse1

p_ccpr ~ 0*av0prse0 

"
ecravo_mod_sec_mod_m <- sem(model = ecravo_mod_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m)

# AAR anx gating S1 posint avo ---------------------------------------------

ecranx_allfree_avo_mod <- paste(posint_avo_mod, ECR_moderation("anx","avo", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecranx_allfree_avo_mod_m <- sem(model = ecranx_allfree_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_avo_mod <- paste(posint_avo_mod, ECR_moderation("anx","avo", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecranx_afixed_avo_mod_m <- sem(model = ecranx_afixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_avo_mod <- paste(posint_avo_mod, ECR_moderation("anx","avo", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecranx_pfixed_avo_mod_m <- sem(model = ecranx_pfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_avo_mod <- paste(posint_avo_mod, ECR_moderation("anx","avo", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecranx_apfixed_avo_mod_m <- sem(model = ecranx_apfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_avo_mod <- paste(posint_avo_mod, ECR_moderation("anx","avo", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecranx_aonly_avo_mod_m <- sem(model = ecranx_aonly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_avo_mod <- paste(posint_avo_mod, ECR_moderation("anx","avo", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecranx_ponly_avo_mod_m <- sem(model = ecranx_ponly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_mod_avo_mod <- "
p_ccpr ~ pravo1 #p
pavo1 ~ pravo1
pavo0 ~pravo0


pravo1 ~~ pravo0
pavo1 ~~ pavo0

p_ccpt ~~ p_ccpr
p_ccpr ~~ 0*pavo1
p_ccpr ~~ 0*pavo0
p_ccpt ~ 0*pavo0
p_ccpt ~ 0*pavo1
p_ccpt ~ 0*pravo0
p_ccpt ~ 0*pravo1
 ECRanx_1 ~~ 0*an1prav1 
ECRanx_0 ~~ 0*an1prav1 
ECRanx_1 ~~ 0*an0prav1 
ECRanx_0 ~~ 0*an0prav1 
an1prav1 ~~ an1prav1 
an0prav1 ~~ an0prav1 
pravo1 ~~ 0*an1prav1
pravo1 ~~ 0*an0prav1
pravo0 ~~ 0*an1prav1
pravo0 ~~ 0*an0prav1 
p_ccpr ~ 0*ECRanx_1

p_ccpr ~ ECRanx_0 
p_ccpr ~ an1prav1

p_ccpr ~ 0*an0prav1 
"
ecranx_mod_avo_mod_m <- sem(model = ecranx_mod_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m, ecranx_mod_avo_mod_m)
# no evidence that anxiety gates avoidance during positive interaction. 
ecranx_mod_avo_mod_m <- ecranx_allfree_avo_mod_m 
ecranx_mod_avo_mod <- ecranx_allfree_avo_mod
# AAR anx gating S1 posint anx -----------------------------------------

ecranx_allfree_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_allfree_anx_mod_m <- sem(model = ecranx_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_afixed_anx_mod_m <- sem(model = ecranx_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_pfixed_anx_mod_m <- sem(model = ecranx_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_apfixed_anx_mod_m <- sem(model = ecranx_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_aonly_anx_mod_m <- sem(model = ecranx_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_ponly_anx_mod_m <- sem(model = ecranx_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m)

ecranx_mod_anx_mod <- "

p_ccpt ~ b*panx1 #a
p_ccpt ~ a*pranx1 #a
p_ccpr ~ b*panx0 # a
p_ccpr ~ a*pranx0 #a


pranx1 ~~ pranx0
panx1~~panx0
p_ccpt ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

 ECRanx_1 ~~ 0*an1pran1 
ECRanx_0 ~~ 0*an1pran1 
ECRanx_1 ~~ 0*an1pran0 
ECRanx_0 ~~ 0*an1pran0 
ECRanx_1 ~~ 0*an0pran1 
ECRanx_0 ~~ 0*an0pran1 
ECRanx_1 ~~ 0*an0pran0 
ECRanx_0 ~~ 0*an0pran0
an1pran1 ~~ an1pran1 
an1pran0 ~~ an1pran0 
an0pran1 ~~ an0pran1 
an0pran0 ~~ an0pran0
pranx1 ~~ 0*an1pran1
pranx1 ~~ 0*an1pran0
pranx1 ~~ 0*an0pran1
pranx1 ~~ 0*an0pran0
pranx0 ~~ 0*an1pran1
pranx0 ~~ 0*an1pran0
pranx0 ~~ 0*an0pran1
pranx0 ~~ 0*an0pran0 
p_ccpt ~ i*ECRanx_1

p_ccpt ~ 0*ECRanx_0

p_ccpr ~ 0*ECRanx_1

p_ccpr ~ 0*ECRanx_0 
p_ccpt ~ 0*an1pran1

p_ccpt ~ 0*an0pran1

p_ccpr ~ 0*an1pran0

p_ccpr ~ an0pran0 
pranx1 ~~ ECRanx_1
"
ecranx_mod_anx_mod_m <- sem(model = ecranx_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
# Marginal effect where the extent to which baseline anxiety predicts contrarianism during the positive interaction in partner is gated by partner's anxiety. In particular, partner's anxiety attenuates this relationship. 
anova(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)
# AAR anx gating S1 posint sec --------------------------------------------


ecranx_allfree_sec_mod <- paste(posint_sec_mod, ECR_moderation("anx","sec", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecranx_allfree_sec_mod_m <- sem(model = ecranx_allfree_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_sec_mod <- paste(posint_sec_mod, ECR_moderation("anx","sec", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecranx_afixed_sec_mod_m <- sem(model = ecranx_afixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_sec_mod <- paste(posint_sec_mod, ECR_moderation("anx","sec", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecranx_pfixed_sec_mod_m <- sem(model = ecranx_pfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_sec_mod <- paste(posint_sec_mod, ECR_moderation("anx","sec", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecranx_apfixed_sec_mod_m <- sem(model = ecranx_apfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_sec_mod <- paste(posint_sec_mod, ECR_moderation("anx","sec", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecranx_aonly_sec_mod_m <- sem(model = ecranx_aonly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_sec_mod <- paste(posint_sec_mod, ECR_moderation("anx","sec", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecranx_ponly_sec_mod_m <- sem(model = ecranx_ponly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m)

ecranx_mod_sec_mod <- "
p_ccpt ~ 0*psec1 #a
p_ccpt ~ 0*prsec1 #a
p_ccpt ~ 0*psec0 #p
p_ccpt ~ k*prsec0 #p
p_ccpr ~ 0*psec1 #p
p_ccpr ~ k*prsec1 #p
p_ccpr ~ 0*psec0 # a
p_ccpr ~ l*prsec0 #a

prsec1 ~~ prsec0
psec1~~psec0
p_ccpt ~~ p_ccpr
psec1 ~ prsec1
psec0 ~prsec0

 ECRanx_1 ~~ 0*an1prse1 
ECRanx_0 ~~ 0*an1prse1 
ECRanx_1 ~~ 0*an1prse0 
ECRanx_0 ~~ 0*an1prse0 
ECRanx_1 ~~ 0*an0prse1 
ECRanx_0 ~~ 0*an0prse1 
ECRanx_1 ~~ 0*an0prse0 
ECRanx_0 ~~ 0*an0prse0
an1prse1 ~~ an1prse1 
an1prse0 ~~ an1prse0 
an0prse1 ~~ an0prse1 
an0prse0 ~~ an0prse0
prsec1 ~~ 0*an1prse1
prsec1 ~~ 0*an1prse0
prsec1 ~~ 0*an0prse1
prsec1 ~~ 0*an0prse0
prsec0 ~~ 0*an1prse1
prsec0 ~~ 0*an1prse0
prsec0 ~~ 0*an0prse1
prsec0 ~~ 0*an0prse0 
p_ccpt ~ 0*ECRanx_1

p_ccpt ~ 0*ECRanx_0

p_ccpr ~ 0*ECRanx_1

p_ccpr ~ 0*ECRanx_0 
p_ccpt ~ k*an1prse0

p_ccpt ~ 0*an0prse0

p_ccpr ~ l*an1prse1

p_ccpr ~ l*an1prse0

p_ccpr ~ 0*an0prse1

p_ccpr ~ 0*an0prse0 
"
ecranx_mod_sec_mod_m <- sem(model = ecranx_mod_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

# S1 AAR Posint Save Models  ----------------------------------------------
avav_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m)
avav_syn_model_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod)
df <- mapply(pull_fitmeasures, avav_model_list, avav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_p_aar_avo_saam_avo"
fulldf <- df

avan_model_list <- c(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)
avan_syn_model_list <- c(ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod, ecravo_mod_anx_mod)
df <- mapply(pull_fitmeasures, avan_model_list, avan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_p_aar_avo_saam_anx"
fulldf <- bind_rows(fulldf, df)

avse_model_list <- c(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m)
avse_syn_model_list <- c(ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod, ecravo_mod_sec_mod)
df <- mapply(pull_fitmeasures, avse_model_list, avse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_p_aar_avo_saam_sec"
fulldf <- bind_rows(fulldf, df)


anav_model_list <- c(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m, ecranx_mod_avo_mod_m)
anav_syn_model_list <- c(ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod, ecranx_mod_avo_mod)
df <- mapply(pull_fitmeasures, anav_model_list, anav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_p_aar_anx_saam_avo"
fulldf <- bind_rows(fulldf, df)

anan_model_list <- c(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)
anan_syn_model_list <- c(ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod, ecranx_mod_anx_mod)
df <- mapply(pull_fitmeasures, anan_model_list, anan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_p_aar_anx_saam_anx"
fulldf <- bind_rows(fulldf, df)

anse_model_list <- c(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
anse_syn_model_list <- c(ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
df <- mapply(pull_fitmeasures, anse_model_list, anse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_p_aar_anx_saam_sec"
fulldf <- bind_rows(fulldf, df)


s1_p_aar_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m,
                         ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m,ecravo_mod_anx_mod_m,
                         ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m,ecravo_mod_sec_mod_m, 
                         ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m,ecranx_mod_avo_mod_m,
                         ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m,  ecranx_mod_anx_mod_m,
                         ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
s1_p_aar_syn_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod,
                       ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod,ecravo_mod_anx_mod,
                       ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod,ecravo_mod_sec_mod,
                       ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod,ecranx_mod_avo_mod,
                       ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod,  ecranx_mod_anx_mod,
                       ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
s1_p_aar_df <- posnegint_formoderation
names(s1_p_aar_model_list) <- c("ecravo_allfree_avo_mod_m", "ecravo_afixed_avo_mod_m", "ecravo_pfixed_avo_mod_m","ecravo_apfixed_avo_mod_m","ecravo_aonly_avo_mod_m","ecravo_ponly_avo_mod_m", "ecravo_mod_avo_mod_m",
                                "ecravo_allfree_anx_mod_m", "ecravo_afixed_anx_mod_m", "ecravo_pfixed_anx_mod_m","ecravo_apfixed_anx_mod_m","ecravo_aonly_anx_mod_m","ecravo_ponly_anx_mod_m","ecravo_mod_anx_mod_m",
                                "ecravo_allfree_sec_mod_m", "ecravo_afixed_sec_mod_m", "ecravo_pfixed_sec_mod_m","ecravo_apfixed_sec_mod_m","ecravo_aonly_sec_mod_m","ecravo_ponly_sec_mod_m","ecravo_mod_sec_mod_m",
                                "ecranx_allfree_avo_mod_m", "ecranx_afixed_avo_mod_m", "ecranx_pfixed_avo_mod_m","ecranx_apfixed_avo_mod_m","ecranx_aonly_avo_mod_m","ecranx_ponly_avo_mod_m","ecranx_mod_avo_mod_m",
                                "ecranx_allfree_anx_mod_m", "ecranx_afixed_anx_mod_m", "ecranx_pfixed_anx_mod_m","ecranx_apfixed_anx_mod_m","ecranx_aonly_anx_mod_m","ecranx_ponly_anx_mod_m",  "ecranx_mod_anx_mod_m",
                                "ecranx_allfree_sec_mod_m", "ecranx_afixed_sec_mod_m", "ecranx_pfixed_sec_mod_m","ecranx_apfixed_sec_mod_m","ecranx_aonly_sec_mod_m","ecranx_ponly_sec_mod_m", "ecranx_mod_sec_mod_m")
names(s1_p_aar_syn_list) <- c("ecravo_allfree_avo_mod", "ecravo_afixed_avo_mod", "ecravo_pfixed_avo_mod","ecravo_apfixed_avo_mod","ecravo_aonly_avo_mod","ecravo_ponly_avo_mod", "ecravo_mod_avo_mod",
                              "ecravo_allfree_anx_mod", "ecravo_afixed_anx_mod", "ecravo_pfixed_anx_mod","ecravo_apfixed_anx_mod","ecravo_aonly_anx_mod","ecravo_ponly_anx_mod","ecravo_mod_anx_mod",
                              "ecravo_allfree_sec_mod", "ecravo_afixed_sec_mod", "ecravo_pfixed_sec_mod","ecravo_apfixed_sec_mod","ecravo_aonly_sec_mod","ecravo_ponly_sec_mod","ecravo_mod_sec_mod",
                              "ecranx_allfree_avo_mod", "ecranx_afixed_avo_mod", "ecranx_pfixed_avo_mod","ecranx_apfixed_avo_mod","ecranx_aonly_avo_mod","ecranx_ponly_avo_mod","ecranx_mod_avo_mod",
                              "ecranx_allfree_anx_mod", "ecranx_afixed_anx_mod", "ecranx_pfixed_anx_mod","ecranx_apfixed_anx_mod","ecranx_aonly_anx_mod","ecranx_ponly_anx_mod",  "ecranx_mod_anx_mod",
                              "ecranx_allfree_sec_mod", "ecranx_afixed_sec_mod", "ecranx_pfixed_sec_mod","ecranx_apfixed_sec_mod","ecranx_aonly_sec_mod","ecranx_ponly_sec_mod", "ecranx_mod_sec_mod")

# Initialize Models -------------------------------------------------------

ecravo_allfree_avo_mod_m <-  ecravo_afixed_avo_mod_m <-  ecravo_pfixed_avo_mod_m <- ecravo_apfixed_avo_mod_m <- ecravo_aonly_avo_mod_m <- ecravo_ponly_avo_mod_m <-  ecravo_mod_avo_mod_m <- ""
ecravo_allfree_avo_mod <- ecravo_afixed_avo_mod <- ecravo_pfixed_avo_mod <- ecravo_apfixed_avo_mod <- ecravo_aonly_avo_mod <- ecravo_ponly_avo_mod <-  ecravo_mod_avo_mod <- ""

ecravo_allfree_anx_mod_m <- ecravo_afixed_anx_mod_m  <- ecravo_pfixed_anx_mod_m <- ecravo_apfixed_anx_mod_m <- ecravo_aonly_anx_mod_m <- ecravo_ponly_anx_mod_m <-  ecravo_mod_anx_mod_m <- ""
ecravo_allfree_anx_mod <- ecravo_afixed_anx_mod <- ecravo_pfixed_anx_mod <- ecravo_apfixed_anx_mod <- ecravo_aonly_anx_mod<- ecravo_ponly_anx_mod <- ecravo_mod_anx_mod <- ""

ecravo_allfree_sec_mod_m <- ecravo_afixed_sec_mod_m <- ecravo_pfixed_sec_mod_m <- ecravo_apfixed_sec_mod_m <- ecravo_aonly_sec_mod_m <- ecravo_ponly_sec_mod_m <- ecravo_mod_sec_mod_m <- ""
ecravo_allfree_sec_mod <- ecravo_afixed_sec_mod <- ecravo_pfixed_sec_mod <- ecravo_apfixed_sec_mod <- ecravo_aonly_sec_mod <- ecravo_ponly_sec_mod <-  ecravo_mod_sec_mod <- ""

ecranx_allfree_avo_mod_m <- ecranx_afixed_avo_mod_m <- ecranx_pfixed_avo_mod_m <- ecranx_apfixed_avo_mod_m <- ecranx_aonly_avo_mod_m <- ecranx_ponly_avo_mod_m <- ecranx_mod_avo_mod_m <- ""
ecranx_allfree_avo_mod <- ecranx_afixed_avo_mod <- ecranx_pfixed_avo_mod <- ecranx_apfixed_avo_mod <- ecranx_aonly_avo_mod <- ecranx_ponly_avo_mod <-  ecranx_mod_avo_mod <- ""

ecranx_allfree_anx_mod_m <- ecranx_afixed_anx_mod_m <-  ecranx_pfixed_anx_mod_m <- ecranx_apfixed_anx_mod_m <- ecranx_aonly_anx_mod_m <- ecranx_ponly_anx_mod_m <- ecranx_mod_anx_mod_m <- ""
ecranx_allfree_anx_mod <- ecranx_afixed_anx_mod <- ecranx_pfixed_anx_mod <- ecranx_apfixed_anx_mod <- ecranx_aonly_anx_mod <- ecranx_ponly_anx_mod <- ecranx_mod_anx_mod <- ""

ecranx_allfree_sec_mod_m <- ecranx_afixed_sec_mod_m <- ecranx_pfixed_sec_mod_m <- ecranx_apfixed_sec_mod_m <- ecranx_aonly_sec_mod_m <- ecranx_ponly_sec_mod_m <- ecranx_mod_sec_mod_m <- ""
ecranx_allfree_sec_mod <- ecranx_afixed_sec_mod <- ecranx_pfixed_sec_mod <- ecranx_apfixed_sec_mod <- ecranx_aonly_sec_mod <- ecranx_ponly_sec_mod <- ecranx_mod_sec_mod <- ""




# AAR avo gating S2 posint anx -----------------------------------------



ecravo_allfree_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_allfree_anx_mod_m <- sem(model = ecravo_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_afixed_anx_mod_m <- sem(model = ecravo_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_pfixed_anx_mod_m <- sem(model = ecravo_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_apfixed_anx_mod_m <- sem(model = ecravo_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_aonly_anx_mod_m <- sem(model = ecravo_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_ponly_anx_mod_m <- sem(model = ecravo_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m)


ecravo_mod_anx_mod <- "
p_ccpt ~ b*panx1 #a
p_ccpt ~ a*pranx1 #a
p_ccpr ~ b*panx0 # a
p_ccpr ~ a*pranx0 #a


pranx1 ~~ pranx0
panx1~~panx0
p_ccpt ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

 ECRavo_1 ~~ 0*av1pan1 
ECRavo_0 ~~ 0*av1pan1 
ECRavo_1 ~~ 0*av1pan0 
ECRavo_0 ~~ 0*av1pan0 
ECRavo_1 ~~ 0*av0pan1 
ECRavo_0 ~~ 0*av0pan1 
ECRavo_1 ~~ 0*av0pan0 
ECRavo_0 ~~ 0*av0pan0
av1pan1 ~~ av1pan1 
av1pan0 ~~ av1pan0 
av0pan1 ~~ av0pan1 
av0pan0 ~~ av0pan0
panx1 ~~ 0*av1pan1
panx1 ~~ 0*av1pan0
panx1 ~~ 0*av0pan1
panx1 ~~ 0*av0pan0
panx0 ~~ 0*av1pan1
panx0 ~~ 0*av1pan0
panx0 ~~ 0*av0pan1
panx0 ~~ 0*av0pan0 
p_ccpt ~ l*ECRavo_1

p_ccpt ~ ECRavo_0

p_ccpr ~ l*ECRavo_1

p_ccpr ~ l*ECRavo_0 
p_ccpt ~ l*av1pan1

p_ccpt ~ l*av0pan1

p_ccpr ~ 0*av1pan0

p_ccpr ~ a*av0pan0 
av0pan1 ~~ av0pan0
"
ecravo_mod_anx_mod_m <- sem(model = ecravo_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
# Avoidance gates effect of increases of anxiety on patient and partner coupling. Increases in anxiety predict more dependency during the positive interaction in both patient and partner.This is amplified in patients by their own avoidance and their partner's avoidance. In partner's, this effect is attenuated if the partner is lower in avoidance.
anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)




# AAR anx gating S2 posint anx -----------------------------------------

ecranx_allfree_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod), "\n") #,"\nan0pan1 ~~ an0pan0","\nan1pan1 ~~ an1pan0","\nan1pan1 ~~ an0pan0", "\nECRanx_0 ~~ panx0", "\n")
ecranx_allfree_anx_mod_m <- sem(model = ecranx_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_afixed_anx_mod_m <- sem(model = ecranx_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_pfixed_anx_mod_m <- sem(model = ecranx_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_apfixed_anx_mod_m <- sem(model = ecranx_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_aonly_anx_mod_m <- sem(model = ecranx_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_ponly_anx_mod_m <- sem(model = ecranx_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m)

ecranx_mod_anx_mod <-  "
p_ccpt ~ b*panx1 #a
p_ccpt ~ a*pranx1 #a
p_ccpr ~ b*panx0 # a
p_ccpr ~ a*pranx0 #a


pranx1 ~~ pranx0
panx1~~panx0
p_ccpt ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

 ECRanx_1 ~~ 0*an1pan1 
ECRanx_0 ~~ 0*an1pan1 
ECRanx_1 ~~ 0*an1pan0 
ECRanx_0 ~~ 0*an1pan0 
ECRanx_1 ~~ 0*an0pan1 
ECRanx_0 ~~ 0*an0pan1 
ECRanx_1 ~~ 0*an0pan0 
ECRanx_0 ~~ 0*an0pan0
an1pan1 ~~ an1pan1 
an1pan0 ~~ an1pan0 
an0pan1 ~~ an0pan1 
an0pan0 ~~ an0pan0
panx1 ~~ 0*an1pan1
panx1 ~~ 0*an1pan0
panx1 ~~ 0*an0pan1
panx1 ~~ 0*an0pan0
panx0 ~~ 0*an1pan1
panx0 ~~ 0*an1pan0
panx0 ~~ 0*an0pan1
panx0 ~~ 0*an0pan0 
p_ccpt ~ ECRanx_1

p_ccpt ~ 0*ECRanx_0

p_ccpr ~ ECRanx_1

p_ccpr ~ 0*ECRanx_0 
p_ccpt ~ 0*an1pan1

p_ccpt ~ 0*an0pan1

p_ccpr ~ an1pan0

p_ccpr ~ 0*an0pan0 

"
ecranx_mod_anx_mod_m <- sem(model = ecranx_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
# Patient's anxious attachment style gates the extent to which increases in anxiety couple with dependency during the positive interaction for the partner. It weakens this association. 
anova(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)



# S2 AAR Posint Save Models  ----------------------------------------------
avav_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m)
avav_syn_model_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod)
df <- mapply(pull_fitmeasures, avav_model_list, avav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_p_aar_avo_saam_avo"
fulldf <- df

avan_model_list <- c(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)
avan_syn_model_list <- c(ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod, ecravo_mod_anx_mod)
df <- mapply(pull_fitmeasures, avan_model_list, avan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_p_aar_avo_saam_anx"
fulldf <- bind_rows(fulldf, df)

avse_model_list <- c(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m)
avse_syn_model_list <- c(ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod, ecravo_mod_sec_mod)
df <- mapply(pull_fitmeasures, avse_model_list, avse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_p_aar_avo_saam_sec"
fulldf <- bind_rows(fulldf, df)


anav_model_list <- c(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m, ecranx_mod_avo_mod_m)
anav_syn_model_list <- c(ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod, ecranx_mod_avo_mod)
df <- mapply(pull_fitmeasures, anav_model_list, anav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_p_aar_anx_saam_avo"
fulldf <- bind_rows(fulldf, df)

anan_model_list <- c(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)
anan_syn_model_list <- c(ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod, ecranx_mod_anx_mod)
df <- mapply(pull_fitmeasures, anan_model_list, anan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_p_aar_anx_saam_anx"
fulldf <- bind_rows(fulldf, df)

anse_model_list <- c(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
anse_syn_model_list <- c(ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
df <- mapply(pull_fitmeasures, anse_model_list, anse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_p_aar_anx_saam_sec"
fulldf <- bind_rows(fulldf, df)


s2_p_aar_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m,
                         ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m,ecravo_mod_anx_mod_m,
                         ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m,ecravo_mod_sec_mod_m, 
                         ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m,ecranx_mod_avo_mod_m,
                         ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m,  ecranx_mod_anx_mod_m,
                         ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
s2_p_aar_syn_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod,
                       ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod,ecravo_mod_anx_mod,
                       ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod,ecravo_mod_sec_mod,
                       ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod,ecranx_mod_avo_mod,
                       ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod,  ecranx_mod_anx_mod,
                       ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
s2_p_aar_df <- posnegint_formoderation
names(s2_p_aar_model_list) <- c("ecravo_allfree_avo_mod_m", "ecravo_afixed_avo_mod_m", "ecravo_pfixed_avo_mod_m","ecravo_apfixed_avo_mod_m","ecravo_aonly_avo_mod_m","ecravo_ponly_avo_mod_m", "ecravo_mod_avo_mod_m",
                                "ecravo_allfree_anx_mod_m", "ecravo_afixed_anx_mod_m", "ecravo_pfixed_anx_mod_m","ecravo_apfixed_anx_mod_m","ecravo_aonly_anx_mod_m","ecravo_ponly_anx_mod_m","ecravo_mod_anx_mod_m",
                                "ecravo_allfree_sec_mod_m", "ecravo_afixed_sec_mod_m", "ecravo_pfixed_sec_mod_m","ecravo_apfixed_sec_mod_m","ecravo_aonly_sec_mod_m","ecravo_ponly_sec_mod_m","ecravo_mod_sec_mod_m",
                                "ecranx_allfree_avo_mod_m", "ecranx_afixed_avo_mod_m", "ecranx_pfixed_avo_mod_m","ecranx_apfixed_avo_mod_m","ecranx_aonly_avo_mod_m","ecranx_ponly_avo_mod_m","ecranx_mod_avo_mod_m",
                                "ecranx_allfree_anx_mod_m", "ecranx_afixed_anx_mod_m", "ecranx_pfixed_anx_mod_m","ecranx_apfixed_anx_mod_m","ecranx_aonly_anx_mod_m","ecranx_ponly_anx_mod_m",  "ecranx_mod_anx_mod_m",
                                "ecranx_allfree_sec_mod_m", "ecranx_afixed_sec_mod_m", "ecranx_pfixed_sec_mod_m","ecranx_apfixed_sec_mod_m","ecranx_aonly_sec_mod_m","ecranx_ponly_sec_mod_m", "ecranx_mod_sec_mod_m")
names(s2_p_aar_syn_list) <- c("ecravo_allfree_avo_mod", "ecravo_afixed_avo_mod", "ecravo_pfixed_avo_mod","ecravo_apfixed_avo_mod","ecravo_aonly_avo_mod","ecravo_ponly_avo_mod", "ecravo_mod_avo_mod",
                              "ecravo_allfree_anx_mod", "ecravo_afixed_anx_mod", "ecravo_pfixed_anx_mod","ecravo_apfixed_anx_mod","ecravo_aonly_anx_mod","ecravo_ponly_anx_mod","ecravo_mod_anx_mod",
                              "ecravo_allfree_sec_mod", "ecravo_afixed_sec_mod", "ecravo_pfixed_sec_mod","ecravo_apfixed_sec_mod","ecravo_aonly_sec_mod","ecravo_ponly_sec_mod","ecravo_mod_sec_mod",
                              "ecranx_allfree_avo_mod", "ecranx_afixed_avo_mod", "ecranx_pfixed_avo_mod","ecranx_apfixed_avo_mod","ecranx_aonly_avo_mod","ecranx_ponly_avo_mod","ecranx_mod_avo_mod",
                              "ecranx_allfree_anx_mod", "ecranx_afixed_anx_mod", "ecranx_pfixed_anx_mod","ecranx_apfixed_anx_mod","ecranx_aonly_anx_mod","ecranx_ponly_anx_mod",  "ecranx_mod_anx_mod",
                              "ecranx_allfree_sec_mod", "ecranx_afixed_sec_mod", "ecranx_pfixed_sec_mod","ecranx_apfixed_sec_mod","ecranx_aonly_sec_mod","ecranx_ponly_sec_mod", "ecranx_mod_sec_mod")


# Load Data for Anxious Ambivalence and Security (Anx > Anxious Ambivalence; Avo > Security) ---------------------------------------------------------------
posnegint_personality <- read.csv("../data/posnegint_aapr_anxamb.csv")



# Wrangle data for moderation analyses ------------------------------------
center <- function(x) {
  x = x-mean(x, na.rm = TRUE)
  return(x)
}
posnegint_formoderation <- dplyr::select(posnegint_personality, PTNUM, starts_with("p_"), scpt, ccpt, scpr, ccpr, starts_with("ECR"), starts_with("pra"), starts_with("pa"), starts_with("prs"), starts_with("ps")) %>% mutate_at(vars(c(ccpt, ccpr, starts_with("ECR"), starts_with("pra"), starts_with("pa"), starts_with("prs"), starts_with("ps"))), funs(center)) %>% mutate(an0pran0 = ECRanx_0*pranx0, an0pan0 = ECRanx_0*panx0, 
                                                                                                                                                                                                                                                                                                                                                                                  an1pran1 = ECRanx_1*pranx1, an1pan1 = ECRanx_1*panx1,
                                                                                                                                                                                                                                                                                                                                                                                  an0prav0 = ECRanx_0*pravo0, an0pav0 = ECRanx_0*pavo0, 
                                                                                                                                                                                                                                                                                                                                                                                  an1prav1 = ECRanx_1*pravo1, an1pav1 = ECRanx_1*pavo1,
                                                                                                                                                                                                                                                                                                                                                                                  an0prse0 = ECRanx_0*prsec0, an0pan0 = ECRanx_0*psec0, 
                                                                                                                                                                                                                                                                                                                                                                                  an1prse1 = ECRanx_1*prsec1, an1pan1 = ECRanx_1*psec1,
                                                                                                                                                                                                                                                                                                                                                                                  av0pran0 = ECRavo_0*pranx0, av0pan0 = ECRavo_0*panx0, 
                                                                                                                                                                                                                                                                                                                                                                                  av1pran1 = ECRavo_1*pranx1, av1pan1 = ECRavo_1*panx1,
                                                                                                                                                                                                                                                                                                                                                                                  av0prav0 = ECRavo_0*pravo0, av0pav0 = ECRavo_0*pavo0, 
                                                                                                                                                                                                                                                                                                                                                                                  av1prav1 = ECRavo_1*pravo1, av1pav1 = ECRavo_1*pavo1,
                                                                                                                                                                                                                                                                                                                                                                                  av0prse0 = ECRavo_0*prsec0, av0pan0 = ECRavo_0*psec0, 
                                                                                                                                                                                                                                                                                                                                                                                  av1prse1 = ECRavo_1*prsec1, av1pan1 = ECRavo_1*psec1,
                                                                                                                                                                                                                                                                                                                                                                                  an0ccpr0 = ECRanx_0*ccpr, an1ccpt1 = ECRanx_1*ccpt,
                                                                                                                                                                                                                                                                                                                                                                                  av0ccpr0 = ECRavo_0*ccpr, av1ccpt1 = ECRanx_1*ccpt,
                                                                                                                                                                                                                                                                                                                                                                                  an1ccpr0 = ECRanx_1*ccpr, an0ccpt1 = ECRanx_0*ccpt,
                                                                                                                                                                                                                                                                                                                                                                                  av1ccpr0 = ECRavo_1*ccpr, av0ccpt1 = ECRanx_0*ccpt,
                                                                                                                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                                                                                                                  #partner
                                                                                                                                                                                                                                                                                                                                                                                  an1pran0 = ECRanx_1*pranx0, an1pan0 = ECRanx_1*panx0, 
                                                                                                                                                                                                                                                                                                                                                                                  an0pran1 = ECRanx_0*pranx1, an0pan1 = ECRanx_0*panx1,
                                                                                                                                                                                                                                                                                                                                                                                  an1prav0 = ECRanx_1*pravo0, an1pav0 = ECRanx_1*pavo0, 
                                                                                                                                                                                                                                                                                                                                                                                  an0prav1 = ECRanx_0*pravo1, an0pav1 = ECRanx_0*pavo1,
                                                                                                                                                                                                                                                                                                                                                                                  an1prse0 = ECRanx_1*prsec0, an1pan0 = ECRanx_1*psec0, 
                                                                                                                                                                                                                                                                                                                                                                                  an0prse1 = ECRanx_0*prsec1, an0pan1 = ECRanx_0*psec1,
                                                                                                                                                                                                                                                                                                                                                                                  av1pran0 = ECRavo_1*pranx0, av1pan0 = ECRavo_1*panx0, 
                                                                                                                                                                                                                                                                                                                                                                                  av0pran1 = ECRavo_0*pranx1, av0pan1 = ECRavo_0*panx1,
                                                                                                                                                                                                                                                                                                                                                                                  av1prav0 = ECRavo_1*pravo0, av1pav0 = ECRavo_1*pavo0, 
                                                                                                                                                                                                                                                                                                                                                                                  av0prav1 = ECRavo_0*pravo1, av0pav1 = ECRavo_0*pavo1,
                                                                                                                                                                                                                                                                                                                                                                                  av1prse0 = ECRavo_1*prsec0, av1pan0 = ECRavo_1*psec0, 
                                                                                                                                                                                                                                                                                                                                                                                  av0prse1 = ECRavo_0*prsec1, av0pan1 = ECRavo_0*psec1)


#rescaling for all the models
posnegint_formoderation <- dplyr::mutate_at(posnegint_formoderation, vars(starts_with("p_")), list(~100*.)) %>% dplyr::mutate_at(vars(starts_with("ECR")), list(~10*.))





# Initialize Models -------------------------------------------------------

ecravo_allfree_avo_mod_m <-  ecravo_afixed_avo_mod_m <-  ecravo_pfixed_avo_mod_m <- ecravo_apfixed_avo_mod_m <- ecravo_aonly_avo_mod_m <- ecravo_ponly_avo_mod_m <-  ecravo_mod_avo_mod_m <- ""
ecravo_allfree_avo_mod <- ecravo_afixed_avo_mod <- ecravo_pfixed_avo_mod <- ecravo_apfixed_avo_mod <- ecravo_aonly_avo_mod <- ecravo_ponly_avo_mod <-  ecravo_mod_avo_mod <- ""

ecravo_allfree_anx_mod_m <- ecravo_afixed_anx_mod_m  <- ecravo_pfixed_anx_mod_m <- ecravo_apfixed_anx_mod_m <- ecravo_aonly_anx_mod_m <- ecravo_ponly_anx_mod_m <-  ecravo_mod_anx_mod_m <- ""
ecravo_allfree_anx_mod <- ecravo_afixed_anx_mod <- ecravo_pfixed_anx_mod <- ecravo_apfixed_anx_mod <- ecravo_aonly_anx_mod<- ecravo_ponly_anx_mod <- ecravo_mod_anx_mod <- ""

ecravo_allfree_sec_mod_m <- ecravo_afixed_sec_mod_m <- ecravo_pfixed_sec_mod_m <- ecravo_apfixed_sec_mod_m <- ecravo_aonly_sec_mod_m <- ecravo_ponly_sec_mod_m <- ecravo_mod_sec_mod_m <- ""
ecravo_allfree_sec_mod <- ecravo_afixed_sec_mod <- ecravo_pfixed_sec_mod <- ecravo_apfixed_sec_mod <- ecravo_aonly_sec_mod <- ecravo_ponly_sec_mod <-  ecravo_mod_sec_mod <- ""

ecranx_allfree_avo_mod_m <- ecranx_afixed_avo_mod_m <- ecranx_pfixed_avo_mod_m <- ecranx_apfixed_avo_mod_m <- ecranx_aonly_avo_mod_m <- ecranx_ponly_avo_mod_m <- ecranx_mod_avo_mod_m <- ""
ecranx_allfree_avo_mod <- ecranx_afixed_avo_mod <- ecranx_pfixed_avo_mod <- ecranx_apfixed_avo_mod <- ecranx_aonly_avo_mod <- ecranx_ponly_avo_mod <-  ecranx_mod_avo_mod <- ""

ecranx_allfree_anx_mod_m <- ecranx_afixed_anx_mod_m <-  ecranx_pfixed_anx_mod_m <- ecranx_apfixed_anx_mod_m <- ecranx_aonly_anx_mod_m <- ecranx_ponly_anx_mod_m <- ecranx_mod_anx_mod_m <- ""
ecranx_allfree_anx_mod <- ecranx_afixed_anx_mod <- ecranx_pfixed_anx_mod <- ecranx_apfixed_anx_mod <- ecranx_aonly_anx_mod <- ecranx_ponly_anx_mod <- ecranx_mod_anx_mod <- ""

ecranx_allfree_sec_mod_m <- ecranx_afixed_sec_mod_m <- ecranx_pfixed_sec_mod_m <- ecranx_apfixed_sec_mod_m <- ecranx_aonly_sec_mod_m <- ecranx_ponly_sec_mod_m <- ecranx_mod_sec_mod_m <- ""
ecranx_allfree_sec_mod <- ecranx_afixed_sec_mod <- ecranx_pfixed_sec_mod <- ecranx_apfixed_sec_mod <- ecranx_aonly_sec_mod <- ecranx_ponly_sec_mod <- ecranx_mod_sec_mod <- ""


# AAR sec gating S1 negint avo ---------------------------------------------

ecravo_allfree_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_allfree_avo_mod_m <- sem(model = ecravo_allfree_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_afixed_avo_mod_m <- sem(model = ecravo_afixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_pfixed_avo_mod_m <- sem(model = ecravo_pfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_apfixed_avo_mod_m <- sem(model = ecravo_apfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_aonly_avo_mod_m <- sem(model = ecravo_aonly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_ponly_avo_mod_m <- sem(model = ecravo_ponly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m)

ecravo_mod_avo_mod<- "

#paths that are not particularly strong
pavo0 ~ 0*ccpr
ccpr ~ 0*pravo0

#strong paths
pavo1 ~  a*ccpt
ccpt ~ c*pravo1 
ccpr ~ c*pravo1
pravo1 ~~ pravo0
pavo1~~pavo0
ccpt ~~ ccpr
pavo1 ~ pravo1
pavo0 ~pravo0
ac:=a*c

 ECRavo_1 ~~ 0*av1prav1 
ECRavo_0 ~~ 0*av1prav1 
ECRavo_1 ~~ 0*av0prav1 
ECRavo_0 ~~ 0*av0prav1 
av1prav1 ~~ av1prav1 
av0prav1 ~~ av0prav1 
pravo1 ~~ 0*av1prav1
pravo1 ~~ 0*av0prav1
pravo0 ~~ 0*av1prav1
pravo0 ~~ 0*av0prav1 
ccpt ~ 0*ECRavo_1

ccpt ~ h*ECRavo_0

ccpr ~ i*ECRavo_1

ccpr ~ 0*ECRavo_0 
ccpt ~ i*av1prav1

ccpt ~ l*av0prav1

ccpr ~ l*av1prav1

ccpr ~ h*av0prav1 

"
ecravo_mod_avo_mod_m <- sem(model = ecravo_mod_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m) 


# AAR avo gating S1 negint anx -----------------------------------------
ecravo_allfree_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_allfree_anx_mod_m <- sem(model = ecravo_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_afixed_anx_mod_m <- sem(model = ecravo_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_pfixed_anx_mod_m <- sem(model = ecravo_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_apfixed_anx_mod_m <- sem(model = ecravo_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_aonly_anx_mod_m <- sem(model = ecravo_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_ponly_anx_mod_m <- sem(model = ecravo_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m) 

ecravo_mod_anx_mod <- "

panx1 ~  f*ccpt
panx0 ~ ccpt
panx0 ~ f*ccpr

ccpt ~ h*pranx1 
ccpt ~ d*pranx0
ccpr ~ d*pranx1
ccpr ~ h*pranx0
pranx1 ~~ pranx0
panx1~~panx0
ccpt ~~ ccpr

panx1~pranx1
panx0 ~ pranx0
 ECRavo_1 ~~ 0*av1pran1 
ECRavo_0 ~~ 0*av1pran1 
ECRavo_1 ~~ 0*av1pran0 
ECRavo_0 ~~ 0*av1pran0 
ECRavo_1 ~~ 0*av0pran1 
ECRavo_0 ~~ 0*av0pran1 
ECRavo_1 ~~ 0*av0pran0 
ECRavo_0 ~~ 0*av0pran0
av1pran1 ~~ av1pran1 
av1pran0 ~~ av1pran0 
av0pran1 ~~ av0pran1 
av0pran0 ~~ av0pran0
pranx1 ~~ 0*av1pran1
pranx1 ~~ 0*av1pran0
pranx1 ~~ 0*av0pran1
pranx1 ~~ 0*av0pran0
pranx0 ~~ 0*av1pran1
pranx0 ~~ 0*av1pran0
pranx0 ~~ 0*av0pran1
pranx0 ~~ 0*av0pran0 
ccpt ~ 0*ECRavo_1

ccpt ~ l*ECRavo_0

ccpr ~ l*ECRavo_1

ccpr ~ 0*ECRavo_0 
ccpt ~ l*av1pran1

ccpt ~ 0*av1pran0

ccpt ~ l*av0pran1

ccpt ~ v*av0pran0

ccpr ~ v*av1pran1

ccpr ~ 0*av1pran0

ccpr ~ l*av0pran1

ccpr ~ 0*av0pran0 
"

ecravo_mod_anx_mod_m <- sem(model = ecravo_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)

# AAR sec gating S1 negint sec --------------------------------------------

ecravo_allfree_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_allfree_sec_mod_m <- sem(model = ecravo_allfree_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_afixed_sec_mod_m <- sem(model = ecravo_afixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_pfixed_sec_mod_m <- sem(model = ecravo_pfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_apfixed_sec_mod_m <- sem(model = ecravo_apfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_aonly_sec_mod_m <- sem(model = ecravo_aonly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_ponly_sec_mod_m <- sem(model = ecravo_ponly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m) 

ecravo_mod_sec_mod <- "

psec1 ~  f*ccpt
psec1 ~ 0*ccpr
psec0 ~ b*ccpt
psec0 ~ 0*ccpr
ccpt ~ 0*prsec1 
ccpt ~ 0*prsec0
ccpr ~ h*prsec1
ccpr ~ 0*prsec0
prsec1 ~~ prsec0
psec1~~psec0
ccpt ~~ ccpr


psec1~prsec1
psec0 ~ prsec0
 ECRavo_1 ~~ 0*av1prse1 
ECRavo_0 ~~ 0*av1prse1 
ECRavo_1 ~~ 0*av0prse1 
ECRavo_0 ~~ 0*av0prse1 
av1prse1 ~~ av1prse1 
av0prse1 ~~ av0prse1 
prsec1 ~~ 0*av1prse1
prsec1 ~~ 0*av0prse1
prsec0 ~~ 0*av1prse1
prsec0 ~~ 0*av0prse1 
ccpr ~ 0*ECRavo_1

ccpr ~ h*ECRavo_0 
ccpr ~ h*av1prse1

ccpr ~ b*av0prse1 
"
ecravo_mod_sec_mod_m <- sem(model = ecravo_mod_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m) 

# AAR anx/amb gating S1 negint avo ---------------------------------------------

ecranx_allfree_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecranx_allfree_avo_mod_m <- sem(model = ecranx_allfree_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecranx_afixed_avo_mod_m <- sem(model = ecranx_afixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecranx_pfixed_avo_mod_m <- sem(model = ecranx_pfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecranx_apfixed_avo_mod_m <- sem(model = ecranx_apfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecranx_aonly_avo_mod_m <- sem(model = ecranx_aonly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecranx_ponly_avo_mod_m <- sem(model = ecranx_ponly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m) 


ecranx_mod_avo_mod <- "
#paths that are not particularly strong
pavo0 ~ 0*ccpr
ccpr ~ 0*pravo0

#strong paths
pavo1 ~  a*ccpt
ccpt ~ c*pravo1 
ccpr ~ c*pravo1
pravo1 ~~ pravo0
pavo1~~pavo0
ccpt ~~ ccpr
pavo1 ~ pravo1
pavo0 ~pravo0
ac:=a*c

 ECRanx_1 ~~ 0*an1prav1 
ECRanx_0 ~~ 0*an1prav1 
ECRanx_1 ~~ 0*an0prav1 
ECRanx_0 ~~ 0*an0prav1 
an1prav1 ~~ an1prav1 
an0prav1 ~~ an0prav1 
pravo1 ~~ 0*an1prav1
pravo1 ~~ 0*an0prav1
pravo0 ~~ 0*an1prav1
pravo0 ~~ 0*an0prav1 
ccpt ~ l*ECRanx_1

ccpt ~ v*ECRanx_0

ccpr ~ v*ECRanx_1

ccpr ~ l*ECRanx_0 
ccpt ~ l*an1prav1

ccpt ~ l*an0prav1

ccpr ~ v*an1prav1

ccpr ~ v*an0prav1 
"
ecranx_mod_avo_mod_m <- sem(model = ecranx_mod_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
# Typically patient avoidance evokes contrarianim in both patient and partner. This amplified for the patient if patient experiences broader attachment anxiety. This effect is weakened  for partner if patient is high in braoder attachment anxiety. 
anova(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m, ecranx_mod_avo_mod_m)

# AAR anx gating S1 negint anx -----------------------------------------

ecranx_allfree_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_allfree_anx_mod_m <- sem(model = ecranx_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_afixed_anx_mod_m <- sem(model = ecranx_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_pfixed_anx_mod_m <- sem(model = ecranx_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_apfixed_anx_mod_m <- sem(model = ecranx_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_aonly_anx_mod_m <- sem(model = ecranx_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_ponly_anx_mod_m <- sem(model = ecranx_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m)

ecranx_mod_anx_mod <-"
panx1 ~  f*ccpt
panx0 ~ ccpt
panx0 ~ f*ccpr

ccpt ~ h*pranx1 
ccpt ~ d*pranx0
ccpr ~ d*pranx1
ccpr ~ h*pranx0
pranx1 ~~ pranx0
panx1~~panx0
ccpt ~~ ccpr

panx1~pranx1
panx0 ~ pranx0
 ECRanx_1 ~~ 0*an1pran1 
ECRanx_0 ~~ 0*an1pran1 
ECRanx_1 ~~ 0*an1pran0 
ECRanx_0 ~~ 0*an1pran0 
ECRanx_1 ~~ 0*an0pran1 
ECRanx_0 ~~ 0*an0pran1 
ECRanx_1 ~~ 0*an0pran0 
ECRanx_0 ~~ 0*an0pran0
an1pran1 ~~ an1pran1 
an1pran0 ~~ an1pran0 
an0pran1 ~~ an0pran1 
an0pran0 ~~ an0pran0
pranx1 ~~ 0*an1pran1
pranx1 ~~ 0*an1pran0
pranx1 ~~ 0*an0pran1
pranx1 ~~ 0*an0pran0
pranx0 ~~ 0*an1pran1
pranx0 ~~ 0*an1pran0
pranx0 ~~ 0*an0pran1
pranx0 ~~ 0*an0pran0 
ccpt ~ v*ECRanx_1

ccpt ~ j*ECRanx_0

ccpr ~ 0*ECRanx_1

ccpr ~ v*ECRanx_0 
ccpt ~ 0*an1pran1

ccpt ~ j*an1pran0

ccpt ~ 0*an0pran1

ccpt ~ 0*an0pran0

ccpr ~ 0*an1pran1

ccpr ~ 0*an1pran0

ccpr ~ 0*an0pran1

ccpr ~ 0*an0pran0 
"
ecranx_mod_anx_mod_m <- sem(model = ecranx_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)


# AAR anx gating S1 negint sec --------------------------------------------


ecranx_allfree_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_allfree_sec_mod_m <- sem(model = ecranx_allfree_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_afixed_sec_mod_m <- sem(model = ecranx_afixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_pfixed_sec_mod_m <- sem(model = ecranx_pfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_apfixed_sec_mod_m <- sem(model = ecranx_apfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_aonly_sec_mod_m <- sem(model = ecranx_aonly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_ponly_sec_mod_m <- sem(model = ecranx_ponly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m)

ecranx_mod_sec_mod_m <-ecranx_allfree_sec_mod_m
ecranx_mod_sec_mod<-ecranx_allfree_sec_mod
# S1 AAR Negint Save Models  ----------------------------------------------
avav_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m)
avav_syn_model_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod)
df <- mapply(pull_fitmeasures, avav_model_list, avav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_n_aar2_avo_saam_avo"
fulldf <- df

avan_model_list <- c(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)
avan_syn_model_list <- c(ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod, ecravo_mod_anx_mod)
df <- mapply(pull_fitmeasures, avan_model_list, avan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_n_aar2_avo_saam_anx"
fulldf <- bind_rows(fulldf, df)

avse_model_list <- c(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m)
avse_syn_model_list <- c(ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod, ecravo_mod_sec_mod)
df <- mapply(pull_fitmeasures, avse_model_list, avse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_n_aar2_avo_saam_sec"
fulldf <- bind_rows(fulldf, df)


anav_model_list <- c(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m, ecranx_mod_avo_mod_m)
anav_syn_model_list <- c(ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod, ecranx_mod_avo_mod)
df <- mapply(pull_fitmeasures, anav_model_list, anav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_n_aar2_anx_saam_avo"
fulldf <- bind_rows(fulldf, df)

anan_model_list <- c(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)
anan_syn_model_list <- c(ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod, ecranx_mod_anx_mod)
df <- mapply(pull_fitmeasures, anan_model_list, anan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_n_aar2_anx_saam_anx"
fulldf <- bind_rows(fulldf, df)

anse_model_list <- c(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
anse_syn_model_list <- c(ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
df <- mapply(pull_fitmeasures, anse_model_list, anse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_n_aar2_anx_saam_sec"
fulldf <- bind_rows(fulldf, df)


s1_n_aar2_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m,
                         ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m,ecravo_mod_anx_mod_m,
                         ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m,ecravo_mod_sec_mod_m, 
                         ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m,ecranx_mod_avo_mod_m,
                         ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m,  ecranx_mod_anx_mod_m,
                         ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
s1_n_aar2_syn_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod,
                       ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod,ecravo_mod_anx_mod,
                       ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod,ecravo_mod_sec_mod,
                       ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod,ecranx_mod_avo_mod,
                       ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod,  ecranx_mod_anx_mod,
                       ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
s1_n_aar2_df <- posnegint_formoderation
names(s1_n_aar2_model_list) <- c("ecravo_allfree_avo_mod_m", "ecravo_afixed_avo_mod_m", "ecravo_pfixed_avo_mod_m","ecravo_apfixed_avo_mod_m","ecravo_aonly_avo_mod_m","ecravo_ponly_avo_mod_m", "ecravo_mod_avo_mod_m",
                                "ecravo_allfree_anx_mod_m", "ecravo_afixed_anx_mod_m", "ecravo_pfixed_anx_mod_m","ecravo_apfixed_anx_mod_m","ecravo_aonly_anx_mod_m","ecravo_ponly_anx_mod_m","ecravo_mod_anx_mod_m",
                                "ecravo_allfree_sec_mod_m", "ecravo_afixed_sec_mod_m", "ecravo_pfixed_sec_mod_m","ecravo_apfixed_sec_mod_m","ecravo_aonly_sec_mod_m","ecravo_ponly_sec_mod_m","ecravo_mod_sec_mod_m",
                                "ecranx_allfree_avo_mod_m", "ecranx_afixed_avo_mod_m", "ecranx_pfixed_avo_mod_m","ecranx_apfixed_avo_mod_m","ecranx_aonly_avo_mod_m","ecranx_ponly_avo_mod_m","ecranx_mod_avo_mod_m",
                                "ecranx_allfree_anx_mod_m", "ecranx_afixed_anx_mod_m", "ecranx_pfixed_anx_mod_m","ecranx_apfixed_anx_mod_m","ecranx_aonly_anx_mod_m","ecranx_ponly_anx_mod_m",  "ecranx_mod_anx_mod_m",
                                "ecranx_allfree_sec_mod_m", "ecranx_afixed_sec_mod_m", "ecranx_pfixed_sec_mod_m","ecranx_apfixed_sec_mod_m","ecranx_aonly_sec_mod_m","ecranx_ponly_sec_mod_m", "ecranx_mod_sec_mod_m")
names(s1_n_aar2_syn_list) <- c("ecravo_allfree_avo_mod", "ecravo_afixed_avo_mod", "ecravo_pfixed_avo_mod","ecravo_apfixed_avo_mod","ecravo_aonly_avo_mod","ecravo_ponly_avo_mod", "ecravo_mod_avo_mod",
                              "ecravo_allfree_anx_mod", "ecravo_afixed_anx_mod", "ecravo_pfixed_anx_mod","ecravo_apfixed_anx_mod","ecravo_aonly_anx_mod","ecravo_ponly_anx_mod","ecravo_mod_anx_mod",
                              "ecravo_allfree_sec_mod", "ecravo_afixed_sec_mod", "ecravo_pfixed_sec_mod","ecravo_apfixed_sec_mod","ecravo_aonly_sec_mod","ecravo_ponly_sec_mod","ecravo_mod_sec_mod",
                              "ecranx_allfree_avo_mod", "ecranx_afixed_avo_mod", "ecranx_pfixed_avo_mod","ecranx_apfixed_avo_mod","ecranx_aonly_avo_mod","ecranx_ponly_avo_mod","ecranx_mod_avo_mod",
                              "ecranx_allfree_anx_mod", "ecranx_afixed_anx_mod", "ecranx_pfixed_anx_mod","ecranx_apfixed_anx_mod","ecranx_aonly_anx_mod","ecranx_ponly_anx_mod",  "ecranx_mod_anx_mod",
                              "ecranx_allfree_sec_mod", "ecranx_afixed_sec_mod", "ecranx_pfixed_sec_mod","ecranx_apfixed_sec_mod","ecranx_aonly_sec_mod","ecranx_ponly_sec_mod", "ecranx_mod_sec_mod")

# Initialize Models -------------------------------------------------------

ecravo_allfree_avo_mod_m <-  ecravo_afixed_avo_mod_m <-  ecravo_pfixed_avo_mod_m <- ecravo_apfixed_avo_mod_m <- ecravo_aonly_avo_mod_m <- ecravo_ponly_avo_mod_m <-  ecravo_mod_avo_mod_m <- ""
ecravo_allfree_avo_mod <- ecravo_afixed_avo_mod <- ecravo_pfixed_avo_mod <- ecravo_apfixed_avo_mod <- ecravo_aonly_avo_mod <- ecravo_ponly_avo_mod <-  ecravo_mod_avo_mod <- ""

ecravo_allfree_anx_mod_m <- ecravo_afixed_anx_mod_m  <- ecravo_pfixed_anx_mod_m <- ecravo_apfixed_anx_mod_m <- ecravo_aonly_anx_mod_m <- ecravo_ponly_anx_mod_m <-  ecravo_mod_anx_mod_m <- ""
ecravo_allfree_anx_mod <- ecravo_afixed_anx_mod <- ecravo_pfixed_anx_mod <- ecravo_apfixed_anx_mod <- ecravo_aonly_anx_mod<- ecravo_ponly_anx_mod <- ecravo_mod_anx_mod <- ""

ecravo_allfree_sec_mod_m <- ecravo_afixed_sec_mod_m <- ecravo_pfixed_sec_mod_m <- ecravo_apfixed_sec_mod_m <- ecravo_aonly_sec_mod_m <- ecravo_ponly_sec_mod_m <- ecravo_mod_sec_mod_m <- ""
ecravo_allfree_sec_mod <- ecravo_afixed_sec_mod <- ecravo_pfixed_sec_mod <- ecravo_apfixed_sec_mod <- ecravo_aonly_sec_mod <- ecravo_ponly_sec_mod <-  ecravo_mod_sec_mod <- ""

ecranx_allfree_avo_mod_m <- ecranx_afixed_avo_mod_m <- ecranx_pfixed_avo_mod_m <- ecranx_apfixed_avo_mod_m <- ecranx_aonly_avo_mod_m <- ecranx_ponly_avo_mod_m <- ecranx_mod_avo_mod_m <- ""
ecranx_allfree_avo_mod <- ecranx_afixed_avo_mod <- ecranx_pfixed_avo_mod <- ecranx_apfixed_avo_mod <- ecranx_aonly_avo_mod <- ecranx_ponly_avo_mod <-  ecranx_mod_avo_mod <- ""

ecranx_allfree_anx_mod_m <- ecranx_afixed_anx_mod_m <-  ecranx_pfixed_anx_mod_m <- ecranx_apfixed_anx_mod_m <- ecranx_aonly_anx_mod_m <- ecranx_ponly_anx_mod_m <- ecranx_mod_anx_mod_m <- ""
ecranx_allfree_anx_mod <- ecranx_afixed_anx_mod <- ecranx_pfixed_anx_mod <- ecranx_apfixed_anx_mod <- ecranx_aonly_anx_mod <- ecranx_ponly_anx_mod <- ecranx_mod_anx_mod <- ""

ecranx_allfree_sec_mod_m <- ecranx_afixed_sec_mod_m <- ecranx_pfixed_sec_mod_m <- ecranx_apfixed_sec_mod_m <- ecranx_aonly_sec_mod_m <- ecranx_ponly_sec_mod_m <- ecranx_mod_sec_mod_m <- ""
ecranx_allfree_sec_mod <- ecranx_afixed_sec_mod <- ecranx_pfixed_sec_mod <- ecranx_apfixed_sec_mod <- ecranx_aonly_sec_mod <- ecranx_ponly_sec_mod <- ecranx_mod_sec_mod <- ""




# AAR sec gating S2 negint avo ---------------------------------------------
ecravo_allfree_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_allfree_avo_mod_m <- sem(model = ecravo_allfree_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_afixed_avo_mod_m <- sem(model = ecravo_afixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_pfixed_avo_mod_m <- sem(model = ecravo_pfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_apfixed_avo_mod_m <- sem(model = ecravo_apfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_aonly_avo_mod_m <- sem(model = ecravo_aonly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_avo_mod <- paste(negint_avo_mod, ECR_moderation("avo","avo", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_avo_mod),"\n")
ecravo_ponly_avo_mod_m <- sem(model = ecravo_ponly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_mod_avo_mod <-"
#paths that are not particularly strong
pavo0 ~ 0*ccpr
ccpr ~ 0*pravo0

#strong paths
pavo1 ~  a*ccpt
ccpt ~ c*pravo1 
ccpr ~ c*pravo1
pravo1 ~~ pravo0
pavo1~~pavo0
ccpt ~~ ccpr
pavo1 ~ pravo1
pavo0 ~pravo0
ac:=a*c

 ECRavo_1 ~~ 0*av1ccpt1 
ECRavo_0 ~~ 0*av1ccpt1 
ECRavo_1 ~~ 0*av0ccpt1 
ECRavo_0 ~~ 0*av0ccpt1 
av1ccpt1 ~~ av1ccpt1 
av0ccpt1 ~~ av0ccpt1 
av1ccpt1 ~~ 0*ccpt 
av1ccpt1 ~~ 0*ccpr 
av0ccpt1 ~~ 0*ccpt 
av0ccpt1 ~~ 0*ccpr  
pavo1 ~ 0*ECRavo_1

pavo1 ~ 0*ECRavo_0 
pavo1 ~ 0*av1ccpt1

pavo1 ~ av0ccpt1 
"
ecravo_mod_avo_mod_m <- sem(model = ecravo_mod_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
ecravo_mod_avo_mod_m <- ecravo_allfree_mod_avo_mod_m
ecravo_mod_avo_mod <- ecravo_allfree_mod_avo_mod
anova(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m) 

# No sign of s2 moderation for AAR avo on avo process

# AAR avo gating S2 negint anx -----------------------------------------



ecravo_allfree_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_allfree_anx_mod_m <- sem(model = ecravo_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_afixed_anx_mod_m <- sem(model = ecravo_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_pfixed_anx_mod_m <- sem(model = ecravo_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_apfixed_anx_mod_m <- sem(model = ecravo_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_aonly_anx_mod_m <- sem(model = ecravo_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_anx_mod <- paste(negint_anx_mod, ECR_moderation("avo","anx", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecravo_ponly_anx_mod_m <- sem(model = ecravo_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m)

ecravo_mod_anx_mod <- "
panx1 ~  f*ccpt
panx0 ~ ccpt
panx0 ~ f*ccpr

ccpt ~ h*pranx1 
ccpt ~ d*pranx0
ccpr ~ d*pranx1
ccpr ~ h*pranx0
pranx1 ~~ pranx0
panx1~~panx0
ccpt ~~ ccpr

panx1~pranx1
panx0 ~ pranx0
 ECRavo_1 ~~ 0*av1ccpt1 
ECRavo_0 ~~ 0*av1ccpt1 
ECRavo_1 ~~ 0*av1ccpr0 
ECRavo_0 ~~ 0*av1ccpr0 
ECRavo_1 ~~ 0*av0ccpt1 
ECRavo_0 ~~ 0*av0ccpt1 
ECRavo_1 ~~ 0*av0ccpr0 
ECRavo_0 ~~ 0*av0ccpr0
av1ccpt1 ~~ av1ccpt1 
av1ccpr0 ~~ av1ccpr0 
av0ccpt1 ~~ av0ccpt1 
av0ccpr0 ~~ av0ccpr0
av1ccpt1 ~~ 0*ccpt 
av1ccpt1 ~~ 0*ccpr 
av1ccpr0 ~~ 0*ccpt 
av1ccpr0 ~~ 0*ccpr 
av0ccpt1 ~~ 0*ccpt 
av0ccpt1 ~~ 0*ccpr 
av0ccpr0 ~~ 0*ccpt 
av0ccpr0 ~~ 0*ccpr 
panx1 ~ 0*ECRavo_1

panx1 ~ 0*ECRavo_0

panx0 ~ 0*ECRavo_1

panx0 ~ j*ECRavo_0 
panx1 ~ av1ccpt1

panx1 ~ 0*av0ccpt1

panx0 ~ j*av1ccpt1

panx0 ~ 0*av1ccpr0

panx0 ~ 0*av0ccpt1

panx0 ~ 0*av0ccpr0 
"
ecravo_mod_anx_mod_m <- sem(model = ecravo_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)

# AAR sec gating S2 negint sec --------------------------------------------

ecravo_allfree_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_allfree_sec_mod_m <- sem(model = ecravo_allfree_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_afixed_sec_mod_m <- sem(model = ecravo_afixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_pfixed_sec_mod_m <- sem(model = ecravo_pfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_apfixed_sec_mod_m <- sem(model = ecravo_apfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_aonly_sec_mod_m <- sem(model = ecravo_aonly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_sec_mod <- paste(negint_sec_mod, ECR_moderation("avo","sec", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecravo_ponly_sec_mod_m <- sem(model = ecravo_ponly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m) 

ecravo_mod_sec_mod_m <- ecravo_allfree_sec_mod_m
ecravo_mod_sec_mod <- ecravo_allfree_sec_mod
# AAR anx/amb gating S2 negint avo ---------------------------------------------


ecranx_allfree_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ),model_syntax =  negint_avo_mod),"\n")
ecranx_allfree_avo_mod_m <- sem(model = ecranx_allfree_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), model_syntax = negint_avo_mod),"\n")
ecranx_afixed_avo_mod_m <- sem(model = ecranx_afixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), model_syntax = negint_avo_mod),"\n")
ecranx_pfixed_avo_mod_m <- sem(model = ecranx_pfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ),model_syntax =  negint_avo_mod),"\n")
ecranx_apfixed_avo_mod_m <- sem(model = ecranx_apfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ),model_syntax =  negint_avo_mod),"\n")
ecranx_aonly_avo_mod_m <- sem(model = ecranx_aonly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_avo_mod <- paste(negint_avo_mod, ECR_moderation("anx","avo", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ),model_syntax =   negint_avo_mod),"\n")
ecranx_ponly_avo_mod_m <- sem(model = ecranx_ponly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m)

ecranx_mod_avo_mod_m <- ecranx_allfree_avo_mod_m
ecranx_mod_avo_mod <- ecranx_allfree_avo_mod
# AAR anx gating S2 negint anx -----------------------------------------

ecranx_allfree_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_allfree_anx_mod_m <- sem(model = ecranx_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_afixed_anx_mod_m <- sem(model = ecranx_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_pfixed_anx_mod_m <- sem(model = ecranx_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_apfixed_anx_mod_m <- sem(model = ecranx_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_aonly_anx_mod_m <- sem(model = ecranx_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_anx_mod <- paste(negint_anx_mod, ECR_moderation("anx","anx", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_anx_mod),"\n")
ecranx_ponly_anx_mod_m <- sem(model = ecranx_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m)

ecranx_mod_anx_mod <- "
panx1 ~  f*ccpt
panx0 ~ ccpt
panx0 ~ f*ccpr

ccpt ~ h*pranx1 
ccpt ~ d*pranx0
ccpr ~ d*pranx1
ccpr ~ h*pranx0
pranx1 ~~ pranx0
panx1~~panx0
ccpt ~~ ccpr

panx1~pranx1
panx0 ~ pranx0
 ECRanx_1 ~~ 0*an1ccpt1 
ECRanx_0 ~~ 0*an1ccpt1 
ECRanx_1 ~~ 0*an1ccpr0 
ECRanx_0 ~~ 0*an1ccpr0 
ECRanx_1 ~~ 0*an0ccpt1 
ECRanx_0 ~~ 0*an0ccpt1 
ECRanx_1 ~~ 0*an0ccpr0 
ECRanx_0 ~~ 0*an0ccpr0
an1ccpt1 ~~ an1ccpt1 
an1ccpr0 ~~ an1ccpr0 
an0ccpt1 ~~ an0ccpt1 
an0ccpr0 ~~ an0ccpr0
an1ccpt1 ~~ 0*ccpt 
an1ccpt1 ~~ 0*ccpr 
an1ccpr0 ~~ 0*ccpt 
an1ccpr0 ~~ 0*ccpr 
an0ccpt1 ~~ 0*ccpt 
an0ccpt1 ~~ 0*ccpr 
an0ccpr0 ~~ 0*ccpt 
an0ccpr0 ~~ 0*ccpr 
panx1 ~ 0*ECRanx_1

panx1 ~ l*ECRanx_0

panx0 ~ l*ECRanx_1

panx0 ~ 0*ECRanx_0 
panx1 ~ l*an1ccpt1

panx1 ~ l*an0ccpt1

panx0 ~ an1ccpt1

panx0 ~ 0*an1ccpr0

panx0 ~ l*an0ccpt1

panx0 ~ 0*an0ccpr0 
"
ecranx_mod_anx_mod_m <- sem(model = ecranx_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
# Mod preferred. Extent to which patients dependency predicts increases in anxiety in patient is gated by patient and partner's broader anxiety whereby this association is weakened or goes the opposite direction when patient or partner is higher in broad anxiety. In partner's, extent to which partner reports increases in anxiety in response to patient's dependency is amplified when patient is high  in anxiety but attenuated when partner is high in attachment anxiety. 
ecranx_mod_anx_mod_m <- ecranx_allfree_anx_mod_m
ecranx_mod_anx_mod <- ecranx_allfree_anx_mod

anova(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)




# AAR anx gating S2 negint sec --------------------------------------------


ecranx_allfree_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_allfree_sec_mod_m <- sem(model = ecranx_allfree_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_afixed_sec_mod_m <- sem(model = ecranx_afixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_pfixed_sec_mod_m <- sem(model = ecranx_pfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_apfixed_sec_mod_m <- sem(model = ecranx_apfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_aonly_sec_mod_m <- sem(model = ecranx_aonly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_sec_mod <- paste(negint_sec_mod, ECR_moderation("anx","sec", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), negint_sec_mod),"\n")
ecranx_ponly_sec_mod_m <- sem(model = ecranx_ponly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


anova(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m)
ecranx_mod_sec_mod_m <- ecranx_allfree_sec_mod_m
ecranx_mod_sec_mod <- ecranx_allfree_sec_mod
# S2 AAR Negint Save Models  ----------------------------------------------
avav_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m)
avav_syn_model_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod)
df <- mapply(pull_fitmeasures, avav_model_list, avav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_n_aar2_avo_saam_avo"
fulldf <- df

avan_model_list <- c(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)
avan_syn_model_list <- c(ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod, ecravo_mod_anx_mod)
df <- mapply(pull_fitmeasures, avan_model_list, avan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_n_aar2_avo_saam_anx"
fulldf <- bind_rows(fulldf, df)

avse_model_list <- c(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m)
avse_syn_model_list <- c(ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod, ecravo_mod_sec_mod)
df <- mapply(pull_fitmeasures, avse_model_list, avse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_n_aar2_avo_saam_sec"
fulldf <- bind_rows(fulldf, df)


anav_model_list <- c(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m, ecranx_mod_avo_mod_m)
anav_syn_model_list <- c(ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod, ecranx_mod_avo_mod)
df <- mapply(pull_fitmeasures, anav_model_list, anav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_n_aar2_anx_saam_avo"
fulldf <- bind_rows(fulldf, df)

anan_model_list <- c(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)
anan_syn_model_list <- c(ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod, ecranx_mod_anx_mod)
df <- mapply(pull_fitmeasures, anan_model_list, anan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_n_aar2_anx_saam_anx"
fulldf <- bind_rows(fulldf, df)

anse_model_list <- c(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
anse_syn_model_list <- c(ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
df <- mapply(pull_fitmeasures, anse_model_list, anse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_n_aar2_anx_saam_sec"
fulldf <- bind_rows(fulldf, df)


s2_n_aar2_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m,
                         ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m,ecravo_mod_anx_mod_m,
                         ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m,ecravo_mod_sec_mod_m, 
                         ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m,ecranx_mod_avo_mod_m,
                         ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m,  ecranx_mod_anx_mod_m,
                         ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
s2_n_aar2_syn_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod,
                       ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod,ecravo_mod_anx_mod,
                       ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod,ecravo_mod_sec_mod,
                       ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod,ecranx_mod_avo_mod,
                       ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod,  ecranx_mod_anx_mod,
                       ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
s2_n_aar2_df <- posnegint_formoderation
names(s2_n_aar2_model_list) <- c("ecravo_allfree_avo_mod_m", "ecravo_afixed_avo_mod_m", "ecravo_pfixed_avo_mod_m","ecravo_apfixed_avo_mod_m","ecravo_aonly_avo_mod_m","ecravo_ponly_avo_mod_m", "ecravo_mod_avo_mod_m",
                                "ecravo_allfree_anx_mod_m", "ecravo_afixed_anx_mod_m", "ecravo_pfixed_anx_mod_m","ecravo_apfixed_anx_mod_m","ecravo_aonly_anx_mod_m","ecravo_ponly_anx_mod_m","ecravo_mod_anx_mod_m",
                                "ecravo_allfree_sec_mod_m", "ecravo_afixed_sec_mod_m", "ecravo_pfixed_sec_mod_m","ecravo_apfixed_sec_mod_m","ecravo_aonly_sec_mod_m","ecravo_ponly_sec_mod_m","ecravo_mod_sec_mod_m",
                                "ecranx_allfree_avo_mod_m", "ecranx_afixed_avo_mod_m", "ecranx_pfixed_avo_mod_m","ecranx_apfixed_avo_mod_m","ecranx_aonly_avo_mod_m","ecranx_ponly_avo_mod_m","ecranx_mod_avo_mod_m",
                                "ecranx_allfree_anx_mod_m", "ecranx_afixed_anx_mod_m", "ecranx_pfixed_anx_mod_m","ecranx_apfixed_anx_mod_m","ecranx_aonly_anx_mod_m","ecranx_ponly_anx_mod_m",  "ecranx_mod_anx_mod_m",
                                "ecranx_allfree_sec_mod_m", "ecranx_afixed_sec_mod_m", "ecranx_pfixed_sec_mod_m","ecranx_apfixed_sec_mod_m","ecranx_aonly_sec_mod_m","ecranx_ponly_sec_mod_m", "ecranx_mod_sec_mod_m")
names(s2_n_aar2_syn_list) <- c("ecravo_allfree_avo_mod", "ecravo_afixed_avo_mod", "ecravo_pfixed_avo_mod","ecravo_apfixed_avo_mod","ecravo_aonly_avo_mod","ecravo_ponly_avo_mod", "ecravo_mod_avo_mod",
                              "ecravo_allfree_anx_mod", "ecravo_afixed_anx_mod", "ecravo_pfixed_anx_mod","ecravo_apfixed_anx_mod","ecravo_aonly_anx_mod","ecravo_ponly_anx_mod","ecravo_mod_anx_mod",
                              "ecravo_allfree_sec_mod", "ecravo_afixed_sec_mod", "ecravo_pfixed_sec_mod","ecravo_apfixed_sec_mod","ecravo_aonly_sec_mod","ecravo_ponly_sec_mod","ecravo_mod_sec_mod",
                              "ecranx_allfree_avo_mod", "ecranx_afixed_avo_mod", "ecranx_pfixed_avo_mod","ecranx_apfixed_avo_mod","ecranx_aonly_avo_mod","ecranx_ponly_avo_mod","ecranx_mod_avo_mod",
                              "ecranx_allfree_anx_mod", "ecranx_afixed_anx_mod", "ecranx_pfixed_anx_mod","ecranx_apfixed_anx_mod","ecranx_aonly_anx_mod","ecranx_ponly_anx_mod",  "ecranx_mod_anx_mod",
                              "ecranx_allfree_sec_mod", "ecranx_afixed_sec_mod", "ecranx_pfixed_sec_mod","ecranx_apfixed_sec_mod","ecranx_aonly_sec_mod","ecranx_ponly_sec_mod", "ecranx_mod_sec_mod")

# Initialize Models -------------------------------------------------------

ecravo_allfree_avo_mod_m <-  ecravo_afixed_avo_mod_m <-  ecravo_pfixed_avo_mod_m <- ecravo_apfixed_avo_mod_m <- ecravo_aonly_avo_mod_m <- ecravo_ponly_avo_mod_m <-  ecravo_mod_avo_mod_m <- ""
ecravo_allfree_avo_mod <- ecravo_afixed_avo_mod <- ecravo_pfixed_avo_mod <- ecravo_apfixed_avo_mod <- ecravo_aonly_avo_mod <- ecravo_ponly_avo_mod <-  ecravo_mod_avo_mod <- ""

ecravo_allfree_anx_mod_m <- ecravo_afixed_anx_mod_m  <- ecravo_pfixed_anx_mod_m <- ecravo_apfixed_anx_mod_m <- ecravo_aonly_anx_mod_m <- ecravo_ponly_anx_mod_m <-  ecravo_mod_anx_mod_m <- ""
ecravo_allfree_anx_mod <- ecravo_afixed_anx_mod <- ecravo_pfixed_anx_mod <- ecravo_apfixed_anx_mod <- ecravo_aonly_anx_mod<- ecravo_ponly_anx_mod <- ecravo_mod_anx_mod <- ""

ecravo_allfree_sec_mod_m <- ecravo_afixed_sec_mod_m <- ecravo_pfixed_sec_mod_m <- ecravo_apfixed_sec_mod_m <- ecravo_aonly_sec_mod_m <- ecravo_ponly_sec_mod_m <- ecravo_mod_sec_mod_m <- ""
ecravo_allfree_sec_mod <- ecravo_afixed_sec_mod <- ecravo_pfixed_sec_mod <- ecravo_apfixed_sec_mod <- ecravo_aonly_sec_mod <- ecravo_ponly_sec_mod <-  ecravo_mod_sec_mod <- ""

ecranx_allfree_avo_mod_m <- ecranx_afixed_avo_mod_m <- ecranx_pfixed_avo_mod_m <- ecranx_apfixed_avo_mod_m <- ecranx_aonly_avo_mod_m <- ecranx_ponly_avo_mod_m <- ecranx_mod_avo_mod_m <- ""
ecranx_allfree_avo_mod <- ecranx_afixed_avo_mod <- ecranx_pfixed_avo_mod <- ecranx_apfixed_avo_mod <- ecranx_aonly_avo_mod <- ecranx_ponly_avo_mod <-  ecranx_mod_avo_mod <- ""

ecranx_allfree_anx_mod_m <- ecranx_afixed_anx_mod_m <-  ecranx_pfixed_anx_mod_m <- ecranx_apfixed_anx_mod_m <- ecranx_aonly_anx_mod_m <- ecranx_ponly_anx_mod_m <- ecranx_mod_anx_mod_m <- ""
ecranx_allfree_anx_mod <- ecranx_afixed_anx_mod <- ecranx_pfixed_anx_mod <- ecranx_apfixed_anx_mod <- ecranx_aonly_anx_mod <- ecranx_ponly_anx_mod <- ecranx_mod_anx_mod <- ""

ecranx_allfree_sec_mod_m <- ecranx_afixed_sec_mod_m <- ecranx_pfixed_sec_mod_m <- ecranx_apfixed_sec_mod_m <- ecranx_aonly_sec_mod_m <- ecranx_ponly_sec_mod_m <- ecranx_mod_sec_mod_m <- ""
ecranx_allfree_sec_mod <- ecranx_afixed_sec_mod <- ecranx_pfixed_sec_mod <- ecranx_apfixed_sec_mod <- ecranx_aonly_sec_mod <- ecranx_ponly_sec_mod <- ecranx_mod_sec_mod <- ""



# AAR sec gating S1 posint avo ---------------------------------------------
ecravo_allfree_avo_mod <- paste(posint_avo_mod, ECR_moderation("avo","avo", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecravo_allfree_avo_mod_m <- sem(model = ecravo_allfree_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_avo_mod <- paste(posint_avo_mod, ECR_moderation("avo","avo", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecravo_afixed_avo_mod_m <- sem(model = ecravo_afixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_avo_mod <- paste(posint_avo_mod, ECR_moderation("avo","avo", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecravo_pfixed_avo_mod_m <- sem(model = ecravo_pfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_avo_mod <- paste(posint_avo_mod, ECR_moderation("avo","avo", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecravo_apfixed_avo_mod_m <- sem(model = ecravo_apfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_avo_mod <- paste(posint_avo_mod, ECR_moderation("avo","avo", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecravo_aonly_avo_mod_m <- sem(model = ecravo_aonly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_avo_mod <- paste(posint_avo_mod, ECR_moderation("avo","avo", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecravo_ponly_avo_mod_m <- sem(model = ecravo_ponly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m)
ecravo_mod_avo_mod_m <- ecravo_allfree_avo_mod_m
ecravo_mod_avo_mod <- ecravo_allfree_avo_mod
# No evidence that baseline avoidance on contrarianism in partner is gated by braoder avoidance. Direct effect of braoder avoidance in patient and partner such that avoidance predicts dependency in partner in positive interaction.

# AAR sec gating S1 posint anx -----------------------------------------

ecravo_allfree_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_allfree_anx_mod_m <- sem(model = ecravo_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_afixed_anx_mod_m <- sem(model = ecravo_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_pfixed_anx_mod_m <- sem(model = ecravo_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_apfixed_anx_mod_m <- sem(model = ecravo_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_aonly_anx_mod_m <- sem(model = ecravo_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_ponly_anx_mod_m <- sem(model = ecravo_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m)

ecravo_mod_anx_mod <- "
p_ccpt ~ b*panx1 #a
p_ccpt ~ a*pranx1 #a
p_ccpr ~ b*panx0 # a
p_ccpr ~ a*pranx0 #a


pranx1 ~~ pranx0
panx1~~panx0
p_ccpt ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

 ECRavo_1 ~~ 0*av1pran1 
ECRavo_0 ~~ 0*av1pran1 
ECRavo_1 ~~ 0*av1pran0 
ECRavo_0 ~~ 0*av1pran0 
ECRavo_1 ~~ 0*av0pran1 
ECRavo_0 ~~ 0*av0pran1 
ECRavo_1 ~~ 0*av0pran0 
ECRavo_0 ~~ 0*av0pran0
av1pran1 ~~ av1pran1 
av1pran0 ~~ av1pran0 
av0pran1 ~~ av0pran1 
av0pran0 ~~ av0pran0
pranx1 ~~ 0*av1pran1
pranx1 ~~ 0*av1pran0
pranx1 ~~ 0*av0pran1
pranx1 ~~ 0*av0pran0
pranx0 ~~ 0*av1pran1
pranx0 ~~ 0*av1pran0
pranx0 ~~ 0*av0pran1
pranx0 ~~ 0*av0pran0 
p_ccpt ~ 0*ECRavo_1

p_ccpt ~ ECRavo_0

p_ccpr ~ 0*ECRavo_1

p_ccpr ~ 0*ECRavo_0 
p_ccpt ~ 0*av1pran1

p_ccpt ~ b*av0pran1

p_ccpr ~ a*av1pran0

p_ccpr ~ 0*av0pran0 

"
ecravo_mod_anx_mod_m <- sem(model = ecravo_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m,ecravo_mod_anx_mod_m)

# AAR sec gating S1 posint sec --------------------------------------------

ecravo_allfree_sec_mod <- paste(posint_sec_mod, ECR_moderation("avo","sec", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecravo_allfree_sec_mod_m <- sem(model = ecravo_allfree_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_sec_mod <- paste(posint_sec_mod, ECR_moderation("avo","sec", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecravo_afixed_sec_mod_m <- sem(model = ecravo_afixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_sec_mod <- paste(posint_sec_mod, ECR_moderation("avo","sec", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecravo_pfixed_sec_mod_m <- sem(model = ecravo_pfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_sec_mod <- paste(posint_sec_mod, ECR_moderation("avo","sec", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecravo_apfixed_sec_mod_m <- sem(model = ecravo_apfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_sec_mod <- paste(posint_sec_mod, ECR_moderation("avo","sec", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecravo_aonly_sec_mod_m <- sem(model = ecravo_aonly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_sec_mod <- paste(posint_sec_mod, ECR_moderation("avo","sec", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecravo_ponly_sec_mod_m <- sem(model = ecravo_ponly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m)

ecravo_mod_sec_mod <- "

p_ccpt ~ 0*psec1 #a
p_ccpt ~ 0*prsec1 #a
p_ccpt ~ 0*psec0 #p
p_ccpt ~ prsec0 #p
p_ccpr ~ 0*psec1 #p
p_ccpr ~ prsec1 #p
p_ccpr ~ 0*psec0 # a
p_ccpr ~ prsec0 #a

prsec1 ~~ prsec0
psec1~~psec0
p_ccpt ~~ p_ccpr
psec1 ~ prsec1
psec0 ~prsec0

 ECRavo_1 ~~ 0*av1prse1 
ECRavo_0 ~~ 0*av1prse1 
ECRavo_1 ~~ 0*av1prse0 
ECRavo_0 ~~ 0*av1prse0 
ECRavo_1 ~~ 0*av0prse1 
ECRavo_0 ~~ 0*av0prse1 
ECRavo_1 ~~ 0*av0prse0 
ECRavo_0 ~~ 0*av0prse0
av1prse1 ~~ av1prse1 
av1prse0 ~~ av1prse0 
av0prse1 ~~ av0prse1 
av0prse0 ~~ av0prse0
prsec1 ~~ 0*av1prse1
prsec1 ~~ 0*av1prse0
prsec1 ~~ 0*av0prse1
prsec1 ~~ 0*av0prse0
prsec0 ~~ 0*av1prse1
prsec0 ~~ 0*av1prse0
prsec0 ~~ 0*av0prse1
prsec0 ~~ 0*av0prse0 
p_ccpt ~ l*ECRavo_1

p_ccpt ~ l*ECRavo_0

p_ccpr ~ 0*ECRavo_1

p_ccpr ~ ECRavo_0 
p_ccpt ~ 0*av1prse0

p_ccpt ~ v*av0prse0

p_ccpr ~ v*av1prse1

p_ccpr ~ 0*av1prse0

p_ccpr ~ v*av0prse1

p_ccpr ~ l*av0prse0 
"
ecravo_mod_sec_mod_m <- sem(model = ecravo_mod_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m)


# AAR anx gating S1 posint avo ---------------------------------------------

ecranx_allfree_avo_mod <- paste(posint_avo_mod, ECR_moderation("anx","avo", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecranx_allfree_avo_mod_m <- sem(model = ecranx_allfree_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_avo_mod <- paste(posint_avo_mod, ECR_moderation("anx","avo", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecranx_afixed_avo_mod_m <- sem(model = ecranx_afixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_avo_mod <- paste(posint_avo_mod, ECR_moderation("anx","avo", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecranx_pfixed_avo_mod_m <- sem(model = ecranx_pfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_avo_mod <- paste(posint_avo_mod, ECR_moderation("anx","avo", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecranx_apfixed_avo_mod_m <- sem(model = ecranx_apfixed_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_avo_mod <- paste(posint_avo_mod, ECR_moderation("anx","avo", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecranx_aonly_avo_mod_m <- sem(model = ecranx_aonly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_avo_mod <- paste(posint_avo_mod, ECR_moderation("anx","avo", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_avo_mod),"\n")
ecranx_ponly_avo_mod_m <- sem(model = ecranx_ponly_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m)

ecranx_mod_avo_mod <- "
p_ccpr ~ pravo1 #p
pavo1 ~ pravo1
pavo0 ~pravo0


pravo1 ~~ pravo0
pavo1 ~~ pavo0

p_ccpt ~~ p_ccpr
p_ccpr ~~ 0*pavo1
p_ccpr ~~ 0*pavo0
p_ccpt ~ 0*pavo0
p_ccpt ~ 0*pavo1
p_ccpt ~ 0*pravo0
p_ccpt ~ 0*pravo1
 ECRanx_1 ~~ 0*an1prav1 
ECRanx_0 ~~ 0*an1prav1 
ECRanx_1 ~~ 0*an0prav1 
ECRanx_0 ~~ 0*an0prav1 
an1prav1 ~~ an1prav1 
an0prav1 ~~ an0prav1 
pravo1 ~~ 0*an1prav1
pravo1 ~~ 0*an0prav1
pravo0 ~~ 0*an1prav1
pravo0 ~~ 0*an0prav1 
p_ccpr ~ 0*ECRanx_1

p_ccpr ~ ECRanx_0 
p_ccpr ~ an1prav1

p_ccpr ~ 0*an0prav1 
"
ecranx_mod_avo_mod_m <- sem(model = ecranx_mod_avo_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_mod_avo_mod_m <- ecranx_allfree_avo_mod_m
ecranx_mod_avo_mod <- ecranx_allfree_avo_mod
# AAR anx gating S1 posint anx -----------------------------------------

ecranx_allfree_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_allfree_anx_mod_m <- sem(model = ecranx_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_afixed_anx_mod_m <- sem(model = ecranx_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_pfixed_anx_mod_m <- sem(model = ecranx_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_apfixed_anx_mod_m <- sem(model = ecranx_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_aonly_anx_mod_m <- sem(model = ecranx_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_ponly_anx_mod_m <- sem(model = ecranx_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m)

ecranx_mod_anx_mod <- "

p_ccpt ~ b*panx1 #a
p_ccpt ~ a*pranx1 #a
p_ccpr ~ b*panx0 # a
p_ccpr ~ a*pranx0 #a


pranx1 ~~ pranx0
panx1~~panx0
p_ccpt ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

 ECRanx_1 ~~ 0*an1pran1 
ECRanx_0 ~~ 0*an1pran1 
ECRanx_1 ~~ 0*an1pran0 
ECRanx_0 ~~ 0*an1pran0 
ECRanx_1 ~~ 0*an0pran1 
ECRanx_0 ~~ 0*an0pran1 
ECRanx_1 ~~ 0*an0pran0 
ECRanx_0 ~~ 0*an0pran0
an1pran1 ~~ an1pran1 
an1pran0 ~~ an1pran0 
an0pran1 ~~ an0pran1 
an0pran0 ~~ an0pran0
pranx1 ~~ 0*an1pran1
pranx1 ~~ 0*an1pran0
pranx1 ~~ 0*an0pran1
pranx1 ~~ 0*an0pran0
pranx0 ~~ 0*an1pran1
pranx0 ~~ 0*an1pran0
pranx0 ~~ 0*an0pran1
pranx0 ~~ 0*an0pran0 
p_ccpt ~ i*ECRanx_1

p_ccpt ~ ECRanx_0

p_ccpr ~ 0*ECRanx_1

p_ccpr ~ i*ECRanx_0 
p_ccpt ~ i*an1pran1

p_ccpt ~ 0*an0pran1

p_ccpr ~ an1pran0

p_ccpr ~ 0*an0pran0 
pranx1 ~~ ECRanx_1
"
ecranx_mod_anx_mod_m <- sem(model = ecranx_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
# Marginal effect where the extent to which baseline anxiety predicts contrarianism during the positive interaction in partner is gated by partner's anxiety. In particular, partner's anxiety attenuates this relationship. 
anova(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)
# AAR anx gating S1 posint sec --------------------------------------------


ecranx_allfree_sec_mod <- paste(posint_sec_mod, ECR_moderation("anx","sec", 1, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecranx_allfree_sec_mod_m <- sem(model = ecranx_allfree_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_sec_mod <- paste(posint_sec_mod, ECR_moderation("anx","sec", 1, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecranx_afixed_sec_mod_m <- sem(model = ecranx_afixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_sec_mod <- paste(posint_sec_mod, ECR_moderation("anx","sec", 1, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecranx_pfixed_sec_mod_m <- sem(model = ecranx_pfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_sec_mod <- paste(posint_sec_mod, ECR_moderation("anx","sec", 1, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecranx_apfixed_sec_mod_m <- sem(model = ecranx_apfixed_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_sec_mod <- paste(posint_sec_mod, ECR_moderation("anx","sec", 1, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecranx_aonly_sec_mod_m <- sem(model = ecranx_aonly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_sec_mod <- paste(posint_sec_mod, ECR_moderation("anx","sec", 1, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_sec_mod),"\n")
ecranx_ponly_sec_mod_m <- sem(model = ecranx_ponly_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


anova(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m)

ecranx_mod_sec_mod <- "
p_ccpt ~ 0*psec1 #a
p_ccpt ~ 0*prsec1 #a
p_ccpt ~ 0*psec0 #p
p_ccpt ~ prsec0 #p
p_ccpr ~ 0*psec1 #p
p_ccpr ~ prsec1 #p
p_ccpr ~ 0*psec0 # a
p_ccpr ~ prsec0 #a

prsec1 ~~ prsec0
psec1~~psec0
p_ccpt ~~ p_ccpr
psec1 ~ prsec1
psec0 ~prsec0

 ECRanx_1 ~~ 0*an1prse1 
ECRanx_0 ~~ 0*an1prse1 
ECRanx_1 ~~ 0*an1prse0 
ECRanx_0 ~~ 0*an1prse0 
ECRanx_1 ~~ 0*an0prse1 
ECRanx_0 ~~ 0*an0prse1 
ECRanx_1 ~~ 0*an0prse0 
ECRanx_0 ~~ 0*an0prse0
an1prse1 ~~ an1prse1 
an1prse0 ~~ an1prse0 
an0prse1 ~~ an0prse1 
an0prse0 ~~ an0prse0
prsec1 ~~ 0*an1prse1
prsec1 ~~ 0*an1prse0
prsec1 ~~ 0*an0prse1
prsec1 ~~ 0*an0prse0
prsec0 ~~ 0*an1prse1
prsec0 ~~ 0*an1prse0
prsec0 ~~ 0*an0prse1
prsec0 ~~ 0*an0prse0 
p_ccpt ~ 0*ECRanx_1

p_ccpt ~ l*ECRanx_0

p_ccpr ~ 0*ECRanx_1

p_ccpr ~ v*ECRanx_0 
p_ccpt ~ 0*an1prse0

p_ccpt ~ v*an0prse0

p_ccpr ~ v*an1prse1

p_ccpr ~ v*an1prse0

p_ccpr ~ v*an0prse1

p_ccpr ~ l*an0prse0 
"
ecranx_mod_sec_mod_m <- sem(model = ecranx_mod_sec_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)

# S1 AAR Posint Save Models  ----------------------------------------------
avav_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m)
avav_syn_model_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod)
df <- mapply(pull_fitmeasures, avav_model_list, avav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_p_aar2_avo_saam_avo"
fulldf <- df

avan_model_list <- c(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)
avan_syn_model_list <- c(ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod, ecravo_mod_anx_mod)
df <- mapply(pull_fitmeasures, avan_model_list, avan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_p_aar2_avo_saam_anx"
fulldf <- bind_rows(fulldf, df)

avse_model_list <- c(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m)
avse_syn_model_list <- c(ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod, ecravo_mod_sec_mod)
df <- mapply(pull_fitmeasures, avse_model_list, avse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_p_aar2_avo_saam_sec"
fulldf <- bind_rows(fulldf, df)


anav_model_list <- c(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m, ecranx_mod_avo_mod_m)
anav_syn_model_list <- c(ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod, ecranx_mod_avo_mod)
df <- mapply(pull_fitmeasures, anav_model_list, anav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_p_aar2_anx_saam_avo"
fulldf <- bind_rows(fulldf, df)

anan_model_list <- c(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)
anan_syn_model_list <- c(ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod, ecranx_mod_anx_mod)
df <- mapply(pull_fitmeasures, anan_model_list, anan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_p_aar2_anx_saam_anx"
fulldf <- bind_rows(fulldf, df)

anse_model_list <- c(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
anse_syn_model_list <- c(ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
df <- mapply(pull_fitmeasures, anse_model_list, anse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s1_p_aar2_anx_saam_sec"
fulldf <- bind_rows(fulldf, df)


s1_p_aar2_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m,
                         ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m,ecravo_mod_anx_mod_m,
                         ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m,ecravo_mod_sec_mod_m, 
                         ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m,ecranx_mod_avo_mod_m,
                         ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m,  ecranx_mod_anx_mod_m,
                         ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
s1_p_aar2_syn_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod,
                       ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod,ecravo_mod_anx_mod,
                       ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod,ecravo_mod_sec_mod,
                       ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod,ecranx_mod_avo_mod,
                       ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod,  ecranx_mod_anx_mod,
                       ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
s1_p_aar2_df <- posnegint_formoderation
names(s1_p_aar2_model_list) <- c("ecravo_allfree_avo_mod_m", "ecravo_afixed_avo_mod_m", "ecravo_pfixed_avo_mod_m","ecravo_apfixed_avo_mod_m","ecravo_aonly_avo_mod_m","ecravo_ponly_avo_mod_m", "ecravo_mod_avo_mod_m",
                                "ecravo_allfree_anx_mod_m", "ecravo_afixed_anx_mod_m", "ecravo_pfixed_anx_mod_m","ecravo_apfixed_anx_mod_m","ecravo_aonly_anx_mod_m","ecravo_ponly_anx_mod_m","ecravo_mod_anx_mod_m",
                                "ecravo_allfree_sec_mod_m", "ecravo_afixed_sec_mod_m", "ecravo_pfixed_sec_mod_m","ecravo_apfixed_sec_mod_m","ecravo_aonly_sec_mod_m","ecravo_ponly_sec_mod_m","ecravo_mod_sec_mod_m",
                                "ecranx_allfree_avo_mod_m", "ecranx_afixed_avo_mod_m", "ecranx_pfixed_avo_mod_m","ecranx_apfixed_avo_mod_m","ecranx_aonly_avo_mod_m","ecranx_ponly_avo_mod_m","ecranx_mod_avo_mod_m",
                                "ecranx_allfree_anx_mod_m", "ecranx_afixed_anx_mod_m", "ecranx_pfixed_anx_mod_m","ecranx_apfixed_anx_mod_m","ecranx_aonly_anx_mod_m","ecranx_ponly_anx_mod_m",  "ecranx_mod_anx_mod_m",
                                "ecranx_allfree_sec_mod_m", "ecranx_afixed_sec_mod_m", "ecranx_pfixed_sec_mod_m","ecranx_apfixed_sec_mod_m","ecranx_aonly_sec_mod_m","ecranx_ponly_sec_mod_m", "ecranx_mod_sec_mod_m")
names(s1_p_aar2_syn_list) <- c("ecravo_allfree_avo_mod", "ecravo_afixed_avo_mod", "ecravo_pfixed_avo_mod","ecravo_apfixed_avo_mod","ecravo_aonly_avo_mod","ecravo_ponly_avo_mod", "ecravo_mod_avo_mod",
                              "ecravo_allfree_anx_mod", "ecravo_afixed_anx_mod", "ecravo_pfixed_anx_mod","ecravo_apfixed_anx_mod","ecravo_aonly_anx_mod","ecravo_ponly_anx_mod","ecravo_mod_anx_mod",
                              "ecravo_allfree_sec_mod", "ecravo_afixed_sec_mod", "ecravo_pfixed_sec_mod","ecravo_apfixed_sec_mod","ecravo_aonly_sec_mod","ecravo_ponly_sec_mod","ecravo_mod_sec_mod",
                              "ecranx_allfree_avo_mod", "ecranx_afixed_avo_mod", "ecranx_pfixed_avo_mod","ecranx_apfixed_avo_mod","ecranx_aonly_avo_mod","ecranx_ponly_avo_mod","ecranx_mod_avo_mod",
                              "ecranx_allfree_anx_mod", "ecranx_afixed_anx_mod", "ecranx_pfixed_anx_mod","ecranx_apfixed_anx_mod","ecranx_aonly_anx_mod","ecranx_ponly_anx_mod",  "ecranx_mod_anx_mod",
                              "ecranx_allfree_sec_mod", "ecranx_afixed_sec_mod", "ecranx_pfixed_sec_mod","ecranx_apfixed_sec_mod","ecranx_aonly_sec_mod","ecranx_ponly_sec_mod", "ecranx_mod_sec_mod")

# Initialize Models -------------------------------------------------------

ecravo_allfree_avo_mod_m <-  ecravo_afixed_avo_mod_m <-  ecravo_pfixed_avo_mod_m <- ecravo_apfixed_avo_mod_m <- ecravo_aonly_avo_mod_m <- ecravo_ponly_avo_mod_m <-  ecravo_mod_avo_mod_m <- ""
ecravo_allfree_avo_mod <- ecravo_afixed_avo_mod <- ecravo_pfixed_avo_mod <- ecravo_apfixed_avo_mod <- ecravo_aonly_avo_mod <- ecravo_ponly_avo_mod <-  ecravo_mod_avo_mod <- ""

ecravo_allfree_anx_mod_m <- ecravo_afixed_anx_mod_m  <- ecravo_pfixed_anx_mod_m <- ecravo_apfixed_anx_mod_m <- ecravo_aonly_anx_mod_m <- ecravo_ponly_anx_mod_m <-  ecravo_mod_anx_mod_m <- ""
ecravo_allfree_anx_mod <- ecravo_afixed_anx_mod <- ecravo_pfixed_anx_mod <- ecravo_apfixed_anx_mod <- ecravo_aonly_anx_mod<- ecravo_ponly_anx_mod <- ecravo_mod_anx_mod <- ""

ecravo_allfree_sec_mod_m <- ecravo_afixed_sec_mod_m <- ecravo_pfixed_sec_mod_m <- ecravo_apfixed_sec_mod_m <- ecravo_aonly_sec_mod_m <- ecravo_ponly_sec_mod_m <- ecravo_mod_sec_mod_m <- ""
ecravo_allfree_sec_mod <- ecravo_afixed_sec_mod <- ecravo_pfixed_sec_mod <- ecravo_apfixed_sec_mod <- ecravo_aonly_sec_mod <- ecravo_ponly_sec_mod <-  ecravo_mod_sec_mod <- ""

ecranx_allfree_avo_mod_m <- ecranx_afixed_avo_mod_m <- ecranx_pfixed_avo_mod_m <- ecranx_apfixed_avo_mod_m <- ecranx_aonly_avo_mod_m <- ecranx_ponly_avo_mod_m <- ecranx_mod_avo_mod_m <- ""
ecranx_allfree_avo_mod <- ecranx_afixed_avo_mod <- ecranx_pfixed_avo_mod <- ecranx_apfixed_avo_mod <- ecranx_aonly_avo_mod <- ecranx_ponly_avo_mod <-  ecranx_mod_avo_mod <- ""

ecranx_allfree_anx_mod_m <- ecranx_afixed_anx_mod_m <-  ecranx_pfixed_anx_mod_m <- ecranx_apfixed_anx_mod_m <- ecranx_aonly_anx_mod_m <- ecranx_ponly_anx_mod_m <- ecranx_mod_anx_mod_m <- ""
ecranx_allfree_anx_mod <- ecranx_afixed_anx_mod <- ecranx_pfixed_anx_mod <- ecranx_apfixed_anx_mod <- ecranx_aonly_anx_mod <- ecranx_ponly_anx_mod <- ecranx_mod_anx_mod <- ""

ecranx_allfree_sec_mod_m <- ecranx_afixed_sec_mod_m <- ecranx_pfixed_sec_mod_m <- ecranx_apfixed_sec_mod_m <- ecranx_aonly_sec_mod_m <- ecranx_ponly_sec_mod_m <- ecranx_mod_sec_mod_m <- ""
ecranx_allfree_sec_mod <- ecranx_afixed_sec_mod <- ecranx_pfixed_sec_mod <- ecranx_apfixed_sec_mod <- ecranx_aonly_sec_mod <- ecranx_ponly_sec_mod <- ecranx_mod_sec_mod <- ""




# AAR sec gating S2 posint anx -----------------------------------------



ecravo_allfree_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_allfree_anx_mod_m <- sem(model = ecravo_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_afixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_afixed_anx_mod_m <- sem(model = ecravo_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_pfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_pfixed_anx_mod_m <- sem(model = ecravo_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecravo_apfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_apfixed_anx_mod_m <- sem(model = ecravo_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_aonly_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_aonly_anx_mod_m <- sem(model = ecravo_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecravo_ponly_anx_mod <- paste(posint_anx_mod, ECR_moderation("avo","anx", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecravo_ponly_anx_mod_m <- sem(model = ecravo_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m)


ecravo_mod_anx_mod <- "
p_ccpt ~ b*panx1 #a
p_ccpt ~ a*pranx1 #a
p_ccpr ~ b*panx0 # a
p_ccpr ~ a*pranx0 #a


pranx1 ~~ pranx0
panx1~~panx0
p_ccpt ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

 ECRavo_1 ~~ 0*av1pan1 
ECRavo_0 ~~ 0*av1pan1 
ECRavo_1 ~~ 0*av1pan0 
ECRavo_0 ~~ 0*av1pan0 
ECRavo_1 ~~ 0*av0pan1 
ECRavo_0 ~~ 0*av0pan1 
ECRavo_1 ~~ 0*av0pan0 
ECRavo_0 ~~ 0*av0pan0
av1pan1 ~~ av1pan1 
av1pan0 ~~ av1pan0 
av0pan1 ~~ av0pan1 
av0pan0 ~~ av0pan0
panx1 ~~ 0*av1pan1
panx1 ~~ 0*av1pan0
panx1 ~~ 0*av0pan1
panx1 ~~ 0*av0pan0
panx0 ~~ 0*av1pan1
panx0 ~~ 0*av1pan0
panx0 ~~ 0*av0pan1
panx0 ~~ 0*av0pan0 
p_ccpt ~ l*ECRavo_1

p_ccpt ~ f*ECRavo_0

p_ccpr ~ f*ECRavo_1

p_ccpr ~ l*ECRavo_0 
p_ccpt ~ f*av1pan1

p_ccpt ~ a*av0pan1

p_ccpr ~ f*av1pan0

p_ccpr ~ f*av0pan0 
av0pan1 ~~ av0pan0
"
ecravo_mod_anx_mod_m <- sem(model = ecravo_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
# Avoidance gates effect of increases of anxiety on patient and partner coupling. Increases in anxiety predict more dependency during the positive interaction in both patient and partner.This is amplified in patients by their own avoidance and their partner's avoidance. In partner's, this effect is attenuated if the partner is lower in avoidance.
anova(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)




# AAR anx gating S2 posint anx -----------------------------------------

ecranx_allfree_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 2, "allfree",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod), "\n") #,"\nan0pan1 ~~ an0pan0","\nan1pan1 ~~ an1pan0","\nan1pan1 ~~ an0pan0", "\nECRanx_0 ~~ panx0", "\n")
ecranx_allfree_anx_mod_m <- sem(model = ecranx_allfree_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_afixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 2, "afixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_afixed_anx_mod_m <- sem(model = ecranx_afixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_pfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 2, "pfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_pfixed_anx_mod_m <- sem(model = ecranx_pfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

ecranx_apfixed_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 2, "apfixed",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_apfixed_anx_mod_m <- sem(model = ecranx_apfixed_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_aonly_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 2, "aonly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_aonly_anx_mod_m <- sem(model = ecranx_aonly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


ecranx_ponly_anx_mod <- paste(posint_anx_mod, ECR_moderation("anx","anx", 2, "ponly",c("aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "ggggg", "hhhhh","paaaa", "pbbbb", "pcccc", "pdddd", "peeee", "pffff", "pggggg", "phhhhh" ), posint_anx_mod),"\n")
ecranx_ponly_anx_mod_m <- sem(model = ecranx_ponly_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
anova(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m)

ecranx_mod_anx_mod <-  "
p_ccpt ~ b*panx1 #a
p_ccpt ~ a*pranx1 #a
p_ccpr ~ b*panx0 # a
p_ccpr ~ a*pranx0 #a


pranx1 ~~ pranx0
panx1~~panx0
p_ccpt ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

 ECRanx_1 ~~ 0*an1pan1 
ECRanx_0 ~~ 0*an1pan1 
ECRanx_1 ~~ 0*an1pan0 
ECRanx_0 ~~ 0*an1pan0 
ECRanx_1 ~~ 0*an0pan1 
ECRanx_0 ~~ 0*an0pan1 
ECRanx_1 ~~ 0*an0pan0 
ECRanx_0 ~~ 0*an0pan0
an1pan1 ~~ an1pan1 
an1pan0 ~~ an1pan0 
an0pan1 ~~ an0pan1 
an0pan0 ~~ an0pan0
panx1 ~~ 0*an1pan1
panx1 ~~ 0*an1pan0
panx1 ~~ 0*an0pan1
panx1 ~~ 0*an0pan0
panx0 ~~ 0*an1pan1
panx0 ~~ 0*an1pan0
panx0 ~~ 0*an0pan1
panx0 ~~ 0*an0pan0 
p_ccpt ~ a*ECRanx_1

p_ccpt ~ l*ECRanx_0

p_ccpr ~ 0*ECRanx_1

p_ccpr ~ 0*ECRanx_0 
p_ccpt ~ a*an1pan1

p_ccpt ~ l*an0pan1

p_ccpr ~ a*an1pan0

p_ccpr ~ a*an0pan0 

"
ecranx_mod_anx_mod_m <- sem(model = ecranx_mod_anx_mod, data = posnegint_formoderation, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
# Patient's anxious attachment style gates the extent to which increases in anxiety couple with dependency during the positive interaction for the partner. It weakens this association. 
anova(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)



# S2 AAR Posint Save Models  ----------------------------------------------
avav_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m)
avav_syn_model_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod)
df <- mapply(pull_fitmeasures, avav_model_list, avav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_p_aar2_avo_saam_avo"
fulldf <- df

avan_model_list <- c(ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m, ecravo_mod_anx_mod_m)
avan_syn_model_list <- c(ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod, ecravo_mod_anx_mod)
df <- mapply(pull_fitmeasures, avan_model_list, avan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_p_aar2_avo_saam_anx"
fulldf <- bind_rows(fulldf, df)

avse_model_list <- c(ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m, ecravo_mod_sec_mod_m)
avse_syn_model_list <- c(ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod, ecravo_mod_sec_mod)
df <- mapply(pull_fitmeasures, avse_model_list, avse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_p_aar2_avo_saam_sec"
fulldf <- bind_rows(fulldf, df)


anav_model_list <- c(ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m, ecranx_mod_avo_mod_m)
anav_syn_model_list <- c(ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod, ecranx_mod_avo_mod)
df <- mapply(pull_fitmeasures, anav_model_list, anav_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_p_aar2_anx_saam_avo"
fulldf <- bind_rows(fulldf, df)

anan_model_list <- c(ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m, ecranx_mod_anx_mod_m)
anan_syn_model_list <- c(ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod, ecranx_mod_anx_mod)
df <- mapply(pull_fitmeasures, anan_model_list, anan_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_p_aar2_anx_saam_anx"
fulldf <- bind_rows(fulldf, df)

anse_model_list <- c(ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
anse_syn_model_list <- c(ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
df <- mapply(pull_fitmeasures, anse_model_list, anse_syn_model_list, list( posnegint_formoderation) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "s2_p_aar2_anx_saam_sec"
fulldf <- bind_rows(fulldf, df)


s2_p_aar2_model_list <- c(ecravo_allfree_avo_mod_m, ecravo_afixed_avo_mod_m, ecravo_pfixed_avo_mod_m,ecravo_apfixed_avo_mod_m,ecravo_aonly_avo_mod_m,ecravo_ponly_avo_mod_m, ecravo_mod_avo_mod_m,
                         ecravo_allfree_anx_mod_m, ecravo_afixed_anx_mod_m, ecravo_pfixed_anx_mod_m,ecravo_apfixed_anx_mod_m,ecravo_aonly_anx_mod_m,ecravo_ponly_anx_mod_m,ecravo_mod_anx_mod_m,
                         ecravo_allfree_sec_mod_m, ecravo_afixed_sec_mod_m, ecravo_pfixed_sec_mod_m,ecravo_apfixed_sec_mod_m,ecravo_aonly_sec_mod_m,ecravo_ponly_sec_mod_m,ecravo_mod_sec_mod_m, 
                         ecranx_allfree_avo_mod_m, ecranx_afixed_avo_mod_m, ecranx_pfixed_avo_mod_m,ecranx_apfixed_avo_mod_m,ecranx_aonly_avo_mod_m,ecranx_ponly_avo_mod_m,ecranx_mod_avo_mod_m,
                         ecranx_allfree_anx_mod_m, ecranx_afixed_anx_mod_m, ecranx_pfixed_anx_mod_m,ecranx_apfixed_anx_mod_m,ecranx_aonly_anx_mod_m,ecranx_ponly_anx_mod_m,  ecranx_mod_anx_mod_m,
                         ecranx_allfree_sec_mod_m, ecranx_afixed_sec_mod_m, ecranx_pfixed_sec_mod_m,ecranx_apfixed_sec_mod_m,ecranx_aonly_sec_mod_m,ecranx_ponly_sec_mod_m, ecranx_mod_sec_mod_m)
s2_p_aar2_syn_list <- c(ecravo_allfree_avo_mod, ecravo_afixed_avo_mod, ecravo_pfixed_avo_mod,ecravo_apfixed_avo_mod,ecravo_aonly_avo_mod,ecravo_ponly_avo_mod, ecravo_mod_avo_mod,
                       ecravo_allfree_anx_mod, ecravo_afixed_anx_mod, ecravo_pfixed_anx_mod,ecravo_apfixed_anx_mod,ecravo_aonly_anx_mod,ecravo_ponly_anx_mod,ecravo_mod_anx_mod,
                       ecravo_allfree_sec_mod, ecravo_afixed_sec_mod, ecravo_pfixed_sec_mod,ecravo_apfixed_sec_mod,ecravo_aonly_sec_mod,ecravo_ponly_sec_mod,ecravo_mod_sec_mod,
                       ecranx_allfree_avo_mod, ecranx_afixed_avo_mod, ecranx_pfixed_avo_mod,ecranx_apfixed_avo_mod,ecranx_aonly_avo_mod,ecranx_ponly_avo_mod,ecranx_mod_avo_mod,
                       ecranx_allfree_anx_mod, ecranx_afixed_anx_mod, ecranx_pfixed_anx_mod,ecranx_apfixed_anx_mod,ecranx_aonly_anx_mod,ecranx_ponly_anx_mod,  ecranx_mod_anx_mod,
                       ecranx_allfree_sec_mod, ecranx_afixed_sec_mod, ecranx_pfixed_sec_mod,ecranx_apfixed_sec_mod,ecranx_aonly_sec_mod,ecranx_ponly_sec_mod, ecranx_mod_sec_mod)
s2_p_aar2_df <- posnegint_formoderation
names(s2_p_aar2_model_list) <- c("ecravo_allfree_avo_mod_m", "ecravo_afixed_avo_mod_m", "ecravo_pfixed_avo_mod_m","ecravo_apfixed_avo_mod_m","ecravo_aonly_avo_mod_m","ecravo_ponly_avo_mod_m", "ecravo_mod_avo_mod_m",
                                "ecravo_allfree_anx_mod_m", "ecravo_afixed_anx_mod_m", "ecravo_pfixed_anx_mod_m","ecravo_apfixed_anx_mod_m","ecravo_aonly_anx_mod_m","ecravo_ponly_anx_mod_m","ecravo_mod_anx_mod_m",
                                "ecravo_allfree_sec_mod_m", "ecravo_afixed_sec_mod_m", "ecravo_pfixed_sec_mod_m","ecravo_apfixed_sec_mod_m","ecravo_aonly_sec_mod_m","ecravo_ponly_sec_mod_m","ecravo_mod_sec_mod_m",
                                "ecranx_allfree_avo_mod_m", "ecranx_afixed_avo_mod_m", "ecranx_pfixed_avo_mod_m","ecranx_apfixed_avo_mod_m","ecranx_aonly_avo_mod_m","ecranx_ponly_avo_mod_m","ecranx_mod_avo_mod_m",
                                "ecranx_allfree_anx_mod_m", "ecranx_afixed_anx_mod_m", "ecranx_pfixed_anx_mod_m","ecranx_apfixed_anx_mod_m","ecranx_aonly_anx_mod_m","ecranx_ponly_anx_mod_m",  "ecranx_mod_anx_mod_m",
                                "ecranx_allfree_sec_mod_m", "ecranx_afixed_sec_mod_m", "ecranx_pfixed_sec_mod_m","ecranx_apfixed_sec_mod_m","ecranx_aonly_sec_mod_m","ecranx_ponly_sec_mod_m", "ecranx_mod_sec_mod_m")
names(s2_p_aar2_syn_list) <- c("ecravo_allfree_avo_mod", "ecravo_afixed_avo_mod", "ecravo_pfixed_avo_mod","ecravo_apfixed_avo_mod","ecravo_aonly_avo_mod","ecravo_ponly_avo_mod", "ecravo_mod_avo_mod",
                              "ecravo_allfree_anx_mod", "ecravo_afixed_anx_mod", "ecravo_pfixed_anx_mod","ecravo_apfixed_anx_mod","ecravo_aonly_anx_mod","ecravo_ponly_anx_mod","ecravo_mod_anx_mod",
                              "ecravo_allfree_sec_mod", "ecravo_afixed_sec_mod", "ecravo_pfixed_sec_mod","ecravo_apfixed_sec_mod","ecravo_aonly_sec_mod","ecravo_ponly_sec_mod","ecravo_mod_sec_mod",
                              "ecranx_allfree_avo_mod", "ecranx_afixed_avo_mod", "ecranx_pfixed_avo_mod","ecranx_apfixed_avo_mod","ecranx_aonly_avo_mod","ecranx_ponly_avo_mod","ecranx_mod_avo_mod",
                              "ecranx_allfree_anx_mod", "ecranx_afixed_anx_mod", "ecranx_pfixed_anx_mod","ecranx_apfixed_anx_mod","ecranx_aonly_anx_mod","ecranx_ponly_anx_mod",  "ecranx_mod_anx_mod",
                              "ecranx_allfree_sec_mod", "ecranx_afixed_sec_mod", "ecranx_pfixed_sec_mod","ecranx_apfixed_sec_mod","ecranx_aonly_sec_mod","ecranx_ponly_sec_mod", "ecranx_mod_sec_mod")








# Save Models and Data Run ------------------------------------------------
write.csv(fulldf, paste0(project_dir, "/output/bsem_personalitymod/fit_indices_process_ECRmoderation_models_combined.csv"), row.names= FALSE)


models_to_save <- list(s1_n_ecr_model_list, s1_n_ecr_syn_list, s1_n_ecr_df, 
                            s2_n_ecr_model_list, s2_n_ecr_syn_list, s2_n_ecr_df, 
                            s1_p_ecr_model_list, s1_p_ecr_syn_list, s1_p_ecr_df, 
                            s2_p_ecr_model_list, s2_p_ecr_syn_list, s2_p_ecr_df, 
                            s1_n_aar_model_list, s1_n_aar_syn_list, s1_n_aar_df, 
                            s2_n_aar_model_list, s2_n_aar_syn_list, s2_n_aar_df, 
                            s1_p_aar_model_list, s1_p_aar_syn_list, s1_p_aar_df,
                            s2_p_aar_model_list, s2_p_aar_syn_list, s2_p_aar_df,
                            s1_n_aar2_model_list, s1_n_aar2_syn_list, s1_n_aar2_df, 
                            s2_n_aar2_model_list, s2_n_aar2_syn_list, s2_n_aar2_df, 
                            s1_p_aar2_model_list, s1_p_aar2_syn_list, s1_p_aar2_df,
                            s2_p_aar2_model_list, s2_p_aar2_syn_list, s2_p_aar2_df)
names(models_to_save) <- c("s1_n_ecr_model_list", "s1_n_ecr_syn_list", "s1_n_ecr_df", 
                                "s2_n_ecr_model_list", "s2_n_ecr_syn_list", "s2_n_ecr_df", 
                                "s1_p_ecr_model_list", "s1_p_ecr_syn_list", "s1_p_ecr_df", 
                                "s2_p_ecr_model_list", "s2_p_ecr_syn_list", "s2_p_ecr_df", 
                                "s1_n_aar_model_list", "s1_n_aar_syn_list", "s1_n_aar_df", 
                                "s2_n_aar_model_list", "s2_n_aar_syn_list", "s2_n_aar_df", 
                                "s1_p_aar_model_list", "s1_p_aar_syn_list", "s1_p_aar_df",
                                "s2_p_aar_model_list", "s2_p_aar_syn_list", "s2_p_aar_df",
                                "s1_n_aar2_model_list", "s1_n_aar2_syn_list", "s1_n_aar2_df", 
                                "s2_n_aar2_model_list", "s2_n_aar2_syn_list", "s2_n_aar2_df", 
                                "s1_p_aar2_model_list", "s1_p_aar2_syn_list", "s1_p_aar2_df",
                                "s2_p_aar2_model_list", "s2_p_aar2_syn_list", "s2_p_aar2_df")

save(models_to_save, cor_ecr, cor_aar, cor_aar2, file = "../output/bsem_personalitymod/models_run.RData"))





