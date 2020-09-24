# Set up paths ------------------------------------------------------------
## n.b. make sure that working directory set to the location of where this file exists
# Source info and functions --------------------------------------------------------

source("../support_fx.R")


# Load Packages -----------------------------------------------------------

if (!require(pacman)) { install.packages("pacman"); library(pacman) }
p_load(tidyverse, R.matlab,lavaan,lattice, MplusAutomation)


# Load Data ---------------------------------------------------------------

posnegint_personality <- read.csv("../data/posnegint_personality.csv")


# Load Base Syntax --------------------------------------------------------

load("../output/bsem_personalitymod/models_run.RData")
load("../output/bsem_process/model_strings.RData")


# Avo Avo N S1 ------------------------------------------------------------
ecravo_mod_avo_mod_syn <- models_to_save[["s1_n_ecr_syn_list"]][["ecravo_mod_avo_mod"]]
ecravo_mod_avo_mod_df <- models_to_save[["s1_n_ecr_df"]]
aaravo_mod_avo_mod_syn <- models_to_save[["s1_n_aar_syn_list"]][["ecravo_mod_avo_mod"]]
aaravo_mod_avo_mod_df <- models_to_save[["s1_n_aar_df"]]

avo_mod_avo_mod <- "

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

## AAR
ccpt ~ 0*AARavo_1

ccpt ~ 0*AARavo_0

ccpr ~ 0*AARavo_1

ccpr ~ 0*AARavo_0 
ccpt ~ aa1prav1

ccpt ~ 0*aa0prav1

ccpr ~ aa1prav1

ccpr ~ 0*aa0prav1
 AARavo_1 ~~ 0*aa1prav1 
AARavo_0 ~~ 0*aa1prav1 
AARavo_1 ~~ 0*aa0prav1 
AARavo_0 ~~ 0*aa0prav1 
aa1prav1 ~~ aa1prav1 
aa0prav1 ~~ aa0prav1 
pravo1 ~~ 0*aa1prav1
pravo1 ~~ 0*aa0prav1
pravo0 ~~ 0*aa1prav1
pravo0 ~~ 0*aa0prav1 
aa1prav1 ~~ av1prav1
pavo0 ~ AARavo_0
"
aaravo_mod_avo_mod_df <- rename(aaravo_mod_avo_mod_df, aa0prav1 = av0prav1, aa1prav1 = av1prav1, 
                                AARavo_0 = ECRavo_0, AARavo_1 = ECRavo_1) %>% dplyr::select(PTNUM, aa0prav1, aa1prav1, AARavo_0, AARavo_1)
tmp_df <- left_join(ecravo_mod_avo_mod_df, aaravo_mod_avo_mod_df)
avo_mod_avo_mod_m <- sem(avo_mod_avo_mod, tmp_df, mimic = "Mplus", estimator = "MLR") # just look at this due to issues with bayesian SEM for the incremental analyses
avo_mod_avo_mod_m <- runBSEM(modsyntax(tmp_df, avo_mod_avo_mod)) # min p-value for r on cc is .252; panx ~ cc is B = .11, p = .028

# Anx Avo N S1 ------------------------------------------------------------

ecranx_mod_avo_mod_syn <- models_to_save[["s1_n_ecr_syn_list"]][["ecranx_mod_avo_mod"]]
ecranx_mod_avo_mod_df <- models_to_save[["s1_n_ecr_df"]]
aaranx_mod_avo_mod_syn <- models_to_save[["s1_n_aar_syn_list"]][["ecranx_mod_avo_mod"]]
aaranx_mod_avo_mod_df <- models_to_save[["s1_n_aar_df"]]

anx_mod_avo_mod <- "
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
pravo0 ~~ 0*an0prav1 
ccpt ~ ECRanx_1

ccpt ~ ECRanx_0

ccpr ~ ECRanx_1

ccpr ~ ECRanx_0 
ccpt ~ an1prav1

ccpt ~ an0prav1

ccpr ~ an1prav1

ccpr ~ an0prav1 

## AAR
 AARanx_1 ~~ 0*aa1prav1 
AARanx_0 ~~ 0*aa1prav1 
AARanx_1 ~~ 0*aa0prav1 
AARanx_0 ~~ 0*aa0prav1 
aa1prav1 ~~ aa1prav1 
aa0prav1 ~~ aa0prav1 
pravo1 ~~ 0*aa1prav1
pravo1 ~~ 0*aa0prav1
pravo0 ~~ aa1prav1
pravo0 ~~ 0*aa0prav1 
ccpt ~ k*AARanx_1

ccpt ~ 0*AARanx_0

ccpr ~ j*AARanx_1

ccpr ~ 0*AARanx_0 
ccpt ~ k*aa1prav1

ccpt ~ k*aa0prav1

ccpr ~ j*aa1prav1

ccpr ~ j*aa0prav1 
ECRanx_1 ~~ AARanx_1
pavo1 ~ ECRanx_1
pravo1 ~ ECRanx_1
an1prav1 ~~ aa1prav1
pravo0 ~ ECRanx_0
pravo0 ~ AARanx_1
an1prav1 ~~ pravo0
pravo1 ~ ECRanx_0
"
aaranx_mod_avo_mod_df <- rename(aaranx_mod_avo_mod_df, aa0prav1 = an0prav1, aa1prav1 = an1prav1, 
                                AARanx_0 = ECRanx_0, AARanx_1 = ECRanx_1) %>% dplyr::select(PTNUM, aa0prav1, aa1prav1, AARanx_0, AARanx_1)
tmp_df <- left_join(ecranx_mod_avo_mod_df, aaranx_mod_avo_mod_df)
anx_mod_avo_mod_m <- sem(anx_mod_avo_mod, tmp_df, mimic= "Mplus", estimator = "MLR") # min p-value for r on cc is .252; panx ~ cc is B = .11, p = .028

#anx_mod_avo_mod_m <- runBSEM(modsyntax(tmp_df, anx_mod_avo_mod)) # min p-value for r on cc is .252; panx ~ cc is B = .11, p = .028



# Anx Anx N S1 ------------------------------------------------------------

ecranx_mod_anx_mod_syn <- models_to_save[["s1_n_ecr_syn_list"]][["ecranx_mod_anx_mod"]]
ecranx_mod_anx_mod_df <- models_to_save[["s1_n_ecr_df"]]
aaranx_mod_anx_mod_syn <- models_to_save[["s1_n_aar_syn_list"]][["ecranx_mod_anx_mod"]]
aaranx_mod_anx_mod_df <- models_to_save[["s1_n_aar_df"]]

anx_mod_anx_mod <- "

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

## AAR


AARanx_1 ~~ 0*aa1pran1 
AARanx_0 ~~ 0*aa1pran1 
AARanx_1 ~~ 0*aa1pran0 
AARanx_0 ~~ 0*aa1pran0 
AARanx_1 ~~ 0*aa0pran1 
AARanx_0 ~~ 0*aa0pran1 
AARanx_1 ~~ 0*aa0pran0 
AARanx_0 ~~ 0*aa0pran0
aa1pran1 ~~ aa1pran1 
aa1pran0 ~~ aa1pran0 
aa0pran1 ~~ aa0pran1 
aa0pran0 ~~ aa0pran0
pranx1 ~~ 0*aa1pran1
pranx1 ~~ 0*aa1pran0
pranx1 ~~ 0*aa0pran1
pranx1 ~~ 0*aa0pran0
pranx0 ~~ 0*aa1pran1
pranx0 ~~ 0*aa1pran0
pranx0 ~~ 0*aa0pran1
pranx0 ~~ 0*aa0pran0 
ccpt ~ k*AARanx_1

ccpt ~ 0*AARanx_0

ccpr ~ j*AARanx_1

ccpr ~ 0*AARanx_0 
ccpt ~ k*aa1pran1

ccpt ~ 0*aa1pran0

ccpt ~ 0*aa0pran1

ccpt ~ aa0pran0

ccpr ~ j*aa1pran1

ccpr ~ 0*aa1pran0

ccpr ~ j*aa0pran1

ccpr ~ 0*aa0pran0 
aa1pran0 ~~ an1pran0
AARanx_1 ~~ ECRanx_1
"
aaranx_mod_anx_mod_df <- rename(aaranx_mod_anx_mod_df, aa0pran1 = an0pran1, aa1pran1 = an1pran1,
                                aa1pran0 = an1pran0, aa0pran0 = an0pran0,
                                AARanx_0 = ECRanx_0, AARanx_1 = ECRanx_1) %>% dplyr::select(PTNUM, aa0pran1, aa1pran1,aa1pran0, aa0pran0, AARanx_0, AARanx_1)
tmp_df <- left_join(ecranx_mod_anx_mod_df, aaranx_mod_anx_mod_df)
anx_mod_anx_mod_m <- sem(anx_mod_anx_mod, tmp_df, mimic= "Mplus", estimator = "MLR") # min p-value for r on cc is .252; panx ~ cc is B = .11, p = .028

anx_mod_avo_mod_m <- runBSEM(modsyntax(tmp_df, anx_mod_anx_mod)) 


# Avo Anx N S1 ------------------------------------------------------------

ecravo_mod_anx_mod_syn <- models_to_save[["s1_n_ecr_syn_list"]][["ecravo_mod_anx_mod"]]
ecravo_mod_anx_mod_df <- models_to_save[["s1_n_ecr_df"]]
aaravo_mod_anx_mod_syn <- models_to_save[["s1_n_aar_syn_list"]][["ecravo_mod_anx_mod"]]
aaravo_mod_anx_mod_df <- models_to_save[["s1_n_aar_df"]]

avo_mod_anx_mod <- "

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
ccpt ~ ECRavo_1

ccpt ~ 0*ECRavo_0

ccpr ~ 0*ECRavo_1

ccpr ~ ECRavo_0 
ccpt ~ av1pran1

ccpt ~ av1pran0

ccpt ~ 0*av0pran1

ccpt ~ 0*av0pran0

ccpr ~ av1pran1

ccpr ~ av1pran0

ccpr ~ 0*av0pran1

ccpr ~ 0*av0pran0 

##AAR
AARavo_1 ~~ 0*aa1pran1 
AARavo_0 ~~ 0*aa1pran1 
AARavo_1 ~~ 0*aa1pran0 
AARavo_0 ~~ 0*aa1pran0 
AARavo_1 ~~ 0*aa0pran1 
AARavo_0 ~~ 0*aa0pran1 
AARavo_1 ~~ 0*aa0pran0 
AARavo_0 ~~ 0*aa0pran0
aa1pran1 ~~ aa1pran1 
aa1pran0 ~~ aa1pran0 
aa0pran1 ~~ aa0pran1 
aa0pran0 ~~ aa0pran0
pranx1 ~~ 0*aa1pran1
pranx1 ~~ 0*aa1pran0
pranx1 ~~ 0*aa0pran1
pranx1 ~~ 0*aa0pran0
pranx0 ~~ 0*aa1pran1
pranx0 ~~ 0*aa1pran0
pranx0 ~~ 0*aa0pran1
pranx0 ~~ 0*aa0pran0 
ccpt ~ 0*AARavo_1

ccpt ~ 0*AARavo_0

ccpr ~ 0*AARavo_1

ccpr ~ 0*AARavo_0 
ccpt ~ aa1pran1

ccpt ~ 0*aa1pran0

ccpt ~ 0*aa0pran1

ccpt ~ aaa*aa0pran0

ccpr ~ aaa*aa1pran1

ccpr ~ aaa*aa1pran0

ccpr ~ 0*aa0pran1

ccpr ~ 0*aa0pran0 
"
aaravo_mod_anx_mod_df <- rename(aaravo_mod_anx_mod_df, aa0pran1 = av0pran1, aa1pran1 = av1pran1,
                                aa1pran0 = av1pran0, aa0pran0 = av0pran0,
                                AARavo_0 = ECRavo_0, AARavo_1 = ECRavo_1) %>% dplyr::select(PTNUM, aa0pran1, aa1pran1,aa1pran0, aa0pran0, AARavo_0, AARavo_1)
tmp_df <- left_join(ecranx_mod_anx_mod_df, aaravo_mod_anx_mod_df)

avo_mod_anx_mod_m <- sem(avo_mod_anx_mod, tmp_df, mimic= "Mplus", estimator = "MLR") # min p-value for r on cc is .252; panx ~ cc is B = .11, p = .028

avo_mod_anx_mod_m <- runBSEM(modsyntax(tmp_df, avo_mod_anx_mod)) 

# Avo Anx N S2 ------------------------------------------------------------
s2_ecravo_mod_anx_mod_syn <- models_to_save[["s2_n_ecr_syn_list"]][["ecravo_mod_anx_mod"]]
s2_ecravo_mod_anx_mod_df <- models_to_save[["s2_n_ecr_df"]]
s2_aaravo_mod_anx_mod_syn <- models_to_save[["s2_n_aar_syn_list"]][["ecravo_mod_anx_mod"]]
s2_aaravo_mod_anx_mod_df <- models_to_save[["s2_n_aar_df"]]

s2_avo_mod_anx_mod <- "

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
## AAR

AARavo_1 ~~ 0*aa1ccpt1 
AARavo_0 ~~ 0*aa1ccpt1 
AARavo_1 ~~ 0*aa1ccpr0 
AARavo_0 ~~ 0*aa1ccpr0 
AARavo_1 ~~ 0*aa0ccpt1 
AARavo_0 ~~ 0*aa0ccpt1 
AARavo_1 ~~ 0*aa0ccpr0 
AARavo_0 ~~ 0*aa0ccpr0
aa1ccpt1 ~~ aa1ccpt1 
aa1ccpr0 ~~ aa1ccpr0 
aa0ccpt1 ~~ aa0ccpt1 
aa0ccpr0 ~~ aa0ccpr0
aa1ccpt1 ~~ 0*ccpt 
aa1ccpt1 ~~ 0*ccpr 
aa1ccpr0 ~~ 0*ccpt 
aa0ccpt1 ~~ 0*ccpt 
aa0ccpt1 ~~ 0*ccpr 
aa0ccpr0 ~~ 0*ccpt 
aa0ccpr0 ~~ 0*ccpr 
panx1 ~ 0*AARavo_1

panx1 ~ lll*AARavo_0

panx0 ~ lll*AARavo_1

panx0 ~ 0*AARavo_0 
panx1 ~ 0*aa1ccpt1

panx1 ~ lll*aa0ccpt1

panx0 ~ 0*aa1ccpt1

panx0 ~ lll*aa1ccpr0

panx0 ~ 0*aa0ccpt1

panx0 ~ aa0ccpr0 
av1ccpt1 ~~ aa1ccpt1
av1ccpr0 ~~ av1ccpt1
ccpr ~~ aa1ccpr0
av0ccpr0 ~~ av1ccpt1
aa1ccpr0 ~~ av1ccpr0
aa1ccpt1 ~~ aa1ccpr0
"
s2_aaravo_mod_anx_mod_df <- rename(s2_aaravo_mod_anx_mod_df, aa1ccpt1 = av1ccpt1,
                                   aa0ccpt1 = av0ccpt1, aa0ccpr0 = av0ccpr0,
                                   aa1ccpr0 = av1ccpr0,
                                AARavo_0 = ECRavo_0, AARavo_1 = ECRavo_1, AARanx_1 = ECRanx_1, AARanx_0 = ECRanx_0) %>% dplyr::select(PTNUM, starts_with("aa"), AARavo_0, AARavo_1, AARanx_1, AARanx_0)
tmp_df <- left_join(s2_ecravo_mod_anx_mod_df, s2_aaravo_mod_anx_mod_df)
s2_avo_mod_anx_mod_m <- sem(s2_avo_mod_anx_mod, tmp_df, mimic= "Mplus", estimator = "MLR") # min p-value for r on cc is .252; panx ~ cc is B = .11, p = .028

avo_mod_anx_mod_m <- runBSEM(modsyntax(tmp_df, s2_avo_mod_anx_mod)) 


ecr_aar <- "
ccpt ~ AARavo_1 
ccpt ~ AARavo_0 
ccpt ~ ECRavo_1 
ccpt ~ ECRavo_0
ccpt ~ AARanx_1 
ccpt ~ AARanx_0 
ccpt ~ ECRanx_1 
ccpt ~ ECRanx_0

ccpr ~ AARavo_1 
ccpr ~ AARavo_0 
ccpr ~ ECRavo_1 
ccpr ~ ECRavo_0
ccpr ~ AARanx_1 
ccpr ~ AARanx_0 
ccpr ~ ECRanx_1 
ccpr ~ ECRanx_0

"
ecr_aar_m <- runBSEM(modsyntax(tmp_df, ecr_aar)) 



