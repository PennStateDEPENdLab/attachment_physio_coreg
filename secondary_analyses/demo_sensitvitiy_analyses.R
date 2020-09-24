
# Set up paths ------------------------------------------------------------
# assumes that wd is where this file is located in file structure

# Source info and functions --------------------------------------------------------

source("../support_fx.R")


# Load Packages -----------------------------------------------------------

if (!require(pacman)) { install.packages("pacman"); library(pacman) }
p_load(tidyverse, R.matlab,lavaan,lattice, MplusAutomation)


# Load Data ---------------------------------------------------------------

posnegint_personality <-read.csv("../data/posnegint_personality.csv")
demo_df <- read.csv("../data/demo_df.csv") %>% dplyr::select(PTNUM, p_age_0, p_age_1, sex_num_1, sex_num_0, race_num_1, race_num_0,gaf_1, gaf_0) %>% rename(age0 = p_age_0,
                                                                                                                                                            age1 = p_age_1, 
                                                                                                                                                            f1 = sex_num_1, # sex num codes females as 1 and males as 0
                                                                                                                                                            f0 = sex_num_0,
                                                                                                                                                            r1 = race_num_1, 
                                                                                                                                                            r0 = race_num_0)
posnegint_personality <- left_join(posnegint_personality, demo_df)
# Load Model String for Simpler Process Models ----------------------------

load(paste0(project_dir, "/output/bsem_process/model_strings.RData"))

negint_avo_mod <- syn_model_list_full[["negint_avo_mod"]]
negint_anx_mod <- syn_model_list_full[["negint_anx_mod"]]
r_negint_avo_mod <- "
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
ccpt ~ r1 + r0
ccpr ~ r1 + r0

r1 ~~ r0

"
r_negint_avo_mod_m <- runBSEM(modsyntax(posnegint_personality, r_negint_avo_mod)) # indirect effect qualitatively similar that quantitaively only marginal -- driven by the inclusion of race as a competitive predictor on coreg (even though p = .26 for r)

f_negint_avo_mod <- "
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
ccpt ~ f1 + f0
ccpr ~ f1 + f0

f1 ~~ f0

"
f_negint_avo_mod_m <- runBSEM(modsyntax(posnegint_personality, f_negint_avo_mod)) # min p-value for f on cc is .061; indirect effect remains significant


age_negint_avo_mod <- "
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
ccpt ~ age1 + age0
ccpr ~ age1 + age0

age1 ~~ age0

"
age_negint_avo_mod_m <- runBSEM(modsyntax(posnegint_personality, age_negint_avo_mod)) # min p is .41; indirect effect remains significant

r_negint_anx_mod <- "
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
ccpt ~ r0 + r1
ccpr ~ r0 + r1
r0 ~~ r1

"
r_negint_anx_mod_m <- runBSEM(modsyntax(posnegint_personality, r_negint_anx_mod)) # min p-value for r on cc is .252; panx ~ cc is B = .11, p = .028

f_negint_anx_mod <- "
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
ccpt ~ f0 + f1
ccpr ~ f0 + f1
f0 ~~ f1

"
f_negint_anx_mod_m <- runBSEM(modsyntax(posnegint_personality, f_negint_anx_mod)) # min p-value is .068; effect of cc on panx is still p = .028

age_negint_anx_mod <- "
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
ccpt ~ age0 + age1
ccpr ~ age0 + age1
age0 ~~ age1

"
age_negint_anx_mod_m <- runBSEM(modsyntax(posnegint_personality, age_negint_anx_mod)) # min p-value is .512; p-value is still .028


r_cc <- "
ccpt ~ r0 + r1
ccpr ~ r0 + r1

"
r_cc_m <- runBSEM(modsyntax(posnegint_personality, r_cc)) # .218 min p


f_cc <- "
ccpt ~ f0 + f1
ccpr ~ f0 + f1

"
f_cc_m <- runBSEM(modsyntax(posnegint_personality, f_cc)) # .051 min p

age_cc <- "
ccpt ~ age0 + age1
ccpr ~ age0 + age1

"
age_cc_m <- runBSEM(modsyntax(posnegint_personality, age_cc)) #.43 min p

posnegint_personality <- left_join(posnegint_personality, gaf)
posnegint_personality <- haven::zap_labels(posnegint_personality)

gaf_negint_anx_mod <- "
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
ccpt ~ gaf_0 + gaf_1
ccpr ~ gaf_0 + gaf_1
gaf_0 ~~ gaf_1

"
gaf_negint_anx_mod_m <- runBSEM(modsyntax(posnegint_personality, gaf_negint_anx_mod)) # min p-value for r on cc is .252; panx ~ cc is B = .11, p = .028



gaf_negint_avo_mod <- "
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
ccpt ~ gaf_1 + gaf_0
ccpr ~ gaf_1 + gaf_0

gaf_1 ~~ gaf_0

"
gaf_negint_avo_mod_m <- runBSEM(modsyntax(posnegint_personality, gaf_negint_avo_mod)) # indirect effect qualitatively similar that quantitaively only marginal -- driven by the inclusion of race as a competitive predictor on coreg (even though p = .26 for r)


