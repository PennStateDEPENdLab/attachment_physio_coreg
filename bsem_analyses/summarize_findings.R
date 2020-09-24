# Set up paths ------------------------------------------------------------
## assume wd is the file where this file is located. 
# Source info and functions --------------------------------------------------------

source("../support_fx.R")


# Load Packages -----------------------------------------------------------

if (!require(pacman)) { install.packages("pacman"); library(pacman) }
p_load(tidyverse, R.matlab,lavaan,lattice, MplusAutomation)



# Load Model Objects ------------------------------------------------------

load("../output/bsem_personalitymod/models_run.RData")
#load("../output/bsem_personalitymod/models_run_cominbed.RData"))

load("../output/bsem_process/model_strings.RData")

negint_avo_mod <- syn_model_list_full[["negint_avo_mod"]]
negint_anx_mod <- syn_model_list_full[["negint_anx_mod"]]
negint_sec_mod <- syn_model_list_full[["negint_sec_mod"]]
posint_avo_mod <- syn_model_list_full[["posint_avo_mod"]]
posint_anx_mod <- syn_model_list_full[["posint_anx_mod"]]
posint_sec_mod <- syn_model_list_full[["posint_sec_mod"]]
posnegint_personality <- models_to_save[["s1_n_ecr_df"]]

negint_avo_mod_m <- sem(model = negint_avo_mod, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
posint_avo_mod_m <- sem(model = posint_avo_mod, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
negint_anx_mod_m <- sem(model = negint_anx_mod, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
posint_anx_mod_m <- sem(model = posint_anx_mod, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
negint_sec_mod_m <- sem(model = negint_sec_mod, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
posint_sec_mod_m <- sem(model = posint_sec_mod, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
negint_avo_mod_bsem_m <- runBSEM(modsyntax(posnegint_personality, negint_avo_mod))
posint_avo_mod_bsem_m <- runBSEM(modsyntax(posnegint_personality, posint_avo_mod))

negint_anx_mod_bsem_m <- runBSEM(modsyntax(posnegint_personality, negint_anx_mod))
posint_anx_mod_bsem_m <- runBSEM(modsyntax(posnegint_personality, posint_anx_mod))

# Process Signals ---------------------------------------------------------
summary(negint_avo_mod_m)
summary(posint_avo_mod_m)

## Increases in avoidance mediated by contrarianism during negative interaction
summary(negint_anx_mod_m)
summary(posint_anx_mod_m)
# Posint anx -- note the sign flipping phenomenon and how best to handle that > probs separate out...
# during the negative interaction, patient's security elicits partner's becoming physiologically pulled into patient. If patient pulled into partner's physiology, report increases in security afterwards. 
# during the positive interaction partner's baseline security predicts patients becoming physiologically entrained with partner and predicts partner pulling away physiologically 

summary(negint_sec_mod_m)
summary(posint_sec_mod_m)
# increases in security in patient are associated with patients being physiologically entrained with partner.
# Baseline security in patients predicts partners becoming pulled into patient's physiology


# Signal 1: Avoidance Moderates Avoidance ---------------------------------
#ECR
summary(models_to_save[["s1_n_ecr_model_list"]][["ecravo_mod_avo_mod_m"]]) 
ecravo_mod_avo_mod_syn <- models_to_save[["s1_n_ecr_syn_list"]][["ecravo_mod_avo_mod"]]
ecravo_mod_avo_mod_df <- models_to_save[["s1_n_ecr_df"]]
ecravo_mod_avo_mod_bsem_m <- runBSEM(modsyntax(ecravo_mod_avo_mod_df, ecravo_mod_avo_mod_syn))

summary(models_to_save[["s2_n_ecr_model_list"]][["ecravo_mod_avo_mod_m"]]) # NOTHING
summary(models_to_save[["s1_p_ecr_model_list"]][["ecravo_mod_avo_mod_m"]]) 
p_ecravo_mod_avo_mod_syn <- models_to_save[["s1_p_ecr_syn_list"]][["ecravo_mod_avo_mod"]] # .348
p_ecravo_mod_avo_mod_df <- models_to_save[["s1_p_ecr_df"]]
p_ecravo_mod_avo_mod_bsem_m <- runBSEM(modsyntax(p_ecravo_mod_avo_mod_df, p_ecravo_mod_avo_mod_syn))
# No interactions
#AAR
summary(models_to_save[["s1_n_aar_model_list"]][["ecravo_mod_avo_mod_m"]])
aaravo_mod_avo_mod_syn <- models_to_save[["s1_n_aar_syn_list"]][["ecravo_mod_avo_mod"]]
aaravo_mod_avo_mod_df <- models_to_save[["s1_n_aar_df"]]
aaravo_mod_avo_mod_bsem_m <- runBSEM(modsyntax(aaravo_mod_avo_mod_df, aaravo_mod_avo_mod_syn))

summary(models_to_save[["s2_n_aar_model_list"]][["ecravo_mod_avo_mod_m"]]) # NOTHING
summary(models_to_save[["s1_p_aar_model_list"]][["ecravo_mod_avo_mod_m"]]) #NOTHING
p_aaravo_mod_avo_mod_syn <- models_to_save[["s1_p_aar_syn_list"]][["ecravo_mod_avo_mod"]]
p_aaravo_mod_avo_mod_df <- models_to_save[["s1_p_aar_df"]]
p_aaravo_mod_avo_mod_bsem_m <- runBSEM(modsyntax(p_aaravo_mod_avo_mod_df, p_aaravo_mod_avo_mod_syn))

# Signal 2: Avoidance gates anxiety ----------------------------------------------------------------

summary(models_to_save[["s1_n_ecr_model_list"]][["ecravo_mod_anx_mod_m"]]) # nothing
summary(models_to_save[["s2_n_ecr_model_list"]][["ecravo_mod_anx_mod_m"]])# doesn't hold up under bsem
ecravo_mod_anx_mod_syn <- models_to_save[["s2_n_ecr_syn_list"]][["ecravo_mod_anx_mod"]] 
ecravo_mod_anx_mod_df <- models_to_save[["s2_n_ecr_df"]]
ecravo_mod_anx_mod_bsem_m <- runBSEM(modsyntax(ecravo_mod_anx_mod_df, ecravo_mod_anx_mod_syn))

summary(models_to_save[["s1_p_ecr_model_list"]][["ecravo_mod_anx_mod_m"]]) # Nothing
summary(models_to_save[["s2_p_ecr_model_list"]][["ecravo_mod_anx_mod_m"]])
p_ecravo_mod_anx_mod_syn <- models_to_save[["s2_p_ecr_syn_list"]][["ecravo_mod_anx_mod"]] 
p_ecravo_mod_anx_mod_df <- models_to_save[["s2_p_ecr_df"]]
p_ecravo_mod_anx_mod_bsem_m <- runBSEM(modsyntax(p_ecravo_mod_anx_mod_df, p_ecravo_mod_anx_mod_syn))

summary(models_to_save[["s1_n_aar_model_list"]][["ecravo_mod_anx_mod_m"]])
aaravo_mod_anx_mod_syn <- models_to_save[["s1_n_aar_syn_list"]][["ecravo_mod_anx_mod"]] 
aaravo_mod_anx_mod_df <- models_to_save[["s1_n_aar_df"]]
aaravo_mod_anx_mod_bsem_m <- runBSEM(modsyntax(aaravo_mod_anx_mod_df, aaravo_mod_anx_mod_syn))

summary(models_to_save[["s2_n_aar_model_list"]][["ecravo_mod_anx_mod_m"]]) 
aaravo2_mod_anx_mod_syn <- models_to_save[["s2_n_aar_syn_list"]][["ecravo_mod_anx_mod"]] 
aaravo2_mod_anx_mod_df <- models_to_save[["s2_n_aar_df"]]
aaravo2_mod_anx_mod_bsem_m <- runBSEM(modsyntax(aaravo2_mod_anx_mod_df, aaravo2_mod_anx_mod_syn))

summary(models_to_save[["s1_p_aar_model_list"]][["ecravo_mod_anx_mod_m"]]) # did not survive bsem
p_aaravo_mod_anx_mod_syn <- models_to_save[["s1_p_aar_syn_list"]][["ecravo_mod_anx_mod"]] 
p_aaravo_mod_anx_mod_df <- models_to_save[["s1_p_aar_df"]]
p_aaravo_mod_anx_mod_bsem_m <- runBSEM(modsyntax(p_aaravo_mod_anx_mod_df, p_aaravo_mod_anx_mod_syn))
summary(models_to_save[["s2_p_aar_model_list"]][["ecravo_mod_anx_mod_m"]])  
p2_aaravo_mod_anx_mod_syn <- models_to_save[["s2_p_aar_syn_list"]][["ecravo_mod_anx_mod"]] 
p2_aaravo_mod_anx_mod_df <- models_to_save[["s2_p_aar_df"]]
p2_aaravo_mod_anx_mod_bsem_m <- runBSEM(modsyntax(p2_aaravo_mod_anx_mod_df, p2_aaravo_mod_anx_mod_syn))


# Signal 3: Axiety gates avoidance ----------------------------------------------------------------

summary(models_to_save[["s1_n_ecr_model_list"]][["ecranx_mod_avo_mod_m"]]) # Nothing
ecranx_mod_avo_mod_syn <- models_to_save[["s1_n_ecr_syn_list"]][["ecranx_mod_avo_mod"]]
ecranx_mod_avo_mod_df <- models_to_save[["s1_n_ecr_df"]]
ecranx_mod_avo_mod_bsem_m <- runBSEM(modsyntax(ecranx_mod_avo_mod_df, ecranx_mod_avo_mod_syn))

summary(models_to_save[["s2_n_ecr_model_list"]][["ecranx_mod_avo_mod_m"]]) # Nothing
summary(models_to_save[["s1_p_ecr_model_list"]][["ecranx_mod_avo_mod_m"]]) # Nothing

summary(models_to_save[["s1_n_aar_model_list"]][["ecranx_mod_avo_mod_m"]]) #excessive dep
aardep_mod_avo_mod_syn <- models_to_save[["s1_n_aar_syn_list"]][["ecranx_mod_avo_mod"]]
aardep_mod_avo_mod_df <- models_to_save[["s1_n_aar_df"]]
aardep_mod_avo_mod_bsem_m <- runBSEM(modsyntax(aardep_mod_avo_mod_df, aardep_mod_avo_mod_syn))

summary(models_to_save[["s2_n_aar_model_list"]][["ecranx_mod_avo_mod_m"]])#nothing
summary(models_to_save[["s1_p_aar_model_list"]][["ecranx_mod_avo_mod_m"]]) #Nothing 
p_aardep_mod_avo_mod_syn <- models_to_save[["s1_p_aar_syn_list"]][["ecranx_mod_avo_mod"]]
p_aardep_mod_avo_mod_df <- models_to_save[["s1_p_aar_df"]]
p_aardep_mod_avo_mod_bsem_m <- runBSEM(modsyntax(p_aardep_mod_avo_mod_df, p_aardep_mod_avo_mod_syn))


summary(models_to_save[["s1_n_aar2_model_list"]][["ecranx_mod_avo_mod_m"]]) #amb -> does not hold true under bsem
aaramb_mod_avo_mod_syn <- models_to_save[["s1_n_aar2_syn_list"]][["ecranx_mod_avo_mod"]]
aaramb_mod_avo_mod_df <- models_to_save[["s1_n_aar2_df"]]
aaramb_mod_avo_mod_bsem_m <- runBSEM(modsyntax(aaramb_mod_avo_mod_df, aaramb_mod_avo_mod_syn))

summary(models_to_save[["s2_n_aar2_model_list"]][["ecranx_mod_avo_mod_m"]]) #nothing
summary(models_to_save[["s1_p_aar2_model_list"]][["ecranx_mod_avo_mod_m"]]) #Nothing 
p_aaramb_mod_avo_mod_syn <- models_to_save[["s1_p_aar2_syn_list"]][["ecranx_mod_avo_mod"]]
p_aaramb_mod_avo_mod_df <- models_to_save[["s1_p_aar2_df"]]
p_aaramb_mod_avo_mod_bsem_m <- runBSEM(modsyntax(p_aaramb_mod_avo_mod_df, p_aaramb_mod_avo_mod_syn))

# Signal 4: Anxiety gates anxiety ----------------------------------------------------------------

summary(models_to_save[["s1_n_ecr_model_list"]][["ecranx_mod_anx_mod_m"]])
ecranx_mod_anx_mod_syn <- models_to_save[["s1_n_ecr_syn_list"]][["ecranx_mod_anx_mod"]]
ecranx_mod_anx_mod_df <- models_to_save[["s1_n_ecr_df"]]
ecranx_mod_anx_mod_bsem_m <- runBSEM(modsyntax(ecranx_mod_anx_mod_df, ecranx_mod_anx_mod_syn))

summary(models_to_save[["s2_n_ecr_model_list"]][["ecranx_mod_anx_mod_m"]]) 
ecranx2_mod_anx_mod_syn <- models_to_save[["s2_n_ecr_syn_list"]][["ecranx_mod_anx_mod"]]
ecranx2_mod_anx_mod_df <- models_to_save[["s2_n_ecr_df"]]
ecranx2_mod_anx_mod_bsem_m <- runBSEM(modsyntax(ecranx2_mod_anx_mod_df, ecranx2_mod_anx_mod_syn))

summary(models_to_save[["s1_p_ecr_model_list"]][["ecranx_mod_anx_mod_m"]]) 
p_ecranx_mod_anx_mod_syn <- models_to_save[["s1_p_ecr_syn_list"]][["ecranx_mod_anx_mod"]]
p_ecranx_mod_anx_mod_df <- models_to_save[["s1_p_ecr_df"]]
p_ecranx_mod_anx_mod_bsem_m <- runBSEM(modsyntax(p_ecranx_mod_anx_mod_df, p_ecranx_mod_anx_mod_syn))

summary(models_to_save[["s2_p_ecr_model_list"]][["ecranx_mod_anx_mod_m"]])
p2_ecranx_mod_anx_mod_syn <- models_to_save[["s2_p_ecr_syn_list"]][["ecranx_mod_anx_mod"]]
p2_ecranx_mod_anx_mod_df <- models_to_save[["s2_p_ecr_df"]]
p2_ecranx_mod_anx_mod_bsem_m <- runBSEM(modsyntax(p2_ecranx_mod_anx_mod_df, p2_ecranx_mod_anx_mod_syn))

summary(models_to_save[["s1_n_aar_model_list"]][["ecranx_mod_anx_mod_m"]])
aardep_mod_anx_mod_syn <- models_to_save[["s1_n_aar_syn_list"]][["ecranx_mod_anx_mod"]]
aardep_mod_anx_mod_df <- models_to_save[["s1_n_aar_df"]]
aardep_mod_anx_mod_bsem_m <- runBSEM(modsyntax(aardep_mod_anx_mod_df, aardep_mod_anx_mod_syn))

summary(models_to_save[["s2_n_aar_model_list"]][["ecranx_mod_anx_mod_m"]]) #nothing
summary(models_to_save[["s1_p_aar_model_list"]][["ecranx_mod_anx_mod_m"]]) #Nothing 
summary(models_to_save[["s2_p_aar_model_list"]][["ecranx_mod_anx_mod_m"]])  
p_aardep_mod_anx_mod_syn <- models_to_save[["s2_p_aar_syn_list"]][["ecranx_mod_anx_mod"]]
p_aardep_mod_anx_mod_df <- models_to_save[["s2_p_aar_df"]]
p_aardep_mod_anx_mod_bsem_m <- runBSEM(modsyntax(p_aardep_mod_anx_mod_df, p_aardep_mod_anx_mod_syn))


summary(models_to_save[["s1_n_aar2_model_list"]][["ecranx_mod_anx_mod_m"]]) # nothing
summary(models_to_save[["s2_n_aar2_model_list"]][["ecranx_mod_anx_mod_m"]]) # nothing
summary(models_to_save[["s1_p_aar2_model_list"]][["ecranx_mod_anx_mod_m"]]) 
p_aaramb_mod_anx_mod_syn <- models_to_save[["s1_p_aar2_syn_list"]][["ecranx_mod_anx_mod"]]
p_aaramb_mod_anx_mod_df <- models_to_save[["s1_p_aar2_df"]]
p_aaramb_mod_anx_mod_bsem_m <- runBSEM(modsyntax(p_aaramb_mod_anx_mod_df, p_aaramb_mod_anx_mod_syn))

summary(models_to_save[["s2_p_aar2_model_list"]][["ecranx_mod_anx_mod_m"]])  
p2_aaramb_mod_anx_mod_syn <- models_to_save[["s2_p_aar2_syn_list"]][["ecranx_mod_anx_mod"]]
p2_aaramb_mod_anx_mod_df <- models_to_save[["s2_p_aar2_df"]]
p2_aaramb_mod_anx_mod_bsem_m <- runBSEM(modsyntax(p2_aaramb_mod_anx_mod_df, p2_aaramb_mod_anx_mod_syn))


# Supplemental Analyses ---------------------------------------------------



# Security gates avoidance ------------------------------------------------

summary(models_to_save[["s1_n_aar2_model_list"]][["ecravo_mod_avo_mod_m"]]) 
aarsec_mod_avo_mod_syn <- models_to_save[["s1_n_aar2_syn_list"]][["ecravo_mod_avo_mod"]]
aarsec_mod_avo_mod_syn <- "
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

 ECRavo_0 ~~ 0*av1prav1 
ECRavo_1 ~~ 0*av0prav1 
av1prav1 ~~ av1prav1 
av0prav1 ~~ av0prav1 
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
ECRavo_1 ~~ av1prav1
pravo1 ~~ ECRavo_1
pravo1 ~~ av1prav1
ECRavo_0 ~~ av0prav1
pravo0 ~~ ECRavo_0
av0prav1 ~~ pavo0
"
aarsec_mod_avo_mod_df <- models_to_save[["s1_n_aar2_df"]]
aarsec_mod_avo_mod_syn_m <- sem(aarsec_mod_avo_mod_syn, aarsec_mod_avo_mod_df, mimic= "Mplus", estimator = "MLR") # min p-value for r on cc is .252; panx ~ cc is B = .11, p = .028

aarsec_mod_avo_mod_bsem_m <- runBSEM(modsyntax(aarsec_mod_avo_mod_df, aarsec_mod_avo_mod_syn))
summary(models_to_save[["s2_n_aar2_model_list"]][["ecravo_mod_avo_mod_m"]]) # nothing
summary(models_to_save[["s1_p_aar2_model_list"]][["ecravo_mod_avo_mod_m"]]) #nothing 
p_aarsec_mod_avo_mod_syn <- models_to_save[["s1_p_aar2_syn_list"]][["ecravo_mod_avo_mod"]]
p_aarsec_mod_avo_mod_df <- models_to_save[["s1_p_aar2_df"]]
p_aarsec_mod_avo_mod_syn <- "
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
 ECRavo_0 ~~ 0*av1prav1 
ECRavo_1 ~~ 0*av0prav1 
av1prav1 ~~ av1prav1 
av0prav1 ~~ av0prav1 
pravo1 ~~ 0*av1prav1
pravo1 ~~ 0*av0prav1
pravo0 ~~ 0*av1prav1
pravo0 ~~ 0*av0prav1 
p_ccpr ~ ECRavo_1

p_ccpr ~ ECRavo_0 
p_ccpr ~ av1prav1

p_ccpr ~ av0prav1 
av1prav1 ~~ ECRavo_1
pravo1 ~~ ECRavo_1
ECRavo_0 ~~ av0prav1
pravo0 ~~ ECRavo_0
"
p_aarsec_mod_avo_mod_syn_m <- sem(p_aarsec_mod_avo_mod_syn, p_aarsec_mod_avo_mod_df, mimic= "Mplus", estimator = "MLR") # min p-value for r on cc is .252; panx ~ cc is B = .11, p = .028

p_aarsec_mod_avo_mod_bsem_m <- runBSEM(modsyntax(p_aarsec_mod_avo_mod_df, p_aarsec_mod_avo_mod_syn)) #.119*2



# Security gates anxiety --------------------------------------------------

summary(models_to_save[["s1_n_aar2_model_list"]][["ecravo_mod_anx_mod_m"]]) 
aarsec_mod_anx_mod_syn <- models_to_save[["s1_n_aar2_syn_list"]][["ecravo_mod_anx_mod"]]
aarsec_mod_anx_mod_df <- models_to_save[["s1_n_aar2_df"]]
aarsec_mod_anx_mod_bsem_m <- runBSEM(modsyntax(aarsec_mod_anx_mod_df, aarsec_mod_anx_mod_syn))

summary(models_to_save[["s2_n_aar2_model_list"]][["ecravo_mod_anx_mod_m"]]) # nothing
summary(models_to_save[["s1_p_aar2_model_list"]][["ecravo_mod_anx_mod_m"]]) 
p_aarsec_mod_anx_mod_syn <- models_to_save[["s1_p_aar2_syn_list"]][["ecravo_mod_anx_mod"]]
p_aarsec_mod_anx_mod_df <- models_to_save[["s1_p_aar2_df"]]
p_aarsec_mod_anx_mod_bsem_m <- runBSEM(modsyntax(p_aarsec_mod_anx_mod_df, p_aarsec_mod_anx_mod_syn))

summary(models_to_save[["s2_p_aar2_model_list"]][["ecravo_mod_anx_mod_m"]]) 
p2_aarsec_mod_anx_mod_syn <- models_to_save[["s2_p_aar2_syn_list"]][["ecravo_mod_anx_mod"]]
p2_aarsec_mod_anx_mod_df <- models_to_save[["s2_p_aar2_df"]]
p2_aarsec_mod_anx_mod_syn <- "

p_ccpt ~ b*panx1 #a
p_ccpt ~ a*pranx1 #a
p_ccpr ~ b*panx0 # a
p_ccpr ~ a*pranx0 #a


pranx1 ~~ pranx0
panx1~~panx0
p_ccpt ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

 #ECRavo_1 ~~ 0*av1pan1 
ECRavo_0 ~~ 0*av1pan1 
#ECRavo_1 ~~ 0*av1pan0 
ECRavo_0 ~~ 0*av1pan0 
ECRavo_1 ~~ 0*av0pan1 
#ECRavo_0 ~~ 0*av0pan1 
ECRavo_1 ~~ 0*av0pan0 
#ECRavo_0 ~~ 0*av0pan0
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
ECRavo_0 ~~ av0pan0
av1pan0 ~~ av0pan0
ECRavo_1 ~~ av1pan1
av1pan0 ~~ ECRavo_1
av1pan1 ~~ av1pan0
ECRavo_0 ~~ av0pan1
"
p2_aarsec_mod_anx_mod_syn_m <- sem(p2_aarsec_mod_anx_mod_syn, p2_aarsec_mod_anx_mod_df, mimic= "Mplus", estimator = "MLR") # min p-value for r on cc is .252; panx ~ cc is B = .11, p = .028

p2_aarsec_mod_anx_mod_bsem_m <- runBSEM(modsyntax(p2_aarsec_mod_anx_mod_df, p2_aarsec_mod_anx_mod_syn))



# Security gates security -------------------------------------------------

summary(models_to_save[["s1_n_aar2_model_list"]][["ecravo_mod_sec_mod_m"]]) 
summary(models_to_save[["s2_n_aar2_model_list"]][["ecravo_mod_sec_mod_m"]]) 
summary(models_to_save[["s1_p_aar2_model_list"]][["ecravo_mod_sec_mod_m"]]) #Nothing 



# Avoidance gates security ------------------------------------------------


summary(models_to_save[["s1_n_ecr_model_list"]][["ecravo_mod_sec_mod_m"]])
summary(models_to_save[["s2_n_ecr_model_list"]][["ecravo_mod_sec_mod_m"]]) 
summary(models_to_save[["s1_p_ecr_model_list"]][["ecravo_mod_sec_mod_m"]])

summary(models_to_save[["s1_n_aar_model_list"]][["ecravo_mod_sec_mod_m"]])
summary(models_to_save[["s2_n_aar_model_list"]][["ecravo_mod_sec_mod_m"]]) 
summary(models_to_save[["s1_p_aar_model_list"]][["ecravo_mod_sec_mod_m"]])

# Anxiety gates security --------------------------------------------------



summary(models_to_save[["s1_n_ecr_model_list"]][["ecranx_mod_sec_mod_m"]]) # need to rerun process_ECRmod for this to wokr 
summary(models_to_save[["s2_n_ecr_model_list"]][["ecranx_mod_sec_mod_m"]]) 
summary(models_to_save[["s1_p_ecr_model_list"]][["ecranx_mod_sec_mod_m"]])

summary(models_to_save[["s1_n_aar_model_list"]][["ecranx_mod_sec_mod_m"]])
summary(models_to_save[["s2_n_aar_model_list"]][["ecranx_mod_sec_mod_m"]]) 
summary(models_to_save[["s1_p_aar_model_list"]][["ecranx_mod_sec_mod_m"]])

summary(models_to_save[["s1_n_aar2_model_list"]][["ecranx_mod_sec_mod_m"]])
summary(models_to_save[["s2_n_aar2_model_list"]][["ecranx_mod_sec_mod_m"]]) 
summary(models_to_save[["s1_p_aar2_model_list"]][["ecranx_mod_sec_mod_m"]])


# Understanding the discrepancies -----------------------------------------
## Here, comparing differences between AAR avo, AAR anx, ECR avo and ECR anx
df1 <- models_to_save[["s1_n_ecr_df"]] %>% dplyr::select(PTNUM, ECRavo_0, ECRavo_1, ECRanx_1, ECRanx_0)
df2 <- models_to_save[["s1_n_aar_df"]] %>% dplyr::select(PTNUM, ECRavo_0, ECRavo_1, ECRanx_1, ECRanx_0)
names(df2) <- c("PTNUM", "AARavo_0", "AARavo_1", "AARanx_1", "AARanx_0")
df1 <- left_join(df1, df2)
df1_long <- gather(df1, key = "key", value = "value", -PTNUM)  %>% separate(key, into = c("State", "Trait"),sep = 3) %>% spread(key = "State", value = "value")
ggplot(df1_long, aes(ECR, AAR)) %>% geom_point() + facet_wrap(~Trait) 







