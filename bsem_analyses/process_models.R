
# Set up paths ------------------------------------------------------------
output_dir <- "../output"
bsem_process_res_dir <- "../output/bsem_process"
bsem_personalitymod_res_dir <- "../output/bsem_personalitymod"
if(!dir.exists(output_dir)) {dir.create(output_dir)}
if(!dir.exists(bsem_process_res_dir)) {dir.create(bsem_process_res_dir)}
if(!dir.exists(bsem_personalitymod_res_dir)) {dir.create(bsem_personalitymod_res_dir)}

# Source functions --------------------------------------------------------
source(paste0(project_dir, "/code/support_fx.R"))

# Load Packages -----------------------------------------------------------

if (!require(pacman)) { install.packages("pacman"); library(pacman) }
p_load(tidyverse, R.matlab,lavaan,lattice, MplusAutomation)

# Load Data ---------------------------------------------------------------

posnegint_personality <- read.csv("../data/posnegint_personality.csv")

# Process Model for Negint Avo --------------------------------------------



avo_allfree <- "
pavo1 ~ scpt + ccpt + scpr + ccpr
pavo0 ~ scpt + ccpt + scpr + ccpr
scpt ~ pravo1 + pravo0
ccpt ~ pravo1 + pravo0
scpr ~ pravo1 + pravo0
ccpr ~ pravo1 + pravo0
pravo1 ~~ pravo0
pavo1~~pavo0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr

pavo1~pravo1
pavo0 ~ pravo0

"

avo_allfree_m <- sem(model = avo_allfree, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

avo_afixed <- "
pavo1 ~ a*scpt
pavo1 ~  b*ccpt
pavo1 ~ scpr 
pavo1 ~ ccpr
pavo0 ~ scpt
pavo0 ~ ccpt
pavo0 ~ a*scpr
pavo0 ~ b*ccpr
scpt ~ c*pravo1 
scpt ~ pravo0
ccpt ~ d*pravo1 
ccpt ~ pravo0
scpr ~ pravo1 
scpr ~c*pravo0
ccpr ~ pravo1
ccpr ~ d*pravo0
pravo1 ~~ pravo0
pavo1~~pavo0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr


pavo1~pravo1
pavo0 ~ pravo0
"

avo_afixed_m <- sem(model = avo_afixed, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

avo_pfixed <- "
pavo1 ~ scpt
pavo1 ~  ccpt
pavo1 ~ a*scpr 
pavo1 ~ b*ccpr
pavo0 ~ a*scpt
pavo0 ~ b*ccpt
pavo0 ~ scpr
pavo0 ~ ccpr
scpt ~ pravo1 
scpt ~ c*pravo0
ccpt ~ pravo1 
ccpt ~ d*pravo0
scpr ~ c*pravo1 
scpr ~pravo0
ccpr ~ d*pravo1
ccpr ~ pravo0
pravo1 ~~ pravo0
pavo1~~pavo0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr

pavo1~pravo1
pavo0 ~ pravo0

"

avo_pfixed_m <- sem(model = avo_pfixed, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


avo_apfixed <- "
pavo1 ~ e*scpt
pavo1 ~  f*ccpt
pavo1 ~ a*scpr 
pavo1 ~ b*ccpr
pavo0 ~ a*scpt
pavo0 ~ b*ccpt
pavo0 ~ e*scpr
pavo0 ~ f*ccpr
scpt ~ g*pravo1 
scpt ~ c*pravo0
ccpt ~ h*pravo1 
ccpt ~ d*pravo0
scpr ~ c*pravo1 
scpr ~g*pravo0
ccpr ~ d*pravo1
ccpr ~ h*pravo0
pravo1 ~~ pravo0
pavo1~~pavo0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr


pavo1~pravo1
pavo0 ~ pravo0
"

avo_apfixed_m <- sem(model = avo_apfixed, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)






avo_aonly <- "
pavo1 ~ scpt
pavo1 ~  a*ccpt
pavo0 ~ scpr
pavo0 ~ ccpr
scpt ~ pravo1 
ccpt ~ c*pravo1 
scpr ~pravo0
ccpr ~ pravo0
pravo1 ~~ pravo0
pavo1~~pavo0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr
ac:=a*c

pavo1~pravo1
pavo0 ~ pravo0
"

avo_aonly_m <- sem(model = avo_aonly, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


avo_ponly <- "
pavo1 ~ scpr 
pavo1 ~ ccpr
pavo0 ~ scpt
pavo0 ~ ccpt
scpt ~ pravo0
ccpt ~ pravo0
scpr ~ pravo1 
ccpr ~ pravo1
pravo1 ~~ pravo0
pavo1~~pavo0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr


pavo1~pravo1
pavo0 ~ pravo0
"

avo_ponly_m <- sem(model = avo_ponly, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


# adds in paths that were significant from competitive model and fixes paths that are approximately zero (i.e., P > .4)
avo_mod <- "
#paths that are not particularly strong
pavo1 ~ 0*scpt
pavo0 ~ scpr
pavo0 ~ 0*ccpr
scpt ~ 0*pravo1 
scpt ~ 0*pravo0
scpr ~ 0*pravo0
ccpr ~ 0*pravo0

#strong paths
pavo1 ~  a*ccpt
ccpt ~ c*pravo1 
scpr ~ pravo1
ccpr ~ c*pravo1
pravo1 ~~ pravo0
pavo1~~pavo0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr
pavo1 ~ pravo1
pavo0 ~pravo0
ac:=a*c

"

avo_mod_m <- sem(model = avo_mod, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

#Re do so only cc
avo_mod <- "
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

"

avo_mod_m <- sem(model = avo_mod, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(avo_allfree_m, avo_apfixed_m, avo_afixed_m, avo_pfixed_m, avo_aonly_m, avo_ponly_m, avo_mod_m) # avo mod m preferred but ponly is close second
model_list <- c(avo_allfree_m, avo_apfixed_m, avo_afixed_m, avo_pfixed_m, avo_aonly_m, avo_ponly_m, avo_mod_m) # avo mod m preferred but ponly is close second
syn_model_list <- c(avo_allfree, avo_apfixed, avo_afixed, avo_pfixed, avo_aonly, avo_ponly, avo_mod)
syn_model_list_full <- syn_model_list 
df <- mapply(pull_fitmeasures, model_list, syn_model_list, list(posnegint_personality)) 
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "negint_avo"
fulldf <- df


# Process Model for Negint Anx --------------------------------------------



anx_allfree <- "
panx1 ~ scpt + ccpt + scpr + ccpr
panx0 ~ scpt + ccpt + scpr + ccpr
scpt ~ pranx1 + pranx0
ccpt ~ pranx1 + pranx0
scpr ~ pranx1 + pranx0
ccpr ~ pranx1 + pranx0
pranx1 ~~ pranx0
panx1~~panx0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr

panx1~pranx1
panx0 ~ pranx0

"

anx_allfree_m <- sem(model = anx_allfree, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anx_afixed <- "
panx1 ~ a*scpt
panx1 ~  b*ccpt
panx1 ~ scpr 
panx1 ~ ccpr
panx0 ~ scpt
panx0 ~ ccpt
panx0 ~ a*scpr
panx0 ~ b*ccpr
scpt ~ c*pranx1 
scpt ~ pranx0
ccpt ~ d*pranx1 
ccpt ~ pranx0
scpr ~ pranx1 
scpr ~c*pranx0
ccpr ~ pranx1
ccpr ~ d*pranx0
pranx1 ~~ pranx0
panx1~~panx0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr

panx1~pranx1
panx0 ~ pranx0

"

anx_afixed_m <- sem(model = anx_afixed, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anx_pfixed <- "
panx1 ~ scpt
panx1 ~  ccpt
panx1 ~ a*scpr 
panx1 ~ b*ccpr
panx0 ~ a*scpt
panx0 ~ b*ccpt
panx0 ~ scpr
panx0 ~ ccpr
scpt ~ pranx1 
scpt ~ c*pranx0
ccpt ~ pranx1 
ccpt ~ d*pranx0
scpr ~ c*pranx1 
scpr ~pranx0
ccpr ~ d*pranx1
ccpr ~ pranx0
pranx1 ~~ pranx0
panx1~~panx0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr


panx1~pranx1
panx0 ~ pranx0
"

anx_pfixed_m <- sem(model = anx_pfixed, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


anx_apfixed <- "
panx1 ~ e*scpt
panx1 ~  f*ccpt
panx1 ~ a*scpr 
panx1 ~ b*ccpr
panx0 ~ a*scpt
panx0 ~ b*ccpt
panx0 ~ e*scpr
panx0 ~ f*ccpr
scpt ~ g*pranx1 
scpt ~ c*pranx0
ccpt ~ h*pranx1 
ccpt ~ d*pranx0
scpr ~ c*pranx1 
scpr ~g*pranx0
ccpr ~ d*pranx1
ccpr ~ h*pranx0
pranx1 ~~ pranx0
panx1~~panx0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr


panx1~pranx1
panx0 ~ pranx0
"

anx_apfixed_m <- sem(model = anx_apfixed, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)






anx_aonly <- "
panx1 ~ scpt
panx1 ~  a*ccpt
panx0 ~ scpr
panx0 ~ ccpr
scpt ~ pranx1 
ccpt ~ c*pranx1 
scpr ~pranx0
ccpr ~ pranx0
pranx1 ~~ pranx0
panx1~~panx0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr
ac:=a*c

panx1~pranx1
panx0 ~ pranx0
"

anx_aonly_m <- sem(model = anx_aonly, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


anx_ponly <- "
panx1 ~ scpr 
panx1 ~ ccpr
panx0 ~ scpt
panx0 ~ ccpt
scpt ~ pranx0
ccpt ~ pranx0
scpr ~ pranx1 
ccpr ~ pranx1
pranx1 ~~ pranx0
panx1~~panx0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr

panx1~pranx1
panx0 ~ pranx0
"

anx_ponly_m <- sem(model = anx_ponly, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


anx_mod <- "
panx1 ~  f*ccpt
panx1 ~ scpr 
panx0 ~ ccpt
panx0 ~ scpr
panx0 ~ f*ccpr
scpt ~ a*pranx0
scpt ~ b*pranx1
scpr ~ a*pranx0
scpr ~ b*pranx1

ccpt ~ h*pranx1 
ccpt ~ d*pranx0
ccpr ~ d*pranx1
ccpr ~ h*pranx0
pranx1 ~~ pranx0
panx1~~panx0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr
panx1~~0*scpt
panx0~~0*scpt

panx1~pranx1
panx0 ~ pranx0
"

anx_mod_m <- sem(model = anx_mod, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
# Re do so only cc

anx_mod <- "
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
"

anx_mod_m <- sem(model = anx_mod, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)




anova(anx_allfree_m, anx_apfixed_m, anx_afixed_m, anx_pfixed_m, anx_aonly_m, anx_ponly_m, anx_mod_m) 
model_list <- c(anx_allfree_m, anx_apfixed_m, anx_afixed_m, anx_pfixed_m, anx_aonly_m, anx_ponly_m, anx_mod_m) 

syn_model_list <- c(anx_allfree, anx_apfixed, anx_afixed, anx_pfixed, anx_aonly, anx_ponly, anx_mod) 
syn_model_list_full <- c(syn_model_list_full, syn_model_list)
df <- mapply(pull_fitmeasures, model_list, syn_model_list) 
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "negint_anx"
fulldf <- bind_rows(fulldf, df)



# Process model for Negint Sec --------------------------------------------
sec_allfree <- "
psec1 ~ scpt + ccpt + scpr + ccpr
psec0 ~ scpt + ccpt + scpr + ccpr
scpt ~ prsec1 + prsec0
ccpt ~ prsec1 + prsec0
scpr ~ prsec1 + prsec0
ccpr ~ prsec1 + prsec0
prsec1 ~~ prsec0
psec1~~psec0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr

psec1~prsec1
psec0 ~ prsec0

"

sec_allfree_m <- sem(model = sec_allfree, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

sec_afixed <- "
psec1 ~ a*scpt
psec1 ~  b*ccpt
psec1 ~ scpr 
psec1 ~ ccpr
psec0 ~ scpt
psec0 ~ ccpt
psec0 ~ a*scpr
psec0 ~ b*ccpr
scpt ~ c*prsec1 
scpt ~ prsec0
ccpt ~ d*prsec1 
ccpt ~ prsec0
scpr ~ pravo1 
scpr ~c*prsec0
ccpr ~ prsec1
ccpr ~ d*prsec0
prsec1 ~~ prsec0
psec1~~psec0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr


psec1~prsec1
psec0 ~ prsec0
"

sec_afixed_m <- sem(model = sec_afixed, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

sec_pfixed <- "
psec1 ~ scpt
psec1 ~  ccpt
psec1 ~ a*scpr 
psec1 ~ b*ccpr
psec0 ~ a*scpt
psec0 ~ b*ccpt
psec0 ~ scpr
psec0 ~ ccpr
scpt ~ prsec1 
scpt ~ c*prsec0
ccpt ~ prsec1 
ccpt ~ d*prsec0
scpr ~ c*prsec1 
scpr ~prsec0
ccpr ~ d*prsec1
ccpr ~ prsec0
prsec1 ~~ prsec0
psec1~~psec0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr

psec1~prsec1
psec0 ~ prsec0

"

sec_pfixed_m <- sem(model = sec_pfixed, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


sec_apfixed <- "
psec1 ~ e*scpt
psec1 ~  f*ccpt
psec1 ~ a*scpr 
psec1 ~ b*ccpr
psec0 ~ a*scpt
psec0 ~ b*ccpt
psec0 ~ e*scpr
psec0 ~ f*ccpr
scpt ~ g*prsec1 
scpt ~ c*prsec0
ccpt ~ h*prsec1 
ccpt ~ d*prsec0
scpr ~ c*prsec1 
scpr ~g*prsec0
ccpr ~ d*prsec1
ccpr ~ h*prsec0
prsec1 ~~ prsec0
psec1~~psec0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr


psec1~prsec1
psec0 ~ prsec0
"

sec_apfixed_m <- sem(model = sec_apfixed, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)






sec_aonly <- "
psec1 ~ scpt
psec1 ~  a*ccpt
psec0 ~ scpr
psec0 ~ ccpr
scpt ~ prsec1 
ccpt ~ c*prsec1 
scpr ~prsec0
ccpr ~ prsec0
prsec1 ~~ prsec0
psec1~~psec0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr
ac:=a*c

psec1~prsec1
psec0 ~ prsec0
"

sec_aonly_m <- sem(model = sec_aonly, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


sec_ponly <- "
psec1 ~ scpr 
psec1 ~ ccpr
psec0 ~ scpt
psec0 ~ ccpt
scpt ~ prsec0
ccpt ~ prsec0
scpr ~ prsec1 
ccpr ~ prsec1
prsec1 ~~ prsec0
psec1~~psec0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr


psec1~prsec1
psec0 ~ prsec0
"

sec_ponly_m <- sem(model = sec_ponly, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)



sec_mod <- "
psec1 ~ e*scpt
psec1 ~  f*ccpt
psec1 ~ 0*scpr 
psec1 ~ 0*ccpr
psec0 ~ a*scpt
psec0 ~ b*ccpt
psec0 ~ 0*scpr
psec0 ~ 0*ccpr
scpt ~ g*prsec1 
scpt ~ h*prsec0
ccpt ~ 0*prsec1 
ccpt ~ 0*prsec0
scpr ~ 0*prsec1 
scpr ~0*prsec0
ccpr ~ h*prsec1
ccpr ~ 0*prsec0
prsec1 ~~ prsec0
psec1~~psec0
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr


psec1~prsec1
psec0 ~ prsec0
"

sec_mod_m <- sem(model = sec_mod, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

sec_mod <- "
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
"

sec_mod_m <- sem(model = sec_mod, data = posnegint_personality, missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

anova(sec_allfree_m, sec_apfixed_m, sec_afixed_m, sec_pfixed_m, sec_aonly_m, sec_ponly_m, sec_mod_m)

model_list <- c(sec_allfree_m, sec_apfixed_m, sec_afixed_m, sec_pfixed_m, sec_aonly_m, sec_ponly_m, sec_mod_m)
syn_model_list <- c(sec_allfree, sec_apfixed, sec_afixed, sec_pfixed, sec_aonly, sec_ponly, sec_mod)
syn_model_list_full <- c(syn_model_list_full, syn_model_list) 
df <- mapply(pull_fitmeasures, model_list, syn_model_list, list(posnegint_personality)) 
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "negint_sec"
fulldf <- bind_rows(fulldf, df)


# Process Model for Posint Avo --------------------------------------------



posint_avo_allfree <- "
p_scpt ~ pavo1  # actor
p_scpt ~ pravo1 # actor
p_scpt ~pavo0 #partner
p_scpt ~pravo0 #p
p_scpr ~ pavo1 #p
p_scpr ~ pravo1 #p
p_scpr ~ pavo0 #a
p_scpr ~ pravo0 #a
p_ccpt ~ pavo1 #a
p_ccpt ~ pravo1 #a
p_ccpt ~ pavo0 #p
p_ccpt ~ pravo0 #p
p_ccpr ~ pavo1 #p
p_ccpr ~ pravo1 #p
p_ccpr ~ pavo0 # a
p_ccpr ~ pravo0 #a

pravo1 ~~ pravo0
pavo1~~pavo0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
pavo1 ~ pravo1
pavo0 ~pravo0

"

posint_avo_allfree_m <- sem(model = posint_avo_allfree, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


posint_avo_afixed <- "
p_scpt ~ a*pavo1  # actor
p_scpt ~ b*pravo1 # actor
p_scpt ~pavo0 #partner
p_scpt ~pravo0 #p
p_scpr ~ pavo1 #p
p_scpr ~ pravo1 #p
p_scpr ~ a*pavo0 #a
p_scpr ~ b*pravo0 #a
p_ccpt ~ c*pavo1 #a
p_ccpt ~ d*pravo1 #a
p_ccpt ~ pavo0 #p
p_ccpt ~ pravo0 #p
p_ccpr ~ pavo1 #p
p_ccpr ~ pravo1 #p
p_ccpr ~ c*pavo0 # a
p_ccpr ~ d*pravo0 #a


pravo1 ~~ pravo0
pavo1~~pavo0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
pavo1 ~ pravo1
pavo0 ~pravo0

"

posint_avo_afixed_m <- sem(model = posint_avo_afixed, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)



posint_avo_pfixed <- "
p_scpt ~ pavo1  # actor
p_scpt ~ pravo1 # actor
p_scpt ~a*pavo0 #partner
p_scpt ~b*pravo0 #p
p_scpr ~ a*pavo1 #p
p_scpr ~ b*pravo1 #p
p_scpr ~ pavo0 #a
p_scpr ~ pravo0 #a
p_ccpt ~ pavo1 #a
p_ccpt ~ pravo1 #a
p_ccpt ~ c*pavo0 #p
p_ccpt ~ d*pravo0 #p
p_ccpr ~ c*pavo1 #p
p_ccpr ~ d*pravo1 #p
p_ccpr ~ pavo0 # a
p_ccpr ~ pravo0 #a


pravo1 ~~ pravo0
pavo1~~pavo0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
pavo1 ~ pravo1
pavo0 ~pravo0

"

posint_avo_pfixed_m <- sem(model = posint_avo_pfixed, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

posint_avo_apfixed <- "

p_scpt ~ a*pavo1  # actor
p_scpt ~ b*pravo1 # actor
p_scpt ~c*pavo0 #partner
p_scpt ~d*pravo0 #p
p_scpr ~ c*pavo1 #p
p_scpr ~ d*pravo1 #p
p_scpr ~ a*pavo0 #a
p_scpr ~ b*pravo0 #a
p_ccpt ~ e*pavo1 #a
p_ccpt ~ f*pravo1 #a
p_ccpt ~ g*pavo0 #p
p_ccpt ~ h*pravo0 #p
p_ccpr ~ g*pavo1 #p
p_ccpr ~ h*pravo1 #p
p_ccpr ~ e*pavo0 # a
p_ccpr ~ f*pravo0 #a


pravo1 ~~ pravo0
pavo1~~pavo0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
pavo1 ~ pravo1
pavo0 ~pravo0

"

posint_avo_apfixed_m <- sem(model = posint_avo_apfixed, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
posint_avo_aonly <- "
p_scpt ~ pavo1  # actor
p_scpt ~ pravo1 # actor
p_scpr ~ pavo0 #a
p_scpr ~ pravo0 #a
p_ccpt ~ pavo1 #a
p_ccpt ~ pravo1 #a
p_ccpr ~ pavo0 # a
p_ccpr ~ pravo0 #a


pravo1 ~~ pravo0
pavo1~~pavo0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
pavo1 ~ pravo1
pavo0 ~pravo0

"

posint_avo_aonly_m <- sem(model = posint_avo_aonly, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

posint_avo_ponly <- "

p_scpt ~pavo0 #partner
p_scpt ~pravo0 #p
p_scpr ~ pavo1 #p
p_scpr ~ pravo1 #p
p_ccpt ~ pavo0 #p
p_ccpt ~ pravo0 #p
p_ccpr ~ pavo1 #p
p_ccpr ~ pravo1 #p



pravo1 ~~ pravo0
pavo1~~pavo0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
pavo1 ~ pravo1
pavo0 ~pravo0

"

posint_avo_ponly_m <- sem(model = posint_avo_ponly, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


posint_avo_mod <- "

p_scpt ~ a*pavo0 #partner
p_scpt ~ b*pravo0 #p
p_scpr ~ a*pavo1 #p
p_scpr ~ b*pravo1 #p
p_ccpr ~ pravo1 #p



pravo1 ~~ pravo0
pavo1 ~~ pavo0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
pavo1 ~ pravo1
pavo0 ~ pravo0
p_ccpt ~ 0*pavo0
p_ccpt ~ 0*pavo1
p_ccpt ~ 0*pravo0
p_ccpt ~ 0*pravo1
"

posint_avo_mod_m <- sem(model = posint_avo_mod, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

posint_avo_mod2 <- "

p_scpt ~pravo0 #partner
p_scpr ~ pravo1 #p
p_ccpr ~ pravo1 #p
pavo1 ~ pravo1
pavo0 ~pravo0


pravo1 ~~ pravo0
pavo1 ~~ pavo0

p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
p_scpt ~~ 0*pavo1 
p_scpt ~~ 0*pavo0
p_scpr ~~ 0*pavo1
p_scpr ~~ 0*pavo0
p_ccpr ~~ 0*pavo1
p_ccpr ~~ 0*pavo0
p_ccpt ~ 0*pavo0
p_ccpt ~ 0*pavo1
p_ccpt ~ 0*pravo0
p_ccpt ~ 0*pravo1
"

posint_avo_mod2_m <- sem(model = posint_avo_mod2, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

posint_avo_mod3 <- "

p_scpt ~pavo0 #partner
p_scpr ~ pavo1 #p
p_ccpr ~ pravo1 #p
pavo1 ~ pravo1
pavo0 ~pravo0


pravo1 ~~ pravo0
pavo1 ~~ pavo0

p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
p_scpt ~~ 0*pavo1 
p_scpt ~~ 0*pavo0
p_scpr ~~ 0*pavo1
p_scpr ~~ 0*pavo0
p_ccpr ~~ 0*pavo1
p_ccpr ~~ 0*pavo0
"

posint_avo_mod3_m <- sem(model = posint_avo_mod3, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


posint_avo_mod <- posint_avo_mod2
posint_avo_mod_m <- posint_avo_mod2_m
posint_avo_mod <- "

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
"

posint_avo_mod_m <- sem(model = posint_avo_mod, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)



anova(posint_avo_allfree_m, posint_avo_apfixed_m, posint_avo_afixed_m, posint_avo_pfixed_m, posint_avo_aonly_m, posint_avo_ponly_m, posint_avo_mod2_m, posint_avo_mod_m)  # favors ponly (which by frequentist measures fits well), greater 

model_list <- c(posint_avo_allfree_m, posint_avo_apfixed_m, posint_avo_afixed_m, posint_avo_pfixed_m, posint_avo_aonly_m, posint_avo_ponly_m, posint_avo_mod_m)  # favors ponly (which by frequentist measures fits well), greater 
syn_model_list <- c(posint_avo_allfree, posint_avo_apfixed, posint_avo_afixed, posint_avo_pfixed, posint_avo_aonly, posint_avo_ponly, posint_avo_mod)  # favors ponly (which by frequentist measures fits well), greater 
syn_model_list_full <- c(syn_model_list_full, syn_model_list)

df <- mapply(pull_fitmeasures, model_list, syn_model_list, list(dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.))) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "posint_avo"
fulldf <- bind_rows(fulldf, df)


# Process Model for Posint Anx --------------------------------------------




posint_anx_allfree <- "
p_scpt ~ panx1  # actor
p_scpt ~ pranx1 # actor
p_scpt ~panx0 #partner
p_scpt ~pranx0 #p
p_scpr ~ panx1 #p
p_scpr ~ pranx1 #p
p_scpr ~ panx0 #a
p_scpr ~ pranx0 #a
p_ccpt ~ panx1 #a
p_ccpt ~ pranx1 #a
p_ccpt ~ panx0 #p
p_ccpt ~ pranx0 #p
p_ccpr ~ panx1 #p
p_ccpr ~ pranx1 #p
p_ccpr ~ panx0 # a
p_ccpr ~ pranx0 #a

pranx1 ~~ pranx0
panx1~~panx0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

"

posint_anx_allfree_m <- sem(model = posint_anx_allfree, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


posint_anx_afixed <- "
p_scpt ~ a*panx1  # actor
p_scpt ~ b*pranx1 # actor
p_scpt ~panx0 #partner
p_scpt ~pranx0 #p
p_scpr ~ panx1 #p
p_scpr ~ pranx1 #p
p_scpr ~ a*panx0 #a
p_scpr ~ b*pranx0 #a
p_ccpt ~ c*panx1 #a
p_ccpt ~ d*pranx1 #a
p_ccpt ~ panx0 #p
p_ccpt ~ pranx0 #p
p_ccpr ~ panx1 #p
p_ccpr ~ pranx1 #p
p_ccpr ~ c*panx0 # a
p_ccpr ~ d*pranx0 #a


pranx1 ~~ pranx0
panx1~~panx0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

"

posint_anx_afixed_m <- sem(model = posint_anx_afixed, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)



posint_anx_pfixed <- "
p_scpt ~ panx1  # actor
p_scpt ~ pranx1 # actor
p_scpt ~a*panx0 #partner
p_scpt ~b*pranx0 #p
p_scpr ~ a*panx1 #p
p_scpr ~ b*pranx1 #p
p_scpr ~ panx0 #a
p_scpr ~ pranx0 #a
p_ccpt ~ panx1 #a
p_ccpt ~ pranx1 #a
p_ccpt ~ c*panx0 #p
p_ccpt ~ d*pranx0 #p
p_ccpr ~ c*panx1 #p
p_ccpr ~ d*pranx1 #p
p_ccpr ~ panx0 # a
p_ccpr ~ pranx0 #a


pranx1 ~~ pranx0
panx1~~panx0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

"

posint_anx_pfixed_m <- sem(model = posint_anx_pfixed, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

posint_anx_apfixed <- "

p_scpt ~ a*panx1  # actor
p_scpt ~ b*pranx1 # actor
p_scpt ~c*panx0 #partner
p_scpt ~d*pranx0 #p
p_scpr ~ c*panx1 #p
p_scpr ~ d*pranx1 #p
p_scpr ~ a*panx0 #a
p_scpr ~ b*pranx0 #a
p_ccpt ~ e*panx1 #a
p_ccpt ~ f*pranx1 #a
p_ccpt ~ g*panx0 #p
p_ccpt ~ h*pranx0 #p
p_ccpr ~ g*panx1 #p
p_ccpr ~ h*pranx1 #p
p_ccpr ~ e*panx0 # a
p_ccpr ~ f*pranx0 #a


pranx1 ~~ pranx0
panx1~~panx0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

"

posint_anx_apfixed_m <- sem(model = posint_anx_apfixed, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
posint_anx_aonly <- "
p_scpt ~ panx1  # actor
p_scpt ~ pranx1 # actor
p_scpr ~ panx0 #a
p_scpr ~ pranx0 #a
p_ccpt ~ panx1 #a
p_ccpt ~ pranx1 #a
p_ccpr ~ panx0 # a
p_ccpr ~ pranx0 #a


pranx1 ~~ pranx0
panx1~~panx0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

"

posint_anx_aonly_m <- sem(model = posint_anx_aonly, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

posint_anx_ponly <- "

p_scpt ~panx0 #partner
p_scpt ~pranx0 #p
p_scpr ~ panx1 #p
p_scpr ~ pranx1 #p
p_ccpt ~ panx0 #p
p_ccpt ~ pranx0 #p
p_ccpr ~ panx1 #p
p_ccpr ~ pranx1 #p



pranx1 ~~ pranx0
panx1~~panx0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

"

posint_anx_ponly_m <- sem(model = posint_anx_ponly, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)



posint_anx_mod <- "
p_scpt ~ c*panx1  # actor
p_scpr ~ c*pranx1 #p
p_ccpt ~ b*panx1 #a
p_ccpt ~ a*pranx1 #a
p_ccpr ~ b*panx0 # a
p_ccpr ~ a*pranx0 #a


pranx1 ~~ pranx0
panx1~~panx0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

"

posint_anx_mod_m <- sem(model = posint_anx_mod, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


posint_anx_mod2 <- "
p_scpt ~ a*pranx1 #+ pranx0  # actor
p_scpr ~ a*pranx1 #+ pranx0 #p
p_ccpt ~ pranx1 #+ pranx0 #a
#p_ccpt ~ pranx1 #a
p_ccpr ~ pranx0  #+ pranx1 # a
#p_ccpr ~ pranx0 #a


pranx1 ~~ pranx0
#panx1~~panx0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
#panx1 ~ pranx1
#panx0 ~pranx0
#p_scpt ~~ 0*panx1 
p_scpt ~~ 0*pranx0
#p_scpr ~~ 0*pranx1
p_scpr ~~ 0*pranx0
#p_ccpr ~~ 0*pranx1
p_ccpr ~~ 0*pranx0
p_ccpt ~~ 0*pranx0
"

posint_anx_mod2_m <- sem(model = posint_anx_mod2, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

posint_anx_mod3 <- "
p_scpt ~ a*panx1 + panx0  # actor
p_scpr ~ a*panx1 + panx0 #p
p_ccpt ~ panx1 #+ panx0 #a
p_ccpr ~ panx0#  + panx1 # a
panx1~~panx0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
"

posint_anx_mod3_m <- sem(model = posint_anx_mod3, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

posint_anx_mod <- "
p_ccpt ~ b*panx1 #a
p_ccpt ~ a*pranx1 #a
p_ccpr ~ b*panx0 # a
p_ccpr ~ a*pranx0 #a


pranx1 ~~ pranx0
panx1~~panx0
p_ccpt ~~ p_ccpr
panx1 ~ pranx1
panx0 ~pranx0

"

posint_anx_mod_m <- sem(model = posint_anx_mod, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


anova(posint_anx_allfree_m, posint_anx_apfixed_m, posint_anx_afixed_m, posint_anx_pfixed_m, posint_anx_aonly_m, posint_anx_ponly_m, posint_anx_mod_m)  # favors modified version of model

model_list <- c(posint_anx_allfree_m, posint_anx_apfixed_m, posint_anx_afixed_m, posint_anx_pfixed_m, posint_anx_aonly_m, posint_anx_ponly_m, posint_anx_mod_m)  # favors modified version of model
syn_model_list <- c(posint_anx_allfree, posint_anx_apfixed, posint_anx_afixed, posint_anx_pfixed, posint_anx_aonly, posint_anx_ponly, posint_anx_mod)  # favors modified version of model
syn_model_list_full <- c(syn_model_list_full, syn_model_list)

df <- mapply(pull_fitmeasures, model_list, syn_model_list, list(dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.))) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))

df$mname <- "posint_anx"
fulldf <- bind_rows(fulldf, df)



# Process Model for Posint Sec --------------------------------------------



posint_sec_allfree <- "
p_scpt ~ psec1  # actor
p_scpt ~ prsec1 # actor
p_scpt ~psec0 #partner
p_scpt ~prsec0 #p
p_scpr ~ psec1 #p
p_scpr ~ prsec1 #p
p_scpr ~ psec0 #a
p_scpr ~ prsec0 #a
p_ccpt ~ psec1 #a
p_ccpt ~ prsec1 #a
p_ccpt ~ psec0 #p
p_ccpt ~ prsec0 #p
p_ccpr ~ psec1 #p
p_ccpr ~ prsec1 #p
p_ccpr ~ psec0 # a
p_ccpr ~ prsec0 #a

prsec1 ~~ prsec0
psec1~~psec0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
psec1 ~ prsec1
psec0 ~prsec0

"

posint_sec_allfree_m <- sem(model = posint_sec_allfree, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)


posint_sec_afixed <- "
p_scpt ~ a*psec1  # actor
p_scpt ~ b*prsec1 # actor
p_scpt ~psec0 #partner
p_scpt ~prsec0 #p
p_scpr ~ psec1 #p
p_scpr ~ prsec1 #p
p_scpr ~ a*psec0 #a
p_scpr ~ b*prsec0 #a
p_ccpt ~ c*psec1 #a
p_ccpt ~ d*prsec1 #a
p_ccpt ~ psec0 #p
p_ccpt ~ prsec0 #p
p_ccpr ~ psec1 #p
p_ccpr ~ prsec1 #p
p_ccpr ~ c*psec0 # a
p_ccpr ~ d*prsec0 #a


prsec1 ~~ prsec0
psec1~~psec0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
psec1 ~ prsec1
psec0 ~prsec0

"

posint_sec_afixed_m <- sem(model = posint_sec_afixed, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)



posint_sec_pfixed <- "
p_scpt ~ psec1  # actor
p_scpt ~ prsec1 # actor
p_scpt ~a*psec0 #partner
p_scpt ~b*prsec0 #p
p_scpr ~ a*psec1 #p
p_scpr ~ b*prsec1 #p
p_scpr ~ psec0 #a
p_scpr ~ prsec0 #a
p_ccpt ~ psec1 #a
p_ccpt ~ prsec1 #a
p_ccpt ~ c*psec0 #p
p_ccpt ~ d*prsec0 #p
p_ccpr ~ c*psec1 #p
p_ccpr ~ d*prsec1 #p
p_ccpr ~ psec0 # a
p_ccpr ~ prsec0 #a


prsec1 ~~ prsec0
psec1~~psec0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
psec1 ~ prsec1
psec0 ~prsec0

"

posint_sec_pfixed_m <- sem(model = posint_sec_pfixed, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

posint_sec_apfixed <- "

p_scpt ~ a*psec1  # actor
p_scpt ~ b*prsec1 # actor
p_scpt ~c*psec0 #partner
p_scpt ~d*prsec0 #p
p_scpr ~ c*psec1 #p
p_scpr ~ d*prsec1 #p
p_scpr ~ a*psec0 #a
p_scpr ~ b*prsec0 #a
p_ccpt ~ e*psec1 #a
p_ccpt ~ f*prsec1 #a
p_ccpt ~ g*psec0 #p
p_ccpt ~ h*prsec0 #p
p_ccpr ~ g*psec1 #p
p_ccpr ~ h*prsec1 #p
p_ccpr ~ e*psec0 # a
p_ccpr ~ f*prsec0 #a


prsec1 ~~ prsec0
psec1~~psec0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
psec1 ~ prsec1
psec0 ~prsec0

"

posint_sec_apfixed_m <- sem(model = posint_sec_apfixed, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
posint_sec_aonly <- "
p_scpt ~ psec1  # actor
p_scpt ~ prsec1 # actor
p_scpr ~ psec0 #a
p_scpr ~ prsec0 #a
p_ccpt ~ psec1 #a
p_ccpt ~ prsec1 #a
p_ccpr ~ psec0 # a
p_ccpr ~ prsec0 #a


prsec1 ~~ prsec0
psec1~~psec0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
psec1 ~ prsec1
psec0 ~prsec0

"

posint_sec_aonly_m <- sem(model = posint_sec_aonly, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

posint_sec_ponly <- "

p_scpt ~psec0 #partner
p_scpt ~prsec0 #p
p_scpr ~ psec1 #p
p_scpr ~ prsec1 #p
p_ccpt ~ psec0 #p
p_ccpt ~ prsec0 #p
p_ccpr ~ psec1 #p
p_ccpr ~ prsec1 #p



prsec1 ~~ prsec0
psec1~~psec0
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
psec1 ~ prsec1
psec0 ~prsec0

"

posint_sec_ponly_m <- sem(model = posint_sec_ponly, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)




posint_sec_mod <- "
p_scpt ~ 0*psec1  # actor
p_scpt ~ prsec1 # actor
p_scpt ~0*psec0 #partner
p_scpt ~0*prsec0 #p
p_scpr ~ psec1 #p
p_scpr ~ 0*prsec1 #p
p_scpr ~ 0*psec0 #a
p_scpr ~ 0*prsec0 #a
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
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr
psec1 ~ prsec1
psec0 ~prsec0

"

posint_sec_mod_m <- sem(model = posint_sec_mod, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)

posint_sec_mod <- "
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

"

posint_sec_mod_m <- sem(model = posint_sec_mod, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)



anova(posint_sec_allfree_m, posint_sec_apfixed_m, posint_sec_afixed_m, posint_sec_pfixed_m, posint_sec_aonly_m, posint_sec_ponly_m, posint_sec_mod_m) 

model_list <- c(posint_sec_allfree_m, posint_sec_apfixed_m, posint_sec_afixed_m, posint_sec_pfixed_m, posint_sec_aonly_m, posint_sec_ponly_m, posint_sec_mod_m)  # fsecrs ponly (which by frequentist measures fits well), greater 
syn_model_list <- c(posint_sec_allfree, posint_sec_apfixed, posint_sec_afixed, posint_sec_pfixed, posint_sec_aonly, posint_sec_ponly, posint_sec_mod)  # favors ponly (which by frequentist measures fits well), greater 
syn_model_list_full <- c(syn_model_list_full, syn_model_list)

df <- mapply(pull_fitmeasures, model_list, syn_model_list, list(dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.))) )
if(class(df) == "list") {df <- as.data.frame(do.call(rbind, df)) } else {df <- as.data.frame(t(df))}
df <- df%>% mutate_all(funs(as.numeric(.)))
df$mname <- "posint_sec"
fulldf <- bind_rows(fulldf, df)

ordering <- c("allfree", "afixed", "pfixed", "apfixed", "aonly", "ponly", "mod")
fulldf <- fulldf %>% group_by(mname) %>% mutate(model_v = ordering) %>%  ungroup() %>% mutate(analysis_name = paste0(mname, "_", model_v))
names(syn_model_list_full) <- as.vector(fulldf$analysis_name)

save(syn_model_list_full, file ="../output/bsem_process/model_strings.RData")

df_order <- data.frame(mname = c("negint_avo", "negint_anx", "negint_sec", "posint_avo", "posint_anx", "posint_sec"), model_type = 1:length(unique(fulldf$mname)))
fulldf <- left_join(fulldf, df_order)
fulldf <- group_by(fulldf, model_type) %>% mutate(mnumber = 1:length(V1)) %>% ungroup()
fulldf <- arrange(fulldf, fulldf$model_type)

write.csv(fulldf, "../output/bsem_process/fit_indices_allprocess_models.csv", row.names= FALSE)

# Combined Models -- not nested within others -----------------------------

posnegint_avo_mod <- "
#positive interaction regressions

p_scpt ~pravo0 #partner
p_scpr ~ pravo1 #p
p_ccpr ~ pravo1 #p
p_scpt ~~ 0*pavo1 
p_scpt ~~ 0*pavo0
p_scpr ~~ 0*pavo1
p_scpr ~~ 0*pavo0
p_ccpr ~~ 0*pavo1
p_ccpr ~~ 0*pavo0
p_ccpt ~ 0*pavo0
p_ccpt ~ 0*pavo1
p_ccpt ~ 0*pravo0
p_ccpt ~ 0*pravo1

#negint regressions
pavo0 ~ scpr
pavo1 ~  a*ccpt
ccpt ~ c*pravo1 
scpr ~ pravo1
ccpr ~ c*pravo1

pavo1 ~ 0*scpt
pavo0 ~ 0*ccpr
scpt ~ 0*pravo1 
scpt ~ 0*pravo0
scpr ~ 0*pravo0
ccpr ~ 0*pravo0
pavo1 ~ pravo1
pavo0 ~pravo0

#state covariation
pravo1 ~~ pravo0
pavo1~~pavo0

#posint covariation
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr

#negint covariation
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr

#posnegint covariation
p_scpt ~ scpt
p_scpr ~ scpr
    
#indirect paths
ac:=a*c


"

posnegint_avo_mod_m <- sem(model = posnegint_avo_mod, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
runBSEM(modsyntax(dat =dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), posnegint_avo_mod))



posnegint_anx_mod <- "
#posint regressions
p_scpt ~ cc*panx1  # actor
p_scpr ~ cc*pranx1 #p
p_ccpt ~ bb*panx1 #a
p_ccpt ~ aa*pranx1 #a
p_ccpr ~ bb*panx0 # a
p_ccpr ~ aa*pranx0 #a

#negint regressions
panx1 ~  f*ccpt
panx1 ~ scpr 
panx0 ~ ccpt
panx0 ~ scpr
panx0 ~ f*ccpr
scpt ~ a*pranx0
scpt ~ b*pranx1
scpr ~ a*pranx0
scpr ~ b*pranx1

ccpt ~ h*pranx1 
ccpt ~ d*pranx0
ccpr ~ d*pranx1
ccpr ~ h*pranx0

panx1~pranx1
panx0 ~ pranx0


#state covariation
pranx1 ~~ pranx0
panx1~~panx0


#negint covariation
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr
panx1~~0*scpt
panx0~~0*scpt

#posint covariation
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr

#posnegint covariation
p_scpr ~ scpr
p_scpt ~ 0*scpt
p_scpt ~ 0*ccpt
p_scpt ~ 0*scpr 
p_scpt ~ 0*ccpr
p_ccpt ~ 0*scpt
p_ccpt ~ 0*ccpt
p_ccpt ~ 0*scpr 
p_ccpt ~ 0*ccpr
p_ccpr ~ 0*scpt
p_ccpr ~ 0*ccpt
p_ccpr ~ 0*scpr 
p_ccpr ~ 0*ccpr
p_scpr ~ 0*ccpt
p_scpr ~ 0*scpt 
p_scpr ~ 0*ccpr



"

posnegint_anx_mod_m <- sem(model = posnegint_anx_mod, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
runBSEM(modsyntax(dat =dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), posnegint_anx_mod))

# trying to reduce the number of parameters - omitting sc from model; go with mod2 because preserves paths present and/of interest in simpler model and increases K:n ratio

posnegint_anx_mod2 <- "
#posint regressions
p_ccpt ~ bb*panx1 #a
p_ccpt ~ aa*pranx1 #a
p_ccpr ~ bb*panx0 # a
p_ccpr ~ aa*pranx0 #a

#negint regressions
panx1 ~  f*ccpt
panx0 ~ ccpt
panx0 ~ f*ccpr

ccpt ~ h*pranx1 
ccpt ~ d*pranx0
ccpr ~ d*pranx1
ccpr ~ h*pranx0

panx1~pranx1
panx0 ~ pranx0


#state covariation
pranx1 ~~ pranx0
panx1~~panx0


#negint covariation
ccpt ~~ ccpr

#posint covariation
p_ccpt ~~ p_ccpr

#posnegint covariation
p_ccpt ~ 0*ccpt
p_ccpt ~ 0*ccpr
p_ccpr ~ 0*ccpt
p_ccpr ~ 0*ccpr




"

posnegint_anx_mod2_m <- sem(model = posnegint_anx_mod2, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
runBSEM(modsyntax(dat =dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), posnegint_anx_mod2))



posnegint_avo_mod2 <- "
#positive interaction regressions

p_ccpr ~ pravo1 #p
p_ccpr ~~ 0*pavo1
p_ccpr ~~ 0*pavo0
p_ccpt ~ 0*pavo0
p_ccpt ~ 0*pavo1
p_ccpt ~ 0*pravo0
p_ccpt ~ 0*pravo1

#negint regressions
pavo1 ~  a*ccpt
ccpt ~ c*pravo1 
ccpr ~ c*pravo1

pavo0 ~ 0*ccpr
ccpr ~ 0*pravo0
pavo1 ~ pravo1
pavo0 ~pravo0

#state covariation
pravo1 ~~ pravo0
pavo1~~pavo0

#posint covariation
p_ccpt ~~ p_ccpr

#negint covariation
ccpt ~~ ccpr

#posnegint covariation
p_ccpr ~ 0*ccpr
p_ccpt ~ 0*ccpt
p_ccpr ~ 0*ccpt
p_ccpt ~ 0*ccpr
#indirect paths
ac:=a*c


"

posnegint_avo_mod2_m <- sem(model = posnegint_avo_mod2, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
runBSEM(modsyntax(dat =dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), posnegint_avo_mod2))

posnegint_sec_mod <- "
#posint regressions
p_scpt ~ 0*psec1  # actor
p_scpt ~ prsec1 # actor
p_scpt ~0*psec0 #partner
p_scpt ~0*prsec0 #p
p_scpr ~ psec1 #p
p_scpr ~ 0*prsec1 #p
p_scpr ~ 0*psec0 #a
p_scpr ~ 0*prsec0 #a
p_ccpt ~ 0*psec1 #a
p_ccpt ~ 0*prsec1 #a
p_ccpt ~ 0*psec0 #p
p_ccpt ~ prsec0 #p
p_ccpr ~ 0*psec1 #p
p_ccpr ~ prsec1 #p
p_ccpr ~ 0*psec0 # a
p_ccpr ~ prsec0 #a



#negint regressions
psec1 ~ e*scpt
psec1 ~  f*ccpt
psec1 ~ 0*scpr 
psec1 ~ 0*ccpr
psec0 ~ a*scpt
psec0 ~ b*ccpt
psec0 ~ 0*scpr
psec0 ~ 0*ccpr
scpt ~ g*prsec1 
scpt ~ h*prsec0
ccpt ~ 0*prsec1 
ccpt ~ 0*prsec0
scpr ~ 0*prsec1 
scpr ~0*prsec0
ccpr ~ h*prsec1
ccpr ~ 0*prsec0

psec1~prsec1
psec0 ~ prsec0


#state covariation
prsec1 ~~ prsec0
psec1~~psec0

#posint covariation
p_scpt ~~ p_ccpt
p_scpt ~~ p_scpr 
p_scpt ~~ p_ccpr
p_ccpt ~~ p_scpr 
p_ccpt ~~ p_ccpr
p_scpr ~~ p_ccpr

#negint covariation
scpt ~~ ccpt
scpt ~~ scpr 
scpt ~~ ccpr
ccpt ~~ scpr 
ccpt ~~ ccpr
scpr ~~ ccpr

#posnegint covariation
p_scpr ~ scpr
p_scpt ~ scpt
p_scpt ~ 0*ccpt
p_scpt ~ 0*scpr 
p_scpt ~ 0*ccpr
p_ccpt ~ 0*scpt
p_ccpt ~ 0*ccpt
p_ccpt ~ 0*scpr 
p_ccpt ~ 0*ccpr
p_ccpr ~ 0*scpt
p_ccpr ~ 0*ccpt
p_ccpr ~ 0*scpr 
p_ccpr ~ 0*ccpr
p_scpr ~ 0*ccpt
p_scpr ~ 0*scpt 
p_scpr ~ 0*ccpr


"

posnegint_sec_mod_m <- sem(model = posnegint_sec_mod, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
runBSEM(modsyntax(dat =dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), posnegint_sec_mod))


posnegint_sec_mod2 <- "
#posint regressions
p_ccpt ~ 0*psec1 #a
p_ccpt ~ 0*prsec1 #a
p_ccpt ~ 0*psec0 #p
p_ccpt ~ prsec0 #p
p_ccpr ~ 0*psec1 #p
p_ccpr ~ prsec1 #p
p_ccpr ~ 0*psec0 # a
p_ccpr ~ prsec0 #a



#negint regressions
psec1 ~  f*ccpt
psec1 ~ 0*ccpr
psec0 ~ b*ccpt
psec0 ~ 0*ccpr
ccpt ~ 0*prsec1 
ccpt ~ 0*prsec0
ccpr ~ h*prsec1
ccpr ~ 0*prsec0

psec1~prsec1
psec0 ~ prsec0


#state covariation
prsec1 ~~ prsec0
psec1~~psec0

#posint covariation
p_ccpt ~~ p_ccpr

#negint covariation
ccpt ~~ ccpr

#posnegint covariation
p_ccpt ~ 0*ccpt
p_ccpt ~ 0*ccpr
p_ccpr ~ 0*ccpt
p_ccpr ~ 0*ccpr


"

posnegint_sec_mod2_m <- sem(model = posnegint_sec_mod2, data = dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), missing = "ML",estimator  = "MLR", meanstructure = TRUE, mimic = "Mplus", conditional.x = FALSE)
runBSEM(modsyntax(dat =dplyr::mutate_at(posnegint_personality, vars(starts_with("p_")), list(~100*.)), posnegint_sec_mod2))
# so psec0 ~ ccpt goes from sig to marginal (.025 to .028) and ccpr ~ prsec1 goes from .024 to .036. But other effects remain. End up with 25 additional free parameters too... Ask Michael but feels like should still go with the simpler model SINCE adding additional parameters

syn_comb_models <- c(posnegint_avo_mod, posnegint_avo_mod2,posnegint_anx_mod,posnegint_anx_mod2, posnegint_sec_mod, posnegint_sec_mod2)
names(syn_comb_models) <- c("posnegint_avo_mod", "posnegint_avo_mod2","posnegint_anx_mod","posnegint_anx_mod2", "posnegint_sec_mod", "posnegint_sec_mod2")
save(syn_comb_models, file = "../output/bsem_process/model_comb_strings.RData")


## Summary of findings from process results
# Baseline avoidance in patient predicts contrarian in both patient and partner during negative interaction and contrarianism during the positive interaction for the partner. Contrarianism is sticky with patient where it subsequently is associated with heightened feels of attachment avoidance in patient.

#Baseline momentary anxiety predicts within person tendency to be contrarian during the positive interaction and marginally so in the negative interaction. This is true in both patients and partners. Within person effect such that exhibiting dependency during the interaction amplifies anxiety (being contrarian CAN lead to decreases in anxiety too, not just weaker increases). 



