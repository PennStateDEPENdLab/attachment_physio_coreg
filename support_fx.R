

ECR_moderation <-
  function(ecr_var,
           saam_var,
           stage = 1,
           version,
           params = c(
             "aaaa",
             "bbbb",
             "cccc",
             "dddd",
             "eeee",
             "ffff",
             "gggg",
             "hhhhh",
             "paaaa",
             "pbbbb",
             "pcccc",
             "pdddd",
             "peeee",
             "pffff",
             "pggggg",
             "phhhhh"
           ),
           model_syntax,
           dat = posnegint_formoderation) {
    ## TO FIX: for ECR anx and SAAM anx, not correctly labeling actor vs. partner paths - can trust allfree
    ## TO FIX: S1+S2 not tested
    ## To implement: AAR or ECR (have be an option where sub out ECR with AAR)
    if (!version %in% c("allfree", "afixed", "pfixed", "aonly", "ponly", "apfixed")) {
      return(print("Invalid request"))
    }
    coupling_params <- c("ccpt", "ccpr", "p_ccpt", "p_ccpr")
    
    
    #Grab names of named parameters for model comparison
    p_aaa = params[[9]]
    p_bbb = params[[10]]
    p_ccc = params[[11]]
    p_ddd = params[[12]]
    
    p_eee = params[[13]]
    p_fff = params[[14]]
    p_ggg = params[[15]]
    p_hhh = params[[16]]
    
    #This block of code gets all of the covariance and regression paths from the model specified
    orig_modelsyntax <- modsyntax(dat, model_syntax)
    orig_model_vars <- orig_modelsyntax[[2]] %>% strsplit(";")
    orig_model_vars_char <- orig_model_vars[[1]] # unlist
    with_commands <-
      orig_model_vars_char[grep("*WITH*", orig_model_vars_char)]
    with_commands_df <-
      data.frame(fullstatement = as.vector(with_commands)) %>% separate(fullstatement,
                                                                        into = c("var1", "var2"),
                                                                        sep = " WITH ") %>% separate(var2,
                                                                                                     into = c("var2", "zero_constraints"),
                                                                                                     sep = "@") %>% separate(var2,
                                                                                                                             into = c("var2", "param_constraints"),
                                                                                                                             sep = "\\*") %>% dplyr::select(-param_constraints) %>% mutate(var1 = gsub("\\n", "", var1),
                                                                                                                                                                                           var2 = gsub("\\n", "", var2))
    with_commands_df$sep = "WITH"
    on_commands <-
      orig_model_vars_char[grep("*ON*", orig_model_vars_char)]
    on_commands_df <-
      data.frame(fullstatement = as.vector(on_commands)) %>% separate(fullstatement,
                                                                      into = c("var1", "var2"),
                                                                      sep = " ON ") %>% separate(var2,
                                                                                                 into = c("var2", "zero_constraints"),
                                                                                                 sep = "@") %>% separate(var2,
                                                                                                                         into = c("var2", "param_constraints"),
                                                                                                                         sep = "\\*") %>% dplyr::select(-param_constraints) %>% mutate(var1 = gsub("\\n", "", var1),
                                                                                                                                                                                       var2 = gsub("\\n", "", var2))
    on_commands_df$sep = "ON"
    commands_df <-
      bind_rows(with_commands_df, on_commands_df) %>% mutate(
        zero_constraints = if_else(is.na(zero_constraints), 999, 0),
        var1 = gsub(" ", "", var1),
        var2 = gsub(" ", "", var2)
      )
    # Specify the variables other than coupling parameters that will be in the moderated mediation analysees
    x = ecr_var
    y = saam_var
    # Ensure that the predictors covary -- if not specified mplus won't do this and that'll be a HUGE MI
    preamble <- paste0("\nECR", x, "_1 ~~ ECR", x, "_0")
    
    # Grab the prefix for the interaction parameters. This is only important for stage 2 moderation if looking at positive interaction coupling parameters
    if (stage == 1) {
      prefix = "pr"
    } else if (stage == 2) {
      prefix = "p"
    } else {
      prefix = "pr"
      suffix = "p"
    }
    #varianes that need to be fixed to zero
    x_shortened <-
      substr(x, 1, 2) # grab two first letters of ECR var
    y_shortened = substr(y, 1, 2) # grab first two letters of SAAM var
    if (stage == 1) {
      int_11 <- paste0(x_shortened, "1", prefix, y_shortened, "1")
      int_10 <- paste0(x_shortened, "1", prefix, y_shortened, "0")
      int_01 <- paste0(x_shortened, "0", prefix, y_shortened, "1")
      int_00 <- paste0(x_shortened, "0", prefix, y_shortened, "0")
    } else if (stage == 2) {
      # note: this currently assumes that can't have pos and neg together so will need to fix that at a later time
      if (any(commands_df$var1 %in% c("p_ccpt", "p_ccpr")) ||
          any(commands_df$var2 %in% c("p_ccpt", "p_ccpr"))) {
        int_11 <- paste0(x_shortened, "1", prefix, y_shortened, "1")
        int_10 <- paste0(x_shortened, "1", prefix, y_shortened, "0")
        int_01 <- paste0(x_shortened, "0", prefix, y_shortened, "1")
        int_00 <- paste0(x_shortened, "0", prefix, y_shortened, "0")
        
      } else {
        int_11 <- paste0(x_shortened, "1ccpt1")
        int_10 <- paste0(x_shortened, "1ccpr0")
        int_01 <- paste0(x_shortened, "0ccpt1")
        int_00 <- paste0(x_shortened, "0ccpr0")
        
      }
      
    } else {
      if ((commands_df$var1 %in% c("p_ccpt", "p_ccpr")) ||
          (commands_df$var2 %in% c("p_ccpt", "p_ccpr"))) {
        int_11 <- paste0(x_shortened, "1", prefix, y_shortened, "1")
        int_10 <- paste0(x_shortened, "1", prefix, y_shortened, "0")
        int_01 <- paste0(x_shortened, "0", prefix, y_shortened, "1")
        int_00 <- paste0(x_shortened, "0", prefix, y_shortened, "0")
        int2_11 <-
          paste0(x_shortened, "1", suffix, y_shortened, "1")
        int2_10 <-
          paste0(x_shortened, "1", suffix, y_shortened, "0")
        int2_01 <-
          paste0(x_shortened, "0", suffix, y_shortened, "1")
        int2_00 <-
          paste0(x_shortened, "0", suffix, y_shortened, "0")
      } else {
        int_11 <- paste0(x_shortened, "1", prefix, y_shortened, "1")
        int_10 <- paste0(x_shortened, "1", prefix, y_shortened, "0")
        int_01 <- paste0(x_shortened, "0", prefix, y_shortened, "1")
        int_00 <- paste0(x_shortened, "0", prefix, y_shortened, "0")
        int2_11 <- paste0(x_shortened, "1ccpt1")
        int2_10 <- paste0(x_shortened, "1ccpr0")
        int2_01 <- paste0(x_shortened, "0ccpt1")
        int2_00 <- paste0(x_shortened, "0ccpr0")
        
        
      }
      
    }
    if (!stage %in% c(1, 2)) {
      int_params = c(int_11,
                     int_10,
                     int_01,
                     int_00,
                     int2_11,
                     int2_10,
                     int2_01,
                     int2_00)
      
    } else {
      int_params = c(int_11, int_10, int_01, int_00)
      
    }
    ecr_int <- c()
    int_int <- c()
    int_coupling <- c()
    for (int in int_params) {
      for (ecr in c(paste0("ECR", x, "_1"), paste0("ECR", x, "_0"))) {
        ecr_tmp <- paste0("\n", ecr, " ~~ 0*", int)
        ecr_int <- paste(ecr_int, ecr_tmp)
      }
      for (int2 in int_params) {
        if (int2 == int) {
          int_int_tmp <- paste0("\n", int, " ~~ ", int2)
          int_int <- paste(int_int, int_int_tmp)
        }
      }
      for (coupling_param in coupling_params) {
        int_coupling_tmp <- paste0("\n", coupling_param, " ~~ 0*", int)
        int_coupling <- paste(int_coupling, int_coupling_tmp)
      }
    }
    if (stage == 1) {
      int_prelevels <- paste0(
        "\n",
        prefix,
        y,
        "1 ~~ 0*",
        int_11,
        "\n",
        prefix,
        y,
        "1 ~~ 0*",
        int_10,
        "\n",
        prefix,
        y,
        "1 ~~ 0*",
        int_01,
        "\n",
        prefix,
        y,
        "1 ~~ 0*",
        int_00,
        "\n",
        prefix,
        y,
        "0 ~~ 0*",
        int_11,
        "\n",
        prefix,
        y,
        "0 ~~ 0*",
        int_10,
        "\n",
        prefix,
        y,
        "0 ~~ 0*",
        int_01,
        "\n",
        prefix,
        y,
        "0 ~~ 0*",
        int_00
      )
      
      
    } else if (stage == 2) {
      if (any(commands_df$var1 %in% c("p_ccpt", "p_ccpr")) ||
          any(commands_df$var2 %in% c("p_ccpt", "p_ccpr"))) {
        int_prelevels <- paste0(
          "\n",
          prefix,
          y,
          "1 ~~ 0*",
          int_11,
          "\n",
          prefix,
          y,
          "1 ~~ 0*",
          int_10,
          "\n",
          prefix,
          y,
          "1 ~~ 0*",
          int_01,
          "\n",
          prefix,
          y,
          "1 ~~ 0*",
          int_00,
          "\n",
          prefix,
          y,
          "0 ~~ 0*",
          int_11,
          "\n",
          prefix,
          y,
          "0 ~~ 0*",
          int_10,
          "\n",
          prefix,
          y,
          "0 ~~ 0*",
          int_01,
          "\n",
          prefix,
          y,
          "0 ~~ 0*",
          int_00
        )
        
        
      } else {
        int_prelevels <- c()
        for (int in int_params) {
          int_prelevels_tmp <-
            paste0("\n", int, " ~~ 0*ccpt \n", int, " ~~ 0*ccpr")
          int_prelevels <- paste(int_prelevels, int_prelevels_tmp)
        }
      }
    } else {
      if ((commands_df$var1 %in% c("p_ccpt", "p_ccpr")) ||
          (commands_df$var2 %in% c("p_ccpt", "p_ccpr"))) {
        int_prelevels <- c()
        for (int in int_params) {
          int_prelevels_tmp <-
            paste0("\n",
                   prefix,
                   y,
                   "1 ~~ 0*",
                   int,
                   "\n ",
                   prefix,
                   y,
                   "0 ~~ 0*",
                   int)
          int_prelevels <- paste(int_prelevels, int_prelevels_tmp)
        }
        
      } else {
        int_prelevels <- c()
        for (int in int_params) {
          int_prelevels_tmp <-
            paste0("\n", int, " ~~ 0*ccpt \n", int, " ~~ 0*ccpr")
          int_prelevels <- paste(int_prelevels, int_prelevels_tmp)
        }
        int_prelevels2 <- paste0(
          "\n",
          prefix,
          y,
          "1 ~~ 0*",
          int_11,
          "\n",
          prefix,
          y,
          "1 ~~ 0*",
          int_10,
          "\n",
          prefix,
          y,
          "1 ~~ 0*",
          int_01,
          "\n",
          prefix,
          y,
          "1 ~~ 0*",
          int_00,
          "\n",
          prefix,
          y,
          "0 ~~ 0*",
          int_11,
          "\n",
          prefix,
          y,
          "0 ~~ 0*",
          int_10,
          "\n",
          prefix,
          y,
          "0 ~~ 0*",
          int_01,
          "\n",
          prefix,
          y,
          "0 ~~ 0*",
          int_00
        )
        
        int_prelevels <- paste(int_prelevels, int_prelevels2)
      }
    }
    variances <-
      paste(ecr_int, int_int, int_prelevels, sep = "\n") %>% str_split("\\n")
    variances_df  <-
      data.frame(fullstatement = as.vector(variances[[1]])) %>% dplyr::mutate(fullstatement = as.character(fullstatement)) %>% dplyr::filter(fullstatement != "", fullstatement != " ") %>% mutate(fullstatement2 = fullstatement) %>% separate(fullstatement,
                                                                                                                                                                                                                                                into = c("var1", "var2"),
                                                                                                                                                                                                                                                sep = "~~") %>% mutate(var2 = gsub("0\\*", "", var2)) %>% mutate(
                                                                                                                                                                                                                                                  var1 = gsub(" ", "", var1),
                                                                                                                                                                                                                                                  var2 = gsub(" ", "", var2),
                                                                                                                                                                                                                                                  var12 = paste0(var1, var2)
                                                                                                                                                                                                                                                )
    aaa = params[[1]]
    bbb = params[[2]]
    ccc = params[[3]]
    ddd = params[[4]]
    eee = params[[5]]
    fff = params[[6]]
    ggg = params[[7]]
    hhh = params[[8]]
    
    if (stage == 1) {
      moderation_vars <- c(paste0(prefix, x, "1"), paste0(prefix, x, "0"))
    } else if (stage == 2) {
      if ((commands_df$var1 %in% c("p_ccpt", "p_ccpr")) ||
          (commands_df$var2 %in% c("p_ccpt", "p_ccpr"))) {
        moderation_vars <- c(paste0(prefix, x, "1"), paste0(prefix, x, "0"))
        
      } else {
        moderation_vars <- c(paste0("ECR", x, "_1"), paste0("ECR", x, "_0"))
        
      }
      
    } else {
      if ((commands_df$var1 %in% c("p_ccpt", "p_ccpr")) ||
          (commands_df$var2 %in% c("p_ccpt", "p_ccpr"))) {
        moderation_vars <-
          c(
            paste0(prefix, x, "1"),
            paste0(prefix, x, "0"),
            paste0(suffix, x, "1"),
            paste0(suffix, x, "0")
          )
      } else {
        moderation_vars <-
          c(
            paste0("ECR", x, " _1"),
            paste0("ECR", x, "_0"),
            paste0(prefix, x, "1"),
            paste0(prefix, x, "0")
          )
      }
    }
    mod_terms <- int_params
    # initialize the full syntax across the different parts of the model
    base_moderation_syntax <- c()
    base_ecr_syntax <- c()
    base_modterms_syntax <- c()
    if (stage == 1) {
      for (coupling_var in coupling_params) {
        for (moderation_var in moderation_vars) {
          tmp <- paste0("\n", coupling_var, " ~ ", moderation_var)
          base_moderation_syntax <- c(base_moderation_syntax, tmp)
        }
        for (j in c(paste0("ECR", x, "_1"), paste0("ECR", x, "_0"))) {
          tmp2 <- paste0("\n", coupling_var, " ~ ", j)
          base_ecr_syntax <- c(base_ecr_syntax, tmp2)
        }
        for (k in mod_terms) {
          tmp3 <- paste0("\n", coupling_var, " ~ ", k)
          base_modterms_syntax <- c(base_modterms_syntax, tmp3)
        }
      }
    } else if (stage == 2) {
      for (coupling_var in coupling_params) {
        if (coupling_var %in% c("p_ccpt", "p_ccpr")) {
          for (moderation_var in moderation_vars) {
            tmp <- paste0("\n", coupling_var, " ~ ", moderation_var)
            base_ecr_syntax <- c(base_ecr_syntax, tmp)
          }
          for (outcome in c(paste0(prefix, y, "1"), paste0(prefix, y, "0"))) {
            tmp2 <- paste0("\n", coupling_var, " ~ ", outcome)
            base_moderation_syntax <-
              c(base_moderation_syntax, tmp2)
          }
          for (k in mod_terms) {
            tmp3 <- paste0("\n", coupling_var, " ~ ", k)
            base_modterms_syntax <- c(base_modterms_syntax, tmp3)
          }
        }
      }
      for (outcome in c(paste0(prefix, y, "1"), paste0(prefix, y, "0"))) {
        for (moderation_var in moderation_vars) {
          tmp <- paste0("\n", outcome, " ~ ", moderation_var)
          base_ecr_syntax <- c(base_ecr_syntax, tmp)
        }
        for (k in mod_terms) {
          tmp3 <- paste0("\n ", outcome, " ~ ", k)
          base_modterms_syntax <- c(base_modterms_syntax, tmp3)
        }
        for (coupling_var in coupling_params) {
          if (!coupling_var %in% c("p_ccpt", "p_ccpr"))
            tmp2 <- paste0("\n ", outcome, " ~ ", coupling_var)
          base_moderation_syntax <-
            c(base_moderation_syntax, tmp2)
        }
      }
      
      
      
    } else {
      for (coupling_var in coupling_params) {
        for (moderation_var in moderation_vars) {
          tmp <- paste0("\n", coupling_var, " ~ ", moderation_var)
          base_moderation_syntax <- c(base_moderation_syntax, tmp)
        }
        for (j in c(paste0("ECR", x, "_1"), paste0("ECR", x, "_0"))) {
          tmp2 <- paste0("\n", coupling_var, " ~ ", j)
          base_ecr_syntax <- c(base_ecr_syntax, tmp2)
        }
        for (k in mod_terms) {
          tmp3 <- paste0("\n", coupling_var, " ~ ", k)
          base_modterms_syntax <- c(base_modterms_syntax, tmp3)
        }
      }
      for (coupling_var in coupling_params) {
        for (moderation_var in moderation_vars) {
          if (coupling_var %in% c("p_ccpt", "p_ccpr")) {
            tmp <- paste0("\n", coupling_var, " ~ ", moderation_var)
            base_moderation_syntax <- c(base_moderation_syntax, tmp)
          } else {
            tmp <- paste0("\n", moderation_var, " ~ ", coupling_var)
            base_moderation_syntax <- c(base_moderation_syntax, tmp)
          }
        }
      }
      for (moderation_var in moderation_vars) {
        for (j in c(paste0("ECR", x, "_1"), paste0("ECR", x, "_0"))) {
          tmp2 <- paste0("\n", moderation_var, " ~ ", j)
          base_ecr_syntax <- c(base_ecr_syntax, tmp2)
        }
        for (k in mod_terms) {
          tmp_3 <- paste0("\n ", moderation_var, " ~ ", k)
          
        }
      }
      
    }
    base_ecr_df <-
      data.frame(fullstatement = as.vector(base_ecr_syntax)) %>% mutate(fullstatement = gsub("\\n", "", fullstatement)) %>% separate(fullstatement,
                                                                                                                                     into = c("var1", "var2"),
                                                                                                                                     sep =  " ~ ") %>% mutate(sep = "ON")
    base_moderation_df <-
      data.frame(fullstatement = as.vector(base_moderation_syntax)) %>% mutate(fullstatement = gsub("\\n", "", fullstatement)) %>% separate(fullstatement,
                                                                                                                                            into = c("var1", "var2"),
                                                                                                                                            sep =  " ~ ") %>% mutate(sep = "ON")
    base_modterms_df <-
      data.frame(fullstatement = as.vector(base_modterms_syntax)) %>% mutate(fullstatement = gsub("\\n", "", fullstatement)) %>% separate(fullstatement,
                                                                                                                                          into = c("var1", "var2"),
                                                                                                                                          sep =  " ~ ") %>% mutate(sep = "ON")
    if (any(commands_df$var1 %in% c("p_ccpt", "p_ccpr")) ||
        any(commands_df$var2 %in% c("p_ccpt", "p_ccpr"))) {
      base_modterms_df <-
        dplyr::filter(base_modterms_df, var1 %in% c("p_ccpt", "p_ccpr"))
      base_moderation_df <-
        dplyr::filter(base_moderation_df, var1 %in% c("p_ccpt", "p_ccpr"))
      base_ecr_df <-
        dplyr::filter(base_ecr_df, var1 %in% c("p_ccpt", "p_ccpr"))
      
      
    }
    
    base_modterms_df <-
      base_modterms_df %>% mutate(var2_rep = var2) %>% separate(var2_rep, into = c("ecr_var", "saam_var"), 3) %>% mutate(
        var1_DyadID = if_else(var1 %in% c("ccpt", "p_ccpt") ||
                                grepl("1", var1), 1, 0),
        saam_var_DyadID = if_else(grepl("0", saam_var), 0, 1),
        ecr_var_DyadID = if_else(grepl("0", ecr_var), 0, 1)
      ) %>% mutate(
        path = if_else(var1_DyadID == ecr_var_DyadID, "A", "P"),
        prefix = case_when(
          stage == 1 ~ "S1",
          stage == 2 ~ "S2",
          grepl("pr", saam_var) ~ "S1",
          grepl("pt", saam_var) ~ "S2"
        ),
        coupling_suffix = case_when(
          var1 %in% c("ccpt", "ccpr", "ccpt1", "ccpr0") ~ "N",
          ecr_var %in% c("ccpt", "ccpr", "ccpt1", "ccpr0") ~ "N",
          saam_var %in% c("ccpt", "ccpr", "ccpt1", "ccpr0") ~ "N",
          TRUE ~ "P"
        )
      )
    
    
    base_modterms_df <-
      mutate(
        base_modterms_df,
        varpattern = paste0(
          coupling_suffix,
          prefix,
          var1_DyadID,
          saam_var_DyadID,
          ecr_var_DyadID
        )
      ) %>% mutate(
        allfree_prefix = "",
        aonly_prefix = if_else(path == "A", "", "0*"),
        ponly_prefix = if_else(path == "P", "", "0*"),
        afixed_prefix = case_when(
          path == "P" ~ "",
          varpattern  == "NS1111" ~ paste0(aaa, "*"),
          varpattern == "NS1000" ~ paste0(aaa, "*"),
          varpattern == "NS1101" ~ paste0(bbb, "*"),
          varpattern == "NS1010" ~ paste0(bbb, "*"),
          varpattern == "NS2111" ~ paste0(eee, "*"),
          varpattern == "NS2000" ~ paste0(eee, "*"),
          varpattern == "NS2101" ~ paste0(fff, "*"),
          varpattern == "NS2010" ~ paste0(fff, "*"),
          varpattern  == "PS1111" ~ paste0(p_aaa, "*"),
          varpattern == "PS1000" ~ paste0(p_aaa, "*"),
          varpattern == "PS1101" ~ paste0(p_bbb, "*"),
          varpattern == "PS1010" ~ paste0(p_bbb, "*"),
          varpattern == "PS2111" ~ paste0(p_eee, "*"),
          varpattern == "PS2000" ~ paste0(p_eee, "*"),
          varpattern == "PS2101" ~ paste0(p_fff, "*"),
          varpattern == "PS2010" ~ paste0(p_fff, "*")
        ),
        pfixed_prefix = case_when(
          path == "A" ~ "",
          varpattern == "NS1110" ~ paste0(ccc, "*"),
          varpattern == "NS1001" ~ paste0(ccc, "*"),
          varpattern == "NS1100" ~ paste0(ddd, "*"),
          varpattern == "NS1011" ~ paste0(ddd, "*"),
          varpattern == "NS2110" ~ paste0(ggg, "*"),
          varpattern == "NS2001" ~ paste0(ggg, "*"),
          varpattern == "NS2100" ~ paste0(hhh, "*"),
          varpattern == "NS2011" ~ paste0(hhh, "*"),
          varpattern == "PS1110" ~ paste0(p_ccc, "*"),
          varpattern == "PS1001" ~ paste0(p_ccc, "*"),
          varpattern == "PS1100" ~ paste0(p_ddd, "*"),
          varpattern == "PS1011" ~ paste0(p_ddd, "*"),
          varpattern == "PS2110" ~ paste0(p_ggg, "*"),
          varpattern == "PS2001" ~ paste0(p_ggg, "*"),
          varpattern == "PS2100" ~ paste0(p_hhh, "*"),
          varpattern == "PS2011" ~ paste0(p_hhh, "*")
        ),
        indist_prefix = case_when(
          varpattern == "NS1111" ~ paste0(aaa, "*"),
          varpattern == "NS1000" ~ paste0(aaa, "*"),
          varpattern == "NS1101" ~ paste0(bbb, "*"),
          varpattern == "NS1010" ~ paste0(bbb, "*"),
          varpattern == "NS2111" ~ paste0(eee, "*"),
          varpattern == "NS2000" ~ paste0(eee, "*"),
          varpattern == "NS2101" ~ paste0(fff, "*"),
          varpattern == "NS2010" ~ paste0(fff, "*"),
          varpattern == "NS1110" ~ paste0(ccc, "*"),
          varpattern == "NS1001" ~ paste0(ccc, "*"),
          varpattern == "NS1100" ~ paste0(ddd, "*"),
          varpattern == "NS1011" ~ paste0(ddd, "*"),
          varpattern == "NS2110" ~ paste0(ggg, "*"),
          varpattern == "NS2001" ~ paste0(ggg, "*"),
          varpattern == "NS2100" ~ paste0(hhh, "*"),
          varpattern == "NS2011" ~ paste0(hhh, "*"),
          varpattern == "PS1111" ~ paste0(p_aaa, "*"),
          varpattern == "PS1000" ~ paste0(p_aaa, "*"),
          varpattern == "PS1101" ~ paste0(p_bbb, "*"),
          varpattern == "PS1010" ~ paste0(p_bbb, "*"),
          varpattern == "PS2111" ~ paste0(p_eee, "*"),
          varpattern == "PS2000" ~ paste0(p_eee, "*"),
          varpattern == "PS2101" ~ paste0(p_fff, "*"),
          varpattern == "PS2010" ~ paste0(p_fff, "*"),
          varpattern == "PS1110" ~ paste0(p_ccc, "*"),
          varpattern == "PS1001" ~ paste0(p_ccc, "*"),
          varpattern == "PS1100" ~ paste0(p_ddd, "*"),
          varpattern == "PS1011" ~ paste0(p_ddd, "*"),
          varpattern == "PS2110" ~ paste0(p_ggg, "*"),
          varpattern == "PS2001" ~ paste0(p_ggg, "*"),
          varpattern == "PS2100" ~ paste0(p_hhh, "*"),
          varpattern == "PS2011" ~ paste0(p_hhh, "*")
        )
      )
    
    
    
    
    base_modterms_df <-
      mutate(
        base_modterms_df,
        full_saam_var = case_when(
          grepl("av", saam_var) ~ gsub("av", "avo", saam_var),
          grepl("an", saam_var) ~ gsub("an", "anx", saam_var),
          grepl("se", saam_var) ~ gsub("se", "sec", saam_var),
          saam_var == "ccpt1" ~ "ccpt",
          saam_var == "ccpr0" ~ "ccpr",
          TRUE ~ saam_var
        )
      )
    
    
    commands_df <-
      mutate(
        commands_df,
        var1 = gsub(" ", "", var1),
        var2 = gsub(" ", "", var2),
        var12 = paste0(var1, var2),
        var21 = paste0(var2, var1)
      )
    base_modterms_df <-
      mutate(
        base_modterms_df,
        base_vars = paste0(var1, full_saam_var),
        base_vars = gsub(" ", "", base_vars),
        path_included_in_base_model = if_else(
          base_vars %in% c(
            dplyr::filter(commands_df, zero_constraints != 0) %>% pull(var12) %>% as.vector(),
            dplyr::filter(commands_df, zero_constraints != 0) %>% pull(var21) %>% as.vector()
          ),
          TRUE,
          FALSE
        )
      )
    
    base_modterms_df <-
      mutate(
        base_modterms_df,
        full_ecr_var = case_when(
          grepl("av", ecr_var) ~ gsub("av", "avo", ecr_var),
          grepl("an", ecr_var) ~ gsub("an", "anx", ecr_var)
        ),
        full_ecr_var = gsub("0", "_0", full_ecr_var),
        full_ecr_var = gsub("1", "_1", full_ecr_var),
        ecr_base_vars = paste0(var1, "ECR", full_ecr_var)
      ) %>% mutate_all(funs(gsub(" ", "", .)))
    
    base_ecr_df <-
      mutate(
        base_ecr_df,
        var1_DyadID = case_when(grepl("pt", var1) ~ 1,
                                grepl("1", var1) ~ 1,
                                TRUE ~ 0),
        var2_DyadID = case_when(grepl("1", var2) ~ 1,
                                grepl("pt", var2) ~ 1,
                                TRUE ~ 0),
        path = if_else(var1_DyadID == var2_DyadID, "A", "P"),
        var12 = paste0(var1, var2),
        coupling_suffix = if_else(var1 %in% c("ccpt", "ccpr"), "N", "P"),
        base_vars = paste0(var1_DyadID, var2_DyadID),
        varpattern = paste0(coupling_suffix, base_vars),
        path_included_in_ecr_model = if_else(var12 %in% as.vector(
          dplyr::filter(base_modterms_df, path_included_in_base_model == TRUE) %>% pull(ecr_base_vars)
        ), TRUE, FALSE)
      ) %>%
      mutate(
        allfree_prefix = "",
        aonly_prefix = if_else(path == "A", "", "0*"),
        ponly_prefix = if_else(path == "P", "", "0*"),
        afixed_prefix = case_when(
          path == "P" ~ "",
          varpattern  == "N11" ~ paste0("e", aaa, "*"),
          varpattern == "N00" ~ paste0("e", aaa, "*"),
          varpattern == "P11" ~ paste0("e", bbb, "*"),
          varpattern == "P00" ~ paste0("e", bbb, "*")
        ),
        pfixed_prefix = case_when(
          path == "A" ~ "",
          varpattern  == "N10" ~ paste0("e", ccc, "*"),
          varpattern == "N01" ~ paste0("e", ccc, "*"),
          varpattern == "P10" ~ paste0("e", ddd, "*"),
          varpattern == "P01" ~ paste0("e", ddd, "*")
        ),
        indist_prefix = case_when(
          varpattern  == "N11" ~ paste0("e", aaa, "*"),
          varpattern == "N00" ~ paste0("e", aaa, "*"),
          varpattern == "P11" ~ paste0("e", bbb, "*"),
          varpattern == "P00" ~ paste0("e", bbb, "*"),
          varpattern  == "N10" ~ paste0("e", ccc, "*"),
          varpattern == "N01" ~ paste0("e", ccc, "*"),
          varpattern == "P10" ~ paste0("e", ddd, "*"),
          varpattern == "P01" ~ paste0("e", ddd, "*")
        )
      )
    
    ecr_vars <-
      dplyr::filter(base_ecr_df, path_included_in_ecr_model == TRUE) %>% pull(var2) %>% unique() %>% as.vector()
    modterms_vars <-
      dplyr::filter(base_modterms_df, path_included_in_base_model == TRUE) %>% pull(var2) %>% unique() %>% as.vector()
    commands_var1 <-
      dplyr::filter(commands_df, !is.na(var2)) %>% pull(var1) %>% unique() %>% as.vector()
    commands_var2 <-
      dplyr::filter(commands_df, !is.na(var2)) %>% pull(var2) %>% unique() %>% as.vector()
    model_vars <-
      c(ecr_vars, modterms_vars, commands_var1, commands_var2)
    base_modterms_df <-
      dplyr::mutate(base_modterms_df, var12 = paste0(var1, var2))
    variances_df <-
      mutate(
        variances_df,
        path_included_in_base_model = if_else(var12 %in% c(commands_df$var12, commands_df$var21), 1, 0),
        path_included_in_modterms = if_else(
          var12 %in% c((
            dplyr::filter(base_modterms_df, path_included_in_base_model == TRUE) %>% pull(ecr_base_vars) %>% as.vector()
          ),
          dplyr::filter(base_modterms_df, path_included_in_base_model == TRUE) %>% pull(base_vars) %>% as.vector(),
          dplyr::filter(base_modterms_df, path_included_in_base_model == TRUE) %>% pull(var12) %>% as.vector()
          ),
          1,
          0
        ),
        var1_include_in_model = if_else(var1 %in% model_vars, 1, 0),
        var2_included_in_model = if_else(var2 %in% model_vars, 1, 0),
        paths_inclusion = paste0(
          path_included_in_base_model,
          path_included_in_modterms,
          var1_include_in_model,
          var2_included_in_model
        ),
        include_in_output = if_else(paths_inclusion == "0011", TRUE, FALSE)
      )
    # Now that have all the possible combinations
    # Filter out things that useless AND add in actor and partner paths
    # To do this make a data frame with all of these lines
    
    #p_ccpr ~ pravo1
    
    #p_ccpr ~ pravo0
    
    #p_ccpr ~ ECRavo_0
    #p_ccpr ~ ECRavo_1
    
    #p_ccpr ~ av0prav1
    #p_ccpr ~ av1prav1
    #p_ccpr ~ av0prav0
    #p_ccpr ~ av1prav0
    
    #p_ccpt ~ pravo1
    #p_ccpt ~ pravo0
    
    #p_ccpt ~ ECRavo_1
    #p_ccpt ~ ECRavo_0
    
    #p_ccpt ~ av0prav1
    #p_ccpt ~ av1prav1
    #p_ccpt ~ av0prav0
    #p_ccpt ~ av1prav0
    
    #ccpr ~ pravo1
    #ccpr ~ ECRavo_0
    #ccpr ~ av0prav1
    #ccpr ~ ECRavo_1
    #ccpr ~ av1prav1
    #ccpr ~ pravo0
    #ccpr ~ av0prav0
    #ccpr ~ av1prav0
    
    #ccpt ~ pravo1
    #ccpt ~ ECRavo_0
    #ccpt ~ av0prav1
    #ccpt ~ ECRavo_1
    #ccpt ~ av1prav1
    #ccpt ~ pravo0
    #ccpt ~ av0prav0
    #ccpt ~ av1prav0
    # need to stitch together ecr, modterms, and variances df
    
    to_output_variances_vec <-
      dplyr::filter(variances_df, include_in_output == TRUE) %>% pull(fullstatement2) %>% paste(collapse = "\n")
    
    if (version == "allfree") {
      base_ecr_vec <-
        dplyr::filter(base_ecr_df, path_included_in_ecr_model == TRUE) %>% dplyr::mutate(to_output = paste0("\n", var1, " ~ ", allfree_prefix, var2)) %>% pull(to_output) %>% paste(collapse = "\n")
      base_modterms_vec <-
        dplyr::filter(base_modterms_df, path_included_in_base_model == TRUE) %>% dplyr::mutate(to_output = paste0("\n", var1, " ~ ", allfree_prefix, var2)) %>% pull(to_output) %>% paste(collapse = "\n")
      a <-
        paste(to_output_variances_vec,
              base_ecr_vec,
              base_modterms_vec,
              collapse = "\n")
      return(a)
    } else if (version == "afixed") {
      base_ecr_vec <-
        dplyr::filter(base_ecr_df, path_included_in_ecr_model == TRUE) %>% dplyr::mutate(to_output = paste0("\n", var1, " ~ ", afixed_prefix, var2)) %>% pull(to_output) %>% paste(collapse = "\n")
      base_modterms_vec <-
        dplyr::filter(base_modterms_df, path_included_in_base_model == TRUE) %>% dplyr::mutate(to_output = paste0("\n", var1, " ~ ", afixed_prefix, var2)) %>% pull(to_output) %>% paste(collapse = "\n")
      b <-
        paste(to_output_variances_vec,
              base_ecr_vec,
              base_modterms_vec,
              collapse = "\n")
      
      return(b)
    } else if (version == "pfixed") {
      base_ecr_vec <-
        dplyr::filter(base_ecr_df, path_included_in_ecr_model == TRUE) %>% dplyr::mutate(to_output = paste0("\n", var1, " ~ ", pfixed_prefix, var2)) %>% pull(to_output) %>% paste(collapse = "\n")
      base_modterms_vec <-
        dplyr::filter(base_modterms_df, path_included_in_base_model == TRUE) %>% dplyr::mutate(to_output = paste0("\n", var1, " ~ ", pfixed_prefix, var2)) %>% pull(to_output) %>% paste(collapse = "\n")
      c <-
        paste(to_output_variances_vec,
              base_ecr_vec,
              base_modterms_vec,
              collapse = "\n")
      
      return(c)
    } else if (version == "apfixed") {
      base_ecr_vec <-
        dplyr::filter(base_ecr_df, path_included_in_ecr_model == TRUE) %>% dplyr::mutate(to_output = paste0("\n", var1, " ~ ", indist_prefix, var2)) %>% pull(to_output) %>% paste(collapse = "\n")
      base_modterms_vec <-
        dplyr::filter(base_modterms_df, path_included_in_base_model == TRUE) %>% dplyr::mutate(to_output = paste0("\n", var1, " ~ ", indist_prefix, var2)) %>% pull(to_output) %>% paste(collapse = "\n")
      d <-
        paste(to_output_variances_vec,
              base_ecr_vec,
              base_modterms_vec,
              collapse = "\n")
      
      
      return(d)
    } else if (version == "aonly") {
      base_ecr_vec <-
        dplyr::filter(base_ecr_df, path_included_in_ecr_model == TRUE) %>% dplyr::mutate(to_output = paste0("\n", var1, " ~ ", aonly_prefix, var2)) %>% pull(to_output) %>% paste(collapse = "\n")
      base_modterms_vec <-
        dplyr::filter(base_modterms_df, path_included_in_base_model == TRUE) %>% dplyr::mutate(to_output = paste0("\n", var1, " ~ ", aonly_prefix, var2)) %>% pull(to_output) %>% paste(collapse = "\n")
      e <-
        paste(to_output_variances_vec,
              base_ecr_vec,
              base_modterms_vec,
              collapse = "\n")
      
      return(e)
    } else if (version == "ponly") {
      base_ecr_vec <-
        dplyr::filter(base_ecr_df, path_included_in_ecr_model == TRUE) %>% dplyr::mutate(to_output = paste0("\n", var1, " ~ ", ponly_prefix, var2)) %>% pull(to_output) %>% paste(collapse = "\n")
      base_modterms_vec <-
        dplyr::filter(base_modterms_df, path_included_in_base_model == TRUE) %>% dplyr::mutate(to_output = paste0("\n", var1, " ~ ", ponly_prefix, var2)) %>% pull(to_output) %>% paste(collapse = "\n")
      f <-
        paste(to_output_variances_vec,
              base_ecr_vec,
              base_modterms_vec,
              collapse = "\n")
      
      return(f)
    }
    
  }


univariateoutiers_min <- function(vec) {
  min <- as.numeric(quantile(vec, .25))
  max <- as.numeric(quantile(vec, .75))
  iqr = IQR(vec)
  min_cutoff <- min - 1.5 * (iqr)
  return(min_cutoff)
  
  
  
}
univariateoutiers_max <- function(vec) {
  min <- as.numeric(quantile(vec, .25))
  max <- as.numeric(quantile(vec, .75))
  iqr = IQR(vec)
  max_cutoff <- max + 1.5 * (iqr)
  return(max_cutoff)
  
  
  
}

corwithtarget <-
  function(df,
           omit = NULL,
           target,
           withvars = NULL,
           pmin = NULL,
           partial = NULL,
           absrmin = NULL,
           digits = 3,
           prewhiten = FALSE,
           orderbyr = FALSE) {
    require(forecast)
    
    if (!is.null(omit)) {
      dnames <- which(names(df) %in% omit)
      df <- df[,-1 * dnames]
    }
    
    if (is.null(withvars)) {
      withvars <- names(df)[which(!names(df) %in% target)]
    }
    
    if (!is.null(partial)) {
      df <- as.data.frame(lapply(df, function(col) {
        residuals(lm(col ~ as.matrix(df[, partial])))
      }))
    }
    
    res <- sapply(target, function(tv) {
      cvec <- sapply(withvars, function(wv) {
        #prewhiten?
        if (prewhiten) {
          r <- residuals(lm(df[, tv] ~ df[, wv]))
          a <- auto.arima(r)
          x <- Arima(df[, wv], model = a)$residuals
          y <- Arima(df[, tv], model = a)$residuals
        } else {
          x <- df[, wv]
          y <- df[, tv]
        }
        
        tryCatch(
          rc <-
            Hmisc::rcorr(x, y),
          error = function(e) {
            print(e)
            
          }
        )
        list(r = round(rc$r[1, 2], 3), p = round(rc$P[1, 2], 3))
      })
      
      if (!is.null(pmin)) {
        sigr <- which(unlist(cvec["p",]) <= pmin)
        if (length(sigr) == 0L) {
          cvec <- c()
        } else {
          cvec <- cvec[, sigr, drop = FALSE]
        }
      }
      
      if (!is.null(absrmin)) {
        goodr <- which(abs(unlist(cvec["r",])) >= absrmin)
        if (length(goodr) == 0L) {
          cvec <- c()
        } else {
          cvec <- cvec[, goodr, drop = FALSE]
        }
      }
      
      #be sure that we never include the correlation of the variable with itself
      selfmatch <- dimnames(cvec)[[2]] == tv
      cvec <- cvec[,!selfmatch, drop = FALSE]
      
      #reorder by correlation size if requested
      if (orderbyr == TRUE) {
        cvec <- cvec[, order(unlist(cvec[1,]), decreasing = TRUE)]
      }
      
      return(cvec)
      
      #print(cvec)
    }, simplify = FALSE)
    
    return(res)
  }

runBSEM <- function(modelsyntaxobj) {
  mobj = mplusObject(
    TITLE = "Testing",
    usevariables = as.vector(modelsyntaxobj[[1]]),
    ANALYSIS = "ESTIMATOR=BAYES; PROCESSORS = 2;BITERATIONS = (49000)",
    OUTPUT = "STANDARDIZED;",
    MODEL = modelsyntaxobj[[2]],
    rdata = modelsyntaxobj[[4]]
  )
  mbin <- "~/Applications/Mplus/mplus" # change based on where mplus lives locally
  fitted <-
    mplusModeler(
      mobj,
      modelout = paste0(modelsyntaxobj[[5]], ".inp"),
      run = TRUE,
      Mplus_command = mbin,
      hashfilename = FALSE
    )
}

modsyntax <- function(dat, model) {
  div10 <- function(x) {
    return(x / 10)
  }
  mult10 <- function(x) {
    return(x * 10)
  }
  
  mname = deparse(substitute(model))
  d <- sem(
    model,
    dat,
    missing = "ML",
    estimator = "MLR",
    meanstructure = TRUE,
    mimic = "Mplus"
  )
  paramest <- parameterestimates(d)
  allvarnames = ""
  modsyntax = ""
  allvarsyntax = ""
  lavmodsyntax = ""
  for (i in 1:length(paramest$lhs)) {
    lhs = paramest$lhs[[i]]
    op = paramest$op[[i]]
    rhs = paramest$rhs[[i]]
    label = paramest$label[[i]]
    param = paramest$est[[i]]
    lavlinesyntax = ifelse((label == "" ||
                              op == ":=" ||
                              is.null(label)),
                           paste0(lhs, op, rhs, "\n"),
                           paste0(lhs, op, label, "*", rhs, "\n")
    )
    lavlinesyntax2 = ifelse(param == 0, paste0(lhs, op, "0*", rhs, "\n"), lavlinesyntax)
    lavmodsyntax = paste0(lavmodsyntax, lavlinesyntax2)
    
    if (op == "~~") {
      opr = " WITH "
    } else if (op == "~") {
      opr = " ON "
    } else if (op == "~1") {
      lhs = paste0("[", lhs, "]")
      
      opr = ""
      
    } else {
      opr = op
    }
    if (label == "" ||
        (op == ":=") || is.null(label)) {
      lab = ""
    } else {
      lab = paste0("* (", label, ")")
    }
    if (op == ":=") {
      linesyntax = ""
      newvarname = label
      allvarnames = paste(allvarnames, newvarname, sep = " ")
      newvarsyntax = paste0(lhs, " = ", rhs, lab, ";\n")
      allvarsyntax = paste0(allvarsyntax, newvarsyntax)
      modsyntax = paste0(modsyntax, linesyntax)
    } else if (param == 0) {
      linesyntax = paste0(lhs, opr, rhs, "@0;\n")
      modsyntax = paste0(modsyntax, linesyntax)
    } else {
      linesyntax = paste0(lhs, opr, rhs, lab, "; \n")
      modsyntax = paste0(modsyntax, linesyntax)
    }
  }
  if (allvarsyntax != "") {
    modsyntax = paste0(modsyntax,
                       "\n",
                       "MODEL CONSTRAINT: \n",
                       "NEW (",
                       allvarnames,
                       " ); \n",
                       allvarsyntax)
  }
  
  usevardf <-
    data.frame(
      var_lhs = as.vector(as.character(paramest$lhs)),
      opr = as.vector(paramest$op),
      var_rhs = as.vector(as.character(paramest$rhs))
    )
  usevardf <- dplyr::filter(usevardf,  opr != ":=", opr != "~1")
  usevardf_long <-
    tidyr::gather(usevardf, key = "var_side", value = "var", -opr) %>% dplyr::filter(var != "")
  
  usevar_asvector <- unique(as.vector(usevardf_long$var))
  fname <- paste0(bsem_dir, "/", mname, "_df.dat")
  print(fname)
  datObj <- prepareMplusData(dat, fname)
  return(list(usevar_asvector, modsyntax, datObj, dat, mname, d))
}

pull_fitmeasures <-
  function(m, y, data_frame = posnegint_personality) {
    if (is.null(m) || is.null(y)) {
      new_fits <- c(0, 0, 0, 0,
                    0, 0, 0, 0)
      return(new_fits)
    }
    m_name <- y
    bsem_out <- runBSEM(modsyntax(dat = data_frame, m_name))
    dic <- bsem_out$results$summaries$DIC
    new_fits <-
      as.vector(fitmeasures(
        m,
        fit.measures = c(
          "unrestricted.logl",
          "aic",
          "bic",
          "baseline.df",
          "cfi.scaled",
          "rmsea.scaled",
          "srmr"
        )
      ))
    new_fits <- c(new_fits, dic)
    return(new_fits)
    
  }
