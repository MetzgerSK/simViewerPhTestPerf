# DGP settings ----
tmax     <- 75      # always same across all scens, for now.
scaleAll <- 0.15

# >> Scenarios
scenarios <- NULL
scCnt <- 1

# If runStart object isn't in memory, means the file's being called from the
# Shiny app, to obtain scenario definitions.  Skip any of the steps that import
# RNGs (b/c wd won't be correct, if calling from app, which will cause an error).
mainSimCall <- ("runStart" %in% ls()) # TRUE if in memory (=called from main sim), FALSE if not

# (Scenario 1 ====
## ** redacted for PA replic files **
    sc  <-  NULL

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 2 ====
## ** redacted for PA replic files **
    sc <- scenarios[[1]]

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 3 ====
## ** redacted for PA replic files **
    sc <- scenarios[[1]]

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 4 ====
## ** redacted for PA replic files **
    sc <- scenarios[[1]]

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 5 ====
## ** redacted for PA replic files **
    sc <- scenarios[[4]]

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 6 ====
## ** redacted for PA replic files **
    sc <- scenarios[[4]]

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 7 ====
## Description: Same as Sc1, except that true FF is now fully linear
    sc <- scenarios[[1]]
    sc$rng   <- c("nObs100"  = 673996775,  
                  "nObs250"  = 1731113347, 
                  "nObs1000" = 1216115821)  
    
    sc$datType = "cont"

    sc$scale = scaleAll
    sc$shape = 1
    
    sc$betaList = c(x1 = 0.1, x2 = 0)
    sc$tdeList  = c(x2 = 1)
    sc$tFF = "log"
    
    sc$x1FF = expr(rnorm(n))
    sc$x2FF = expr(rbinom(n, 1, 0.5))
    
    sc$rgtSpec = as.formula(Surv(`_t0`, `_t`, `_d`) ~ x1 + x2)

    sc$censPerc = c(0, 0.25, 0.5)

    sc$tmax = tmax    # in case you decide to customize later

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 8 ====
## Description: Same as Sc7, only with a shape = 0.75.
    sc <- scenarios[[7]]
    sc$rng   <- c("nObs100"  = 1539015601, 
                  "nObs250"  = 750464840,  
                  "nObs1000" = 162134646)   
    sc$shape <- 0.75

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 9 ====
## Description: Same as Sc7, only with a shape = 1.25.
    sc <- scenarios[[7]]
    sc$rng   <- c("nObs100"  = 418058719,  
                  "nObs250"  = 1710680955, 
                  "nObs1000" = 1619803210)  
    sc$shape <- 1.25

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 10 ====
## ** redacted for PA replic files **
    sc <- scenarios[[1]]

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 11 ====
## ** redacted for PA replic files **
    sc <- scenarios[[2]]

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 12 ====
## ** redacted for PA replic files **
    sc <- scenarios[[3]]

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 13 ====
## ** redacted for PA replic files **
    sc <- scenarios[[4]]

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 14 ====
## ** redacted for PA replic files **
    sc <- scenarios[[5]]
    
    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 15 ====
## ** redacted for PA replic files **
    sc <- scenarios[[6]]

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 16 ====
## Description: Same as Sc7, but x2 now has a main effect (not just TDE).  The
##              effect's half the size of the TDE, but signed in the same way.
    sc <- scenarios[[7]]
    sc$rng     <- c("nObs100"  = 1341999138, 
                    "nObs250"  = 709648089,  
                    "nObs1000" = 1663999576)  
    
    sc$betaList[["x2"]] <- 0.5

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 17 ====
## Description: Same as Sc8, but x2 now has a main effect (not just TDE).  The
##              effect's half the size of the TDE, but signed in the same way.
    sc <- scenarios[[8]]
    sc$rng     <- c("nObs100"  = 805332153,  
                    "nObs250"  = 684172345, 
                    "nObs1000" = 32799908)  
    
    sc$betaList[["x2"]] <- 0.5

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 18 ====
## Description: Same as Sc9, but x2 now has a main effect (not just TDE).  The
##              effect's half the size of the TDE, but signed in the same way.
    sc <- scenarios[[9]]
    sc$rng     <- c("nObs100"  = 507179838,  
                    "nObs250"  = 511815685,  
                    "nObs1000" = 1518936267)  
    
    sc$betaList[["x2"]] <- 0.5

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 19 ====
## ** redacted for PA replic files **
    sc <- scenarios[[1]]

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 20 ====
## ** redacted for PA replic files **
    sc <- scenarios[[2]]

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 21 ====
## ** redacted for PA replic files **
    sc <- scenarios[[3]]

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 22 ====
## ** redacted for PA replic files **
    sc <- scenarios[[4]]

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 23 ====
## ** redacted for PA replic files **
    sc <- scenarios[[5]]

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 24 ====
## ** redacted for PA replic files **
    sc <- scenarios[[6]]

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 25 ====
## Description: Same as Sc7, but x2 now has a main effect (not just TDE).  The
##              effect's half the size of the TDE, but signed in the OPPOSITE way.
    sc <- scenarios[[7]]
    sc$rng     <- c("nObs100"  = 1466481537, 
                    "nObs250"  = 1436830036, 
                    "nObs1000" = 846173436)   
    
    sc$betaList[["x2"]] <- -0.5

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 26 ====
## Description: Same as Sc8, but x2 now has a main effect (not just TDE).  The
##              effect's half the size of the TDE, but signed in the OPPOSITE way.
    sc <- scenarios[[8]]
    sc$rng     <- c("nObs100"  = 475777811, 
                    "nObs250"  = 491470612, 
                    "nObs1000" = 846505441)  
    
    sc$betaList[["x2"]] <- -0.5

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1

# (Scenario 27 ====
## Description: Same as Sc9, but x2 now has a main effect (not just TDE).  The
##              effect's half the size of the TDE, but signed in the OPPOSITE way.
    sc <- scenarios[[9]]
    sc$rng     <- c("nObs100"  = 735196852,  
                    "nObs250"  = 1527499691, 
                    "nObs1000" = 417500067)   
    
    sc$betaList[["x2"]] <- -0.5

    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1
    
# (Scenarios 28-54 ====
## Exact same as their counterparts above, except that they're the true Keele
## redux Sc3-4 equivalent: using runif() for x1.  (To ensure you can replicate
## the general gist of that paper.)  You'll need to swap the x1FF statement.

# We're totally setting the RNGs with a loop, though.
## Import the enormous set of RNGs you generated via random.org
if(isTRUE(mainSimCall)){
    rngs_sc28_54 <- 
         readr::read_delim("../rngs_sc28-54.txt", 
             delim = "\t", escape_double = FALSE, 
             col_names = FALSE, comment = "//", trim_ws = TRUE,
             show_col_types = FALSE) %>%
            
            mutate(X1_mod = if_else(mod(X2,2)==0,
                                1*10^ceiling(log10(X1)) + X1,
                                X1)
            ) %>%
            select(X1_mod) %>%
            pull
}
for(c in 1:length(scenarios)){
    # Load this 1-27 scenario into its 28-54 equivalent
    sc <- scenarios[[c]]
    
    # If this scenario value isn't null, fill in information.
    if(!is.null(sc)){
        # Change up the RNGs
        if(isTRUE(mainSimCall)){
            ## get the set of three for this 28-54
            rng3 <- rngs_sc28_54[c((3*c - 2):(3*c))]
            ## save the vector names, apply to set of three
            names(rng3) <- names(sc$rng)
            
            ## insert the new seeds
            sc$rng <- rng3
        }
        # Change up the x1 gen stmt to runif
        sc$x1FF <- expr(runif(n))
    }
    
    # Store in scenarios obj, advance to next
    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1
}
    
# Try to tidy up, to help with memory    
suppressWarnings(rm(c, rng3, rngs_sc28_54, sc))   # will still execute, even if rng3 et al. don't exist (only throws warning)

# (Scenarios 55-81 ====
## Exact same as Sc. 1-27, except that x2 is now normally distributed.

# Like 28-54, though, we're totally setting the RNGs with a loop.
if(isTRUE(mainSimCall)){
    rngs_sc55_81 <- 
         readr::read_delim("../rngs_sc55-81.txt", 
             delim = "\t", escape_double = FALSE, 
             col_names = FALSE, comment = "//", trim_ws = TRUE,
             show_col_types = FALSE) %>%

            mutate(X1_mod = if_else(mod(X2,2)==0,
                                1*10^ceiling(log10(X1)) + X1,
                                X1)
            ) %>%
            select(X1_mod) %>%
            pull
}
for(c in 1:27){
    # Load this 1-27 scenario into its 55-81 equivalent
    sc <- scenarios[[c]]
    
    # If this scenario value isn't null, fill in information.
    if(!is.null(sc)){
        # Change up the RNGs
        if(isTRUE(mainSimCall)){
            ## get the set of three for this 55-81 
            rng3 <- rngs_sc55_81[c((3*c - 2):(3*c))]
            ## save the vector names, apply to set of three
            names(rng3) <- names(sc$rng)
            
            ## insert the new seeds
            sc$rng <- rng3
        }
        
        # Change up the x2 gen stmt to rnorm
        sc$x2FF <- expr(rnorm(n, sd = 0.25))
    }
    # Store in scenarios obj, advance to next
    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1
}
suppressWarnings(rm(c, rng3, rngs_sc55_81, sc))   # will still execute, even if rng3 et al. don't exist (only throws warning)

# (Scenarios 82-108 ====
## These are spiritually similar to Scs. 28-54, except that they match the
## first two REPORTED scenarios in the Keele redux paper (Scs. 5 and 6 in the
## Keele redux's ___scens file.)  Done to ensure those results replicate.
if(isTRUE(mainSimCall)){
    rngs_sc82_108 <- 
         readr::read_delim("../rngs_sc82-108.txt", 
             delim = "\t", escape_double = FALSE, 
             col_names = FALSE, comment = "//", trim_ws = TRUE,
             show_col_types = FALSE) %>%

            mutate(X1_mod = if_else(mod(X2,2)==0,
                                1*10^ceiling(log10(X1)) + X1,
                                X1)
            ) %>%
            select(X1_mod) %>%
            pull
}
for(c in 1:27){
    # Load this 1-27 scenario into its 82-108 equivalent
    sc <- scenarios[[c]]
    
    # If this scenario value isn't null, fill in information.
    if(!is.null(sc)){
        # Change up the RNGs
        if(isTRUE(mainSimCall)){
            ## get the set of three for this 82-108
            rng3 <- rngs_sc82_108[c((3*c - 2):(3*c))]
            ## save the vector names, apply to set of three
            names(rng3) <- names(sc$rng)
            
            ## insert the new seeds
            sc$rng <- rng3
        }
        
        # Need to change x1's distro + its main effect
        sc$x1FF <- expr(sample(22:90, size=n, replace=TRUE))
        sc$betaList[["x1"]] <- ifelse(sc$betaList[["x1"]]!=0, 0.001, 0) # if there's a value, replace it.  Otherwise, leave it 
        
        # Depending on the scenario, will need to moderate other x1-related effects
        ## squared
        if("x1_2" %in% names(sc$betaList)){
            sc$betaList[["x1_2"]] <- 0.001    
        
        ## interactive
        } else if("x1_x2" %in% names(sc$betaList)){
            sc$betaList[["x1_x2"]] <- 0.004    
        }
    }
    
    # Store in scenarios obj, advance to next
    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1
}

# Try to tidy up, to help with memory    
suppressWarnings(rm(c, rng3, rngs_sc82_108, sc))   # will still execute, even if rng3 et al. don't exist (only throws warning)

# (Scenarios 109-135 ====
## Identical to 1-27 EXCEPT x1's dispersion is larger, to see if it's x1's
## magnitude or its variance that affects its performance.
if(isTRUE(mainSimCall)){
    rngs_sc109_135 <- 
         readr::read_delim("../rngs_sc109-135.txt", 
             delim = "\t", escape_double = FALSE, 
             col_names = FALSE, comment = "//", trim_ws = TRUE,
             show_col_types = FALSE) %>%

            mutate(X1_mod = if_else(mod(X2,2)==0,
                                1*10^ceiling(log10(X1)) + X1,
                                X1)
            ) %>%
            select(X1_mod) %>%
            pull
}
for(c in 1:27){
    # Load this 1-27 scenario into its 109-135 equivalent
    sc <- scenarios[[c]]
    
    # If this scenario value isn't null, fill in information.
    if(!is.null(sc)){
        # Change up the RNGs
        if(isTRUE(mainSimCall)){
            ## get the set of three for this 109-135
            rng3 <- rngs_sc109_135[c((3*c - 2):(3*c))]
            ## save the vector names, apply to set of three
            names(rng3) <- names(sc$rng)
            
            ## insert the new seeds
            sc$rng <- rng3
        }
        
        # Change up x1's dispersion
        sc$x1FF <- expr(rnorm(n, sd = 5))
    }
    
    # Store in scenarios obj, advance to next
    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1
}
suppressWarnings(rm(c, rng3, rngs_sc109_135, sc))

# (Scenarios 136-162 ====
## Absolutely identical to 109-135 (which are identical to 1-27 except for x1's
## dispersion).  Only diff is that the dispersion for this set is 3, whereas it
## was 5 in the previous set.
if(isTRUE(mainSimCall)){
    rngs_sc136_162 <- 
         readr::read_delim("../rngs_sc136-162.txt", 
             delim = "\t", escape_double = FALSE, 
             col_names = FALSE, comment = "//", trim_ws = TRUE,
             show_col_types = FALSE) %>%
            
            ## if the second column's even, append a 1 to the front of first column
            ## (random.org's max number is below the highest theoretical seed number
            ## in Stata, so this is what I do there to get more of the potential
            ## seeds 'in play' when I pull RNGs.  Same seems to hold for R re:
            ## highest theoretical seed, so I do it here, too).
            mutate(X1_mod = if_else(mod(X2,2)==0,
                                1*10^ceiling(log10(X1)) + X1,
                                X1)
            ) %>%
            select(X1_mod) %>%
            pull
}
for(c in 1:27){
    # Load this 1-27 scenario into its 136-162 equivalent
    sc <- scenarios[[c]]
    
    # If this scenario value isn't null, fill in information.
    if(!is.null(sc)){
        # Change up the RNGs
        if(isTRUE(mainSimCall)){
            ## get the set of three for this 136-162 
            rng3 <- rngs_sc136_162[c((3*c - 2):(3*c))]
            ## save the vector names, apply to set of three
            names(rng3) <- names(sc$rng)
            
            ## insert the new seeds
            sc$rng <- rng3
        }
        
        # Change up x1's dispersion
        sc$x1FF <- expr(rnorm(n, sd = 3))
    }
    
    # Store in scenarios obj, advance to next
    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1
}
suppressWarnings(rm(c, rng3, rngs_sc136_162, sc))

# (Scenarios 163-189 ====
## Absolutely identical to 136-162, which is the scenario where x1's dispersion
## is 3.  Difference: you upped x1's mean to 60 (which also means dropping
## the coefficients' sizes.)
if(isTRUE(mainSimCall)){
    rngs_sc163_189 <- 
         readr::read_delim("../rngs_sc163-189.txt", 
             delim = "\t", escape_double = FALSE, 
             col_names = FALSE, comment = "//", trim_ws = TRUE,
             show_col_types = FALSE) %>%
            
            ## if the second column's even, append a 1 to the front of first column
            ## (random.org's max number is below the highest theoretical seed number
            ## in Stata, so this is what I do there to get more of the potential
            ## seeds 'in play' when I pull RNGs.  Same seems to hold for R re:
            ## highest theoretical seed, so I do it here, too).
            mutate(X1_mod = if_else(mod(X2,2)==0,
                                1*10^ceiling(log10(X1)) + X1,
                                X1)
            ) %>%
            select(X1_mod) %>%
            pull
}
for(c in 1:27){
    # Load this 1-27 scenario into its 163-189 equivalent
    sc <- scenarios[[c]]
    
    # If this scenario value isn't null, fill in information.
    if(!is.null(sc)){
        # Change up the RNGs
        if(isTRUE(mainSimCall)){
            ## get the set of three for this 163-189
            rng3 <- rngs_sc163_189[c((3*c - 2):(3*c))]
            ## save the vector names, apply to set of three
            names(rng3) <- names(sc$rng)
            
            ## insert the new seeds
            sc$rng <- rng3
        }
        
        # Change up x1's dispersion AND its mean
        sc$x1FF <- expr(rnorm(n, mean=60, sd = 3))
        
        # Shrink the coefficient sizes
        sc$betaList <- modify_at(sc$betaList, vars(contains("x1")), ~.x/100)
        sc$tdeList  <- modify_at(sc$tdeList , vars(contains("x1")), ~.x/100)
    }
    
    # Store in scenarios obj, advance to next
    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1
}
suppressWarnings(rm(c, rng3, rngs_sc163_189, sc))

# (Scenarios 190-216 ====
## Identical to 163-189, except that x1's SD is 1.
if(isTRUE(mainSimCall)){
    rngs_sc190_216 <- 
         readr::read_delim("../rngs_sc190-216.txt", 
             delim = "\t", escape_double = FALSE, 
             col_names = FALSE, comment = "//", trim_ws = TRUE,
             show_col_types = FALSE) %>%
            
            ## if the second column's even, append a 1 to the front of first column
            ## (random.org's max number is below the highest theoretical seed number
            ## in Stata, so this is what I do there to get more of the potential
            ## seeds 'in play' when I pull RNGs.  Same seems to hold for R re:
            ## highest theoretical seed, so I do it here, too).
            mutate(X1_mod = if_else(mod(X2,2)==0,
                                1*10^ceiling(log10(X1)) + X1,
                                X1)
            ) %>%
            select(X1_mod) %>%
            pull
}
for(c in 1:27){
    # Load this 163-189 scenario into its 190-216 equivalent 
    ## (WILL NEED TO ADD 162 to c (can't change c's range easily b/c then
    ## screws with the rng3 subsetting)) 
    sc <- scenarios[[c+162]]
    
    # If this scenario value isn't null, fill in information.
    if(!is.null(sc)){
        # Change up the RNGs
        if(isTRUE(mainSimCall)){
            ## get the set of three for this 190-216 
            rng3 <- rngs_sc190_216[c((3*c - 2):(3*c))]
            ## save the vector names, apply to set of three
            names(rng3) <- names(sc$rng)
            
            ## insert the new seeds
            sc$rng <- rng3
        }
        
        # Change up x1's dispersion AND its mean
        sc$x1FF <- expr(rnorm(n, mean = 60, sd = 1))
        
        ## (Coefficient sizes already shrunk -> loaded scs. 163-189 as starters
        ##  for scs. 190-216, and coeffs are already shrunk in 163-189)
    }
    
    # Store in scenarios obj, advance to next
    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1
}
suppressWarnings(rm(c, rng3, rngs_sc190_216, sc))

# (Scenarios 217-243 ====
## Identical to Sc. 1-27, except that you use the same shrunk x1 coefficients as
## Scs. 163-216, to ensure you have a clean comparison.
if(isTRUE(mainSimCall)){
    rngs_sc217_243 <- 
         readr::read_delim("../rngs_sc217-243.txt", 
             delim = "\t", escape_double = FALSE, 
             col_names = FALSE, comment = "//", trim_ws = TRUE,
             show_col_types = FALSE) %>%
            
            ## if the second column's even, append a 1 to the front of first column
            ## (random.org's max number is below the highest theoretical seed number
            ## in Stata, so this is what I do there to get more of the potential
            ## seeds 'in play' when I pull RNGs.  Same seems to hold for R re:
            ## highest theoretical seed, so I do it here, too).
            mutate(X1_mod = if_else(mod(X2,2)==0,
                                1*10^ceiling(log10(X1)) + X1,
                                X1)
            ) %>%
            select(X1_mod) %>%
            pull
}
for(c in 1:27){
    # Load this 1-27 scenario into its 217-243 equivalent
    sc <- scenarios[[c]]
    
    # If this scenario value isn't null, fill in information.
    if(!is.null(sc)){
        # Change up the RNGs
        if(isTRUE(mainSimCall)){
            ## get the set of three for this 217-243
            rng3 <- rngs_sc217_243[c((3*c - 2):(3*c))]
            ## save the vector names, apply to set of three
            names(rng3) <- names(sc$rng)
            
            ## insert the new seeds
            sc$rng <- rng3
        }
    
        # Shrink the coefficient sizes
        sc$betaList <- modify_at(sc$betaList, vars(contains("x1")), ~.x/100)
        sc$tdeList  <- modify_at(sc$tdeList , vars(contains("x1")), ~.x/100)
    }
    
    # Store in scenarios obj, advance to next
    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1
}
suppressWarnings(rm(c, rng3, rngs_sc217_243, sc))

# (Scenarios 244-270 ====
## Identical to Sc. 217-243 (which is same as Scs. 1-27 except w/shrunk x1 coeffs),
## but x1's dispersion is 3
if(isTRUE(mainSimCall)){
    rngs_sc244_270 <- 
         readr::read_delim("../rngs_sc244-270.txt", 
             delim = "\t", escape_double = FALSE, 
             col_names = FALSE, comment = "//", trim_ws = TRUE,
             show_col_types = FALSE) %>%
            
            ## if the second column's even, append a 1 to the front of first column
            ## (random.org's max number is below the highest theoretical seed number
            ## in Stata, so this is what I do there to get more of the potential
            ## seeds 'in play' when I pull RNGs.  Same seems to hold for R re:
            ## highest theoretical seed, so I do it here, too).
            mutate(X1_mod = if_else(mod(X2,2)==0,
                                1*10^ceiling(log10(X1)) + X1,
                                X1)
            ) %>%
            select(X1_mod) %>%
            pull
}
for(c in 1:27){
    # Load this 217-243 scenario into its 244-270 equivalent
    ## (WILL NEED TO ADD 216 to c (can't change c's range easily b/c then
    ## screws with the rng3 subsetting)) 
    sc <- scenarios[[c+216]]
    
    # If this scenario value isn't null, fill in information.
    if(!is.null(sc)){
        # Change up the RNGs
        if(isTRUE(mainSimCall)){
            ## get the set of three for this 217-243
            rng3 <- rngs_sc244_270[c((3*c - 2):(3*c))]
            ## save the vector names, apply to set of three
            names(rng3) <- names(sc$rng)
            
            ## insert the new seeds
            sc$rng <- rng3
        }
    
        # Change up x1's dispersion
        sc$x1FF <- expr(rnorm(n, sd = 3))
    }
    
    # Store in scenarios obj, advance to next
    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1
}
suppressWarnings(rm(c, rng3, rngs_sc244_270, sc))

# (Scenarios 271-297 ====
## Identical to Sc. 28-54 (w/x1~runif), except that you use the same shrunk x1
## coefficients as Scs. 163-216, to ensure you have a clean comparison.
##
## REMEMBER: runif will only run with Corr==0, based on what's currently commented
##           in/out.  (sighs)
if(isTRUE(mainSimCall)){
    rngs_sc271_297 <- 
         readr::read_delim("../rngs_sc271-297.txt", 
             delim = "\t", escape_double = FALSE, 
             col_names = FALSE, comment = "//", trim_ws = TRUE,
             show_col_types = FALSE) %>%
            
            ## if the second column's even, append a 1 to the front of first column
            ## (random.org's max number is below the highest theoretical seed number
            ## in Stata, so this is what I do there to get more of the potential
            ## seeds 'in play' when I pull RNGs.  Same seems to hold for R re:
            ## highest theoretical seed, so I do it here, too).
            mutate(X1_mod = if_else(mod(X2,2)==0,
                                1*10^ceiling(log10(X1)) + X1,
                                X1)
            ) %>%
            select(X1_mod) %>%
            pull
}
for(c in 1:27){
    # Load this 28-54 scenario into its 271-297 equivalent
    ## (WILL NEED TO ADD 27 to c (can't change c's range easily b/c then
    ## screws with the rng3 subsetting)) 
    sc <- scenarios[[c+27]]
    
    # If this scenario value isn't null, fill in information.
    if(!is.null(sc)){
        # Change up the RNGs
        if(isTRUE(mainSimCall)){
            ## get the set of three for this 271-297
            rng3 <- rngs_sc271_297[c((3*c - 2):(3*c))]
            ## save the vector names, apply to set of three
            names(rng3) <- names(sc$rng)
            
            ## insert the new seeds
            sc$rng <- rng3
        }
    
        # Shrink the coefficient sizes
        sc$betaList <- modify_at(sc$betaList, vars(contains("x1")), ~.x/100)
        sc$tdeList  <- modify_at(sc$tdeList , vars(contains("x1")), ~.x/100)
    }
    
    # Store in scenarios obj, advance to next
    scenarios[[scCnt]] <- sc
    scCnt <- scCnt + 1
}
suppressWarnings(rm(c, rng3, rngs_sc271_297, sc))

rm(mainSimCall)