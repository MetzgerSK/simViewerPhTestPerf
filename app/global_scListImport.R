# argument extractor
getArgList <- function(e){
    # INPUT: two possibilities
    #
    ## - expr(FUNCTION HERE)
    ## - "FUNCTION HERE"
    #
    # Passing the function, without being wrapped in double-quotes or expr(),
    # will NOT work.  Don't do it.

    # e has to be the **raw** call (not a str).  If it isn't the call, make
    # it the call.
    if(!is.language(e)) e <- str2lang(e)

    # Extract
    res <- match.fun(e[[1]]) %>% match.call(.,e) %>% as.list

    # Get the defaults
    defs <- formals(as.character(e[[1]]))

    # Combine res + any of the active defaults (by
    # tossing the extras from defs), return.
    c(res, defs[!(names(defs) %in% names(res))])
}

## Load up the scList object, actually
try(source("mainMCsims___scens.R", local=TRUE))

# ^ This will now load everything, b/c you added a conditional in the ___scens
#   file to check whether it's being called from the main sim file or outside
#   of it.

# Give each element a name based on its numeric position, so you can toss the nulls
names(scenarios) <- map_chr(1:length(scenarios), ~paste0(.x))
scenarios_cmpt <- compact(scenarios)    # tosses nulls

sc_bList <-
    # Bring in betas
    map_dfr(1:length(scenarios_cmpt),
            ~pluck(scenarios_cmpt, .x, "betaList")
    ) %>%
      # Bring in TDEs
      bind_cols(.,
        {map_dfr(
            1:length(scenarios_cmpt),
            ~ pluck(scenarios_cmpt, .x, "tdeList")
         ) %>%
          rename_with(.fn = function(c) paste0("tde_", c))
        }
      ) %>%
    cbind(., sc = names(scenarios_cmpt)) %>%
      # Bring in everything else you want (this is the nuclear option)
      left_join(.,
                lapply(scenarios_cmpt, function(x){
                    llply(x, function(y){
                        if(!is.character(y)) deparse(y) else y
                    })
                }) %>%
                 ldply(., data.frame) %>%
                 filter(rng != ")") %>%
                 select(x1FF, x2FF, datType, tFF) %>%  # modify this if you want more than these vars to stay
                 mutate(sc = names(scenarios_cmpt)),
                by="sc")
