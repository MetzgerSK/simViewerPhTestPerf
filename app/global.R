library(shiny)
library(shinyWidgets)
library(shinyjs)
library(magrittr)
library(magick)
library(rlang)
library(plyr)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(ggtext)

# Called in line: fontawesome (but that auto-installs w/shiny)
# ((readr's referenced in ___scens, but that chunk
# of the code never runs when called from the Shiny app))
#***********************************************************
# Dir where graphs are located, relative to app folder +
# getting list of imgs
dir <- "../graphs/"
imgList <-  list.files(dir, pattern=".*",
                       full.names=TRUE) %>%
                .[grep("png$",.)]             # PNGs only
imgList.ec <- list.files("../graphs_ec",
                         pattern=".*",
                       full.names=TRUE) %>%
                .[grep("png$",.)]
imgList.pv <- list.files("../graphs_pv/", 
                         pattern=".*", 
                       full.names=TRUE) %>%   
                .[grep("png$",.)]   

# Crop the img with the legend and save, for reference
## (just pull the first sc-specific png that comes up in the list)
legend <- image_read(imgList %>% .[grep("/sc", ., perl=TRUE)] %>% .[1]) %>%
            image_crop("281x193+1793+195")
              ## Ref for dimension syntax: http://www.imagemagick.org/Magick++/Geometry.html
                # - the final image will be 281 by 192
                # - BEFORE you make that cut, move over to the right by 1793 px
                #   (then cut from 1793 to 1793+281)
                # - BEFORE you cut, move down by 195 px (then cut 195+193)

fName_leg <- "_legend.png"
image_write(legend, path = fName_leg, format = "png")

# Mega-scenarios list
scMegaList <- list(
                "x1 Squared Term" = "sqTerm",
                "x1_x2 Interaction"  = "int",
                "Vanilla (x1 + x2 only)" = "van"
              )

# Scenario subsets
scSubList <- list(
               "TDE Only" = 1,
               "Main Effect = 0.5 of TDE" = 2,
               "Main Effect = -0.5 of TDE" = 3
             )

# % of RC
rcPercList <- c("None" = "p0",
                "25%"  = "p25",
                "50%"  = "p50" )

# List of scenario equivalences
source("global_scListImport.R", local=TRUE)
    ## moved into a separate R file so that Stata can call this separate
    ## file, when it goes to get (e.g.) x1's SD.

# With redaction for PA, add NAs for x1_2, x1_x2
sc_bList %<>% mutate(x1_2  = NA,
                     x1_x2 = NA)

## Form up list of vectors
scEquivList <-
    map(list(
            # "sqTerm" = expr(!is.na(x1_2)),
            # "int"    = expr(!is.na(x1_x2)),
            "van"    = expr(is.na(x1_2) & is.na(x1_x2))
        ),
        ~ filter(sc_bList, !!.x) %>% pull(sc))
