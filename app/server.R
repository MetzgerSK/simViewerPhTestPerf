## >>>>>> SERVER CODE <<<<<< --------
server <- function(input, output, session) {

    # Value is int?
    is.wholenumber <-
        function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

    # -- getArgList() moved to global_scListImport.R --

    # Disable certain inputs ====
    disable("x1Distro")
    disable("x2Distro")
    disable("scenMega")

    # OBSVR: x1 SDs ====
    ## spit list of valid x1 SDs back to UI, given selected scenario
    observe({
        req(grepl(input$x1Distro, "norm"))

        choiceVec <- relvSc()

        cV1 <-
            choiceVec %>%
              pull(x1SD) %>%
              sort

        updateSelectInput(
            inputId = "x1SD",
            choices = cV1,
            selected = cV1[[1]]
        )

        cV2 <-
            choiceVec %>%
              pull(x1Mean) %>%
              sort

        updateSelectInput(
            inputId = "x1Mean",
            choices = cV2,
            selected = cV2[[1]]
        )
    })


    # OBSVR: get list of ns, given these selections ====
    ## A bit dangerous, because you'll be using the IMAGE NAMES to obtain this
    ## (b/c it's something you set in the main sim file, not ___scens).  Let's
    ## see how badly things explode, though.
    observe({
        req(length(fNames())!=0)  # In case there's no match for this particular combo

        tempN <- str_match(fNames(), "n=([0-9]*)")[,2]

        # Get current
        cur <- input$nObs

        # Update dropdown
        updateSelectInput(
            inputId = "nObs",
            choices = tempN,
            selected = ifelse(cur %in% tempN, cur, tempN[[1]])
        )
    })


    # HELPER: update b mag sc dropdown (uses KaTeX, so insta-displays) ====
    observe({
        req(length(relvSc())!=0)  # In case there's no match for this particular combo

        # Candidate list
        scList <- relvSc()

        # Subset, if ~N
        if(grepl("norm", input$x1Distro)){
            scList <-
                scList %>%
                  filter(x1SD==input$x1SD) %>%  # narrow to correct SD
                  filter(x1Mean==input$x1Mean)  # narrow to correct mean
        }

        # Add any appropriate terms, depending on scenario
        ## (If it's vanilla, nothing to do.)
        if(input$scenMega=="van") {
            dropdown_add <- "x_1"     # Just put x1 again to prevent errors
            variable_add <- expr(x1)  # see above
        ## If sq term, need to add x_1^2
        } else if(input$scenMega=="sqTerm"){
            dropdown_add <- "x^2_1"
            variable_add <- expr(x1_2)
        ## If interaction, need to add x1_x2
        } else {
            dropdown_add <- "x_1x_2"
            variable_add <- expr(x1_x2)
        }

        scList <- scList %>% distinct(x1,x2,tde_x2,!!variable_add)            # get unique val sets for params

        toggleState("mainDiv_bScSel", condition=nrow(scList)>1)

        if(nrow(scList)>1){

            # Get the x1, x2, x2_TDE values
            coefVals <-
                scList %>%
                select(x1,x2,tde_x2,!!variable_add)

            # Make the pretty labels
            covars <- c("x_1", "x_2", dropdown_add, "x_2\\ln(t)") %>% unique  # uniq to toss any duplcs, given how you coded the vanilla sc

            # Do up the option choices
            optCodes <-
                map(1:nrow(coefVals), function(big) # Iterate over every row in the coefVals dfr
                    map2_chr(coefVals[big,], covars,
                             ~ifelse(.x!="0",
                                     paste0(ifelse(as.double(.x)>0, " + ", " "),
                                            .x,.y),
                                     "")
                    ) %>%
                      paste0(collapse="") %>%
                      str_replace(., "^ *[+] *", "")    # nuke any extra sign at front
                )
            optNms <- map_chr(1:length(optCodes), ~paste0(.x))
            opts_bScSel <- setNames(optNms, optCodes)

        # If no matches, set as "none"
        } else{
            opts_bScSel <- setNames("1", list("\\\\\\text{Only one possibility exists}"))
        }

        # Update.
        updateSelectizeInput(
            session, "main_bScSel",
            choices = opts_bScSel,
                selected = opts_bScSel %>% .[length(.)], # choose last elem--will give the small coeffs for those with both, which are likely what you'll end up reporting in the paper.
            options = list(render = I("
                {
                    item:   function(item, escape) {
                      var html = katex.renderToString(item.label);
                      return '<div>' + html + '</div>';
                    },
                    option: function(item, escape) {
                      var html = katex.renderToString(item.label);
                      return '<div>' + html + '</div>';
                    }
                }")
            )
        )
    })


    # HELPER: relvSc() - get relevant scenarios ====
    relvSc <- reactive({

        temp <-
            sc_bList %>%
              filter(sc %in% scEquivList[[input$scenMega]]) %>%
              filter(grepl(input$x1Distro, x1FF)) %>%  # to deal w/1-27 vs. 28-54
              filter(grepl(input$x2Distro, x2FF)) %>%  # to deal w/1-27 vs. 55-81
              filter(
                  case_when(
                    input$mainTDE==1 ~ x2==0,
                    input$mainTDE==2 ~ x2!=0 & sign(x2)==sign(tde_x2),
                    input$mainTDE==3 ~ x2!=0 & sign(x2)!=sign(tde_x2)
                )
              )

        # extract mean + SD
        ## If the rows exist, at this point, they'll have a value for mean+SD
        ## Because of how map_df's working, have to do like this, or else you'll
        ## get a sd and mean variable that contain a nested dataframe.
        if(grepl("norm", input$x1Distro)){
            temp <-
                bind_cols(temp,
                      map(temp$x1FF, getArgList) %>% map_df(., ~.x["sd"]),
                      map(temp$x1FF, getArgList) %>% map_df(., ~.x["mean"])
            ) %>%
              rename(x1SD = sd, x1Mean = mean)
        }

        temp
    })

    # D HELPER: narrow to correct set
    narrowScSet <- function(obj){
        # Narrow down to relevant set of three graphs
        res <- obj

        if(grepl("norm", input$x1Distro)){
            res <- res %>%
              filter(x1SD==input$x1SD) %>%  # narrow to correct SD
              filter(x1Mean==input$x1Mean)  # narrow to correct mean
        }

        # If length(scSub)>1, pull option from b mag dropdown
        ## (or, really: if there's more than one set of three, THEN enter the if())
        if(nrow(res)/3 > 1){
            req(input$main_bScSel)
            c <- as.numeric(input$main_bScSel)
            # if (1), will need first full row
            res <- res[c((3*c - 2):(3*c)),]  ## ASSUMES SORT ORDER IS SAME in scSub as in the main_bScSel observer (which it should be)
        }

        # Narrow to sc vector
        res <- pull(res,sc)

        # Return res
        res
    }

    # HELPER: Get relevant img file names, given selections ====
    fNames <- reactive({
        # Narrow down to relevant set of three graphs (accts for SD + mean + any bmag dropdown)
        scSub <- narrowScSet(relvSc())

        # Get relevant scenario range
        open <- paste0("_mega - sc ", scSub[1], "-", scSub[3])

        # Get relv regex (NOT the all graph)
        regEx <- paste0(open, ", n=([0-9]*), dt=", input$datType)

        # Pull imgs from file with this syntax
        fileNm <- imgList %>%
                    .[grep(regEx, ., perl=TRUE)] %>%
                    .[grep("all", ., perl=TRUE, invert=TRUE)] %>% # toss the all correlation graph

                    # (Put the file names going from smallest to largest n)
                    str_sort(numeric=TRUE)

        # Show the info message if there are no files meeting the specified criteria
        ## CONDITION: will show/enable if condition evals to true.
        map(c("m","ec","pv"), 
            function(x){
                toggleElement(paste0("noImgDiv", "_",x), condition=length(fileNm)==0)
                toggleElement(paste0("imgDiv"  , "_",x), condition=length(fileNm)!=0)
            }
        )
        toggleElement("imgDiv", condition=length(fileNm)!=0)
        toggleState("main_bScSel", condition=length(fileNm)!=0)

        # Return
        return(fileNm)
    })

    # HELPER: EC - Get relevant img file names, given selections ====
    # (function's nearly identical to fNames()--just didn't want to have to
    #  deal with generalizing that one so that it worked with both)
    fNames.ec <- reactive({
        # Narrow down to relevant set of three graphs (accts for SD + mean + any bmag dropdown)
        scSub <- narrowScSet(relvSc())

        # Get relevant scenario range
        open <- paste0("empCorr - sc ", scSub[1], "-", scSub[3])

        # Get relv regex
        regEx <- paste0(open, ", n=", input$nObs, "\\.")

        # Pull imgs from file with this syntax
        fileNm <- imgList.ec %>%
                    .[grep(regEx, ., perl=TRUE)]

        # Return
        return(fileNm)
    })

    # HELPER: PV - Get relevant img file names, given selections ====
    # (function's nearly identical to fNames()--just didn't want to have to
    #  deal with generalizing that one so that it worked with both)
    fNames.pv <- reactive({
        # Narrow down to relevant set of three graphs (accts for SD + mean + any bmag dropdown)
        scSub <- narrowScSet(relvSc())
        
        # Get relevant scenario range
        open <- paste0("scatter - sc ", scSub[as.numeric(input$sh_pv)])
        
        # Get relv regex
        regEx <- paste0(open, ", n=", input$nObs, ", dT=", input$datType, "\\.")
        
        # Pull imgs from file with this syntax
        fileNm <- imgList.pv %>% 
                    .[grep(regEx, ., perl=TRUE)] 

        # Return
        return(fileNm)
    })
    
    
    # HELPER: Image crop/prep ====
    imgFin <- reactive({
        req(length(fNames())!=0)  # In case there's no match for this particular combo

        # Subset to appropriate N
        subset <- fNames() %>%
                    .[grep(paste0("n=", input$nObs), ., perl=TRUE)]

        # Crop footer off of neg gph
        crpAmt_b <- 160
        negGph <- image_read(subset[grep("neg", subset)]) %>%
                    image_crop(paste0(image_info(.)$width, "x", image_info(.)$height-crpAmt_b))
        image_write(negGph, "negCrp.png")

        # Crop title + Corr=0 off of pos gph (+ the overall y-axis title, b/c corr(x1,x2) is already
        # in the neg gph)
        crpAmt <- 510
        crpAmt_l <- 80  # to get rid of overarching Corr() title on left

        posGph <- image_read(subset[grep("pos", subset)])
            # Slice off bottom, to place as sep image later (b/c your slicing
            # and dicing for the double y-axis issue makes the footer unreadable)
            crpAmt_bXTtl <- crpAmt_b - 60
            footer <- posGph %>%
                        image_crop(paste0(image_info(.)$width-crpAmt_l, "x", crpAmt_bXTtl,
                                          "+0+",image_info(.)$height-crpAmt_bXTtl))
            image_write(footer, "footer.png")

        # Resume the manipulating for main gph
        posGph <- posGph %>%
                    image_crop(paste0(image_info(.)$width-crpAmt_l, "x",
                                      image_info(.)$height-crpAmt-crpAmt_bXTtl,
                                      "+",crpAmt_l,"+", crpAmt) )

        # Get discrep btwn super-cropped posGph and negGph, add back as border, then recrop right
        wDiff <- image_info(negGph)$width -image_info(posGph)$width

        # Add back padding on left, based on discrep btwn width of neg and this gph
        posGph <- image_border(posGph, "#FFF", wDiff) %>%
                    # Crop off the extra added border on right, making the two images' dims match
                    image_crop(paste0(image_info(.)$width-wDiff, "x", image_info(.)$height))
        image_write(posGph, "posCrp.png")

        return(c("negCrp.png","posCrp.png","footer.png"))
    })


    # HELPER: form up the renderImage calls ====
    observe({
        lapply(seq_along(imgFin()), function(i) {
            output[[paste0("images", i)]] <-
              renderImage({
                  return(list(
                    src = imgFin()[i],
                    width="950"
                  ))
                }, deleteFile = FALSE
              )
        })
    })


    # OUTPUT: create the image grid ====
    output$imgs <- renderUI({
        # Render everything
        lapply(seq_along(imgFin()), function(i) {
            imageOutput(paste0("images", i), height="auto")
        })
    })


    # OUTPUT: the legend ====
    output$imgs_leg <- {
        perc_leg <- 0.55
        renderImage(
            list(src=fName_leg,
                 width =image_info(legend)$width*perc_leg,
                 height=image_info(legend)$height*perc_leg),
            deleteFile=FALSE
        )
    }

    # OUTPUT: emp corr plot ====
    output$mC_empCorrGphs <- {
        perc_ec <- 0.3075 # % by which to shrunk img's size
        renderImage(
            list(src=fNames.ec(),
                 width =image_info(image_read(fNames.ec()))$width*perc_ec,
                 height=image_info(image_read(fNames.ec()))$height*perc_ec),
            deleteFile=FALSE
        )
            # ^ Will throw error to console if no such image exists, but doesn't
            #   affect app's output.  (Already dealt with the "no graph" case for
            #   both the main and EC/PV graphs under fNames().)
    }


    # OUTPUT: p-value distr plot ====
    output$mC_pValDistroGphs <- {
        perc_pv <- 0.3075 # % by which to shrunk img's size
        renderImage(
            list(src=fNames.pv(),
                 width =image_info(image_read(fNames.pv()))$width*perc_pv, 
                 height=image_info(image_read(fNames.pv()))$height*perc_pv),
            deleteFile=FALSE
        )
            # ^ Will throw error to console if no such image exists, but doesn't
            #   affect app's output.  (Already dealt with the "no graph" case for
            #   both the main and EC/PV graphs under fNames().)
    }
}
