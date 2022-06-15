# Insert a noImgDiv
noImgDiv <-
    'p("Simulations not run for the combination of selected parameters in the left sidebar.")'

## >>>>>> UI CODE <<<<<< --------
ui <- fluidPage(
    useShinyjs(),

    tags$head(
      # For KaTeX functionality (dropdown for diff b mag scs)
      tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css",
                integrity="sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH", crossorigin="anonymous"),
      tags$script(src="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js",
                  integrity="sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm", crossorigin="anonymous"),

      # CSS
      tags$style(HTML("
        /* Much nicer way of coding the div-in-div trick */
        .inline-block-center {
          text-align: center;
        }
        .inline-block-center div {
          display: inline-block;
          text-align: left;
        }

        /* Brute-force override of legend div's height */
        #imgs_leg{
            height:100% !important;
        }

        /* For div w/FYI in it abt. Scs. 28-54 */
        .fyi_sc28_54{
            margin-top: 1.5em;
            text-align: center;
            background-color: #a5a5a5;
            padding: 10px;
            border-radius: 5px;
            color: #000;
        }

        /* Caption/aside */
        .caption{
            font-size: 0.8em;
            font-style: italic;
            color: #666;
        }

        /* Modal title tweaks */
        .modal-header{
            background-color: #333;
            color: #FFF;
            text-align: center;
            font-weight: 700;
        }

        /* for noImgDivs */
        .noImgDiv{
            margin-top:2em;
            text-align:center;
        }

    /*    /* For the stubborn dropdown */
        #main_bScSel .selectize-input{
            width: 300px !important;  */
        }
    "))),

    # Enable
    withMathJax(),

    # Title
    titlePanel("PH Calculation Performance: Simulation Graphs"),
    sidebarLayout(

        # Left Sidebar ====
        sidebarPanel(
            # Which scenario
            selectInput(
                "scenMega",
                label = "True DGP?",
                choices = scMegaList,
                selected = scMegaList[[3]]
            ),

            # Main/TDE Properties
            selectInput(
                "mainTDE",
                label = "\\(x_2\\)'s Main Effect vs. TDE Size?",
                choices = scSubList,
                selected = scSubList[[1]]
            ),

            # Set the n (in case multiple)
            selectInput(
                "nObs",
                label="# of Observations?",
                choices = c(100,1000),
            ),

            # datType
            radioGroupButtons(
                inputId = "datType",
                label = "Duration Recorded As?",
                choices = c("Continuous"="cont", "Coerced SS"="ss"),
                selected = "cont",
                checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                justified = TRUE,
                status = "primary"
            ),

            # How is x1 distributed? (to tap into 28-54)
            radioGroupButtons(
                inputId = "x1Distro",
                label = "\\(x_1\\) Distributed As?",
                choices = c("Normal"="norm", "Uniform"="unif"),
                selected = "norm",
                checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                justified = TRUE
            ),

            # If x1 is normal, get list of different SDs (for the time being--may need to add another slider for the mean later)
            conditionalPanel(
                condition = "input.x1Distro == 'norm'",

                selectInput(
                    inputId = "x1Mean",
                    label = "\\(x_1\\)'s Mean?",
                    choices = c(0),
                    selected = 1
                ),

                selectInput(
                    inputId = "x1SD",
                    label = "\\(x_1\\)'s SD?",
                    choices = c(1),
                    selected = 1
                )
            ),

            # How is x2 distributed? (to tap into 55-81)
            radioGroupButtons(
                inputId = "x2Distro",
                label = "\\(x_2\\) Distributed As?",
                choices = c("Normal"="norm", "Binomial"="binom"),
                selected = "binom",
                checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                justified = TRUE
            ),

            HTML("<hr style='border-top:1px solid #bbb;'>"),
            h4("Graph Legend"),
            div(class="inline-block-center",
                div(imageOutput("imgs_leg"))
            )
        ),

        # Main Panel (graph) ====
        mainPanel(
            tabsetPanel(
                ## Main Graphs ####
                tabPanel("Main",
                    # If it's a scenario where we have both small and big betas,
                    # give selector box. (the show/hide is implemented in server.R)
                    #
                    # (The choices get populated in server.R, too.)
                    div(id="mainDiv_bScSel",
                        div(
                            selectInput(
                                "main_bScSel",
                                label=HTML("Select Linear Combination <br/>
                                           <span style='font-size:0.75em;font-weight:400;font-style:italic;'>(If applicable)</span>"),
                                choices=NULL,
                                width="300px"
                            )
                        )
                    ),

                    # Graphs
                    div(id="imgDiv_m", style="position:relative;",
                        uiOutput("imgs"),

                        # FYI div about Scs. 28-54, to explain sudden dearth of displayed gphs
                        conditionalPanel(
                            condition = "input.x1Distro == 'unif'",

                            div(
                                class = "fyi_sc28_54",
                                span(
                                    style = "font-size:1.25em",
                                    fontawesome::fa("exclamation-triangle", margin_right = 5)
                                ),
                                HTML(
                                    "Scenarios in which \\( x_1 \\sim \\mathcal{U}[0,1]\\)
                                    only have simulations for \\(\\text{Corr}(x_1,x_2)=0\\)
                                    and \\(n=100\\)."
                                )
                            )
                        )
                    ),

                    # No gphs for this set of sim chars
                    div(id="noImgDiv_m", class="noImgDiv",
                        eval(parse(text=noImgDiv))
                    )
                ),

                ## Empirical Correlations ####
                tabPanel("Empirical Correlations", style="margin-top:10px;",
                    # No gphs for this set of sim chars
                    div(id="noImgDiv_ec", class="noImgDiv",
                        eval(parse(text=noImgDiv))
                    ),

                    # Yes gphs for this set of sim chars
                    div(id="imgDiv_ec",
                        # Note about datType/RC type/%
                        HTML("<p><em>Note</em>: the way in which the duration is recorded, the RC type, and the RC percentage are irrelevant
                             for examining the covariates' empirical correlations.  For a given draw, the covariates are generated first, then all
                             the other operations happen subsequently using that same draw of covariates.</p>"),

                        # The graph
                        div(class="inline-block-center",
                            div(imageOutput("mC_empCorrGphs") )
                        )
                    ),
                ),

                ## p-val distribs ####
                tabPanel(HTML("<em>p</em>-values: Distributions"), style="margin-top:10px;",
                    # No gphs for this set of sim chars
                    div(id="noImgDiv_pv", class="noImgDiv",
                        eval(parse(text=noImgDiv))
                    ),

                    # Yes gphs for this set of sim chars
                    div(id="imgDiv_pv",
                        # The graph
                        div(class="inline-block-center",
                            # shape selector
                            radioGroupButtons(
                                inputId = "sh_pv",
                                label = "Select Column",
                                choices = c("\\(p = 0.75\\)"=2,
                                            "\\(p = 1\\)"=1,
                                            "\\(p = 1.25\\)"=3),
                                selected = 2,
                                checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                justified = TRUE,
                                status = "primary"
                            ),
                            div(imageOutput("mC_pValDistroGphs") )
                        )
                    )
                )
            )
        )
    )
)