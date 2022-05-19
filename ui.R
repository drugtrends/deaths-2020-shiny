#For CODURF 2020 data received in Mar 2022
#N. Man
library(shiny)
# library(shinydashboard)
library(plotly)
library(shinycustomloader)
# library(shinyjs)

ui <- function(request) {
  bootstrapPage('',
    tags$head(
      # includeScript("google_analytics.js"),
# tried includeCSS and tag$style with !important to get slider to show up green but didn't work
      # includeCSS("DT-theme.css"), # css file in main directory
      # tags$style(type = 'text/css',
      #   '.irs-from,.irs-to,.irs-single,.irs-grid-pol{background:#965DA6 !important }',
      #   '.irs-bar{border-top:1px solid #965DA6;border-bottom:1px solid #965DA6;background:#965DA6 !important }'
      # ),
# https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
      tags$script(src="dimension.js"),
#https://stackoverflow.com/questions/30096187/favicon-in-shiny
#https://www.w3.org/2005/10/howto-favicon
      tags$link(rel="icon", type="image/png", href="favicon.png")
    ),
    theme="NIDIP-theme.css", # !important in here worked
# Navigation bar ---------------------------------------------------------------
mainPanel(width=9,
  navbarPage(
    title= "Deaths induced by:",
    id="Page",
  # All drugs menu tab ---------------------------------------------------------------
    navbarMenu("All drugs",

    # All drugs by jurisdiction, intent, age and sex (Tables 1a, 1b & 1c) -------------------
      tabPanel(value="AllPage",
        "Drug-induced deaths by jurisdiction, age and sex",
        h2("Drug-induced deaths by jurisdiction, intent, age and sex"),

        tabsetPanel(type="tabs",id="AllPage",
          tabPanel("Plot",
              withLoader(plotlyOutput("AllPlot", width="100%", height="600px"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              fluidRow(
                includeHTML("fnoteAll.html"),
                conditionalPanel(condition="input.Rshow",
                  HTML("<small>Current version refers to the Cause of Death data obtained for this report. Previous version refers to the Cause of Death <a href='https://drugtrends.shinyapps.io/deaths_2019/'>data</a> obtained for <a href='https://ndarc.med.unsw.edu.au/resource-analytics/trends-drug-induced-deaths-australia-1997-2019'>last year's publication</a> on Trends in drug-induced deaths 1997-2019.</small>")
                )
              )
          ),
          tabPanel("Notes", includeHTML("notesAll.html"))
        )
      ),

    # Remoteness by jurisdiction and intent (Tables R1, R4 & R5) ----------------------------
      tabPanel(value="RAPage",
        "Drug-induced deaths by jurisdiction and remoteness area",
        h2("Drug-induced deaths by jurisdiction, remoteness area and intent"),

        tabsetPanel(type="tabs",id="RAPage",
          tabPanel("Plot",
              withLoader(plotlyOutput("RAPlot", width="100%", height="600px"),
                         type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteRA.html"),
              conditionalPanel(condition="input.Rshow",
                HTML("<small>Current version refers to the Cause of Death data obtained for this report. Previous version refers to the Cause of Death <a href='https://drugtrends.shinyapps.io/deaths_2019/'>data</a> obtained for <a href='https://ndarc.med.unsw.edu.au/resource-analytics/trends-drug-induced-deaths-australia-1997-2019'>last year's publication</a> on Trends in drug-induced deaths 1997-2019.</small>")
              )
          ),
          tabPanel("Notes", includeHTML("notesRem.html"))
        )
      ),

    # Remoteness percents (Tables R) -----------------------------------------
      tabPanel(value="RPPage",
        "Percentages of drug-induced deaths by remoteness area",
        h2("Percentages of drug-induced deaths by remoteness area"),

        tabsetPanel(type="tabs",id="RPPage",
          tabPanel("Plot",
              withLoader(plotlyOutput("RPPlot", width="100%", height="600px"),
                         type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteRP.html")
          ),
          tabPanel("Notes", includeHTML("notesRem.html"))
        )
      ),

    # All drug-induced deaths by drug, jurisdiction, intent and/or sex (Table 12, 12b & 12c)-------------------------
      tabPanel(value="DTJPage",
        "Drug-induced deaths by drug, jurisdiction and sex",
        h2("Drug-induced deaths by drug, jurisdiction, intent and sex"),
      
        tabsetPanel(type="tabs",id="DTJPage",
          tabPanel("Plot",
              withLoader(plotlyOutput("DTJPlot", width="100%", height="600px"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteDTJ.html")
           ),
           tabPanel("Notes", includeHTML("notesDT.html"))
         )
      ),


    # All drug-induced deaths by drug, age and intent (Table 12 & 12a)-----------------------
      tabPanel(value="DTAPage",
        "Drug-induced deaths by drug and age",
        h2("Drug-induced deaths by drug, age and intent"),

        tabsetPanel(type="tabs",id="DTAPage",
          tabPanel("Plot",
              withLoader(plotlyOutput("DTAPlot", width="100%", height="600px"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteDTA.html")
           ),
          tabPanel("Notes", includeHTML("notesDT.html"))
        )
      ),
      
    # Cross-sectional profile of drug-induced deaths -------------------
      tabPanel(value="XPPage",
        "Cross-sectional profile of drug-induced deaths",
        h2("Cross-sectional profile of drug-induced deaths"),

        tabsetPanel(type="tabs",id="XPPage",
          tabPanel("Plot",
              # withLoader(
              conditionalPanel(condition="input.jur!='Tasmania' &
                  input.jur!='Australian Capital Territory' &
                  input.jur!='Northern Territory'",
                fluidRow(
                  column(2,
                    withLoader(plotlyOutput("SexPlot", width="100%", height="400px"),
                  type="image", loader="DT_NIDIP_tween.gif")
                  ),
                  column(7,
                      plotlyOutput("AgePlot", width="100%", height="400px")
                  ),
                  column(3,
                    plotlyOutput("IntPlot", width="100%", height="400px")
                  )
                ),
                fluidRow(
                  column(4,
                    # conditionalPanel(condition="input.jur!='Tasmania' &
                    #   input.jur!='Australian Capital Territory' &
                    #   input.jur!='Northern Territory'",
                      plotlyOutput("RemPlot", width="100%", height="500px")
                    # ),
                    # conditionalPanel(condition="input.jur=='Northern Territory' |
                    #   input.jur=='Australian Capital Territory' |
                    #   input.jur=='Tasmania'",
                    #   HTML("<br><br><br>Remoteness area data not available.")
                    # )
                  ),
                  column(8,
                # ),
                # fluidRow(
                    # conditionalPanel(condition="input.jur=='Australia'
                    #   | input.jur=='New South Wales' | input.jur=='Victoria'
                    #   | input.jur=='Queensland'",
                      plotlyOutput("DTPlot", width="100%", height="500px")
                    # )
                  )
                ),
                # type="image", loader="DT_NIDIP_tween.gif"),
                fluidRow(includeHTML("fnoteXP.html"))
              ),
              conditionalPanel(condition="input.jur=='Northern Territory' |
                  input.jur=='Tasmania' | input.jur=='Australian Capital Territory'",
                HTML("<br><br>Numbers are too small to show for the jurisdiction.")
              )
          ),
          tabPanel("Notes", includeHTML("notesAll.html"))
        )
      )
    ),


  # Opioids menu tab -------------------------------------------------------------
    navbarMenu("Opioids",

    # Opioids by opioid, age and intent (Table 4) -----------------------------------------
      tabPanel(value="O4Page",
        "Opioid-induced deaths by opioid and age",
        h2("Opioid-induced deaths by opioid, age and intent"),

        tabsetPanel(type="tabs",id="O4Page",
          tabPanel("Plot",
              withLoader(plotlyOutput("O4Plot", width="100%", height="600px"), 
                  type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteOp4.html")
          ),
          tabPanel("Notes", includeHTML("notesOp.html"))
        )
      ),

    # Opioids by intent, opioid and sex (Table 5) -----------------------------------------
      tabPanel(value="O5Page",
        "Opioid-induced deaths by opioid and sex",
        h2("Opioid-induced deaths by opioid, intent and sex"),

        tabsetPanel(type="tabs",id="O5Page",
          tabPanel("Plot",
              withLoader(plotlyOutput("O5Plot", width="100%", height="600px"),
                type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteOp5.html")
          ),
          tabPanel("Notes", includeHTML("notesOp.html"))
        )
      ),

    # Opioids by intent, jurisdiction and sex (Table 6) ---------------------------------
      tabPanel(value="O6Page",
        "Opioid-induced deaths by sex and jurisdiction",
        h2("Opioid-induced deaths by intent, jurisdiction and sex"),

        tabsetPanel(type="tabs",id="O6Page",
          tabPanel("Plot",
              withLoader(plotlyOutput("O6Plot", width="100%", height="600px"),
                type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteOp6.html")
          ),
          tabPanel("Notes", includeHTML("notesOp.html"))
        )
      ),
  
    # Exclusive opioids by age and intent (Table 10)-----------------------------------------
      tabPanel(value="E0Page",
        "Opioid-induced deaths by exclusive opioid type and age",
        h2("Opioid-induced deaths by exclusive opioid type, age and intent"),
        
        tabsetPanel(type="tabs",id="E0Page",
          tabPanel("Plot",
              withLoader(plotlyOutput("E0Plot", width="100%", height="600px"), 
                  type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteEOp.html")
          ),
          tabPanel("Notes", includeHTML("notesEOp.html"))
        )
      ),
      
    # Exclusive opioids by jurisdiction, intent and sex (Table 9 & 11)-----------------------
      tabPanel(value="E9Page",
        "Opioid-induced deaths by exclusive opioid type, jurisdiction and sex",
        h2("Opioid-induced deaths by exclusive opioid type, jurisdiction, intent and sex"),
        
        tabsetPanel(type="tabs",id="E9Page",
          tabPanel("Plot",
              withLoader(plotlyOutput("E9Plot", width="100%", height="600px"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteEOp.html")
          ),
          tabPanel("Notes", includeHTML("notesEOp.html"))
        )
      ),
      
    # # Exclusive opioids percents (Tables 10 & 11) -----------------------------------------
    #   tabPanel(value="EPPage",
    #     "Percentages of opioid-induced deaths by exclusive opioid types",
    #     h2("Opioid-induced deaths by exclusive opioid type as percentages of all opioid-induced deaths"),
    #     
    #     tabsetPanel(type="tabs",id="EPPage",
    #       tabPanel("Plot",
    #           withLoader(plotlyOutput("EPPlot", width="100%", height="600px"),
    #             type="image", loader="DT_NIDIP_tween.gif"),
    #           includeHTML("fnoteEPOp.html")
    #       ),
    #       tabPanel("Notes", includeHTML("notesEOp.html"))
    #     )
    #   ),
  
    # Opioids with other drugs by age and intent (Table 7)-----------------------------------
    tabPanel(value="W7Page",
      "Opioid-induced deaths by other drugs involved, and by age",
      h2("Opioid-induced deaths by other drugs involved, and by age and intent"),
  
      tabsetPanel(type="tabs",id="W7Page",
        tabPanel("Plot",
            withLoader(plotlyOutput("W7Plot", width="100%", height="600px"),
                type="image", loader="DT_NIDIP_tween.gif"),
            includeHTML("fnoteWOp.html")
        ),
        tabPanel("Notes", includeHTML("notesWOp.html"))
      )
    ),
  
    # Opioids with other drugs by sex and intent (Table 8) ----------------------------------
    tabPanel(value="W8Page",
      "Opioid-induced deaths by other drugs involved, and by sex",
      h2("Opioid-induced deaths by other drugs involved, and by sex and intent"),
  
        tabsetPanel(type="tabs",id="W8Page",
          tabPanel("Plot",
            # mainPanel(width=9,
              withLoader(plotlyOutput("W8Plot", width="100%", height="600px"),
                  type="image", loader="DT_NIDIP_tween.gif"),
              includeHTML("fnoteWOp.html")
          ),
          tabPanel("Notes", includeHTML("notesWOp.html"))
        )
      )
    ),

  # Explanatory Notes tab ---------------------------------------------------------------
    tabPanel(value="Method",
      "Explanatory notes", includeHTML("notesExplanatory.html")
    ),
  # Citation tab ------------------------------------------------------------
    tabPanel(value="Cites",
      "Citation & acknowledgements",includeHTML("Citation.html")
    )
  )
  ),
  conditionalPanel(condition="input.dimension>1199",
# https://stackoverflow.com/questions/14628601/can-i-add-background-color-only-for-padding - not for narrow screens
    HTML("<div class='col-sm-0' style='padding-top: 50px;
      background-image:
        linear-gradient(to bottom,
          rgba(150,93,166, 1) 0%,
          rgba(150,93,166, 1) 100%),
        linear-gradient(to bottom,
          rgba(150,93,166, 1) 0%,
          rgba(150,93,166, 1) 100%);
      background-clip: content-box, padding-box'>"),
    HTML("</div>"),
  ),
  conditionalPanel(condition="input.dimension>991 & input.dimension<1200",
    HTML("<div class='col-sm-0' style='padding-top: 80px;
      background-image:
        linear-gradient(to bottom,
          rgba(150,93,166, 1) 0%,
          rgba(150,93,166, 1) 100%),
        linear-gradient(to bottom,
          rgba(150,93,166, 1) 0%,
          rgba(150,93,166, 1) 100%);
      background-clip: content-box, padding-box'>"),
    HTML("</div>"),
  ),
  conditionalPanel(condition="input.dimension>767 & input.dimension<992",
    HTML("<div class='col-sm-0' style='padding-top: 120px;
      background-image:
        linear-gradient(to bottom,
          rgba(150,93,166, 1) 0%,
          rgba(150,93,166, 1) 100%),
        linear-gradient(to bottom,
          rgba(150,93,166, 1) 0%,
          rgba(150,93,166, 1) 100%);
      background-clip: content-box, padding-box'>"),
    HTML("</div>"),
  ),
  conditionalPanel(condition="input.Page!='Method' & input.Page!='Cites' & eval('input.'+input.Page)=='Plot'",
    sidebarPanel(id="Sidebar",width=3,
    # year slider###############
      conditionalPanel(
        condition="input.Page!='RAPage' & input.Page!='RPPage' &
        input.Page!='E9Page' & input.Page!='E0Page' & input.Page!='EPPage' & input.Page!='XPPage'",
        sliderInput("yr97", "Period",
          min=1997, max=2020,
          value=c(1997, 2020), sep=""
        )
      ),
      conditionalPanel(
        condition="input.Page=='E9Page' | input.Page=='E0Page' | input.Page=='EPPage'",
        sliderInput("yr07", "Period",
          min=2007, max=2020,
          value=c(2007, 2020), sep=""
        )
      ),
      conditionalPanel(
        condition="input.Page=='RAPage' | input.Page=='RPPage'",
        sliderInput("yr09", "Period",
          min=2009, max=2020,
          value=c(2009, 2020), sep=""
        )
      ),
      conditionalPanel(
        condition="input.Page=='XPPage'",
        selectInput("yrX", "Period",
          seq(2009, 2020,1),
          selected=2020
        )
      ),

  ####For user-defined year intervals
    # radioButtons("xax", "Interval between years:",
    #   choices=c(1, 2, 5), inline=T, selected=2
    # ),
###HTML info on width of plot for WIP: <rect class="nsewdrag drag" width="854" height="474" ;"></rect>

    # y-variable list###############
      conditionalPanel(
        condition="input.Page!='RPPage' & input.Page!='EPPage' & input.Page!='XPPage' ",
        selectInput("yax", "Plot:",
           c(
             "Number of deaths"="num",
             "Crude death rate"="cr",
             "Crude death rate (95% CI)"="crci",
             "Age standardised death rate"="sr",
             "Age standardised death rate (95% CI)"="srci"
           ),
           selected="sr"
        )
      ),
      conditionalPanel(condition="(input.yax=='sr' | input.yax=='srci')
          & input.Page!='RPPage' & input.Page!='EPPage' & input.Page!='XPPage'",
        HTML("<div style='color:red'><small>NB: Age standardised rates are only calculated for <i>All ages</i>. Please select <b>Plot</b> from the above for <b>Number of deaths</b> or <b>Crude death rate</b> for data by specific age groups.</small></div><br>")
      ),

    # Also show all drug-induced & crude rate checkbox (Am, C, W7 & W8 Pages | sr y-axis)###############
      conditionalPanel(condition="((input.yax=='num' | input.yax=='cr') &
        (input.Page=='AmPage' | input.Page=='CPage' |
        input.Page=='O4Page' | input.Page=='O5Page' | input.Page=='O6Page' | 
        input.Page=='W7Page' | input.Page=='W8Page' |
          ( input.Page=='AllPage' & input.ageAll.length==1 ) |
          ( input.Page=='RAPage' & (( input.jurR=='Australia' & 
          ((input.dropRA=='Age' & input.RAra.length==1) | (input.dropRA=='Region' & input.ageAll.length==1)) )
          | (input.jurR!='Australia' & input.Rra.length==1)) &
          ((input.dropSI=='Sex' & input.sex4R=='People') | (input.dropSI=='Intent' & input.sexC[0]=='People')) ) |
          ( input.Page=='DTJPage' & ((input.DTJdrop=='IntSx' & input.DTdrug.length==1) | (input.DTJdrop=='Drug' & input.sexC.length==1)) ) |
          ( input.Page=='DTAPage' & ((input.DTAdrop=='Age_Intent' & input.DTdrug.length==1) | (input.DTAdrop=='Drug' & input.ageAll.length==1)) ))) | input.yax=='sr'", # | input.yax=='srci'
        HTML("<b>Also show:</b>"),
        conditionalPanel(condition="( (input.yax=='num' | input.yax=='cr') &
          (input.Page=='AmPage' | input.Page=='CPage' |
          input.Page=='O4Page' | input.Page=='O5Page' | input.Page=='O6Page' | 
          input.Page=='W7Page' | input.Page=='W8Page') ) |
        ( input.yax=='sr' & ( input.Page=='AmPage' | input.Page=='CPage' |
          (input.Page=='O6Page' & (input.sexC.length==1 | input.cod2C.length==1)) |
          ((input.Page=='O4Page' | input.Page=='W7Page') &
            (input.dropOA=='Drug' | input.cod4C.length==1 | (input.cod4C.length==2 & input.codS==2))) |
          ( input.Page=='O5Page' & ( (input.O5drop!='Intent' & (input.cod4C.length==1 | (input.cod4C.length==2 & input.codS==2))) | 
            (input.O5drop!='Sex' & input.sexC.length==1) | (input.O5drop!='Opioid' & input.OdrugC.length==1) ) ) |
          (input.Page=='W8Page' & (input.Wdrug.length==1 |
            (input.dropSI=='Intent' & input.sexC.length==1) |
            ( input.dropSI=='Sex' & (input.cod4C.length==1 | (input.cod4C.length==2 & input.codS==2 & input.sex4R!='MF')) )
        )) ) )",
          checkboxInput("Ashow", "All drug-induced deaths",value=F)
        ),
        conditionalPanel(condition=
        "(input.yax=='num' | input.yax=='cr' | (input.yax=='sr' & input.crude==false)) &
        (( input.Page=='AllPage' & (input.ageAll.length==1 | input.yax=='sr')) |
        ( input.Page=='RAPage' & (( input.jurR=='Australia' &
          ((input.dropRA=='Age' & input.RAra.length==1) | (input.dropRA=='Region' & (input.ageAll.length==1 | input.yax=='sr'))) )
          | (input.jurR!='Australia' & input.Rra.length==1)) &
          ((input.dropSI=='Sex' & input.sex4R=='People') | (input.dropSI=='Intent' & input.sexC[0]=='People')) ))",
        # "(input.yax=='num' | input.yax=='cr') &
        # (( input.Page=='AllPage' & input.ageAll.length==1 ) |
        # ( input.Page=='RAPage' & (( input.jurR=='Australia' &
        #   ((input.dropRA=='Age' & input.RAra.length==1) | (input.dropRA=='Region' & input.ageAll.length==1)) )
        #   | (input.jurR!='Australia' & input.Rra.length==1)) &
        #   ((input.dropSI=='Sex' & input.sex4R=='People') | (input.dropSI=='Intent' & input.sexC[0]=='People')) ) |
        # ( input.Page=='DTJPage' & ((input.DTJdrop=='IntSx' & input.DTdrug.length==1) | (input.DTJdrop=='Drug' & input.sexC.length==1)) ) |
        # ( input.Page=='DTAPage' & ((input.DTAdrop=='Age_Intent' & input.DTdrug.length==1) | (input.DTAdrop=='Drug' & input.ageAll.length==1)) ))",
          checkboxInput("Rshow", "Show previous year's data release",value=T)
        ),
        conditionalPanel(condition="input.yax=='sr' & input.Page!='RPPage' & input.Page!='EPPage' & input.Page!='XPPage'", # | input.yax=='srci'
          checkboxInput("crude","Crude rate",value=F)
        )
      ),

    # jurisdiction selection###############
      conditionalPanel(
        condition="input.Page=='AllPage' | input.Page=='O6Page'
        | input.Page=='E9Page' | input.Page=='EPPage' | input.Page=='XPPage'",
        selectInput("jur", "Jurisdiction:",
          c("Australia",
            "Australian Capital Territory",
            "New South Wales",
            "Northern Territory",
            "Queensland",
            "South Australia",
            "Tasmania",
            "Victoria",
            "Western Australia")
          )
      ),
      conditionalPanel(
        condition="input.Page=='RAPage' | input.Page=='RPPage'",
        selectInput("jurR", "Jurisdiction:",
          c("Australia",
            "Australian Capital Territory",
            "New South Wales",
            "Northern Territory",
            "Queensland",
            "South Australia",
            "Tasmania",
            "Victoria",
            "Western Australia"),
          selected="Australia"
        )
      ),

    # radio button panelS###############
      conditionalPanel(condition="((
        (input.Page=='RAPage' & input.jurR!='Australia') |
        (input.Page=='DTJPage' & input.DTjur=='Australia') |
        input.Page=='O5Page' | input.Page=='O6Page' |
        input.Page=='E9Page' | input.Page=='W8Page')
        & input.yax!='sr' & input.yax!='srci') | input.Page=='RPPage' | input.Page=='EPPage'",
        #   radioButtons("ageR", "Age range:",inline=TRUE,
        #     choices=c(
        #       "All ages"#, "15 to 64"="15-64"
        #     ),
        #     selected=c("All ages")
        # ),
        conditionalPanel(
          condition="input.Page=='RPPage' | input.Page=='EPPage'",
          radioButtons("sexR", "Sex:",
            choices=c(
              "Male",
              "Female",
              "People"
            ),
            selected=c("People")
          )
        # ),
        # conditionalPanel(
        #   condition="input.Page=='RPPage' | input.Page=='EPPage'",
        #   radioButtons("codR", "Intent:",
        #     c("All", "Unintentional"),
        #     selected="All"
        #   )
        )
      ),

    # Drug type with jurisdiction panels (DTJPage)#######
      conditionalPanel(condition="input.Page=='DTJPage'",
        selectInput("DTjur", "Jurisdiction:",
          c("Australia",
            "Australian Capital Territory",
            "New South Wales",
            "Northern Territory",
            "Queensland",
            "South Australia",
            "Tasmania",
            "Victoria",
            "Western Australia")
        ),

        # conditionalPanel(condition="input.DTjur=='Australia'",
        #   selectInput("DTage", "Age range:",
        #    choices=c(
        #      "All ages","15 to 64"="15-64"
        #    ),
        #    selected=c("All ages")
        #   )
        # ),
        radioButtons("DTJdrop", "Variable for dropdown list:",
          choices=c(
              "Sex & Intent"="IntSx",
              "Drug"="Drug"
          ), #inline=T,
          selected=c("IntSx")
        ),

        conditionalPanel(condition="input.DTJdrop=='IntSx'",
          # conditionalPanel(condition="input.DTjur=='Australia'",
            selectInput("DTIsex", "Sex:",
              choices=c("People",
                "Female",
                "Male",
                "Male & Female"="MF"),
              selected=c("People")
            ),
            conditionalPanel(condition="input.DTIsex=='People'",
              selectInput("DTIcod", "Intent:",
                choices=c("All", "Unintentional", "Intentional", "Undetermined"),
                selected=c("All")
              )
            )
          # ),
          # conditionalPanel(condition="input.DTjur!='Australia' |
          #   (input.DTjur=='Australia' & input.DTIsex!='People')",
          #   selectInput("DTIcod2", "Intent:",
          #     choices=c("All", "Unintentional"),
          #     selected=c("All")
          #   )
          # )
        )
        #drug type selection below
        #intent & age checkboxes later
      ),

    # Drug type with age panels (DTAPage)##################
      conditionalPanel(condition="input.Page=='DTAPage'",
        radioButtons("DTAdrop", "Variable for dropdown list:",
            choices=c(
              "Age & Intent"="Age_Intent",
              "Drug"="Drug"
            ),inline=T,
            selected=c("Age_Intent")
        ),
        conditionalPanel(condition="input.DTAdrop=='Age_Intent'",
          selectInput("DTAIcod", "Intent:",
            choices=c("All", "Unintentional"),
            selected=c("All")
          ),

          conditionalPanel(condition="input.yax!='sr' & input.yax!='srci'",
            selectInput("DTAIage", "Age:",
              choices=c(
                "All ages",
                # "15 to 64"="15-64",
                "15 to 24"="15-24",
                "25 to 34"="25-34",
                "35 to 44"="35-44",
                "45 to 54"="45-54",
                "55 to 64"="55-64",
                "65 to 74"="65-74",
                "75 to 84"="75-84",
                "85+"
              ),
              selected=c("All ages")
            )
          )
        )
        #drug type selection below
        #intent & age checkboxes later
      ),
    # Drug type selection (DTJ & DTA Pages)#######
      conditionalPanel(condition="(input.Page=='DTAPage' & input.DTAdrop=='Drug')
        | (input.Page=='DTJPage' & input.DTJdrop=='Drug')",
        selectInput("DTdrugD", label=NULL,
          choices=c(
            "OPIOIDS",
            "heroin",
            "natural & semi-synthetic opioids",
            "methadone",
            "synthetic opioids",
            "AMPHETAMINES",
            "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC &\nANTIPARKINSONISM DRUGS",
            "barbiturates",
            "benzodiazepines",
            "antiepileptic & sedative-hypnotic drugs, unspecified (e.g. pregabalin)"
            ="antiepileptic & sedative-hypnotic drugs,\nunspecified",
            "ANTIDEPRESSANTS",
            "tricyclic & tetracyclic antidepressants",
            "other & unspecified antidepressants",
            "ANTIPSYCHOTICS & NEUROLEPTICS",
            "other & unspecified antipsychotics (e.g. quetiapine)"
            ="other & unspecified antipsychotics",
            "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS"="NONOPIOID ANALGESICS",
            "4-aminophenol derivatives (e.g. paracetamol)"="4-aminophenol derivatives",
            "other nonsteroidal anti-inflammatory drugs",
            "ALCOHOL",
            "COCAINE",
            "CANNABINOIDS"
          ),
          selected=c("OPIOIDS")
        )
      ),
    # Drug type checkbox (DTJ & DTA Pages)#######
      conditionalPanel(condition="(input.Page=='DTAPage' & input.DTAdrop!='Drug')
        | (input.Page=='DTJPage' & input.DTJdrop!='Drug')",
        checkboxInput("DTdrugA", label=HTML("<b>Drug:</b>"),value=TRUE
        ),
        HTML("<div style='margin-left: 6%;'>"),
        checkboxGroupInput("DTdrug", NULL, #"Drug:",
          choices=c(
            "OPIOIDS",
            "heroin",
            "natural & semi-synthetic opioids",
            "methadone",
            "synthetic opioids",
            "ALCOHOL",
            "AMPHETAMINES",
            "ANTIDEPRESSANTS",
            "tricyclic & tetracyclic antidepressants",
            "other & unspecified antidepressants",
            "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC &\nANTIPARKINSONISM DRUGS",
            "barbiturates",
            "benzodiazepines",
            "antiepileptic & sedative-hypnotic drugs, unspecified (e.g. pregabalin)"
            ="antiepileptic & sedative-hypnotic drugs,\nunspecified",
            "ANTIPSYCHOTICS & NEUROLEPTICS",
            "other & unspecified antipsychotics (e.g. quetiapine)"
            ="other & unspecified antipsychotics",
            "CANNABINOIDS",
            "COCAINE",
            "NONOPIOID ANALGESICS, ANTIPYRETICS & ANTIRHEUMATICS"="NONOPIOID ANALGESICS",
            "4-aminophenol derivatives (e.g. paracetamol)"="4-aminophenol derivatives",
            "other nonsteroidal anti-inflammatory drugs"
          ),
          selected=c("OPIOIDS", "ALCOHOL", "AMPHETAMINES", "ANTIDEPRESSANTS", 
            "ANTIEPILEPTIC, SEDATIVE-HYPNOTIC &\nANTIPARKINSONISM DRUGS",
            "ANTIPSYCHOTICS & NEUROLEPTICS", "CANNABINOIDS",
            "COCAINE", "NONOPIOID ANALGESICS"
          )
        ),
        HTML("</div>")
      ),

    # Opioid, sex & intent control panel (O5Page)###############
      conditionalPanel(condition =
        "input.Page=='O5Page'",
        radioButtons("O5drop", "Variable for dropdown list:",
          choices=c(
            "Opioid",
            "Intent",
            "Sex"
          ),inline=T,
          selected=c("Opioid")
        )
      ),

    # Opioid/Remoteness and age panels (RA, O4, E0 & W7 Pages)###############
      conditionalPanel(
        condition="input.Page=='RAPage' & input.jurR=='Australia'",
        radioButtons("dropRA", "Variable for dropdown list:",
          choices=c(
            "Remoteness region"="Region",
            "Age"
          ),inline=T,
          selected=c("Age")
        ),

        conditionalPanel(condition="input.dropRA=='Region'",
          conditionalPanel(
            condition="input.jurR=='Australia'",
            selectInput("RAraR", NULL,
               choices=c(
                 "Major Cities",
                 "Regional and Remote",
                 "-Inner Regional"="Inner Regional",
                 "-Outer Regional"="Outer Regional",
                 "-Remote and Very Remote"="Remote and Very Remote"
               ),
               selected=c(
                 "Regional and Remote"
               )
            )
          )
        )
      ),
      conditionalPanel(condition =
        "input.Page=='O4Page' | input.Page=='E0Page' | input.Page=='W7Page'",
        radioButtons("dropOA", "Variable for dropdown list:",
          choices=c(
            "Drug", "Age"
          ),inline=T,
          selected=c("Drug")
        )
      ),

        conditionalPanel(condition="input.yax!='sr' & input.yax!='srci' & 
          ((input.Page=='RAPage' & input.dropRA=='Age' & input.jurR=='Australia') |
          ((input.Page=='O4Page' | input.Page=='E0Page' | input.Page=='W7Page') &
          input.dropOA=='Age'))",
          selectInput("ageROA", label=NULL,
            choices=c(
              "All ages",
              # "15 to 64"="15-64",
              "15 to 24"="15-24",
              "25 to 34"="25-34",
              "35 to 44"="35-44",
              "45 to 54"="45-54",
              "55 to 64"="55-64",
              "65 to 74"="65-74",
              "75 to 84"="75-84",
              "85+"
            ),
            selected=c("All ages")
          )
        ),
        # RA remoteness checkbox later above opioid panels
        # O4 drug panel under Opioid checkbox (O4 & O5 Pages)
        # E0 drug panel under Exclusive opioid checkbox (E0 & E9 Pages)
        # W7 drug panel under Other drugs with opioid checkboxes (W7 & S8 Pages)
        # Age checkbox at the end

      conditionalPanel(condition="input.dropOA=='Drug'",
        conditionalPanel(condition="input.Page=='W7Page'",
          selectInput("W7Ddrug", label="Other drugs involved:",
            choices=c(
              "4-aminophenol derivatives (e.g. paracetamol)"="4-aminophenol derivatives",
              "Alcohol",#="All opioids with alcohol",
              "Amphetamines",#="All opioids with amphetamines",
              "Antidepressants",
              "Antiepileptic & sedative-hypnotic drugs, unspecified (e.g. pregabalin)"
             ="Antiepileptic & sedative-hypnotic drugs,\nunspecified",
              "Antipsychotics & neuroleptics",
              "Benzodiazepines"
            ),
            selected=c("Alcohol")
          )
        ),
        conditionalPanel(condition="input.Page=='E0Page'",
          selectInput("E0Odrug", label="Opioid types involved:",
            choices=c(
              "Exclusive heroin",
              # "Exclusive pharmaceutical opioids",
              "Exclusive other opioids",
              # "Illicit & pharmaceutical opioids",
              "Heroin & other opioids",
              # "Other & unspecified opioids"
              "Opium & unspecified opioids"
            ),
            selected=c(
              "Exclusive heroin" )
          )
        )
      ),
      conditionalPanel(condition =
        "input.Page=='O4Page' & input.dropOA=='Drug' |
        input.Page=='O5Page' & input.O5drop=='Opioid'",
        selectInput("OdrugO", label="Opioid involved:",
          choices=c(
            "All opioids",
            "Heroin",
            # "Opium",
            "Methadone",
            "Natural & semi-synthetic opioids",
            "Synthetic opioids",
            "Other & unspecified opioids"
          ),
          selected=c("All opioids")
        )
      ),

    # intent & sex panels (All, RA, RP, EP, W8 & E9 + O5 Pages)###############
      conditionalPanel(
        condition="input.Page=='AllPage' | input.Page=='RAPage' | input.Page=='W8Page' | input.Page=='E9Page'",
        radioButtons("dropSI", "Variable for dropdown list:",
           choices=c(
             "Intent", "Sex"
           ), inline=T,
           selected=c("Intent")
        )
      ),
      conditionalPanel(condition="(input.dropSI=='Sex' & 
        (input.Page=='AllPage' | input.Page=='RAPage' | input.Page=='W8Page' | input.Page=='E9Page')) |
        (input.O5drop=='Sex' & input.Page=='O5Page')",
        selectInput("sex4R", label=NULL,
          choices=c("People", "Female", "Male","Male & Female"="MF")
        )
      ),
      conditionalPanel(condition="input.Page=='AllPage'",
        conditionalPanel(condition= "input.dropSI!='Intent'",
          checkboxGroupInput(
            "AllScod", label="Intent:",
            c("All", "Unintentional", "Intentional", "Undetermined","Other"),
            selected=c("All", "Unintentional")
          )
        ),
        # Intent checkbox for W8 & E9 later below with other pages
        conditionalPanel(condition="input.dropSI=='Intent'",
          selectInput("AllIcod", label=NULL,
            choices=c("All", "Unintentional", "Intentional", "Undetermined","Other")
          )
        )
      ),
      conditionalPanel(condition=
        "(input.dropSI=='Intent' & input.Page=='W8Page') |
        (input.O5drop=='Intent' & input.Page=='O5Page')",
        selectInput("cod4R", label=NULL,
          choices=c("All", "Unintentional", "Intentional", "Undetermined")
        )
      ),
      conditionalPanel(condition="input.Page=='E9Page' & input.dropSI=='Intent'",
        selectInput("E9Icod", label=NULL,
          choices=c("All", "Unintentional", "Intentional")
        )
      ),
      conditionalPanel(condition="input.Page=='RPPage' | input.Page=='EPPage'",
        HTML("<b>Intent:</b>")
      ),
      conditionalPanel(
        condition="(input.Page=='RAPage' & input.dropSI=='Intent') | input.Page=='RPPage' | input.Page=='EPPage'",
        selectInput("codR", NULL,
          c("All", "Unintentional","All & Unintentional"="AA"),
          selected="All"
        )
      ),
      # Sex checkboxes for later below with other pages

    # Sex checkbox (All, RA, O5, O6, W8 & E9 Pages)###############
      conditionalPanel(condition="input.Page=='O6Page' |
        ((input.Page=='AllPage' | input.Page=='RAPage' | input.Page=='W8Page' | input.Page=='E9Page')
        & input.dropSI!='Sex') | (input.Page=='O5Page' & input.O5drop!='Sex')
        | (input.Page=='DTJPage' & input.DTJdrop=='Drug')",# & input.DTjur=='Australia'
        # conditionalPanel(condition="input.Page=='DTJPage' & input.DTJdrop=='Drug' & input.DTjur=='Australia' & input.DTage=='15-64' & input.sexC!='People'",
        #   HTML("<i>Please select All ages for data by male and/or female.</i>")
        # ),
        checkboxGroupInput("sexC", label="Sex:",
           choices=c("People", "Female", "Male"),
           selected=c("People", "Female", "Male")
        ),
      ),

    # Age range checkbox (CPage)###############
      conditionalPanel(
        condition="input.Page=='CPage' & input.yax!='sr' & input.yax!='srci'",
        checkboxGroupInput("Cage", "Age range:",
          choices=c(
            " All ages"#," 15 to 64"=" 15-64"
          ),
          selected=c(" All ages")#," 15-64")
        )
      ),
    # Intent(2C) checkbox (O6, RA, C, Am, DTA & DTJ Pages)###############
      conditionalPanel(
        condition="input.Page=='O6Page' |
        input.Page=='CPage' | input.Page=='AmPage' |
        (input.Page=='RAPage' & input.dropSI=='Sex') |
        (input.Page=='DTAPage' & input.DTAdrop=='Drug')",# | (input.Page=='DTJPage' & input.DTJdrop=='Drug' & input.!='Australia')
        checkboxGroupInput("cod2C", "Intent:",
          c("All", "Unintentional"),
          selected="All"
        )
      ),

    # Intent(3C) checkbox (E0 & E9 Pages)###############
      conditionalPanel(
        condition="input.Page=='E0Page' | (input.Page=='E9Page' & input.dropSI=='Sex')",
        checkboxGroupInput("cod3C", "Intent:",
          c("All", "Unintentional", "Intentional"),
          selected="All"
        )
      ),

    # Intent checkbox (4C) (DTJ, O4, O5, W7 & W8 Pages)###############
      conditionalPanel(condition="input.Page=='O4Page' | input.Page=='W7Page' |
        (input.Page=='W8Page' & input.dropSI!='Intent') | 
        (input.Page=='O5Page' & input.O5drop!='Intent') |
        (input.Page=='DTJPage' & input.DTJdrop=='Drug')", # & input.DTjur=='Australia'
        checkboxGroupInput(
          "cod4C", label="Intent:",
          c("All", "Unintentional", "Intentional", "Undetermined"),
          selected=c("All", "Unintentional")
        )
      ),

    # Intent split plot radio button (DTJ, O4, O5, W7, W8, Am, C & DTA Pages)###############
      conditionalPanel(condition="(input.AllScod.length==2 & input.Page=='AllPage' & input.dropSI=='Sex' & input.sex4R!='MF') |
        (input.cod4C.length==2 & ( input.Page=='O4Page' | input.Page=='W7Page' |
        (input.Page=='DTJPage' & input.DTJdrop=='Drug') |
        (input.Page=='O5Page' & input.O5drop!='Intent' & (input.O5drop!='Sex' | input.sex4R!='MF') ) |
        (input.Page=='W8Page' & input.dropSI!='Intent' & input.sex4R!='MF') )) |
        (input.cod3C.length==2 & ( input.Page=='E0Page' |
        (input.Page=='E9Page' & input.dropSI!='Intent' & input.sex4R!='MF') )) |
        (input.cod2C.length==2 & ( input.Page=='AmPage' | input.Page=='CPage' |
        (input.Page=='DTAPage' & input.DTAdrop=='Drug') ))",# & input.DTjur=='Australia'
        radioButtons("codS",label="Show intent as:",
          choices=c("Single plot"=1,"Split plot"=2),
          inline=T, selected=1
        )
      ),

    # Remoteness region checkboxes (RAPage)###############
      conditionalPanel(
        condition="input.Page=='RAPage' & input.dropRA=='Age'",
        conditionalPanel(
          condition="input.jurR=='Australia'",
          checkboxGroupInput("RAra", "Remoteness region:",
             choices=c(
               "Major Cities",
               "Regional and Remote",
               "-Inner Regional"="Inner Regional",
               "-Outer Regional"="Outer Regional",
               "-Remote and Very Remote"="Remote and Very Remote"
             ),
             selected=c(
               "Major Cities",
               "Regional and Remote"
             )
          )
        ),
        conditionalPanel(
          condition="input.jurR!='Australia'",
          checkboxGroupInput("Rra", "Remoteness region:",
              choices=c(
                "Major Cities",
                "Regional and Remote"
              ),
              selected=c(
                "Major Cities",
                "Regional and Remote"
              )
          )
        )
      ),

    # Other drugs with opioid checkboxes (W7 & W8 Pages)###############
      conditionalPanel(condition="input.Page=='W8Page' |
        (input.Page=='W7Page' & input.dropOA=='Age')",
        checkboxGroupInput("Wdrug", "All opioids with:",
          choices=c(
            "4-aminophenol derivatives (e.g. paracetamol)"="4-aminophenol derivatives",
            "Alcohol",#="All opioids with alcohol",
            "Amphetamines",#="All opioids with amphetamines",
            "Antidepressants",
            "Antiepileptic & sedative-hypnotic drugs, unspecified (e.g. pregabalin)"
           ="Antiepileptic & sedative-hypnotic drugs,\nunspecified",
            "Antipsychotics & neuroleptics",
            "Benzodiazepines"
          ),
          selected=c("Alcohol")
        )
      ),
    # Opioid checkbox (O4 & O5 Pages)###############
      conditionalPanel(condition="(input.Page=='O4Page' & input.dropOA!='Drug')
        | (input.Page=='O5Page' & input.O5drop!='Opioid')",
        checkboxGroupInput("OdrugC", "Opioid:",
          choices=c(
              "All opioids",
              "Heroin",
              # "Opium",
              "Methadone",
              "Natural & semi-synthetic opioids",
              "Synthetic opioids",
              "Other & unspecified opioids"
          ),
          selected=c("All opioids")
        )
      ),
    # Exclusive opioid checkbox (E9 & E0 Pages)###############
      conditionalPanel(condition="input.Page=='E9Page' |
        (input.Page=='E0Page' & input.dropOA=='Age')",
        checkboxGroupInput("Edrug", "Drug:",
          choices=c(
            # "Exclusive illicit opioids",
            "Exclusive heroin",
            # "Exclusive pharmaceutical opioids",
            "Exclusive other opioids",
            # "Illicit & pharmaceutical opioids",
            "Heroin & other opioids",
            # "Other & unspecified opioids"
            "Opium & unspecified opioids"
          ),
          selected=c(
            "Exclusive heroin",
            "Exclusive other opioids"
          )
        )
      ),

    # Age checkbox (All, Am, RA, O4, E0, W7 & DTA Pages)###############
      conditionalPanel(condition="(input.Page=='AllPage' | input.Page=='AmPage'
        | (input.Page=='RAPage' & input.jurR=='Australia' & input.dropRA!='Age')
        | ((input.Page=='O4Page' | input.Page=='E0Page' | input.Page=='W7Page')
        & input.dropOA=='Drug') | (input.Page=='DTAPage' & input.DTAdrop=='Drug'))
        & input.yax!='sr' & input.yax!='srci'",
        checkboxInput("ageAllA", label=HTML("<b>Age:</b>"),value=F
        ),
        HTML("<div style='margin-left: 6%;'>"),
        checkboxGroupInput("ageAll", NULL, #"Age:",
          c("All ages",
             # "15 to 64"="15-64",
             "15 to 24"="15-24",
             "25 to 34"="25-34",
             "35 to 44"="35-44",
             "45 to 54"="45-54",
             "55 to 64"="55-64",
             "65 to 74"="65-74",
             "75 to 84"="75-84",
             "85+"
            ),
          selected=c(
            "15 to 24"="15-24",
            "25 to 34"="25-34",
            "35 to 44"="35-44",
            "45 to 54"="45-54",
            "55 to 64"="55-64",
            "65 to 74"="65-74",
            "75 to 84"="75-84",
            "85+"
          )
        ),
        HTML("</div>")
      )
    )
  ),
# to identify Tabs: https://www.geeksforgeeks.org/how-to-use-dynamic-variable-names-in-javascript/
  conditionalPanel(condition="input.dimension>767 & 
    (input.Page=='Method' | input.Page=='Cites' | eval('input.'+input.Page)=='Notes')",
    fluidRow(column(includeHTML("DT-logos.html"),width=3,offset=9))
  ),
#####indeterminate checkbox update not working yet - addEventListener not working???
#https://www.w3schools.com/js/js_htmldom_eventlistener.asp
  tags$script(src="DropChk.js")
  )
    
#DRUG TRENDS COLOUR:
#DT: #475c07
#NIDIP: #6e2a8d
#EDRS: #de761c
#IDRS: #00aeef
#DNet: #c4161c
# bootstrap breakpoints: 480, >767 <768, >991 <992, >1199 <1200
}
