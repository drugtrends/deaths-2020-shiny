#For ABS COD 2020 data received in Mar 2022
#N. Man
#library(shiny) #already loaded in UI
library(ggplot2)
#library(plotly) #already loaded in UI
library(shinycustomloader)

#https://appsilon.com/fast-data-loading-from-files-to-r/
# load("death_2019.Rdata")
load("death_2020.Rdata")

ColtypFn <- function(Pd,Gp,Yax=yax,Varc="Age",Vart="Intent",Split="",EstForm="") {
  # print(paste(EstForm,( (Varc!="" & Vart!="") | EstForm>"Colo" ) & Split!=Varc & (Split!=Vart | EstForm=="Type2") ,Varc,Vart,Split))
  # Gp <- ggplot(data=Pd)
  if (( (Varc!="" & Vart!="") | EstForm>"Colo" ) & Split!=Varc & (Split!=Vart | EstForm=="Type2") ) {
    Var <- paste0(Varc,Vart)
    if (Varc==Vart) Var <- Varc
    Gp <- Gp + scale_colour_manual( values=get(paste0(Var,"cols")),name=NULL ) +
      scale_linetype_manual( values=get(paste0(Var,"type")),name=NULL )
    if (Yax=="srci" | Yax=="crci") Gp <- Gp + scale_fill_manual( values=get(paste0(Var,"cols")),name=NULL )
    if (EstForm<="Alph") {
      Gp <- Gp + aes(colour=get(Var), linetype=get(Var))
      if (Yax=="srci" | Yax=="crci") Gp <- Gp + aes(fill=get(Var))
    }
  } else if (( Varc!="" | EstForm=="Colo" ) & Split!=Varc) {
    Gp <- Gp + scale_colour_manual( values=get(paste0(Varc,"cols")),name=NULL )
    if (Yax=="srci" | Yax=="crci") Gp <- Gp + scale_fill_manual( values=get(paste0(Varc,"cols")),name=NULL )
    if (Varc!="") {
      Gp <- Gp + aes(colour=get(Varc))
      if (Yax=="srci" | Yax=="crci") Gp <- Gp + aes(fill=get(Varc))
    }
  } else if ( Vart!="" & Split!=Vart ) {
    Gp <- Gp + aes(linetype=get(Vart)) +
      scale_linetype_manual( values=get(paste0(Vart,"type")),name=NULL )
  } # else if (EstForm=="" & RVData>0) {
  #   if (RVData==2) {
  #     Gp <- Gp + aes(color=Release) +
  #       scale_color_manual(values=Notecolo)
  #   }
    # Gp <- Gp + scale_shape_manual(values=Noteshap) +
    #   geom_point(data=subset(Pd,Note!=""),aes(shape=Note,text=NULL),size=2) #,show.legend=F) - does nothing
  # }
  # Gp <- Gp + scale_shape_manual(values=Noteshap) +
  #   # geom_point(data=subset(Pd,Note=="revised"),aes(text=NULL),shape=1,size=2) + #,show.legend=F) +
  #   # geom_point(data=subset(Pd,Note=="prelim."),aes(text=NULL),shape=4,size=2) #,show.legend=F)
  #   geom_point(data=subset(Pd,Note!=""),aes(shape=Note,text=NULL),size=2,show.legend=F)
  return(Gp)
}

PlotFn <- function(Pd,Gp,Yax,Yr,Labc,Labt,EstForm="",RVData=1) {
  if (Yax=="num") {
    Gp <- Gp + geom_line() + aes(y=n, text=paste0(
        "Year: ",year," <i>",Note,"</i>",
        "<br>Number of deaths: ",n,
        "<br>",Labt,": ",get(Labt),
        "<br>",Labc,": ",get(Labc)
      )) +
      scale_y_continuous(limits=c(0, max(Pd$n, 250))) +
      labs(y="Number of deaths")
  } else if (Yax=="cr" | Yax=="crci") {
    if (Yax=="cr") {
      Gp <- Gp + geom_line() + aes(y=cr, text=paste0(
          "Year: ",year," <i>",Note,"</i>",
          "<br>Number of deaths: ",n,
          "<br>Crude rate: ",cr_p, # rounded off in dataset instead to avoid potential confidentiality issue with small numbers being discoverable
          "<br>",Labt,": ",get(Labt),
          "<br>",Labc,": ",get(Labc),
          ifelse(year<2020,"",paste0("<br>Rate ratio (vs 2019): ",crr_p))
        ))
    } else {
#"Warning: Ignoring unknown aesthetics: text" because it is used with geom_line but we don't want text box with the ribbon
      Gp <- Gp + aes(y=cr) + geom_line(aes(text=paste0(
          "Year: ",year," <i>",Note,"</i>",
          "<br>Number of deaths: ",n,
          "<br>Crude rate: ",cr_p,
          "<br>",Labt,": ",get(Labt),
          "<br>",Labc,": ",get(Labc)
        ))) +
        geom_ribbon(aes(ymin=cr_lci,ymax=cr_uci), alpha=0.1,size=0)
    }
    Gp <- Gp + scale_y_continuous(limits=c(0, max(Pd$cr_uci, 2.5,na.rm=T))) +
      labs(y="Crude death rate per 100,000")
  } else if (Yax=="sr" | Yax=="srci") {
    # Pd <- subset(Pd,Age=="All ages")
    Gp <- Gp + aes(y=sr)
    if (Yax=="sr") {
      if (EstForm=="Alph") {
        Gp <- Gp + geom_line( aes(text=paste0(
            "Year: ",year," <i>",Note,"</i>",
            "<br>Number of deaths: ",n,
            "<br>Age standardised rate: ",sr_p,
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc),
            ifelse(year<2020,"",paste0("<br>Rate ratio (vs 2019): ",srr_p))
          ),alpha="Age standardised") ) +

          geom_line(aes(y=cr, text=paste0(
            "Year: ",year," <i>",Note,"</i>",
            "<br>Number of deaths: ",n,
            "<br>Crude rate: ",cr_p,
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc),
            ifelse(year<2020,"",paste0("<br>Rate ratio (vs 2019): ",crr_p)))
          ,alpha="Crude")) +
          scale_alpha_manual(values=c(1,.3),name=NULL)
      } else if (EstForm=="Colo") {
        # if (nrow(unique(Pd[,Labt]))==1) {
        Gp <- Gp + geom_line( aes(text=paste0(
            "Year: ",year," <i>",Note,"</i>",
            "<br>Number of deaths: ",n, 
            "<br>Age standardised rate: ",sr_p,
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc),
            ifelse(year<2020,"",paste0("<br>Rate ratio (vs 2019): ",srr_p))
          ),color="Age standardised") ) +
          geom_line( aes(y=cr, text=paste0(
            "Year: ",year," <i>",Note,"</i>",
            "<br>Number of deaths: ",n,
            "<br>Crude rate: ",cr_p,
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc),
            ifelse(year<2020,"",paste0("<br>Rate ratio (vs 2019): ",crr_p))
          ),color="Crude") )
      } else if (EstForm=="Colo2") {
        Gp <- Gp + geom_line( aes(text=paste0(
            "Year: ",year," <i>",Note,"</i>",
            "<br>Number of deaths: ",n,
            "<br>Age standardised rate: ",sr_p,
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc),
            ifelse(year<2020,"",paste0("<br>Rate ratio (vs 2019): ",srr_p))
          ),color=paste("Age standardised",get(Labt),sep=","),linetype=paste("Age standardised",get(Labt),sep=",")) ) +
          geom_line( aes(y=cr, text=paste0(
            "Year: ",year," <i>",Note,"</i>",
            "<br>Number of deaths: ",n,
            "<br>Crude rate: ",cr_p,
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc),
            ifelse(year<2020,"",paste0("<br>Rate ratio (vs 2019): ",crr_p))
          ),color=paste("Crude",get(Labt),sep=","),linetype=paste("Crude",get(Labt),sep=",")) )
      } else if (EstForm=="Type2") {
        Gp <- Gp + geom_line( aes(text=paste0(
            "Year: ",year," <i>",Note,"</i>",
            "<br>Number of deaths: ",n,
            "<br>Age standardised rate: ",sr_p,
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc),
            ifelse(year<2020,"",paste0("<br>Rate ratio (vs 2019): ",srr_p))
          ),color=paste("Age standardised",get(Labc),sep=","),linetype=paste("Age standardised",get(Labc),sep=",")) ) +
          geom_line( aes(y=cr, text=paste0(
            "Year: ",year," <i>",Note,"</i>",
            "<br>Number of deaths: ",n,
            "<br>Crude rate: ",cr_p,
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc),
            ifelse(year<2020,"",paste0("<br>Rate ratio (vs 2019): ",crr_p))
          ),color=paste("Crude",get(Labc),sep=","),linetype=paste("Crude",get(Labc),sep=",")) )
      } else {
        Gp <- Gp + geom_line() + aes(text=paste0(
            "Year: ",year," <i>",Note,"</i>",
            "<br>Number of deaths: ",n,
            "<br>Age standardised rate: ",sr_p,
            "<br>",Labt,": ",get(Labt),
            "<br>",Labc,": ",get(Labc),
            ifelse(year<2020,"",paste0("<br>Rate ratio (vs 2019): ",srr_p))
          ))
      }
    } else {
      Gp <- Gp + geom_line( aes(text=paste0(
          "Year: ",year," <i>",Note,"</i>",
          "<br>Number of deaths: ",n,
          "<br>Age standardised rate: ",sr_p,
          "<br>",Labt,": ",get(Labt),
          "<br>",Labc,": ",get(Labc)
        )) ) + geom_ribbon(aes(ymin=sr_lci, ymax=sr_uci), alpha=0.1, size=0)
    }
    if (EstForm=="") {
      Gp <- Gp + labs(y="Age standardised death rate per 100,000")
    } else {
      Gp <- Gp + labs(y="Death rate per 100,000")
    }
    Gp <- Gp + scale_y_continuous(limits=c(0, max(Pd$sr_uci,Pd$cr_uci,2.5,na.rm=T)))
  }
  if (RVData>0) {
    if (EstForm=="" | EstForm=="Colo2") #Yax!="sr" | 
      Gp <- Gp + #scale_shape_manual(values=Noteshap) +
        # geom_point(data=subset(Pd,Note==""),aes(x=year),shape=16,show.legend=F) +
        geom_point(data=subset(Pd,Note=="(revised)"),aes(x=year+.08),shape=1,size=1.5,show.legend=F) +
        geom_point(data=subset(Pd,Note=="(prelim.)"),aes(x=year+.08),shape=4,size=1.5,show.legend=F) +
        geom_point(data=subset(Pd,Note==""),shape=16,size=.5)
      # geom_point(data=subset(Pd,Note!=""),aes(text=NULL),show.legend=F)
    if (Yax=="sr" & EstForm=="Colo2") { # | EstForm=="Colo"
      Gp <- Gp +
        geom_point(data=subset(Pd,Note=="(revised)"),aes(x=year+.08,y=cr),shape=1,size=1.5,color="red") +
        geom_point(data=subset(Pd,Note=="(prelim.)"),aes(x=year+.08,y=cr),shape=4,size=1.5,color="red") +
        geom_point(data=subset(Pd,Note==""),aes(y=cr),shape=16,size=.5,color="red")
      # } # else if (EstForm=="Alph") {
      #   Gp <- Gp +
      #   geom_point(data=subset(Pd,Note=="revised"),aes(x=year+.1,y=cr,text=NULL),shape=1,alpha=0.3,show.legend=F) +
      #   geom_point(data=subset(Pd,Note=="prelim."),aes(x=year+.1,y=cr,text=NULL),shape=4,alpha=0.3,show.legend=F)
      # } else if (EstForm!="") {
      #   Gp <- Gp +
      #   geom_point(data=subset(Pd,Note=="revised"),aes(x=year+.1,y=cr,text=NULL),shape=1,show.legend=F) +
      #   geom_point(data=subset(Pd,Note=="prelim."),aes(x=year+.1,y=cr,text=NULL),shape=4,show.legend=F)
    }
    # } else {
      if (EstForm=="" & RVData==2) {
        Gp <- Gp + aes(color=Release) +
          scale_color_manual(values=Notecolo,name=NULL)
        if (Yax=="srci" | Yax=="crci")
          Gp <- Gp + aes(fill=Release) +
            scale_fill_manual(values=Notecolo,name=NULL)
      }
    # }
  }

  Gp <- Gp + labs(x="Year") + aes(x=year, group=1) +
    scale_x_continuous(breaks=seq(Yr[1],Yr[2],2)) +
    theme_light() + theme(
      legend.title=element_blank(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.x=element_blank())
  return(Gp)
}

SplitFn <- function(Pd,Gp,Vars,PageDim) {
  if (PageDim<992) {
    Gp <- Gp + facet_grid(rows=vars(get(Vars)))
  } else {
    Gp <- Gp + facet_grid(cols=vars(get(Vars)))
    if (PageDim<1200) {
      Gp <- Gp + theme(
        axis.text.x=element_text(angle=90)
        # axis.title.x=element_text() # vjust & lineheight not working
      )
    #   LO("h")
    #   LX(0)
    #   LY(-.2)
    }
  }
  # NB: need to have grid codes last
  Gp <- Gp + theme(strip.background=element_rect(fill="#B693BF"),
      strip.text=element_text(color="#ffffff", face="bold") )
  return(Gp)
}

PlyFn <- function(Gp,Lt,O=LO(),X=LX(),Y=LY(),Split="") {  #Pd,Yax,
    if (Split=="") {
      tag_col <- "grey"
    } else {
      tag_col <- "#ffffff"
    }

    ggplotly(Gp, tooltip="text") %>%
      # add_markers(data=pr,
      #   # type="scatter",
      #   # mode="markers",
      #   y=get(Yax),
      #   x=year,
      #   marker=list(
      #     color='grey',
      #     symbol="cross"
      # )) %>%
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource-analytics/trends-drug-induced-deaths-australia-1997-2020">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x=0, xanchor="left",
        y=1.04, yanchor="top",
        showarrow=F, font=list(size=10, color=tag_col)
      ) %>%
      layout(
        images=list(
          source="NIDIP-Logo.png",
          x=0.01, xanchor="left", y=.99, yanchor="top",
          sizex=0.07, sizey=0.2,
          xref="paper", yref="paper"
        ))  %>%
      add_annotations(
        text=Lt, xref="paper", yref="paper",
        x=X, xanchor="left",
        y=Y, yanchor="bottom",
        legendtitle=T, showarrow=F
      ) %>%
      layout(legend=list(orientation=O, y=Y, yanchor="top"), margin=list(b=80)) %>% 
      config(displaylogo=F, modeBarButtonsToRemove=list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d","zoomIn2d","zoomOut2d","autoScale2d","toggleSpikelines"))
  # https://stackoverflow.com/questions/42763280/r-ggplot-and-plotly-axis-margin-wont-change
  # if (Split!="") Gp[['x']][['layout']][['annotations']][[1]][['y']] <- -0.13
  # Gp
}
# Allow for site's state to be bookmarked via the url
# See https://shiny.rstudio.com/articles/bookmarking-state.html for details
enableBookmarking("url")

server <- function(input, output, session) {

  # Allow direct linking to specific tabs (with default configs)  
  observe({
  # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    #Shorten URL - https://shiny.rstudio.com/reference/shiny/latest/setBookmarkExclude.html
    setBookmarkExclude(c("plotly_hover-A","plotly_afterplot-A","plotly_relayout-A",
       ".clientValue-default-plotlyCrosstalkOpts","dimension","DTdrugA","ageAllA",
       "AllPage","RAPage","RPPage","DTJPage","DTAPage","O4Page","O5Page","O6Page",
       "E0Page","E9Page","EPPage","W7Page","W8Page","AmPage","CPage"))
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })

#Using this instead to identify Tabs: https://www.geeksforgeeks.org/how-to-use-dynamic-variable-names-in-javascript/
#so selection panel shows instead of logos when switching to another Plot
  # observeEvent( eventExpr=input$Page,handlerExpr={
  #   updateTabsetPanel(session, "Tab",
  #     selected="Plot")
  # })
  
  LX <- reactiveVal(1.02)
  LO <- reactiveVal("v")
  LY <- reactiveVal(0.99)
  observeEvent( eventExpr=input$dimension,handlerExpr={
    if (input$dimension<992) {
      LO("h")
      LY(-.2)
      LX(0)
    } else {
      LO("v")
      LY(0.99)
      LX(1.02)
      # if (Split!="" & input$dimension<1200) {
      #   LO("h")
      #   LX(0)
      #   LY(-.2)
      # }
    }
  })
  observeEvent( eventExpr=input$DTdrugA, ignoreInit=T, handlerExpr={
    if (input$DTdrugA==T) {
      updateCheckboxGroupInput(session,"DTdrug",NULL,choices=c(
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
      ),selected=c(
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
        "antiepileptic & sedative-hypnotic drugs,\nunspecified",
        "ANTIPSYCHOTICS & NEUROLEPTICS",
        "other & unspecified antipsychotics",
        "CANNABINOIDS",
        "COCAINE",
        "NONOPIOID ANALGESICS",
        "4-aminophenol derivatives",
        "other nonsteroidal anti-inflammatory drugs"
      ))
    } else if (input$DTdrugA==F) {
      updateCheckboxGroupInput(session,"DTdrug",NULL,
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
        selected=character(0)
      )
    }
  })
  observeEvent( eventExpr=input$ageAllA, ignoreInit=T, handlerExpr={
    if (input$ageAllA==T) {
      updateCheckboxGroupInput(session,"ageAll",NULL,choices=c(
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
        selected=c(
          "All ages",
          # "15-64",
          "15-24",
          "25-34",
          "35-44",
          "45-54",
          "55-64",
          "65-74",
          "75-84",
          "85+"
        )
      )
    } else if (input$ageAllA==F) {
      updateCheckboxGroupInput(session,"ageAll",NULL,choices=c(
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
        selected=character(0)
      )
    }
  })

# Cross-sectional profile of DID - Sex ------------------------------------------
  output$SexPlot <- renderPlotly({
    #needs to be sorted [order(...)] & made distinct
    Split <- ""
    Title <- "Sex" # paste0(input$jur,", Year: ",input$yrX)
    JUR <- input$jur
    if (JUR=="Australia") {
    } else {
    }

    pd <- subset(COD2020_All,Release=="Current" &
            jurisdiction==input$jur & Intent=="All" &
            year==input$yrX & Sex!="People" & Age=="All ages") %>%
        group_by(jurisdiction) %>% 
        mutate(
          percent=round(n/sum(n)*100,2),
          # percent=round(n/N*100,4),
          # percent=n/N*100,
        )
    
    if (JUR=="Australia") {
    } else {
    }

    if (JUR=="Australia") { # & input$ageR=="All ages"
    } else {
    }

    validate(need(nrow(pd) > 0, "No data selected"))
    gp <- ggplot(pd, aes(x=Sex, y=percent, fill=Sex, group=1, text=paste0(
        "Year: ",year," <i>",Note,"</i>",
        "<br>Number of deaths: ",n,
        "<br>Percent: ",format(round(percent,1),nsmall=1), "%",
        "<br>Jurisdiction: ",jurisdiction,
        "<br>Sex: ",Sex,
        "<br>Intent: ",Intent
      ))) +
      # geom_area() +
      geom_bar(stat="identity") +
      labs(x="",y="Percent of drug-induced deaths",title=Title) + # x="Sex"
      scale_fill_manual(values=c("red","blue")) +
      scale_y_continuous(limits=c(0,100))

    gp <- gp + theme_light() + theme(
        legend.title=element_blank(),
        legend.position="none",
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())

    PlyFn(Gp=gp,Lt="",X=LX(),Y=LY(),O=LO(),Split=Split)
  })

# Cross-sectional profile of DID - Age by sex ------------------------------------------
  output$AgePlot <- renderPlotly({
    #needs to be sorted [order(...)] & made distinct
    Split <- ""
    Title <- "Age" #paste0(input$jur,", year: ",input$yrX)
    JUR <- input$jur

    if (JUR=="Australia") {
    } else {
    }

    pd <- subset(COD2020_All,Release=="Current" &
        jurisdiction==input$jur & Intent=="All" &
        year==input$yrX & Sex!="People" & Age!="All ages") %>%
      group_by(Sex) %>% 
      mutate(
        percent=round(n/sum(n)*100,2),
        # percent=round(n/N*100,4),
        # percent=n/N*100,
      ) %>%
      ungroup %>%
      mutate(
        percent=ifelse(Sex=="Female",-percent,percent)
      ) %>%
      distinct

    # print(nrow(subset(pd,is.na(n))))
    if (nrow(subset(pd,is.na(n)))>0) {
      N_all <- subset(COD2020_All,Release=="Current" &
        jurisdiction==input$jur & Intent=="All" &
        year==input$yrX & Sex=="People" & Age=="All ages")$n

      pd <- subset(COD2020_All,Release=="Current" &
          jurisdiction==input$jur & Intent=="All" &
          year==input$yrX & Sex=="People" & Age!="All ages") %>%
        mutate(
          percent=round(n/N_all*100,2),
          # percent=round(n/N*100,4),
          # percent=n/N*100,
        ) %>%
        ungroup %>%
        subset(!is.na(percent)) %>%
        distinct

      sexcols <- c("#6e2a8d")
    } else {
      sexcols <- c("red","blue")
    }

    validate(need(nrow(pd) > 0, "No data selected"))
    gp <- ggplot(pd, aes(y=Age, x=percent, fill=Sex, group=1, text=paste0(
        "Year: ",year," <i>",Note,"</i>",
        "<br>Number of deaths: ",n,
        "<br>Percent: ",format(round(abs(percent),1),nsmall=1), "%",
        "<br>Jurisdiction: ",jurisdiction,
        "<br>Sex: ",Sex,
        "<br>Age: ",Age,
        "<br>Intent: ",Intent
      ))) +
      # geom_area() +
      geom_col(stat="identity") +
      labs(y="",x="Percent of drug-induced deaths",title=Title) + # y="Age"
      scale_fill_manual(values=sexcols) + theme_light()
    
    if (length(sexcols)==2) {
      gp <- gp +
        scale_x_continuous(breaks=seq(-100,100,10),labels=abs(seq(-100,100,10))) +
        theme(legend.title=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())
    } else {
      gp <- gp + theme(
          legend.position="none",
          legend.title=element_blank(),
          panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank()
        )       
    }

    PlyFn(Gp=gp,Lt="",X=LX(),Y=LY(),O=LO(),Split=Split)
  })

# Cross-sectional profile of DID - Remoteness area ------------------------------------------
  output$RemPlot <- renderPlotly({
    #needs to be sorted [order(...)] & made distinct
    Split <- ""
    # Title <- paste0(input$jur,", Year: ",input$yrX)
    Title <- "Remoteness area"
    JUR <- input$jur
    if (JUR=="Australia") {
    } else {
    }

    pd <- subset(COD2020_Rem,Release=="Current" &
            jurisdiction==input$jur & Intent=="All" &
            year==input$yrX & Sex=="People" & Age=="All ages") %>%
        group_by(jurisdiction) %>% 
        mutate(
          percent=round(n/sum(n)*100,2),
          # percent=round(n/N*100,4),
          # percent=n/N*100,
        )
    
    if (JUR=="Australia") {
      pd <- filter(pd, Region!="Regional and Remote") %>%
        group_by(year, Intent, Sex, jurisdiction, Age) %>% 
        # distinct() %>%
        mutate(
          # percent=round(n/sum(n)*100,2),
          # percent=round(n/N*100,4),
          percent=n/N*100,
          # Region=factor(Region, levels=c( "Remote and Very Remote",
          #   "Outer Regional", "Inner Regional", "Major Cities"
          # ))
        )
    } else {
      pd <- filter(pd, Region=="Regional and Remote" | Region=="Major Cities" ) %>%
        group_by(year, Intent, Sex, jurisdiction, Age) %>% 
        # distinct() %>%
        mutate(
          # percent=round(n/sum(n)*100,2),
          # percent=round(n/N*100,4),
          percent=n/N*100,
          # Region=factor(Region, levels=c( "Regional and Remote",
          #   "Major Cities"
          # ))
        )
    }

    if (JUR=="Australia") { # & input$ageR=="All ages"
    } else {
    }

    validate(need(nrow(pd) > 0, "No data selected"))
    gp <- ggplot(pd, aes(x=Region, y=percent, fill=Region, group=1, text=paste0(
        "Year: ",year," <i>",Note,"</i>",
        "<br>Number of deaths: ",n,
        "<br>Percent: ",format(round(percent,1),nsmall=1), "%",
        "<br>Area: ",Region,
        "<br>Jurisdiction: ",jurisdiction,
        "<br>Age: ",Age,
        "<br>Sex: ",Sex,
        "<br>Intent: ",Intent
      ))) +
      # geom_area() +
      geom_bar(stat="identity") +
      labs(x="",y="Percent of drug-induced deaths",title=Title) +
      scale_fill_manual(values=Regioncols,name=NULL)

    gp <- gp + theme_light() + theme(
        legend.title=element_blank(),
        legend.position="none",
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        axis.text.x=element_text(angle=90)
      )

    PlyFn(Gp=gp,Lt="",X=LX(),Y=LY(),O=LO(),Split=Split)
  })

# Cross-sectional profile of DID - Intent ------------------------------------------
  output$IntPlot <- renderPlotly({
    #needs to be sorted [order(...)] & made distinct
    Split <- ""
    Title <- "Intent" # paste0(input$jur,", Year: ",input$yrX)
    JUR <- input$jur
    if (JUR=="Australia") {
    } else {
    }

    N_all <- subset(COD2020_All,Release=="Current" &
            jurisdiction==input$jur & Intent=="All" &
            year==input$yrX & Sex=="People" & Age=="All ages")$n

    pd <- subset(COD2020_All,Release=="Current" &
            jurisdiction==input$jur & Intent!="All" &
            year==input$yrX & Sex=="People" & Age=="All ages" & !is.na(n)) %>%
        group_by(jurisdiction) %>% 
        mutate(
          # percent=round(n/sum(n)*100,2),
          # percent=round(n/N*100,4),
          percent=n/N_all*100,
        )

    if (JUR=="Australia") { # & input$ageR=="All ages"
    } else {
    }

    validate(need(nrow(pd) > 0, "No data selected"))
    gp <- ggplot(pd, aes(x=Intent, y=percent, group=1, text=paste0( #fill=Intent, 
        "Year: ",year," <i>",Note,"</i>",
        "<br>Number of deaths: ",n,
        "<br>Percent: ",format(round(percent,1),nsmall=1), "%",
        "<br>Jurisdiction: ",jurisdiction,
        "<br>Sex: ",Sex,
        "<br>Intent: ",Intent
      ))) +
      # geom_area() +
      geom_bar(stat="identity",fill="#6e2a8d") +
      labs(x="",y="Percent of drug-induced deaths",title=Title) + #x="Sex"
      scale_y_continuous(limits=c(0,100))
      # scale_fill_manual(values="#6e2a8d")

    gp <- gp + theme_light() + theme(
        legend.title=element_blank(),
        legend.position="none",
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        axis.text.x=element_text(angle=90))

    PlyFn(Gp=gp,Lt="",X=LX(),Y=LY(),O=LO(),Split=Split)
  })

# Cross-sectional profile of DID - Drug type ------------------------------------------
  output$DTPlot <- renderPlotly({
    #needs to be sorted [order(...)] & made distinct
    Split <- ""
    # Title <- paste0(input$jur,", Year: ",input$yrX)
    Title <- "Drug involved"
    JUR <- input$jur
    if (JUR=="Australia") {
    } else {
    }

    N_all <- subset(COD2020_All,Release=="Current" &
            jurisdiction==input$jur & Intent=="All" &
            year==input$yrX & Sex=="People" & Age=="All ages")$n

    pd <- subset(COD2020_DT,
            jurisdiction==input$jur & Intent=="All" &
            year==input$yrX & Sex=="People" & Age=="All ages" & grepl("[A-Z]",Drug)) %>%
        mutate(
          percent=n/N_all*100,
          # percent=round(n/sum(n)*100,2),
          # percent=round(n/N*100,4),
          # percent=n/N*100,
        ) %>%
        subset(!is.na(percent))
    
    if (JUR=="Australia") {
    } else {
    }

    validate(need(nrow(pd) > 0, "No data selected"))
    gp <- ggplot(pd, aes(x=Drug, y=percent, fill=Drug, group=1, text=paste0(
        "Year: ",year," <i>",Note,"</i>",
        "<br>Number of deaths: ",n,
        "<br>Percent: ",format(round(percent,1),nsmall=1), "%",
        "<br>Drug involved: ",Drug,
        "<br>Jurisdiction: ",jurisdiction,
        "<br>Age: ",Age,
        "<br>Sex: ",Sex,
        "<br>Intent: ",Intent
      ))) +
      # geom_area() +
      geom_bar(stat="identity") +
      labs(x="",y="Percent of drug-induced deaths",title=Title) +
      scale_fill_manual(values=Drugcols,name=NULL)

    gp <- gp + theme_light() + theme(
        legend.title=element_blank(),
        legend.position="none",
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        axis.text.x=element_text(angle=90)
      )

    PlyFn(Gp=gp,Lt="",X=LX(),Y=LY(),O=LO(),Split=Split)
  })

# All drug deaths by jurisdiction, Intent, Sex & Age (Table 1a, 1b, 1c) -----------------------------------------------------------------
  output$AllPlot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    AGE <- input$ageAll
    if (yax=="sr" | yax=="srci") AGE <- "All ages"
    if (input$dropSI=="Intent") {
      INTENT <- input$AllIcod
      SEX <- input$sexC
      labt <- "Sex"
    } else if (input$dropSI=="Sex") {
      INTENT <- input$AllScod
      SEX <- input$sex4R
      if (input$sex4R=="MF") {
        SEX <- c("Female","Male")
        Split <- "Sex"
      }
      labt <- "Intent"
    }
    labc <- "Age"

    Title <- input$jur
    if (length(SEX)==1) Title <- paste0(Title,", ",SEX)
    if (length(AGE)==1) Title <- paste0(Title,", Age: ",AGE)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)

    varc <- labc
    vart <- labt
    if (length(AGE)==1 & estimateForm=="") {
      varc <- ""
      Legend <- labt
    }
    if (input$dropSI=="Intent") {
      if (length(SEX)==1) {
        vart <- ""
        if (estimateForm!="") {
          Legend <- "Estimate type"
          estimateForm <- "Colo"
        } else {
          Legend <- varc
        }
      } else {
        if (estimateForm!="") {
          Legend <- "Estimate type by Sex"
          estimateForm <- "Colo2"
        } else if (length(AGE)>1) {
          Legend <- "Age by Sex"
        }
      }
    } else if (input$dropSI=="Sex") {
      if (length(input$AllScod)==1 | (length(input$AllScod)==2 & input$codS==2 & Split=="")) {
        vart <- ""
        if (length(input$AllScod)==2) Split <- labt
        if (estimateForm!="") {
          Legend <- "Estimate type"
          estimateForm <- "Colo"
        } else {
          Legend <- varc
        }
      } else {
        if (estimateForm!="") {
          Legend <- "Estimate type by Intent"
          estimateForm <- "Colo2"
        } else if (length(AGE)>1) {
          Legend <- "Age by Intent"
        }
      }
    }

    pd <- subset( COD2020_All, jurisdiction==input$jur &
      Sex %in% SEX & Age %in% AGE & Intent %in% INTENT &
      (year>=yr[1] & year<=yr[2]) )

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    rvData <- 1
    if (varc!="" | input$Rshow==F) { #| vart!="" | estimateForm!="" | yax=="srci" | yax=="sr"
      pd <- subset( pd, Release=="Current")
      # if (varc!="" | vart!="" | estimateForm!="") {
      #   rvData <- 0
      # } else {
      #   Legend <- "Release\nversion"
      # }
    } else { #if (yax!="srci" & yax!="sr" & input$Rshow) {
      rvData <- 2
      if (vart=="") {
        Legend <- "Version"#\nby version"
      } else {
        Legend <- paste(Legend,"by Version")
      }
    }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO(),Split=Split) #Pd=pd,Yax=yax,
  })

  # Remoteness by jurisdiction, Intent, Sex and Age (Table R) ------------------------------------------
  output$RAPlot <- renderPlotly({
    yr <- input$yr09
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    if (input$dropSI=="Intent") {
      INTENT <- input$codR
      if (INTENT=="AA") {
        INTENT <- c("All","Unintentional")
        Split <- "Intent"
      }
      SEX <- input$sexC
      labt <- "Sex"
    } else if (input$dropSI=="Sex") {
      SEX <- input$sex4R
      if (SEX=="MF") {
        SEX <- c("Female","Male")
        Split <- "Sex"
      }
      INTENT <- input$cod2C
      labt <- "Intent"
    }

    if (input$jurR=="Australia") {
      if (input$dropRA=="Region") {
        AGE <- input$ageAll
        REGION <- input$RAraR
        labc <- "Age"
      } else {
        AGE <- input$ageROA
        REGION <- input$RAra
        labc <- "Region"
      }
    } else {
      AGE <- "All ages" #input$ageR
      REGION <- input$Rra
      labc <- "Region"
    }
    if (yax=="sr" | yax=="srci") AGE <- "All ages"

    Title <- paste0(input$jurR,", Intent:",INTENT)
    if (length(REGION)==1) Title <- paste0(Title,", Region: ",REGION)
    if (length(AGE)==1) Title <- paste0(Title,", Age: ",AGE)
    if (length(SEX)==1) Title <- paste0(Title,", ",SEX)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)

    pd <- subset(COD2020_Rem, jurisdiction==input$jurR & Age %in% AGE &
      Sex %in% SEX & Intent %in% INTENT & Region %in% REGION &
      (year>=yr[1] & year<=yr[2]) )

    # gp <- ggplot(pd) + aes(colour=RegionIntent, linetype=RegionIntent) +
    #     labs(title=paste0(input$jurR,", All ages") ) +
    #     scale_colour_manual(values=RegionIntentcols) +
    #     scale_linetype_manual(values=RegionIntenttype)

    varc <- labc
    # labc <- "Age"
    vart <- labt
    # labt <- "Region"

    if (length(get(toupper(labc)))==1 & length(get(toupper(labt)))==1) {
      vart <- ""
      if (estimateForm!="") {
        varc <- "Age" #dummy for color
        estimateForm <- "Colo"
        Legend <- "Estimate type"
      } else {
        varc <- ""
        Legend <- ""
      }
    } else if (length(get(toupper(labc)))==1) {
      if (estimateForm!="") {
        varc <- "Age" #dummy for color
        estimateForm <- "Colo2"
        Legend <- "Estimate type by Region"
      } else {
        varc <- ""
        Legend <- labt
      }
    } else if (length(get(toupper(labt)))==1) {
      vart <- ""
      Legend <- labc
    } else {
      Legend <- paste(labc,"by",labt)
    }

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    rvData <- 1
    if (varc!="" | input$Rshow==F | SEX[1]!="People" | AGE[1]!="All ages") { #| vart!="" | estimateForm!="" | yax=="srci" | yax=="sr"
      pd <- subset( pd, Release=="Current")
      # if (varc!="" | vart!="" | estimateForm!="") {
      #   rvData <- 0
      # } else {
      #   Legend <- "Release\nversion"
      # }
    } else { #if (yax!="srci" & yax!="sr" & input$Rshow) {
      rvData <- 2
      if (vart=="") {
        Legend <- "Version"
      } else {
        Legend <- paste(Legend,"by Version")
      }
    }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)
    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO(),Split=Split)
  })

  # Remoteness area as percentage (Tables R) ------------------------------------------
  output$RPPlot <- renderPlotly({
    #needs to be sorted [order(...)] & made distinct
    Split <- ""
    Title <- paste0(input$jurR,", Age: ","All ages") #input$ageR
    INTENT <- input$codR
    if (INTENT=="AA") {
      INTENT <- c("All","Unintentional")
      Split <- "Intent"
    } else {
      Title <- paste0(Title,", Intent: ",INTENT)
    }

    pd <- subset(COD2020_Rem,Release=="Current" &
            jurisdiction==input$jurR & Intent %in% INTENT &
            (year>=input$yr09[[1]] & year<=input$yr09[[2]]))
    
    if (input$jurR=="Australia") {
      pd <- subset(pd, Age=="All ages" & Sex==input$sexR) #input$ageR
    } else {
      pd <- subset(pd, Age=="All ages" & Sex=="People") #input$ageR
    }

    if (input$jurR=="Australia" & input$sexR=="People") { # & input$ageR=="All ages"
      pd <- filter(pd, Region!="Regional and Remote") %>%
        group_by(year, Intent, Sex, jurisdiction, Age) %>% 
        # distinct() %>%
        mutate(
          # percent=round(n/sum(n)*100,2),
          # percent=round(n/N*100,4),
          percent=n/N*100,
          Region=factor(Region, levels=c( "Remote and Very Remote",
            "Outer Regional", "Inner Regional", "Major Cities"
          )))
    } else {
      pd <- filter(pd, Region=="Regional and Remote" | Region=="Major Cities" ) %>%
        group_by(year, Intent, Sex, jurisdiction, Age) %>% 
        # distinct() %>%
        mutate(
          # percent=round(n/sum(n)*100,2),
          # percent=round(n/N*100,4),
          percent=n/N*100,
          Region=factor(Region, levels=c( "Regional and Remote",
            "Major Cities"
          )))
    }

    validate(need(nrow(pd) > 0, "No data selected"))
    gp <- ggplot(pd, aes(x=year, y=percent, fill=Region, group=1, text=paste0(
        "Year: ",year," <i>",Note,"</i>",
        "<br>Number of deaths: ",n,
        "<br>Percent: ",format(round(percent,1),nsmall=1), "%",
        "<br>Area: ",Region,
        "<br>Jurisdiction: ",jurisdiction,
        "<br>Age: ",Age,
        "<br>Sex: ",Sex,
        "<br>Intent: ",Intent
      ))) +
      # geom_area() +
      geom_bar(stat="identity") +
      labs(x="Year",y="Percent of drug-induced deaths",title=Title) +
      scale_fill_manual(values=Regioncols,name=NULL) +
      scale_x_continuous(breaks=seq(input$yr09[[1]],input$yr09[[2]],2) )

    gp <- gp + theme_light() + theme(legend.title=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())
    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }

    PlyFn(Gp=gp,Lt="Remoteness area",X=LX(),Y=LY(),O=LO(),Split=Split)
  })

# All drugs by type, jurisdiction, Intent and/or Sex (Table 12, 12b & 12c) ------------------------------------
  output$DTJPlot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    AGE <- "All ages" #input$ageR
    # if (yax=="sr" | yax=="srci") AGE <- "All ages"
    # if (input$DTjur!="Australia") {
      AGE <- "All ages"
      SEX <- "People"
      # INTENT <- input$cod2C
    # }
    # if (input$DTjur=="Australia") {
      if (input$DTJdrop=="IntSx") {
        SEX <- input$DTIsex
        if (input$DTIsex=="MF") {
          SEX <- c("Female","Male")
          Split <- "Sex"
        }
        if (input$DTIsex=="People" | input$DTjur=="Australia") {
          INTENT <- input$DTIcod
        } else {
          INTENT <- "All" #input$DTIcod2
        }
      } else if (input$DTJdrop=="Drug") {
        INTENT <- input$cod4C
        SEX <- input$sexC
      }
    # }
    if (input$DTJdrop=="IntSx") {
      DRUG <- input$DTdrug
    } else if (input$DTJdrop=="Drug") {
      DRUG <- input$DTdrugD
    }

    Title <- paste0(input$DTjur,", Age: ",AGE)
    if (length(SEX)==1) Title <- paste0(Title,", Sex: ",SEX)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)
    if (length(DRUG)==1) Title <- paste0(Title,", Drug involved: ",DRUG)

    pd <- subset(COD2020_DT, Intent %in% INTENT & Drug %in% DRUG
      & Age==AGE & Sex %in% SEX & jurisdiction==input$DTjur
      & (year>=yr[1] & year<=yr[2] ))

    if (input$DTJdrop=="IntSx") {
      labt <- "Intent"
      labc <- "Drug"
      if (length(DRUG)==1) {
        vart <- ""
        varc <- ""
        Legend <- ""
      } else {
        vart <- "Drug"
        varc <- "Drug"
        Legend <- "Drug involved"
      }

      if (input$DTIsex!="People") {
        validate(need(nrow(pd) > 0, "Please select All ages for age range for data by male and/or female."))
      }
    }
    
    if (input$DTJdrop=="Drug") {
      vart <- "Intent"
      labt <- "Intent"
      varc <- "Sex"
      labc <- "Sex"
      Legend <- ""
      if (length(SEX)==1) {
        varc <- ""
        if (estimateForm!="") {
          varc <- "Age"
          estimateForm <- "Colo"
          Legend <- "Estimate type"
        }
      }
      if (input$DTjur=="Australia") {
        if (length(INTENT)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
          vart <- ""
          if (length(INTENT)==2 & input$codS==2) Split <- labt
          if (length(SEX)>1) {
            Legend <- "Sex"
          }
        } else if (length(SEX)>1) {
          Legend <- "Sex by Intent"
        } else if (estimateForm!="") {
          estimateForm <- "Colo2"
          Legend <- "Estimate type by Intent"
        } else {
          Legend <- "Intent"
        }

        if (input$sexC[[1]]!="People") {
          validate(need(nrow(pd) > 0, "Please select All ages for age range for data by male and/or female."))
        }
      }
      if (input$DTjur!="Australia") {
        if (length(INTENT)==1) {
          vart <- ""
          Legend <- ""
        } else {
          Legend <- "Intent"
          if (estimateForm!="") {
            estimateForm <- "Colo2"
            Legend <- "Estimate type by Intent"
          }
        }
      }
    }

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Drug:' to show all drugs involved.)"))
    rvData <- 1
    # if (varc!="" | yax=="srci" | yax=="sr" | input$Rshow==F) { #| vart!="" | estimateForm!=""
    #   pd <- subset( pd, Release=="Current")
    #   # if (varc!="" | vart!="" | estimateForm!="") {
    #   #   rvData <- 0
    #   # } else {
    #   #   Legend <- "Release\nversion"
    #   # }
    # } else { #if (yax!="srci" & yax!="sr" & input$Rshow) {
    #   rvData <- 2
    #   Legend <- "Release date"#
    # }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO(),Split=Split)
  })

  # All drugs by type, Age & Intent (Tables 12 & 12a) ----------------------------------------------------------
  output$DTAPlot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    if (input$DTAdrop=="Drug") {
      AGE <- input$ageAll
      INTENT <- input$cod2C
      DRUG <- input$DTdrugD
      if (length(input$cod2C)==2 & input$codS==2) Split <- "Intent"
    } else if (input$DTAdrop=="Age_Intent") {
      AGE <- input$DTAIage
      INTENT <- input$DTAIcod
      DRUG <- input$DTdrug
    }
    if (yax=="sr" | yax=="srci") AGE <- "All ages"

    Title <- "Australia"
    # if (length(SEX)==1) Title <- paste0(Title,", Sex: ",SEX)
    if (length(AGE)==1) Title <- paste0(Title,", Age: ",AGE)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)
    if (length(DRUG)==1) Title <- paste0(Title,", Drug involved: ",DRUG)

    if (input$DTAdrop=="Age_Intent") {
      labt <- "Intent"
      labc <- "Drug"
      if (length(DRUG)==1) {
        vart <- ""
        varc <- ""
        Legend <- ""
        if (estimateForm!="") {
          varc <- "Age" # dummy for colors with Drug as default varc
          Legend <- "Estimate type"
          estimateForm <- "Colo"
        }
      } else {
        vart <- "Drug"
        varc <- "Drug"
        Legend <- "Drug involved"
      }
    }
    if (input$DTAdrop=="Drug") {
      vart <- "Intent"
      labt <- "Intent"
      varc <- "Age"
      labc <- "Age"
      if (length(AGE)==1 & estimateForm=="") varc <- ""
      if ( length(input$cod2C)==1 | (length(input$cod2C)==2 & input$codS==2)) {
        vart <- ""
        # if (length(input$cod2C)==2) Split <- "Intent"
        if (estimateForm!="") {
          Legend <- "Estimate type"
          estimateForm <- "Colo"
        } else if (length(AGE)==1) {
          Legend <- ""
        } else {
          Legend <- "Age"
        }
      } else if (estimateForm!="") {
          Legend <- "Estimate type by Intent"
          estimateForm <- "Colo2"
      } else if (length(AGE)==1) {
          Legend <- "Intent"
      } else {
        Legend <- "Age by Intent"
      }
    }

    pd <- subset( COD2020_DT, jurisdiction=="Australia" &
        Sex=="People" & Age %in% AGE & Intent %in% INTENT &
        Drug %in% DRUG & #nature=="Underlying" &
        (year>=yr[1] & year<=yr[2]) )
    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Drug:'/'Age:' to show all drugs involved/age.)"))
    rvData <- 1
    # if (varc!="" | yax=="srci" | yax=="sr" | input$Rshow==F) { #| vart!="" | estimateForm!=""
    #   pd <- subset( pd, Release=="Current")
    #   # if (varc!="" | vart!="" | estimateForm!="") {
    #   #   rvData <- 0
    #   # } else {
    #   #   Legend <- "Release\nversion"
    #   # }
    # } else { #if (yax!="srci" & yax!="sr" & input$Rshow) {
    #   rvData <- 2
    #   Legend <- "Release date"
    # }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO(),Split=Split)
  })

  # O4 - by opioid, Age & Intent (Table 4) -----------------------------------------------------------------
  output$O4Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    INTENT <- input$cod4C
    if (input$dropOA=="Drug") {
      AGE <- input$ageAll
      OPIOID <- input$OdrugO
      labc <- "Age"
    } else if (input$dropOA=="Age") {
      AGE <- input$ageROA
      OPIOID <- input$OdrugC
      labc <- "Opioid"
    }
    if (yax=="sr" | yax=="srci") AGE <- "All ages"

    Title <- "Australia"
    # if (length(SEX)==1) Title <- paste0(Title,", Sex: ",SEX)
    if (length(AGE)==1) Title <- paste0(Title,", Age: ",AGE)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)
    if (length(OPIOID)==1) Title <- paste0(Title,", Opioid involved: ",OPIOID)

    pd <- subset(COD2020_Op, subset=(jurisdiction=="Australia" &
      Sex=="People" & Intent %in% INTENT & Age %in% AGE &
      Opioid %in% OPIOID & (year>=yr[1] & year<=yr[2])))

    vart <- "Intent"
    labt <- "Intent"
    varc <- labc
    if (length( get(toupper(labc)) )==1) varc <- ""
    if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
      vart <- ""
      if (length(input$cod4C)==2) Split <- labt
      if (estimateForm!="") {
        if (varc!="Opioid") {
          varc <- "Age" # dummy for colors
          Legend <- "Estimate type"
          estimateForm <- "Colo"
        } else {
          vart <- labt
          Legend <- "Estimate type by Opioid"
          estimateForm <- "Type2"
        }
      } else {
        Legend <- varc
      }
    } else if (estimateForm!="" & varc!="Opioid") {
      varc <- "Age" # dummy for colors
      Legend <- "Estimate type by Intent"
      estimateForm <- "Colo2"
    } else {
      Legend <- paste(labc,"by Intent")
    }
    if (length( get(toupper(labc)) )==1 & estimateForm<"Colo") Legend <- vart

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    if ( input$Ashow==F | yax=="crci" | yax=="srci" | (yax=="sr" & ( input$dropOA=="Age" &
      (length(input$cod4C)>2 | (length(input$cod4C)==2 & input$codS==1) ) )) ) {
      pd <- subset(pd, primary=="Opioid-induced")
    }
    gp <- ggplot(pd) + labs(title=Title)

    rvData <- 1
    if ( input$Ashow==T & (yax=="num" | yax=="cr" | (yax=="sr" & ( input$dropOA=="Drug" |
      length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) )) ) ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(.3,1))
      Legend <- paste0("Death data type by<br>",Legend)
      if (yax=="sr" & estimateForm!="") rvData <- 0
    } # else if (varc=="" & vart=="" & estimateForm=="") {
    #   rvData <- 1
    #   Legend <- "Release\nversion"
    # }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }
    
    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO(),Split=Split)
  })

  # O5 - by opioid, Intent & Sex (Table 5) -----------------------------------------------------------------
  output$O5Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    AGE <- "All ages" #input$ageR
    # if (yax=="sr" | yax=="srci") AGE <- "All ages"
    Split <- ""
    Legend <- ""

    SEX <- input$sexC
    INTENT <- input$cod4C
    OPIOID <- input$OdrugC
    if (input$O5drop=="Opioid") {
      OPIOID <- input$OdrugO
      labt <- "Intent"
      labc <- "Sex"
    } else if (input$O5drop=="Intent") {
      INTENT <- input$cod4R
      labt <- "Sex"
      labc <- "Opioid"
    } else if (input$O5drop=="Sex") {
      SEX <- input$sex4R
      if (input$sex4R=="MF") {
        SEX <- c("Female","Male")
        Split <- "Sex"
      }
      labt <- "Intent"
      labc <- "Opioid"
    }

    Title <- "Australia"
    if (length(SEX)==1) Title <- paste0(Title,", Sex: ",SEX)
    if (length(AGE)==1) Title <- paste0(Title,", Age: ",AGE)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)
    if (length(OPIOID)==1) Title <- paste0(Title,", Opioid involved: ",OPIOID)

    pd <- subset(COD2020_Op, Age==AGE & 
      Opioid %in% OPIOID & Intent %in% INTENT & Sex %in% SEX &
      jurisdiction=="Australia" & (year>=yr[1] & year<=yr[2]))

    vart <- labt
    varc <- labc
    if (length(get(toupper(varc)))==1) {
      if (estimateForm!="") {
        varc <- "Age" # dummy for colors
        estimateForm <- "Colo2"
        Legend <- paste("Estimate type by",vart)
      } else {
        varc <- ""
        Legend <- vart
      }
    }
    if (input$O5drop!="Intent") {
      if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2 & Split=="") ) {
        if (length(input$cod4C)==2) Split <- labt
        if (varc==labc & estimateForm!="") {
            estimateForm <- "Type2"
            Legend <- paste("Estimate type by",varc)
        } else {
          vart <- ""
          if (estimateForm!="" & varc=="Age") {
            estimateForm <- "Colo"
            Legend <- "Estimate type"
          } else {
            Legend <- varc
          }
        }
      } else if (varc==labc) {
        Legend <- paste(varc,"by",vart)
      }
    } else if (input$O5drop=="Intent") {
      # vart <- "Sex"
      # varc <- "Opioid"
      if (length(SEX)==1) {
        if (varc==labc & estimateForm!="") {
            vart <- "Intent" # dummy for linetype
            estimateForm <- "Type2"
            Legend <- paste("Estimate type by",varc)
        } else {
          vart <- ""
          if (estimateForm!="" & varc=="Age") {
            estimateForm <- "Colo"
            Legend <- "Estimate type"
          } else {
            Legend <- varc
          }
        }
      } else if (varc==labc) {
        Legend <- paste(varc,"by",vart)
      }
    }

    validate(need(nrow(pd) > 0, "No data selected"))
    # rvData <- 0
    # if (varc=="" & vart=="" & estimateForm=="") {
    #   rvData <- 1
    #   Legend <- "Release\nversion"
    # }
    if ( input$Ashow==F | yax=="crci" | yax=="srci" | (yax=="sr" & ( (input$O5drop!='Intent' & (length(input$cod4C)>2 | (length(input$cod4C)==2 & input$codS==1))) + 
            (input$O5drop!='Sex' & length(input$sexC)>1) + (input$O5drop!='Opioid' & length(input$OdrugC)>1) )>1)) {
      pd <- subset(pd, primary=="Opioid-induced")
    }
    gp <- ggplot(pd) + labs(title=Title)

    rvData <- 1
    if ( input$Ashow==T & (yax=="num" | yax=="cr" | (yax=="sr" & ( (input$O5drop!='Intent' & (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2))) | 
            (input$O5drop!='Sex' & length(input$sexC)==1) | (input$O5drop!='Opioid' & length(input$OdrugC)==1) ))) ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(.3,1))
      Legend <- paste0("Death data type by<br>",Legend)
      if (yax=="sr" & estimateForm!="") rvData <- 0
    } # else if (varc=="" & vart=="" & estimateForm=="") {
    #   rvData <- 1
    #   Legend <- "Release\nversion"
    # }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    # gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO(),Split=Split)
  })
  
  # O6 - by jurisdiction, Sex & Intent (Table 6) -------------------------------------------
  output$O6Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    AGE <- "All ages" #input$ageR
    # if (yax=="sr" | yax=="srci") AGE <- "All ages"
    Split <- ""

    pd <- subset(COD2020_Op, Age==AGE & Opioid=="All opioids" &
        jurisdiction==input$jur & Intent %in% input$cod2C & Sex %in% input$sexC &
        (year>=yr[1] & year<=yr[2]))

    # gp <- ggplot(pd) +
    #   aes(colour=SexIntent, linetype=SexIntent) +
    #   labs(title=paste0(input$jur,", Age: ",AGE) ) +
    #   scale_colour_manual(values=SexIntentcols) +
    #   scale_linetype_manual(values=SexIntenttype)

    Title <- paste0(input$jur,", Age: ",AGE)
    vart <- "Intent"
    labt <- "Intent"
    varc <- "Sex"
    labc <- "Sex"
    Legend <- "Sex by Intent"
    if (length(input$sexC)==1) {
      Title <- paste0(Title,", Sex: ",input$sexC)
      if (estimateForm!="") {
        estimateForm <- "Colo2"
        Legend <- "Estimate type by Intent"
      } else {
        varc <- ""
        Legend <- vart
      }
    }
    if (length(input$cod2C)==1) {
      Title <- paste0(Title,", Intent: ",input$cod2C)
      if (estimateForm!="") {
        estimateForm <- "Type2"
        Legend <- "Estimate type by Sex"
      } else {
        vart <- ""
        Legend <- varc
      }
    }
    if (vart=="" & varc=="" & estimateForm!="") {
      varc <- "Age" #dummy for color
      estimateForm <- "Colo"
      Legend <- "Estimate type"
    }

    validate(need(nrow(pd) > 0, "No data selected"))
    if ( input$Ashow==F | yax=="crci" | yax=="srci" | (yax=="sr" &  length(input$sexC)>1 & length(input$cod2C)>1) ) {
      pd <- subset(pd, primary=="Opioid-induced")
    }
    gp <- ggplot(pd) + labs(title=Title)

    rvData <- 1
    if ( input$Ashow==T & (yax=="num" | yax=="cr" | (yax=="sr" & (length(input$sexC)==1 | length(input$cod2C)==1))) ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(.3,1))
      Legend <- paste0("Death data type by<br>",Legend)
      if (yax=="sr" & estimateForm!="") rvData <- 0
    } # else if (varc=="" & vart=="" & estimateForm=="") {
    #   rvData <- 1
    #   Legend <- "Release\nversion"
    # }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    # gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO())
  })
  
  # Opioids with other drugs by Age & Intent (Table 7) ----------------------------------------------------------
  output$W7Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    INTENT <- input$cod4C
    if (input$dropOA=="Drug") {
      AGE <- input$ageAll
      DRUG <- input$W7Ddrug
      labc <- "Age"
    } else if (input$dropOA=="Age") {
      AGE <- input$ageROA
      DRUG <- input$Wdrug
      labc <- "Drug"
    }
    if (yax=="sr" | yax=="srci") AGE <- "All ages"

    Title <- "Australia"
    # if (length(SEX)==1) Title <- paste0(Title,", Sex: ",SEX)
    if (length(AGE)==1) Title <- paste0(Title,", Age: ",AGE)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)
    if (length(DRUG)==1) Title <- paste0(Title,", Drug involved: ",DRUG)

    pd <- subset(COD2020_OpW, subset=(#jurisdiction=="Australia" &
      Sex=="People" & Intent %in% INTENT & Age %in% AGE &
      Drug %in% DRUG & (year>=yr[1] & year<=yr[2])))

    vart <- "Intent"
    labt <- "Intent"
    varc <- labc
    if (length( get(toupper(labc)) )==1) varc <- ""
    if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2) ) {
      vart <- ""
      if (length(input$cod4C)==2) Split <- labt
      if (estimateForm!="") {
        if (varc!="Drug") {
          varc <- "Age" # dummy for colors
          Legend <- "Estimate type"
          estimateForm <- "Colo"
        } else {
          vart <- labt
          Legend <- "Estimate type by Drug"
          estimateForm <- "Type2"
        }
      } else {
        Legend <- varc
      }
    } else if (estimateForm!="" & varc!="Drug") {
      varc <- "Age" # dummy for colors
      Legend <- "Estimate type by Intent"
      estimateForm <- "Colo2"
    } else {
      Legend <- paste(labc,"by Intent")
    }
    if (length(get(toupper(labc)))==1 & estimateForm<"Colo") Legend <- vart

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    if ( input$Ashow==F | yax=="crci" | yax=="srci" | (yax=="sr" & ( input$dropOA=="Age" &
      (length(input$cod4C)>2 | (length(input$cod4C)==2 & input$codS==1)) )) ) {
      pd <- subset(pd, primary=="Opioid-induced")
    }
    gp <- ggplot(pd) + labs(title=Title)

    rvData <- 1
    if ( input$Ashow==T & (yax=="num" | yax=="cr" | ( yax=="sr" & (input$dropOA=="Drug" |
      length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2)) )) ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(.3,1))
      Legend <- paste0("Death data type by<br>",Legend)
      if (yax=="sr" & estimateForm!="") rvData <- 0
    } # else if (varc=="" & vart=="" & estimateForm=="") {
    #   rvData <- 1
    #   Legend <- "Release\nversion"
    # }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO(),Split=Split)
  })
  
  # Opioids and other drugs by Sex (Table 8) ------------------------------------------
  output$W8Plot <- renderPlotly({
    yr <- input$yr97
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""
    AGE <- "All ages" #input$ageR
    # if (yax=="sr" | yax=="srci") AGE <- "All ages"

    if (input$dropSI=="Sex") {
      INTENT <- input$cod4C
      SEX <- input$sex4R
      if (input$sex4R=="MF") {
        SEX <- c("Female","Male")
        Split <- "Sex"
      }
      labt <- "Intent"
    } else if (input$dropSI=="Intent") {
      INTENT <- input$cod4R
      SEX <- input$sexC
      labt <- "Sex"
    }

    Title <- paste0("Australia, Age: ",AGE)
    if (length(SEX)==1) Title <- paste0(Title,", Sex: ",SEX)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)
    if (length(input$Wdrug)==1) Title <- paste0(Title,", Drug involved: ",input$Wdrug)

    pd <- subset(COD2020_OpW, Drug %in% input$Wdrug &
      Age==AGE & Intent %in% INTENT & Sex %in% SEX &
      (year>=yr[1] & year<=yr[2]) )

    varc <- "Drug"
    labc <- "Drug"
    Legend <- paste(labc,"by",labt)
    if (length(input$Wdrug)==1) {
      varc <- ""
      Legend <- labt
      if (estimateForm!="") {
        varc <- "Age" # dummy for colors with Drug as default varc
        # if (vart=="") {
        #   Legend <- paste("Estimate type")
        #   estimateForm <- "Colo"
        # } else {
          Legend <- paste("Estimate type by",labt)
          estimateForm <- "Colo2"
        # }
      }
    }
    if (input$dropSI=="Sex") {
      vart <- "Intent"
      if (length(input$cod4C)==1 | (length(input$cod4C)==2 & input$codS==2 & Split=="") ) {
        if (estimateForm!="") {
          if (varc=="Drug") {
            Legend <- "Estimate type by Drug"
            estimateForm <- "Type2"
          } else {
            vart <- ""
            Legend <- paste("Estimate type")
            estimateForm <- "Colo"
          }
        } else {
          vart <- ""
          Legend <- varc
        }
        if (length(input$cod4C)==2) Split <- labt
      }
    } else if (input$dropSI=="Intent") {
      vart <- "Sex"
      if (length(SEX)==1) {
        if (estimateForm!="") {
          if (varc=="Drug") {
            Legend <- "Estimate type by Drug"
            estimateForm <- "Type2"
          } else {
            vart <- ""
            Legend <- paste("Estimate type")
            estimateForm <- "Colo"
          }
        } else {
          vart <- ""
          Legend <- varc
        }
      }
    }

    validate(need(nrow(pd) > 0, "No data selected"))
    if ( input$Ashow==F | yax=="crci" | yax=="srci" | (yax=="sr" & estimateForm=="Alph") ) {
      pd <- subset(pd, primary=="Opioid-induced")
    }
    gp <- ggplot(pd) + labs(title=Title)

    rvData <- 1
    if ( input$Ashow==T & (yax=="num" | yax=="cr" | (yax=="sr" & estimateForm!="Alph")) ) {
      gp <- gp + aes(alpha=primary) +
        scale_alpha_manual(values=c(.3,1),name=NULL)
      Legend <- paste0("Death data type by<br>",Legend)
      if (yax=="sr" & estimateForm!="") rvData <- 0
    } # else if (varc=="" & vart=="" & estimateForm=="") {
    #   rvData <- 1
    #   Legend <- "Release\nversion"
    # }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm,RVData=rvData)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO(),Split=Split)
  })

  # Exclusive Opioids by Age and Intent (Table 10) --------------------------------------------------------------
  output$E0Plot <- renderPlotly({
    yr <- input$yr07
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    Split <- ""

    if (input$dropOA=="Drug") {
      AGE <- input$ageAll
      OPIOID <- input$E0Odrug
      labc <- "Age"
    } else if (input$dropOA=="Age") {
      AGE <- input$ageROA
      OPIOID <- input$Edrug
      labc <- "Opioid"
    }
    if (yax=="sr" | yax=="srci") AGE <- "All ages"

    Title <- paste0("Australia, People")
    if (length(OPIOID)==1) Title <- paste0(Title,", Opioid involved: ",OPIOID)
    if (length(AGE)==1) Title <- paste0(Title,", Age: ",AGE)
    if (length(input$cod3C)==1) Title <- paste0(Title,", Intent: ",input$cod3C)

    pd <- filter(COD2020_OpE, Sex=="People" & jurisdiction=="Australia" &
        Opioid %in% OPIOID & Intent %in% input$cod3C & Age %in% AGE &
        (year>=yr[1] & year<=yr[2]))
      
    vart <- "Intent"
    labt <- "Intent"
    if (length(input$cod3C)==1 | (length(input$cod3C)==2 & input$codS==2) ) {
      vart <- ""
      if (length(input$cod3C)==2) Split <- labt
      Legend <- labc
    }
    # if (length(input$cod3C)==1) vart <- ""
    if (input$dropOA=="Drug") {
      varc <- "Age"
      # labc <- "Age"
      # AGE <- input$ageAll
      # if (yax=="sr" | yax=="srci") AGE <- "All ages"
      if (estimateForm!="") {
        if (vart=="") {
          Legend <- paste("Estimate type")
          estimateForm <- "Colo"
        } else {
          Legend <- paste("Estimate type by",labt)
          estimateForm <- "Colo2"
        }
      } else if (length(AGE)==1) {
        varc <- ""
        Legend <- vart
      } else {
        Legend <- "Age by Intent"
      }
    } else if (input$dropOA=="Age") {
      varc <- "Opioid"
      # labc <- "Opioid"
      # AGE <- input$ageROA
      # if (yax=="sr" | yax=="srci") AGE <- "All ages"
      if (length(OPIOID)==1) {
        if (estimateForm!="") {
          varc <- "Age" # dummy for colors with Opioid as default varc
          if (vart=="") {
            Legend <- paste("Estimate type")
            estimateForm <- "Colo"
          } else {
            Legend <- paste("Estimate type by",labt)
            estimateForm <- "Colo2"
          }
        } else {
          varc <- ""
          Legend <- vart
        }
      } else {
        Legend <- "Opioid by Intent"
      }
    }

    # rvData <- 0
    # if (varc=="" & vart=="" & estimateForm=="") {
    #   rvData <- 1
    #   Legend <- "Release\nversion"
    # }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    validate(need(nrow(pd) > 0, "No data selected. (Please interact again with the control panel for plot to show if you have ticked 'Age:' to show all age groups.)"))
    gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO(),Split=Split)
  })

  # Exclusive opioids by jurisdiction, Intent and Sex (Table 9 & 11) -----------------------
  output$E9Plot <- renderPlotly({
    yr <- input$yr07
    yax <- input$yax
    estimateForm <- "Alph"
    if (input$crude==F | yax!="sr") estimateForm <- ""
    AGE <- "All ages" #input$ageR
    # if (yax=="sr" | yax=="srci") AGE <- "All ages"
    Split <- ""

    # OPIOID <- input$Edrug
    if (input$dropSI=="Intent") {
      INTENT <- input$E9Icod
      SEX <- input$sexC
    } else if (input$dropSI=="Sex") {
      INTENT <- input$cod3C
      SEX <- input$sex4R
      if (input$sex4R=="MF") {
        SEX <- c("Female","Male")
        Split <- "Sex"
      }
    }

    Title <- paste0(input$jur,", Age: ",AGE)
    if (length(SEX)==1) Title <- paste0(Title,", Sex: ",SEX)
    if (length(INTENT)==1) Title <- paste0(Title,", Intent: ",INTENT)
    
    pd <- filter(COD2020_OpE, jurisdiction==input$jur & Age==AGE &
        Sex %in% SEX & Intent %in% INTENT & Opioid %in% input$Edrug & 
          (year>=yr[1] & year<=yr[2]) )

    varc <- "Opioid"
    labc <- "Opioid"
    if (input$dropSI=="Sex") {
      vart <- "Intent"
      labt <- "Intent"
      Legend <- "Opioid by Intent"
      if (length(INTENT)==1 | (length(input$cod3C)==2 & input$codS==2 & Split=="") ) {
        vart <- ""
        if (length(input$cod3C)==2) Split <- labt
        Legend <- labc
      }
    } else if (input$dropSI=="Intent") {
      vart <- "Sex"
      labt <- "Sex"
      Legend <- "Opioid by Sex"
      if (length(SEX)==1) {
        vart <- ""
        Legend <- labc
      }
    }
    if (length(input$Edrug)==1) {
      varc <- ""
      Legend <- vart
      if (estimateForm!="") {
        varc <- "Age" # dummy for colors with Opioid as default varc
        if (vart=="") {
          Legend <- paste("Estimate type")
          estimateForm <- "Colo"
        } else {
          Legend <- paste("Estimate type by",labt)
          estimateForm <- "Colo2"
        }
      }
    }

    # rvData <- 0
    # if (varc=="" & vart=="" & estimateForm=="") {
    #   rvData <- 1
    #   Legend <- "Release\nversion"
    # }
    if (estimateForm=="Alph") Legend <- paste0(Legend,"<br>by Estimate type")

    validate(need(nrow(pd) > 0, "No data selected"))
    gp <- ggplot(pd) + labs(title=Title)
    gp <- ColtypFn(Pd=pd,Gp=gp,Yax=yax,Varc=varc,Vart=vart,Split=Split,EstForm=estimateForm)
    gp <- PlotFn(Pd=pd,Gp=gp,Yax=yax,Yr=yr,Labt=labt,Labc=labc,EstForm=estimateForm)

    if (Split!="") {
      pageDim <- input$dimension
      gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
        if (pageDim<1200) {
          LO("h")
          LX(0)
          LY(-.2)
        }
    }

    PlyFn(Gp=gp,Lt=Legend,X=LX(),Y=LY(),O=LO(),Split=Split)
  })
  
  # # Exclusive opioids as percents ------------------------------------------
  # output$EPPlot <- renderPlotly({
  #   Split <- ""
  #   INTENT <- input$codR
  #   if (INTENT=="AA") {
  #     INTENT <- c("All","Unintentional")
  #     Split <- "Intent"
  #   }
  #   #needs to be sorted [order(...)]
  #   #weird proportions plot from 2015 onwards because of duplicates by AUS
  #   #- need to make distinct
  #   pd <- filter(COD2020_OpE,
  #       # Opioid %in%
  #       #   c("Exclusive heroin",
  #       #     # "Exclusive pharmaceutical opioids",
  #       #     "Exclusive other opioids",
  #       #     # "Illicit & pharmaceutical opioids",
  #       #     "Heroin & other opioids",
  #       #     # "Other & unspecified opioids"
  #       #     "Opium & unspecified opioids") &
  #       Intent %in% INTENT & Age=="All ages" & #input$ageR
  #       (year>=input$yr07[[1]] & year<=input$yr07[[2]]) & 
  #       Sex==input$sexR & jurisdiction==input$jur) %>%
  #     group_by(year, Intent, Sex, jurisdiction, Age) %>% 
  #     # distinct() %>%
  #     mutate(
  #       # percent=round(n/sum(n)*100,2),
  #       # percent=round(n/N*100,4),
  #       percent=n/N*100,
  #       Opioid=factor(Opioid, levels=c("Opium & unspecified opioids",
  #                                  "Heroin & other opioids",
  #                                  "Exclusive other opioids",
  #                                  "Exclusive heroin")))
  # 
  #   validate(need(nrow(pd) > 0, "No data selected"))
  #   gp <- ggplot(pd, aes(x=year, y=percent, fill=Opioid, group=1, text=paste0(
  #       "Year: ",year," <i>",Note,"</i>",
  #       "<br>Number of deaths: ",n,
  #       "<br>Percent: ",format(round(percent,1),nsmall=1), "%",
  #       "<br>Opioid: ",Opioid,
  #       "<br>Intent: ",Intent,
  #       "<br>Sex: ",Sex))) +
  #     # geom_area() +
  #     geom_bar(stat="identity") +
  #     labs(x="Year", y="Percent of opioid induced deaths") +
  #     scale_x_continuous(breaks=seq(input$yr07[[1]],input$yr07[[2]],2) ) +
  #     scale_fill_manual(values=Opioidcols,name=NULL)
  #   
  #   
  #   gp <- gp + theme_light() + theme(legend.title=element_blank(),
  #       panel.grid.minor.x=element_blank(),
  #       panel.grid.major.x=element_blank())
  #   if (Split!="") {
  #     pageDim <- input$dimension
  #     gp <- SplitFn(Pd=pd,Gp=gp,Vars=Split,PageDim=pageDim)
  #       if (pageDim<1200) {
  #         LO("h")
  #         LX(0)
  #         LY(-.2)
  #       }
  #   }
  # 
  #   PlyFn(Gp=gp,Lt="Opioid",X=LX(),Y=LY(),O=LO(),Split=Split)
  # })

}
