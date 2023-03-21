options(shiny.maxRequestSize=100*1024^2)

library(shiny)
library(shinydashboard)
library(scales)
library(tidyverse)
library(tidyr)
library(janitor)
library(lubridate)
library(glue)
library(forcats)
library(GGally)
library(plotly)
# library(broom.helpers)
library(DT)
library(skimr)
# library(summarytools)
library(reactable)
library(reactablefmtr)
library(nnet)

df_example <- read_csv("example.csv")

# fix the styles, will over-ride later
# see https://rstudio.github.io/shinydashboard/appearance.html#statuses-and-colors
input_element_color <- "primary" 
highlight_color <- "olive" 
regular_color <- "navy"
# actual colors used:
# uw madison red #da004c
# darker red #8b0037

header <- dashboardHeader(
  title = "DairyCoPilot",
  tags$li(a(href = "https://github.com/walleser/dairy-comp",
            img(
              # src = "https://hr.wisc.edu/wp-content/uploads/2019/04/uw-crest-red-300x300.png",
              src = "download (1).jpg",
              title = "DairyCoPilot", height = "50px"
            ),
            style = "padding-top:0px; padding-bottom:0px;"),
          class = "dropdown")
)

sidebar <- dashboardSidebar(
  tags$style("@import url(https://use.fontawesome.com/releases/v5.14.0/css/all.css);"),
  sidebarMenu(
    id = "sidebar",
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Pivot Table", tabName = "pivot", icon = icon("table")),
    menuItem("Regression Analysis", tabName = "regression", icon = icon("chart-line")),
    menuItem("GitHub Repo", icon = icon("file-code-o"), 
             href = "https://github.com/walleser/dairy-comp"),
    menuItem("Documentation", tabName = "documentation", icon = icon("book"))
  )
)

body <- dashboardBody(
  tags$style("@import url(https://use.fontawesome.com/releases/v5.14.0/css/all.css);"),
  # shinydashboard has fixed colors mapped to their CSS classes.  We over-ride those 
  # to produce a custom color palette.  Some details can be found here: https://stackoverflow.com/questions/36080529/r-shinydashboard-customize-box-status-color
  tags$style(HTML(
    "
    .box.box-solid.box-primary>.box-header {
    color:#fff;
    background:#646569
    }
    
    .box.box-solid.box-primary{
    border-bottom-color:#646569;
    border-left-color:#646569;
    border-right-color:#646569;
    border-top-color:#646569;
    }
    
    .bg-olive {
    background-color: #8F0000!important;
    }
    
    .bg-navy {
    background-color: #8F0000!important;
    }
    
    .skin-blue .main-header .navbar { background-color: #EB2317!important; }
    
    .skin-blue .main-header .logo { background-color: #9B0000; }
    .skin-blue .main-header .logo:hover { background-color: #8F0000; }
    .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
    border-left-color: #8F0000; 
    }
    .skin-blue .main-header .navbar .sidebar-toggle:hover{ background-color:#9B0000; }
    .skin-blue .main-header .navbar .dropdown-menu li a:hover{ background:#8F0000; }
    "
  )),
  tags$style(HTML(
    "
    .small-box h3 {
      font-size: 28px;
    }
    
    .small-box .icon-large {
      font-size: 55px;
    }
    "
  )),
  tabItems(
    ## Dashboard ---------------------------------------------------------------
    tabItem(
      tabName = "dashboard",
      ## Inputs ----------------------------------------------------------------
      column(
        width = 2,
        ## Input
        box(title = "Input", width = NULL, solidHeader = TRUE, status = input_element_color,
            collapsible = TRUE, collapsed = FALSE,
            # Input: Farm Name ----
            textInput("farm_name", "Farm Name",
                      # value = "",
                      value = "DairyCoPilot",
                      placeholder = "DairyCoPilot"),
            
            # Input: DairyComp Extraction Date ----
            dateInput("dairy_comp_extraction_date", "DairyComp Extraction Date",
                      value = as.character(lubridate::today()), 
                      format = "mm/dd/yy"),
            
            # Input: Earliest Fresh Date ----
            dateInput("earliest_fresh_date", "Earliest Fresh Date for Analysis",
                      value = as.character(lubridate::today() - lubridate::period(num = 1, units = "year")), 
                      format = "mm/dd/yy"),
            
            # Input: Latest Fresh Date ----
            dateInput("latest_fresh_date", "Latest Fresh Date for Analysis",
                      value = as.character(lubridate::today()), 
                      format = "mm/dd/yy"),
            
            # Horizontal line ----
            tags$hr(),
            
            selectInput("type1", "Choose Data Type:",
                        c("Raw" = "raw",
                          "Clean" = "clean")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # # Horizontal line ----
            # tags$hr(),
            # 
            # # Input: Checkbox if file has header ----
            # checkboxInput("header", "Header", TRUE),
            # 
            # # Input: Select separator ----
            # radioButtons("sep", "Separator",
            #              choices = c(Comma = ",",
            #                          Semicolon = ";",
            #                          Tab = "\t"),
            #              selected = ","),
            # 
            # # Input: Select quotes ----
            # radioButtons("quote", "Quote",
            #              choices = c(None = "",
            #                          "Double Quote" = '"',
            #                          "Single Quote" = "'"),
            #              selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Output: Generate file ----
            downloadButton("downloadData", "Generate CSV File"),
            
            # Horizontal line ----
            tags$hr()
            
        )
        # ,
        # 
        # box(title = "Advanced Input", width = NULL, solidHeader = TRUE, status = input_element_color,
        #     collapsible = TRUE, collapsed = TRUE,
        #     
        #     # Input: Start Date ------------------------------------------------
        #     dateInput("start_date", "Start Date",
        #               value = as.character(lubridate::today() - lubridate::period(num = 1, units = "year")), 
        #               format = "mm/dd/yy"),
        #     # Input: End Date --------------------------------------------------
        #     dateInput("end_date", "End Date",
        #               value = as.character(lubridate::today()), 
        #               format = "mm/dd/yy"),
        #     # Horizontal line ----
        #     tags$hr()
        #     
        # )
      ),
      
      ## Outputs ---------------------------------------------------------------
      column(width = 8,
             ## Contents
             box(title = "Contents", width = NULL, solidHeader = TRUE, status = input_element_color,
                 collapsible = TRUE, collapsed = FALSE,
                 
                 # Output: Contents ----
                 DT::dataTableOutput("contents")
                 
             ),
             
             ## Summary
             box(title = "Summary", width = NULL, solidHeader = TRUE, status = input_element_color,
                 collapsible = TRUE, collapsed = FALSE,
                 
                 # Output: Summary ----
                 verbatimTextOutput("df_summary")
                 # DT::dataTableOutput("df_summary")
                 
                 # htmlOutput("df_summary")
                 
             )
      )
      
    ),
    
    ## Regression Analysis -----------------------------------------------------
    tabItem(
      tabName = "regression",
      ## Inputs ----------------------------------------------------------------
      column(
        width = 2,
        ## Input
        box(title = "Input", width = NULL, solidHeader = TRUE, status = input_element_color,
            collapsible = TRUE, collapsed = FALSE,
            
            # Input: Response Variable -----------------------------------------
            uiOutput("response_ui"),
            # Input: Explanatory Variable(s) -----------------------------------
            uiOutput("explanatory_ui"),
            
            # Horizontal line ----
            tags$hr()
            
        )
      ),
      
      ## Outputs ---------------------------------------------------------------
      column(
        width = 4,
        ## Output
        box(title = "Summary", width = NULL, solidHeader = TRUE, status = input_element_color,
            collapsible = TRUE, collapsed = FALSE,
            
            tableOutput("summary")
            
        ),
        
        ## Output
        box(title = "ANOVA", width = NULL, solidHeader = TRUE, status = input_element_color,
            collapsible = TRUE, collapsed = FALSE,
            
            tableOutput("anova")
            
        )
      ),
      
      ## Plots -----------------------------------------------------------------
      column(
        width = 6,
        ## Plot
        box(title = "Generalized Pairs Plot", width = NULL, solidHeader = TRUE, status = input_element_color,
            collapsible = TRUE, collapsed = FALSE,
            
            plotlyOutput("plot1")
            
        ),
        
        box(title = "Coefficient Plot", width = NULL, solidHeader = TRUE, status = input_element_color,
            collapsible = TRUE, collapsed = FALSE,

            plotlyOutput("plot2")

        )
        
      )
      
    ),
    
    ## Pivot Table -------------------------------------------------------------
    tabItem(
      tabName = "pivot",
      ## Inputs ----------------------------------------------------------------
      column(
        width = 2,
        ## Input
        box(title = "Input", width = NULL, solidHeader = TRUE, status = input_element_color,
            collapsible = TRUE, collapsed = FALSE,
            
            # Input: Row Variable ----------------------------------------------
            uiOutput("row_ui"),
            uiOutput("ref_row_ui"),
            # Input: Column Variable -------------------------------------------
            uiOutput("column_ui"),
            uiOutput("ref_column_ui"),
            
            # Horizontal line ----
            tags$hr()
            
        ),
        
        box(title = "Advanced Input", width = NULL, solidHeader = TRUE, status = input_element_color,
            collapsible = TRUE, collapsed = TRUE,
            
            # Input: Confidence Level ------------------------------------------
            numericInput("conf_level", "Confidence Level", 95, min = 1, max = 100, step = 1),
            
            # Horizontal line ----
            tags$hr()
            
        )
      ),
      
      ## Outputs ---------------------------------------------------------------
      column(
        width = 4,
        ## Input
        box(title = "Pivot Table", width = NULL, solidHeader = TRUE, status = input_element_color,
            collapsible = TRUE, collapsed = FALSE,
            
            # tableOutput("pivot_table")
            reactableOutput("pivot_table")
            
        ),

        ## Input
        box(title = "Odds Ratio", width = NULL, solidHeader = TRUE, status = input_element_color,
            collapsible = TRUE, collapsed = FALSE,

            # tableOutput("odds_ratio")
            reactableOutput("odds_ratio")
            

        )
      ),

      ## Plots -----------------------------------------------------------------
      column(
        width = 6,
        ## Plot
        box(title = "Comparative Bar Plot", width = NULL, solidHeader = TRUE, status = input_element_color,
            collapsible = TRUE, collapsed = FALSE,

            plotlyOutput("plot3")

        ),

        box(title = "Forest Plot", width = NULL, solidHeader = TRUE, status = input_element_color,
            collapsible = TRUE, collapsed = FALSE,

            plotlyOutput("plot4")

        )

      )
      
    ),
    
    ## Documentation -----------------------------------------------------------
    tabItem(
      tabName = "documentation",
      h2("Methodology"),
      div("This documentation provides instructions to generate a CSV file for analysis using DairyCoPilot. Please refer to the ", a("DC305 reference guide", href = "https://dc-help.vas.com/ReferenceGuide/Home-DC305RefGuide.htm"), " for additional information regarding DairyCOMP305 structure and usage."),
      
      tags$ol(
        tags$li("Open DairyCOMP305 desktop application. Note the last date when records were updated. If a backup copy is used from another dairy, this may not be the current date."),
        tags$li("Confirm all necessary items are created in ALTER (ALTER is accessed under File dropdown menu). Note: changes made in ALTER affect entire program."),
        tags$ol(
          tags$li("Users can run the command and DairyComp will show a notfication warning the item does not exist."),
          tags$li("Users can search the items directly in ALTER.")
        ),
        tags$li("Add missing items to Items menu."),
        tags$ol(
          tags$li("Click Add [Ins]."),
          tags$li("Enter the following information for each item not in dairy records."),
          tags$table(
            border = 1, 
            tags$tbody(
              tags$tr(
                tags$td(align = "left", strong("Item Name")),
                tags$td(align = "left", strong("Item Type")),
                tags$td(align = "left", strong("Loc/Op1")),
                tags$td(align = "left", strong("Len/Op2")),
                tags$td(align = "left", strong("Item Description"))
              ),
              
              tags$tr(
                tags$td(align = "left", "ID"),
                tags$td(align = "left", "32"),
                tags$td(align = "left", "210"),
                tags$td(align = "left", "4"),
                tags$td(align = "left", "Animal's identification")
              ),
              
              tags$tr(
                tags$td(align = "left", "LACT"),
                tags$td(align = "left", "1"),
                tags$td(align = "left", "41"),
                tags$td(align = "left", "1"),
                tags$td(align = "left", "Lactation number")
              ),
              
              tags$tr(
                tags$td(align = "left", "FDAT"),
                tags$td(align = "left", "18"),
                tags$td(align = "left", "71"),
                tags$td(align = "left", "2"),
                tags$td(align = "left", "Fresh date that initiated this lactation")
              ),
              
              tags$tr(
                tags$td(align = "left", "BDAT"),
                tags$td(align = "left", "18"),
                tags$td(align = "left", "37"),
                tags$td(align = "left", "2"),
                tags$td(align = "left", "Birth date of the animal")
              ),
              
              tags$tr(
                tags$td(align = "left", "RC"),
                tags$td(align = "left", "3"),
                tags$td(align = "left", "43"),
                tags$td(align = "left", "1"),
                tags$td(align = "left", "Reproductive code (1 through 8)")
              ),
              
              tags$tr(
                tags$td(align = "left", "DRYLG"),
                tags$td(align = "left", "8"),
                tags$td(align = "left", "181"),
                tags$td(align = "left", ""),
                tags$td(align = "left", "Dry log SCC")
              ),
              
              tags$tr(
                tags$td(align = "left", "DDRY"),
                tags$td(align = "left", "49"),
                tags$td(align = "left", "FDAT"),
                tags$td(align = "left", "PDDAT"),
                tags$td(align = "left", "Days dry (prior to the current calving)")
              ),
              
              tags$tr(
                tags$td(align = "left", "PDCC"),
                tags$td(align = "left", "49"),
                tags$td(align = "left", "FDAT"),
                tags$td(align = "left", "PCDAT"),
                tags$td(align = "left", "Previous days carried calf (gestation length)")
              ),
              
              tags$tr(
                tags$td(align = "left", "PDIM"),
                tags$td(align = "left", "95"),
                tags$td(align = "left", "DIM"),
                tags$td(align = "left", "-1"),
                tags$td(align = "left", "Previous lactation days in milk (lactation length)")
              ),
              
              tags$tr(
                tags$td(align = "left", "EASE"),
                tags$td(align = "left", "1"),
                tags$td(align = "left", "114"),
                tags$td(align = "left", "1"),
                tags$td(align = "left", "Calving ease score, 1 to 5 scale")
              ),
              
              tags$tr(
                tags$td(align = "left", "CNUM"),
                tags$td(align = "left", "76"),
                tags$td(align = "left", "CALF"),
                tags$td(align = "left", "(use 0)"),
                tags$td(align = "left", "Number of calves born to start the current lactation")
              ),
              
              tags$tr(
                tags$td(align = "left", "CLIVE"),
                tags$td(align = "left", "73"),
                tags$td(align = "left", "FRESH"),
                tags$td(align = "left", "(Esc) 4"),
                tags$td(align = "left", "Calf alive or dead (A = alive, D = dead)")
              ),
              
              tags$tr(
                tags$td(align = "left", "CSEX"),
                tags$td(align = "left", "73"),
                tags$td(align = "left", "FRESH"),
                tags$td(align = "left", "(Esc) 1"),
                tags$td(align = "left", "Calf sex (M, F, MM, FF, FM, etc.)")
              ),
              
              tags$tr(
                tags$td(align = "left", "FTDIM"),
                tags$td(align = "left", "81"),
                tags$td(align = "left", "1"),
                tags$td(align = "left", "0"),
                tags$td(align = "left", "Days in milk at first test")
              ),
              
              tags$tr(
                tags$td(align = "left", "FCDAT"),
                tags$td(align = "left", "94"),
                tags$td(align = "left", "FDAT"),
                tags$td(align = "left", "1"),
                tags$td(align = "left", "First Calving date")
              ),
              
              tags$tr(
                tags$td(align = "left", "FSTPJ"),
                tags$td(align = "left", "86"),
                tags$td(align = "left", "1"),
                tags$td(align = "left", "0"),
                tags$td(align = "left", "First test 305-d ME projection of milk prod")
              ),
              
              tags$tr(
                tags$td(align = "left", "FSTBF"),
                tags$td(align = "left", "83"),
                tags$td(align = "left", "1"),
                tags$td(align = "left", "0"),
                tags$td(align = "left", "First test milk fat percent")
              ),
              
              tags$tr(
                tags$td(align = "left", "FSTPR"),
                tags$td(align = "left", "83"),
                tags$td(align = "left", "1"),
                tags$td(align = "left", "1"),
                tags$td(align = "left", "First test milk protein percent")
              ),
              
              tags$tr(
                tags$td(align = "left", "PEAKM"),
                tags$td(align = "left", "82"),
                tags$td(align = "left", "99"),
                tags$td(align = "left", "0"),
                tags$td(align = "left", "Peak milk production in the current lactation")
              ),
              
              tags$tr(
                tags$td(align = "left", "DCAR"),
                tags$td(align = "left", "171"),
                tags$td(align = "left", "45"),
                tags$td(align = "left", "1"),
                tags$td(align = "left", "Sold or Died 'Condition Affecting Record' code (numeric)")
              ),
              
              tags$tr(
                tags$td(align = "left", "PR305"),
                tags$td(align = "left", "93"),
                tags$td(align = "left", "305ME"),
                tags$td(align = "left", "-1"),
                tags$td(align = "left", "Previous lactation 305ME")
              ),
              
              tags$tr(
                tags$td(align = "left", "LOG1"),
                tags$td(align = "left", "88"),
                tags$td(align = "left", "1"),
                tags$td(align = "left", "1"),
                tags$td(align = "left", "Log SCC on first test day")
              )
              
            )
          )
        ),
        br(),
        tags$li("Copy the following command into the command line."),
        tags$ol(
          tags$li(strong("EVENTS  ID LACT LCTGP FDAT BDAT FCDAT RC DRYLG DDRY PDCC PDIM EASE CNUM CLIVE CSEX FTDIM FSTPJ FSTBF FSTPR PEAKM DCAR  PR305 LOG1\\2SI")),
          tags$li("Can also save in the DairyComp dropdown menu for further use."),
          tags$ol(
            tags$li("ALTER - Commands (3)"),
            tags$ol(
              tags$li("Add [Ins]"),
              tags$ol(
                tags$li("Abbreviation: DCEXTR"),
                tags$li("Content: EVENTS  ID LACT LCTGP FDAT BDAT FCDAT RC DRYLG DDRY PDCC PDIM EASE CNUM CLIVE CSEX FTDIM FSTPJ FSTBF FSTPR PEAKM DCAR  PR305 LOG1\\2SI")
              )
            ),
            tags$li("Select program setup under File menu."),
            tags$li("Select MENU tab"),
            tags$ol(
              tags$li("Click Add Top Menu"),
              tags$ol(
                tags$li("Name Menu.")
              ),
              tags$li("Click Menu header where you would like to save the command."),
              tags$ol(
                tags$li("Add Submenu."),
                tags$li("Click F2 or click on created menu name."),
                tags$ol(
                  tags$li("Title."),
                  tags$li("Command."),
                  tags$li("Add description.")
                )
              ),
              tags$li("The command can now be referenced directly from dropdown menu for further use.")
            )
          )
        ),
        tags$li("Select date approximately 2 to 2.5 years prior to DC305 date."),
        tags$li("Select last date for analysis."),
        tags$li("Select Events."),
        tags$ol(
          tags$li("Select All includes the most information and is generally preferred."),
          tags$li("Can also individually select Events."),
          tags$ol(
            tags$li("MUST include", strong("FRESH"), ".")
          ),
          tags$li("Select optional REM pattern."),
          tags$ol(
            tags$li("Select default 'none' by clicking 'OK'")
          ),
          tags$li("This may take a few minutes.")
        ),
        tags$li("Write data to CSV."),
        tags$ol(
          tags$li("Find floppy disk icon above command line."),
          tags$li("Click downward facing triangle to the right of the icon."),
          tags$li("Save as CSV. Do not save as XLS."),
          tags$li("Choose location and file name for CSV.")
        ),
        tags$li("The downloaded CSV is now ready to be analyzed using DairyCoPilot.")
      ),
      
      h2("Glossary"),
      p(strong("ID: "), "Animal's identification"),
      p(strong("LACT: "), "Lactation number"),
      p(strong("LCTGP: "), "Lactation groups (1, 2, or 3)"),
      p(strong("FDAT: "), "Fresh date that initiated this lactation"),
      p(strong("BDAT: "), "Birth date of the animal"),
      # p(strong("TOCU1: "), "Date of last move to close-up pen"),
      # p(strong("TOCU2: "), "Date of previous move to close-up pen"),
      p(strong("RC: "), "Reproductive code (1 through 8)"),
      p(strong("DRYLG: "), "Dry log SCC"),
      p(strong("DDRY: "), "Days dry (prior to the current calving)"),
      p(strong("PDCC: "), "Previous days carried calf (gestation length)"),
      p(strong("PDIM: "), "Previous lactation days in milk (lactation length)"),
      p(strong("EASE: "), "Calving ease score, 1 to 5 scale"),
      p(strong("CNUM: "), "Number of calves born to start the current lactation"),
      p(strong("CLIVE: "), "Calf alive or dead (A = alive, D = dead)"),
      p(strong("CSEX: "), "Calf sex (M, F, MM, FF, FM, etc.)"),
      p(strong("FTDIM: "), "Days in milk at first test"),
      p(strong("FSTPJ: "), "First test 305-d ME projection of milk prod"),
      p(strong("FSTBF: "), "First test milk fat percent"),
      p(strong("FSTPR: "), "First test milk protein percent"),
      p(strong("PEAKM: "), "Peak milk production in the current lactation"),
      p(strong("DCAR: "), 'Sold or Died "Condition Affecting Record" code (numeric)'),
      p(strong("DATS: "), "Date sold"),
      p(strong("REMS: "), "Remark of the sold event"),
      p(strong("DATD: "), "Date died"),
      p(strong("REMD: "), "Remark of the died 1dmfevent"),
      p(strong("1DMF: "), "Date of 1st milk fever in the current lactation"),
      p(strong("REMMF: "), "Remark of 1st milk fever in the current lactation"),
      p(strong("XMF: "), "Count of all milk fever events in the current lactation"),
      p(strong("1DRP: "), "Date of 1st retained placenta in the current lactation"),
      p(strong("REMRP: "), "Remark of 1st retained placenta in the current lactation"),
      p(strong("XRP: "), "Count of all RP events in the current lactation"),
      p(strong("1DME: "), "Date of 1st metritis event in the current lactation"),
      p(strong("REMME: "), "Remark of 1st metritis event in the current lactation"),
      p(strong("XMETR: "), "Count of all metritis events in the current lactation"),
      p(strong("1DKE: "), "Date of 1st ketosis event in the current lactation"),
      p(strong("REMKE: "), "Remark of 1st ketosis event in the current lactation"),
      p(strong("XKET: "), "Count of all ketosis events in the current lactation"),
      p(strong("1DDA: "), "Date of 1st DA event in the current lactation"),
      p(strong("REMDA: "), "Remark of 1st DA event in the current lactation"),
      p(strong("XDA: "), "Count of all DA events in the current lactation"),
      p(strong("1DPN: "), "Date of 1st pneumonia event in the current lactation"),
      p(strong("REMPN: "), "Remark of pneumonia event in the current lactation"),
      p(strong("XPNEU: "), "Count of all pneumonia events in the current lactation"),
      p(strong("1DDIA: "), "Date of 1st diarrhea event in the current lactation"),
      p(strong("REMDI: "), "Remark of 1st diarrhea event in the current lactation"),
      p(strong("XDIAR: "), "Count of all diarrhea events in the current lactation"),
      p(strong("1DLAM: "), "Date of 1st lame event in the current lactation"),
      p(strong("REMLM: "), "Remark of 1st lame event in the current lactation"),
      p(strong("XLAME: "), "Count of all lame events in the current lactation"),
      p(strong("1DFT: "), "Date of 1st foot trim event in the current lactation"),
      p(strong("REMFT: "), "Remark of 1st foot trim event in the current lactation"),
      p(strong("XFTRM: "), "Count of all foot trim events in the current lactation"),
      p(strong("1DMA: "), "Date of 1st mastitis event in the current lactation"),
      p(strong("REMMA: "), "Remark of 1st mastitis event in the current lactation"),
      p(strong("XMAST: "), "Count of all mastitis events in the current lactation"),
      p(strong("1DIL: "), "Date of 1st ILLMISC event in the current lactation"),
      p(strong("REMIL: "), "Remark of 1st ILLMISC event in the current lactation"),
      p(strong("XILL: "), "Count of all ILLMISC events in the current lactation"),
      
      h2("Contacts"),
      p("We encourage suggestions of new features and improvements to make the 
        application more helpful. The developers can be contacted below."),
      tags$ul(tags$li("Srikanth Aravamuthan (",
                      a("aravamuthan@wisc.edu",
                        href = "mailto:aravamuthan@wisc.edu"),
                      ")"),
              tags$li("Emil Walleser (",
                      a("walleser@wisc.edu",
                        href = "mailto:walleser@wisc.edu"),
                      ")"),
              tags$li("Dorte Dopfer (",
                      a("dopfer@wisc.edu",
                        href = "mailto:dopfer@wisc.edu"),
                      ")")),
      
      h2("Acknowledgments"),
      p("This project would not have been possible without Dr. Gary Oetzel who provided the initial motivation to begin this project and whose previous work with analyzing health data in DairyComp was an essential starting place."),
      
      h2("Example"),
      # Output: Download Example Data ----
      downloadButton("downloadExample", "Download Example Data"),
      br(),
      br(),
      
      img(src = "download (1).jpg", height = 300, width = 300, style='border:4px solid #000000'),
      br(),
      br()
      
    )
  )
)



ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  ## Check that inputs meet restrictions ---------------------------------------
  observe({
    # Don't throw an error if the field is left blank momentarily
    req(
      input$file1,
      # input$header,
      # input$sep,
      # input$quote,
      cancelOutput = TRUE
    )
    
    showWarningIf <- function(condition, message) {
      if (condition) {
        showNotification(message, type = "warning")
      }
    }
    
  })
  
  # # A queue of notification IDs
  # ids <- character(0)
  # # A counter
  # n <- 0
  # 
  # observeEvent(input$file1, {
  #   id <- showNotification("Please wait for application to process the data and generate the output.",
  #                          duration = NULL,
  #                          type = "warning")
  # })
  
  # observeEvent(df(), {
  #   if (length(ids) > 0)
  #     removeNotification(ids[1])
  #   ids <<- ids[-1]
  # })
  
  ## Reactive elements ---------------------------------------------------------
  df <- reactive({
    req(
      input$file1,
      input$type1,
      # input$header,
      # input$sep,
      # input$quote,
      cancelOutput = TRUE
    )
    
    id <- showNotification("Reading data...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    f <- function(prev, curr, roll = 3) {
      if_else(as.numeric(curr - prev) > roll, curr, prev) %>%
        lubridate::as_date()
    }
    
    cols <- 
      c("ID" = NA, 
        "LACT" = NA, 
        # "LCTGP" = NA, 
        "FDAT" = NA, 
        "BDAT" = NA, 
        "FCDAT" = NA, 
        "RC" = NA, 
        "DRYLG" = NA, 
        "DDRY" = NA, 
        "PDCC" = NA, 
        "PDIM" = NA, 
        "EASE" = NA, 
        "CNUM" = NA, 
        "CLIVE" = NA, 
        "CSEX" = NA, 
        "FTDIM" = NA, 
        "FSTPJ" = NA, 
        "FSTBF" = NA, 
        "FSTPR" = NA, 
        "PEAKM" = NA, 
        "DCAR" = NA, 	
        "PR305" = NA, 
        "LOG1" = NA, 	
        "Event" = NA, 
        "Date" = NA,  
        "Remark" = NA, 
        "Protocols" = NA, 
        "Technician" = NA)       
    
    cols_events <- 
      c(
        MF_1_Date = NA_Date_,
        RP_1_Date = NA_Date_,
        METR_1_Date = NA_Date_,
        KETOSIS_1_Date = NA_Date_,
        PNEU_1_Date = NA_Date_,
        DIARHEA_1_Date = NA_Date_,
        LAME_1_Date = NA_Date_,
        FOOTRIM_1_Date = NA_Date_,
        MAST_1_Date = NA_Date_,
        ILLMISC_1_Date = NA_Date_
      )
    
    rows_events <- 
      c("FRESH",
        "ABORT",
        "SOLD",
        "DIED",
        "MF",
        "RP",
        "METR",
        "KETOSIS",
        "DA",
        "PNEU",
        "DIARHEA",
        "LAME",
        "FOOTRIM",
        "ILLMISC",
        "MAST")
    
    # df <- 
    #   read.csv(
    #     input$file1$datapath,
    #     header = input$header,
    #     sep = input$sep,
    #     quote = input$quote
    #   )
    
    df <- read_csv(file = input$file1$datapath, guess_max = 100000)
    
    if(input$type1 == "raw") {
      df <- 
        df %>% 
        # filter(between(lubridate::mdy(Date), input$start_date, input$end_date)) %>% 
        # mutate(Date = lubridate::mdy(Date)) %>% 
        # group_by(ID, LACT, Event) %>% 
        # arrange(ID, LACT, Event, Date) %>% 
        # mutate(
        #   x = Date,
        #   x = purrr::accumulate(x, f) %>% 
        #     lubridate::as_date()
        # ) %>% 
        # ungroup() %>% 
        # mutate(Date = as.character(Date)) %>% 
        # distinct(ID, LACT, Event, x, .keep_all = TRUE) %>% 
        mutate(
          Event = 
            case_when(
              Event == 'FRESH' ~ 'FRESH',
              Event == 'ABORT' ~ 'ABORT',
              Event == 'SOLD' ~ 'SOLD',
              Event == 'DIED' ~ 'DIED',
              Event == 'MF' ~ 'MF',
              Event == 'RP' ~ 'RP',
              Event == 'METR' ~ 'METR',
              Event == 'KETOSIS' ~ 'KETOSIS',
              # convert RDA and LDA to single event
              Event == 'DA' ~ 'DA',
              Event == 'LDA' ~ 'DA',
              Event == 'RDA' ~ 'DA',
              Event == 'PNEU' ~ 'PNEU',
              Event == 'SCOURS' ~ 'DIARHEA',
              Event == 'LAME' ~ 'LAME',
              Event == 'FOOTRIM' ~ 'FOOTRIM',
              Event == 'ILLMISC' ~ 'ILLMISC',
              Event == 'MAST' ~ 'MAST',
              # convert alternative codes to single event
              Event == 'MET' ~ 'METR',
              Event == 'KET' ~ 'KETOSIS',
              Event == 'DIARRH' ~ 'DIARHEA',
              Event == 'FOOTRMK' ~ 'FOOTRIM',
              Event == 'TRIM' ~ 'FOOTRIM',
              TRUE ~ Event
            )
        ) %>% 
        filter(Event != 'NA') %>%
        # transform 0 to NA
        mutate_at(vars(FTDIM,FSTPJ,FSTBF,FSTPR,PEAKM,DCAR,PR305), ~ifelse(. == "0", NA, .)) %>% 
        mutate(LOG1 = ifelse(is.na(FSTPJ), NA, LOG1)) %>% 
        # add missing columns
        add_column(!!!cols[!names(cols) %in% names(.)]) %>% 
        select(!!!names(.)[names(.) %in% names(cols)]) %>% 
        # paste together remark and protocol
        mutate(Remark = glue("{Remark}_{Protocols}_{Technician}")) %>% 
        select(-Protocols, -Technician) %>% 
        filter(Event %in% rows_events) %>% 
        group_by(ID, LACT, Event) %>% 
        # append event number
        mutate(Event = glue("{Event}_{row_number()}")) %>% 
        ungroup() %>% 
        # select(-Technician) %>% 
        # pivot data from wide to long by date and remark
        pivot_longer(c(Date,Remark)) %>% 
        # append event description
        mutate(name = glue("{Event}_{name}")) %>% 
        select(-Event) %>% 
        # rowwise steps can be moved here
        # pivot data from long to wide by event name
        pivot_wider(id_cols = c(-name,-value), names_from  = name, values_from = value) %>% 
        add_column(!!!cols_events[!names(cols_events) %in% names(.)]) %>% 
        # drop na
        # filter(!is.na(FRESH)) %>%
        filter(LACT > 0) %>% 
        # filter out fresh dates
        # filter(lubridate::mdy(FDAT) >= input$earliest_fresh_date) %>%
        filter(between(lubridate::mdy(FDAT), input$earliest_fresh_date, input$latest_fresh_date)) %>%
        mutate_at(vars(ends_with("_Date")), lubridate::mdy) %>%
        mutate_at(vars(ends_with("_Remark")), as.character) %>%
        # rowwise count of all events
        rowwise() %>% 
        mutate(XMF = sum(!is.na(c_across(starts_with("MF_") & ends_with("_Date")))),
               XRP = sum(!is.na(c_across(starts_with("RP_") & ends_with("_Date")))),
               XMETR = sum(!is.na(c_across(starts_with("METR_") & ends_with("_Date")))),
               XKET = sum(!is.na(c_across(starts_with("KETOSIS_") & ends_with("_Date")))),
               XDA = sum(!is.na(c_across(starts_with("DA_") & ends_with("_Date")))),
               XPNEU = sum(!is.na(c_across(starts_with("PNEU_") & ends_with("_Date")))),
               XDIAR = sum(!is.na(c_across(starts_with("DIARHEA_") & ends_with("_Date")))),
               XFTRM = sum(!is.na(c_across(starts_with("FOOTRIM_") & ends_with("_Date")))),
               XLAME = sum(!is.na(c_across(starts_with("LAME_") & ends_with("_Date")))),
               XMAST = sum(!is.na(c_across(starts_with("MAST_") & ends_with("_Date")))),
               XILL = sum(!is.na(c_across(starts_with("ILLMISC_") & ends_with("_Date"))))) %>% 
        ungroup() %>% 
        mutate(
          DATS = SOLD_1_Date,
          REMS = SOLD_1_Remark,
          DATD = DIED_1_Date,
          REMD = DIED_1_Remark,
          DATR = 
            case_when(
              !is.na(DATS) ~ DATS,
              !is.na(DATD) ~ DATD,
              TRUE ~ NA_Date_
            ),
          REMR = 
            case_when(
              !is.na(DATS) ~ REMS,
              !is.na(DATD) ~ REMD,
              TRUE ~ NA_character_
            )
        ) %>% 
        select(-c(SOLD_1_Date,SOLD_1_Remark,DIED_1_Date,DIED_1_Remark)) %>% 
        filter(between(str_length(BDAT), 8, 10),
               between(str_length(FDAT), 8, 10)) %>% 
        mutate(FCDAT = ifelse(between(str_length(FCDAT), 8, 10), NA, FCDAT)) %>% 
        mutate_at(vars(ID, RC), forcats::as_factor) %>% 
        mutate_at(vars(BDAT, FDAT, FCDAT), lubridate::mdy) %>%
        mutate_at(vars(CLIVE, CSEX), as.character) %>%
        # mutate_at(vars(Technician), as.character) %>%
        mutate_at(vars(LACT, FTDIM, FSTPJ, FSTBF, FSTPR, PEAKM, DCAR, PR305, DRYLG, LOG1, LACT, DDRY, PDCC, PDIM, EASE, CNUM), as.numeric) %>%
        mutate(
          LCTGP = 
            ifelse(LACT >= 3, 3, LACT) %>% 
            forcats::as_factor(),
          FRESH_MONTH = format_ISO8601(FDAT, precision = "ym"),
          DIM = ifelse(is.na(DATR), input$dairy_comp_extraction_date - FDAT, DATR - FDAT),
          `FRESH 372 TO 7` = ifelse(between(DIM, 7, 372), 1, 0),
          `FRESH 386 TO 21` = ifelse(between(DIM, 21, 386), 1, 0),
          `FRESH 425 TO 60` = ifelse(between(DIM, 60, 425), 1, 0),
          BIRTH_DATE = BDAT,
          `AGE_AT_CALVING_(MONTHS)` = interval(BIRTH_DATE, FDAT) %/% months(1),
          `AGE_AT_CALVING_1ST_LACT_(MONTHS)` = interval(BIRTH_DATE, FCDAT) %/% months(1), 
          DDRY = ifelse(LACT != 1, DDRY, NA),
          ABORT = ifelse(PDCC < 260, 1, 0),
          PDIM = ifelse(LACT != 1, PDIM, NA),
          # calving variables
          `CALVING_EASE_>=2` = ifelse(EASE >= 2, 1, 0),
          `CALVING_EASE_>=3` = ifelse(EASE >= 3, 1, 0),
          CNUM = ifelse((CNUM != 0) | (EASE == 0), CNUM, NA),
          CSEX = ifelse(!is.na(CNUM), CSEX, NA),
          CLIVE = ifelse(!is.na(CNUM), CLIVE, NA),
          TWINS = 
            case_when(
              CNUM > 1 ~ 1,
              CNUM == 1 ~ 0,
              TRUE ~ NA_real_
            ),
          STILLBIRTH = ifelse(str_detect(CLIVE, "D"), 1, 0),
          CALF_SEX = CSEX,
          MALE_CALF = ifelse(str_detect(CSEX, "M"), 1, 0),
          # fat protein ratio
          FPR = FSTBF/FSTPR,
          `FPR>1.4` = ifelse(FPR > 1.4, 1, 0),
          # sold
          SOLD = ifelse(!is.na(DATS), 1, 0),
          SOLD_DIM = DATS - FDAT,
          `SOLD<=60` = 
            case_when(
              SOLD == 0 ~ 0,
              SOLD_DIM <= 60 ~ 1,
              TRUE ~ 0
            ),
          # died
          DIED = ifelse(!is.na(DATD), 1, 0),
          DIED_DIM = DATD - FDAT,
          `DIED<=60` = 
            case_when(
              DIED == 0 ~ 0,
              DIED_DIM <= 60 ~ 1,
              TRUE ~ 0
            ),
          # removed
          REMVD = ifelse(!is.na(DATR), 1, 0),
          REMVD_DIM = DATR - FDAT,
          `REMVD<=60` = 
            case_when(
              REMVD == 0 ~ 0,
              REMVD_DIM <= 60 ~ 1,
              TRUE ~ 0
            ),
          # milk fever
          MLK_FVR = ifelse(!is.na(MF_1_Date), 1, 0),
          MLK_FVR_DIM = MF_1_Date - FDAT,
          `MLK_FVR<=7` = 
            case_when(
              MLK_FVR == 0 ~ 0,
              MLK_FVR_DIM <= 7 ~ 1,
              TRUE ~ 0
            ),
          # retained placenta
          RP = ifelse(!is.na(RP_1_Date), 1, 0),
          RP_DIM = RP_1_Date - FDAT,
          `RP<=7` = 
            case_when(
              RP == 0 ~ 0,
              RP_DIM <= 7 ~ 1,
              TRUE ~ 0
            ),
          # metritis
          METR = ifelse(!is.na(METR_1_Date), 1, 0),
          METR_DIM = METR_1_Date - FDAT,
          `METR<=21` = 
            case_when(
              METR == 0 ~ 0,
              METR_DIM <= 21 ~ 1,
              TRUE ~ 0
            ),
          # ketosis
          KET = ifelse(!is.na(KETOSIS_1_Date), 1, 0),
          KET_DIM = KETOSIS_1_Date - FDAT,
          `KET<=60` = 
            case_when(
              KET == 0 ~ 0,
              KET_DIM <= 60 ~ 1,
              TRUE ~ 0
            ),
          # da
          DA = ifelse(!is.na(DA_1_Date), 1, 0),
          DA_DIM = DA_1_Date - FDAT,
          `DA<=60` = ifelse(!is.na(DA_1_Date), ifelse(DA_DIM <= 60, 1, 0), 0),
          `DA<=60` = 
            case_when(
              DA == 0 ~ 0,
              DA_DIM <= 60 ~ 1,
              TRUE ~ 0
            ),
          # pneumonia
          PNEU = ifelse(!is.na(PNEU_1_Date), 1, 0),
          PNEU_DIM = PNEU_1_Date - FDAT,
          `PNEU<=60` = 
            case_when(
              PNEU == 0 ~ 0,
              PNEU_DIM <= 60 ~ 1,
              TRUE ~ 0
            ),
          # diarrhea
          DIARR = ifelse(!is.na(DIARHEA_1_Date), 1, 0),
          DIARR_DIM = DIARHEA_1_Date - FDAT,
          `DIARR<=60` = 
            case_when(
              DIARR == 0 ~ 0,
              DIARR_DIM <= 60 ~ 1,
              TRUE ~ 0
            ),
          # lame
          LAME = ifelse(!is.na(LAME_1_Date), 1, 0),
          LAME_DIM = LAME_1_Date - FDAT,
          `LAME<=60` = 
            case_when(
              LAME == 0 ~ 0,
              LAME_DIM <= 60 ~ 1,
              TRUE ~ 0
            ),
          # foot trim
          FTRM = ifelse(!is.na(FOOTRIM_1_Date), 1, 0),
          FTRM_DIM = FOOTRIM_1_Date - FDAT,
          `FTRM<=60` = 
            case_when(
              FTRM == 0 ~ 0,
              FTRM_DIM <= 60 ~ 1,
              TRUE ~ 0
            ),
          # mastitis
          MAST = ifelse(!is.na(MAST_1_Date), 1, 0),
          MAST_DIM = MAST_1_Date - FDAT,
          `MAST<=60` = 
            case_when(
              MAST == 0 ~ 0,
              MAST_DIM <= 60 ~ 1,
              TRUE ~ 0
            ),
          # miscellaneous illness
          ILLMISC = ifelse(!is.na(ILLMISC_1_Date), 1, 0),
          ILLMISC_DIM = ILLMISC_1_Date - FDAT,
          `ILLMISC<=60` = 
            case_when(
              ILLMISC == 0 ~ 0,
              ILLMISC_DIM <= 60 ~ 1,
              TRUE ~ 0
            )
        ) %>% 
        # group_by(ID) %>% 
        # slice_max(LACT) %>% 
        # ungroup() %>% 
        mutate_at(vars(ends_with("_DIM")), as.numeric) %>%
        mutate_at(vars(CLIVE, CSEX, CALF_SEX, FRESH_MONTH, `FRESH 372 TO 7`:`FRESH 425 TO 60`, ABORT, `CALVING_EASE_>=2`:`CALVING_EASE_>=3`, TWINS:STILLBIRTH, MALE_CALF, `FPR>1.4`:`ILLMISC<=60`, -ends_with("_DIM")), forcats::as_factor) %>% 
        mutate_at(vars(RC, CLIVE, CSEX, CALF_SEX, FRESH_MONTH), ~ forcats::fct_relevel(., sort(levels(.)))) %>% 
        mutate_at(vars(`FRESH 372 TO 7`:`FRESH 425 TO 60`, ABORT, `CALVING_EASE_>=2`:`CALVING_EASE_>=3`, TWINS:STILLBIRTH, MALE_CALF, `FPR>1.4`:`ILLMISC<=60`, -ends_with("_DIM")), ~ forcats::fct_relevel(., c("0", "1")))
    }
    
    if(input$type1 == "clean") {
      df <- 
        df %>% 
        mutate_at(vars(BDAT, FDAT, FCDAT, BIRTH_DATE), lubridate::mdy) %>%
        mutate_at(vars(ends_with("_Date"), starts_with("DAT")), lubridate::mdy) %>%
        # mutate_at(vars(Technician), as.character) %>%
        mutate_at(vars(ends_with("_Remark"), starts_with("REM")), as.character) %>%
        mutate_at(vars(FTDIM, FSTPJ, FSTBF, FSTPR, PEAKM, DCAR, PR305, DRYLG, LOG1, LACT, DDRY, PDCC, PDIM, EASE, CNUM, XMF:XILL, DIM, `AGE_AT_CALVING_(MONTHS)`:`AGE_AT_CALVING_1ST_LACT_(MONTHS)`, FPR), as.numeric) %>%
        mutate_at(vars(ends_with("_DIM")), as.numeric) %>%
        mutate_at(vars(ID, RC, CLIVE, CSEX, CALF_SEX, FRESH_MONTH, `FRESH 372 TO 7`:`FRESH 425 TO 60`, LCTGP, ABORT, `CALVING_EASE_>=2`:`CALVING_EASE_>=3`, TWINS:STILLBIRTH, MALE_CALF, `FPR>1.4`:`ILLMISC<=60`, -ends_with("_DIM")), forcats::as_factor) %>% 
        mutate_at(vars(RC, CLIVE, CSEX, CALF_SEX, FRESH_MONTH), ~ forcats::fct_relevel(., sort(levels(.)))) %>% 
        mutate_at(vars(`FRESH 372 TO 7`:`FRESH 425 TO 60`, ABORT, `CALVING_EASE_>=2`:`CALVING_EASE_>=3`, TWINS:STILLBIRTH, MALE_CALF, `FPR>1.4`:`ILLMISC<=60`, -ends_with("_DIM")), ~ forcats::fct_relevel(., c("0", "1")))
    }
    
    df
    
  })
  
  output$contents <- 
    DT::renderDataTable(
      {
        head(df(), n = 100)
      }, 
      options = list(scrollX = TRUE)
    )
  
  output$df_summary <-
    renderPrint(
      {
        skimr::skim_without_charts(df())
      }
    )
  
  # output$df_summary <-
  #   DT::renderDataTable(
  #     {
  #       skimr::skim_without_charts(df())
  #     }, 
  #     options = list(scrollX = TRUE)
  #   )
  
  # output$df_summary <- 
  #   renderUI({
  #     summarytools::view(summarytools::dfSummary(df()),
  #                        method = "render",
  #                        omit.headings = TRUE,
  #                        footnote = NA,
  #                        bootstrap.css = FALSE,
  #                        graph.magnif = 0.8)
  #   })
  
  # Output: Download Clean Data ----
  output$downloadData  <- downloadHandler(
    filename = str_c(make_clean_names(str_c(input$farm_name, "clean_data", sep = "_")), ".csv"),
    content = function(file) {
      write.csv(df(), file, row.names = FALSE)
    }
  )

  # Output: Download Example Data ----
  output$downloadExample  <- downloadHandler(
    filename = "example.csv",
    content = function(file) {
      write.csv(df_example, file, row.names = FALSE)
    }
  )
  
  # Output: Response UI Select Input ----
  output$response_ui <- renderUI({
    cols <- 
      df() %>% 
      names()
    
    cols_date <- 
      df() %>% 
      select_if(lubridate::is.Date) %>% 
      names()
    
    cols_nonbinomial <- 
      df() %>% 
      select_if(is.factor) %>% 
      select_if(~ nlevels(.) != 2) %>% 
      names()
    
    selectInput(
      'response',
      'Select Response Variable',
      choices = cols[!(cols %in% c("ID", cols_date, cols_nonbinomial))],
      # selected = cols,
      # multiple = TRUE
    )
  })
  
  # Output: Explanatory UI Select Input ----
  output$explanatory_ui <- renderUI({
    cols <- 
      df() %>% 
      names()
    
    cols_date <- 
      df() %>% 
      select_if(lubridate::is.Date) %>% 
      names()
    
    cols_uninomial <- 
      df() %>% 
      select_if(is.factor) %>% 
      select_if(~ nlevels(.) == 1) %>% 
      names()
    
    selectInput(
      'explanatory',
      'Select Explanatory Variable(s)',
      choices = cols[!(cols %in% c("ID", input$response, cols_date, cols_uninomial))],
      # selected = cols,
      multiple = TRUE
    )
  })
  
  f <- reactive({
    req(
      input$file1,
      input$response,
      input$explanatory,
      df(),
      cancelOutput = TRUE
    )
    
    input$explanatory %>% 
      str_c("`", ., "`") %>% 
      str_flatten(collapse = " + ") %>% 
      str_c("`", input$response, "`", " ~ ", .) %>% 
      as.formula()

    # input$explanatory %>% 
    #   str_flatten(collapse = " + ") %>% 
    #   str_c(input$response, ., sep = " ~ ") %>% 
    #   as.formula()
    
  })
    
  mod <- reactive({
    req(
      input$file1,
      input$response,
      input$explanatory,
      df(),
      f(),
      cancelOutput = TRUE
    )
    
    cond <- 
      df() %>% 
      select(!!sym(input$response)) %>% 
      summarize_all(is.numeric) %>% 
      pull()
    
    if(cond) {
      mod <- lm(f(), data = df())
    }
    else {
      mod <- glm(f(), data = df(), family = binomial)
    }
    
    mod
    
  })
  
  # Output: Summary Table ----
  output$summary <- renderTable({
    mod() %>% 
      broom::tidy()
    
  })

  # Output: ANOVA Table ----
  output$anova <- renderTable({
    mod() %>% 
      anova() %>% 
      broom::tidy()
    
  })
  
  # Output: Generalized Pairs Plot ----
  output$plot1 <- renderPlotly({
    gg <- 
      df() %>% 
      select(c(input$response, input$explanatory)) %>% 
      GGally::ggpairs(
        upper = "blank",
        diag = list(continuous = GGally::wrap("barDiag", fill = "#8F0000", color = "#8F0000", alpha = 0.5),
                    discrete = GGally::wrap("barDiag", fill = "#8F0000", color = "#8F0000", alpha = 0.5)),
        lower = list(continuous = GGally::wrap("points", fill = "#8F0000", color = "#8F0000", alpha = 0.5),
                     combo = GGally::wrap("box_no_facet", fill = "#8F0000", color = "#8F0000", alpha = 0.5),
                     discrete = GGally::wrap("facetbar", fill = "#8F0000", color = "#8F0000", alpha = 0.5))
      ) + 
      theme_bw() +
      theme(strip.background = element_rect(fill = "white"))
    
    plotly::ggplotly(gg) %>% 
      layout(autosize = TRUE, 
             margin = list(l = 75,
                           r = 75,
                           b = 75,
                           t = 75,
                           pad = 10)) %>%
      config(displaylogo = FALSE)
    
  })
  
  df_coef <- reactive({
    req(
      input$file1,
      input$response,
      input$explanatory,
      f(),
      mod(),
      df(),
      cancelOutput = TRUE
    )
    
    mod() %>% 
      coef() %>% 
      as.data.frame() %>% 
      rename_all(~ "Parameter Estimate") %>% 
      bind_cols(
        mod() %>% 
          confint() %>% 
          as.data.frame()
      ) %>% 
      rownames_to_column(var = "row") %>% 
      filter(row != "(Intercept)") %>% 
      mutate(`Explanatory Variable` = str_match(row, str_c(input$explanatory, collapse = "|"))) %>% 
      mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .)) %>% 
      mutate_if(is.numeric, ~ ifelse(. < 0.001, 0.001, .)) %>% 
      mutate_if(is.numeric, ~ ifelse(. > 1000, 1000, .)) %>% 
      rename_all(~ str_replace(., " %", "%"))
    
  })
  
  # output$plot2 <- renderPlotly({
  #   gg <- 
  #     mod() %>% 
  #     GGally::ggcoef_model() + 
  #     theme_bw() +
  #     theme(strip.background = element_rect(fill = "white"))
  #   
  #   plotly::ggplotly(gg)
  #   
  # })
  
  # Output: Coefficient Plot ----
  output$plot2 <- renderPlotly({
    gg <-
      df_coef() %>% 
      ggplot() +
      geom_point(aes(y = `Parameter Estimate`,
                     x = `Explanatory Variable`,
                     color = `Explanatory Variable`,
                     group = row,
                     text = str_c("</br>Term: ", row,
                                  "</br>Parameter Estimate: ", round(`Parameter Estimate`,3),
                                  "</br>2.5%: ", round(`2.5%`,3),
                                  "</br>97.5%: ", round(`97.5%`,3))),
                 size = 2,
                 alpha = 0.5,
                 position = position_dodge(width = 0.5)) +
      geom_linerange(aes(x = `Explanatory Variable`,
                         ymin = `2.5%`,
                         ymax = `97.5%`,
                         color = `Explanatory Variable`,
                         group = row,
                         text = str_c("</br>Term: ", row,
                                      "</br>Parameter Estimate: ", round(`Parameter Estimate`,3),
                                      "</br>2.5%: ", round(`2.5%`,3),
                                      "</br>97.5%: ", round(`97.5%`,3))),
                     size = 2,
                     alpha = 0.5,
                     # lineend = "round",
                     position = position_dodge(width = 0.5)) +
      coord_flip() +
      theme_bw()
    
    gg %>% 
      ggplotly(tooltip = c("text")) %>% 
      layout(autosize = TRUE, 
             margin = list(l = 75,
                           r = 75,
                           b = 75,
                           t = 75,
                           pad = 10)) %>%
      config(displaylogo = FALSE)

  })
  
  # Output: Row UI Select Input ----
  output$row_ui <- renderUI({
    cols <- 
      df() %>% 
      select(-ID) %>% 
      names()
    
    cols_factor <- 
      df() %>% 
      select(-ID) %>% 
      select_if(is.factor) %>% 
      names()
    
    cols_multinomial <- 
      df() %>% 
      select(-ID) %>% 
      select_if(is.factor) %>% 
      select_if(~ nlevels(.) >= 2) %>% 
      names()
    
    selectInput(
      'row',
      'Select Row Variable',
      choices = cols_multinomial,
      selected = "CSEX",
      # multiple = TRUE
    )
  })
  
  # Output: Reference Row UI Select Input ----
  output$ref_row_ui <- renderUI({
    levels_row <- 
      df() %>% 
      summarize(levels(!!sym(input$row))) %>% 
      pull()
    
    selectInput(
      'ref_row',
      'Select Reference Level of Row Variable',
      choices = levels_row,
      selected = levels_row[1],
      # multiple = TRUE
    )
  })
  
  # Output: Column UI Select Input ----
  output$column_ui <- renderUI({
    cols <- 
      df() %>% 
      select(-ID) %>% 
      names()
    
    cols_factor <- 
      df() %>% 
      select(-ID) %>% 
      select_if(is.factor) %>% 
      names()

    cols_multinomial <- 
      df() %>% 
      select(-ID) %>% 
      select_if(is.factor) %>% 
      select_if(~ nlevels(.) >= 2) %>% 
      names()
    
    selectInput(
      'column',
      'Select Column Variable',
      choices = cols_multinomial,
      selected = "CLIVE",
      # multiple = TRUE
    )
  })
  
  # Output: Reference Column UI Select Input ----
  output$ref_column_ui <- renderUI({
    levels_column <- 
      df() %>% 
      summarize(levels(!!sym(input$column))) %>% 
      pull()
    
    selectInput(
      'ref_column',
      'Select Reference Level of Column Variable',
      choices = levels_column,
      selected = levels_column[1],
      # multiple = TRUE
    )
  })
  
  # Pivot Table Data Frame ----
  df_pivot_table <- reactive({
    req(
      input$file1,
      input$row,
      input$column,
      input$ref_row,
      input$ref_column,
      df(),
      cancelOutput = TRUE
    )
    
    df() %>% 
      mutate(!!input$row := fct_relevel(!!sym(input$row), input$ref_row),
             !!input$column := fct_relevel(!!sym(input$column), input$ref_column)) %>% 
      drop_na(!!sym(input$row), !!sym(input$column)) %>% 
      tabyl(!!sym(input$row), !!sym(input$column)) %>% 
      adorn_totals(c("row", "col")) %>% 
      as.data.frame() %>% 
      rename(row = !!sym(input$row))
    
  })
  
  # Output: Pivot Table ----
  output$pivot_table <- renderReactable({
    req(
      input$file1,
      input$row,
      input$column,
      input$ref_row,
      input$ref_column,
      df_pivot_table(),
      cancelOutput = TRUE
    )
    
    pal <- c('#36a1d6', '#76b8de', '#a0bfd9', '#ffffff', '#d88359', '#d65440', '#c62c34')
    
    df_pivot_table() %>% 
      reactable(
        columns = list(
          row = colDef(name = input$row,
                       style = list(fontWeight = "bold"))
        ),
        columnGroups = list(
          colGroup(name = input$column, columns = names(.)[-1])
        ),
        defaultColDef = colDef(
          cell = color_tiles(
            df_pivot_table(), 
            span = TRUE, 
            colors = pal, 
            opacity = 0.75
          )
        ),
        sortable = FALSE,
        bordered = TRUE,
      ) %>%
      add_legend(
        df_pivot_table(),
        col_name = 'Total',
        title = 'title',
        footer = 'footer',
        colors = pal
      )
    
  })
  
  # Odds Ratio Data Frame ----
  df_odds_ratio <- reactive({
    req(
      input$file1,
      input$row,
      input$column,
      input$ref_row,
      input$ref_column,
      input$conf_level,
      df(),
      cancelOutput = TRUE
    )
    
    f <- 
      input$column %>% 
      str_c("`", ., "`", " ~ ", "`", input$row, "`") %>% 
      as.formula()
    
    # f <- 
    #   input$column %>% 
    #   str_c(input$row, sep = " ~ ") %>% 
    #   as.formula()

    df_mod <- 
      df() %>% 
      mutate(!!input$row := fct_relevel(!!sym(input$row), input$ref_row),
             !!input$column := fct_relevel(!!sym(input$column), input$ref_column)) %>% 
      drop_na(!!sym(input$row), !!sym(input$column))
    
    mod <- multinom(f, data = df_mod)
    
    nlevels.col <- 
      df_mod %>% 
      summarize(nlevels(!!sym(input$column))) %>% 
      pull()
    
    if(nlevels.col == 2){
      
      level.col <- 
        df_mod %>% 
        summarize(levels(!!sym(input$column))) %>% 
        slice(n()) %>% 
        pull()
      
      coef.mod <- 
        mod %>% 
        coef() %>% 
        exp() %>% 
        as.data.frame() %>% 
        rename_all(~ str_c("OR %.", input$column, level.col))
      
      confint.mod <- 
        mod %>% 
        confint(level = input$conf_level/100) %>% 
        exp() %>% 
        as.data.frame() %>% 
        rename_all(~ str_c(., ".", input$column, level.col))
      
    } else{
      
      coef.mod <- 
        mod %>% 
        coef() %>% 
        t() %>% 
        exp() %>% 
        as.data.frame() %>% 
        rename_all(~ str_c("OR %.", input$column, .))
      
      confint.mod <- 
        mod %>% 
        confint(level = input$conf_level/100) %>% 
        exp() %>% 
        as.data.frame() %>% 
        rename_all(~ str_replace(., " %\\.", str_c(" %\\.", input$column)))
      
    }
    
    coef.mod %>% 
      bind_cols(confint.mod) %>% 
      rownames_to_column(var = "row") %>% 
      filter(row != "(Intercept)") %>% 
      pivot_longer(-row) %>% 
      separate(name, c("conf.level", "column"), sep = " %.") %>% 
      pivot_wider(names_from = conf.level,
                  values_from = value) %>% 
      rename_at(vars(ncol(.) - 1, ncol(.)), ~ str_c(., "%"))
    
  })
  
  # Output: Odds Ratio Table ----
  output$odds_ratio <- renderReactable({
    req(
      input$file1,
      input$row,
      input$column,
      input$ref_row,
      input$ref_column,
      input$conf_level,
      df(),
      df_odds_ratio(),
      cancelOutput = TRUE
    )
    
    df_mod <- 
      df() %>% 
      mutate(!!input$row := fct_relevel(!!sym(input$row), input$ref_row),
             !!input$column := fct_relevel(!!sym(input$column), input$ref_column))
    
    level.col <- 
      df_mod %>% 
      summarize(levels(!!sym(input$column))) %>% 
      pull()
    
    tab <- 
      df_odds_ratio() %>% 
      rename(` OR` = OR) %>% 
      pivot_wider(names_from = column,
                  names_glue = "{column} {.value}",
                  values_from = c(` OR`, ends_with("%"))) %>% 
      select(row, sort(current_vars())) %>% 
      rename_at(vars(-row), ~ str_replace(., " OR", "OR")) %>% 
      mutate(row = str_replace_all(row, input$row, str_c(input$row, ": "))) %>% 
      mutate_if(is.numeric, ~ signif(., digits = 4))
    
    ls.colDef <- 
      2:(length(names(tab))) %>% 
      purrr::map(~ colDef(name = str_split(names(tab)[.x], " ")[[1]][2])) %>% 
      purrr::set_names(names(tab)[2:(length(names(tab)))])
    
    # ls.colDef <- 
    #   list(
    #     row = colDef(name = "",
    #                  style = list(fontWeight = "bold"))
    #   ) %>% 
    #   prepend(ls.colDef, before = 1)
    
    ls.colGroup <-
      2:(length(level.col)) %>% 
      purrr::map(~ colGroup(name = str_c(input$column, ": ", level.col[.x]), columns = names(tab)[(3*.x-4):(3*.x-2)]))
    
    tab %>% 
      reactable(
        columns = ls.colDef,
        columnGroups = ls.colGroup,
        sortable = FALSE,
        # searchable = TRUE,
        bordered = TRUE,
        # highlight = TRUE
      )
    
  })
  
  # Output: Comparative Bar Plot ----
  output$plot3 <- renderPlotly({
    gg <- 
      df() %>% 
      drop_na(!!sym(input$row), !!sym(input$column)) %>% 
      ggplot() +
      geom_bar(aes(x = !!sym(input$row),
                   fill = !!sym(input$column),
                   color = !!sym(input$column),
                   text = str_c("</br>", input$row, ": ", x,
                                "</br>", input$column, ": ", color,
                                "</br>Count: ", ..count..)),
               position = position_dodge(width = 0.9, preserve = "single"),
               alpha = 0.5) +
      theme_bw()
    
    gg %>% 
      ggplotly(tooltip = c("text")) %>% 
      layout(autosize = TRUE, 
             margin = list(l = 75,
                           r = 75,
                           b = 75,
                           t = 75,
                           pad = 10)) %>%
      config(displaylogo = FALSE)
    
  })
  
  # Output: Forest Plot ----
  output$plot4 <- renderPlotly({
    gg <- 
      df_odds_ratio() %>% 
      rename(lower = ncol(.) - 1,
             upper = ncol(.)) %>% 
      mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .)) %>% 
      mutate_if(is.numeric, ~ ifelse(. < 0.001, 0.001, .)) %>% 
      mutate_if(is.numeric, ~ ifelse(. > 1000, 1000, .)) %>% 
      ggplot() +
      geom_point(aes(y = OR,
                     x = row,
                     color = column,
                     text = str_c("</br>Row: ", row,
                                  "</br>Column: ", column,
                                  "</br>OR: ", round(OR,3),
                                  "</br>", (100-input$conf_level)/2 , "%: ", round(lower,3),
                                  "</br>", 100-(100-input$conf_level)/2 , "%: ", round(upper,3))),
                 size = 2,
                 alpha = 0.5,
                 position = position_dodge(width = 0.5)) +
      geom_linerange(aes(x = row,
                         ymin = lower,
                         ymax = upper,
                         color = column,
                         text = str_c("</br>Row: ", row,
                                      "</br>Column: ", column,
                                      "</br>OR: ", round(OR,3),
                                      "</br>", (100-input$conf_level)/2 , "%: ", round(lower,3),
                                      "</br>", 100-(100-input$conf_level)/2 , "%: ", round(upper,3))),
                     size = 2,
                     alpha = 0.5,
                     position = position_dodge(width = 0.5)) +
      coord_flip() +
      theme_bw()
    
    gg %>% 
      ggplotly(tooltip = c("text")) %>% 
      layout(autosize = TRUE, 
             margin = list(l = 75,
                           r = 75,
                           b = 75,
                           t = 75,
                           pad = 10)) %>%
      config(displaylogo = FALSE)
    
  })
  
}

shinyApp(ui, server)



