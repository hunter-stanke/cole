#library(tidyverse)
library(shiny)
library(leaflet)
library(shiny.semantic)
library(magrittr)
library(rgdal)
library(sf)
library(rFIA)
library(shinycssloaders)
library(shinyWidgets)
library(semantic.dashboard)
library(tigris) # For counties
library(DT)
library(raster)
library(dplyr)
library(leaflet.extras)
library(rgeos)
library(purrr)
library(stringr)
library(prompter)
library(stringr)
# library(shinyFiles)


# install.packages(c('leaflet','shiny.semantic','magrittr','rgdal','sf','rFIA',
#                    'shinycssloaders','shinyWidgets','semantic.dashboard','tigris','DT','raster','dplyr'))

source("getDF.R", local = TRUE)
source("helpers.R")

fia_dir <<- "./FIA"
grouping.variables.df = read.csv("fia_grouping_variable_names.csv")
output.variables.df = read.csv("output_variable_names.csv") %>%
  dplyr::bind_rows(data.frame(rfia_name = grouping.variables.df$code,
                              cole_name = grouping.variables.df$name)) %>%
  dplyr::mutate_all(tolower)


## Lists to hold drawn shapefiles
sf_list <- rbind() 


## Read spatial data
states <- sf::st_read(here::here('data/spatial/states/states.shp'), quiet = TRUE) %>%
  dplyr::arrange(State)
counties <- sf::st_read(here::here('data/spatial/counties/counties.shp'), quiet = TRUE) %>%
  dplyr::mutate(County = paste0(State, ' - ', County)) %>%
  dplyr::arrange(County)
huc6 <- sf::st_read(here::here('data/spatial/huc6/huc6.shp'), quiet = TRUE) %>%
  dplyr::arrange(HUC6)
national_forests <- sf::st_read(here::here('data/spatial/national_forests/national_forests.shp'), quiet = TRUE) %>%
  dplyr::arrange(Forest)
usfs_regions <- sf::st_read(here::here('data/spatial/usfs_regions/usfs_regions.shp'), quiet = TRUE) %>%
  dplyr::arrange(Region)

grid_cell_style = "
  align-items: center;
  border: 1px solid #ccc;
  display: flex;
  justify-content: center;
"

min_year = "1999"
max_year = as.character(lubridate::year(lubridate::today()))
msu_green = "#18453B"

ui <- dashboardPage(
  dashboardHeader(title = "COLE 2.0", color = "grey", inverted = TRUE), # Can put logo here
  dashboardSidebar(side = "left", size = "thin", color = "grey", inverted = TRUE,
                   sidebarMenu(
                     menuItem(tabName = 'about', 'About'),
                     menuItem(tabName = "tab1", "Define Region of Interest"),
                     menuItem(tabName = "tab2", "Define Quantities of Interest"))),
  dashboardBody(tabItems(
    tabItem(
      tabName = 'about',
      semanticPage(
        #title = "Carbon Online Estimator 2.0",
        h1("Carbon Online Estimator 2.0"),
        br(),
        p("The second generation of the Carbon Online Estimator Tool (COLE 2.0) 
          is a web-based calculator that helps users estimate and monitor forest carbon stocks across 
          large geographic regions of interest (i.e., 75,000 acres and larger). 
          The tool is designed to provide a simple and accessible way for landowners, foresters, and
          other stakeholders to assess the carbon impact of different management 
          strategies and make informed decisions about forest management."),
        p("In addition to forest carbon estimates, the tool also provides 
          estimates of tree density, forested area, merchantable volume, and tree 
          biomass. These additional features enable users to better understand 
          and manage their forest resources, for example, supporting continuous
          timber market assessments and forest health monitoring."),
        p("Finally, the second generation of COLE offers users additional flexibility
          in defining geographic regions of interest. First, the new tool 
          offers users the ability to upload their own spatial polygons to define 
          regions of interest. This feature allows users to analyze specific areas
          in more detail and get a more accurate estimate of their forest carbon stocks. 
          Furthermore, the tool also provides users with the ability to create 
          their own spatial regions of interest using point and draw functionality
          on integrated maps. This feature allows users to draw custom boundaries 
          around specific areas of interest, such as a large ownership, and generate 
          estimates of forest carbon stocks based on those boundaries."),
        h2("Limitations"),
        p("COLE draws exclusively from data collected on the USFS Forest Inventory 
        and Analysis (FIA) plot network. The FIA program is considered the nation's
        forest census, and reports on the status, trends, and health of forests 
        through annual on-the-ground sampling. So, the accuracy of COLE-generated 
        estimates are related to the sample size of the underlying data. The more 
        filters that a user applies, the more important it becomes to choose a 
        larger geographic region of interest 
        (multiple counties, states, etc.) over which to apply these filters. 
        While COLE will not prevent you from generating estimates for a very specific population of interest
        (e.g. an aboveground carbon stocks for overstocked, privately owned,
        white oak/red oak/hickory stands in Elk County, PA), the user should be 
        aware that such a detailed report on a small scale is subject to high-levels
        of uncertainty due to low sample sizes.")
      ),
        h2("History"),
        p("The first-generation of  COLE was originally developed by Paul Van 
        Deusen (National Council for Air and Stream Improvment) and Linda Heath 
        (US Forest Service) to aid in voluntary reporting of greenhouse gases as 
        described in section 1605(b) of the Energy Policy Act of 1992. COLE, and 
        NE-GTR-343 (also known as, “Methods for calculating forest ecosystem and 
        harvested carbon with standard estimates for forest types of the United 
        States”) are based on similar data and conversion factors."),
        p("Past versions of COLE allowed users to create a growth and yield 
          prediction of carbon pools according to forest type, ownership class, 
          and a variety of other variables. While at present, COLE 2.0 does not
          support projection of carbon stocks into the future, this functionality
          is intended to be revived in forthcoming iterations of the tool.")
    ),
    tabItem(
      tabName = "tab1", 
      semanticPage(
        title = "COLE",
        sidebar_layout(
          sidebar_panel(
            width = 2,

            prompter::use_prompt(),
            
            
            br(),
            h3("Select region of interest using one of:"),
            hr(),
            prettyRadioButtons("spatial_domain", 
                         label = "",
                         #"Choose one of the following options to define your region of interest:\n",
                         choices = c("States" = 'states',
                                     "Counties" = 'counties',
                                     "HUC6 Basins" = 'watersheds',
                                     "Forest Service regional boundaries" = 'usfs_regions',
                                     "National Forest boundaries" = 'national_forests',
                                     "Draw polygons" = 'draw_polygons',
                                     "Draw circles" = 'draw_circles',
                                     "Upload spatial polygons layer" = 'shapefile'),
                         selected = 'states'
                         ),
            # State selector
            conditionalPanel(condition = "input.spatial_domain == 'states'",
                             br(),
                             selectInput("states", "Select states:",       
                                         choices = states$State, multiple = TRUE)
            ),
            # Watershed selector
            conditionalPanel(condition = "input.spatial_domain == 'counties'",
                             br(),
                             selectInput("counties", "Select counties:",       
                                         choices = counties$County, multiple = TRUE)
            ),
            # Watershed selector
            conditionalPanel(condition = "input.spatial_domain == 'watersheds'",
                             br(),
                             selectInput("watersheds", "Select HUC6 Basins:",       
                                         choices = huc6$HUC6, multiple = TRUE)
            ),
            # USFS Region selector
            conditionalPanel(condition = "input.spatial_domain == 'usfs_regions'",
                             br(),
                             selectInput("usfs_regions", "Select USFS Regions:",       
                                         choices = usfs_regions$Region, multiple = TRUE)
            ),
            # National Forest selector
            conditionalPanel(condition = "input.spatial_domain == 'national_forests'",
                             br(),
                             selectInput("national_forests", "Select National Forests:",       
                                         choices = national_forests$Forest, multiple = TRUE)
            ),
            # User drawn shapes
            conditionalPanel(condition = "input.spatial_domain == 'draw_polygons'",
                             br(),
                             h4('Use interactive map tools to draw your polygons (see top right corner of map).')
            ),
            # User drawn shapes
            conditionalPanel(condition = "input.spatial_domain == 'draw_circles'",
                             br(),
                             h4('Use interactive map tools to draw your circles (see top right corner of map).')
            ),
            # Shapefile upload
            conditionalPanel(condition = "input.spatial_domain == 'shapefile'",
                             br(),
                             fileInput("shapefile", "Choose shapefile to upload (.shp, .dbf, .prj, and .shx files):",
                                       multiple = TRUE,
                                       accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj")),
            ),
            

            
            br(),
            h3("Select time period of interest:"),
            hr(),
            
            
            
            # Most recent estimates only?
            checkboxInput("most_recent", 
                          "Use only the most recent data from each state?", 
                          FALSE) %>%
              prompter::add_prompt(
                position = "right", 
                size = 'large',
                type = NULL,
                message = "If region of interest spans multiple state boundaries, most recent reporting years in each state will be combined for estimation purposes.",
                permanent = FALSE,
                rounded = TRUE, 
                animate = FALSE
              ),
            
            ## What time periods are needed?
            setSliderColor(msu_green, sliderId = 1),
            # https://divadnojnarg.github.io/post/customsliderinput/
            conditionalPanel(condition = "!input.most_recent",
                             br(),
                             shiny::sliderInput("years_range", label = "Years Range:", 
                                                value = c(as.Date(min_year, format = "%Y"), as.Date(max_year, format = "%Y")), 
                                                min = as.Date(min_year, format = "%Y"), 
                                                max = as.Date(max_year, format = "%Y"),
                                                timeFormat = "%Y", step = 365),
                             )  %>%
              prompter::add_prompt(
                position = "right", 
                size = 'large',
                type = NULL,
                message = "Availability of reporting years varies by state, and not all years listed may be available for a particular region of interest. Estimation is limited to the reporting years available in the FIA Database.",
                permanent = FALSE,
                rounded = TRUE, 
                animate = FALSE
              ),
            
            

            br(),
            h3("Select processing options:"),
            hr(),
            
            checkboxInput('merge_spatial_domain',
                          'Merge selected spatial features into single region of interest?',
                          TRUE) %>%
              prompter::add_prompt(
                position = "right", 
                size = 'large',
                type = NULL,
                message = "If yes, boundaries of selected regions will be dissolved and estimation will occur for the aggregated region. That is, only one combined estimate will be returned for all selected regions. Otherwise, seperate estimates will be returned for each selected feature.",
                permanent = FALSE,
                rounded = TRUE, 
                animate = FALSE
              ),
            br(),
            
            # If running locally, select file location
            checkboxInput("run_local", 
                          "Run computation offline?", 
                          FALSE) %>%
              prompter::add_prompt(
                position = "right", 
                size = 'large',
                type = NULL,
                message = "Should estimation reference a local instance of the FIADB? Not relevant for online estimation.",
                permanent = FALSE,
                rounded = TRUE, 
                animate = FALSE
              ),
            conditionalPanel(condition = "input.run_local",
                             br(),
                             textInput("fia_dir", 
                                       "Path to directory where FIA data are stored (csv format required):", 
                                       value = "./FIA")),

            
            
            # Loading image
            conditionalPanel(
              condition="($('html').hasClass('shiny-busy'))",
              HTML('<center><img src="images/busy.gif" width="80"></center>')
            ),
          ), # End sidebar panel 1
          main_panel(
            width = 5,
            tags$style(type = "text/css", "#map {height: calc(100vh - 100px) !important; width: calc(65vw - 100px) !important;}"),
            leafletOutput('map')
          ) # End sidebar panel 2
        ), # End of semanticPage
      )
    ), # End tab1
    tabItem(
      tabName = "tab2",
      semanticPage(
        sidebar_layout(
          sidebar_panel(
            
            br(),
            h3("Select forestland definition:")  %>%
              prompter::add_prompt(
                position = "bottom-right", 
                size = 'large',
                type = NULL,
                message = "Forest land must be at least 10-percent stocked by trees of any size, including land that formerly had such tree cover and that will be naturally or artificially regenerated. Forest land includes transition zones, such as areas between heavily forested and nonforested lands that are at least 10-percent stocked with trees and forest areas adjacent to urban and builtup lands. The minimum area for classification of forest land is 1 acre and 120 feet wide measured stem-to-stem from the outer-most edge. Unimproved roads and trails, streams, and clearings in forest areas are classified as forest if less than 120 feet wide. Timber land is a subset of forest land that is producing or is capable of producing crops of industrial wood and not withdrawn from timber utilization by statute or administrative regulation.",
                permanent = FALSE,
                rounded = TRUE, 
                animate = FALSE
              ),
            hr(),
            # landType argument
            prettyRadioButtons("landType", "",
                               choices = c("Forestland" = "forest",
                                           "Timberland" = "timber"
                               )),
            
            
            br(),
            h3("Select variable of interest:") %>%
              prompter::add_prompt(
              position = "right", 
              size = 'large',
              type = NULL,
              message = "
              (1) Forest carbon: tonnes C by IPCC forest carbon pools (includes soils & forest litter); 
              (2) Forest area: acres of forest/ timberland;
              (3) Tree biomass: short tons dry biomass by tree component;
              (4) Tree density: trees per acre and tree basal area;
              (5) Merchantable volume: cubic feet and board feet of merchantable wood",
              permanent = FALSE,
              rounded = TRUE, 
              animate = FALSE
            ),
            hr(),
            
            ## Select quantity of interest
            prettyRadioButtons("dataType", 
                               label = "",
                               choices = c("Forest carbon" = "carbon", 
                                           "Forest area" = "area", 
                                           "Tree biomass" = "biomass", 
                                           "Trees density" = "tpa",
                                           "Merchantable volume" = "volume"),
                               selected = 'carbon'),
            
            
            ## By pool or all pools?
            conditionalPanel(condition = "input.dataType == 'carbon'",
                             br(),
                             h4("Return estimates for each IPCC forest carbon pool?") %>%
                               prompter::add_prompt(
                                 position = "right", 
                                 size = 'large',
                                 type = NULL,
                                 message = "If yes, seperate estimates will be produced for the following forest carbon pools: aboveground live, belowground live, dead wood, forest litter, and soil organic materials. If no, estimates from each pool will be combined into a single estimate of total forest carbon.",
                                 permanent = FALSE,
                                 rounded = TRUE, 
                                 animate = FALSE
                               ),
                             hr(),
                             prettyRadioButtons("byPool", 
                                                "", 
                                                choices = rev(c("Yes, return estimates for each pool." = TRUE, 
                                                                "No, combine all pools." = FALSE)),
                                                selected = FALSE),
            ),
            ## Tree type
            conditionalPanel(condition = "['biomass', 'tpa', 'volume'].includes(input.dataType)",
                             br(),
                             h4("Select type of trees for estimates:")  %>%
                               prompter::add_prompt(
                                 position = "right", 
                                 size = 'large',
                                 type = NULL,
                                 message = "All trees includes all stems, live and dead, greater than 1 in. DBH. Live/Dead includes all stems greater than 1 in. DBH which are live or dead (leaning less than 45 degrees), respectively. Growing-stock includes live stems greater than 5 in. DBH which contain at least one 8 ft merchantable log.",
                                 permanent = FALSE,
                                 rounded = TRUE, 
                                 animate = FALSE
                               ),
                             hr(),
                             prettyRadioButtons("treeType", "",
                                                choices = c("Live trees only" = "live",
                                                            "Dead trees only" = "dead",
                                                            "Growing-stock only" = "gs",
                                                            "All trees" = "all"
                                                ),
                                                selected = 'live')
            ),
            
            
            br(),
            h3("Group estimates by (optional):") %>%
              prompter::add_prompt(
                position = "right", 
                size = 'large',
                type = NULL,
                message = "Optionally select sub-populations for estimation. For example, if 'Ownership' is selected, seperate estimates will be returned for each ownership type encounted on the FIA plot network.",
                permanent = FALSE,
                rounded = TRUE, 
                animate = FALSE
              ),
            hr(),
            # Select grpBy value/array
            selectInput("groupBy", "",
                        choices = c("None"), multiple = TRUE),
            
            
            br(),
            ## Hide advanced? 
            checkboxInput("advanced", 
                          "Hide advanced settings?", 
                          TRUE),
            br(),
            # Estimator - advanced
            conditionalPanel(condition = '!input.advanced',
                             br(),
                             h4("Advanced settings:"),
                             hr(),
                             br(),
                             # Select method
                             selectInput("method", "Select estimator to use:",
                                         choices = c("Temporally indifferent" = "TI", 
                                                     "Annual" = "annual", 
                                                     "Simple moving average" = "SMA", 
                                                     "Linear moving average" = "LMA", 
                                                     "Exponential moving average" = "EMA"))  %>%
                               prompter::add_prompt(
                                 position = "right", 
                                 size = 'large',
                                 type = NULL,
                                 message = "The temporally-indifferent estimator is FIA's unofficial default estimator, effectively reducing the annual inventory into a periodic one. Alternative design-based estimators include the annual estimator (annual panels, or estimates from plots measured in the same year), simple moving average (combines annual panels with equal weight), linear moving average (combine annual panels with weights that decay linearly with time since measurement), and exponential moving average (combine annual panels with weights that decay exponentially with time since measurement).",
                                 permanent = FALSE,
                                 rounded = TRUE, 
                                 animate = FALSE
                               ),
                             conditionalPanel(condition = "input.method == 'EMA'",
                                              numeric_input("lambda", "Decay parameter (0, 1):",
                                                            0.5, min = 0, max = 1, step = 0.1)
                             ),
                             h4(),
                             HTML("<p>See <a href='https://www.fs.usda.gov/nrs/pubs/jrnl/2020/nrs_2020_stanke_001.pdf'>Stanke et al. (2020)</a> for a complete description of these estimators and tradeoffs between precision and temporal specificity.</p>")
                             
                             # br(),
                             # # Area domain equation box
                             # strong('Define conditions of interest in terms of FIADB variables:'),
                             # div(
                             #   accordion(list(list(title = "Conditions of Interest", content =
                             #                         list(selectInput("areaDomainVariable", "Variable:",
                             #                                          c("OWNGRPCD")),
                             #                              selectInput("areaDomainComparator", "Comparator:",
                             #                                          c("==", "!=", "<", "<=", ">", ">=", "%in%")),
                             #                              
                             #                              conditionalPanel(condition = "input.areaDomainComparator != '%in%'",
                             #                                               numeric_input("areaDomainValue", "Value:",
                             #                                                             0, min = 0, max = 100, step = 1),
                             #                              ),
                             #                              
                             #                              conditionalPanel(condition = "input.areaDomainComparator == '%in%'",
                             #                                               numeric_input("areaDomainValueMin", "Min value:",
                             #                                                             0, min = 0, max = 100, step = 1),
                             #                                               numeric_input("areaDomainValueMax", "Max value:",
                             #                                                             0, min = 0, max = 100, step = 1),
                             #                              ),
                             #                              
                             #                              br(),
                             #                              verbatimTextOutput("areaDomainOut")  %>% 
                             #                                tagAppendAttributes(style= 'color:green;'),
                             #                              
                             #                              actionButton("areaDomainButton", label = "New Statement"),
                             #                              
                             #                              actionButton("areaDomainButtonOr", label = "Add OR Statement"),
                             #                              actionButton("areaDomainButtonAnd", label = "Add AND Statement"),
                             #                              actionButton("areaDomainButtonClear", label = "Clear Statement(s)")
                             #                         ))))
                             # ),
                             # 
                             # br(),
                             # # Conditional panel for tree-level inputs
                             # conditionalPanel(condition = "['area', 'tpa', 'biomass', 'volume'].includes(input.dataType)",
                             #                  
                             #                  # Tree domain equation box
                             #                  strong('Define trees of interest in terms of FIADB variables:'),
                             #                  div(accordion(list(list(
                             #                    title = "Trees of Interest", content = list(
                             #                      selectInput("treeDomainVariable", "Variable:",
                             #                                  c("None")),
                             #                      selectInput("treeDomainComparator", "Comparator:",
                             #                                  c("==", "!=", "<", "<=", ">", ">=", "%in%")),
                             #                      
                             #                      conditionalPanel(condition = "treeDomainComparator != '%in%'",
                             #                                       numeric_input("treeDomainValue", "Value:",
                             #                                                     0, min = 0, max = 100, step = 1),
                             #                      ),
                             #                      
                             #                      conditionalPanel(condition = "treeDomainComparator == '%in%'",
                             #                                       numeric_input("treeDomainValueMin", "Min value:",
                             #                                                     0, min = 0, max = 100, step = 1),
                             #                                       numeric_input("treeDomainValueMax", "Max value:",
                             #                                                     0, min = 0, max = 100, step = 1),
                             #                      ),
                             #                      
                             #                      br(),
                             #                      verbatimTextOutput("treeDomainOut")  %>% 
                             #                        tagAppendAttributes(style= 'color:green;'),
                             #                      
                             #                      actionButton("treeDomainButton", label = "New Statement"),
                             #                      
                             #                      actionButton("treeDomainButtonOr", label = "Add OR Statement"),
                             #                      actionButton("treeDomainButtonAnd", label = "Add AND Statement"),
                             #                      actionButton("treeDomainButtonClear", label = "Clear Statement(s)")
                             #                    ))))
                             #                  ),
                             # )
                             
                             
            ),
            
            br(),
            ## Trigger hand off to rFIA
            actionBttn("get_results", "Get results!", color = "success",
                       style = "jelly",
                       icon = NULL,
                       block = TRUE),

          ), # End sidebar panel
          
          main_panel(
            tabset(tabs =
                     list(
                       list(menu = "Results", id = 'results', content = list(br(),
                                                                             semantic_DTOutput("result_table"), 
                                                                             br(),
                                                                             conditionalPanel('output.result_table != null',
                                                                                              br(),
                                                                                              downloadBttn("download_results", 
                                                                                                           "Download Results", 
                                                                                                           size = "sm", 
                                                                                                           style = "simple", 
                                                                                                           color = "primary")
                                                                             )
                                                                             )),
                       list(menu = "R Code", id = 'code', content = list(verbatimTextOutput('function_string'), br(), br()))
                     ),
                   active = "results",
                   id = "results_tabset"
            ),
            
            
            ## Need to select region of interest first
            conditionalPanel(condition = "input.spatial_domain == 'states' && input.states == null",
                             p("Please select at least one state, or otherwise define your region of interest.")),
            conditionalPanel(condition = "input.spatial_domain == 'counties' && input.counties == null",
                             p("Please select at least one county, or otherwise define your region of interest.")),
            conditionalPanel(condition = "input.spatial_domain == 'watersheds' && input.watersheds == null",
                             p("Please select at least one watershed (HUC 6), or otherwise define your region of interest.")),
            conditionalPanel(condition = "input.spatial_domain == 'usfs_regions' && input.usfs_regions == null",
                             p("Please select at least one USFS region, or otherwise define your region of interest.")),
            conditionalPanel(condition = "input.spatial_domain == 'national_forests' && input.national_forests == null",
                             p("Please select at least one National Forest, or otherwise define your region of interest.")),
            conditionalPanel(condition = "input.spatial_domain == 'draw_polygons' && output.n_features == 0",
                             p("Please draw at least one polygon, or otherwise define your region of interest.")),
            conditionalPanel(condition = "input.spatial_domain == 'draw_circles' && output.n_features == 0",
                             p("Please draw at least one circle, or otherwise define your region of interest.")),
            conditionalPanel(condition = "input.spatial_domain == 'shapefile' && input.shapefile == null",
                             p("Please upload shapefile, or otherwise define your region of interest.")),
            
            # Loading image
            conditionalPanel(
              condition="($('html').hasClass('shiny-busy'))",
              img(src = "images/busy.gif", width = 80)
            ),
            
          ), # End main panel
        ))
    
    )
  ))
)





server <- shinyServer(function(input, output, session) {

  ## Temporal selection 
  output$years <- renderText(paste(input[['years_range']], collapse = ", "))
  selected_years = reactive({
    min <- format(as.Date(input[['years_range']][1], 
                          format="%Y-%m-%d"),"%Y")
    max <- format(as.Date(input[['years_range']][2], 
                          format="%Y-%m-%d"),"%Y")
  })

  # Get years to display as integers on slider bar
  output$YearsSliderText <- renderText(paste(cbind(
    format(as.Date(input[['years_range']][1], format="%Y-%m-%d"),"%Y"),
    format(as.Date(input[['years_range']][2], format="%Y-%m-%d"),"%Y")
  ), collapse = " - "))
  
  
  
  # Drawn polygons & circles
  output$n_features <- renderText(0)
  outputOptions(output, "n_features", suspendWhenHidden = FALSE)
  observe({
    if(input$spatial_domain == "draw_polygons") {
      # Clear sf_list
      sf_list <<- rbind()
      # Redraw map and toolbar
      output$map <- renderLeaflet({
        leaflet(width="100%", height="100%") %>% 
          addBaseMapSelector() %>% 
          setView( -96, 38, zoom = 4) %>%
          addDrawToolbar(rectangleOptions = FALSE,
                         editOptions = editToolbarOptions(edit = FALSE,
                                                          remove = TRUE),
                         polylineOptions = FALSE,
                         circleOptions = FALSE,
                         circleMarkerOptions = FALSE,
                         markerOptions = FALSE)
      })
      

    } else if (input$spatial_domain == "draw_circles") {
      # Clear sf_list
      sf_list <<- rbind()
      # Redraw map and toolbar
      output$map <- renderLeaflet({
        leaflet(width="100%", height="100%") %>% 
          addBaseMapSelector() %>% 
          setView( -96, 38, zoom = 4) %>%
          addDrawToolbar(rectangleOptions = FALSE,
                         editOptions = editToolbarOptions(edit = FALSE,
                                                          remove = TRUE),
                         polygonOptions = FALSE,
                         polylineOptions = FALSE,
                         circleOptions = TRUE,
                         circleMarkerOptions = FALSE,
                         markerOptions = FALSE)
      })
    } else {
      # Clear sf_list
      sf_list <<- rbind()
      # Redraw map without toolbar
      output$map <- renderLeaflet({
        leaflet(width="100%", height="100%") %>% 
          addBaseMapSelector() %>% 
          setView( -96, 38, zoom = 4)
      })
    }
  })
  
  
  ## Draw appropriate polygons when selected via radio buttons
  observe({
    if (input$spatial_domain == "states") {
      output$map <- renderLeaflet({
        leaflet(width="100%", height="100%") %>% 
          setView( -96, 38, zoom = 4) %>% 
          addBaseMapSelector() %>%
          addPolygons(data=states, 
                      fill = TRUE,
                      fillColor = "white", 
                      fillOpacity = 0,
                      color = 'grey',
                      stroke = TRUE, 
                      weight = 1,
                      popup = ~State,
                      layerId = states$State,
                      highlight = highlightOptions(weight = 5,
                                                   color = "red",
                                                   fillOpacity = 0,
                                                   bringToFront = TRUE))
      })
    }
  })
  observe({
    if (input$spatial_domain == "counties") {
      output$map <- renderLeaflet({
        leaflet(width="100%", height="100%") %>% 
          setView( -96, 38, zoom = 4) %>% 
          addBaseMapSelector() %>%
          addPolygons(data=counties, 
                      fill = TRUE,
                      fillColor = "white", 
                      fillOpacity = 0,
                      color = 'grey',
                      stroke = TRUE, 
                      weight = 1,
                      popup = ~County,
                      layerId = counties$County,
                      highlight = highlightOptions(weight = 5,
                                                   color = "red",
                                                   fillOpacity = 0,
                                                   bringToFront = TRUE))
      })
    }
  })
  observe({
    if (input$spatial_domain == "watersheds") {
      output$map <- renderLeaflet({
        leaflet(width="100%", height="100%") %>% 
          setView( -96, 38, zoom = 4) %>% 
          addBaseMapSelector() %>%
          addPolygons(data=huc6, 
                      fill = TRUE,
                      fillColor = "white", 
                      fillOpacity = 0,
                      color = 'grey',
                      stroke = TRUE, 
                      weight = 1,
                      popup = ~HUC6,
                      layerId = huc6$HUC6,
                      highlight = highlightOptions(weight = 5,
                                                   color = "red",
                                                   fillOpacity = 0,
                                                   bringToFront = TRUE))
      })
    }
  })
  observe({
    if (input$spatial_domain == "national_forests") {
      output$map <- renderLeaflet({
        leaflet(width="100%", height="100%") %>% 
          setView( -96, 38, zoom = 4) %>% 
          addBaseMapSelector() %>%
          addPolygons(data=national_forests, 
                      fill = TRUE,
                      fillColor = "white", 
                      fillOpacity = 0,
                      color = 'grey',
                      stroke = TRUE, 
                      weight = 1,
                      popup = ~Forest,
                      layerId = national_forests$Forest,
                      highlight = highlightOptions(weight = 5,
                                                   color = "red",
                                                   fillOpacity = 0,
                                                   bringToFront = TRUE))
      })
    }
  })
  observe({
    if (input$spatial_domain == "usfs_regions") {
      output$map <- renderLeaflet({
        leaflet(width="100%", height="100%") %>% 
          setView( -96, 38, zoom = 4) %>% 
          addBaseMapSelector() %>%
          addPolygons(data=usfs_regions, 
                      fill = TRUE,
                      fillColor = "white", 
                      fillOpacity = 0,
                      color = 'grey',
                      stroke = TRUE, 
                      weight = 1,
                      popup = ~Region,
                      layerId = usfs_regions$Region,
                      highlight = highlightOptions(weight = 5,
                                                   color = "red",
                                                   fillOpacity = 0,
                                                   bringToFront = TRUE))
      })
    }
  })

  
  ## Update dropdowns when clicking on map
  observeEvent(input$map_shape_click, {

    if (input$spatial_domain == "states") {
      # Add/ drop from list
      if (input$map_shape_click$id %in% input$states) {
        selected_states <- input$states[!(input$states %in% input$map_shape_click$id)]
      } else {
        selected_states <- c(input$states, input$map_shape_click$id)
      }
      # Update states in dropdown
      updateSelectInput(session, "states", selected = selected_states)
    }
    if (input$spatial_domain == "counties") {
      # Add/ drop from list
      if (input$map_shape_click$id %in% input$counties) {
        selected_counties <- input$counties[!(input$counties %in% input$map_shape_click$id)]
      } else {
        selected_counties <- c(input$counties, input$map_shape_click$id)
      }
      # Update counties in dropdown
      updateSelectInput(session, "counties", selected = selected_counties)
    }
    if (input$spatial_domain == "watersheds") {
      # Add/ drop from list
      if (input$map_shape_click$id %in% input$watersheds) {
        selected_watersheds <- input$watersheds[!(input$watersheds %in% input$map_shape_click$id)]
      } else {
        selected_watersheds <- c(input$watersheds, input$map_shape_click$id)
      }
      # Update watersheds in dropdown
      updateSelectInput(session, "watersheds", selected = selected_watersheds)
    }
    if (input$spatial_domain == "usfs_regions") {
      # Add/ drop from list
      if (input$map_shape_click$id %in% input$usfs_regions) {
        selected_usfs_regions <- input$usfs_regions[!(input$usfs_regions %in% input$map_shape_click$id)]
      } else {
        selected_usfs_regions <- c(input$usfs_regions, input$map_shape_click$id)
      }
      # Update usfs_regions in dropdown
      updateSelectInput(session, "usfs_regions", selected = selected_usfs_regions)
    }
    if (input$spatial_domain == "national_forests") {
      # Add/ drop from list
      if (input$map_shape_click$id %in% input$national_forests) {
        selected_national_forests <- input$national_forests[!(input$national_forests %in% input$map_shape_click$id)]
      } else {
        selected_national_forests <- c(input$national_forests, input$map_shape_click$id)
      }
      # Update national_forests in dropdown
      updateSelectInput(session, "national_forests", selected = selected_national_forests)
    }
  })

  
  ## Update reactive lists when selecting from dropdown
  observe({
    updateSelectInput(session, "states")
  })
  observe({
    updateSelectInput(session, "counties")
  })
  observe({
    updateSelectInput(
      session, "watersheds")
  })
  observe({
    updateSelectInput(
      session, "usfs_regions")
  })
  observe({
    updateSelectInput(
      session, "national_forests")
  })
  
  
  ## Get feature count
  observeEvent(input$map_draw_all_features, {
    output$n_features <- reactive({length(input$map_draw_all_features$features)})
  })

  
  # Read in shapefiles and display with leaflet
  # Most of this code comes from this book:
  # https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html
  observe({
    
    map <- getShapefile(input$shapefile)
    if(!is.null(map)) {
      map <- map %>%
        sf::st_as_sf() %>%
        sf::st_transform(4326) %>%
        sf::st_make_valid()
    }
    # map <- getShapefile(input$shapefile)

    # Select states
    if(!is.null(map)) {
      # Find which states the polygon(s) overlaps
      states_overlap <- sf::st_intersects(states, map, sparse = FALSE)
      selected_states <- states$State[unlist(apply(states_overlap, 1, FUN = function(x) {any(x)}))]
    }

    if(!is.null(map)) {
      output$map <- renderLeaflet({
        # https://www.victormandela.com/project/interactive-maps-with-leaflet-shapefile-and-leaflet/
        leaflet(map) %>%
          addBaseMapSelector() %>%
          setView( -96, 38, zoom = 4) %>%
          addPolygons(color = 'grey',
                      weight = 1,
                      fillOpacity = 0,
                      fillColor = "white",
                      highlight = highlightOptions(weight = 5,
                                                   color = "red",
                                                   fillOpacity = 0,
                                                   bringToFront = TRUE))
      })
    }
  })


  
  # TAB 2
  # Populate "How to group estimates" dropdown
  observe({
    if(!is.null(input$dataType)){
      carbonSelected <- "carbon" %in% c(input$dataType)
    } else {
      carbonSelected <- FALSE
    }
    
    updateSelectInput(
      session, "groupBy", "",
      choices = getGroupByOptions(grouping.variables.df, carbonSelected, FALSE)
    )
    
  })
  
  # # Populate "Area domain" dropdown
  # observe({
  #   # Don't use tree variables in area domain
  #   carbonSelected <- TRUE
  #   
  #   updateSelectInput(
  #     session, "areaDomainVariable", "Variable:",
  #     # Return values of list without names; sort codes
  #     choices = sort(getGroupByOptions(grouping.variables.df, carbonSelected, FALSE))
  #   )
  #   
  # })
  # 
  # # Area domain equation editor
  # areaDomainEquations <- reactiveValues(
  #   eq = c()
  # )
  # observeEvent(input$areaDomainButton, {
  #   if (input$areaDomainComparator != "%in%") {
  #     equation <- paste(input$areaDomainVariable, input$areaDomainComparator, input$areaDomainValue)
  #   } else {
  #     equation <- paste0(input$areaDomainVariable, " ", input$areaDomainComparator, 
  #                       " c(", input$areaDomainValueMin, ",", input$areaDomainValueMax, ")")
  #   }
  #   
  #   output$areaDomainOut <- renderText({
  #     equation
  #   })
  #   
  #   areaDomainEquations$eq <- c(equation)
  # })
  # 
  # observeEvent(input$areaDomainButtonOr, {
  #   if (input$areaDomainComparator != "%in%") {
  #     equation <- paste(input$areaDomainVariable, input$areaDomainComparator, input$areaDomainValue)
  #   } else {
  #     equation <- paste0(input$areaDomainVariable, " ", input$areaDomainComparator, 
  #                        " c(", input$areaDomainValueMin, ",", input$areaDomainValueMax, ")")
  #   }
  #   
  #   areaDomainEquations$eq <- c(areaDomainEquations$eq, "|", equation)
  #   
  #   output$areaDomainOut <- renderText({
  #     paste(areaDomainEquations$eq)
  #   })
  # })
  # 
  # observeEvent(input$areaDomainButtonAnd, {
  #   if (input$areaDomainComparator != "%in%") {
  #     equation <- paste(input$areaDomainVariable, input$areaDomainComparator, input$areaDomainValue)
  #   } else {
  #     equation <- paste0(input$areaDomainVariable, " ", input$areaDomainComparator, 
  #                        " c(", input$areaDomainValueMin, ",", input$areaDomainValueMax, ")")
  #   }
  #   
  #   areaDomainEquations$eq <- c(areaDomainEquations$eq, "&", equation)
  #   
  #   output$areaDomainOut <- renderText({
  #     paste(areaDomainEquations$eq)
  #   })
  # })
  # 
  # observeEvent(input$areaDomainButtonClear, {
  #   areaDomainEquations$eq <- c()
  #   
  #   output$areaDomainOut <- renderText({
  #     ""
  #   })
  # })
  # 
  # 
  # # Populate "Tree domain" dropdown
  # observe({
  #   # Use tree variables in area domain
  #   carbonSelected <- FALSE
  #   
  #   updateSelectInput(
  #     session, "treeDomainVariable", "Variable:",
  #     # Return values of list without names; sort codes
  #     choices = sort(getGroupByOptions(grouping.variables.df, carbonSelected, FALSE))
  #   )
  #   
  # })
  # 
  # # Tree domain equation editor
  # treeDomainEquations <- reactiveValues(
  #   eq = c()
  # )
  # observeEvent(input$treeDomainButton, {
  #   if (input$treeDomainComparator != "%in%") {
  #     equation <- paste(input$treeDomainVariable, input$treeDomainComparator, input$treeDomainValue)
  #   } else {
  #     equation <- paste0(input$treeDomainVariable, " ", input$treeDomainComparator, 
  #                        " c(", input$treeDomainValueMin, ",", input$treeDomainValueMax, ")")
  #   }
  #   
  #   output$treeDomainOut <- renderText({
  #     equation
  #   })
  #   
  #   treeDomainEquations$eq <- c(equation)
  # })
  # 
  # observeEvent(input$treeDomainButtonOr, {
  #   if (input$treeDomainComparator != "%in%") {
  #     equation <- paste(input$treeDomainVariable, input$treeDomainComparator, input$treeDomainValue)
  #   } else {
  #     equation <- paste0(input$treeDomainVariable, " ", input$treeDomainComparator, 
  #                        " c(", input$treeDomainValueMin, ",", input$treeDomainValueMax, ")")
  #   }
  #   
  #   treeDomainEquations$eq <- c(treeDomainEquations$eq, "|", equation)
  #   
  #   output$treeDomainOut <- renderText({
  #     paste(treeDomainEquations$eq)
  #   })
  # })
  # 
  # observeEvent(input$treeDomainButtonAnd, {
  #   if (input$treeDomainComparator != "%in%") {
  #     equation <- paste(input$treeDomainVariable, input$treeDomainComparator, input$treeDomainValue)
  #   } else {
  #     equation <- paste0(input$treeDomainVariable, " ", input$treeDomainComparator, 
  #                        " c(", input$treeDomainValueMin, ",", input$treeDomainValueMax, ")")
  #   }
  #   
  #   treeDomainEquations$eq <- c(treeDomainEquations$eq, "&", equation)
  #   
  #   output$treeDomainOut <- renderText({
  #     paste(treeDomainEquations$eq)
  #   })
  # })
  # 
  # observeEvent(input$treeDomainButtonClear, {
  #   treeDomainEquations$eq <- c()
  #   
  #   output$treeDomainOut <- renderText({
  #     ""
  #   })
  # })
  
  
  ## Save results as a reactive value so we can download with out recomputing
  react_vals <- reactiveValues(raw_df = NULL)
  
  ## When someone clicks get results, hand inputs to rFIA
  observeEvent(input$get_results, {
    
    
    ## Load spatial data/ get overlapping states
    aoi.prep <- prep_aoi(spatial_domain = input$spatial_domain,
                         merge_spatial_domain = input$merge_spatial_domain,
                         inputStates = input$states,
                         inputCounties = input$counties,
                         inputHUC6 = input$watersheds,
                         inputForests = input$national_forests,
                         inputRegions = input$usfs_regions,
                         states = states,
                         counties = counties,
                         huc6 = huc6,
                         national_forests = national_forests,
                         usfs_regions = usfs_regions,
                         drawn_shapes = input$map_draw_all_features$features,
                         uploaded_shapes = input$shapefile)
    aoi <- aoi.prep[[1]]
    selected_states <- aoi.prep[[2]]
    
    
    
    ## Assuming aoi has been defined already, proceed w/ estimation
    if (length(selected_states) > 0) {
      
      ## Set up remote FIA.Database
      db <- readFIA(states = selected_states, 
                    dir = input$fia_dir, 
                    inMemory = TRUE)
      
      ## If most recent subset, clip
      if (input$most_recent) db <- clipFIA(db, mostRecent = TRUE)
      
      ## Generate estimates
      if (input$dataType == 'carbon') {

        df <- rFIA::carbon(db = db, 
                           polys = aoi,
                           byPool = as.logical(input$byPool),
                           landType = input$landType,
                           grpBy = eval(input$groupBy),
                           #areaDomain = eval(parse(text = input$areaDomain)),
                           method = input$method,
                           lambda = input$lambda,
                           totals = TRUE)
        
      } else if (input$dataType == 'area') {
      
        df <- rFIA::area(db = db, 
                         polys = aoi,
                         landType = input$landType,
                         grpBy = eval(input$groupBy),
                         #areaDomain = eval(parse(text = input$areaDomain)),
                         method = input$method,
                         lambda = input$lambda,
                         totals = TRUE)
        
      } else if (input$dataType == 'tpa') {
        df <- rFIA::tpa(db = db, 
                        polys = aoi,
                        landType = input$landType,
                        grpBy = eval(input$groupBy),
                        treeType = input$treeType,
                        #areaDomain = eval(parse(text = input$areaDomain)),
                        #treeDomain = eval(parse(text = input$treeDomain)),
                        method = input$method,
                        lambda = input$lambda,
                        totals = TRUE)
        
      } else if (input$dataType == 'volume') {
        df <- rFIA::volume(db = db, 
                           polys = aoi,
                           landType = input$landType,
                           grpBy = eval(input$groupBy),
                           treeType = input$treeType,
                           #areaDomain = eval(parse(text = input$areaDomain)),
                           #treeDomain = eval(parse(text = input$treeDomain)),
                           method = input$method,
                           lambda = input$lambda,
                           totals = TRUE) %>%
          dplyr::select(-dplyr::starts_with('SAW_CF'))
        
      } else if (input$dataType == 'biomass') {
        df <- rFIA::biomass(db = db, 
                            polys = aoi,
                            landType = input$landType,
                            grpBy = eval(input$groupBy),
                            treeType = input$treeType,
                            #areaDomain = eval(parse(text = input$areaDomain)),
                            #treeDomain = eval(parse(text = input$treeDomain)),
                            method = input$method,
                            lambda = input$lambda,
                            totals = TRUE) %>%
          ## This will get confusing for folks, save for rFIA
          dplyr::select(-dplyr::starts_with('CARB'))
      }


      ## Time range filter - faster to do this than subset ahead of time
      if (!input$most_recent) {
        df <- df %>%
          dplyr::filter(YEAR >= lubridate::year(input[['years_range']][1]),
                        YEAR <= lubridate::year(input[['years_range']][2]))
      }
      
      ## formatting output dataframe
      df <- df %>% 
        dplyr::mutate_if(is.numeric, round, 2) %>%
        dplyr::mutate(dplyr::across(dplyr::ends_with('TOTAL'), round, 0)) %>%
        dplyr::select(-dplyr::any_of(c('N', 'nPlots_TREE')))
      
      df.names <- data.frame(rfia_name = tolower(names(df))) %>%
        dplyr::left_join(output.variables.df, by = 'rfia_name') %>%
        # Protect names of user uploaded files
        dplyr::mutate(cole_name = dplyr::case_when(is.na(cole_name) ~ rfia_name,
                                                   TRUE ~ cole_name)) %>%
        dplyr::mutate(cole_name = stringr::str_to_sentence(cole_name))
      names(df) <- df.names$cole_name
      
      ## Set up example code
      output.string <- write_rFIA_code(input$dataType,
                                       input$spatial_domain,
                                       input$merge_spatial_domain,
                                       input$shapefile,
                                       selected_states,
                                       input$fia_dir,
                                       input$most_recent,
                                       input$byPool,
                                       input$treeType,
                                       input$landType,
                                       input$groupBy,
                                       input$method,
                                       input$lambda)
      
      
      ## Render output
      react_vals$raw_df <- df
      output$result_table <- DT::renderDataTable(semantic_DT(df))
      output$function_string <- renderText(output.string)
      
    }
    
    
  })

  
  ## Download results
  output$download_results <- downloadHandler(
        filename = function() {
          paste("cole_results", ".csv", sep = "")
        },
        content = function(file) {
          write.csv(react_vals$raw_df, file, row.names = FALSE)
        }
      )
  
  
})

shinyApp(ui = ui, server = server)
