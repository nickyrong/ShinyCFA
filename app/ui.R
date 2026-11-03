rm(list=ls())
library(shiny)
library(leaflet)
library(dygraphs)
library(plotly)
library(shinythemes)
library(waiter) #loading screen/spinner


options(shiny.sanitize.errors = TRUE)
linebreaks <- function(n){HTML(strrep(br(), n))}


  
# Define UI
shinyUI(fluidPage(
  
  use_waiter(),
  waiter_show_on_load(html = spin_fading_circles(), color = "black"), # place at the top before content
  
  
  theme = shinytheme("yeti"),
  tags$head(HTML("<title>Hydrometric Data Tool for WSC HYDAT</title>")),

  # Change font color of error message to red
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
        font-weight: bold;
      }
    "))
  ),
  
  # SideBar UI configuration
  pageWithSidebar(
    
    headerPanel(title=div("Hydrometric Data Tool for WSC HYDAT")
  ),

    
    sidebarPanel(
      width = 3,
      h6(htmlOutput('HYDAT_version')),

  # Use server-side selectize for large option set
  selectizeInput("stn_id_input", label = h3("WSC Station ID"),
         choices = NULL,
         selected = NULL,
         multiple = FALSE,
         # maxOptions caps how many matches the dropdown renders. A huge value
         # made typing a common prefix (e.g. "0") try to draw thousands of DOM
         # nodes and freeze the browser; 50 keeps it snappy while server-side
         # search still finds any station.
         options = list(placeholder = "Type to search WSC Station ID…",
            openOnFocus = FALSE,
            maxOptions = 50,
            loadThrottle = 300)),
      
      selectInput("wy_start", h3("Start of Water Year"), 
                  c("January" = 1, "February" = 2, "March" = 3,
                    "April" = 4, "May" = 5, "June" = 6,
                    "July" = 7, "August" = 8, "September" = 9,
                    "October" = 10, "November" = 11, "December" = 12),
                  selected = "January"),
      br(),
      h3("Station Info"),
      h6(htmlOutput("stn_input_info")),

      # Catchment boundary export. Only rendered once a station is picked; the
      # buttons themselves additionally wait on output.basin_available, since the
      # WSC basin GeoPackages are optional (see read_basin_raw() in functions.R).
      conditionalPanel(
        condition = "input.stn_id_input",
        wellPanel(
          h4("Export Watershed Polygon"),
          h6(htmlOutput("basin_export_info")),
          conditionalPanel(
            condition = "output.basin_available",
            div(style = "display:flex; flex-wrap:wrap; gap:6px;",
                downloadButton("download_basin_shp", "Shapefile (.zip)", class = "btn-sm"),
                downloadButton("download_basin_kml", "Google Earth (.kml)", class = "btn-sm"))
          )
        )
      ),
      
      # Add this to your sidebar panel, replacing or updating the current Station Info section
      conditionalPanel(
        condition = "output.has_selected_station",
        wellPanel(
          h4("Selected Station"),
          verbatimTextOutput("selected_station_info"),
          actionButton("clear_station", "Clear Selection", class = "btn-warning btn-sm")
        )
      ),
      
      # Add this to the sidebar panel near the existing station selection controls
      conditionalPanel(
        condition = "output.has_selected_station",
        wellPanel(
          h4("Selected Station from Map"),
          verbatimTextOutput("selected_station_info"),
          actionButton("clear_station", "Clear Selection", class = "btn-warning btn-sm")
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Read Me",
                 # htmlOutput("README") #it is technically a markdown render but HTML works
                 div(
                   h2("Authors"),
                   tags$ul(
                     tags$li(tags$a("Nick Rong", href = "https://github.com/nickyrong"))
                   ),

                   h2("Overview"),
                   p("This app allows the user to enter a Water Survey of Canada (WSC) station ID and access streamflow data from the ",
                     tags$a("Hydat Database", href = "https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html"),
                     ". The data can be summarized in tabular or graphical formats, and flood frequency analysis (FFA), regional analysis, and an average (regime) hydrograph can also be produced. The published HYDAT record can optionally be extended toward the present with ",
                     strong("provisional realtime unit-value data"), " (see below)."),

                   p("The functionality to extract data from the Hydat Database in this app is provided by the ",
                     tags$a("tidyhydat package", href = "https://github.com/ropensci/tidyhydat"), "."),

                   tags$br(),

                   h2("App Functionality:"),
                   p("Currently the app allows the user to:"),
                   tags$ul(
                     tags$li("View WSC hydrometric station locations on an interactive map, filterable by station name and by year of data coverage, and select a station directly from the map"),
                     tags$li("View a station's drainage basin on the map and export the polygon as a Shapefile or Google Earth KML"),
                     tags$li("Select a hydrometric station and summarize streamflow data in a table (daily, monthly, yearly) and download the summarized data"),
                     tags$li("View an interactive daily-discharge time series coloured by data-quality flag"),
                     tags$li("View an average (regime) hydrograph by day of the water year: a mean line with a user-selectable shaded percentile band and mean-annual-discharge (MAD) line, with the record optionally split into multiple periods for comparison and trimmed by water-year bounds"),
                     tags$li("Overlay the most recent 365 days on the hydrograph (“Plot Current Year”), using the provisional realtime data"),
                     tags$li("Run a Flood Frequency Analysis on the annual daily or instantaneous maximum streamflow series using various distributions"),
                     tags$li("Perform a Regional Analysis: pool stations automatically from a target location (BC hydrologic zones inside BC, or a Region-of-Influence donor group elsewhere in Canada), by BC hydrologic zone, or by manual selection — then build a regional flood-frequency envelope curve (with an automated power-law envelope fit) and characterise the regional instantaneous-to-daily peak ratio (Qinst/Qdaily)"),
                     tags$li("Retrieve provisional realtime unit-value data from WSC to extend the summary table, time series, and hydrograph beyond the published HYDAT record")
                   ),

                   tags$br(),

                   h2("Provisional Realtime Unit-Value Data:"),
                   p("The published HYDAT record typically lags the present by one to two years. To bridge that gap, the ",
                     strong("Explore Data"), " tab can retrieve provisional “unit value” (sub-daily) data from two ECCC sources: the ",
                     tags$a("WSC hydrometric collaboration site",
                            href = "https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/UnitValueData/",
                            target = "_blank"),
                     " (a static archive back to 2011, with quality flags) and the ECCC realtime web service (via ",
                     tags$a("tidyhydat", href = "https://github.com/ropensci/tidyhydat"),
                     "’s realtime_ws, ~18 months, updated to now). ",
                     "These data are aggregated to complete-day daily averages and appended after the last HYDAT day."),
                   p("Only days after the end of the HYDAT record are used, and for those days the app fills forward by priority: ",
                     strong("HYDAT → corrected → raw → realtime web service"),
                     " — the corrected archive signal is preferred, raw fills where corrected is unavailable, and the realtime ",
                     "web service fills the most recent tail (from the last archive day up to today) that the static archive cannot yet reach. ",
                     "Web-service days carry no grading code (shown as “Unspecified”, source “Realtime (WS)”); archive days ",
                     "carry a data-quality flag using the WSC grading codes ",
                     "(Unspecified, Ice, Estimated, Partial Day, Dry, Revised, Undefined, Unusable); see the ",
                     tags$a("data disclaimer & grading codes (PDF)",
                            href = paste0("https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/",
                                          "www/UnitValueData/UnitValueData_Disclaimer.pdf"),
                            target = "_blank"), "."),
                   p(strong("Important: "),
                     "provisional data are static, unapproved, and subject to revision. They are provided to visualise recent conditions only and are ",
                     strong("deliberately excluded"),
                     " from the Flood Frequency Analysis and the Regional Analysis, which use the published HYDAT record exclusively."),

                   tags$br(),

                   h2("Frequency Analysis Methods:"),
                   p("The frequency distribution(s) are fitted using L-moments method from R package ", 
                     tags$a("lmom", href = "https://cran.r-project.org/web/packages/lmom/index.html"), 
                     " by J. R. M. Hosking. L-moments of the sample data are calculated and distribution parameters are then estimated from the calculated L-moments."),
                   
                   p("Return periods of annual maximum discharge are calculated using the Weibull formula: ", 
                     tags$em("Tr = (N+1) / m"), " where ", tags$em("N"), " is the sample size and ", 
                     tags$em("m"), " is the rank. Probability of Non-exceedance is the inverse of return period: ", 
                     tags$em("P = 1 / Tr"), ". The frequency distribution plot x-axis has been transformed into probabilistic scale: ", 
                     tags$em("η = −log(−log(P)) = −log[−log(1 − 1/Tr)]"), " where ", 
                     tags$em("η"), " is the reduced variate."),
                   
                   strong("Further readings:"), tags$br(),
                   tags$ul(
                     tags$li("Hosking, J., & Wallis, J. (1997). Regional Frequency Analysis: An Approach Based on L-Moments. Cambridge: Cambridge University Press. doi:10.1017/CBO9780511529443"),
                     tags$li("Makkonen, L. (2006). Plotting Positions in Extreme Value Analysis, Journal of Applied Meteorology and Climatology, 45(2), 334-340. doi:10.1175/JAM2349.1")
                   ),
                   
                   tags$br(),
                   
                   h2("Feedback"),
                   p("For bugs and feature requests, please ",
                     tags$a("create an issue on GitHub", href = "https://github.com/nickyrong/ShinyCFA/issues"), "."),
                   p("Please note that this project was created under a learning objective and it is provided in the event it is useful to others. Accordingly, it is not necessarily under active development."),

                   tags$br(),

                   h3("📄 License & Disclaimer", style = "color: #34495e;"),
                   div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; border-left: 4px solid #6c757d;",
                     h5("License", style = "color: #495057;"),
                     p("Copyright © 2026 Nick Rong"),
                     div(style = "display: flex; align-items: center; margin: 10px 0;",
                       p("Released under the License: GNU General Public License v3.0", style = "margin: 0; margin-right: 10px;"),
                       a(href = "https://www.gnu.org/licenses/gpl-3.0", target = "_blank",
                         img(src = "https://www.gnu.org/graphics/gplv3-88x31.png",
                             alt = "GNU GPL v3",
                             style = "height: 20px; width: auto;")
                       )
                     ),
                     p("Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an \"AS IS\" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License."),
                     hr(style = "margin: 15px 0; border-color: #dee2e6;"),
                     h5("Disclaimer", style = "color: #495057;"),
                     p(style = "font-size: 0.9em; color: #6c757d;", "This application is provided for research and educational purposes. While we strive for accuracy, users should verify data for critical applications. The developers are not responsible for any decisions made based on the data provided through this application.")
                   ),

                   tags$br()
                 )
        ),
        
        tabPanel("Stations Map",
                 br(),
                 actionButton("map_filter_toggle", "Station Filters",
                              icon = icon("filter"),
                              class = "btn-info btn-sm"),
                 em(textOutput("map_filter_count", inline = TRUE)),
                 conditionalPanel(
                   condition = "input.map_filter_toggle % 2 == 1",
                     wellPanel(
                       fluidRow(
                         column(6,
                           textInput("map_filter_name", "Station Name Contains:",
                                     value = "", placeholder = "e.g. Fraser, Columbia, Creek...")
                         )
                       ),
                       fluidRow(
                         column(3,
                           h5("Year Coverage"),
                           helpText("Show stations with data spanning these years."),
                           numericInput("map_filter_year_from", "Data available from year \u2264:",
                                        value = NA, min = 1800, max = 2030, step = 1),
                           numericInput("map_filter_year_to",   "Data available through year \u2265:",
                                        value = NA, min = 1800, max = 2030, step = 1)
                         ),
                         column(3,
                           h5("Record Length"),
                           helpText("Minimum years of daily flow record."),
                           numericInput("map_filter_min_record", "Minimum record length (years):",
                                        value = NA, min = 1, max = 200, step = 1)
                         ),
                         column(3,
                           h5("Drainage Area (km\u00b2)"),
                           helpText("Gross drainage area range."),
                           numericInput("map_filter_da_min", "Minimum drainage area:",
                                        value = NA, min = 0, step = 1),
                           numericInput("map_filter_da_max", "Maximum drainage area:",
                                        value = NA, min = 0, step = 1)
                         ),
                         column(3,
                           h5("Distance Filter"),
                           helpText("Show stations within a radius of a point."),
                           numericInput("map_filter_lat",     "Latitude:",
                                        value = NA, min = 41, max = 84, step = 0.001),
                           numericInput("map_filter_lon",     "Longitude:",
                                        value = NA, min = -142, max = -51, step = 0.001),
                           numericInput("map_filter_dist_km", "Max distance (km):",
                                        value = NA, min = 0, step = 10)
                         )
                       ),
                       actionButton("map_filter_reset", "Reset Filters",
                                    class = "btn-default btn-sm")
                     )
                 ),
                 br(),
                 leafletOutput("MapPlot", height = 700),
                 br(),
                 "Zoom into map to see station locations. Click on a marker to see station details and select it."
        ),
        
        
        tabPanel("Explore Data",
                 br(),
                 wellPanel(
                   fluidRow(
                     column(7,
                            strong("Provisional realtime unit-value data (WSC collaboration site)"),
                            p(class = "text-muted", style = "margin-top:4px;",
                              "Extend the Summary Table, Time Series, and Hydrograph beyond the ",
                              "published HYDAT record using provisional sub-daily data (corrected ",
                              "where available, otherwise raw). This provisional data is ",
                              strong("excluded"), " from the flood frequency and regional analyses. ",
                              tags$a("Disclaimer & grading codes (PDF)",
                                     href = paste0("https://collaboration.cmc.ec.gc.ca/cmc/",
                                                   "hydrometrics/www/UnitValueData/UnitValueData_Disclaimer.pdf"),
                                     target = "_blank"), ".")
                     ),
                     column(5,
                            conditionalPanel(
                              condition = "!output.realtime_loaded",
                              actionButton("retrieve_realtime", "Retrieve Realtime Unit Data from WSC",
                                           icon = icon("cloud-download-alt"), class = "btn-primary btn-sm")
                            ),
                            conditionalPanel(
                              condition = "output.realtime_loaded",
                              actionButton("remove_realtime", "Remove Realtime Unit Data from WSC",
                                           icon = icon("trash-alt"), class = "btn-warning btn-sm")
                            ),
                            br(), br(),
                            uiOutput("realtime_status")
                     )
                   )
                 ),

                 #Sub-tabs
                 tabsetPanel(
                  
                   #---- Sub tab: Data Summary Table
                   tabPanel("Summary Table",
                            br(),
                            fluidRow(
                              column(3,
                                     selectInput("sum_period", 
                                                 h4("Select Daily, Monthly, or Yearly Summary"),
                                                 c("Daily" = "Daily", 
                                                   "Monthly" = "Monthly", 
                                                   "Yearly" = "Yearly"),
                                                 selected = "Daily"
                                     )
                              ),
                              
                              column(3, 
                                     downloadButton("downloadSummary", "Download Summary")
                              ),
                              column(3 #empty placeholder
                                     
                              ),
                              column(3 #empty placeholder
                                     
                              )
                            ),# End of fluidRow
                            
                            # Vertical alignment for the two
                            tags$style(type='text/css', "#sum_period {margin-top: 25px;}"),
                            tags$style(type='text/css', "#downloadSummary {margin-top: 60px;}"),
                            
                            br(),
                            DT::dataTableOutput("table")
                            
                   ), # End of Data Summary Table sub-tab
                   
                   #---- Sub tab: Time Series
                   tabPanel("Time Series",
                            br(),
                            "This is an interactive graph: drag to zoom in and double click to zoom out",
                            br(),
                            h3("Daily Average Discharge Data"),
                            dygraphOutput(outputId = "tsgraph", height = "600px"),
                            br()
                            
                   ), # End of Time Series sub-tab
                   
                   
                   #---- Sub tab: Average Hydrograph (regime)
                   tabPanel("Hydrograph",
                            br(),
                            p("Average daily hydrograph by day of the water year. ",
                              "Shade a percentile band to show flow variability, and optionally ",
                              "split the record into periods to compare (each period gets its own ",
                              "mean line, band, and mean-annual-discharge (MAD) line). ",
                              "Use the water-year bounds to exclude sparse early/late years ",
                              "(e.g. isolated years before a long gap)."),
                            wellPanel(
                              fluidRow(
                                column(4,
                                       sliderInput("hydro_band", "Shaded percentile band (%)",
                                                   min = 0, max = 100, value = c(10, 90), step = 1)
                                ),
                                column(5,
                                       selectizeInput("hydro_splits",
                                                      "Split into periods at water year(s):",
                                                      choices = NULL, multiple = TRUE,
                                                      options = list(placeholder = "leave empty for the whole record")),
                                       fluidRow(
                                         column(6,
                                                selectizeInput("hydro_year_min",
                                                               "Exclude before water year:",
                                                               choices = NULL,
                                                               options = list(placeholder = "earliest",
                                                                              plugins = list("remove_button")))
                                         ),
                                         column(6,
                                                selectizeInput("hydro_year_max",
                                                               "Exclude after water year:",
                                                               choices = NULL,
                                                               options = list(placeholder = "latest",
                                                                              plugins = list("remove_button")))
                                         )
                                       )
                                ),
                                column(3,
                                       br(),
                                       checkboxInput("hydro_show_band", "Show percentile band", TRUE),
                                       checkboxInput("hydro_show_mad", "Show MAD line(s)", TRUE),
                                       conditionalPanel(
                                         condition = "output.realtime_loaded",
                                         actionButton("hydro_current_year", "Plot Current Year",
                                                      icon = icon("calendar-day"), class = "btn-info btn-sm"),
                                         helpText("Overlays the last 365 days of available data (needs ",
                                                  "realtime unit data retrieved on the Explore Data tab).")
                                       )
                                )
                              )
                            ),
                            plotlyOutput("hydrograph", height = "560px"),
                            br()
                   ) # End of Hydrograph sub-tab
                   
                   #---- Sub tab: Trend Test
                   # tabPanel("Trends",
                   #          "Trend analysis provided by the [FlowScreen Package](https://cran.r-project.org/web/packages/FlowScreen/index.html)",
                   #          br(), 
                   #          shinycssloaders::withSpinner( 
                   #            plotOutput("trends")
                   #          ),#End of busy indicator
                   #          br()
                   # ) # End of Trend sub-tab
                   
                 ) # End of all sub-tabs
        ), # End of Explore Data tab
        
        
        
  tabPanel("Flood Frequency Analysis",
                 
                 #Sub-tabs
                 tabsetPanel(
                   
                   #---- Sub tab: Qdaily
                   tabPanel("Daily Q",
                            br(),
                            h2('1. Settings'),
                            fluidRow(
                              column(4,
                                     selectizeInput("remove_year_Qdaily", "Select Years to Remove", 
                                                    selected = NULL,
                                                    choices = NULL,
                                                    multiple= TRUE
                                     )
                              ),
                              column(4,
                                     sliderInput('selector_days', "Complete Year Threshold",
                                         min = 0, max = 365, value = 355)
                              ),
                              column(4, 
                                     selectizeInput("months_Qdaily", "Select Months to Include", 
                                                    selected = base::month.abb,
                                                    choices = base::month.abb,
                                                    multiple= TRUE
                                     )
                              )
                            ), # End of FluidRow
                            
                            htmlOutput("complete_years_Qdaily"),
          
                            br(),
                            
                            h2('2. Empirical Frequency'),
                            h5("Descriptive Statistics (pastecs::stat.desc):"),
                            verbatimTextOutput("daily_simplestats", placeholder = FALSE),
                            br(),
                            DT::dataTableOutput("daily_ams_table"),
                            br(),
                            plotlyOutput("daily_ams_fig"),
                            br(),
                            
                            h2('3. Analytical Frequency Distribution'),
                            fluidRow(
                              column(6,
                                     selectizeInput('daily_selector_dist', 'Select Distributions',
                                                    choices = NULL, multiple = TRUE)),
                              column(6,
                                     selectizeInput('daily_selector_Tr', 'Select Return Periods (Years)',
                                                    choices = c(2, 5, 10, 20, 25, 30, 40, 50, 
                                                                75, 100, 200, 250, 300,
                                                                400, 500, 1000, 2500, 10000), 
                                                    multiple = TRUE,
                                                    options = list(maxItems = 10), selected = 2))
                              ),
                            h5("Sampled Linear Moments & Ratios (lmom::samlmu):"),
                            verbatimTextOutput("daily_lmoments", placeholder = FALSE),
                            DT::dataTableOutput("daily_dist_table"),
                            br(),
                            plotlyOutput("daily_dist_fig"),
                            
                            linebreaks(30)

                   ), # End of Q daily Frequency sub-tab
                   
                   #---- Sub tab: Frequency Distribution
                   tabPanel("Instantaneous Q",
                            br(),
                            code('Annual Instantaneous Maxs were pre-determined by WSC using calendar year, therefore Water Year setting is not in effect.'),
                            h2('1. Settings'),
                            fluidRow(
                              column(4,
                                     selectizeInput("remove_year_Qinst", "Select Years to Remove", 
                                                    selected = NULL,
                                                    choices = NULL,
                                                    multiple= TRUE
                                     )
                              ),
                              column(4 #placeholder
                              ),
                              column(4 #placeholder
                              )
                            ), # End of FluidRow
                            htmlOutput("selected_years_Qinst"),
                            br(),
                            
                            h2('2. Empirical Frequency'),
                            h5("Descriptive Statistics (pastecs::stat.desc):"),
                            verbatimTextOutput("inst_simplestats", placeholder = FALSE),
                            br(),
                            DT::dataTableOutput("inst_ams_table"),
                            br(),
                            plotlyOutput("inst_ams_fig"),
                            br(),
                            
                            h2('3. Analytical Frequency Distribution'),
                            fluidRow(
                              column(6,
                                     selectizeInput('inst_selector_dist', 'Select Distributions',
                                                    choices = NULL, multiple = TRUE)),
                              column(6,
                                     selectizeInput('inst_selector_Tr', 'Select Return Periods (Years)',
                                                    choices = c(2, 5, 10, 20, 25, 30, 40, 50, 
                                                                75, 100, 200, 250, 300,
                                                                400, 500, 1000, 2500, 10000), 
                                                    multiple = TRUE,
                                                    options = list(maxItems = 10), selected = 2))
                            ),
                            h5("Sampled Linear Moments & Ratios (lmom::samlmu):"),
                            verbatimTextOutput("inst_lmoments", placeholder = FALSE),
                            DT::dataTableOutput("inst_dist_table"),
                            br(),
                            plotlyOutput("inst_dist_fig"),
                            
                            linebreaks(30)

                   ) # End of Q Inst sub-tab
                   

                 ) # End of all sub-tabs
        ),
        
        tabPanel("Regional Analysis",
                 br(),

                 # Sub-tabs for Regional Analysis
                 tabsetPanel(
                   tabPanel("Usage",
                            br(),
                            h3("How to use Regional Analysis"),

                            div(
                              style = paste0("border-left: 6px solid #d9534f; background: #fdf3f2; ",
                                             "padding: 12px 16px; margin: 10px 0 18px 0; border-radius: 4px;"),
                              tags$span(style = "font-size: 1.15em; margin-right: 6px;", "⚠️"),
                              tags$b("Advanced feature — expert review required. "),
                              "Regional flood-frequency analysis (especially the automated Region-of-Influence ",
                              "pooling and the ungauged flood-quantile estimate) involves significant methodological ",
                              "judgement. ", tags$b("Before using any output here in a hydrology deliverable, consult ",
                              "Jaime Cathcart and Nick Rong for guidance."),
                              " Treat all results as screening-level until they have been reviewed."
                            ),

                            h4("1. What is regional analysis, and why do it?"),
                            p("A flood-frequency analysis at a single gauge answers: ",
                              tags$em("\"how big is the 1-in-100-year flood here?\""),
                              " The trouble is that most sites have only a few decades of record (or none at all, if the ",
                              "site is ", tags$b("ungauged"), "), which makes rare-event estimates uncertain. ",
                              tags$b("Regional analysis"), " borrows strength from ", tags$em("other"),
                              " gauges that behave hydrologically like your site — collectively a ",
                              tags$b("pooling group"), " or ", tags$b("region"),
                              " — so the estimate rests on many more station-years of data. This tab helps you (a) ",
                              "build that pooling group and (b) turn it into regional flood estimates and diagnostics."),

                            h4("2. Step one — build your station set (the 'Regionalization' sub-tab)"),
                            p("Everything else on this tab uses whatever stations you gather here. There are three ways to ",
                              "do it; you can mix them (e.g. auto-select, then hand-edit)."),
                            tags$ul(
                              tags$li(tags$b("Option A — automatically, from a location."),
                                      " Type a latitude/longitude or click the mini-map to drop a target pin. The app then ",
                                      "chooses the pooling group for you and redraws the map ", tags$b("live"),
                                      " as you change any setting:",
                                      tags$ul(
                                        tags$li(tags$b("Inside British Columbia"),
                                                " it finds the BC hydrologic zone that contains your point and adds ",
                                                "neighbouring zones out to a distance you choose."),
                                        tags$li(tags$b("Anywhere else in Canada"),
                                                " it computes a ", tags$b("Region of Influence (ROI)"),
                                                " — the gauges most similar to your site in geography ",
                                                tags$em("and"), " catchment character."))),
                              tags$li(tags$b("Option B — by BC hydrologic zone."),
                                      " Pick one or more of the 29 numbered BC zones directly; every gauge in them is added."),
                              tags$li(tags$b("Option C — by station."),
                                      " Type or remove individual WSC station IDs by hand, from anywhere in Canada.")),
                            p("When the set looks right, click ", tags$b("\"Use these stations\""),
                              " (Option A) — the chosen gauges drop into the shared station box that the other sub-tabs read."),

                            h4("3. What each Option A input means"),
                            tags$ul(
                              tags$li(tags$b("Latitude / Longitude, \"Find region\"."),
                                      " The location of ", tags$em("your"), " site (the one you want a flood estimate for). ",
                                      "Clicking the map fills these in automatically."),
                              tags$li(tags$b("(BC) \"Include neighbouring zones within … km\"."),
                                      " 0 km uses only the single zone your point falls in. Increase it to pull in adjacent ",
                                      "zones when your site is near a zone boundary or you want a larger pool.")),
                            p(tags$b("ROI settings (used outside BC):")),
                            tags$ul(
                              tags$li(tags$b("Target drainage area (km²)."),
                                      " The catchment area upstream of your site — the single most important control on flood ",
                                      "magnitude. Enter it if you know it (e.g. from a GIS delineation). If left blank the app ",
                                      "borrows descriptor values from nearby gauges, which is rougher."),
                              tags$li(tags$b("GEO pre-select — nearest gauges (m1)."),
                                      " First the method keeps the ", tags$b("m1"), " geographically closest gauges. Larger m1 ",
                                      "= a wider net before similarity filtering. Typical: 100."),
                              tags$li(tags$b("Final pool size — donors (m2)."),
                                      " From those m1, the ", tags$b("m2"), " most ", tags$em("physically similar"),
                                      " catchments are kept as the pooling group. Smaller m2 = a tighter, more similar group ",
                                      "but fewer station-years; larger m2 = more data but less similar. Typical: 20–30."),
                              tags$li(tags$b("Minimum record length (years)."),
                                      " Donors with shorter records are excluded. Longer records give more reliable at-site ",
                                      "flood estimates; 20 years is a common floor."),
                              tags$li(tags$b("Catchment-similarity descriptors (PHY)."),
                                      " The catchment properties used to judge similarity: drainage area, mean annual ",
                                      "precipitation, elevation, slope, waterbody/wetland fraction, compactness. Tick the ones ",
                                      "relevant to your region — these define what \"similar\" means.")),

                            h4("4. Step two — get numbers and diagnostics"),
                            tags$ul(
                              tags$li(tags$b("Flood quantile estimate (Option A, outside BC)."),
                                      " After the ROI donors are shown, choose a flood series, distribution and return periods ",
                                      "and click ", tags$b("\"Estimate flood quantiles\""),
                                      ". The app fits a weighted log-log regression of the donors' at-site flood quantiles on ",
                                      "drainage area and reads off the estimate — with a 90% prediction interval — at your site. ",
                                      "This is the ", tags$b("ungauged-site estimate"), "."),
                              tags$li(tags$b("Envelope Curve sub-tab."),
                                      " Plots each selected gauge's T-year ", tags$b("unit flood"),
                                      " (flood per km² of catchment) against drainage area on log axes, and draws an ",
                                      "\"envelope\" — a power-law upper bound UnitQ = a·DAᵇ. Use ", tags$b("Auto-fit"),
                                      " to fit a/b, or set them by hand; points are coloured by record length so you can see ",
                                      "which gauges are best-supported."),
                              tags$li(tags$b("Qinst/Qdaily Ratio sub-tab."),
                                      " Characterises how much higher the instantaneous peak runs above the daily-mean peak ",
                                      "(useful for converting daily-based estimates to instantaneous). It pools the annual ",
                                      "ratio across your stations, summarises each by percentiles, and fits a trend against ",
                                      "drainage area. Click ", tags$b("Run Analysis"), " after choosing stations.")),

                            h4("5. Methodology, in brief"),
                            tags$ul(
                              tags$li(tags$b("BC hydrologic zones"),
                                      " come from the BC Streamflow Inventory Study — fixed regions of broadly similar flood ",
                                      "behaviour. Selection is simply which zone your point falls in, plus neighbours."),
                              tags$li(tags$b("Region of Influence (GEO-PHY)"),
                                      " follows Durocher, Burn & Zadeh (2018): narrow to the m1 nearest gauges (GEO), then rank ",
                                      "those by ", tags$b("Mahalanobis distance"),
                                      " on standardized catchment descriptors (PHY) and keep the m2 most similar. Catchment ",
                                      "descriptors are from the HYSETS database (Arsenault et al. 2020)."),
                              tags$li(tags$b("Flood-quantile regression"),
                                      " estimates each donor's at-site quantile by L-moments, then fits a record-length- and ",
                                      "similarity-weighted regression of log(Q) on log(drainage area) and evaluates it at your ",
                                      "site — a practical form of the regional GLS approach.")),

                            # Method illustrations (generated from the app's real ROI engine;
                            # see regional_analysis/roi/make_method_figures.R). Original figures
                            # illustrating the method of Durocher et al. (2018), not reproductions.
                            div(style = "margin: 8px 0 2px 0;",
                                tags$b("Figure 1. "),
                                "How the Region of Influence donor group is selected — a worked example for an ",
                                "ungauged site near Calgary (target ≈ 1,000 km²). Left: the ", tags$b("GEO"),
                                " step keeps the ", tags$em("m1"), " geographically nearest gauges. Right: the ",
                                tags$b("PHY"), " step keeps the ", tags$em("m2"),
                                " catchments most similar in descriptor space (drainage area, precipitation, ",
                                "elevation, slope, waterbody), by Mahalanobis distance. ",
                                tags$em("Illustrates the method of Durocher, Burn & Mostofi Zadeh (2018).")),
                            img(src = "roi_method_geo_phy.png",
                                style = "width: 100%; max-width: 900px; display: block; margin: 4px auto 18px auto;"),
                            div(style = "margin: 8px 0 2px 0;",
                                tags$b("Figure 2. "),
                                "How the ungauged flood estimate is computed. Each donor's at-site 100-year flood ",
                                "is regressed on drainage area (log-log), weighted by record length and catchment ",
                                "similarity; the fit, evaluated at the target's drainage area, gives the estimate ",
                                "and its 90% prediction interval."),
                            img(src = "roi_method_regression.png",
                                style = "width: 100%; max-width: 720px; display: block; margin: 4px auto 18px auto;"),

                            tags$br(),
                            tags$strong("Assumptions, limitations and data sources:"),
                            tags$ul(
                              tags$li("Regional pooling assumes the selected gauges are hydrologically similar to the target ",
                                      "and to each other. Always sanity-check the donor list and the map."),
                              tags$li("The ROI method covers Canada ", tags$b("outside BC"),
                                      "; it draws only from the ~1,700 non-BC WSC gauges that have HYSETS catchment descriptors ",
                                      "(about one third of all non-BC gauges), so donors are limited to that set."),
                              tags$li("Flood-quantile estimates are ", tags$b("screening-level"),
                                      ": the 90% prediction interval is wide by nature and is no substitute for a full at-site ",
                                      "analysis where a good local record exists."),
                              tags$li("Streamflow data are from the Water Survey of Canada HYDAT database via the tidyhydat R ",
                                      "package. Provisional realtime data are never used in Regional Analysis.")),

                            h4("References"),
                            tags$ol(
                              tags$li("Durocher, M., Burn, D.H. & Mostofi Zadeh, S. (2018). A nationwide regional flood ",
                                      "frequency analysis at ungauged sites using ROI/GLS with copulas and super regions. ",
                                      tags$em("Journal of Hydrology"), ", 567, 191–202. ",
                                      tags$a(href = "https://doi.org/10.1016/j.jhydrol.2018.10.011", target = "_blank",
                                             "https://doi.org/10.1016/j.jhydrol.2018.10.011")),
                              tags$li("Arsenault, R., Brissette, F., Martel, J.-L., Troin, M., Lévesque, G., ",
                                      "Davidson-Chaput, J., Gonzalez, M.C., Ameli, A. & Poulin, A. (2020). A comprehensive, ",
                                      "multisource database for hydrometeorological modeling of 14,425 North American ",
                                      "watersheds (HYSETS). ", tags$em("Scientific Data"), ", 7, 243. ",
                                      tags$a(href = "https://doi.org/10.1038/s41597-020-00583-2", target = "_blank",
                                             "https://doi.org/10.1038/s41597-020-00583-2")),
                              tags$li("Burn, D.H. (1990). Evaluation of regional flood frequency analysis with a region of ",
                                      "influence approach. ", tags$em("Water Resources Research"), ", 26(10), 2257–2265. ",
                                      tags$a(href = "https://doi.org/10.1029/WR026i010p02257", target = "_blank",
                                             "https://doi.org/10.1029/WR026i010p02257")),
                              tags$li("Eng, K., Milly, P.C.D. & Tasker, G.D. (2007). Flood regionalization: a hybrid ",
                                      "geographic and predictor-variable region-of-influence regression method. ",
                                      tags$em("Journal of Hydrologic Engineering"), ", 12(6), 585–591. ",
                                      tags$a(href = "https://doi.org/10.1061/(ASCE)1084-0699(2007)12:6(585)", target = "_blank",
                                             "https://doi.org/10.1061/(ASCE)1084-0699(2007)12:6(585)")),
                              tags$li("Merz, R. & Blöschl, G. (2005). Flood frequency regionalisation — spatial proximity ",
                                      "versus catchment attributes. ", tags$em("Journal of Hydrology"), ", 302(1–4), 283–306. ",
                                      tags$a(href = "https://doi.org/10.1016/j.jhydrol.2004.07.018", target = "_blank",
                                             "https://doi.org/10.1016/j.jhydrol.2004.07.018")),
                              tags$li("Hosking, J.R.M. & Wallis, J.R. (1997). ",
                                      tags$em("Regional Frequency Analysis: An Approach Based on L-Moments"),
                                      ". Cambridge University Press. ",
                                      tags$a(href = "https://doi.org/10.1017/CBO9780511529443", target = "_blank",
                                             "https://doi.org/10.1017/CBO9780511529443"))
                            )
                   ),
                   tabPanel("Regionalization",
                            br(),
                            h3("Build the Station Set for Regional Analysis"),
                            p("Choose the group of WSC stream gauges to pool. ",
                              "This selection drives both the ", tags$b("Envelope Curve"),
                              " and ", tags$b("Qinst/Qdaily Ratio"), " sub-tabs."),
                            tags$ol(
                              tags$li(tags$b("Option A – automatically, by location:"),
                                      " enter a latitude/longitude (or click the map) for your site. ",
                                      "Inside BC the enclosing hydrologic zone (plus neighbours within a chosen ",
                                      "distance) is selected; elsewhere in Canada a Region-of-Influence (ROI) donor ",
                                      "group is computed from geographic and catchment-descriptor similarity."),
                              tags$li(tags$b("Option B – by BC hydrologic zone:"),
                                      " pick one or more zones. Every WSC station in those zones ",
                                      "is added automatically. Zone numbers (1–29) match the ",
                                      "labels on the ", tags$b("Stations Map"), " tab (BC Streamflow Inventory)."),
                              tags$li(tags$b("Option C – by station:"),
                                      " type WSC IDs into the station box to add them, or click the × on a ",
                                      "chip to remove one. You can start from A or B and then fine-tune."),
                              tags$li("When you are happy with the set, open the ", tags$b("Envelope Curve"),
                                      " or ", tags$b("Qinst/Qdaily Ratio"), " sub-tab to run the analysis.")
                            ),

                            # ---- Option A: automated selection by location ----
                            wellPanel(
                              h4("Option A — Automated selection by location"),
                              fluidRow(
                                column(3, numericInput("roi_lat", "Latitude (°N)",
                                                       value = NA, min = 41, max = 84, step = 0.01)),
                                column(3, numericInput("roi_lon", "Longitude (°E)",
                                                       value = NA, min = -142, max = -51, step = 0.01)),
                                column(3, br(), actionButton("roi_locate", "Find region",
                                              icon = icon("location-crosshairs"), class = "btn-primary")),
                                column(3, br(), strong(textOutput("roi_mode_label")))
                              ),
                              helpText("Click anywhere on the map to drop the target pin, or type coordinates and press ",
                                       tags$b("Find region"), "."),
                              leaflet::leafletOutput("roi_map", height = 380),
                              br(),

                              # BC branch controls
                              conditionalPanel(
                                condition = "output.roi_mode == 'BC'",
                                h5("BC hydrologic-zone options"),
                                sliderInput("roi_bc_dist",
                                            "Also include nearby BCSI stations within (km):",
                                            min = 0, max = 150, value = 0, step = 5),
                                helpText("0 km selects only the BC Streamflow Inventory stations in the ",
                                         "zone enclosing the point. Increase to also add BCSI stations from ",
                                         "neighbouring zones whose gauge is within this distance of the point ",
                                         "(individual stations, not whole zones).")
                              ),

                              # ROI branch controls
                              conditionalPanel(
                                condition = "output.roi_mode == 'ROI'",
                                h5("Region-of-Influence (ROI) options"),
                                fluidRow(
                                  column(4, numericInput("roi_target_da",
                                                "Target drainage area (km², optional)",
                                                value = NA, min = 0, step = 1)),
                                  column(4, sliderInput("roi_m1",
                                                "GEO pre-select — nearest gauges (m1):",
                                                min = 20, max = 300, value = 100, step = 10)),
                                  column(4, sliderInput("roi_m2",
                                                "Final pool size — donors (m2):",
                                                min = 5, max = 80, value = 25, step = 5))
                                ),
                                fluidRow(
                                  column(4, sliderInput("roi_min_years",
                                                "Min record length (years):",
                                                min = 10, max = 50, value = 20, step = 5)),
                                  column(8, checkboxGroupInput("roi_descriptors",
                                                "Catchment-similarity descriptors (PHY):",
                                                choices = c("Drainage area (log10)"  = "log_da",
                                                            "Mean annual precip"     = "map_mm",
                                                            "Mean elevation"         = "elev_m",
                                                            "Mean slope"             = "slope_deg",
                                                            "Waterbody fraction"     = "waterbody",
                                                            "Wetland fraction"       = "wetland",
                                                            "Compactness (Gravelius)" = "gravelius"),
                                                selected = c("log_da","map_mm","elev_m","slope_deg","waterbody"),
                                                inline = TRUE))
                                ),
                                helpText("ROI donors are drawn from the ~1,700 non-BC WSC gauges that have HYSETS ",
                                         "catchment descriptors. Leaving drainage area blank fills the target's ",
                                         "descriptors from the nearest neighbours (a rougher, geography-led estimate).")
                              ),

                              # ---- ROI flood-quantile estimate at the target ----
                              conditionalPanel(
                                condition = "output.roi_mode == 'ROI'",
                                tags$hr(),
                                h5("Flood quantile estimate at this site (ROI regression)"),
                                helpText("Estimates the T-year flood at the target by a record-length/similarity-",
                                         "weighted log-log regression of the ROI donors' at-site flood quantiles on ",
                                         "drainage area (Durocher et al. 2018). Requires the target drainage area above. ",
                                         "Uses whatever donor group is currently shown on the map."),
                                fluidRow(
                                  column(4, selectInput("roi_est_series", "Flood series:",
                                                choices = c("Annual max daily mean" = "daily",
                                                            "Annual max instantaneous" = "inst"),
                                                selected = "daily")),
                                  column(4, selectInput("roi_est_dist", "Distribution:",
                                                choices = c("GEV", "Gumbel", "LP3"), selected = "GEV")),
                                  column(4, selectInput("roi_est_predictors", "Regression predictors:",
                                                choices = c("Drainage area only" = "da",
                                                            "Drainage area + mean annual precip" = "da_map"),
                                                selected = "da"))
                                ),
                                checkboxGroupInput("roi_est_rp", "Return periods (years):",
                                                   choices = c(2, 5, 10, 20, 50, 100, 200),
                                                   selected = c(2, 10, 20, 50, 100, 200), inline = TRUE),
                                actionButton("roi_estimate_btn", "Estimate flood quantiles",
                                             icon = icon("calculator"), class = "btn-info"),
                                br(), br(),
                                tableOutput("roi_estimate_table"),
                                uiOutput("roi_estimate_note")
                              ),

                              tags$hr(),
                              fluidRow(
                                column(8, strong(textOutput("roi_result_summary"))),
                                column(4, actionButton("roi_apply", "Use these stations →",
                                              icon = icon("arrow-down"), class = "btn-success"))
                              )
                            ),

                            # ---- Option B/C: manual zones & stations ----
                            wellPanel(
                              h4("Option B — (BC only) Add whole hydrologic zones"),
                              selectizeInput(
                                "bc_zone_selector",
                                "Select BC hydrologic zone(s):",
                                choices = setNames(as.character(1:29), paste("Zone", 1:29)),
                                multiple = TRUE,
                                options = list(placeholder = "Type a zone number, or pick from the list…")
                              ),
                              helpText("You can select multiple zones. See the Stations Map for where each zone is."),
                              tags$hr(),
                              h4("Option C — Review / edit the individual stations"),
                              selectizeInput(
                                "bc_zone_station_ids",
                                "WSC station(s) included in the analysis:",
                                choices = NULL,
                                multiple = TRUE,
                                options = list(placeholder = "Type to search and add WSC station IDs…",
                                               maxOptions = 50,
                                               loadThrottle = 300)
                              ),
                              helpText("Add stations from anywhere in Canada here — you are not limited to the BC zones above."),
                              tags$hr(),
                              fluidRow(
                                column(8,
                                       strong(textOutput("regional_selection_count"))
                                ),
                                column(4,
                                       actionButton("clear_regional", "Clear all stations",
                                                    icon = icon("trash"), class = "btn-warning btn-sm")
                                )
                              )
                            )
                   ),
                   tabPanel("Envelope Curve",
                            br(),
                            h3("Regional Envelope Curve"),
                            p("Choose your stations in the ", tags$b("Regionalization"), " sub-tab and a return ",
                              "period, then click ", tags$b("Draw Figure"), " to plot unit peak flow ",
                              tags$em("(UnitQ = a · DA", tags$sup("b"), ")"), " against drainage area. ",
                              "Set the envelope coefficients ", tags$b("a"), " and ", tags$b("b"), " by hand, or click ",
                              tags$b("Auto-fit Envelope"), " to fit a power-law curve that sits on top of every ",
                              "point while minimizing squared distance to them (DDS global search + Levenberg-Marquardt polish)."),
                            wellPanel(
                              fluidRow(
                                column(3,
                                       selectInput(
                                         "envelope_return_period",
                                         "Return Period to Plot",
                                         choices = c("2-year" = "2", "5-year" = "5", "10-year" = "10",
                                                     "25-year" = "25", "50-year" = "50", "100-year" = "100",
                                                     "200-year" = "200", "1000-year" = "1000"),
                                         selected = "10")
                                ),
                                column(3,
                                       numericInput("envelope_a", label = HTML("a (constant) in UnitQ = a &middot; DA<sup>b</sup>"),
                                                    value = NA, min = 0, step = 0.1)
                                ),
                                column(3,
                                       numericInput("envelope_b", label = HTML("b (exponent) in UnitQ = a &middot; DA<sup>b</sup>"),
                                                    value = NA, step = 0.01)
                                ),
                                column(3,
                                       br(),
                                       actionButton("envelope_draw_btn", "Draw Figure",
                                                    icon = icon("chart-area"), class = "btn-primary"),
                                       br(), br(),
                                       actionButton("envelope_autofit_btn", "Auto-fit Envelope",
                                                    icon = icon("wand-magic-sparkles"), class = "btn-success")
                                )
                              ),
                              fluidRow(
                                column(6,
                                       checkboxInput("envelope_show_ids", "Show WSC IDs next to points", value = TRUE)
                                ),
                                column(6,
                                       checkboxInput("envelope_draw_line", "Draw envelope line", value = TRUE)
                                )
                              ),
                              helpText("Draw Figure reads the selected stations' peak-flow records (may take a moment). ",
                                       "The a/b coefficients and the checkboxes update the figure instantly; changing the ",
                                       "station set or return period needs another Draw.")
                            ),
                            br(),
                            plotlyOutput("envelope_plot", height = "520px"),
                            br()
                   ),
                   tabPanel("Qinst/Qdaily Ratio",
                            br(),
                            h3("Instantaneous-to-Daily Peak Flow Ratio"),
                            p("Few stations record instantaneous peaks and their records are short, ",
                              "so individual peaking ratios are noisy. This pools the annual ratio ",
                              tags$em("(annual-maximum instantaneous ÷ annual-maximum daily mean, paired by calendar year)"),
                              " across every station selected in the ", tags$b("Regionalization"), " sub-tab. ",
                              "Each station is summarized by the ", tags$b("median, 75th, and 95th percentile"),
                              " of its ratios, and a regional trend line is fitted through each percentile ",
                              "against drainage area (log scale)."),
                            wellPanel(
                              fluidRow(
                                column(4,
                                       sliderInput("ratio_min_days", "Minimum days for a complete year",
                                                   min = 1, max = 366, value = 300, step = 5)
                                ),
                                column(2,
                                       numericInput("ratio_max", "Max ratio to include",
                                                    value = 5, min = 1, step = 0.5)
                                ),
                                column(2,
                                       numericInput("ratio_min_years", "Min years per station",
                                                    value = 5, min = 1, max = 100, step = 1)
                                ),
                                column(2,
                                       selectInput("ratio_fit", "Trend fit",
                                                   c("Linear" = "linear",
                                                     "Exponential" = "exp",
                                                     "None" = "none"),
                                                   selected = "linear")
                                ),
                                column(2,
                                       br(),
                                       actionButton("run_ratio", "Run Analysis",
                                                    icon = icon("chart-line"), class = "btn-primary")
                                )
                              ),
                              fluidRow(
                                column(8,
                                       checkboxGroupInput("ratio_pcts", "Statistic lines to show",
                                                          choices = c("Mean", "P75", "P95", "P99"),
                                                          selected = c("Mean", "P75", "P95", "P99"),
                                                          inline = TRUE)
                                ),
                                column(4,
                                       br(),
                                       checkboxInput("ratio_show_raw",
                                                     "Show all station-year points (faint)",
                                                     value = FALSE)
                                )
                              ),
                              helpText("The station set is taken from the Regionalization sub-tab (manual IDs, or BC hydrologic zones); ",
                                       "click Run to read the records. Only calendar years with at least the chosen number of daily ",
                                       "observations contribute an annual maximum. All other controls (max-ratio cap, min years per ",
                                       "station, trend fit, and percentile selection) update the plot instantly without re-reading data.")
                            ),
                            htmlOutput("ratio_summary"),
                            br(),
                            plotlyOutput("ratio_2d", height = "520px"),
                            br(),
                            h4("Per-station ratio percentiles"),
                            downloadButton("download_ratio_pct", "Download percentiles (CSV)"),
                            downloadButton("download_ratio", "Download station-year ratios (CSV)"),
                            br(), br(),
                            DT::dataTableOutput("ratio_table"),
                            linebreaks(5)
                   )
                 ) # End Regional Analysis sub-tabs
        )

      ) # End of tabsetPanel
      
    ) # End of main panel
    
  ) # End of pageWithSidebar
  
)) # End of shinyUI & fluidPage

