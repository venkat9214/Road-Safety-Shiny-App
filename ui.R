library(shiny)
library(leaflet)

# Choices for drop-downs
vars <- c(
  'Collision Vehicle 1' = 'VEHICLE.TYPE.CODE.1',
  'Collision Vehicle 2' = 'VEHICLE.TYPE.CODE.2',
  "Injuries" = "NUMBER.OF.PERSONS.INJURED",
  "Deaths" = "NUMBER.OF.PERSONS.KILLED"
  )

vars2 <- c(
  "Injuries" = "NUMBER.OF.PERSONS.INJURED",
  "Deaths" = "NUMBER.OF.PERSONS.KILLED"
)

vars3 <- c(
  "All Vehicles" = "",
  "Ambulance" = "AMBULANCE",
  "Bicycle" = "BICYCLE",
  "Bus" = "BUS",
  "Fire Truck" = "FIRE TRUCK",
  "Large Commercial Vehicle(6 or more tires)" = "LARGE COM VEH(6 OR MORE TIRES)",
  "Livery Vehicle" = "LIVERY VEHICLE",
  "Motorcycle" = "MOTORCYCLE",
  "Passenger" = "PASSENGER VEHICLE",
  "Pick-up Truck" = "PICK-UP TRUCK",
  "Scooter" = "SCOOTER",
  "Small Commercial Vehicle(4 tires)" = "SMALL COM VEH(4 TIRES)",
  "Sport Utility/Station Wagon" = "SPORT UTILITY / STATION WAGON",
  "Taxi" = "TAXI",
  "Van" = "VAN"
)

vars4 <- c("All boroughs"="",
  'Manhattan'='MANHATTAN',
  'Brooklyn'='BROOKLYN',
  'Queens'='QUEENS','Bronx'='BRONX',
  'Staten Island'='STATEN ISLAND')

shinyUI(navbarPage("NYC Vehicle Accidents Map", id="nav",
                   
                   
                   
                   
                   tabPanel("Interactive map",
                            div(class="outer",
                                
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")
                                ),
                                
                                leafletOutput("map", width="100%", height="100%"),
                                
                                # Shiny versions prior to 0.11 should use class="modal" instead.
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                              
                                              h2("Vehicle Collisions"),
                                              
                                              selectInput("color", "Color", vars),
                                              selectInput("size", "Size", vars2, selected = "NUMBER.OF.PERSONS.INJURED"),
                                              checkboxInput("cluster", "Add Cluster"),
                                              helpText("Cluster numbers show total accidents for each area", 
                                                       "(applies to all vehicles only)"),
                                              radioButtons("vehicle", "Show Just One Vehicle", vars3, selected = '')
                                ),
                                
                                tags$div(id="cite",
                                         'Data from: ', tags$em('NYPD Motor Vehicle Collisions'), '  | NYC Open Data. 
                                         Details of Motor Vehicle Collisions in New York City provided by the 
                                         Police Department (NYPD).'
                                )
                             )
                   ),
                   
                   tabPanel("Most Dangerous Intersections",
                            h2("TOP 10 Intersections With The Most Accidents"),
                            helpText("Click ACTION BUTTON to view the intersection on the map"),
                            helpText("Choose ALL VEHICLES in interactive map to ensure the right popup info"),
                            hr(),
                            DT::dataTableOutput("toptable")
                   ),   
                   
                   tabPanel("See Your Neighbourhood",
                            fluidRow(
                              column(3,
                                     selectInput("boroughs", "Boroughs", vars4, multiple=TRUE)
                              ),
                              column(3,
                                     conditionalPanel("input.boroughs",
                                                      selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                                     )
                              )
                            ),
                            helpText("Click ACTION BUTTON to view the intersection on the map"),
                            helpText("Choose ALL VEHICLES in interactive map to ensure the right popup info"),
                            hr(),
                            DT::dataTableOutput("vctable")
                   ),
                   
                   
                   ################## Based On Time
                   
                   tabPanel("Time",
                            sidebarLayout(
                              sidebarPanel(
                                helpText("Time"),
                                
                                selectInput("choose", label = h3("Select The Vehicle Type"),
                                            choices = list("All Vehicles" = "all", "BUS" = "BUS", "TAXI" = "TAXI", "SPORT UTILITY / STATION WAGON" = "SPORT UTILITY / STATION WAGON", "UNKNOWN" = "UNKNOWN", "OTHER" = "OTHER", "PICK-UP TRUCK" = "PICK-UP TRUCK", "PASSENGER VEHICLE" = "PASSENGER VEHICLE", "SMALL COM VEH(4 TIRES)" = "SMALL COM VEH(4 TIRES)", "VAN" = "VAN", "LARGE COM VEH(6 OR MORE TIRES)" = "LARGE COM VEH(6 OR MORE TIRES)", "LIVERY VEHICLE" = "LIVERY VEHICLE", "FIRE TRUCK" = "FIRE TRUCK", "BICYCLE" = "BICYCLE", "AMBULANCE" = "AMBULANCE", "MOTORCYCLE" = "MOTORCYCLE", "SCOOTER" = "SCOOTER"),
                                            selected = "all"),
                                
                                selectInput("select", label = h3("Select The Time"),
                                            choices = list("Morning_fatalities" = "Morning_fatalities", "Afternoon_fatalities" = "Afternoon_fatalities", "Evening_fatalities" = "Evening_fatalities", "Night_fatalities" = "Night_fatalities"),
                                            selected = "Morning_fatalities")
                                
                                
                                
                              ),
                              mainPanel(style="position:relative",
                                        plotOutput("plot_ie")
                              )
                            )
                     
                   ),
                   
                   ##################
                   tabPanel("Time Series Forecasting",
                            
                            sidebarLayout(
                              sidebarPanel(
                                numericInput("months", label = "Months to Predict", 
                                             value = 12, min = 12, max = 144, step = 12),
                                selectInput("interval", label = "Prediction Interval",
                                            choices = c("0.80", "0.90", "0.95", "0.99"),
                                            selected = "0.95"),
                                checkboxInput("showgrid", label = "Show Grid", value = TRUE)
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Predicting The Injury", dygraphOutput("dygraph1")),
                                  tabPanel("Predicting The Deaths", dygraphOutput("dygraph2"))
                                  
                                ))
                            )
                   ),
                   
                   
                   
                   ##################
                   
                   tabPanel("About",
                            
                            h4("Data Source"),
                            p("Source: ",a("NYPD Motor Vehicle Collisions | NYC Open Data.",href=
                                             "https://data.cityofnewyork.us/Public-Safety/NYPD-Motor-Vehicle-Collisions/h9gi-nx95")),
                            p("Description: ","Data Details of Motor Vehicle Collisions in 
                              New York City provided by the Police Department (NYPD)."),
                            
                            br(),
                            h4("Author Information"),
                            p("Venkatesh Subramaniam"),
                            p("Email: venkatesh.ase@gmail.com"),
                            p("Github:", a("https://github.com/venkat9214/",href="https://github.com/venkat9214/")),
                            p("LinkedIn:", a("https://www.linkedin.com/in/venkatesh141192",href="https://www.linkedin.com/in/venkatesh141192")),
                            br(),
                            br(),
                            p("")
                            ),
                   
                   conditionalPanel("false", icon("crosshair"))
))
