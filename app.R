# ============================================================
# ADVANCED DATA VISUALIZATION - FINAL PROJECT
# East 3: Natasha, Gabby, Eric, Steven
# December 2025
# ============================================================

# ============================================================
# SET UP
# ============================================================
# Dependencies
library(tidyverse)
library(shiny)
library(markdown)
library(shinythemes)
library(sf)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(scales)

# Load data
df_steve <- st_transform(readRDS("data/steve_df.rds"), 4326)
Street_Lights_clean <- read_csv("data/Street_Lights_clean.csv")
SB_PoliceData_clean <- read_csv("data/SB_PoliceData_clean.csv")
Public_Facilities <- read_csv("data/Public_Facilities.csv")
libraries <- st_transform(readRDS("data/libraries.rds"), 4326)
fire_stations <- st_transform(readRDS("data/fire_stations.rds"), 4326)
parks_locations_features_spatial <- st_transform(readRDS("data/parks_locations_features_spatial.rds"), 4326)
school <- st_read("data/School_Boundaries/School_Boundaries.shp", quiet = TRUE)
census <- st_read("data/2020_CensusData/2020_CensusData.shp", quiet = TRUE)
abandoned_shape <- st_read(
  "data/Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp"
) %>%
  mutate(year = year(as.Date(Date_of_Ou)))

# Natasha data cleaning
# 1) Load & Prepare Data

# Transform to 3857
school <- st_transform(school, 3857)
census <- st_transform(census, 3857)

#Intersection and weights
intersections <- st_intersection(census, school)
intersections$area_overlap <- st_area(intersections)
census$area_census <- st_area(census)

intersections <- intersections %>%
  left_join(census %>% st_drop_geometry() %>% select(GEOID, area_census))

intersections$weight <- intersections$area_overlap / intersections$area_census

#count columns
count_cols <- names(census)
count_cols <- setdiff(
  count_cols,
  c("GEOID", "A00002_1", "A00002_2", "A00002_3", "A14006_1", "geometry", "area_census")
)

#Aggregate to school zones
school_demo <- intersections %>%
  mutate(across(all_of(count_cols), ~ .x * weight, .names = "w_{.col}")) %>%
  group_by(OBJECTID) %>%
  summarize(
    across(starts_with("w_"), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

#calculating percentages
school_clean <- school_demo %>%
  mutate(
    total_pop = as.numeric(w_A03001_1),
    white_percent = as.numeric(100 * w_A03001_2 / w_A03001_1),
    black_percent = as.numeric(100 * w_A03001_3 / w_A03001_1),
    asian_percent = as.numeric(100 * w_A03001_5 / w_A03001_1),
    nonwhite_percent = as.numeric(100 * (w_A03001_1 - w_A03001_2) / w_A03001_1),
    poor_or_struggling_percent = as.numeric(100 * w_B13004_4 / w_B13004_1),
    comfortable_percent = as.numeric(100 * w_B13004_5 / w_B13004_1)
  )

#join back to geometries
schools_enriched <- school %>%
  left_join(school_clean %>% st_drop_geometry(), by = "OBJECTID")

#transform to 4326 for leaflet
schools_enriched_4326 <- st_transform(schools_enriched, 4326)

#race data for stacked bar chart
race_mapping <- tibble::tribble(
  ~race_code, ~race_label,
  "A03001_2", "White",
  "A03001_3", "Black",
  "A03001_4", "Native American",
  "A03001_5", "Asian",
  "A03001_6", "Pacific Islander",
  "A03001_7", "Other",
  "A03001_8", "Two or More Races"
)

race_unpivot <- school_demo %>%
  st_drop_geometry() %>%
  pivot_longer(
    cols = starts_with("w_A03001_"),
    names_to = "race_code",
    values_to = "population"
  ) %>%
  mutate(
    race_code = str_remove(race_code, "^w_"),
    population = as.numeric(population)
  ) %>%
  filter(race_code != "A03001_1") %>%  # Remove total
  left_join(race_mapping, by = "race_code")

max_zones <- nrow(school_clean)


# ============================================================
# User Interface (UI)
# ============================================================
ui <- navbarPage(
  title = "Jurassic DS Associates",
  theme = shinytheme("cerulean"),
  
  # Homepage
  tabPanel(
    "Home",
    tags$img(
      src = "south_bend.jpg",
      style = "width: 55%; height: auto; display: block;"
    ),
    htmlOutput("Home")
  ),
  
  # Steve's section
  tabPanel(
    "Livability",
    titlePanel("Livability by Zip Code"),
        sidebarLayout(
      sidebarPanel(
        selectInput(
          "zip_select",
          "Jump to ZIP",
          choices = sort(unique(df_steve$zip)),
          selected = NULL
        ),
        
        tags$hr(),
        p(strong("How to use this map")),
        p("Use the dropdown to zoom in on a particular zip code."),
        p("Click on the map to pull up some more information about that zip code.")
      ),
      
      mainPanel(
        div(
          style = "background:#f7f7f7; padding:12px; border-radius:8px; margin-bottom:10px;",
          tags$h4("Key Takeaways"),
          tags$ul(
            tags$li("Zip code 46601 ranks #1 overall (lots of amenities per square mile)."),
            tags$li("Livability score was determined using the density of parks, businesses, libraries, street lights, etc. per square mile."),
            tags$li("As one would expect, most of the action in the county is focused on South Bend and Mishiwaka."),
            tags$li("Outer zip codes in St. Joseph County had limited data available and therefore received lower scores.")
          )
        ),
        leafletOutput("Livability", height = 650)
      )
    )
  ),
  
  # Gabby's section
  tabPanel(
    "Lighting & Crime",
    titlePanel("Crime vs. Street Lighting"),
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "lumens",
          "Set Lumens Range",
          min = min(Street_Lights_clean$Lumens_numeric, na.rm = TRUE),
          max = max(Street_Lights_clean$Lumens_numeric, na.rm = TRUE),
          value = c(
            min(Street_Lights_clean$Lumens_numeric, na.rm = TRUE),
            max(Street_Lights_clean$Lumens_numeric, na.rm = TRUE)
          ),
          step = 1000,
          sep = ","
        ),
        tags$hr(),
        
        p(strong("How to use this map")),
        p("Use the lumens slider to filter street lights by brightness."),
        p("Toggle map layers to compare locations of crime, lighting, and police stations."),
        p("Purple markers indicate high-crime, low-light hotspots.")
      ),
      
      mainPanel(
        div(
          style = "background:#f7f7f7; padding:12px; border-radius:8px; margin-bottom:10px;",
          tags$h4("Key Takeaways"),
          tags$ul(
            tags$li("Crime hotspots tend to be in residential areas near the city center."),
            tags$li("Police stations tend to not have crime hotspots around them."),
            tags$li("Increasing lumens, in downtown South Bend and along Western Ave, may be an effective way to improve public safety.")
          )
        ),
        leafletOutput("Lighting", height = 650)
      )
    )
  ),
  
  # Eric's section
  tabPanel(
    "Abandoned Property",
    titlePanel("Abandoned Property Over Time"),
        sidebarLayout(
      sidebarPanel(
        sliderInput(
          inputId = "year_selected",
          label = "Show abandoned properties thru year:",
          min = min(abandoned_shape$year, na.rm = TRUE),
          max = max(abandoned_shape$year, na.rm = TRUE),
          value = max(abandoned_shape$year, na.rm = TRUE),
          step = 1,
          sep = ""
        ),
        
        tags$hr(),
        p(strong("How to use this map")),
        p("Use the slider to filter abandoned properties by year (marks are cumulative)."),
        p("Toggle map layers to compare locations of abandoned properties, parks, fire stations, and libraries."),
        ),
      
      mainPanel(
        div(
          style = "background:#f7f7f7; padding:12px; border-radius:8px; margin-bottom:10px;",
          tags$h4("Key Takeaways"),
          tags$ul(
            tags$li("The biggest spike in abandoned properties happened in 2014 and 2015."),
            tags$li("Many abandoned properties appear concentrated in central or older neighborhoods, often away from parks, libraries, and fire stations."),
            tags$li("Fire stations are scattered to provide citywide coverage, but some abandoned areas may still be relatively far from emergency services."),
            tags$li("Showing where the public service facilities are shows potential areas for investment or redevelopment, particularly where abandoned properties are near amenities like parks and libraries.")
          )
        ),
         leafletOutput("abandoned_map", height = 650)
        )
      )
    ),
  
  # Natasha's section
  tabPanel(
  "Schools",  
  titlePanel("Low Income & Minority Populations by School Zone"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "top_n",
        "Select Top N school zones by population ranking",
        min = 1,
        max = max_zones,
        value = 15,
        step = 1
      ),
      tags$hr(),
      p(strong("How to use this visualization")),
      p("Use the slider to change the number of school zones in the plot."),
    ),
    
    mainPanel(
      div(
        style = "background:#f7f7f7; padding:12px; border-radius:8px; margin-bottom:10px;",
        tags$h4("Key Takeaways"),
        tags$ul(
          tags$li("This dashboard compares minority population and low income population across school zones."),
          tags$li("The demarcation for low income was earnings under 2x the poverty threshold."),
          tags$li("There is a positive correlation among all school zones between minority population and low income; however, high population did not translate to higher nonwhite population percentages."),
          tags$li("School size does not guarantee a higher percent of economically disadvantaged students."),
          
        )
      ),
      plotlyOutput("scatter_plot", height = 500),
      br(),
      plotOutput("race_stack", height = 500)
    )
  )
)
)


# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # Homepage
  output$Home <- renderUI({
    markdown("# Areas of Opportunity for South Bend, Indiana

    This app provides a data-driven look at key trends and indicators for Mayor James Mueller.
    
    ### Topics to Explore
    - **Livability:** Livability scores for each zip code in St. Joseph County. 
    - **Lighting & Crime:** The relationship between street lighting and crime.
    - **Abandoned Property:** Abandoned properties and the potential for investment.
    - **School Demographics:** The relationship between racial demographics and income disparities by school district.
    
    ### How to use this app
    Navigate using the tabs above based on what you want to learn more about. Each visualization has some interactive controls.
    
    ### Who are Jurassic DS Associates?
    We are a team of data scientists from the University of Notre Dame who happen to share an affinity for dinosaurs (it's a nod to another project we worked on together).
    - Natasha Malik
    - Gabby Salera
    - Eric Shin
    - Steven Villalon
    ")
  })
  
  # Steve's Section
  output$Livability <- renderLeaflet({
    
    pal <- colorNumeric(
      palette = "Blues",
      domain  = df_steve$score
    )
    
    leaflet(df_steve) %>%
      addProviderTiles("CartoDB.Positron") |> 
      addPolygons(
        layerId = ~zip,
        fillColor = ~pal(score),
        weight = 1,
        fillOpacity = 0.6,
        color = "#2b2b2b",
        label = ~as.character(zip),
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          style = list(
            "color" = "#111111",
            "font-weight" = "bold",
            "font-size" = "12px",
            "text-shadow" = "0 0 3px white"
          )
        ),
        popup = ~paste0(
          "<b>ZIP: </b>", zip, "<br>",
          "<b>Rank: </b>", rank, " of 23", "<br>",
          "<b>Score: </b>", round(score, 2), "<br>",
          "<b>Parks: </b>", parks_cnt, "<br>",
          "<b>Businesses: </b>", business_cnt, "<br>",
          "<b>Street lights: </b>", street_lights_cnt, "<br>",
          "<b>Fire stations: </b>", fire_station_cnt, "<br>",
          "<b>Police stations: </b>", police_station_cnt, "<br>",
          "<b>Libraries: </b>", library_cnt, "<br>",
          "<b>Abandoned: </b>", abandoned_cnt
        ),
        highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE)
      ) %>%
      addLegend(
        pal = pal,
        values = ~score,
        title = "Livability Score<br>(Darker is better)",
        labFormat = labelFormat(digits = 2),
        position = "topright"
      )
  })
  
  # Select zip from drop down to zoom in on that zip code
  observeEvent(input$zip_select, {
    req(input$zip_select)
    
    # Force both sides to character so the filter always matches
    sel <- df_steve %>% filter(as.character(zip) == as.character(input$zip_select))
    
    req(nrow(sel) == 1)
    
    bb <- as.list(sf::st_bbox(sel))
    
    leafletProxy("Livability") %>%
      fitBounds(bb$xmin, bb$ymin, bb$xmax, bb$ymax)
  })
  
  
  # Gabby's Section
  filtered_lights <- reactive({
    df <- Street_Lights_clean %>%
      filter(
        Lumens_numeric >= input$lumens[1],
        Lumens_numeric <= input$lumens[2]
      )
    
    df
  })
  
  output$Lighting <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      
      # Crime locations
      addCircleMarkers(
        data = SB_PoliceData_clean,
        lng = ~lon,
        lat = ~lat,
        radius = 4,
        color = "red",
        fillOpacity = 0.3,
        group = "Crimes - Red",
        popup = ~paste("Date:", date)
      ) %>%
      
      # Street light locations
      addCircleMarkers(
        data = filtered_lights(),
        lng = ~Lon,
        lat = ~Lat,
        radius = 5,
        color = "yellow",
        fillOpacity = 0.3,
        group = "Street Lights - Yellow",
        popup = ~paste(
          "Pole:", Pole_Number,
          "<br>Lumens:", Lumens_numeric
        )
      ) %>%
      
      # police station locations
      addAwesomeMarkers(
        data = Public_Facilities,
        lng = ~Lon,
        lat = ~Lat,
        icon = awesomeIcons(
          icon = "star",
          library = "fa",
          markerColor = "blue"
        ),
        group = "Police Stations - Blue",
        label = ~POPL_NAME
      ) %>%
      
      # High crime / low light locations
      addCircleMarkers(
        data = Street_Lights_clean %>% filter(High_Crime_Low_Light),
        lng = ~Lon,
        lat = ~Lat,
        radius = 7,
        color = "purple",
        fillOpacity = 0.3,
        group = "Crime Hotspots - Purple",
        popup = ~paste(
          "Pole:", Pole_Number,
          "<br>Lumens:", Lumens_numeric,
          "<br>Crimes (100m):", Crime_Count_100m
        )
      ) %>%
      
      addLayersControl(
        overlayGroups = c("Crimes - Red", "Street Lights - Yellow", "Police Stations - Blue", "Crime Hotspots - Purple"),
        options = layersControlOptions(collapsed = FALSE)
      )}
  )
  

  # Eric's section
  filtered_abandoned <- reactive({
    abandoned_shape %>% filter(year <= input$year_selected)
  })
  
  output$abandoned_map <- renderLeaflet({
    
    leaflet() %>%
      setView(lng = -86.2510, lat = 41.6764, zoom = 13) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      #Abandoned Properties
      addPolygons(
        data = filtered_abandoned(),
        fillColor = "red",
        fillOpacity = 1,
        color = "black",
        weight = 0.5,
        group = "Abandoned Properties - Red",
        popup = ~paste0(
          "<strong>Address:</strong> ", Address_Nu, " ", Street_Nam, " ", Suffix, "<br>",
          "<strong>Outcome:</strong> ", Outcome_St, "<br>",
          "<strong>Year:</strong> ", year, "<br>",
          "<strong>Zip Code:</strong> ", Zip_Code
        )
      ) %>%
      
      #Parks
      addCircleMarkers(
        data = parks_locations_features_spatial,
        popup = ~popup,
        color = "forestgreen",
        radius = 5,
        stroke = FALSE,
        fillOpacity = 1,
        group = "Parks - Green"
      ) %>%
      
      #Fire Stations
      addCircleMarkers(
        data = fire_stations,
        popup = ~popup,
        color = "orange",
        radius = 5,
        stroke = FALSE,
        fillOpacity = 1,
        group = "Fire Stations - Orange"
      ) %>%
      
      #Libraries
      addCircleMarkers(
        data = libraries,
        popup = ~popup,
        color = "blue",
        radius = 7,
        stroke = FALSE,
        fillOpacity = 1,
        group = "Libraries - Blue"
      ) %>%
      
      #Layer Controls
      addLayersControl(
        overlayGroups = c(
          "Abandoned Properties - Red",
          "Parks - Green",
          "Fire Stations - Orange",
          "Libraries - Blue"
        ),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  # Natasha's Section
  
  #top zones by population
  top_zones_data <- reactive({
    schools_enriched_4326 %>%
      arrange(desc(total_pop)) %>%
      slice_head(n = input$top_n)
  })
  
    #Scatter Plot
    output$scatter_plot <- renderPlotly({
      top_ids <- top_zones_data() %>%
        st_drop_geometry() %>%
        pull(OBJECTID)
      
      df <- schools_enriched %>%
        st_drop_geometry() %>%
        filter(OBJECTID %in% top_ids)
      
      p <- ggplot(df, aes(
        x = nonwhite_percent,
        y = poor_or_struggling_percent,
        text = paste0(
          "School Zone: ", OBJECTID, "<br>",
          "% Nonwhite: ", round(nonwhite_percent, 1), "%<br>",
          "% Low Income: ", round(poor_or_struggling_percent, 1), "%<br>",
          "Total Pop: ", prettyNum(round(total_pop), big.mark = ",")
        )
      )) +
        geom_point(alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE) +
        scale_x_continuous(labels = percent_format(scale = 1)) +
        scale_y_continuous(labels = percent_format(scale = 1)) +
        labs(
          title = "Poverty vs Race Demographics by School Zone",
          x = "% Nonwhite",
          y = "% Low Income"
        ) +
        theme_minimal()
      
      ggplotly(p, tooltip = "text")
    })
    
    #Race Composition Stacked Bar
    output$race_stack <- renderPlot({
      top_ids <- top_zones_data() %>%
        st_drop_geometry() %>%
        pull(OBJECTID)
      
      race_unpivot %>%
        filter(OBJECTID %in% top_ids) %>%
        group_by(OBJECTID) %>%
        mutate(pct = population / sum(population, na.rm = TRUE)) %>%
        ungroup() %>%
        ggplot(aes(x = factor(OBJECTID), y = pct, fill = race_label)) +
        geom_col() +
        scale_y_continuous(labels = percent_format()) +
        labs(
          title = paste0("Race Composition (Top ", input$top_n, " School Zones by Population)"),
          x = "School Zone",
          y = "% of Population",
          fill = "Race"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(size = 18),
              axis.title = element_text(size = 16))
    })
}

shinyApp(ui, server)
