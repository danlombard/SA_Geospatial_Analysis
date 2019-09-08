library(sf)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(geogrid)
library(cartogram)

# define css for tooltips
tooltip_css <- "background-color:gray;color:white;padding:5px;border-radius:5px;font-family:sans-serif;font-size:12px;"

# load input data with a projected CRS
proj_sf <- readRDS("sa_attributes.rds") 

col_var = names(proj_sf)
  
## function that receives an sf object and returns a modified sf object
# according to user selections of region and uts


# function that receives a (possibly filtered) sf object and
# outputs the same sf object with geometry modified depending on 
# user-selected representation and variable
adjust_geometry <- function(data, representation, variable) {
  
    var <- switch(variable,
                  "Total GDP" = col_var[11],                                              
                  "GDP per Capita" = col_var[12],                                         
                  "GDP per Capita (USD)" = col_var[13],                                      
                  "Population" = col_var[14],                                              
                  "Murder" = col_var[15],                                                  
                  "Sexual offences" = col_var[16],                                        
                  "Attempted murder" = col_var[17],                                       
                  "Assault with the intent to inflict grievous bodily harm" = col_var[18],
                  "Common assault" = col_var[19],                                          
                  "Common robbery" = col_var[20],                                         
                  "Robbery with aggravating circumstances" = col_var[21],                  
                  "Arson" = col_var[22],                                                  
                  "Malicious damage to property" = col_var[23],                            
                  "Burglary at nonresidential premises" = col_var[24],                    
                  "Burglary at residential premises" = col_var[25],                        
                  "Theft of motor vehicle and motorcycle" = col_var[26],                  
                  "Theft out of or from motor vehicle" = col_var[27],                      
                  "Stock theft" = col_var[28],                                            
                  "Illegal possession of firearms and ammunition" = col_var[29],           
                  "Drug related crime" = col_var[30],                                     
                  "Driving under the influence of alcohol or drugs" = col_var[31],         
                  "Sexual offences detected as a result of police action" = col_var[32],  
                  "All theft not mentioned elsewhere" = col_var[33],                       
                  "Commercial crime" = col_var[34],                                       
                  "Shoplifting" = col_var[35],                                             
                  "Community reported serious crimes" = col_var[36],                     
                  "Carjacking" = col_var[37],                                              
                  "Truck hijacking" = col_var[38],                                        
                  "Robbery at residential premises" = col_var[39],                         
                  "Robbery at non residential premises" = col_var[40],                    
                  "Bank robbery" = col_var[41],                                            
                  "Robbery of cash in transit" = col_var[42],                             
                  "TRIO Crimes" = col_var[43],                                             
                  "Rape" = col_var[44],                                                   
                  "Sexual assault" = col_var[45],                                          
                  "Attempted sexual offences" = col_var[46],                              
                  "Contact sexual offences" = col_var[47]                                 
    )
  
    if (representation == "Geographic") {
    new_sf <- data
    } else if (representation == "Continuous Cartogram") {
    new_sf <- cartogram_cont(data, var)
    } else if (representation == "Non-continuous Cartogram") {
    new_sf <- cartogram_ncont(data, var)
    } else if (representation == "Dorling Cartogram") {
    new_sf <- cartogram_dorling(data, var)
    } else if (representation == "Hexbin") {
    new_cells_hex <- calculate_grid(shape = data, 
                                    grid_type = "hexagonal", seed = 1)
    new_sf <- assign_polygons(data, new_cells_hex)
    } else {
    print("Representation not found.")
    }
  
    # calculate x, y coordinates for geom_text labels
    if (representation == "Hexbin") {
    # hexbin's assign_polygons calculates new coords as V1, V2
      new_sf <- new_sf %>% 
        rename(COORDS_X = V1, COORDS_Y = V2)
    
    } else {
      new_sf <- new_sf %>% 
        mutate(
          CENTROID = purrr::map(geometry, st_centroid),
          COORDS = purrr::map(CENTROID, st_coordinates),
          COORDS_X = purrr::map_dbl(COORDS, 1),
          COORDS_Y = purrr::map_dbl(COORDS, 2)
        )
    }
  
    new_sf
  
}

# function that receives the existing sf object and user-selected variable
# and outputs a list of matching arguments to be used in plots
get_args <- function(new_sf, variable) {
  
  args <- switch(variable, 
                 "Total GDP" = list(
                   plot_var = new_sf$total_gdp, 
                   legend_title = "Total GDP",
                   legend_labels = scales::comma
                 ),
                 "GDP per Capita" = list(
                   plot_var = new_sf$gdp_per_capita, 
                   legend_title = "GDP per Capita",
                   legend_labels = scales::comma
                 ),
                 "GDP per Capita (USD)" = list(
                   plot_var = new_sf$gdp_per_capita_usd, 
                   legend_title = "GDP per Capita (USD)",
                   legend_labels = scales::dollar
                 ),
                 "Murder" = list(
                   plot_var = new_sf$Murder, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Sexual offences" = list(
                   plot_var = new_sf$Sexual_Offences, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Attempted murder" = list(
                   plot_var = new_sf$Attempted_murder, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Assault with the intent to inflict grievous bodily harm" = list(
                   plot_var = new_sf$Assault_with_the_intent_to_inflict_grievous_bodily_harm, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Common assault" = list(
                   plot_var = new_sf$Common_assault, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Common robbery" = list(
                   plot_var = new_sf$Common_robbery, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Robbery with aggravating circumstances" = list(
                   plot_var = new_sf$Robbery_with_aggravating_circumstances, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Arson" = list(
                   plot_var = new_sf$Arson, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Population" = list(
                   plot_var = new_sf$pop_2011 / 1e6, 
                   legend_title = "Population (Million)",
                   legend_labels = scales::comma
                 ),
                 "Malicious damage to property" = list(
                   plot_var = new_sf$Malicious_damage_to_property, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Burglary at nonresidential premises" = list(
                   plot_var = new_sf$Burglary_at_nonresidential_premises, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Burglary at residential premises" = list(
                   plot_var = new_sf$Burglary_at_residential_premises, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Theft of motor vehicle and motorcycle" = list(
                   plot_var = new_sf$Theft_of_motor_vehicle_and_motorcycle, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Theft out of or from motor vehicle" = list(
                   plot_var = new_sf$Theft_out_of_or_from_motor_vehicle, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Stock theft" = list(
                   plot_var = new_sf$Stock_theft, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Illegal possession of firearms and ammunition" = list(
                   plot_var = new_sf$Illegal_possession_of_firearms_and_ammunition, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Drug related crime" = list(
                   plot_var = new_sf$Drug_related_crime, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Driving under the influence of alcohol or drugs" = list(
                   plot_var = new_sf$Driving_under_the_influence_of_alcohol_or_drugs, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Sexual offences detected as a result of police action" = list(
                   plot_var = new_sf$Sexual_offences_detected_as_a_result_of_police_action, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "All theft not mentioned elsewhere" = list(
                   plot_var = new_sf$All_theft_not_mentioned_elsewhere, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Commercial crime" = list(
                   plot_var = new_sf$Commercial_crime, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Shoplifting" = list(
                   plot_var = new_sf$Shoplifting, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Community reported serious crimes" = list(
                   plot_var = new_sf$Community_reported_serious_crimes, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Carjacking" = list(
                   plot_var = new_sf$Carjacking, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Truck hijacking" = list(
                   plot_var = new_sf$Truck_hijacking, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Robbery at residential premises" = list(
                   plot_var = new_sf$Robbery_at_residential_premises, 
                   legend_title = "Number of crimes"
                 ),
                 "Robbery at non residential premises" = list(
                   plot_var = new_sf$Robbery_at_non_residential_premises, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Bank robbery" = list(
                   plot_var = new_sf$Bank_robbery, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Robbery of cash in transit" = list(
                   plot_var = new_sf$Robbery_of_cash_in_transit, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "TRIO Crimes" = list(
                   plot_var = new_sf$TRIO_Crimes, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Rape" = list(
                   plot_var = new_sf$Rape, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Sexual assault" = list(
                   plot_var = new_sf$Sexual_assault, 
                   legend_title = "Sexual assault",
                   legend_labels = scales::comma
                 ),
                 "Attempted sexual offences" = list(
                   plot_var = new_sf$Attempted_sexual_offences, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 ),
                 "Contact sexual offences" = list(
                   plot_var = new_sf$Contact_sexual_offences, 
                   legend_title = "Number of crimes",
                   legend_labels = scales::comma
                 )
  )
  
  args
}


# function that receives previous sf object, list of args, and
# user-defined plot settings and returns the appropriate choropleth
make_choropleth <- function(new_sf, args, color_scheme, representation) {
  
  new_sf <- new_sf %>% 
    mutate(
      tip = paste0(
        "<b>", prov, " : ", args$plot_var, "</b>",
        "</span></div>")
    )
  
  p <- ggplot(
    data = new_sf
  ) +
    geom_sf_interactive(
      aes(fill = args$plot_var,
          tooltip = tip,
          data_id = prov), 
      lwd = 0
    ) +
    geom_sf(
      fill = NA, color = "lightgrey", lwd = 0.5
    ) +    
    scale_fill_viridis_c(
      args$legend_title, 
      option = color_scheme,
      labels = args$legend_labels
    )
  
  plt <- ggiraph(
    ggobj = p, 
    hover_css = "cursor:pointer;stroke-width:5px;fill-opacity:0.8;",
    tooltip_extra_css = tooltip_css, tooltip_opacity = 0.75,
    zoom_max = 5
  )    
  
  plt
  
}

# function that receives sf object and list of args for plotting
# returns appropriate dotplot
make_dotplot <- function(new_sf, args) {
  
  p <- ggplot(
    data = new_sf,
    mapping = aes(
      x = reorder(prov, args$plot_var), 
      y = args$plot_var,
      color = prov)
  ) +
    geom_point_interactive(
      aes(tooltip = paste0(prov, ": ", args$plot_var), 
          data_id = prov)
    ) +
    coord_flip() +
    scale_y_continuous(
      args$legend_title, 
      labels = args$legend_labels
    ) +
    scale_x_discrete("") +
    scale_color_discrete("Province") 
  
  ggiraph(
    code = print(p), 
    hover_css = "cursor:pointer;stroke-width:5px;fill-opacity:0.8;"
  )
  
}












