library(purrr)
library(dplyr)
library(forcats)
library(dash)
# library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(here)
library(ggthemes)
library(dashHtmlComponents)


# 1: Functions

plot1 <- function(subset, cost_subset) {

  y_title <- price_subset$title[price_subset$feature == cost_subset]
  p <- ggplot(subset, aes(x = fct_reorder(city, !!sym(cost_subset)),
                          y = !!sym(cost_subset),
                          fill = city)) +
    labs(x = 'City', y = y_title) +
    geom_col(show.legend = FALSE) +
    theme(axis.text.x = element_text(angle = 45),
          legend.position = "none")
  ggplotly(p)
}

plot2 <- function(subset, earning) {
  subset <- subset %>%
    mutate(surplus = earning - all  )
  p <- ggplot(subset, aes(
    x = fct_reorder(city, surplus),
    y = surplus, 
    fill = city))+
    geom_col(show.legend = FALSE)  +
    labs(y = "Monthly Surplus (USD)", x = "City") +
    theme(axis.text.x = element_text(angle = 45),
          legend.position = "none")
  ggplotly(p)
}

plot3 <- function(subset, cost_subset) {
  y_title <- price_subset$title[price_subset$feature == cost_subset]
  g <- list(   
    showframe = T,
    showcoastlines = T,
    showcountries = T,
    showland = T,
    landcolor = toRGB("grey90")
  )
  
  subset$hovertext <- paste("City:",
                            subset$city,
                            "<br>",
                            y_title,
                            ":",
                            subset[[cost_subset]])
  
  p <- plot_geo(subset)%>% 
    add_markers(

      x = ~longitude, 
      y = ~latitude, 
      text = ~city,
      color = ~city,
      size = subset[[cost_subset]],
      
      # marker = list( showscale = T, size = subset[[cost_subset]]/200, sizemode = 'area'),
      hovertext = ~hovertext,
      hoverinfo = 'text'
    ) %>% 
    layout(geo = g)
  ggplotly(p)
}

plot4 <- function(subset) {
  
  p <- ggplot(subset, aes(
    x = fct_reorder(city, property_price),
    y = property_price, 
    fill = city))+
    geom_col(show.legend = FALSE)  +
    labs(y = "Property Price (USD)", x = "City")+
    theme(axis.text.x = element_text(angle = 45),
          legend.position = "none")
  ggplotly(p)
}


# 2: Load data and data wrangling
data_df <- read.csv("data/processed_data.csv")
price_subset <- data.frame(
  title=c("Total Monthly Cost",
          "Basic Groceries",
          "Childcare", 
          "Entertainment",
          "Fitness", 
          "Monthly Rent",
          "Public Transport",
          "Shopping",
          "Utilities") , 
  feature=c("all",
            "grocery_for_one_person",
            "childcare_for_one_child",
            "entertainment",
            "fitness",
            "rent_for_one_person",
            "transportation_public",
            "shopping",
            "utility_bills"))

#read data
data_df <- read.csv("data/processed_data.csv")



# 3: Declare objects

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
app$title('Global Cost of Living')



SIDEBAR_STYLE <- list(
  "position"= "fixed",
  "top"= 0,
  "left"= 0,
  "bottom"= 0,
  "width"= "20rem",
  "padding"= "2rem 1rem",
  "background-color"= "#2eced0",
  "color"= "black"
)


sidebar <- htmlDiv(list(
  htmlH2("Where Can I Afford To Move To?",style=list("justify"="center", "textAlign"= "center")),
  htmlH6("Explore the cost of living for a single person in different cities around the world", 
          style=list("justify"= "center", "textAlign"= "center")),
  htmlBr(),
  htmlBr(),
  htmlDiv(
    list(
      "Selected By: ",
  dccRadioItems(
        id = 'selection_mode',
        options=list(list('label' = 'Regions', 'value' = 1),
                     list('label' = 'Cities', 'value' = 2)),
        value=2,
        inputStyle=list("margin-left"= "25px",'margin-right'= '5px'))
    )
  ),
  htmlBr(),
  htmlDiv(
    list(dccDropdown(
    id = 'region_selection',
    options = unique(data_df$region) %>% purrr::map(function(col) list(label = col, value = col)),
    placeholder = "Select region",
    value = "Canada",
    multi = TRUE,
  
  )
  )
  ),
  htmlDiv(
    list(dccDropdown(
    id = 'city_selection',
    options = unique(data_df$city) %>% purrr::map(function(col) list(label = col, value = col)),
    placeholder = "Select cities",
    value = c('Vancouver','Calgary','New York','London'),
    multi = TRUE,
    
  )
  )
  ),
  htmlBr(),
  htmlBr(),
  htmlDiv(
    list(
  htmlLabel(list('Select monthly costs: ', dccDropdown(id = 'cost_subset', 
                                                       options = price_subset$feature %>% 
                                                       purrr::map(function(col) list(label = price_subset$title[price_subset$feature == col], 
                                                                                     value = col)),
                                                       placeholder = "Total monthly costs",
                                                       value="all",
                                                       multi = FALSE,
                                                       style=list('marginRight'='10px')
  )
  )
  )
  )
  ),
  htmlBr(),
  htmlBr(),
  htmlDiv(
    list(htmlLabel(list('Expected monthly earnings ($USD)', dccInput(id="earning", 
                                              type="number",
                                              placeholder=3000,
                                              value=3000,
                                              style=list('marginRight'='10px')
  )
  )
  )
  )
  ),
  htmlBr(),
  htmlH6("The currency unit has been converted from Euro to USD and the current rate is 1 Euro = 1.14 USD", 
          style=list("justify"= "center", "textAlign"= "center"))
  ),
  style=SIDEBAR_STYLE
  )

comparison_plot <-  htmlDiv(
            list(
              dccGraph(
              id = "comparison_plot")
                                
                 )
)


monthly_surplus <- htmlDiv(
  list(
    dccGraph(
             id = "monthly_surplus")
  )
)


heat_map <-  htmlDiv(
  list(
    dccGraph(
             id = "heat_map")
  )
)
            
property_price <- htmlDiv(
  list(
    dccGraph(
             id = "property_price")
  )
)

  
footer = htmlFooter(list(dccMarkdown(
  "*The raw data for this dashboard was sourced from this 
  [Kaggle dataset](https://www.kaggle.com/joeypp/cost-of-living-numbeo-dataset). 
  For more details about data processing and the dashboard please refer to 
  the projects [GitHub page](https://github.com/UBC-MDS/Cost_of_living_py).*"
)), 
style=list(
  "textAlign"= "center",
  "justify"= "center",
  "margin-top"= 0,
  "margin-bottom"= 0,
  "font-size"= "11px")
)

data_description = dbcAccordion(list(
  dbcAccordionItem(list(
    htmlP("The sum of all monthly costs excluding childcare.")
  ), title = "All"),   
  dbcAccordionItem(list(
    htmlP("Grocery for one person includes average price of basic fruits, vegetables, 
           diary and meat consumed by one person in a month.")
  ), title="Basic Groceries"),
  dbcAccordionItem(list(
    htmlP("Monthly price of private, full day preschool or kindergarden for 1 kid.")
  ), title="Childcare"), 
  dbcAccordionItem(list(
    htmlP("Entertainment cost includes liquor, cigarettes,dining out and movie cost.")
  ), title="Entertainment"),    
  dbcAccordionItem(list(
    htmlP("Fitness club monthly fee for 1 adult")
  ), title="Fitness"),  
  dbcAccordionItem(list(
    htmlP("Rent for one person considers the average of rent for a one-bedroom in 
          city center and outside city center.")
  ), title="Monthly Rent"),
  dbcAccordionItem(list(
    htmlP("Public transportation includes average monthly cost of taxi's and monthly bus passes.")
  ), title="Public Transport"),
  dbcAccordionItem(list(
    htmlP("Shopping includes buying jeans, summer dress, sports shoes, leather shoes once per month.")
  ), title="Shopping"), 
  dbcAccordionItem(list(
    htmlP("Utlities includes monthly bill for Electricity, Heating, Cooling, Water, Garbage, 
          Prepaid Mobile Tariff Local and Internet")
  ), title="Utilities")          
)
)

content <- dbcContainer(
  list(
    dbcCol(
      list(
        dbcTabs(
          list(
            dbcTab(
              list( 
                htmlBr(),
                dbcRow(dbcCard(list(
                  dbcCardHeader('Monthly Cost Comparison'),
                  dbcCardBody(
                    comparison_plot,
                    style=list('width' = '15', 'height'= '450px')
                  )))),
                htmlBr(),
                dbcRow(list(
                dbcCol(dbcCard(list(
                  dbcCardHeader('How much can you save a month?'),
                  dbcCardBody(
                    monthly_surplus,
                    style=list('width' = '15', 'height'= '450px')
                  )))),
                dbcCol(dbcCard(list(
                  dbcCardHeader('Average property price per square meter'),
                  dbcCardBody(
                    property_price,
                    style=list('width' = '15', 'height'= '450px')
                  )))) 
      )),
      htmlBr(),
      dbcRow(list(
        dbcCol(dbcCard(list(
          dbcCardHeader('Map of living costs'),
          dbcCardBody(
            heat_map,
            style=list('width' = '15', 'height'= '450px')
          ))))
      )),
      htmlBr(),
      htmlBr(),
      dbcCol(list(footer))), 
      label = 'Cost of Living Comparison'),
      dbcTab(
        list( 
        htmlBr(),
        "All the data represents the year 2020.",
        htmlBr(),
        htmlBr(),
        data_description
    ), label = 'Monthly Cost Details')
  ), id="tabs-graph")), width=list("offset"= 3))
)
)


app$layout(htmlDiv(list(sidebar, content)))

#saving_bar 

app$callback(
  list(
    output('comparison_plot', 'figure'),
    output('property_price', 'figure'),
    output('monthly_surplus', 'figure'),
    output('heat_map', 'figure')
    ),
  list(
    input('selection_mode', 'value'),
    input('region_selection', 'value'),
    input('city_selection', 'value'),
    input('cost_subset', 'value'),
    input('earning', 'value')
  ),
 
  function(selection_mode, regions, cities, cost_subset, earning) {
    
    # Start filtering data
    SELECTION_REGION = 1L
    SELECTION_CITY = 2L
    

    
    if (selection_mode == SELECTION_REGION) {
      
      subset <- data_df %>% 
        filter(region %in% regions)


    } 
    else if (selection_mode == SELECTION_CITY) {
      subset <- data_df %>% 
        filter(city %in% cities)

    }
    

    comparison_plot <- plot1(subset, cost_subset)
    monthly_surplus <- plot2(subset, earning)
    property_price <- plot4(subset)
    heat_map <- plot3(subset, cost_subset)

    list(comparison_plot, property_price, monthly_surplus, heat_map)
  }
)

app$callback(
  list(
    output('region_selection', 'style'),
    output('city_selection', 'style')
  ),
  list(
    input('selection_mode', 'value')
  ),
  
  function(selection_mode) {

    SELECTION_REGION = 1L
    SELECTION_CITY = 2L
    
    region_style = list('display' = 'none')
    city_style = list('display'= 'none')
    
    if (selection_mode == SELECTION_REGION){
      region_style = list('display' = 'table', 'width' = '100%')
    }
    else if (selection_mode == SELECTION_CITY){
      city_style = list('display' = 'table', 'width' = '100%')
    }
    
    list(region_style, city_style)
  }
)


app$run_server(host = '0.0.0.0')