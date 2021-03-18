library(styler)
library(tidyverse)
library(jsonlite)
library(httr)
library(DT)
library(leaflet)
library(rvest)
library(tidytext)
library(wordcloud)
library(shiny)
library(shinythemes)
library(shinyjs)

readRenviron(".Renviron")

YELP_TOKEN <- Sys.getenv("YELP_TOKEN")

ui <- navbarPage(
    title = "Yelp Search for Vegetarian and Vegan Food",
    tabPanel(
        title = "Search Tool",
        icon = icon("utensils"),
        sidebarLayout(
            sidebarPanel(
                title = "Search bar",
                selectInput(
                    inputId = "select_type", label = "Vegetarian or Vegan?",
                    choices = c("-", "Vegetarian", "Vegan", selected = "-")
                ),
                actionButton(inputId = "type_execute", label = "select"),
                textInput(inputId = "search_location", label = "Enter a location:"),
                p('(i.e. "Davis", "95616", etc.)'),
                actionButton(inputId = "search_execute", label = "enter"),
                selectInput(
                    inputId = "select_restaurant",
                    label = "Select a business for more information:",
                    choices = c("-", "", selected = "-")
                ),
                actionButton(inputId = "select_execute", label = "select"),
                width = 4 
            ),
            mainPanel(
                h2("Restaurants"),
                DT::dataTableOutput("restaurant_list"),
                h3("Information"),
                tabsetPanel( # for all tabs besides map, I decided to use all type object[] from Yelp business details 
                    tabPanel(title = "Map", leafletOutput("map")),
                    tabPanel(
                        title = "Hours", tableOutput("open_hours"),
                        h5(("Day Key"), align = "left"),
                        p("0 = Monday"),
                        p("1 = Tuesday"),
                        p("2 = Wednesday"),
                        p("3 = Thursday"),
                        p("4 = Friday"),
                        p("5 = Saturday"),
                        p("6 = Sunday"),
                        align = "left",
                        style = "margin-left:100px; margin-right:100px"
                    ),
                    tabPanel(
                        title = "Reviews", tableOutput("yelp_reviews"),
                        textOutput("review_wordcloud"), plotOutput("word_cloud")
                    ),
                    tabPanel(title = "Categories", tableOutput("categories")),
                    tabPanel(
                        title = "Pictures",
                        uiOutput("pic_1"),
                        uiOutput("pic_2"),
                        uiOutput("pic_3")
                    )
                ),
                width = 6
            ),
        ),
    ),
    tabPanel(
        useShinyjs(),
        title = "About",
        icon = icon("info"),
        h2(("Purpose of this app"), align = "center"),
        HTML("<hr width=400>"),
        p("The purpose of this app is to help people find restaurants that serve vegetarian or vegan meals.
      Often times, when going to a restaurant that doesn't advertise itself as having vegetarian or vegan options, 
      the only appropriate foods to order would be side dishes or a salad.
      This tool uses the", a("Yelp Fusion API", href = "https://www.yelp.com/developers/documentation/v3"),
          "to give restaurants that would serve", em("actual meals"), 
          "for those who have vegetarian or vegan diets.",
          align = "center",
          style = "margin-left:100px; margin-right:100px"
        ),
        br(),
        p("Users can choose either vegetarian or vegan and then enter a location. 
      For more information, they can select a business. 
      After selecting a business, users can view a map of where the business is located, the business hours,
      reviews with a word cloud of words often mentioned in the reviews, 
      what categories the business is in (what they're known for), 
      and see pictures from the business.",
          align = "center",
          style = "margin-left:100px; margin-right:100px"
        ),
        br(),
        p("Thanks for trying out this app!",
          align = "center",
          style = "margin-left:100px; margin-right:100px"
        ),
        br(),
        p("Made by Michelle Lei for STA141B W21 Final Project, using",a("Yelp Fusion API.", 
                                                                   href = "https://www.yelp.com/developers/documentation/v3"),
          align = "center",
          style = "margin-left:100px; margin-right:100px"
        ),
        fluidPage(theme = shinytheme("lumen"))
    )
)

server <- function(input, output, session) {
    values <- reactiveValues(data = NULL)
    
    # input location if else
    observe(
        if (is.null(input$search_location) || input$search_location == "") {
            disable("search_execute")
            disable("select_execute")
        }
        else {
            enable("search_execute")
        }
    )
    
    observe(
        if (!is.null(input$select_restaurant) && input$select_restaurant != "") {
            enable("select_execute")
        }
    )
    
    observeEvent(input$search_execute, {
        search_yelp <- GET("https://api.yelp.com/v3/businesses/search",
                           add_headers(Authorization = paste("Bearer", YELP_TOKEN)),
                           query = list(
                               term = input$select_type,
                               location = input$search_location)
        )
        json <- content(search_yelp, as = "text", encoding = "UTF-8")
        business <- fromJSON(json, flatten = TRUE)$businesses
        values$business_list <- business
        updateSelectInput(session,
                          "select_restaurant",
                          choices = c("-", business$name, selected = "-")
        )
    })
    
    # Make list of every place that has Vegetarian or vegan Food
    output$restaurant_list <- DT::renderDataTable(
        if (input$search_location == " ") {
            return()
        }
        else {
            if (is_null(values$business_list)) {
                return()
            }
            else {
                values$business_list %>%
                    select(
                        name,
                        is_closed,
                        display_phone,
                        review_count,
                        rating,
                        price,
                        location.address1,
                    )
            }
        },
        options = list(pageLength = 5)
    )
    
    observeEvent(input$select_execute, {
        req(input$select_restaurant != "-", cancelOutput = TRUE)
        pulled_id <- values$business_list %>%
            filter(name == input$select_restaurant) %>%
            pull(id)
        business_id <- str_glue("https://api.yelp.com/v3/businesses/{id}",
                                id = pulled_id
        )
        business_data <- GET(
            business_id,
            add_headers(Authorization = paste("Bearer", YELP_TOKEN))
        )
        
        json2 <- content(business_data, as = "text", encoding = "UTF-8")
        
        business_reviews <- GET(
            str_glue("https://api.yelp.com/v3/businesses/{id}/reviews",
                     id = pulled_id
            ),
            add_headers(Authorization = paste("Bearer", YELP_TOKEN))
        )
        # json3 is for businesses%>%reviews for selected {id}
        json3 <- content(business_reviews, as = "text", encoding = "UTF-8")
        
        # Reviews tab. limited to 3 reviews
        values$reviews <- fromJSON(json3, flatten = TRUE)$reviews %>% select(rating, text)
        
        # map tab. get map output coordinates
        values$coordinates <- fromJSON(json2, flatten = TRUE)$coordinates
        
        # hours tab information
        values$open_hours <- fromJSON(json2, flatten = TRUE)$hours$open[[1]]
        
        # Reviews tab. library(tidytext) values$tokens will hold word cloud words
        # removed stop_words to clean up unimportant words
        values$token <- values$reviews %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            count(word, sort = TRUE) # find the most common words 
        
        # categories tab info
        values$categories <- fromJSON(json2, flatten = TRUE)$categories[[1]]
        
        # photos tab information.
        values$photos <- fromJSON(json2, flatten = TRUE)$photos
    })
    
    # map tab: map output
    output$map <- renderLeaflet(
        if (is_null(values$coordinates)) {
            return()
        }
        else {
            leaflet() %>%
                addProviderTiles(providers$CartoDB.Positron,
                                 options = providerTileOptions(noWrap = TRUE)
                ) %>%
                addMarkers(
                    data = cbind(
                        values$coordinates$longitude,
                        values$coordinates$latitude
                    ),
                    popup = "Located here"
                )
        }
    )
    
    # Hours tab output
    output$open_hours <- renderTable(
        if (input$select_restaurant == "-") {
            return()
        }
        else {
            values$open_hours
        }
    )
    
    # Reviews tab output
    output$yelp_reviews <- renderTable(
        if (is_null(input$select_restaurant)) {
            return()
        }
        else {
            values$reviews
        },
        caption = "Customer Reviews",
        caption.placement = getOption("xtable.caption.placement", "top"),
        caption.width = getOption("xtable.caption.width", NULL)
    )
    
    # Reviews tab: Word cloud for reviews output.
    output$review_wordcloud <- renderText(
        if (is_null(values$token)) {
            return()
        }
        else {
            "Wordcloud from Reviews"
        }
    )
    # plot wordcloud
    output$word_cloud <- renderPlot(
        if (is_null(values$token)) {
            return()
        }
        else {
            values$token %>%
                with(wordcloud(word,
                               freq = n,
                               min.freq = 1, 
                               max.words = 20, 
                               random.order = FALSE,
                               colors = brewer.pal(8, "PRGn"))
                )
        }
    )
    
    # categories tab output. This tab shows categories associated with the business.
    output$categories <- renderTable(
        if (input$select_restaurant == "-") {
            return()
        }
        else {
            values$categories
        },
        caption = "Categories associated with this business",
        caption.placement = getOption("xtable.caption.placement", "top")
    )
    
    # Pictures tab showing 3 images in the photo album.Yelp API limits the output to 3 images max.
    output$pic_1 <- renderUI(
        if (is_null(input$select_restaurant)) {
            return()
        }
        else {
            tags$img(src = values$photos[1], style = "width: 300px")
        }
    )
    
    output$pic_2 <- renderUI(
        if (is_null(input$select_restaurant)) {
            return()
        }
        else {
            tags$img(src = values$photos[2], style = "width: 300px")
        }
    )
    
    output$pic_3 <- renderUI(
        if (is_null(input$select_restaurant)) {
            return()
        }
        else {
            tags$img(src = values$photos[3], style = "width: 300px")
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
