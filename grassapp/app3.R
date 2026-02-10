#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(shiny)
library(httr)
library(jsonlite)
library(ggimage)
library(googlesheets4)
library(uuid)
library(plotly)

# Google sheets
gs4_auth(path = "sye2026-5dd46ac66f14.json")
sheet <- "https://docs.google.com/spreadsheets/d/19002kMeTJ4caXqI836P4sCkHAB-WvlcL3er5T0K8E-o/edit?gid=198629460#gid=198629460"

# Source images from my github repo
folder_api <- "https://api.github.com/repos/franriee/SYE_Grass_Study/contents/images"
res <- GET(folder_api)
files <- fromJSON(content(res, "text"))
image_urls <- files$download_url
image_names <- basename(image_urls)

# UI for application
ui <- fluidPage(

    # Application title
    titlePanel("What direction do you think the grass is pointing?"),
    
    # If I want to show the image name to the users
    # Image name
    # h3(textOutput("img_name"), align = "left"),
    
    # Image display
    plotlyOutput("img_display", height = "500px", width = "500px"),
    
    h4(textOutput("angle_text"), align = "center"),
    
    br(),
    
    h4(textOutput("counter"), align = "center"), 
   
    br(),
    
    # Text instructions
    h4(strong("Instructions:"), align = "left"),
    p("1. Click the grass to the point that you think most of the grass is laying in.",
      br(),
      "2. Click \"Submit & Next Image\" when you are done.", 
      br(), 
      "3. To exit out of the program, click the X button on the window. Thank you!", 
      align = "left"),
    
    br(),
    
    p(strong("NB:"), "Some images have been rotated.", align = "left"),
    
    br(),
    
    # A Submit Button to submit the data and move to the next image
    actionButton("submit_btn", "Submit & Next Image")
    
)

server <- function(input, output, session) {
  # set seed for randomness sampling
  set.seed(2026)
  
  # Initialise idx value for images
  idx <- reactiveVal(1)
  
  # Generate a unique ID for THIS specific user session
  # This stays the same for the user until they refresh the page
  user_session_id <- uuid::UUIDgenerate() 
 
  # Initialise the image orientation by picking a random rotation (could be 0, 90, 180, or 270 degrees)
  rotation <- reactiveVal(sample(c(0, 90, 180, 270), 1))
  
  # Initialise the angle that the user will change arrow to
  current_angle <- reactiveVal(0)
  
  # Display the angle the user will chose
  output$angle_text <- renderText({
    paste0("Selected Angle: ", round(current_angle(), 0), "°")
  })
  
  # Display the counter
  output$counter <- renderText({
    paste0("Image: ", idx(), " / ", length(image_urls))
  })
  
  # Observe the click
  observe({
    # "source = 'A'" tells Plotly which plot to listen to
    click_data <- event_data("plotly_click", source = "A")
    
    req(click_data)
    
    # Plotly returns coordinates directly on your 0-100 scale! 
    # No need to multiply by 100 anymore.
    dx <- click_data$x - 50
    dy <- click_data$y - 50
    
    res_angle <- (atan2(dx, dy) * 180 / pi) %% 360
    current_angle(res_angle)
    
    print(paste("Click X:", click_data$x, "Click Y:", click_data$y))
  })
  
  # Update google sheet with updates
  observeEvent(input$submit_btn, {
    # Calculate the actual angle (Gemini calculation asked what calculation to use to retrieve angle value given different rotations)
    true_angle <- (current_angle() - rotation()) %% 360
    
    # Upon submission, create the row of data by creating a df
    new_data <- data.frame(
      UserID = user_session_id,      
      Image = image_names[idx()],
      User_Angle = current_angle(),
      True_Angle = true_angle,
      Rotation = rotation(),
      Timestamp = as.character(Sys.time())
    )
    
    # Add the new data to the Google Sheet (sheet)
    sheet_append(sheet, new_data, sheet = "Sheet3")
    
    # Automatically moving to next image after submitting
    new_idx <- idx() + 1
    if (new_idx > length(image_urls)) new_idx <- 1
    idx(new_idx) # reset idx to the new_idx
    
    # Pick a new rotation
    rotation(sample(c(0, 90, 180, 270), 1))
    
    # Reset the arrow to 0 for the next image
    current_angle(0)
  })
  
  
  output$img_display <- renderPlotly({
    # Calculate arrow endpoint based on the angle
    r <- 40
    angle <- current_angle()
    x1 <- 50 + r * sin(angle * pi / 180)
    y1 <- 50 + r * cos(angle * pi / 180)
    
    # We add source = "A" here
    plot_ly(source = "A") %>%
      layout(
        images = list(
          list(source = image_urls[idx()],
               xref = "x", yref = "y",
               x = 0, y = 100,
               sizex = 100, sizey = 100,
               sizing = "stretch", layer = "below")
        ),
        xaxis = list(range = c(0, 100), visible = FALSE, fixedrange = TRUE),
        yaxis = list(range = c(0, 100), visible = FALSE, fixedrange = TRUE)
      ) %>%
      add_segments(x = 50, y = 50, xend = x1, yend = y1, 
                   line = list(color = 'red', width = 4),
                   hoverinfo = "none") %>%
      config(displayModeBar = FALSE) # Hides the plotly menu for a cleaner look
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
