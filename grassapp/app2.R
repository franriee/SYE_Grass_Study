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
    div(plotOutput("img_display", click = "plot_click", width = "500px", height = "500px"), align = "center"),
    
    h4(uiOutput("angle_text"), align = "center"),
    
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
  observeEvent(input$plot_click, {
    req(input$plot_click)
    
    click_x <- input$plot_click$x * 100
    click_y <- input$plot_click$y * 100
    
    # Calculate distance from center (50,50)
    dx <- click_x - 50
    dy <- click_y - 50
    
    # Convert x,y to degrees (using atan2)
    # R's atan2 is (y, x). We adjust math to make 0 degrees "North/Up"
    res_angle <- (atan2(dx, dy) * 180 / pi) %% 360
    current_angle(res_angle)
    
    # debug
    print(paste("Click X:", click_x, "Click Y:", click_y))
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
    sheet_append(sheet, new_data, sheet = "Sheet2")
    
    # Automatically moving to next image after submitting
    new_idx <- idx() + 1
    if (new_idx > length(image_urls)) new_idx <- 1
    idx(new_idx) # reset idx to the new_idx
    
    # Pick a new rotation
    rotation(sample(c(0, 90, 180, 270), 1))
    
    # Reset the arrow to 0 for the next image
    current_angle(0)
  })
  
  # Moving to the next image with a next btn
  #observeEvent(input$next_btn, {
   # new_idx <- idx() + 1
    # if (new_idx > length(image_urls)) new_idx <- 1
    # idx(new_idx)
    
    # Reset the slider to 0 for the new image
    #updateSliderInput(session, "user_value", value = 0)
  #})
  
  # If you want to show the image name to the users
  # output$img_name <- renderText({
  #  image_names[idx()]
  # })
  
  output$img_display <- renderPlot({
    
    # Get current image
    img_url <- image_urls[idx()]
    
    # Variables for the coordinate plane
    xlim <- c(0, 100)
    ylim <- c(0, 100)
    
    # Start point is the center of the plot at (50,50)
    x0 <- 50
    y0 <- 50
    
    # User input from click
    angle <- current_angle()
    
    # Radius of the lines
    r <- 40
    
    # The direction a line points will be based on the angle so find the endpoint by multiplying the angle by the radius + starting point
    x1 <- x0 + r * sin(angle * pi / 180)
    y1 <- y0 + r * cos(angle * pi / 180)
    
    # Draw the cartesian plot
    ggplot() +
      geom_image(aes(x = 50, y = 50, image = img_url), 
                 size = 1.5,
                 angle = rotation()) +
       geom_segment(
        aes(x = x0, y = y0, xend = x1, yend = y1), 
        linewidth = 2,
        color = "red",
        arrow = arrow(length = unit(10, "points"))
      ) +
      coord_fixed(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE) + 
      theme_void() +
      theme(plot.margin = margin(0,0,0,0, "pt"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
