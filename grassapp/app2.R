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
    #titlePanel("What direction do you think the grass is pointing?", align = "center"),
    h2(strong("Grass Orientation Study"), align = "center", style = "margin-top: 40px; margin-bottom: 20px;"),
    
    # Instructions Section
    div(style = "background-color: #f9f9f9; padding: 15px; margin-bottom: 20px;",
        h4(strong("Instructions:")),
        tags$ol(
          tags$li("Observe the grass orientation in the image."),
          tags$li(strong("Click a spot on the image"), " to point the arrow toward the direction most of the grass is pointing."),
          tags$li("Click ", strong("'Submit & Next Image'"), " to save your response.")
        )
    ),
    
    # Notes
    p("Thank you for your time! We appreciate as many responses as you are willing to provide. You may exit at any time by simply closing your browser window."),
    
    hr(),
    
    # Image display
    div(plotOutput("img_display", click = "plot_click", width = "500px", height = "500px"), align = "center"),
   
    # Counter and Button
    div(style = "text-align: center; margin: -10px;",
        h4(textOutput("counter"), style = "margin-top: 25px; margin-bottom: 10px;"),
        actionButton("submit_btn", "Submit & Next Image", style = "margin-top: 20px; margin-bottom: 20px;")
    ),
    
    hr(),

    div(class = "section-header",
        h4(strong("More About This Study:"), style = "margin-top: 25px; margin-bottom: 20px;")
    ),
    div(style = "background-color: #f9f9f9; padding: 20px; border-radius: 10px",
        p(strong("Background Information: "), "This application is part of a Senior Year Experience (SYE) Honors project at St. Lawrence University. 
        Our goal is to develop reliable and efficient methods for measuring grass orientation from field images to better understand 
        changing wind patterns in Savoonga, Alaska. In Arctic communities, residents have long relied on Traditional 
        Ecological Knowledge (TEK), in particular, physically observing how grass lays, to guide hunting and subsistence activities. However, 
        changes in climate and a lack of historical wind data in harsh weather conditions have made these patterns harder to predict and keep track of. 
        We are using components of the Histogram of Oriented Gradients (HOG) algorithm to automate the detection of these orientations."),
        p(strong("Why You Are Clicking: "), "Your input provides the human-verified \"ground truth\" needed to validate our algorithm. By comparing your observations
          to our automated results, we can ensure our tool is accurate enough to support community-led climate monitoring."),
        p(strong("Further Information: "), "This research is being conducted by Francesca Mnenula under the supervision of Dr. Ivan Ramler within the Department of Mathematics, Statistics, and Computer Science. For questions or 
          additional information, please contact Dr. Ivan Ramler at iramler@stlawu.edu.")
        ), 
    br(), br(), br()
    )

server <- function(input, output, session) {
  # set seed for randomness sampling
  set.seed(as.numeric(Sys.time()))
  
  # go through the sequence of images
  random_order <- sample(seq_along(image_urls))
  
  # get the url for the current one sampled
  randomized_urls  <- image_urls[random_order]
  randomized_names <- image_names[random_order]
  
  # Initialise idx value for images
  idx <- reactiveVal(1)
  
  # Generate a unique ID for THIS specific user session
  # This stays the same for the user until they refresh the page
  user_session_id <- uuid::UUIDgenerate() 
 
  # Initialise the image orientation by picking a random rotation (could be 0, 90, 180, or 270 degrees)
  rotation <- reactiveVal(sample(c(0, 90, 180, 270), 1))
  
  # Initialise the angle that the user will change arrow to point in
  current_angle <- reactiveVal(0)
  
  # Display the counter
  output$counter <- renderText({
    paste0("Image: ", idx(), " / ", length(image_urls))
  })
  
  # Observe the click
  observeEvent(input$plot_click, {
    req(input$plot_click)
    
    # Find the coordinates of the point the user clicked (had to multiple by 100 because the regular coord are btwn 0 and 1)
    click_x <- input$plot_click$x * 100
    click_y <- input$plot_click$y * 100
    
    # Calculate distance from center (50,50)
    dx <- click_x - 50
    dy <- click_y - 50
    
    # Convert x,y to degrees (using atan2)
    # Gemini: R's atan2 is (y, x). Adjust the calculation to make 0 degrees "North/Up"
    res_angle <- (atan2(dx, dy) * 180 / pi) %% 360
    
    # Now update the initialised current_angle
    current_angle(res_angle)
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
  
  output$img_display <- renderPlot({
    
    # Get current image
    img_url <- randomized_urls[idx()]
    
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
        linewidth = 7, 
        color = "#4B0082", 
        arrow = arrow(length = unit(12, "points"), type = "closed", angle = 20)
      ) +
      coord_fixed(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE) + 
      theme_void() +
      theme(plot.margin = margin(0,0,0,0, "pt"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
