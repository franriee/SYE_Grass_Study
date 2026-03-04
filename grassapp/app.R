# This is a version of the grassapp that uses plotly 


# Load libraries
library(shiny)
library(plotly)
library(curl)
library(base64enc)
library(jsonlite)
library(googlesheets4)
library(uuid)

# Google sheets connection
gs4_auth(path = "sye2026-5dd46ac66f14.json")
sheet_url <- "https://docs.google.com/spreadsheets/d/19002kMeTJ4caXqI836P4sCkHAB-WvlcL3er5T0K8E-o/edit#gid=198629460"

# Source images from github repo
folder_api <- "https://api.github.com/repos/franriee/SYE_Grass_Study/contents/images"
files <- jsonlite::fromJSON(folder_api)

# Create a data frame to keep the image name and URL synced
img_master <- data.frame(
  name = URLdecode(basename(files$download_url)), # get the file name without the space encoding
  url = files$download_url,
  stringsAsFactors = FALSE
)

# Shuffling the master data frame
# Firstly, retrieve the number of rows/images in the folder -- nrow()
# Then create a scrambled list of those numbers -- sample()
# Now store img_master as the scrambled df of images
set.seed(as.numeric(Sys.time())) # Initialize random seed using system time to vary results each run
img_master <- img_master[sample(nrow(img_master)), ]

# Preload the images to lessen loading time as the user moves between images

# Taking the url web address of each image and converting it into 
# text that the browser can read quicker
# Create a col url to add to img_master that can keep the text version of the url
message("Loading images...")
img_master$uri <- sapply(img_master$url, function(u) {
  raw <- curl::curl_fetch_memory(u)$content
  paste0("data:image/png;base64,", base64encode(raw))
})

# UI for application
ui <- fluidPage(
  # Application title
  h2(strong("Grass Orientation Study"), align = "center"),
  
  # Instructions Section
  div(style = "background-color: #f9f9f9; padding: 15px; margin-bottom: 20px;",
      h4(strong("How to participate:")),
      tags$ol(
        tags$li("Look at the general flow of the grass in the photo below."),
        tags$li(strong("Click anywhere on the image"), " and the purple arrow will point toward your click."),
        tags$li("You can adjust the angle of the arrow by clicking different spots until it matches the grass 
                direction of most of the grass in the photo. (Note that the length of the arrow doesn’t matter for this study.)"),
        #tags$li("Adjust the arrow by clicking different spots until it matches the grass direction of most of the grass in the photo."),
        tags$li("Click ", strong("Submit & Next Image"), " to lock in your answer.")
      )
  ),
  
  # Notes
  p("Thank you for your time! We appreciate as many responses as you are willing to provide. Please note that you may exit at any time by simply closing your browser window."),
  
  hr(),
  
  div(style = "max-width: 600px; margin: auto; display: flex; flex-direction: column; align-items: center;",
      #h4(strong(textOutput("image_name_display")), style = "margin-bottom: 15px;"), # view name of image on screen
      plotlyOutput("img_display", width = "500px", height = "500px"), 
      div(style = "margin-top: 15px;text-align: center;",
          h4(textOutput("counter")),
          actionButton("submit_btn", "Submit & Next Image", class = "btn-primary")
      )
  ),
  
  hr(),
  
  div(class = "section-header",
      h4(strong("More About This Study:"), style = "margin-top: 25px; margin-bottom: 20px;")
  ),
  div(style = "background-color: #f9f9f9; padding: 20px; border-radius: 10px",
      p(strong("Background Information: "), "This application is part of a Data Science Senior Year Experience (SYE) Honors project at St. Lawrence University. 
        Our goal is to develop reliable and efficient methods for measuring grass orientation from field images to better understand 
        changing wind patterns in Savoonga, Alaska. In Arctic communities, residents have long relied on Traditional 
        Ecological Knowledge (TEK), in particular, physically observing how grass lays, to guide hunting and subsistence activities. However, 
        changes in climate and a lack of historical wind data in harsh weather conditions have made these patterns harder to predict and keep track of. 
        We are using components of the Histogram of Oriented Gradients (HOG) algorithm to automate the detection of these orientations."),
      p(strong("Why You Are Clicking: "), "Your input provides the human-verified \"ground truth\" needed to validate our algorithm. By comparing your observations
          to our automated results, we can ensure our tool is accurate enough to support community-led climate monitoring."),
      p(strong("Further Information: "), "This research is being conducted by Francesca Mnenula under the supervision of Dr. Ivan Ramler within the Department of Mathematics, Statistics, Data Science, and Computer Science. For questions or 
          additional information, please contact Dr. Ivan Ramler at iramler@stlawu.edu.")
  ), 
  br(), br(), br())

# Server
server <- function(input, output, session) {
  output$image_name_display <- renderText({
    # Pull the name column from the row matching our current index
    img_master$name[idx()]
  })
  
  # Initialise idx value for images
  idx <- reactiveVal(1) 
  
  # Initialise the angle that the user will change arrow to point in
  current_angle <- reactiveVal(0)
  
  # Initialise the image orientation by picking a random rotation (could be 0, 90, 180, or 270 degrees)
  rotation <- reactiveVal(sample(c(0, 90, 180, 270), 1))
  
  # Generate a unique ID for this specific user session
  # This stays the same for the user until they refresh the page
  user_session_id <- UUIDgenerate()
  
  # Display the counter
  output$counter <- renderText({
    paste0("Image: ", idx(), " / ", nrow(img_master))
  })
  
  # Draw and render the plot
  output$img_display <- renderPlotly({
    req(img_master$uri[idx()])
    
    n <- 100
    g <- seq(0, 1, length.out = n)
    z <- matrix(0, n, n)
    
    plot_ly(
      x = g, y = g, z = z,
      type = "heatmap",
      source = "grass_plot",
      showscale = FALSE,
      hoverinfo = "none",
      colorscale = list(
        list(0, "rgba(0,0,0,0)"),
        list(1, "rgba(0,0,0,0)")
      )
    ) %>%
      layout(
        clickmode = "event",
        xaxis = list(range = c(0,1), visible = FALSE, fixedrange = TRUE),
        yaxis = list(range = c(0,1), visible = FALSE, fixedrange = TRUE, scaleanchor = "x"),
        images = list(list(
          source = img_master$uri[idx()],
          x = 0, y = 1,
          sizex = 1, sizey = 1,
          xref = "x", yref = "y",
          sizing = "contain",
          layer = "below"
        )),
        margin = list(l=0, r=0, t=0, b=0),
        annotations = list(list(
          x = 0.5, y = 0.8,
          ax = 0.5, ay = 0.5,
          xref = "x", yref = "y",
          axref = "x", ayref = "y",
          showarrow = TRUE,
          arrowwidth = 5,
          arrowhead = 3,
          arrowcolor = "#4B0082"
        ))
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Listen for the event that will happen upon plotly click and then process the 
  # changing of the arrow
  # get click coordinates
  # require that the click is not null
  observeEvent(event_data("plotly_click", source = "grass_plot"), {
    d <- event_data("plotly_click", source = "grass_plot") 
    req(d) 
    
    # Angle calculation
    # Plot is on 0 to 1 scale so this is the center
    dx <- d$x - 0.5
    dy <- d$y - 0.5
    
    # Convert x,y to degrees (using atan2)
    # Gemini: R's atan2 is (y, x). Adjust the calculation to make 0 degrees "North/Up"
    res_angle <- (atan2(dx, dy) * 180 / pi) %% 360
    
    # Now update the initialised current_angle
    current_angle(res_angle)
    
    # plotlyProxy to modify only the annotation layer of the plot
    # relayout updates the arrow position instantly without re-rendering the background grass image
    plotlyProxy("img_display", session) %>%
      plotlyProxyInvoke("relayout", list(
        annotations = list(list(
          x = d$x, y = d$y,
          ax = 0.5, ay = 0.5,
          xref = "x", yref = "y",
          axref = "x", ayref = "y",
          showarrow = TRUE,
          arrowwidth = 5,
          arrowhead = 3,
          arrowcolor = "#4B0082"
        ))
      ))
  })
  # Submitting the entry
  observeEvent(input$submit_btn, {
    # Get the click
    d <- event_data("plotly_click", source = "grass_plot")
    req(!is.na(current_angle()))
    
    # Calculate the magnitude of the arrow from the center
    dx <- d$x - 0.5
    dy <- d$y - 0.5
    mag <- sqrt(dx^2 + dy^2)
    
    # Upon submission, create the row of data by creating a df
    # Make sure to retrieve the name of the image the user is seeing
    # Add the new data to the Google Sheet (sheet)
    sheet_append(sheet_url, data.frame(
      UserID = user_session_id,
      Image = img_master$name[idx()], # What row is the user viewing?
      User_Angle = round(current_angle(), 2),
      True_Angle = round((current_angle() - rotation()) %% 360, 2),
      Rotation = rotation(),
      Timestamp = format(Sys.time(), tz = "America/New_York"),
      Coord_x = round(d$x, 2),
      Coord_y = round(d$y, 2),
      Magnitude = round(mag, 2)
    ), sheet = "Results")
    
    # If we are not at the end, then
    # Automatically moving to next image after submitting
    if (idx() < nrow(img_master)) {
      idx(idx() + 1)
      # Pick a new rotation
      rotation(sample(c(0,90,180,270), 1))
      # Reset the angle to 0 for the next ima
      current_angle(0)
      plotlyProxy("img_display", session) %>%
        plotlyProxyInvoke("relayout", list(annotations = list()))
    } else {
      # When user is done, tell them the study is complete
      showModal(modalDialog(
        p("Thank you for contributing to this SYE Study. Your data has been saved. You may now close this browser window."),
      ))
    }
  })

}

shinyApp(ui, server)