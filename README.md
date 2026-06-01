# Estimating Grass Orientation from Drone Images to Determine Wind Patterns

### Author Details
Researcher: Francesca Mnenula, B.S. Data Science and Economics, St. Lawrence University  
Advisor: Dr. Ivan Ramler, Jack and Sylvia Burry Chair in Statistics, Associate Professor, St. Lawrence University

### Overview
This repository contains the code, data, and supporting materials for my Honors Thesis in Data Science at St. Lawrence University:

This project titled "Estimating Grass Orientation from Drone Images to Determine Wind Patterns" explores whether image gradient analysis techniques can be used to estimate dominant tundra grass orientation from aerial drone imagery and evaluate its relationship to prevailing wind patterns. The broader motivation is to investigate computational approaches that may support the interpretation and recalibration of Traditional Ecological Knowledge (TEK) in Arctic environments like the Savoonga region in Alaska.

Using drone imagery collected at the St. Lawrence University Living Laboratory, I developed an R-based image analysis algorithm inspired by components of the Histogram of Oriented Gradients (HOG) method. The resulting orientation estimates were compared with both historical wind data and human assessments collected through a custom R Shiny application.

The main research questions being answered include: can image gradient analysis estimate dominant grass lay direction from aerial drone imagery; how closely do computated gradient analysis estimates align with human perception of the same images; and how well does grass orientation correspond with measured wind patterns?

### Methods
The analysis pipeline includes:
- Image preprocessing and cropping
- Pixel-level gradient extraction
- Orientation estimation using weighted circular statistics
- Comparison against wind observations
- Human validation through a custom R Shiny application

Key techniques used:
- Image gradient analysis
- Histogram of Oriented Gradients (HOG) inspired methodology
- Circular statistics
- Environmental/weather data analysis
- Interactive web application development in R Shiny

### Folder Structure
This repository contains the code, data, applications, and outputs developed throughout the project. The primary folders are summarized below.

1. code/: Contains the R quarto documents used for image processing, gradient analysis, circular statistics calculations, and orientation estimation. The main analysis pipeline is in the grass_images_gradient_analysis document that applies a HOG-inspired approach to estimate dominant grass orientation from images.
2. grassapp/: Contains the source code for the R Shiny application used in the human validation study. Participants viewed grass images and recorded their perceived dominant orientation, allowing comparison between human assessments and computational estimates.
3. images/: Contains the 65 drone images of tundra grass used throughout the study. These images were collected at the St. Lawrence University Living Laboratory and served as the primary dataset for orientation analysis.
4. humangrassdata/: Contains response data collected through the Shiny validation study, including participant assessments of grass orientation.
5. winddata/: Contains wind observations and processed wind datasets used to compare estimated grass orientation against prevailing wind patterns.
6. output/: Stores generated analysis outputs, including circular statistics summaries, processed datasets, and intermediate results used throughout the project.
7. results/: Contains documents used for oral presentations.
8. rot0/, rot90/, rot180/, rot270/: Contain rotated versions of the grass image dataset.
9. media/: Contains figures, visualizations, and supporting media used in project documentation and presentations.
10. docs/: Contain files used to generate and host the project's GitHub Pages website.
11. keep/: Archive folder containing initial exploratory analyses, uncropped images, AI-generated test images, and other materials retained for reference but not used in the final analysis.

### Human Validation
To evaluate the image-based orientation estimates, a custom R Shiny application was developed where volunteers assessed dominant grass orientation from drone imagery. The resulting responses were compared against gradient-based estimates to assess agreement between computational methods and human perception.

### Results
The analysis found promising agreement between human assessments of grass orientation, gradient-based orientation estimates, and recorded wind observations. These findings suggest that image gradient analysis may provide a useful quantitative approach for studying tundra orientation and environmental wind patterns from aerial imagery.

### Interactive Project Website
You can find the project webite at: https://franriee.github.io/SYE_Grass_Study/. The website includes: a full methodology, statistical analysis, visualizations, results, and discussion.

### Acknowledgments
Special thanks to Dr. Ivan Ramler and the broader Arctic wind and Traditional Ecological Knowledge (TEK) research initiative at St. Lawrence University being led by Dr Jon Rosales for their guidance and support throughout this project.

