# SYE_Grass_Study

## Overview
This project is analyzing the orientation of grass in using image-based data. This project aims to support research on how to recalibrate Traditional Ecological Knowledge in the Savoonga region in Alaska using machine learning techniques as an automated method of retrieving orientation data.

### Code
The code folder has code for running the HOG algorithm on the 65 grass images. This code folder is called grass_images_hog. The other 2 folders are currently just tests or storage for some code that was being edited.

### GrassApp
This folder has two versions of the grass orientation study shiny app. The working version is called app2.R. App1.R is a version that utilises ggplot to display the image. This version was slow and generated too much memory. 

### Images
This folder has the 65 grass images that are being used for the study. These are images taken from Dr. Rosales' folder taken from the St. Lawrence University Living Lab.

### Keep
This folder has AI images that I generated at the beginning of this study as we tried to experiment how well we can retrieve "known/AI-generated" orientations of grass. There are also the uncropped images saved here. There is also the grass_df_og which is the original version of the data of circular statistics of the uncropped images. 

### Output
In this folder, you can find the gras_df file of the circular statistics of the images called grass_df.csv under the weighted_datasets subfolder.
