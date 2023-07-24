# The following script is an attempt  to lear image analysis with R.
# The code is a replica of what it done in https://www.youtube.com/watch?v=nkX_gQKsFzQ&t=299s&ab_channel=codeRtime
# I am only doing the best I can to learn and leave a public reccord of what I am learning.
# Also, I comment the code to make each step more understandable. 

# Libraries

library(magick)
library(tidyverse)
library(caret)

## Basics of Image Analysis 

# Realizing that images can be seen as a data matrix 

# Creating a matrix that represents an image

image_matrix <- matrix(0, ncol = 10, nrow= 10)
image_matrix[2,] <- 255
image_matrix[,2] <- 255
image_matrix[9,] <- 255
image_matrix[,9] <- 255

# Visualizing the matrix as an image

image(x  = 1:10, y =  1:10, z = image_matrix, col = gray.colors(100))


# Using an png image in R

current_image <- image_read("Tree.png")
current_image

# Once the image is read we have to turn it into a matrix 

current_image_data <- as.numeric(current_image[[1]][1,,])

View(current_image_data)

# Here we watch the result

current_image_data[1:20]

# we confirm that has the same lenght as the number of pixels
612*522
length(current_image_data)


# Playing arround with the image

image_negate(current_image)

current_image %>%  image_convolve("Dog:0,0,0.5")

current_image %>% image_flip()


## Yale Faces Dataset

# We are going to use Data Science techniques to identify patterns in the Yale's Faces dataset.
# The pattern we are going to be focusing is the light angle on the pictures.
# In the Yale's Faces Dateset there are several images in which the light is proyected on the: right side
# left side, center. The purpose of this code is to identify the images based on the side that the light is projected on.
 
# We create an empty matrix where data is to be allocated

face_data_matrix <- NULL


# We make R read each of the images, since there are several images and not all of them are important to our code we
# use the pattern "light" in list.files to only read the relevant images.
# we then convert the image into matrix, deposit the matrix created on the "face_data_matrix"  that we created before
# and give each row a name according to the specific image from where the data comes.

for (i in list.files(path = "data", pattern = "light", full.names = T)){
  current_image <-image_read(i)
  current_image_data <- as.numeric(current_image[[1]][1,,])
  face_data_matrix <- rbind(face_data_matrix, current_image_data)
  
  
}

# Here we give row names   
rownames(face_data_matrix) <- list.files(path = "data", pattern = "light")

face_data_matrix[1:4, 1:4]
  
# We confirm the number of images  with the number of pixels each has.   

dim(face_data_matrix) 
45*77760
length(face_data_matrix)
# We perform a principal component analysis on the images to find patterns

face_pca <- prcomp(face_data_matrix)

# We plot 
plot(face_pca$x[, 1:2])

# There is a pattern but is not clear which images belong to the which cluster. 
# To make it clear we first identify the rows according to the light angle
# and later we color the point on the Principal Components Analysis accordingly


rightlight_indexes <- grep(rownames(face_pca$x), pattern = "rightlight")
centerlight_indexes <-  grep(rownames(face_pca$x), pattern = "centerlight")
leftlight_indexes <- grep(rownames(face_pca$x), pattern = "leftlight")
points(face_pca$x[rightlight_indexes, 1:2],col= "blue")
points(face_pca$x[centerlight_indexes, 1:2],col= "grey")
points(face_pca$x[leftlight_indexes, 1:2],col= "red")

# The identification is pattern seem correct for the majority of the images. I will later review the "right light" image
# that according to the Principal Component Analysis is more similar to a center light image.

# We are now going to use a Machine Learning Algorithm to identify an unlabeled image.
# This is going to be done by removing the identification  of our images.
# Then we use the machine learning model to reidentify the label

# Lest start by assembling the relevant data.

# I make a matrix that has a column with the values 0, 1, 2 depending on if the image is identified by Yale as
# center light, left light or right light accordingly. As well, the matrix keeps the Principal Component Values of all images.

light <-rep(0, times = nrow(face_pca$x))
light[leftlight_indexes] <- 1
light[rightlight_indexes] <- 2
data_matrix <- cbind(light, face_pca$x)

# I "filter" the data to identify the "right light" image that was not convincing enough on the previous chart

data_matrix %>% 
  as.data.frame(.) %>% 
  filter(light == 2)

# After reviewing the "right light" image that appear to be center light I think it is a ambiguous image.
# I will let it on the dataset to keep it as Yale intended.


# The model

# Random Forest
# This is a good algorithm to make classifications that consists in building decision trees that classify the observations.

model <- train(as.factor(light)~. , data = data_matrix,  trControl = trainControl(method = "LOOCV"), method = "rf" )


# We use a confusion matrix to get an idea of the model performance


model$finalModel


# We see that for most instances the model was able to predict the light correctly.
# But it is in the right light images where more mistakes were made.