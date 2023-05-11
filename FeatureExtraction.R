# load library
library(imager)
library(tidyverse)


lettersList <- list.files(path = "images/letters")

# Prepare dataset
columns = c("Letter")
for (i in 1:16) {
  columns = append(columns, i)
}
df = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(df) = columns

numOfQuad = 4

# Iterate through all letters
for (i in 1:length(lettersList)) { # 1:length(lettersList)
  path = lettersList[i]
  # Get letter name
  str = gsub(".png", "", path)
  # Create row of dataset
  row = c(str)
  
  path = paste0("images/letters/", path)
  print(path)
  img = load.image(path)
  
  width = dim(img)[1]
  height = dim(img)[2]
  
  spanX = width / 4
  spanY = height / 4
  x1 = 1
  x2 = spanX
  y1 = 1
  y2 = spanY
  
  # Iterate quadrants in image
  for (j in 1:numOfQuad) {
    for (k in 1:numOfQuad) {
      # Get target quadrant
      target_img <- imsub(img, x %inr% c(x1, x2), y %inr% c(y1, y2))

      targetWidth = dim(target_img)[1]
      targetHeight = dim(target_img)[2]

      # Sum black pixels
      sum = 0
      for (l in 1:targetHeight) {
        for (m in 1:targetWidth) {
          # Check if pixel is 1 == White or 0 == Black
          sum = sum + color.at(target_img, m, l)[1]
        }
      }
      targetSize = targetWidth * targetHeight
      # Switch Black and White pixels, to count Blacks
      sum = targetSize - sum
      # Get average Black pixels of quadrant
      avgBlkPixels = sum / targetSize
      avgBlkPixels = format(round(avgBlkPixels, 2), nsmall = 2)
      row = append(row, avgBlkPixels)

      x2 = x2 + spanX
      x1 = x1 + spanY
    }
    x1 = 1
    x2 = spanX
    y1 = y1 + spanY
    y2 = y2 + spanY
  }
  # Transform list to df
  df2 = data.frame(row)
  # Add row to df
  df[nrow(df) + 1, ] = row
}

# Set letters as index
rownames(df) <- df$Letter
# Remove leftover letters column
df$Letter <- NULL
# Write file to disk
write.csv(df, file='data/letters.csv',fileEncoding = "UTF-8")
print("DONE")