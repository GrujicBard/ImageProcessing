# load library
library(imager)

# variables
tresh = 0.9
crop = 3

# read image
img = load.image("images/alphabet.png")

# get width and height of image
width = dim(img)[1]
height = dim(img)[2]

# span px between letters (can be changed to inputs)
spanX = width/25
spanY = height/200

# array of alphabet characters
arr_alphabet = letters

arr_alphabet = arr_alphabet[! arr_alphabet %in% c('q', 'w', 'y', 'x')]
arr_alphabet = append(arr_alphabet,'č',after = 3)
arr_alphabet = append(arr_alphabet,'š',after = 19)
arr_alphabet = append(arr_alphabet,'ž',after = 25)


# pixels with value x < tresh => black and x > tresh => white
img <- threshold(img, tresh)

# switch black and white pixels
#img <- 1 - img


# c() combines values into a vector
# %inr% A shortcut for x >= a | x <= b

x1 = 1
x2 = spanX
y1 = 1
y2 = spanY

# counts to 100 letters
counter_1 = 1

# index at arr_alphabet
counter_2 = 1

isUppercase = TRUE

# can change loop to variables
for(i in 1:200){
  for(j in 1:25){
    
    # create path
    str = "images/letters/"
    
    # uppercase or lowercase letter
    letter = arr_alphabet[counter_2]
    letter = toupper(letter)
    
    if(!isUppercase){
      letter = paste(letter, arr_alphabet[counter_2])
    }
    
    str = paste(str, letter)
    str = paste(str, "_")
    
    # add numbering
    if(counter_1 < 10){
      str = paste(str, "00")
    }else if(counter_1 < 100){
      str = paste(str, "0")
    }
    
    str = paste(str, counter_1)
    
    # remove " "
    str = gsub(" ", "", paste(str, ".png"))

    # iterate letters in image
    target_img <- imsub(img, x %inr% c(x1, x2), y %inr% c(y1, y2))
    # crop borders (optional)
    target_img = crop.borders(target_img, nPix = crop)
    
    save.image(target_img, str)
    x2 = x2 + spanX
    x1 = x1 + spanX
    counter_1 = counter_1 + 1
    if(counter_1 >= 101){
      counter_1 = 1
      if(!isUppercase){
        # move index at arr_alphabet
        counter_2 = counter_2 + 1
      }
      # switch letter case
      isUppercase = !isUppercase
    }
  }
  x1 = 1
  x2 = spanX
  y1 = y1 + spanY
  y2 = y2 + spanY

}
