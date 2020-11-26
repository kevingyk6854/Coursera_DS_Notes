# Color Palette Unities in R

## colorRamp: take a palette of colors and return a function that takes values between
## 0 and 1, indicating the extremes of the color palette (see the 'gray' function)
pal <- colorRamp(c("red", "blue"))
pal(0) # red green blue
pal(1)
pal(0.5) # 50% red + 50% blue
pal(seq(0, 1, len = 10))

## colorRampPalette: take a palette of colors and retuan a function that take integer
## arguments and returns a vector of colors interpolating the palette (like heat.colors or topo.colors)
pal <- colorRampPalette(c("red", "yellow"))
pal(2) # "#FF0000" "#FFFF00"

# result: representaiton of the colors in hexadecimal here changing as you go from red to yellow
pal(10) # [1] "#FF0000" "#FF1C00" "#FF3800" "#FF5500" "#FF7100" "#FF8D00"
        # [7] "#FFAA00" "#FFC600" "#FFE200" "#FFFF00"

# =================================================================

# smoothscatter function -> higher density with darker blue
x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x, y)

# =================================================================

library(RColorBrewer)
# brewer.pal(n = number of colors, name = a palette name from a list)
cols <- brewer.pal(3, "BuGn") 

# colorRampPalette() returns functions that interpolate a set of given colors to create new color palettes 
pal <- colorRampPalette(cols)
pal(20)

image(volcano, col = pal(20))

## Notes:
## 1. the "rgb" function can be used to produce any color via red, green, blue proportions
## 2. color transparency can be added via the "alpha" parameter to rgb(r, g, b, alpha)
## 3. the "colorspace" package can be used for a different control over colors




