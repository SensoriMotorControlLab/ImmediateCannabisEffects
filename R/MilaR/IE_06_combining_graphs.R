library(gridExtra)
library(png)

# Read in the png files
img1 <- readPNG("C:/Users/kamilako/OneDrive - York University/cannabis project (Mila)/img/IE_gonogo_new.png")
img2 <- readPNG("C:/Users/kamilako/OneDrive - York University/cannabis project (Mila)/img/IE_visualsearch_propcorrect_new.png")
img3 <- readPNG("C:/Users/kamilako/OneDrive - York University/cannabis project (Mila)/img/IE_taskswitching_new.png")
img4 <- readPNG("C:/Users/kamilako/OneDrive - York University/cannabis project (Mila)/img/IE_trailmaking_new.png")
img5 <- readPNG("C:/Users/kamilako/OneDrive - York University/cannabis project (Mila)/img/IE_nback_new.png")
img6 <- readPNG("C:/Users/kamilako/OneDrive - York University/cannabis project (Mila)/img/IE_tunneling_new.png")


# Combine the images into one plot
grid.arrange(rasterGrob(img1), rasterGrob(img5), 
             rasterGrob(img3), rasterGrob(img4),
             rasterGrob(img2), rasterGrob(img6), ncol = 2)

