#' canc_plot
#'
#' Plot the scars stored in the object big_matrix associated to an image of class Ico. Now updated to use color coding to indicate classifications.
#' @param image.ico Ico.object: Ico class image
#' @param big_matrix matrix: a matrix with stored coordinates (4) of the sampled marks (coordinates 1 and 2 for the length; coordinates 3 and 4 for the width)
#' @param Type character: scars type
#' @author Antonio Profico, Flavia Strani, Pasquale Raia, Daniel DeMiguel
#' @export
#' 
canc_plot<-function (working.area, big_matrix, Type) {
  Type2 <-data.frame(Type)
  Type2$S <- row.names(Type2)
  
  fs = NULL
  
  for(i in 1:length(big_matrix)){ 
    fs1 <- data.frame(big_matrix[i]) %>% 
      mutate(S = as.character(i))
    fs1$pt <- row.names(fs1)
    fs <- rbind(fs, fs1)
    }
  
  fs2 <- fs %>%
    left_join(Type2) %>% 
    filter(pt %in% c(1,2))

  #find total length of entries
  fs.cat <- unique(fs2$S)
  
  plot_Ico(working.area$image, xpos = 0, ypos = 0) #plots the SEM image
 
  #plots the working area
  rect(working.area$work_area[[1]], working.area$work_area[[2]], 
       working.area$work_area[[3]], working.area$work_area[[4]], border = "red", 
       lwd = 2)
  
  #colors the different types so you can check to see if it's categorizing correctly
  colvar <- function(){
    ifelse(fs3$Type[1] == "Co.Scr", "blue", 
    ifelse(fs3$Type[1] == "Fi.Scr", "dodgerblue",
    ifelse(fs3$Type[1] == "Lg.Pit", "firebrick", "orange")))
  }
  
  for(i in 1:length(fs.cat)){
    fs3 <- fs2 %>% filter(S == fs.cat[i])
    #points(fs3$fix_n_x[c(1,2)], fs3$fix_n_y[c(1,2)], col = colvar(), pch = 16)
    points(fs3$fix_n_x[c(1,2)], fs3$fix_n_y[c(1,2)], col = colvar(),type = "l", lwd=2)    
  }
  
  print("Coarse Scratches are blue, fine scratches are lighter blue.")
  print("Large pits are red, small pits are orange.")
 
}
