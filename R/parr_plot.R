
#' parr_plot
#'
#' visualize pairs of scratches which are parallel and/or that cross each other.
#' @param scr matrix: list of scratches returned from cross-parallel
#' @param image.ico Ico.object: Ico class
#' @param big_matrix matrix: a matrix with stored coordinates (4) of the sampled marks (coordinates 1 and 2 for the length; coordinates 3 and 4 for the width)
#' @author Meaghan Wetherell
#' @examples
#' \dontrun{
#' 
#' }
#' @export
#a plot to see if something is parallel or not according to output
parr_plot<-function (image.ico, big_matrix, scr) {
  scr2 <-data.frame(scr) 
  cross.t <- scr2 %>% filter(cross == "YES")
  cross.t2 <- unique(c(cross.t$scratch_1, cross.t$scratch_2))
  
  paral.t <- scr2 %>% filter(paral == "YES")
  paral.t2 <- unique(c(paral.t$scratch_1, paral.t$scratch_2))
  
  fs = NULL
  
  for(i in 1:length(big_matrix)){ 
    fs1 <- data.frame(big_matrix[i]) %>% 
      mutate(S = as.character(i))
    fs1$pt <- row.names(fs1)
    fs <- rbind(fs, fs1)
    }
  
  fs2 <- fs %>%
    filter(pt %in% c(1,2))
  

  #find total length of entries
  fs.cat <- unique(fs2$S)

  #colors the different types so you can check to see if it's categorizing correctly
  #those that cross another one are green. Those that are parallel but not crossed are blue, and dashed.  
  crosscol <- function(){ifelse( fs.cat[i] %in% cross.t2, "green", 
                                ifelse( fs.cat[i] %in% paral.t2, "blue", "red"))}
  paralshp <- function(){ifelse( fs.cat[i] %in% paral.t2, 2, 1)}
  
  plot_Ico(image.ico$image, xpos = 0, ypos = 0) #plots the SEM image
 
  #plots the working area
  rect(image.ico$work_area[[1]], image.ico$work_area[[2]], 
       image.ico$work_area[[3]], image.ico$work_area[[4]], border = "red", 
       lwd = 2)
  

  # colors and labels line pairs.
  for(i in 1:length(fs.cat)){
    fs3 <- fs2 %>% filter(S == fs.cat[i])
    labspot <- fs3 %>% filter(fix_n_x == min(fs3$fix_n_x))
    col.var <- crosscol()
    lty.var <- paralshp()
    points(fs3$fix_n_x[c(1,2)], fs3$fix_n_y[c(1,2)], col = col.var , lty = lty.var, type = "l", lwd=2)
    points(x = labspot$fix_n_x[c(1,2)], y = labspot$fix_n_y[c(1,2)], col = "white", pch = 16, cex = 1.5)
    text(x = labspot$fix_n_x[c(1,2)], y = labspot$fix_n_y[c(1,2)], col = crosscol(), labels = labspot$S, cex = .5)
  }
  
  print(paste("parallel lines:", paste(paral.t2, collapse = " "), " and crossing lines:", paste(cross.t2, collapse = " "), sep = " "))
  print("Green lines cross another, blue and red ones do not.")
  print("Dashed lines are considered parallel to something else, solid ones are not.")
}
