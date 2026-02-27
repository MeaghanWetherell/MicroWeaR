#' scale_Ico
#'
#' Scale an Ico class image with an interactive plot. Allows to select two points on the metric reference and to define its length.
#' @param image.ico Ico.object: class Ico image
#' @param xpos numeric: window x coordinates
#' @param ypos numeric: window y coordinates
#' @param types symbol: symbol type
#' @param cols character: symbol color
#' @param cexs vector: symbol size
#' @param pchs numeric: symbol to use
#' @param sounds logical: if FALSE sounds are silences
#' @param delay numeric: specify seconds after that all devices will be closed
#' @param size.var numeric: If you already know the scale bar size you can enter it here instead of typing it in afterwards.
#' @return Ico.object: class Ico scaled image (based on the provided metric reference)
#' @author Antonio Profico, Flavia Strani, Pasquale Raia, Daniel DeMiguel
#' @export


scale_Ico<-function (image.ico, xpos = 0, ypos = 0, types = "n", cols = "red",
          cexs = NULL, pchs = 3, sounds = FALSE,delay=3, size.var)
{

  if(sounds == FALSE){
  options(locatorBell = FALSE)
  }
  if(sounds == TRUE){
  options(locatorBell = TRUE)
  }

  if (is.null(cexs) == TRUE) {
    cexs <- 1
  }
  plot_Ico(image.ico, xpos=xpos,ypos=ypos)
  print("Select two points for the scale factor")
  scale_p <- NULL
  scale_p_t <- locator(n = 1, type = "n", col = cols, cex = cexs,
                       pch = 3, bg = "red")
  points(scale_p_t$x, scale_p_t$y, cex = cexs, pch = 3, lwd = 2.5,
         col = "red")
  scale_p_t2 <- locator(n = 1, type = "n", col = cols, cex = cexs,
                        pch = 3, bg = "red")
  points(scale_p_t2$x, scale_p_t2$y, cex = cexs, pch = 3, lwd = 2.5,
         col = "red")
  scale_p <- list(x = c(scale_p_t[[1]], scale_p_t2[[1]]), y = c(scale_p_t[[2]],
                                                                scale_p_t2[[2]]))
  if(is.null(size.var)){

    print("Insert length scale in micron")
    length_scale <- as.numeric(readLines(n = 1))
  } else {
    length_scale <- size.var
  }



  dist_scale_p <- abs(scale_p$x[2] - scale_p$x[1])
  dist_scale_p <- length_scale/dist_scale_p
  image.ico$res <- image.ico$res * dist_scale_p
  image.ico$unit <- "micron"
  image.ico$area <- image.ico$area * dist_scale_p #fix from Ondrej-klima
  image.ico$zoom <- max(image.ico$res) * dist_scale_p
  image.ico$xlim <- image.ico$xlim * dist_scale_p
  image.ico$ylim <- image.ico$ylim * dist_scale_p
  image.ico$scale_factor <- dist_scale_p
  Sys.sleep(delay)
  howmanydevices<-length(dev.list())
  replicate(howmanydevices,dev.off())
  return(image.ico)

    if(sounds == FALSE){
  options(locatorBell = TRUE)
  }

}
