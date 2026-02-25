#' samp.traces
#'
#' This function allows to record the marks in an Ico class image through an interactive plot.
#' @param image.ico Ico.object: Ico class
#' @param cexp numeric: symbol size
#' @param lwdp numeric: line width
#' @param sounds logical: if FALSE sounds are silences
#' @param delay numeric: specify seconds after that all devices will be closed
#' @param old.traces matrix: If null, nothing happens. If you want to see where you traced last time without doing it all over again, add in your matrix from before and they will show as yellow ticks.
#' @return matrix: a matrix with stored coordinates (4) of the sampled marks (coordinates 1 and 2 for the lenght; coordinates 3 and 4 for the width)
#' @author Antonio Profico, Flavia Strani, Pasquale Raia, Daniel DeMiguel
#' @export

samp.traces<-function (image.ico, cexp = 0.5, lwdp = NULL, sounds = FALSE, delay=3, old.traces=NULL) 
{
  if(sounds == FALSE){
  options(locatorBell = FALSE)
  }
  if(sounds == TRUE){
  options(locatorBell = TRUE)
  }

  if(is.null(lwdp)==TRUE){
    lwdp<-cexp/10
  }
  graphics.off()
  plot_Ico(image.ico$image, xpos = 0, ypos = 0)
  rect(image.ico$work_area[[1]], image.ico$work_area[[2]], 
       image.ico$work_area[[3]], image.ico$work_area[[4]], border = "red", 
       lwd = 2)
  
  #if you include an old traces file
  if(is.null(old.traces)==FALSE){
    fs = NULL
    
    for(i in 1:length(old.traces)){ 
      fs1 <- data.frame(old.traces[i]) %>% 
        mutate(S = as.character(i))
      fs1$pt <- row.names(fs1)
      fs <- rbind(fs, fs1)
    }
    
    fs2 <- fs %>%
      filter(pt %in% c(1,2))
    
    #find total length of entries
    fs.cat <- unique(fs2$S)
    
    for(i in 1:length(fs.cat)){
      fs3 <- fs2 %>% filter(S == fs.cat[i])
      points(fs3$fix_n_x[c(1,2)], fs3$fix_n_y[c(1,2)], col = "yellow", type = "l", lwd=1)    
    }
  }
  
  
  x11(xpos = -1, ypos = 0, width = 3.5, height = 5)
  plot(NA, xlim = c(0, 20), ylim = c(0, 10), axes = F, xlab = "", 
       ylab = "")
  points(rep(1, 5)[-c(2, 4)], seq(7.5, 1, length = 5)[-c(2, 
                                                         4)], pch = c(-9658, 4, 15), col = c(1, 2, 4), cex = 3, 
         lwd = 5)
  text(rep(1, 5)[-c(2, 4)], seq(7.5, 1, length = 5)[-c(2, 4)], 
       paste(" ", c("start", "stop", "zoom (+/-)")), pos = 4, 
       cex = 1.5, col = c(1, 1, 1))
  j <- 0
  fix <- NULL
  fix <- locator(n = 1, type = "n", col = "red", cex = 8, pch = 3, 
                 bg = "red")
  sel <- which.min(abs(fix$y - seq(7.5, 1, length = 5)[-4]))
  if (sel == 1) {
    j <- j + 1
    dev.off()
    x11(xpos = -1, ypos = 0, width = 3.5, height = 5)
    plot(NA, xlim = c(0, 20), ylim = c(0, 10), axes = F, 
         xlab = "", ylab = "")
    points(rep(1, 5)[-4], seq(7.5, 1, length = 5)[-4], pch = c(17, 
                                                               19, 4, 15), col = c(3, "orange", 2, 4), cex = 3, 
           lwd = 5)
    text(rep(1, 5)[-4], seq(7.5, 1, length = 5)[-4], paste(" ", 
                                                           c("next", "cancel", "stop", "zoom (+/-)")), pos = 4, 
         cex = 1.5)
    dev.set(dev.prev())
  }
  if (sel == 4) {
    j <- j + 1
    dev.off()
    dev.set(dev.prev())
    x11(xpos = -1, ypos = 0, width = 3.5, height = 5)
    plot(NA, xlim = c(0, 20), ylim = c(0, 10), axes = F, 
         xlab = "", ylab = "")
    points(rep(1, 5)[-4], seq(7.5, 1, length = 5)[-4], pch = c(17, 
                                                               19, 4, 15), col = c(3, "orange", 2, 4), cex = 3, 
           lwd = 5)
    text(rep(1, 5)[-4], seq(7.5, 1, length = 5)[-4], paste(" ", 
                                                           c("next", "cancel", "stop", "zoom (+/-)")), pos = 4, 
         cex = 1.5)
    dev.set(dev.prev())
    zm()
  }
  j <- 1
  sel <- 1
  big_matrix <- list()
  while (sel %!in% c(3)) {
    if (sel == 2){
      graphics.off()
      big_matrix <- big_matrix[-length(big_matrix)]
      j <- j
      x11(xpos = -1, ypos = 0, width = 3.5, height = 5)
      plot(NA, xlim = c(0, 20), ylim = c(0, 10), axes = F, 
           xlab = "", ylab = "")
      points(rep(1, 5)[-4], seq(7.5, 1, length = 5)[-4], pch = c(17, 
                                                                 19, 4, 15), col = c(3, "orange", 2, 4), cex = 3, 
             lwd = 5)
      text(rep(1, 5)[-4], seq(7.5, 1, length = 5)[-4], paste(" ", 
                                                             c("next", "cancel", "stop", "zoom (+/-)")), pos = 4, 
           cex = 1.5)
      canc_plot(image.ico,big_matrix)
      dev.set(dev.prev())
      fix <- NULL
      fix <- locator(n = 1, type = "n", col = "red", cex = 8, pch = 3, 
                     bg = "red")
      sel <- which.min(abs(fix$y - seq(7.5, 1, length = 5)[-4]))
      dev.set(dev.prev())
    }
    if (sel == 4) {
      j <- j + 1
      replicate(length(dev.list()), dev.off()) 
      x11(xpos = -1, ypos = 0, width = 3.5, height = 5)
      plot(NA, xlim = c(0, 20), ylim = c(0, 10), axes = F, 
           xlab = "", ylab = "")
      points(rep(1, 5)[-4], seq(7.5, 1, length = 5)[-4], pch = c(17, 
                                                                 19, 4, 15), col = c(3, "orange", 2, 4), cex = 3, 
             lwd = 5)
      text(rep(1, 5)[-4], seq(7.5, 1, length = 5)[-4], paste(" ", 
                                                             c("next", "cancel", "stop", "zoom (+/-)")), pos = 4, 
           cex = 1.5)
      canc_plot(image.ico,NULL)
      zm()
      for (i in 1:length(big_matrix)) {
        points(big_matrix[[i]][c(1, 2), 1], big_matrix[[i]][c(1, 
                                                              2), 2], pch = 4, col = 2, cex = 0.5, lwd = 0.5/10)
        points(big_matrix[[i]][c(1, 2), 1], big_matrix[[i]][c(1, 
                                                              2), 2], col = 2, lwd = 1, type = "l")
        points(big_matrix[[i]][c(3, 4), 1], big_matrix[[i]][c(3, 
                                                              4), 2], pch = 4, col = 2, cex = 0.5, lwd = 0.5/10)
        points(big_matrix[[i]][c(3, 4), 1], big_matrix[[i]][c(3, 
                                                              4), 2], col = 2, lwd = 1, type = "l")
      }
      dev.set(dev.prev())
      fix <- NULL
      fix <- locator(n = 1, type = "n", col = "red", cex = 8, pch = 3, 
                     bg = "red")
      sel <- which.min(abs(fix$y - seq(7.5, 1, length = 5)[-4]))
      dev.set(dev.prev())
      
      if(sel==3){
        replicate(length(dev.list()), dev.off()) 
      break}
      if (sel == 2){
        graphics.off()
        big_matrix <- big_matrix[-length(big_matrix)]
        j <- j
        x11(xpos = -1, ypos = 0, width = 3.5, height = 5)
        plot(NA, xlim = c(0, 20), ylim = c(0, 10), axes = F, 
             xlab = "", ylab = "")
        points(rep(1, 5)[-4], seq(7.5, 1, length = 5)[-4], pch = c(17, 
                                                                   19, 4, 15), col = c(3, "orange", 2, 4), cex = 3, 
               lwd = 5)
        text(rep(1, 5)[-4], seq(7.5, 1, length = 5)[-4], paste(" ", 
                                                               c("next", "cancel", "stop", "zoom (+/-)")), pos = 4, 
             cex = 1.5)
        canc_plot(image.ico,big_matrix)
        dev.set(dev.prev())
        fix <- NULL
        fix <- locator(n = 1, type = "n", col = "red", cex = 8, pch = 3, 
                       bg = "red")
        sel <- which.min(abs(fix$y - seq(7.5, 1, length = 5)[-4]))
        dev.set(dev.prev())
      }
      
      
    }
    fix_n_x <- NULL
    fix_n_y <- NULL
    for (i in 1:4) {
      fix <- NULL
      fix <- locator(n = 1, type = "n", col = "red", cex = 0.1, 
                     pch = 4, bg = "red")
      points(fix$x, fix$y, pch = 4, col = 2, cex = cexp, 
             lwd = lwdp)
      fix_n_x <- c(fix_n_x, fix$x)
      fix_n_y <- c(fix_n_y, fix$y)
      if (i == 2) {
        points(fix_n_x[c(1, 2)], fix_n_y[c(1, 2)], col = 2, 
               lwd = lwdp, type = "l")
      }
      if (i == 4) {
        points(fix_n_x[c(3, 4)], fix_n_y[c(3, 4)], col = 2, 
               lwd = lwdp, type = "l")
      }
      coo_i <- cbind(fix_n_x, fix_n_y)
    }
    big_matrix[[j]] <- coo_i
    dev.set(dev.prev())
    fix <- NULL
    fix <- locator(n = 1, type = "n", col = "red", cex = 8, 
                   pch = 3, bg = "red")
    sel <- which.min(abs(fix$y - seq(7.5, 1, length = 5)[-4]))
    if (sel == 1) {
      j <- j + 1
      dev.set(dev.prev())
    }
  }
  
  if(sel==3){
    Sys.sleep(delay)
replicate(length(dev.list()), dev.off()) 
    }
 
  if(sounds == FALSE){
  options(locatorBell = TRUE)
  }
  
   return(big_matrix)
}


