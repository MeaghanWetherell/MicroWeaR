#' class.Ico
#'
#' Convert an image in an Ico class object. Formats currently supported: .jpeg, .png and .tiff. Limited to greyscale images.
#' @param path character: path of an image file
#' @return image matrix: class Ico image of the loaded file
#' @author Antonio Profico, Flavia Strani, Pasquale Raia, Daniel DeMiguel
#' @export

class.Ico<-function(path){
  image.type = gsub(".*\\.", "", path) #this way you don't have to enter it
  
  if(image.type=="jpg"){
    image<-jpeg::readJPEG(path,native=F)
    if(is.na(dim(image)[3])==FALSE){
      if(dim(image)[3]==3){
      
        image<-image[,,1]
        image <- t(image)
        image <- image[nrow(image):1, ]
        #print("rgb") #turn on for testing.
      }} else {
        #removed the apply code here because on non-square images, it caused distortion.
        #Also, added an else code here.
        
        image <- image[nrow(image):1, ]
        image <- image[, ncol(image):1]   
       # print("Not rgb")
      }

    
  }
  
  if(image.type=="png"){
    image<-png::readPNG(path,native=F)
    
    if(is.na(dim(image)[3])==FALSE){
      if(dim(image)[3]==3){
        image<-image[,,1]
        image <- image[nrow(image):1,  ]
        image <- image[, ncol(image):1]
        print("rgb")
      } else {
        image <- image[nrow(image):1, , ]
        #image <- image[, ncol(image):1]
        print("not rgb")
      } }
    
   
  }
  
  if(image.type %in% c("tiff", "tif")){
    image<-tiff::readTIFF(path,native=F)
    if(is.na(dim(image)[3])==FALSE){
      if(dim(image)[3]==3){
        image<-image[,,1]
        image <- image[nrow(image):1, ]
        image <- image[, ncol(image):1]
      }} else {
        # image <- t(image)
        image <- image[nrow(image):1, ]
        image <- image[, ncol(image):1]
      }
   

    
  }
  
  res<-dim(image)[1:2]
  output<-list("image"=image,"res"=res,"unit"="pixel","area"=min(res)/2,"zoom"=res/2,
               "xlim"=c(0,res[2]),
               "ylim"=c(0,res[1]),
              #"xlim"=c(0,res[1]),
              #"ylim"=c(0,res[2]),
              "scale_factor"=1,
               "name" = path)
  
  class(output) <- "Ico"
  return(output)
}
