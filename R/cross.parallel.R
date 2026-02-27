#' cross.parallel
#'
#' Detect pairs of scratches which are parallel and/or that cross each other.
#' @param big_matrix matrix: a matrix with stored coordinates (4) of the sampled marks (coordinates 1 and 2 for the length; coordinates 3 and 4 for the width)
#' @param Type character: scars type
#' @param working.area.ico Ico.object: Ico class working image, use $ to specify the image.
#' @return numeric: matrix with number of pairs of parallel and/or scratches that cross each other
#' @author Antonio Profico, Flavia Strani, Pasquale Raia, Daniel DeMiguel
#' @examples
#' \dontrun{
#' # A. brevirostris case-study
#' #load sampled scars
#' data("A_br_sam")
#' #load working area
#' data("A_br_war")
#' class<-autom_class(A_br_sam,A_br_war$image)
#' criss_cross=cross.parallel(A_br_sam,A_br_war$image,class$Type)
#' criss_cross
#' # C. elaphus eostephanoceros case-study
#' #load sampled scars
#' data("C_el_sam")
#' #load working area
#' data("C_el_war")
#' class<-autom_class(C_el_sam,C_el_war$image)
#' criss_cross=cross.parallel(C_el_sam,C_el_war$image,class$Type)
#' criss_cross
#' }
#' @export

cross.parallel<-function(big_matrix,working.area.ico,Type){

  slopes<-NULL

  #Pull out a list of all types of features classified as scratches
  Scratch_list<-big_matrix[which(substr(Type,4,6)=="Scr")]


  #what happens if the scratch list is not greater than 1?
  if(length(Scratch_list)>1){
    combinazioni<-combn(length(Scratch_list), 2)
    cross_paral<-cbind(t(combinazioni),NA,NA)
    colnames(cross_paral)<-c("scratch_1","scratch_2","cross","paral")

    for(i in 1:ncol(combinazioni)){


      #Calculate slope in first line of the combination
      A<-Scratch_list[[combinazioni[,i][1]]][1,]
      B<-Scratch_list[[combinazioni[,i][1]]][2,] #used to be 4, should be 2

      ################### Changed
      rise <- A[2]-B[2] #y-y
      run <- A[1]-B[1] #x - x
      slope <- ifelse(rise == 0, 0, #horizontal line
               ifelse(run == 0, 1, #vertical line
                      rise/run)) #other slope
      ###################
      slopes<-c(slopes,slope)
      x<--slope
      b<-A[2]-A[1]*slope


      # calculate slope of second line in combination
      C<-Scratch_list[[combinazioni[,i][2]]][1,]
      D<-Scratch_list[[combinazioni[,i][2]]][2,] #used to be 4, should be 2

      ################### Changed
      rise2 <- C[2]-D[2] #y-y
      run2 <- C[1]-D[1] #x - x
      slope2 <- ifelse(rise2 == 0, 0, #horizontal line
                      ifelse(run2 == 0, 1, #vertical line
                             rise2/run2)) #other slope
      ###################
      slopes<-c(slopes,slope2)
      x2<--slope2 #take absolute?
      b2<-C[2]-C[1]*slope2

      #if the two slopes are almost identical, the coefficient matrix can't be solved.
      x2 <- ifelse(abs(x2 - x) < 0.01, x2*.99, x2)

      coeffMatr<-matrix(c(x,1,x2,1),nrow=2,ncol=2,byrow=TRUE)
      RhsMatr<-matrix(c(b,b2),nrow=2,ncol=1,byrow=TRUE)
      Inverse<-solve(coeffMatr)
      Result<-Inverse %*% RhsMatr
      Result_x<-Result[1]
      Result_y<-Result[2]

      cross<-NULL
      paral<-NULL

      #find the maximum ranges of the two lines in question
      range_x_1<-range(Scratch_list[[combinazioni[,i][1]]][,1])
      range_y_1<-range(Scratch_list[[combinazioni[,i][1]]][,2])
      range_x_2<-range(Scratch_list[[combinazioni[,i][2]]][,1])
      range_y_2<-range(Scratch_list[[combinazioni[,i][2]]][,2])

      if(((Result_x>min(range_x_1)&Result_x<max(range_x_1))&
          (Result_x>min(range_x_2)&Result_x<max(range_x_2))&
          (Result_y>min(range_y_1)&Result_y<max(range_y_1))&
          (Result_y>min(range_y_1)&Result_y<max(range_y_1)))==TRUE){
        cross<-"YES"
      }

      if(((Result_x>min(range_x_1)&Result_x<max(range_x_1))&
          (Result_x>min(range_x_2)&Result_x<max(range_x_2))&
          (Result_y>min(range_y_1)&Result_y<max(range_y_1))&
          (Result_y>min(range_y_1)&Result_y<max(range_y_1)))==FALSE){
        cross<-"NO"
      }

      #also added RANN:: here
      pos_A<-RANN::nn2(Scratch_list[[combinazioni[,i][1]]],t(as.matrix(c(Result_x,Result_y),ncol=2)))$nn.idx[1]
      pos_B<-RANN::nn2(Scratch_list[[combinazioni[,i][2]]],t(as.matrix(c(Result_x,Result_y),ncol=2)))$nn.idx[1]

      dist_A<-sqrt(sum((Scratch_list[[combinazioni[,i][1]]][pos_A,]-c(Result_x,Result_y))^2))
      dist_B<-sqrt(sum((Scratch_list[[combinazioni[,i][2]]][pos_B,]-c(Result_x,Result_y))^2))

      #this is the section I fixed to deal with an initial issue where neither critera was met
      if (any(pmin(dist_A, dist_B) < (working.area.ico$area*2)*2)) {
        paral <- "NO"
      } else {
        paral <- "YES"
      }

      cross_paral[i,3]<-cross
      cross_paral[i,4]<-paral
    }
  }


  slopes_matrix<-matrix(slopes,ncol=2,byrow = T)
  m2<-slopes_matrix[,2]
  m1<-slopes_matrix[,1]
  angle<-round((atan(abs((m2-m1)/(1+m1*m2))))*180/pi,2)
  cross_paral<-cbind(cross_paral,angle) #if it is crossed, if it is parallel, and the angle.
  return(cross_paral)
}
