#' output.Ico
#'
#' Print a summary statistics table reporting the number of pits and scratches (and the size of any sub-category).
#' @param big_matrix matrix: a matrix with stored coordinates (4) of the sampled marks (coordinates 1 and 2 for the lenght; coordinates 3 and 4 for the width)
#' @param Type character: a character vector
#' @param cross_paral matrix: output of the cross.parallel function
#' @param w.area output of Warea.Ico
#' @return output_1 numeric: matrix with sampled marks count and classification
#'
#' * N.pits is the total number of pits
#' * N.sp is the number of small pits
#' * N,lp is the number of large pits
#' * N.scratches is the total number of scratches
#' * N.fs is the number of fine scratches
#' * N.cs is the number of coarse scratches
#' * N.Ps is the parallel scratches
#' * N.Xs is the number of cross-parallel scratches
#' * %Ps is the percentages of scratches that are parallel
#' * %Xs is the percentage of scratches that are cross-parallel
#' * FeatureDensity is the total number of features per mm2
#' * Pmm2 is the number of pits per mm2
#' * Smm2 is the number of scratches per mm2
#' * PitPercent is the percentage of pits
#' * ScrachPercent is the percentage of scratches
#' @author Antonio Profico, Flavia Strani, Pasquale Raia, Daniel DeMiguel, Meaghan Wetherell
#' @examples
#' \dontrun{
#' # A. brevirostris case-study
#' #load sampled scars
#' data("A_br_sam")
#' #load working area
#' data("A_br_war")
#' class<-autom_class(A_br_sam,A_br_war$image)
#' criss_cross<-cross.parallel(A_br_sam,A_br_war$image,class$Type)
#' output.Ico(A_br_sam,class$Type,criss_cross,A_br_war)
#' # C. elaphus eostephanoceros case-study
#' #load sampled scars
#' data("C_el_sam")
#' #load working area
#' data("C_el_war")
#' class<-autom_class(C_el_sam,C_el_war$image)
#' criss_cross<-cross.parallel(C_el_sam,C_el_war$image,class$Type)
#' output.Ico(C_el_sam,class$Type,criss_cross,C_el_war)
#' }
#' @export


output.Ico<-function(big_matrix,Type,cross_paral,w.area){

  #Finds area in mm2
  #sizevar is the reconstructed box sixe (e.g., 200 * 200 um)
  sizevar <-  w.area$work_area[[3]]-w.area$work_area[[1]]

  #squared surface area in mm
  surf = (sizevar^2)/ 10^6 #10^6 is squared mm conversion.

  #create temporary holding items for data
  vett_1_temp<-NULL
  vett_2_temp<-NULL
  for(i in 1:length(big_matrix)){
    vett_1_temp[i]<-sqrt(sum(big_matrix[[i]][1,]-big_matrix[[i]][2,])^2)
    vett_2_temp[i]<-sqrt(sum(big_matrix[[i]][3,]-big_matrix[[i]][4,])^2)}

  vett_1<-vett_1_temp
  vett_2<-vett_2_temp
  vett_1[which(vett_2_temp>vett_1_temp)]=vett_2_temp[which(vett_2_temp>vett_1_temp)]
  vett_2[which(vett_2_temp>vett_1_temp)]=vett_1_temp[which(vett_2_temp>vett_1_temp)]

  #creates a matrix to hold data
  output_1<-matrix(0,nrow=5,ncol=15)
  colnames(output_1)<-c("N.pits","N.sp","N.lp","N.scratches","N.fs","N.cs","N.Ps","N.Xs","%Ps","%Xs", "FeatureDensity", "Pmm2", "Smm2", "PitPercent", "ScratchPercent")
  rownames(output_1)<-c("Count","Mean_length","Sd_length","Mean_width","Sd_width")

  #Calculating Small Pit variables
  output_1[1,2]<-length(which(Type=="Sm.Pit"))
  output_1[2,2]<-mean(vett_1[(which(Type=="Sm.Pit"))])
  output_1[3,2]<-sd(vett_1[(which(Type=="Sm.Pit"))])
  output_1[4,2]<-mean(vett_2[(which(Type=="Sm.Pit"))])
  output_1[5,2]<-sd(vett_2[(which(Type=="Sm.Pit"))])

  #Calculate large pit variables
  output_1[1,3]<-length(which(Type=="Lg.Pit"))
  output_1[2,3]<-mean(vett_1[(which(Type=="Lg.Pit"))])
  output_1[3,3]<-sd(vett_1[(which(Type=="Lg.Pit"))])
  output_1[4,3]<-mean(vett_2[(which(Type=="Lg.Pit"))])
  output_1[5,3]<-sd(vett_2[(which(Type=="Lg.Pit"))])

  #calculate total number of pits
  output_1[1,1]<-output_1[1,2]+output_1[1,3]
  output_1[2,1]<-mean(vett_1[which(Type%in% c("Lg.Pit","Sm.Pit"))])
  output_1[3,1]<-sd(vett_1[which(Type%in% c("Lg.Pit","Sm.Pit"))])
  output_1[4,1]<-mean(vett_2[which(Type%in% c("Lg.Pit","Sm.Pit"))])
  output_1[5,1]<-sd(vett_2[which(Type%in% c("Lg.Pit","Sm.Pit"))])

  # Calculate the fine scratch data
  output_1[1,5]<-length(which(Type=="Fi.Scr"))
  output_1[2,5]<-mean(vett_1[(which(Type=="Fi.Scr"))])
  output_1[3,5]<-sd(vett_1[(which(Type=="Fi.Scr"))])
  output_1[4,5]<-mean(vett_2[(which(Type=="Fi.Scr"))])
  output_1[5,5]<-sd(vett_2[(which(Type=="Fi.Scr"))])

  # calculate coarse scratches
  output_1[1,6]<-length(which(Type=="Co.Scr"))
  output_1[2,6]<-mean(vett_1[(which(Type=="Co.Scr"))])
  output_1[3,6]<-sd(vett_1[(which(Type=="Co.Scr"))])
  output_1[4,6]<-mean(vett_2[(which(Type=="Co.Scr"))])
  output_1[5,6]<-sd(vett_2[(which(Type=="Co.Scr"))])

  # calculate total number of scratches
  output_1[1,4]<-output_1[1,5]+output_1[1,6]
  output_1[2,4]<-mean(vett_1[which(Type%in% c("Fi.Scr","Co.Scr"))])
  output_1[3,4]<-sd(vett_1[which(Type%in% c("Fi.Scr","Co.Scr"))])
  output_1[4,4]<-mean(vett_2[which(Type%in% c("Fi.Scr","Co.Scr"))])
  output_1[5,4]<-sd(vett_2[which(Type%in% c("Fi.Scr","Co.Scr"))])

  #Calculate number of pits per mm2, and number of scratches per mm2
  output_1[1,11] <- (output_1[1,1] + output_1[1,4])/surf #overall density
  output_1[1,12] <- output_1[1,1]/surf #Pit density
  output_1[1,13] <- output_1[1,4]/surf #scratch density

  #Calculate percentage of pits and scratches
  output_1[1,14] <- round(output_1[1,1]/(output_1[1,1]+output_1[1,4])*100,2) #pits
  output_1[1,15] <- round(output_1[1,4]/(output_1[1,1]+output_1[1,4])*100,2) #scratches

  #cross parallel
  if(is.null(cross_paral)==FALSE){
    output_1[1,7]<-length(which(cross_paral[,4]=="YES"))
    output_1[1,8]<-length(which(cross_paral[,3]=="YES"))###
  }

  if(is.null(cross_paral)==FALSE){
    output_1[1,9]<-round(length(unique(as.numeric(cross_paral[(which(cross_paral[,4]=="YES")),c(1,2)])))/
                           length(which(Type %in% c("Fi.Scr","Co.Scr"))),3)*100
    output_1[1,10]<-round(length(unique(as.numeric(cross_paral[(which(cross_paral[,3]=="YES")),c(1,2)])))/
                            length(which(Type %in% c("Fi.Scr","Co.Scr"))),3)*100

  }


  print(paste("number of","pits:",output_1[1,1]))
  print(paste("number of","small pits:",output_1[1,2]))
  print(paste("number of","large pits:",output_1[1,3]))
  print(paste("number of","scratches:",output_1[1,4]))
  print(paste("number of","fine scratches:",output_1[1,5]))
  print(paste("number of","coarse scratches:",output_1[1,6]))
  print(paste("percentage of pits =",output_1[1,14]))
  print(paste("percentage of scratches =",output_1[1,15]))
  print(paste("number of pits/mm2",output_1[1,12]))
  print(paste("number of scratches/mm2",output_1[1,13]))

  return(round(output_1,2))
}
