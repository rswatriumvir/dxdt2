library(dplyr)
library(tidyverse)
library(sticky) #can preserve attributes
library(ade4)

create.dXdTdist <- function(dm # a pairwise distance matrix of features between each observation
){
  dm.pca = dudi.pco(dm, scann = FALSE, nf = 3)$li
  return(dm.pca)
}


create.dXdT <- function(case.data, # data frame with all observations
                        time.var, # variable name or index in case.data for time observation
                        timepoint.id = NULL, # variable name or index in case.data for time point identifiers
                        case.id = NULL, # variable name or index in case.data for cases
                        features, # a data frame or matrix with all the features
                        scheme # scheme for calculating dT
){
  if(! all(rownames(case.data)==rownames(features))){
    stop("Rownames do not match.")
  }

  if(is.null(case.id))
    case.id=rep("Series", nrows(case.data))
  else
    case.id = case.data[,case.id]

  if(!is.null(timepoint.id))
    timepoint.id = case.data[,timepoint.id]

  time_var = case.data[,time.var]

  m = cbind(case.id, timepoint.id, time_var, features)
  dXdT = process_matrix(m, scheme) ## make sure that the output of process_matrix is a data frame
  ## can we attach the original case.data and features and scheme as attributes(??) to the output dXdT data frame
  attributes(dXdT) = list(
    case.data = case.data,
    features = features
  )
}



 # This is the object created with process_matrix
# Column names: Case, Start.tp, End.tp, dT, dX

fitCase <- function(dXdTobj){
  glm(formula = dX^2/dT ~ dT,
      family = gaussian(link = "identity"),
      data = dXdTobj)
}

# subset_case = function(dXdTobj, ...){
#  subset()
# }

fitEachCaseMD3 <- function(dXdTobj){
  sapply(unique(dXdTobj[,"Case"]),
         function(caseID) fitCase(subset(dXdTobj,
                                         Case==caseID)))
}


process_data = function(data,time_scheme) {
  sticky(data) #preserve original data as attribute in dxdt object
  #mat = matrix(, nrow = nrow(data)-1, ncol = 5)
  mat = matrix(nrow = nrow(data)-1, ncol = 5)
  mat = as.data.frame(mat)
  if (time_scheme == "interpoint") {  #format r,c
    for (x in 1:nrow(data)) {
      if (x==nrow(data))
      {break}
      if ((data[x,1] == data[x+1,1])) #making sure subject is same
      {
        mat[x,4] = as.numeric(data[x+1,2])-as.numeric(data[x,2])
        euc_dist = 0
        for (y in 4:ncol(data)) {
          euc_dist =+ (data[x+1,y]-data[x,y])^2
       }
        mat[x,5] = euc_dist/mat[x,4] #dividing by time
        mat[x,2] = as.character(data[x,3])
        mat[x,3] = as.character(data[x+1,3])
        mat[x,1] = as.character(data[x,1])
      }
    else { #skip to next iteration if subject isnt same
      next
    }
    }}

  if (time_scheme == "baseline") {
    print("baseline")
    subject_count = 1
      for (x in 1:nrow(data)) {
        print(x)
        if (x==nrow(data))
        {break}
        else if(data[x,1]==data[x+1,1]) #making sure subject is same
        {
          mat[x,4] = data[x+1,2]-data[subject_count,2]
          euc_dist = 0
          for (y in 4:ncol(data)) {
            euc_dist =+ (data[x,y]-data[subject_count,y])^2
          }
          mat[x,5] = euc_dist/mat[x,4]
          mat[x,2] = data[subject_count,3]
          mat[x,3] = data[x,3]
          mat[x,1] = as.character(data[x,1])
        }
      else {
        subject_count=x+1
        next
      }
      }
  }



  ##removing NA
  for(z in 1:nrow(mat))
  {
    if (is.na(mat[z,1]) & is.na(mat[z,2]) & is.na(mat[z,3]) & is.na(mat[z,4]) & is.na(mat[z,5])) #all columns are NA for that row instance
    {
      mat = mat[-z,] #remove row if all columns are NA

    }
  }
  return(mat)
}


'create.dXdTdist <- function(case.data, # data frame with all observations
                            time.var, # variable name or index in case.data for time observation
                            timepoint.id = NULL, # variable name or index in case.data for time point identifiers
                            case.id = NULL, # variable name or index in case.data for cases
                            dm, # a pairwise distance matrix of features between each observation
                            scheme # scheme for calculating dT

)'

#next steps



