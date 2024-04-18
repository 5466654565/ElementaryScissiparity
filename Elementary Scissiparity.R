# Parameters
n_laps <- 25
n_minGroups <- 4
elementaryID <- list()
groups <- list()
scissiparityCount <- 0

add_new_elementary <- function() {
  newID <- length(elementaryID) + 1 
  newelementary <- sample(setdiff(-1:1, 0), 1, replace = TRUE)
  elementaryID <<- append(elementaryID, newelementary)
  return(newID)
}

scissiparity <- function(groupIndex) {
  group <- groups[[groupIndex]]
  total_IDs <- length(group)
  value <- unlist(elementaryID[group])

 if (total_IDs >= 4 && total_IDs %% 2 == 0 && sum(value) == 0) {
    negativeList <- numeric()
    positiveList <- numeric() 
    negativeValues <- sort(value[value < 0], decreasing = TRUE)
    positiveValues <- sort(value[value > 0])

    if (identical(abs(negativeValues), positiveValues)) {
      
      negativeID <- which(value %in% negativeValues)
      positiveID <- which(value %in% positiveValues)
      
      orderedNegativeID <- negativeID[order(negativeValues)]
      orderedPositiveID <- positiveID[order(positiveValues)]
      
      negativeList <- c(negativeList, orderedNegativeID)
      positiveList <- c(positiveList, orderedPositiveID)
      
      newGroup1 <- c(positiveList[seq(1, length(positiveList), 2)], negativeList[seq(2, length(negativeList), 2)])
      newGroup2 <- c(negativeList[seq(1, length(negativeList), 2)], positiveList[seq(2, length(positiveList), 2)])
      
      groups <- groups[-groupIndex]
      scissiparityCount <<- scissiparityCount + 1
      groups <- append(groups, list(newGroup1))
      groups <- append(groups, list(newGroup2))
    }
  }
}

# Interaction between two elementary numbers
for (i in 1:n_laps) {
  randomProbability <- sample(1:4, 1, replace = TRUE)
  
  if (length(groups) < n_minGroups || randomProbability == 1) {
    elementaryID1 <- add_new_elementary()
    elementaryID2 <- add_new_elementary()
    while (elementaryID[[elementaryID1]] * elementaryID[[elementaryID2]] >= 0) {
      elementaryID <- elementaryID[-length(elementaryID)]
      elementaryID2 <- add_new_elementary()
    }
    newGroup <- c(elementaryID1, elementaryID2)
    newGroupIndex <- length(groups) + 1
    groups <- append(groups, list(newGroup))
    scissiparity(newGroupIndex)
    elementaryID1 <- NULL
    elementaryID2 <- NULL
    newGroup <- NULL
    newGroupIndex <- NULL
    
  } else if (randomProbability  == 2) {
    chosen_index1 <- sample(seq_along(groups), 1)
    chosen_group1 <- groups[[chosen_index1]]
    elementaryID1 <- sample(chosen_group1, 1)
    elementaryID2 <- add_new_elementary()
    while (elementaryID[[elementaryID1]] * elementaryID[[elementaryID2]] >= 0) {
      elementaryID <- elementaryID[-length(elementaryID)]
      elementaryID2 <- add_new_elementary()
    }
    
    groups[[chosen_index1]] <- append(groups[[chosen_index1]], elementaryID2)
    scissiparity(chosen_index1)
    chosen_index1 <- NULL
    chosen_group1 <- NULL
    elementaryID1 <- NULL
    elementaryID2 <- NULL
    
  } else if (randomProbability  == 3) {
    elementaryID1 <- add_new_elementary()
    chosen_index2 <- sample(seq_along(groups), 1)
    chosen_group2 <- groups[[chosen_index2]]
    elementaryID2<- sample(chosen_group2, 1)
    while (elementaryID[[elementaryID1]] * elementaryID[[elementaryID2]] >= 0) {
      elementaryID <- elementaryID[-length(elementaryID)]
      elementaryID1 <- add_new_elementary()
    } 
    groups[[chosen_index2]] <- append(groups[[chosen_index2]], elementaryID1)
    scissiparity(chosen_index2)
    elementaryID1 <- NULL
    chosen_index2 <- NULL
    chosen_group2 <- NULL
    elementaryID2 <- NULL
    
  } else if (randomProbability  == 4) {
    chosen_index1 <- sample(seq_along(groups), 1)
    chosen_group1 <- groups[[chosen_index1]]
    elementaryID1<- sample(chosen_group1, 1)
    temp_group <- chosen_group1
    groups <- groups[-chosen_index1]
    chosen_index2 <- sample(seq_along(groups), 1)
    chosen_group2 <- groups[[chosen_index2]]
    elementaryID2 <- sample(chosen_group2, 1)
    while (elementaryID[[elementaryID1]] * elementaryID[[elementaryID2]] >= 0) {
      chosen_index2 <- sample(seq_along(groups), 1)
      chosen_group2 <- groups[[chosen_index2]]
      elementaryID2 <- sample(chosen_group2, 1)
    }
    groups[[chosen_index2]] <- append(chosen_group2, temp_group)
    scissiparity(chosen_index2)
    temp_group <- NULL
    chosen_index1 <- NULL
    chosen_group1 <- NULL
    elementaryID1 <- NULL
    chosen_index2 <- NULL
    chosen_group2 <- NULL
    elementaryID2 <- NULL
  }
  randomProbability <- NULL
}