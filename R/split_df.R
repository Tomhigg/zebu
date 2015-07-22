#' Split Data Frame 
#' 
#' Function to randomly split a data frame into two sections, potentially for training and validation
#' As R will only allow a single item to be returned by a functio use 
#' 
#' @param dataframe The dataframe to be split
#' @param seed Option to set seet (random number generator) for reproducability, defaults to null
#' 
#' @return A list of training and validation dataframes, extract usiing training  <- input$trainset or input$valset


split_df <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  valset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(valset=valset,testset=testset)}
