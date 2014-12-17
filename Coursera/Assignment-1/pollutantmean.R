# both pollutants items
pollutants <- c("sulfate", "nitrate")

# returns mean of pollutant data for a directory
# NULL if directory OR pollutant is in-valid
pollutantmean <- function(directory, pollutant, ids) {

  # checks for valid directory name
  if (!file.exists(directory)) {
    print(paste(directory, ": No such directory exists."))
    return()
  }

  # represent sum and total of pollutant data elements
  summation <- 0
  total <- 0

  # pollutant argument is valid
  if (pollutant %in% pollutants) {
    for (id in ids) {
      # load csv data for a file according to its ID
      dataset <- paste(directory, "/", format(id), ".csv", sep = "")
      data <- read.csv(dataset)

      # extract data for a particular pollutant, omit NA values
      pollutant_data <- data[,pollutants[match(pollutant, pollutants)]]
      pollutant_data <- pollutant_data[!is.na(pollutant_data)]

      # update total and summation values
      total <- total + length(pollutant_data)
      summation <- summation + sum(pollutant_data)
    }

    return(summation/total)

  # in-valid pollutant argument
  } else {
    print(paste(pollutant, ": No such pollutant in the given dataset.", sep = ""))
    return()
  }
}

# return a character vector representing a data file's name
# according to the ID provided
format <- function(id) {
  if (id < 10) {
    paste("00", id, sep = "")
  } else if (id >= 10 & id < 100) {
    paste("0", id, sep = "")
  } else {
    id
  }
}
