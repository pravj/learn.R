# return data frame having columns for file ID and no. of complete observation cases
complete <- function(directory, ids) {

  # checks for valid directory name
  if (!file.exists(directory)) {
    print(directory, ": No such directory exists.")
    return()
  }

  # vectors that will be columns in the resultant data frame
  id <- c()
  nobs <- c()

  for (ID in ids) {
    # load csv data for a file according to its ID
    dataset <- paste(directory, "/", format(ID), ".csv", sep = "")
    data <- read.csv(dataset)

    # both pollutant's data
    nitrate_data <- data[,"nitrate"]
    sulfate_data <- data[,"sulfate"]

    # subset data where either both or none's value is NA
    complete_data <- nitrate_data[is.na(nitrate_data) == is.na(sulfate_data)]
    # omits rows where both values are NA
    complete_data <- complete_data[!is.na(complete_data)]

    # update both vectors as we traverse
    id <- c(id, ID)
    nobs <- c(nobs, length(complete_data))
  }

  return(data.frame(id, nobs))
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
