#' Get Arrests Data
#' @description Get arrests data from Crime Data Explorer (CDE)
#'
#' @param offense The type of crime
#' @param state The state of the offenders (eg. CA)
#' @param gender The gender of the offenders (default male)
#' @param since Beginning year of the offense (default 1991)
#' @param until Ending year of the offense (default 2010)
#' @param range Age range of the offenders (check CDE classifications for range)
#'
#' @return a data frame containing the arrest counts
#' @export
#'
#' @examples cde_get_data(offense="burglary", state="CA", range="range_13_14")
cde_get_data = function(offense, state, gender="male", since="1991", until="2010", range) {
  APIkey = "H4BjNdS8MMNWA3EbS06cXutBWyX3PxW2X7WcHm44"
  URL = "https://api.usa.gov/crime/fbi/sapi/api/data/arrest/states"

  param = paste("/", state, "/", offense, "/", gender, "/", since, "/",
                until, "?API_KEY=", APIkey, sep="")
  PATH = paste0(URL, param)
  df = jsonlite::fromJSON(PATH)$results

  rownames(df) = df$data_year  # Change index to year
  df = df[range]   # Keep only data for the specific range

  return(df)
}
