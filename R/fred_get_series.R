#' Fetch Series from FRED
#' @description Fetch a series from FRED and return as a data frame
#'
#' @param series_id The id of the series
#' @param observation_start Starting time of the fetched series
#' @param observation_end Ending time of the fetched series
#'
#' @return a data frame containing the series
#' @export
#'
#' @examples
#' fred_get_series(series_id="BOPBCA", observation_start="1960-01-01", observation_end="2010-12-31")
fred_get_series = function(series_id, observation_start, observation_end){
  APIkey = "48748353d2e76baf3e15a04b22d0f034"
  URL = "https://api.stlouisfed.org/fred/series/observations"

  parameters = paste(
    "?series_id=",series_id,
    "&api_key=", APIkey,
    "&file_type=json",
    "&observation_start=", observation_start,
    "&observation_end=", observation_end,
    sep = "")
  PATH = paste0(URL, parameters)

  df = jsonlite::fromJSON(PATH)$observations

  # Change index to Date and keep only the value column. Also change the column name
  rownames(df) = df$date
  df = df[c("value")]
  df[,1] = as.numeric(df[,1])
  colnames(df) = series_id

  return(df)
}
