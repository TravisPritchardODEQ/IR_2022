#' Retrieve surface water monitoring results from The City of Portland Bureau of Environmental Services
#'
#' Data is retrieved from City of Portland Bureau of Environmental Services database.
#' https://aquarius.portlandoregon.gov/
#'
#' @param station Required string vector of station location IDs to be fetched.
#' @param startdate Required string setting the start date of the data being fetched. Format 'yyyy-mm-dd'.
#' @param enddate Required string setting the end date of the data being fetched. Format 'yyyy-mm-dd'.
#' @param char Required string vector identifying the characteristics to be fetched. Options include:
#'   * 'Dissolved oxygen.Field Visits' - Dissolved oxygen concentration grab measurements (mg/l)
#'   * 'Dissolved oxygen.Primary' - Continuous dissolved oxygen concentration (mg/l)
#'   * 'Dissolved oxygen saturation.Field Visits' (%)
#'   * 'Dissolved oxygen saturation.Primary' (%)
#'   * 'pH.Field Visits' - pH grab measurements (pH)
#'   * 'pH.Primary' - Continuous pH (pH)
#'   * 'Specific conductance.Field Visits' - Specific conductance grab measurements (uS/cm)
#'   * 'Specific conductance.Primary' - Continuous Specific conductance (uS/cm)
#'   * 'Temperature.Field Visits' - Water temperature grab measurements (deg C)
#'   * 'Temperature.Primary' - Continuous Water temperature (deg C)
#'   * 'Temperature.7DADM' - 7-Day mean daily maximum water temperature (deg C)
#' @export
#' @return data frame of data

copbes_data <- function(station, startdate, enddate, char) {

  # Testing
  # station=c("92B", "VNB")
  # startdate="2019-04-01"
  # enddate="2019-12-31"
  # char=c("Temperature.Primary", "Dissolved oxygen.Field Visits")

  baseurl <- "https://aquarius.portlandoregon.gov/Export/DataSet?"

  if(!any(char %in% c('Dissolved oxygen.Field Visits', 'Dissolved oxygen.Primary',
                      'Dissolved oxygen saturation.Field Visits', 'Dissolved oxygen saturation.Primary',
                      'pH.Field Visits', 'pH.Primary',
                      'Specific conductance.Field Visits', 'Specific conductance.Primary',
                      'Temperature.Field Visits', 'Temperature.Primary', 'Temperature.7DADM'))) {
    stop("non valid value in 'char'")
  }

  # data frame of all combinations of query items
  df.query <- expand.grid(baseurl=baseurl, station=station, startdate=startdate, enddate=enddate, char=char, stringsAsFactors = FALSE)

  # build data frame with the query url
  df.query <- df.query %>%
    dplyr::mutate(unit=dplyr::case_when(char %in% c('Dissolved oxygen.Field Visits', 'Dissolved oxygen.Primary') ~ "mg/l",
                                        char %in% c('Dissolved oxygen saturation.Field Visits', 'Dissolved oxygen saturation.Primary') ~ "%",
                                        char %in% c('pH.Field Visits', 'pH.Primary') ~ "pH",
                                        char %in% c('Specific conductance.Field Visits', 'Specific conductance.Primary') ~ "uS/cm",
                                        char %in% c('Temperature.Field Visits', 'Temperature.Primary', 'Temperature.7DADM') ~ "degC"),
                  url=paste0(baseurl, "DataSet=", gsub(" ","%20", char, fixed = TRUE), "%40", station,
                               "&StartTime=",startdate, "%2000:00:0%20",
                               "&EndTime=",enddate, "%2000:00:0%20",
                               "&ExportFormat=csv",
                               "&Compressed=false",
                               "&Unit=",unit,
                               "&Timezone=-8")) %>%
    dplyr::select(url, station, char, unit) %>%
    as.data.frame(stringsAsFactors=FALSE)

  # special COP function with httr::GET and other clean up items to use within lapply
  copbes_GET <- function(x) {
    res <- httr::GET(url=x$url)
    df <- httr::content(x=res, type = "text/csv",
                        encoding = "UTF-8",
                        col_types=readr::cols("T","d","d","d", "d", "c", "c"),
                        skip=1)
    names(df) <- c("datetime", "Result.Value", "Grade.Code", "Approval.Level", "Interpolation.Type", "Event.Timestamp", "Comment")
    df$Monitorining.Location.ID <- x$station
    df$Characteristic.Name <- x$char
    df$Result.Unit <- x$unit
    return(df)
  }

  # separate each query result into a list
  df1 <- df.query %>%
    dplyr::group_by(url) %>%
    dplyr::group_split() %>%
    lapply(FUN = copbes_GET)

  # Bind list into single data frame and reorder cols
  df2 <- df1 %>%
    dplyr::bind_rows() %>%
    dplyr::select(Monitorining.Location.ID, datetime, Event.Timestamp,
                  Characteristic.Name, Result.Unit, Result.Value,
                  Interpolation.Type, Grade.Code, Approval.Level, Comment)

  return(df2)

}
