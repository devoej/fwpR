#' Download Lotek collar data from WebService using API
#'
#' This function downloads Lotek collar data and outputs a dataframe.
#'
#' @param Username Webservice username credential
#' @param Password Webservice password credential
#' @param StartDate beginning date of download, assumes from 12AM onwards
#' @param EndDate Webservice username credential, assumes up to 11:59PM
#' @return A dataframe of collar locations within the provided start and end dates, arranged by device SN and date
#' @export

# - Function to download lotek data
#   Original versions of code provided by Lotek
lotekDownload <- function(Username, # WebService username
                          Password, # Webservice password
                          StartDate, # Desired start date
                          EndDate) { # Desired end date

  res <- httr::POST("https://webservice.lotek.com/API/user/login",
                    body = list(grant_type = "password",
                                username = Username,
                                password = Password),
                    encode = "form", httr::verbose())

  key <- as.character(list(httr::content(res))[[1]][1])

  positions <- httr::GET(paste0("https://webservice.lotek.com/API/positions/findByDate?from=",
                               paste0(StartDate, "T07:00:00Z"), # Must be in yyyy-mm-ddThh:mm:00Z format (= 12:00 am, Mtn Standard Time)
                               "&to=",
                               paste0(EndDate, "T06:59:59Z")), # Must be in yyyy-mm-ddThh:mm:00Z format (= 11:59 pm, Mtn Standard Time)
                         httr::add_headers(Authorization = paste("Bearer", key, sep = " ")))

  content <- httr::content(positions, as = "parsed", type = "application/json")

  GPSDat <- dplyr::bind_rows(content) %>%
    dplyr::mutate(DateTimeGMT = lubridate::parse_date_time(RecDateTime, orders = "ymd_HMS"),
           FixStatus = parse_RxStatus(RxStatus),
           Satellites = parse_Satellites(RxStatus)) %>%
    dplyr::select(DeviceID, DateTimeGMT, Latitude, Longitude, Altitude, PDOP, MainV, Temperature, FixStatus, Satellites) %>%
    dplyr::arrange(DeviceID, DateTimeGMT)
  return(GPSDat)
}

# - Function to parse the RxStatus field to match output from manually downloads
#   Function provided by Ben Fostaty at Lotek
parse_RxStatus <- Vectorize(
  function(x){

    binary <- GA::decimal2binary(x, length = 7)

    fix_strategy <- GA::binary2decimal(binary[5:7])


    satellites <- GA::binary2decimal(binary[1:4])

    fix_type <- dplyr::case_when(
      fix_strategy == 0 ~ "No Fix",
      fix_strategy == 1 ~ "1 SV KF",
      fix_strategy == 2 ~ "2 SV KF",
      fix_strategy == 3 ~ "3 SV KF",
      fix_strategy == 4 ~ "4 or more SV KF",
      fix_strategy == 5 ~ "2-D least squares",
      fix_strategy == 6 ~ "3-D least squares",
      fix_strategy == 7 ~ "Dead Reckoning")

    return(paste0(fix_type, ", ", satellites, " SVs"))}
)

# - Function to parse number of satellites
parse_Satellites <- Vectorize(
  function(x){

    binary <- GA::decimal2binary(x, length = 7)

    satellites <- GA::binary2decimal(binary[1:4])

    return(satellites)}
)
