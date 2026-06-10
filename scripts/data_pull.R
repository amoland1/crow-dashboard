# ------------------------------------------------------------------------------
# set up work space
# ------------------------------------------------------------------------------

library(httr)
library(jsonlite)
library(tidyr)
library(lubridate)
library(dplyr)
library(stringr)
library(zoo)
library(hms)
library(readr)
library(padr)

api_key <- Sys.getenv("AIRNOW_API_KEY")
api_url <- paste0("http://www.airnowapi.org/aq/data/?startDate=", start_time,
                  "&endDate=", end_time,
                  "&parameters=", params,
                  "&BBOX=", bbox,
                  "&dataType=C&format=application/json&verbose=1&monitortype=0&includerawconcentrations=1&API_KEY=", api_key)

# ------------------------------------------------------------------------------
# HELPER FUNCTIONS (Efficiency Additions)
# ------------------------------------------------------------------------------

# Efficiency 1: Function to handle AirNow API fetching and initial time parsing
fetch_airnow_raw <- function(start_time, end_time, params, bbox) {
  api_url <- paste0("http://www.airnowapi.org/aq/data/?startDate=", start_time,
                    "&endDate=", end_time,
                    "&parameters=", params,
                    "&BBOX=", bbox,
                    "&dataType=C&format=application/json&verbose=1&monitortype=0&includerawconcentrations=1&API_KEY=DE0DE880-71CA-4A6B-95E5-7A6D57A19B70")
  
  res <- GET(api_url)
  df <- fromJSON(rawToChar(res$content))
  
  # SAFETY CHECK: If API returns an empty [] or an error {}, force it into a safe empty table
  if (!is.data.frame(df)) {
    df <- tibble(UTC = character(), Parameter = character(), Unit = character(), 
                 RawConcentration = numeric(), SiteName = character())
  }
  
  # This part will now run safely even if the API returned no data
  df <- mutate(df, datetime = ymd_hm(UTC))
  df <- mutate(df, pdxtime = with_tz(datetime, tzone = "US/Pacific")) %>%
    mutate(date = date(pdxtime)) %>%
    mutate(hour = hour(pdxtime))
  
  return(df)
}

# Efficiency 2: Function to extract NWS lists, rename variables, and format timezones
extract_and_format_nws <- function(nws_property, value_name) {
  df <- as_tibble(nws_property$values)
  names(df)[names(df) == "value"] <- value_name
  df %>%
    mutate(datetime = ymd_hms(stringr::str_sub(validTime, start = 1L, end = 19L))) %>%
    mutate(pdxtime = with_tz(datetime, tzone = "US/Pacific")) %>%
    mutate(date = date(pdxtime)) %>%
    mutate(hour = hour(pdxtime))
}

# Efficiency 3: Function to separate dates, pivot wide, and update historical CSVs
update_airnow_csv <- function(handy_df, csv_path) {
  handy2 <- handy_df %>%
    separate(pdxtime, into = c("date", "time"), sep = " ", convert = TRUE) %>%
    mutate(time = format(ymd_hms(paste(date, time)), "%H:%M:%S"))
  
  handy2$time[is.na(handy2$time)] <- "00:00:00"
  
  new_data_wide <- handy2 %>%
    pivot_wider(names_from = time, values_from = RawConcentration, id_cols = date) %>%
    mutate(date = as.character(date))
  
  if (file.exists(csv_path)) {
    old_data <- read_csv(csv_path, col_types = cols(.default = "d", date = "c"))
    combined <- bind_rows(new_data_wide, old_data)
    final_data <- combined %>%
      group_by(date) %>%
      summarise(across(everything(), ~ {
        vals <- na.omit(.)
        if (length(vals) == 0) NA else first(vals)
      })) %>%
      arrange(desc(date)) 
  } else {
    final_data <- new_data_wide
  }
  
  write_csv(final_data, csv_path, na = "")
  print(paste("Data saved to", csv_path))
  
  return(new_data_wide) # Returns the wide data back to the script for handytransposed
}

# ------------------------------------------------------------------------------
# retrieve & format forecast data (Using Efficiency 2)
# ------------------------------------------------------------------------------

FURL <- "https://api.weather.gov/gridpoints/PQR/114,101"
FCST <- RETRY("GET", FURL)
status <- status_code(FCST)

data <- fromJSON(rawToChar(FCST$content))

mix    <- extract_and_format_nws(data$properties$mixingHeight, "meters")
twind  <- extract_and_format_nws(data$properties$transportWindSpeed, "twindkmh")
swind  <- extract_and_format_nws(data$properties$windSpeed, "swindkmh")
precip <- extract_and_format_nws(data$properties$probabilityOfPrecipitation, "prob")
temp   <- extract_and_format_nws(data$properties$temperature, "temp")
hum    <- extract_and_format_nws(data$properties$relativeHumidity, "hum")
cloud  <- extract_and_format_nws(data$properties$skyCover, "cloud")

start <- today(tzone = "US/Pacific")
start_datetime <- as.POSIXct(paste(start, "00:00:00"), tz = "US/Pacific") 
end <- max(temp$pdxtime)
hrseq <- seq.POSIXt(start_datetime, end, by = "1 hours")

# Create a data frame with hourly values that fills in null values for download
hourly <- as.data.frame(hrseq)
hourly <- rename(hourly, pdxtime = hrseq)
hourly <- mutate(hourly, pdxtime = ymd_hms(hourly$pdxtime))
hourly <- mutate(hourly, pdxtime = with_tz(pdxtime, tzone = "US/Pacific"))
hourly$mix <- mix$meters[match(hourly$pdxtime, mix$pdxtime)]
hourly$twind <- twind$twindkmh[match(hourly$pdxtime, twind$pdxtime)]
hourly$swind <- swind$swindkmh[match(hourly$pdxtime, swind$pdxtime)]
hourly$precip <- precip$prob[match(hourly$pdxtime, precip$pdxtime)]
hourly$temp <- temp$temp[match(hourly$pdxtime, temp$pdxtime)]
hourly$hum <- hum$hum[match(hourly$pdxtime, hum$pdxtime)]
hourly$cloud<- cloud$cloud[match(hourly$pdxtime, cloud$pdxtime)]
hourly <- pad(hourly)
hourly$mixfill <- na.locf(hourly$mix, na.rm = FALSE)
hourly$twindfill <- na.locf(hourly$twind, na.rm = FALSE)
hourly$swindfill <- na.locf(hourly$swind, na.rm = FALSE)
hourly$precipfill <- na.locf(hourly$precip, na.rm = FALSE)
hourly$tempfill <- na.locf(hourly$temp, na.rm =FALSE)
hourly$humfill <- na.locf(hourly$hum, na.rm =FALSE)
hourly$cloudfill <- na.locf(hourly$cloud, na.rm =FALSE)

#Create dfs for today, noon to noon, and tomorrow 
today <- today(tzone = "US/Pacific")
tomorrow <- today + 1
noon <- force_tz(as.POSIXct(paste(today, "12:00:00"), tz = "US/Pacific")) 
eleven <- force_tz(as.POSIXct(paste(tomorrow, "11:00:00"), tz = "US/Pacific"))
HourlyN2N <- filter(hourly, pdxtime >= noon & pdxtime <= eleven)

#calculate metrics
mixfil24 <- filter(mix, date == today)
mixfiln2n <- filter(mix, pdxtime >= noon & pdxtime <= eleven)
mixfilPM <- filter(mix, date == today & hour >= 18)
mixfiltomorrow <- filter(mix, date == tomorrow)
twindfil24 <- filter(twind, date == today)
twindfiln2n <- filter(twind, pdxtime >= noon & pdxtime <= eleven)
twindfilPM <- filter(twind, date == today & hour >= 18)
twindfiltomorrow <- filter(twind, date == tomorrow)
swindfil24 <- filter(swind, date == today)
swindfiln2n <- filter(swind, pdxtime >= noon & pdxtime <= eleven)
swindfilPM <- filter(swind, date == today & hour >= 18)
swindfiltomorrow <- filter(swind, date == tomorrow)
precipfil24 <- filter(precip, date == today)
precipfiln2n <- filter(precip, pdxtime >= noon & pdxtime <= eleven)
precipfilPM <- filter(precip, date == today & hour >= 18)
precipfiltomorrow <- filter(precip, date == tomorrow)
tempfil24 <- filter(temp, date == today)
tempfiln2n <- filter(temp, pdxtime >= noon & pdxtime <= eleven)
tempfilPM <- dplyr::filter(temp, date == today & hour >= 18)
tempfiltomorrow <- filter(temp, date == tomorrow)
humfil24 <- filter(hum, date == today)
humfiln2n <- filter(hum, pdxtime >= noon & pdxtime <= eleven)
humfilPM <- filter(hum, date == today & hour >= 18)
humfiltomorrow <- filter(hum, date == tomorrow)
cloudfil24 <- filter(cloud, date == today)
cloudfiln2n <- filter(cloud, pdxtime >= noon & pdxtime <= eleven)
cloudfilPM <- filter(cloud, date == today & hour >= 18)
cloudfiltomorrow <- filter(cloud, date == tomorrow)

#convert units for dashboard charts
mixfil24$feet <-mixfil24$meters*3.28084
twindfil24$knots <- twindfil24$knots

# Create table for dashboard charts
hourlydash <- hourly %>%
  mutate(pdxtime2 = pdxtime) %>%
  separate(pdxtime2, into = c("date", "time"), sep = " ") %>%
  slice(match('07:00:00', time):n()) %>%
  mutate(mixfillft = mixfill * 3.28084) %>%
  mutate(twindfillknot = twindfill * .539957) %>%
  mutate(tempfillf = tempfill * (9/5)+32) %>%
  mutate(swindfillmph = swindfill *.621371)

# Update Forecast Archive
start_window <- floor_date(min(hourly$pdxtime), "day")
end_window <- as.POSIXct(Sys.Date() + 2, tz = "US/Pacific")

new_forecast_data <- hourly %>%
  filter(pdxtime >= start_window & pdxtime <= end_window) %>%
  mutate(forecast_run_date = Sys.Date()) %>%
  mutate(mixfillft = mixfill * 3.28084) %>%
  mutate(twindfillknot = twindfill * .539957) %>%
  mutate(tempfillf = tempfill * (9/5) + 32) %>%
  mutate(swindfillmph = swindfill * .621371) %>%
  select(forecast_run_date, pdxtime, mixfillft, twindfillknot, tempfillf, swindfillmph, precipfill, humfill, cloudfill)

archive_file_path <- "data/forecast_archive.csv"

archive_data <- if (file.exists(archive_file_path)) {
  read_csv(archive_file_path, col_types = cols(.default = "d", forecast_run_date = "D", pdxtime = "T"))
} else {
  tibble()
}

combined_archive <- bind_rows(archive_data, new_forecast_data) %>%
  arrange(desc(forecast_run_date)) %>%      # Puts the newest runs at the top
  distinct(pdxtime, .keep_all = TRUE) %>%   # Keeps only the first (newest) row per hour
  arrange(pdxtime)

write_csv(combined_archive, archive_file_path)

#create mean variables for advisory interpretation
mixToday <- mean(mixfil24$meters)
mixN2N <- mean(mixfiln2n$meters)
mixPM <- mean(mixfilPM$meters)
mixTomorrow <- mean(mixfiltomorrow$meters)
twindToday <- mean(twindfil24$twindkmh)
twindN2N <- mean(twindfiln2n$twindkmh)
twindPM <- mean(twindfilPM$twindkmh)
twindTomorrow <- mean(twindfiltomorrow$twindkmh)
swindToday <- mean(swindfil24$swindkmh)
swindN2N <- mean(swindfiln2n$swindkmh)
swindPM <- mean(swindfilPM$swindkmh)
swindTomorrow <- mean(swindfiltomorrow$swindkmh)
precipToday <- mean(precipfil24$prob)
precipN2N <- mean(precipfiln2n$prob)
precipPM <- mean(precipfilPM$prob)
precipTomorrow <- mean(precipfiltomorrow$prob)
tminToday <- mean(tempfil24$temp)
tminN2N <- mean(tempfiln2n$temp)
tminPM <- mean(tempfilPM$temp)
tminTomorrow <- mean(tempfiltomorrow$temp)
tmaxToday <- mean(tempfil24$temp)
tmaxN2N <- mean(tempfiln2n$temp)
tmaxPM <- mean(tempfilPM$temp)
tmaxTomorrow <- mean(tempfiltomorrow$temp)
ventToday <- round(mixToday * twindToday)
ventN2N <- round(mixN2N * twindN2N)
ventPM <- round(mixPM * twindPM)
ventTomorrow <-round( mixTomorrow * twindTomorrow)
humToday <- mean(humfil24$hum)
humN2N <- mean(humfiln2n$hum)
humPM <- mean(humfilPM$hum)
humTomorrow <- mean(humfiltomorrow$hum)
cloudToday <- mean(cloudfil24$cloud)
cloudN2N <- mean(cloudfiln2n$cloud)
cloudPM <- mean(cloudfilPM$cloud)
cloudTomorrow <- mean(cloudfiltomorrow$cloud)

today <- today()
yesterday <- today-1
dby <- today-2
now <- hour(now())


# ------------------------------------------------------------------------------
# download and format historic pollutant data for past two days
# ------------------------------------------------------------------------------

current_utc <- with_tz(Sys.time(), "UTC")
two_days_ago_utc <- current_utc - days(2)
start <- format(two_days_ago_utc, "%Y-%m-%dT07") 
end <- format(current_utc, "%Y-%m-%dT%H")

dbynoon <- force_tz(as.POSIXct(paste(dby, "12:00:00"), tz = "US/Pacific"))
ynoon <- force_tz(as.POSIXct(paste(yesterday, "12:00:00"), tz = "US/Pacific"))
tnoon <- force_tz(as.POSIXct(paste(today, "12:00:00"), tz = "US/Pacific"))
yzero <- force_tz(as.POSIXct(paste(yesterday, "00:00:00"), tz = "US/Pacific"))
yone <- force_tz(as.POSIXct(paste(yesterday, "01:00:00"), tz = "US/Pacific"))
midnight <- force_tz(as.POSIXct(paste(today, "00:00:00"), tz = "US/Pacific")) 

#### Pull PM data (Using Efficiency 1 & 3)
b <- fetch_airnow_raw(start, end, "PM25", "-122.651652,45.475295,-122.564448,45.516207")
pmtest <- b
b <- select(b, Parameter, Unit, RawConcentration, SiteName, pdxtime) %>%
  mutate(RawConcentration = na_if(RawConcentration, -999))

HR24 <- filter(b, pdxtime >= yone & pdxtime <= midnight)
yN2N <- filter(b, pdxtime >= dbynoon & pdxtime < ynoon)
tN2N <- filter(b, pdxtime >= ynoon & pdxtime < tnoon)
handy <- filter(b,pdxtime >= yone & pdxtime <= Sys.time())

tN2NPM <- round(mean(tN2N$RawConcentration),1)
yN2NPM <- round(mean(yN2N$RawConcentration),1)
HR24PM <- round(mean(HR24$RawConcentration, na.rm = TRUE), 1)

# Format, Update CSV, and capture handytransposed in one step
new_pm_wide <- update_airnow_csv(handy, "data/RecentPM.csv")
handytransposed <- new_pm_wide %>% select(date, any_of('00:00:00'), everything())


#### Pull OZONE data - lafeyette (Using Efficiency 1 & 3)
d <- fetch_airnow_raw(start, end, "OZONE", "-122.651652,45.475295,-122.564448,45.516207")
oztest <- d
raw_ozone <- select(d, Parameter, Unit, RawConcentration, SiteName, pdxtime)

OZ_HR24 <- filter(raw_ozone, pdxtime >= yone & pdxtime <= midnight)
OZ_yN2N <- filter(raw_ozone, pdxtime >= dbynoon & pdxtime < ynoon)
OZ_tN2N <- filter(raw_ozone, pdxtime >= ynoon & pdxtime < tnoon)
OZ_handy <- filter(raw_ozone,pdxtime >= yone & pdxtime <= Sys.time())

tN2NOZ <- round(mean(OZ_tN2N$RawConcentration),1)
yN2NOZ <- round(mean(OZ_yN2N$RawConcentration),1)
HR24OZ <- round(mean(OZ_HR24$RawConcentration[OZ_HR24$RawConcentration != -999]), 1)

update_airnow_csv(OZ_handy, "data/RecentOzone.csv")


#### Pull OZONE data - carus (Using Efficiency 1 & 3)
f <- fetch_airnow_raw(start, end, "OZONE", "-122.621387,45.230743,-122.531436,45.284152")
raw_ozone_carus <- select(f, Parameter, Unit, RawConcentration, SiteName, pdxtime)

OZ_HR24_carus <- filter(raw_ozone_carus, pdxtime >= yone & pdxtime <= midnight)
OZ_yN2N_carus <- filter(raw_ozone_carus, pdxtime >= dbynoon & pdxtime < ynoon)
OZ_tN2N_carus <- filter(raw_ozone_carus, pdxtime >= ynoon & pdxtime < tnoon)
OZ_handy_carus <- filter(raw_ozone_carus,pdxtime >= yone & pdxtime <= Sys.time())

tN2NOZ_carus <- round(mean(OZ_tN2N_carus$RawConcentration),1)
yN2NOZ_carus <- round(mean(OZ_yN2N_carus$RawConcentration),1)
HR24OZ_carus <- round(mean(OZ_HR24_carus$RawConcentration[OZ_HR24_carus$RawConcentration != -999], na.rm = TRUE),1)

peakoz8hr_carus <- raw_ozone_carus %>% #feedback update 
  filter(pdxtime >= (Sys.time() - hours(24))) %>%
  mutate(hour_of_day = hour(pdxtime)) %>%
  filter(hour_of_day >= 12 & hour_of_day <= 20) %>%
  pull(RawConcentration) %>%
  mean(na.rm = TRUE)

update_airnow_csv(OZ_handy_carus, "data/RecentOzone_Carus.csv")

# ------------------------------------------------------------------------------
# Export files 
# ------------------------------------------------------------------------------

# Create forecast summary spreadsheet
date <- today(tzone = "US/Pacific")
created <- Sys.time()
HR24no <- NA
no2max_24 <- NA

df <- data.frame(date, mixToday, mixN2N, mixPM, mixTomorrow, twindToday, twindN2N, twindPM, twindTomorrow, swindToday, swindN2N, swindPM, swindTomorrow, precipToday, precipN2N, precipPM, precipTomorrow, tminToday, tminN2N, tminPM, tminTomorrow, tmaxToday, tmaxN2N, tmaxPM, tmaxTomorrow, ventToday, ventN2N, ventPM, ventTomorrow, yN2NPM, HR24PM, HR24OZ, HR24no, no2max_24, humToday, humN2N, humPM, humTomorrow, cloudToday, cloudN2N, cloudPM, cloudTomorrow, created,HR24OZ_carus, peakoz8hr_carus)

df <- mutate(df, date = as_date(df$date))
df <- mutate(df, created = as_date(df$created))

# Read old data and safely remove 'X' column if it exists
old <- read.csv("ForecastSummaryToday.csv")
old <- mutate(old, date = as_date(old$date))
old <- mutate(old, created = as_date(old$created))
old <- select(old, -any_of("X")) 

new <- bind_rows(old, df)

#Export data for use in dashboard
write.csv(new,"ForecastSummaryToday.csv", row.names = FALSE)
save(new, file = "ForecastSummaryToday.Rdata")
save(hourlydash, file = "hourlydash.Rdata")
save(handytransposed, file = "handytransposed.Rdata")
save(b, file = "hourlyobpm.Rdata")
save(raw_ozone, file = "hourlyobozone.rData")
save(raw_ozone_carus, file = "hourlyobozone_carus.rData")
h <- data.frame() 
save(h, file = "hourlyobno.Rdata")

print("Run Complete!")