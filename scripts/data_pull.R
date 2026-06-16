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

setwd("data")

# ------------------------------------------------------------------------------
# HELPER FUNCTIONS
# ------------------------------------------------------------------------------

# Function to handle AirNow API fetching with browser disguise and initial time parsing
fetch_airnow_raw <- function(start_time, end_time, params, bbox) {
  api_url <- paste0("http://www.airnowapi.org/aq/data/?startDate=", start_time,
                    "&endDate=", end_time,
                    "&parameters=", params,
                    "&BBOX=", bbox,
                    "&dataType=C&format=application/json&verbose=1&monitortype=0&includerawconcentrations=1&API_KEY=DE0DE880-71CA-4A6B-95E5-7A6D57A19B70")
  
  res <- GET(api_url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"))
  df <- fromJSON(rawToChar(res$content))
  
  if (!is.data.frame(df)) {
    df <- tibble(UTC = character(), Parameter = character(), Unit = character(), 
                 RawConcentration = numeric(), SiteName = character())
  }
  
  df <- mutate(df, datetime = ymd_hm(UTC))
  df <- mutate(df, pdxtime = with_tz(datetime, tzone = "US/Pacific")) %>%
    mutate(date = date(pdxtime)) %>%
    mutate(hour = hour(pdxtime))
  
  return(df)
}

# Function to extract NWS lists, rename variables, and format timezones
extract_and_format_nws <- function(nws_property, value_name) {
  df <- as_tibble(nws_property$values)
  names(df)[names(df) == "value"] <- value_name
  df %>%
    mutate(datetime = ymd_hms(stringr::str_sub(validTime, start = 1L, end = 19L))) %>%
    mutate(pdxtime = with_tz(datetime, tzone = "US/Pacific")) %>%
    mutate(date = date(pdxtime)) %>%
    mutate(hour = hour(pdxtime))
}

# Function to separate dates, pivot wide, and update historical CSVs
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
  
  return(new_data_wide)
}

# ------------------------------------------------------------------------------
# retrieve & format forecast data
# ------------------------------------------------------------------------------

FURL <- "https://api.weather.gov/gridpoints/PQR/114,101"
FCST <- RETRY("GET", FURL, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"))
status <- status_code(FCST)

data <- fromJSON(rawToChar(FCST$content))

mix    <- extract_and_format_nws(data$properties$mixingHeight, "meters")
twind  <- extract_and_format_nws(data$properties$transportWindSpeed, "twindkmh")
swind  <- extract_and_format_nws(data$properties$windSpeed, "swindkmh")
precip <- extract_and_format_nws(data$properties$probabilityOfPrecipitation, "prob")
temp   <- extract_and_format_nws(data$properties$temperature, "temp")
hum    <- extract_and_format_nws(data$properties$relativeHumidity, "hum")
cloud  <- extract_and_format_nws(data$properties$skyCover, "cloud")

start <- today(tzone = "US/Pacific") - days(1)
start_datetime <- as.POSIXct(paste(start, "00:00:00"), tz = "US/Pacific") 
end <- max(temp$pdxtime)
hrseq <- seq.POSIXt(start_datetime, end, by = "1 hours")

hourly <- as.data.frame(hrseq)
hourly <- rename(hourly, pdxtime = hrseq)

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
hourly$tempfill <- na.locf(hourly$temp, na.rm = FALSE)
hourly$humfill <- na.locf(hourly$hum, na.rm = FALSE)
hourly$cloudfill <- na.locf(hourly$cloud, na.rm = FALSE)

hourly <- hourly %>%
  mutate(date = date(pdxtime),
         hour = hour(pdxtime))

today <- today(tzone = "US/Pacific")
tomorrow <- today + 1
noon <- force_tz(as.POSIXct(paste(today, "12:00:00"), tz = "US/Pacific")) 
eleven <- force_tz(as.POSIXct(paste(tomorrow, "11:00:00"), tz = "US/Pacific"))
HourlyN2N <- filter(hourly, pdxtime >= noon & pdxtime <= eleven)

mixfil24       <- filter(hourly, date == today) %>% mutate(meters = mixfill)
mixfiln2n      <- filter(hourly, pdxtime >= noon & pdxtime <= eleven) %>% mutate(meters = mixfill)
mixfilPM       <- filter(hourly, date == today & hour >= 18) %>% mutate(meters = mixfill)
mixfiltomorrow <- filter(hourly, date == tomorrow) %>% mutate(meters = mixfill)

twindfil24       <- filter(hourly, date == today) %>% mutate(twindkmh = twindfill)
twindfiln2n      <- filter(hourly, pdxtime >= noon & pdxtime <= eleven) %>% mutate(twindkmh = twindfill)
twindfilPM       <- filter(hourly, date == today & hour >= 18) %>% mutate(twindkmh = twindfill)
twindfiltomorrow <- filter(hourly, date == tomorrow) %>% mutate(twindkmh = twindfill)

swindfil24       <- filter(hourly, date == today) %>% mutate(swindkmh = swindfill)
swindfiln2n      <- filter(hourly, pdxtime >= noon & pdxtime <= eleven) %>% mutate(swindkmh = swindfill)
swindfilPM       <- filter(hourly, date == today & hour >= 18) %>% mutate(swindkmh = swindfill)
swindfiltomorrow <- filter(hourly, date == tomorrow) %>% mutate(swindkmh = swindfill)

precipfil24       <- filter(hourly, date == today) %>% mutate(prob = precipfill)
precipfiln2n      <- filter(hourly, pdxtime >= noon & pdxtime <= eleven) %>% mutate(prob = precipfill)
precipfilPM       <- filter(hourly, date == today & hour >= 18) %>% mutate(prob = precipfill)
precipfiltomorrow <- filter(hourly, date == tomorrow) %>% mutate(prob = precipfill)

tempfil24       <- filter(hourly, date == today) %>% mutate(temp = tempfill)
tempfiln2n      <- filter(hourly, pdxtime >= noon & pdxtime <= eleven) %>% mutate(temp = tempfill)
tempfilPM       <- filter(hourly, date == today & hour >= 18) %>% mutate(temp = tempfill)
tempfiltomorrow <- filter(hourly, date == tomorrow) %>% mutate(temp = tempfill)

humfil24       <- filter(hourly, date == today) %>% mutate(hum = humfill)
humfiln2n      <- filter(hourly, pdxtime >= noon & pdxtime <= eleven) %>% mutate(hum = humfill)
humfilPM       <- filter(hourly, date == today & hour >= 18) %>% mutate(hum = humfill)
humfiltomorrow <- filter(hourly, date == tomorrow) %>% mutate(hum = humfill)

cloudfil24       <- filter(hourly, date == today) %>% mutate(cloud = cloudfill)
cloudfiln2n      <- filter(hourly, pdxtime >= noon & pdxtime <= eleven) %>% mutate(cloud = cloudfill)
cloudfilPM       <- filter(hourly, date == today & hour >= 18) %>% mutate(cloud = cloudfill)
cloudfiltomorrow <- filter(hourly, date == tomorrow) %>% mutate(cloud = cloudfill)

mixfil24$feet <- mixfil24$meters * 3.28084
twindfil24$knots <- twindfil24$twindkmh * 0.539957

hourlydash <- hourly %>%
  filter(pdxtime >= as.POSIXct(paste(today, "07:00:00"), tz = "US/Pacific")) %>%
  mutate(mixfillft = mixfill * 3.28084,
         twindfillknot = twindfill * 0.539957,
         tempfillf = tempfill * (9/5) + 32,
         swindfillmph = swindfill * 0.621371)

start_window <- as.POSIXct(Sys.Date(), tz = "US/Pacific")
end_window <- as.POSIXct(Sys.Date() + 3, tz = "US/Pacific")

new_forecast_data <- hourly %>%
  filter(pdxtime >= start_window & pdxtime <= end_window) %>%
  mutate(forecast_run_date = Sys.Date()) %>%
  mutate(mixfillft = mixfill * 3.28084) %>%
  mutate(twindfillknot = twindfill * .539957) %>%
  mutate(tempfillf = tempfill * (9/5) + 32) %>%
  mutate(swindfillmph = swindfill * .621371) %>%
  select(forecast_run_date, pdxtime, mixfillft, twindfillknot, tempfillf, swindfillmph, precipfill, humfill, cloudfill)

archive_file_path = "forecast_archive.csv"

archive_data <- if (file.exists(archive_file_path)) {
  read_csv(archive_file_path, col_types = cols(.default = "d", forecast_run_date = "D", pdxtime = "T"))
} else {
  tibble()
}

combined_archive <- bind_rows(archive_data, new_forecast_data) %>%
  arrange(desc(forecast_run_date)) %>%      
  distinct(pdxtime, .keep_all = TRUE) %>%   
  arrange(pdxtime)

write_csv(combined_archive, archive_file_path)

mixToday <- mean(mixfil24$meters, na.rm = TRUE)
mixN2N <- mean(mixfiln2n$meters, na.rm = TRUE)
mixPM <- mean(mixfilPM$meters, na.rm = TRUE)
mixTomorrow <- mean(mixfiltomorrow$meters, na.rm = TRUE)
twindToday <- mean(twindfil24$twindkmh, na.rm = TRUE)
twindN2N <- mean(twindfiln2n$twindkmh, na.rm = TRUE)
twindPM <- mean(twindfilPM$twindkmh, na.rm = TRUE)
twindTomorrow <- mean(twindfiltomorrow$twindkmh, na.rm = TRUE)
swindToday <- mean(swindfil24$swindkmh, na.rm = TRUE)
swindN2N <- mean(swindfiln2n$swindkmh, na.rm = TRUE)
swindPM <- mean(swindfilPM$swindkmh, na.rm = TRUE)
swindTomorrow <- mean(swindfiltomorrow$swindkmh, na.rm = TRUE)
precipToday <- mean(precipfil24$prob, na.rm = TRUE)
precipN2N <- mean(precipfiln2n$prob, na.rm = TRUE)
precipPM <- mean(precipfilPM$prob, na.rm = TRUE)
precipTomorrow <- mean(precipfiltomorrow$prob, na.rm = TRUE)
tminToday <- mean(tempfil24$temp, na.rm = TRUE)
tminN2N <- mean(tempfiln2n$temp, na.rm = TRUE)
tminPM <- mean(tempfilPM$temp, na.rm = TRUE)
tminTomorrow <- mean(tempfiltomorrow$temp, na.rm = TRUE)
tmaxToday <- mean(tempfil24$temp, na.rm = TRUE)
tmaxN2N <- mean(tempfiln2n$temp, na.rm = TRUE)
tmaxPM <- mean(tempfilPM$temp, na.rm = TRUE)
tmaxTomorrow <- mean(tempfiltomorrow$temp, na.rm = TRUE)
ventToday <- round(mixToday * twindToday)
ventN2N <- round(mixN2N * twindN2N)
ventPM <- round(mixPM * twindPM)
ventTomorrow <-round( mixTomorrow * twindTomorrow)
humToday <- mean(humfil24$hum, na.rm = TRUE)
humN2N <- mean(humfiln2n$hum, na.rm = TRUE)
humPM <- mean(humfilPM$hum, na.rm = TRUE)
humTomorrow <- mean(humfiltomorrow$hum, na.rm = TRUE)
cloudToday <- mean(cloudfil24$cloud, na.rm = TRUE)
cloudN2N <- mean(cloudfiln2n$cloud, na.rm = TRUE)
cloudPM <- mean(cloudfilPM$cloud, na.rm = TRUE)
cloudTomorrow <- mean(cloudfiltomorrow$cloud, na.rm = TRUE)

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

#### Pull PM data
b <- fetch_airnow_raw(start, end, "PM25", "-122.651652,45.475295,-122.564448,45.516207")
pmtest <- b
b <- select(b, Parameter, Unit, RawConcentration, SiteName, pdxtime) %>%
  mutate(RawConcentration = na_if(RawConcentration, -999))

HR24 <- filter(b, pdxtime >= (max(b$pdxtime, na.rm = TRUE) - hours(24)))
yN2N <- filter(b, pdxtime >= dbynoon & pdxtime < ynoon)
tN2N <- filter(b, pdxtime >= ynoon & pdxtime < tnoon)
handy <- filter(b, pdxtime >= yone & pdxtime <= Sys.time())

tN2NPM <- round(mean(tN2N$RawConcentration, na.rm = TRUE), 1)
yN2NPM <- round(mean(yN2N$RawConcentration, na.rm = TRUE), 1)
HR24PM <- round(mean(HR24$RawConcentration, na.rm = TRUE), 1)

new_pm_wide <- update_airnow_csv(handy, "RecentPM.csv")
handytransposed <- new_pm_wide %>% select(date, any_of('00:00:00'), everything())


#### Pull OZONE data - lafeyette
d <- fetch_airnow_raw(start, end, "OZONE", "-122.651652,45.475295,-122.564448,45.516207")
oztest <- d
raw_ozone <- select(d, Parameter, Unit, RawConcentration, SiteName, pdxtime)

OZ_HR24 <- filter(raw_ozone, pdxtime >= (max(raw_ozone$pdxtime, na.rm = TRUE) - hours(24)))
OZ_yN2N <- filter(raw_ozone, pdxtime >= dbynoon & pdxtime < ynoon)
OZ_tN2N <- filter(raw_ozone, pdxtime >= ynoon & pdxtime < tnoon)
OZ_handy <- filter(raw_ozone, pdxtime >= yone & pdxtime <= Sys.time())

tN2NOZ <- round(mean(OZ_tN2N$RawConcentration, na.rm = TRUE), 1)
yN2NOZ <- round(mean(OZ_yN2N$RawConcentration, na.rm = TRUE), 1)
HR24OZ <- round(mean(OZ_HR24$RawConcentration[OZ_HR24$RawConcentration != -999], na.rm = TRUE), 1)

update_airnow_csv(OZ_handy, "RecentOzone.csv")


#### Pull OZONE data - carus
f <- fetch_airnow_raw(start, end, "OZONE", "-122.621387,45.230743,-122.531436,45.284152")
raw_ozone_carus <- select(f, Parameter, Unit, RawConcentration, SiteName, pdxtime)

OZ_HR24_carus <- filter(raw_ozone_carus, pdxtime >= (max(raw_ozone_carus$pdxtime, na.rm = TRUE) - hours(24)))
OZ_yN2N_carus <- filter(raw_ozone_carus, pdxtime >= dbynoon & pdxtime < ynoon)
OZ_tN2N_carus <- filter(raw_ozone_carus, pdxtime >= ynoon & pdxtime < tnoon)
OZ_handy_carus <- filter(raw_ozone_carus, pdxtime >= yone & pdxtime <= Sys.time())

tN2NOZ_carus <- round(mean(OZ_tN2N_carus$RawConcentration, na.rm = TRUE), 1)
yN2NOZ_carus <- round(mean(OZ_yN2N_carus$RawConcentration, na.rm = TRUE), 1)
HR24OZ_carus <- round(mean(OZ_HR24_carus$RawConcentration[OZ_HR24_carus$RawConcentration != -999], na.rm = TRUE), 1)

peakoz8hr_carus <- raw_ozone_carus %>% 
  filter(pdxtime >= (Sys.time() - hours(24))) %>%
  mutate(hour_of_day = hour(pdxtime)) %>%
  filter(hour_of_day >= 12 & hour_of_day <= 20) %>%
  filter(RawConcentration != -999) %>% 
  pull(RawConcentration) %>%
  mean(na.rm = TRUE)

update_airnow_csv(OZ_handy_carus, "RecentOzone_Carus.csv")

# ------------------------------------------------------------------------------
# Export files 
# ------------------------------------------------------------------------------

date <- today(tzone = "US/Pacific")
created <- Sys.time()
HR24no <- NA
no2max_24 <- NA

df <- data.frame(date, mixToday, mixN2N, mixPM, mixTomorrow, twindToday, twindN2N, twindPM, twindTomorrow, swindToday, swindN2N, swindPM, swindTomorrow, precipToday, precipN2N, precipPM, precipTomorrow, tminToday, tminN2N, tminPM, tminTomorrow, tmaxToday, tmaxN2N, tmaxPM, tmaxTomorrow, ventToday, ventN2N, ventPM, ventTomorrow, yN2NPM, HR24PM, HR24OZ, HR24no, no2max_24, humToday, humN2N, humPM, humTomorrow, cloudToday, cloudN2N, cloudPM, cloudTomorrow, created, HR24OZ_carus, peakoz8hr_carus)

df <- mutate(df, date = as_date(df$date))
df <- mutate(df, created = as_date(df$created))

old <- read.csv("ForecastSummaryToday.csv")
old <- mutate(old, date = as_date(old$date))
old <- mutate(old, created = as_date(old$created))
old <- select(old, -any_of("X")) 

new <- bind_rows(old, df)

write.csv(new, "ForecastSummaryToday.csv", row.names = FALSE)
save(new, file = "ForecastSummaryToday.Rdata")
save(hourlydash, file = "hourlydash.Rdata")
save(handytransposed, file = "handytransposed.Rdata")
save(b, file = "hourlyobpm.Rdata")
save(raw_ozone, file = "hourlyobozone.rData")
save(raw_ozone_carus, file = "hourlyobozone_carus.rData")
h <- data.frame() 
save(h, file = "hourlyobno.Rdata")

print("Run Complete!")
