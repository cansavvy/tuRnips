# C. Savonen

# Turnip summaries

# Publishes a report to https://bit.ly/34N0LU9
# Establish base dir
root_dir <- rprojroot::find_root(rprojroot::has_dir(".git"))

# Load library:
library(optparse)

#--------------------------------Set up options--------------------------------#
# Set up optparse options
option_list <- list(
  make_option(
    opt_str = c("-p", "--url"), type = "character",
    default = "", help = "URL")
)

# Parse options
opt <- parse_args(OptionParser(option_list = option_list))

# Name the data directory
data_dir <- file.path(root_dir, "data")
reports_dir <- file.path(root_dir, "reports")
cleaned_data_dir <- file.path(data_dir, "cleaned_data")

# Create the data directory if it doesn't exist
if(!dir.exists(cleaned_data_dir)){
  dir.create(cleaned_data_dir, recursive = TRUE)
}

# Create the reports directory if it doesn't exist
if(!dir.exists(reports_dir)){
  dir.create(reports_dir)
}

# Retrieve currrent day and time
current_date <- Sys.Date()
current_day_of_week <- weekdays(current_date)
current_time <- Sys.time()
noon <- as.POSIXct(strptime(c("12:00"), "%H:%M"), "EST")

# Name turnip file
turnip_file <- file.path(data_dir, paste0(current_date, "-turnips.xlsx"))

# Get the time since last update
if (file.exists(turnip_file)) {
  last_update <- difftime(Sys.time(), file.info(turnip_file)$mtime, units='mins')
} else {
  last_update <- 1200
}

# Download turnip data from Googlesheets if
if (any(!file.exists(turnip_file), as.numeric(last_update) > 5)) {
  system(paste("wget -O", turnip_file,
       "'https://docs.google.com/spreadsheets/d/1RTOuglfnwqMQzZ7BoTDMOTfr4rjVbksv85RwZB6YM74/export?format=xlsx&id=1RTOuglfnwqMQzZ7BoTDMOTfr4rjVbksv85RwZB6YM74'"))
}

# Magrittr pipe
`%>%` <- dplyr::`%>%`

############################### Get answers sheet ##############################
# Get sheet names
sheet_names <- readxl::excel_sheets(turnip_file)

# Collect sheet indices that aren't the overview
sheet_indices <- which(sheet_names != "Overview")

# Make this into a data.frame
all_turnip_prices <- lapply(sheet_indices, function(sheet_num) {
  # Obtain preview of next sheet
  preview <- readxl::read_excel(turnip_file,
                                sheet = sheet_num,
                                col_names = FALSE)

  # Skip these rows of nonsense
  pattern_starts <- which(preview$...1 == "Pattern")

  # Import reported prices
  reported_prices <- readxl::read_excel(turnip_file,
                                        sheet = sheet_num,
                                        col_names = TRUE,
                                        n_max = pattern_starts -3)  %>%
  # Select only columns we want
  dplyr::select(date = Date, AM, PM) %>%
  # Get rid of Price on prefix
  dplyr::mutate(date = gsub("Price on ", "", date)) %>%
  # Make long format
  tidyr::gather("time", "price", -date) %>%
  # Make new variables
  dplyr::mutate(prediction = "reported",
                reported_date = paste0(current_date, current_time))

  # Read in data for real
  turnip_pred <- readxl::read_excel(turnip_file,
                     sheet = sheet_num,
                     col_names = FALSE,
                     skip = pattern_starts) %>%
    t()

  # Remove first row and turn into a data.frame
  turnip_pred  <- turnip_pred [-1, ] %>%
    as.data.frame(stringsAsFactors = FALSE)

  # Name columns we need
  colnames(turnip_pred )[1:3] <- c("date", "time", "minmax")

  # Reformat date column so there isn't NAs
  turnip_pred$date <- rep(unique(turnip_pred$date[!is.na(turnip_pred $date)]),
                           each = 4)

  # Reformat Time column so there isn't NAs
  turnip_pred$time <- rep(c("AM", "AM", "PM", "PM"), 6)

  # Make into long format
  turnip_pred <- turnip_pred %>%
    tidyr::gather("prediction", "price", -date, -time, -minmax) %>%
    dplyr::mutate(price = as.numeric(price),
                  prediction = gsub("V", "prediction_", prediction),
                  reported_date = paste0(current_date, current_time))

  return(list(reported = reported_prices,
              predicted = turnip_pred))
})

# Separate into two lists
reported_prices <- purrr::map(all_turnip_prices, "reported")
predicted_prices <- purrr::map(all_turnip_prices, "predicted")

# Simplfy the names for these lists
simplified_names <- gsub(" |'s|Island|Village", "", sheet_names[sheet_indices])

# Name the islands
names(reported_prices) <- simplified_names
names(predicted_prices) <- simplified_names

# Make into one data.frame
reported_df <- dplyr::bind_rows(reported_prices, .id = "owner")
predicted_df <- dplyr::bind_rows(predicted_prices, .id = "owner")

# Get current prices 
current_am_pm <- ifelse(current_time < noon, "AM", "PM")

# Retrieve them
current_prices <- reported_df %>% 
  dplyr::filter(time == current_am_pm, date == weekdays(current_date)) %>% 
  dplyr::select(owner, price) %>% 
  dplyr::mutate() 

# Get total number of predictions
total_predictions <- predicted_df %>%
  dplyr::group_by(owner) %>%
  dplyr::summarize(total_predictions = dplyr::n()) %>%
  tibble::deframe()

# Get max prices per owner
max_prices <- predicted_df %>%
  dplyr::group_by(owner) %>%
  dplyr::summarize(max_price = max(price)) %>%
  tibble::deframe()

# Obtain which owners haven't reported
unreported_owners <- names(max_prices)[which(max_prices == 0)]

# Get a summary
prediction_summary <- predicted_df %>%
  dplyr::filter(price %in% max_prices) %>%
  dplyr::group_by(owner) %>%
  dplyr::summarize(which_days = paste(paste(date, time), collapse = ", "),
                   how_many_predictions = dplyr::n()) %>%
  dplyr::mutate(total_predictions_left = paste("out of", total_predictions), 
                current_prices = dplyr::pull(current_prices, price),  
                max_prices) %>% 
  dplyr::select(owner, current_prices, max_prices, dplyr::everything())

# Put "Not reported" for owners that didn't report enough
prediction_summary$how_many_predictions[which(prediction_summary$owner %in% unreported_owners)] <- "Not reported"

# Put unknown
prediction_summary$which_days[which(prediction_summary$owner %in% unreported_owners)] <- "Unknown"

# Put data together
combined_df <- dplyr::bind_rows(reported_df, predicted_df) %>% 
  dplyr::mutate(when = paste0(date, "-", time))

# Write to file
readr::write_tsv(combined_df, file.path(cleaned_data_dir,
                                        paste0("turnip_report_data_", current_date, ".tsv")))

# Make a summary report about the variant caller and strategy
output_file_1 <- file.path(reports_dir,
                         paste0("turnip_report_", current_date,"_report.Rmd"))

# Make a summary report about the variant caller and strategy
output_file_2 <- file.path(root_dir,
                           paste0("turnip_report_current_report.Rmd"))

# Path to the template file
template_file <- file.path(root_dir,
                           "template",
                           "template_report.Rmd")

# Make copy of template
if (file.exists(template_file)) {
  file.copy(from = template_file, to = output_file_1, overwrite = TRUE)
  file.copy(from = template_file, to = output_file_2, overwrite = TRUE)
} else {
  stop(cat("The Rmd template file ", template_file, " does not exist."))
}

# Run this notebook
rmarkdown::render(output_file_1, "html_document")
rmarkdown::render(output_file_2, "html_document")

# Push to online
setwd(root_dir)
system("git config --global user.email 'cansav09@gmail.com' \n
git config --global user.name 'cansavvy'")
system(paste0("git add", root_dir,"turnip_report_current_report.html \n git commit -m 'automatic update'"))
system("git status")
system("git push")
