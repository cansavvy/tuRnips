# C. Savonen

# Turnip summaries

# Establish base dir
root_dir <- rprojroot::find_root(rprojroot::has_dir(".git"))

# Name the data directory
data_dir <- file.path(root_dir, "data")

# Create the data directory if it doesn't exist
if(!dir.exists(data_dir)){
  dir.create(data_dir)
}

# Retrieve currrent day and time
current_date <- Sys.Date()
current_day_of_week <- weekdays(current_date)
current_time <- Sys.time()

# Name turnip file
turnip_file <- file.path(root_dir, paste0(current_date, "-turnips.xlsx"))

# Download turnip data from Googlesheets
system(paste("wget -O", turnip_file,  
       "'https://docs.google.com/spreadsheets/d/1RTOuglfnwqMQzZ7BoTDMOTfr4rjVbksv85RwZB6YM74/export?format=xlsx&id=1RTOuglfnwqMQzZ7BoTDMOTfr4rjVbksv85RwZB6YM74'"))

# Magrittr pipe
`%>%` <- dplyr::`%>%`

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
  tidyr::gather("time", "price", -date) %>% 
  dplyr::mutate(prediction = "reported")
  
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
  colnames(turnip_pred )[1:3] <- c("day", "time", "minmax")
    
  # Reformat Day column so there isn't NAs
  turnip_pred$day <- rep(unique(turnip_pred$day[!is.na(turnip_pred $day)]), 
                           each = 4)
  
  # Reformat Time column so there isn't NAs
  turnip_pred$time <- rep(c("AM", "AM", "PM", "PM"), 6)
  
  # Make into long format
  turnip_pred <- turnip_pred %>% 
    tidyr::gather("prediction", "price", -day, -time, -minmax) %>% 
    dplyr::mutate(price = as.numeric(price), 
                  prediction = gsub("V", "prediction_"))
  
  return(list(reported = reported_prices, 
              predicted = turnip_pred))
})

# Separate into two lists
reported_prices <- purrr::map(all_turnip_prices, "reported")
predicted_prices <- purrr::map(all_turnip_prices, "predicted")

# Simplfy the names for these lists
simplified_names <- gsub(" |'s|Island", "", sheet_names[sheet_indices])

# Name the islands
names(reported_prices) <- simplified_names
names(predicted_prices) <- simplified_names

# Make into one data.frame
reported_df <- dplyr::bind_rows(reported_prices, .id = "owner")
predicted_df <- dplyr::bind_rows(predicted_prices, .id = "owner")

combined_df <- dplyr::bind_rows(reported_df, predicted_df)


ggplot2::ggplot(reported_df, )



# Run this notebook
rmarkdown::render(output_file, "html_document")
