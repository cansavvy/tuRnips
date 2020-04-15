# C. Savonen

# Turnip summaries

# Establish base dir
root_dir <- rprojroot::find_root(rprojroot::has_dir(".git"))

# Name turnip file
turnip_file <- file.path(root_dir, "turnips.xlsx")

# Download turnip data from Googlesheets
system(paste("wget -O", turnip_file,  
       "'https://docs.google.com/spreadsheets/d/1RTOuglfnwqMQzZ7BoTDMOTfr4rjVbksv85RwZB6YM74/export?format=xlsx&id=1RTOuglfnwqMQzZ7BoTDMOTfr4rjVbksv85RwZB6YM74'"))



current_date <- Sys.Date()
current_day_of_week <- weekdays(current_date)
current_time <- Sys.time()

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
all_sheets <- lapply(sheet_indices, function(sheet_num) {
  # Obtain preview of next sheet
  preview <- readxl::read_excel(turnip_file, 
                                sheet = sheet_num, 
                                col_names = FALSE)
  
  # Skip these rows of nonsense
  skip_till <- which(preview$...1 == "Pattern")
  
  # Read in data for real
  turnip_prices <- readxl::read_excel(turnip_file, 
                     sheet = sheet_num, 
                     col_names = FALSE, 
                     skip = skip_till) %>% 
    t()
  
  # Remove first row and turn into a data.frame
  turnip_prices <- turnip_prices[-1, ] %>% 
    as.data.frame(stringsAsFactors = FALSE)
  
  # Name columns we need
  colnames(turnip_prices)[1:3] <- c("day", "time", "minmax")
    
  # Reformat Day column so there isn't NAs
  turnip_prices$Day <- rep(unique(turnip_prices$Day[!is.na(turnip_prices$Day)]), 
                           each = 4)
  
  # Reformat Time column so there isn't NAs
  turnip_prices$Time <- rep(c("AM", "AM", "PM", "PM"), 6)
  
  # Make into long format
  turnip_prices %>% 
    tidyr::gather("prediction", "price", -day, -time, -minmax)
  
})

names(all_sheets) <- sheet_names[sheet_indices]



# Run this notebook
rmarkdown::render(output_file, "html_document")
