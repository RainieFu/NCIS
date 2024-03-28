income_per_capita_org <- read_csv("/Users/rainiefu/Downloads/P_Data_Extract_From_World_Development_Indicators (1)/e34e2b2a-c915-43de-acd1-40efb026ef33_Data.csv")

new_row <- income_per_capita_org %>%
  filter(`Country Code` %in% c("AFE", "AFW")) %>%
  group_by(`Series Code`) %>%
  summarise(across(`2015 [YR2015]`:`2021 [YR2021]`, mean, na.rm = TRUE)) %>%
  mutate(`Country Name` = "Africa") %>%
  select(`Country Name`, everything())

# Append the new row to the original data
# income_per_capita <- bind_rows(income_per_capita_org, new_row)

library(ggplot2)

# Assuming 'new_row' is your tibble
# Replace 'Your Series Code' with the actual value you want to create a time series plot for

# Melt the data for plotting
melted_data <- tidyr::gather(new_row, key = "Year", value = "Value", -c(`Country Name`, `Series Code`))

# Convert Year to numeric (removing '[YR' and ']' from the column names)
melted_data$Year <- as.numeric(gsub("\\D", "", melted_data$Year))

# Create the time series plot
ggplot(melted_data, aes(x = Year, y = Value, group = `Series Code`, color = `Series Code`)) +
  geom_line() +
  labs(title = paste("Time Series Plot for", new_row$`Country Name`, "-", new_row$`Series Code`),
       x = "Year",
       y = "Value") +
  theme_minimal()

#####################NEW ONE########################
library(readxl)
library(dplyr)
library(tidyverse)

# start reading in data
drug1 <- read_excel("/Users/rainiefu/Downloads/NCIS_drugprice_euro/PPP-02-1-1-1-3.xlsx")
drug1 <- drug1[-(1:2), ]
colnames(drug1) <- drug1[1, ]
drug1 <- drug1[-1, ]
drug1 <- drug1[, colnames(drug1) %in% c("Country", "2019", "2020", "2021")]

drug2 <- read_excel("/Users/rainiefu/Downloads/NCIS_drugprice_euro/PPP-02-1-1-2-3.xlsx")
drug2 <- drug2[-(1:2), ]
colnames(drug2) <- drug2[1, ]
drug2 <- drug2[-1, ]
drug2 <- drug2[, colnames(drug2) %in% c("Country", "2019", "2020", "2021")]

drug3 <- read_excel("/Users/rainiefu/Downloads/NCIS_drugprice_euro/PPP-02-1-1-3-3.xlsx")
drug3 <- drug3[-(1:2), ]
colnames(drug3) <- drug3[1, ]
drug3 <- drug3[-1, ]
drug3 <- drug3[, colnames(drug3) %in% c("Country", "2019", "2020", "2021")]

drug4 <- read_excel("/Users/rainiefu/Downloads/NCIS_drugprice_euro/PPP-02-1-1-4-3.xlsx")
drug4 <- drug4[-(1:2), ]
colnames(drug4) <- drug4[1, ]
drug4 <- drug4[-1, ]
drug4 <- drug4[, colnames(drug4) %in% c("Country", "2019", "2020", "2021")]

drug5 <- read_excel("/Users/rainiefu/Downloads/NCIS_drugprice_euro/PPP-02-1-1-5-3.xlsx")
drug5 <- drug5[-(1:2), ]
colnames(drug5) <- drug5[1, ]
drug5 <- drug5[-1, ]
drug5 <- drug5[, colnames(drug5) %in% c("Country", "2019", "2020", "2021")]

drug6 <- read_excel("/Users/rainiefu/Downloads/NCIS_drugprice_euro/PPP-02-1-1-6-3.xlsx")
drug6 <- drug6[-(1:2), ]
colnames(drug6) <- drug6[1, ]
drug6 <- drug6[-1, ]
drug6 <- drug6[, colnames(drug6) %in% c("Country", "2019", "2020", "2021")]

drug7 <- read_excel("/Users/rainiefu/Downloads/NCIS_drugprice_euro/PPP-02-1-1-7-3.xlsx")
drug7 <- drug7[-(1:2), ]
colnames(drug7) <- drug7[1, ]
drug7 <- drug7[-1, ]
drug7 <- drug7[, colnames(drug7) %in% c("Country", "2019", "2020", "2021")]

drug8 <- read_excel("/Users/rainiefu/Downloads/NCIS_drugprice_euro/PPP-02-1-1-8-3.xlsx")
drug8 <- drug8[-(1:2), ]
colnames(drug8) <- drug8[1, ]
drug8 <- drug8[-1, ]
drug8 <- drug8[, colnames(drug8) %in% c("Country", "2019", "2020", "2021")]

drug9 <- read_excel("/Users/rainiefu/Downloads/NCIS_drugprice_euro/PPP-02-1-1-9-3.xlsx")
drug9 <- drug9[-(1:2), ]
colnames(drug9) <- drug9[1, ]
drug9 <- drug9[-1, ]
drug9 <- drug9[, colnames(drug9) %in% c("Country", "2019", "2020", "2021")]

drug10 <- read_excel("/Users/rainiefu/Downloads/NCIS_drugprice_euro/PPP-02-1-1-10-3.xlsx")
drug10 <- drug10[-(1:2), ]
colnames(drug10) <- drug10[1, ]
drug10 <- drug10[-1, ]
drug10 <- drug10[, colnames(drug10) %in% c("Country", "2019", "2020", "2021")]

###############  Function to modify the column names 2019 2020 2021 ############### 
rename_columns <- function(table_name) {
  # Assuming your data frame is stored as a variable with the same name as the table
  your_data <- get(table_name)
  
  # Extract the drug number from the table name
  drug_number <- gsub("drug", "", table_name)
  
  # Rename columns
  names(your_data)[names(your_data) == "2021"] <- paste0("2021_drug", drug_number)
  names(your_data)[names(your_data) == "2020"] <- paste0("2020_drug", drug_number)
  names(your_data)[names(your_data) == "2019"] <- paste0("2019_drug", drug_number)
  
  # Update the data frame in the global environment
  assign(table_name, your_data, envir = .GlobalEnv)
}

rename_columns("drug1")
rename_columns("drug2")
rename_columns("drug3")
rename_columns("drug4")
rename_columns("drug5")
rename_columns("drug6")
rename_columns("drug7")
rename_columns("drug8")
rename_columns("drug9")
rename_columns("drug10")

###############  Function to join the tables ###############  
inner_join_tables <- function(table_list) {
  # Ensure that the input list is not empty
  if (length(table_list) == 0) {
    stop("Input list is empty.")
  }
  
  # Initial table to start the iteration
  result_table <- table_list[[1]]
  
  # Iterate through the rest of the tables
  for (i in 2:length(table_list)) {
    # Perform inner join on the "Country" column
    result_table <- inner_join(result_table, table_list[[i]], by = "Country")
  }
  
  return(result_table)
}

table_list <- list(drug1, drug2, drug3, drug4, drug5, drug6, drug7, drug8, drug9, drug10)
result_combined <- inner_join_tables(table_list)

# Remove rows with NA in every columns
result_combined <- result_combined[rowSums(is.na(result_combined)) < ncol(result_combined)-1, ]

# Summarize drug price for each year 2021 2020 2019
result_combined$overall_2021 <- rowMeans(result_combined[, grepl("2021", colnames(result_combined))], na.rm = TRUE)
result_combined$overall_2020 <- rowMeans(result_combined[, grepl("2020", colnames(result_combined))], na.rm = TRUE)
result_combined$overall_2019 <- rowMeans(result_combined[, grepl("2019", colnames(result_combined))], na.rm = TRUE)


load_and_combine_tables <- function(folder_path) {
  # Get a list of all .xlsx files in the specified folder
  file_list <- list.files(folder_path, pattern = "\\.xlsx", full.names = TRUE)
  
  # Initialize an empty data frame to store the combined data
  combined_data <- data.frame()
  
  # Loop through each file in the list
  for (file_path in file_list) {
    # Read the Excel file
    current_data <- read_excel(file_path)
    
    # Perform necessary selection steps
    current_data <- current_data[-(1:2), ]
    colnames(current_data) <- current_data[1, ]
    current_data <- current_data[-1, ]
    current_data <- current_data[, colnames(current_data) %in% c("Country", "2019", "2020", "2021")]
    
    # Extract drug names from the file name
    drug_name <- sub(".*/(.*?)\\.xlsx", "\\1", file_path)
    
    # Add drug-specific prefix to column names
    colnames(current_data) <- paste0(drug_name, "_", colnames(current_data))
    
    # Combine the current data with the existing combined data
    if (nrow(combined_data) == 0) {
      combined_data <- current_data
    } else {
      # combined_data <- cbind(combined_data, current_data)
      combined_data <- inner_join(combined_data, current_data, by = "Country")
    }
  }
  
  return(combined_data)
}

# Usage example:
folder_path <- "/Users/rainiefu/Downloads/NCIS_drugprice_euro/"
result_table <- load_and_combine_tables(folder_path)

