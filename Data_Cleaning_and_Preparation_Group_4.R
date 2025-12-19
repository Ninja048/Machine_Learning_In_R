# Hello and welcome to our project! The goal of this file is to clean up the 
# data, format it, and filter out rows that we do not want to focus on.

# Those are our required packages (uncomment if not already installed and run once)

# install.packages(c("readxl", "dplyr", "stringr", "openxlsx"))

library(readxl)    
library(dplyr)     
library(stringr)   
library(openxlsx)  


# -------------------------------------------------------------------------------
# 1. Load the original dataset ("New_Dataset.xlsx") 
# you have to change this to the path where you saved the data
# -------------------------------------------------------------------------------

new_dataset_path        <- file.path("New_Dataset.xlsx")
cleaned_dataset_path    <- file.path("Cleaned_Dataset.xlsx")
rearranged_dataset_path <- file.path("Rearranged_Dataset.xlsx")
final_dataset_path      <- file.path("Final_Dataset_after_peerreview.xlsx")

# read the original Excel file
# read_excel() guesses types from first 1000 rows; they are NA, so we force all to text
data <- read_excel(
  new_dataset_path,
  col_types = "text"   # sets all columns to text
)

# -------------------------------------------------------------------------------
# 2. Remove columns that are completely empty (all NA)
# -------------------------------------------------------------------------------

# identify columns where ALL values are NA
empty_cols <- vapply(
  data,
  function(x) all(is.na(x)),
  logical(1)
)

# optional: see which columns are truly all NA (uncomment if you want to check)
# empty_cols[empty_cols]

# makes sense to keep only columns that are NOT completely NA
data_cleaned <- data[, !empty_cols]

# -------------------------------------------------------------------------------
# 3. drop rows with missing values in required columns
# -------------------------------------------------------------------------------

required_columns <- c(
  "Crop", "Sort", "pH", "EC dS/m", "Nutrient Solution", "Soil Type",
  "Light type", "I Relative humidity (RH) %", "Internal T°"
)

# lets filter out rows where any of the required columns is NA
data_cleaned_required <- data_cleaned %>%
  filter(if_all(all_of(required_columns), ~ !is.na(.)))

# -------------------------------------------------------------------------------
# 4. Remove "unimportant" columns. we dont look at those.
# -------------------------------------------------------------------------------

unimportant_columns <- c(
  "Comments", "Description", "NST°", "InsertLevel mm", "WaterLevel",
  "O2 Method", "O2 mg/L", "Amount", "Reason", "Amount Eject",
  "Diameter", "Height", "BBCH Code", "Phase", "Position in Cube"
)

# drop those columns if they exist
data_final <- data_cleaned_required %>%
  select(-any_of(unimportant_columns))

# -------------------------------------------------------------------------------
# 5. save the cleaned dataset ("Cleaned_Dataset.xlsx")
# -------------------------------------------------------------------------------

write.xlsx(data_final, cleaned_dataset_path, rowNames = FALSE)

# -------------------------------------------------------------------------------
# 6. reload cleaned dataset and sort by UUID and Code
# -------------------------------------------------------------------------------

data_cleaned <- read_excel(cleaned_dataset_path)

# sort rows by UUID and then by Code
data_rearranged <- data_cleaned %>%
  arrange(UUID, Code)

# save the rearranged dataset ("Rearranged_Dataset.xlsx")
write.xlsx(data_rearranged, rearranged_dataset_path, rowNames = FALSE)

# -------------------------------------------------------------------------------
# 7. load rearranged dataset and build feature vectors
# -------------------------------------------------------------------------------

original_data <- read_excel(rearranged_dataset_path)

# initialize a list to store all feature vectors (one entry per harvested plant)
all_feature_vectors <- list()
fv_index <- 1  # this is index for list insertion

# loop over each unique UUID
for (uuid in unique(original_data$UUID)) {
  
  # subset data for this UUID
  uuid_data <- original_data %>%
    filter(UUID == uuid)
  
  # harvested plants: rows where Weight is not NA
  harvested_plants <- uuid_data %>%
    filter(!is.na(Weight))
  
  # process each harvested plant (each row with a Weight)
  if (nrow(harvested_plants) > 0) {
    for (i in seq_len(nrow(harvested_plants))) {
      
      harvested_row <- harvested_plants[i, ]
      code <- harvested_row$Code
      
      # initialize feature vector as a named list
      feature_vector <- list(
        UUID   = uuid,
        Weight = harvested_row$Weight
      )
      
      # count number of hyphens in Code (to determine splitting level)
      splits <- str_count(code, "-")
      
      # -------------------------------------------------------------------------
      # 7.1 start readings (first entry in UUID lineage)
      # -------------------------------------------------------------------------
      
      start_readings <- uuid_data[1, c("pH", "EC dS/m", 
                                       "I Relative humidity (RH) %", 
                                       "Internal T°")]
      
      feature_vector[["pH1"]]                          <- start_readings[["pH"]]
      feature_vector[["EC dS/m1"]]                     <- start_readings[["EC dS/m"]]
      feature_vector[["I Relative humidity (RH) %1"]]  <- start_readings[["I Relative humidity (RH) %"]]
      feature_vector[["Internal T°1"]]                 <- start_readings[["Internal T°"]]
      
      # -------------------------------------------------------------------------
      # 7.2 end readings (from the harvested row itself)
      # -------------------------------------------------------------------------
      
      feature_vector[["pH6"]]                          <- harvested_row[["pH"]]
      feature_vector[["EC dS/m6"]]                     <- harvested_row[["EC dS/m"]]
      feature_vector[["I Relative humidity (RH) %6"]]  <- harvested_row[["I Relative humidity (RH) %"]]
      feature_vector[["Internal T°6"]]                 <- harvested_row[["Internal T°"]]
      
      # -------------------------------------------------------------------------
      # 7.3 intermediate readings based on split level
      # -------------------------------------------------------------------------
      
      if (splits == 0) {
        # case 0: No split. Use entries with Code == uuid, drop first and last,
        # then divide into 4 chunks and compute averages
        
        first_gen_data <- uuid_data %>%
          filter(Code == uuid)
        
        if (nrow(first_gen_data) > 2) {
          middle_data <- first_gen_data[2:(nrow(first_gen_data) - 1), ]
        } else {
          middle_data <- first_gen_data[0, ]
        }
        
        chunk_size <- ifelse(nrow(middle_data) > 0,
                             floor(nrow(middle_data) / 4),
                             0)
        
        if (chunk_size > 0) {
          for (j in 0:3) {
            start_row <- j * chunk_size + 1
            end_row   <- (j + 1) * chunk_size
            if (end_row <= nrow(middle_data)) {
              chunk <- middle_data[start_row:end_row, ]
              chunk_avg <- sapply(chunk[, c("pH", "EC dS/m", 
                                            "I Relative humidity (RH) %", 
                                            "Internal T°")],
                                  function(x) mean(as.numeric(x), na.rm = TRUE))
              
              k <- j + 2  # indices 2,3,4,5
              feature_vector[[paste0("pH", k)]]                         <- chunk_avg[["pH"]]
              feature_vector[[paste0("EC dS/m", k)]]                    <- chunk_avg[["EC dS/m"]]
              feature_vector[[paste0("I Relative humidity (RH) %", k)]] <- chunk_avg[["I Relative humidity (RH) %"]]
              feature_vector[[paste0("Internal T°", k)]]                <- chunk_avg[["Internal T°"]]
            }
          }
        }
        
      } else if (splits == 1) {
        # case 1: one split
        
        first_gen_data <- uuid_data %>%
          filter(Code == uuid)
        
        if (nrow(first_gen_data) > 0) {
          chunk_size <- floor(nrow(first_gen_data) / 2)
          
          # First half (pH2, etc.)
          if (chunk_size > 0) {
            chunk1 <- first_gen_data[1:chunk_size, ]
            avg1 <- sapply(chunk1[, c("pH", "EC dS/m", 
                                      "I Relative humidity (RH) %", 
                                      "Internal T°")],
                           function(x) mean(as.numeric(x), na.rm = TRUE))
            
            feature_vector[["pH2"]]                         <- avg1[["pH"]]
            feature_vector[["EC dS/m2"]]                    <- avg1[["EC dS/m"]]
            feature_vector[["I Relative humidity (RH) %2"]] <- avg1[["I Relative humidity (RH) %"]]
            feature_vector[["Internal T°2"]]                <- avg1[["Internal T°"]]
          }
          
          # second half (pH3 etc.)
          if (chunk_size < nrow(first_gen_data)) {
            chunk2 <- first_gen_data[(chunk_size + 1):nrow(first_gen_data), ]
            avg2 <- sapply(chunk2[, c("pH", "EC dS/m", 
                                      "I Relative humidity (RH) %", 
                                      "Internal T°")],
                           function(x) mean(as.numeric(x), na.rm = TRUE))
            
            feature_vector[["pH3"]]                         <- avg2[["pH"]]
            feature_vector[["EC dS/m3"]]                    <- avg2[["EC dS/m"]]
            feature_vector[["I Relative humidity (RH) %3"]] <- avg2[["I Relative humidity (RH) %"]]
            feature_vector[["Internal T°3"]]                <- avg2[["Internal T°"]]
          }
        }
        
        # determine which branch to use: -1 or -2
        if (grepl("-1", code, fixed = TRUE)) {
          second_gen_code <- paste0(uuid, "-1")
        } else if (grepl("-2", code, fixed = TRUE)) {
          second_gen_code <- paste0(uuid, "-2")
        } else {
          second_gen_code <- NA_character_
        }
        
        if (!is.na(second_gen_code)) {
          second_gen_data <- uuid_data %>%
            filter(Code == second_gen_code)
          
          if (nrow(second_gen_data) > 0) {
            chunk_size_second_gen <- floor(nrow(second_gen_data) / 2)
            
            # first half (pH4, etc.)
            if (chunk_size_second_gen > 0) {
              chunk3 <- second_gen_data[1:chunk_size_second_gen, ]
              avg3 <- sapply(chunk3[, c("pH", "EC dS/m", 
                                        "I Relative humidity (RH) %", 
                                        "Internal T°")],
                             function(x) mean(as.numeric(x), na.rm = TRUE))
              
              feature_vector[["pH4"]]                         <- avg3[["pH"]]
              feature_vector[["EC dS/m4"]]                    <- avg3[["EC dS/m"]]
              feature_vector[["I Relative humidity (RH) %4"]] <- avg3[["I Relative humidity (RH) %"]]
              feature_vector[["Internal T°4"]]                <- avg3[["Internal T°"]]
            }
            
            # second half (pH5, etc.)
            if (chunk_size_second_gen < nrow(second_gen_data)) {
              chunk4 <- second_gen_data[(chunk_size_second_gen + 1):nrow(second_gen_data), ]
              avg4 <- sapply(chunk4[, c("pH", "EC dS/m", 
                                        "I Relative humidity (RH) %", 
                                        "Internal T°")],
                             function(x) mean(as.numeric(x), na.rm = TRUE))
              
              feature_vector[["pH5"]]                         <- avg4[["pH"]]
              feature_vector[["EC dS/m5"]]                    <- avg4[["EC dS/m"]]
              feature_vector[["I Relative humidity (RH) %5"]] <- avg4[["I Relative humidity (RH) %"]]
              feature_vector[["Internal T°5"]]                <- avg4[["Internal T°"]]
            }
          }
        }
        
      } else if (splits == 2) {
        # case 2: two splits
        
        first_gen_data <- uuid_data %>%
          filter(Code == uuid)
        
        if (nrow(first_gen_data) > 0) {
          chunk_size <- floor(nrow(first_gen_data) / 2)
          
          if (chunk_size > 0) {
            chunk1 <- first_gen_data[1:chunk_size, ]
            avg1 <- sapply(chunk1[, c("pH", "EC dS/m", 
                                      "I Relative humidity (RH) %", 
                                      "Internal T°")],
                           function(x) mean(as.numeric(x), na.rm = TRUE))
            feature_vector[["pH2"]]                         <- avg1[["pH"]]
            feature_vector[["EC dS/m2"]]                    <- avg1[["EC dS/m"]]
            feature_vector[["I Relative humidity (RH) %2"]] <- avg1[["I Relative humidity (RH) %"]]
            feature_vector[["Internal T°2"]]                <- avg1[["Internal T°"]]
          }
          
          if (chunk_size < nrow(first_gen_data)) {
            chunk2 <- first_gen_data[(chunk_size + 1):nrow(first_gen_data), ]
            avg2 <- sapply(chunk2[, c("pH", "EC dS/m", 
                                      "I Relative humidity (RH) %", 
                                      "Internal T°")],
                           function(x) mean(as.numeric(x), na.rm = TRUE))
            feature_vector[["pH3"]]                         <- avg2[["pH"]]
            feature_vector[["EC dS/m3"]]                    <- avg2[["EC dS/m"]]
            feature_vector[["I Relative humidity (RH) %3"]] <- avg2[["I Relative humidity (RH) %"]]
            feature_vector[["Internal T°3"]]                <- avg2[["Internal T°"]]
          }
        }
        
        # second generation: take first 2 segments of code (e.g. UUID-1)
        segments <- strsplit(code, "-", fixed = TRUE)[[1]]
        second_gen_code <- paste(segments[1:2], collapse = "-")
        second_gen_data <- uuid_data %>%
          filter(Code == second_gen_code)
        
        if (nrow(second_gen_data) > 0) {
          avg_second <- sapply(second_gen_data[, c("pH", "EC dS/m",
                                                   "I Relative humidity (RH) %",
                                                   "Internal T°")],
                               function(x) mean(as.numeric(x), na.rm = TRUE))
          feature_vector[["pH4"]]                         <- avg_second[["pH"]]
          feature_vector[["EC dS/m4"]]                    <- avg_second[["EC dS/m"]]
          feature_vector[["I Relative humidity (RH) %4"]] <- avg_second[["I Relative humidity (RH) %"]]
          feature_vector[["Internal T°4"]]                <- avg_second[["Internal T°"]]
        }
        
        # third generation: first 3 segments of code
        third_gen_code <- paste(segments[1:3], collapse = "-")
        third_gen_data <- uuid_data %>%
          filter(Code == third_gen_code)
        
        if (nrow(third_gen_data) > 0) {
          avg_third <- sapply(third_gen_data[, c("pH", "EC dS/m",
                                                 "I Relative humidity (RH) %",
                                                 "Internal T°")],
                              function(x) mean(as.numeric(x), na.rm = TRUE))
          feature_vector[["pH5"]]                         <- avg_third[["pH"]]
          feature_vector[["EC dS/m5"]]                    <- avg_third[["EC dS/m"]]
          feature_vector[["I Relative humidity (RH) %5"]] <- avg_third[["I Relative humidity (RH) %"]]
          feature_vector[["Internal T°5"]]                <- avg_third[["Internal T°"]]
        }
        
      } else if (splits == 3) {
        # case 3: three splits
        
        # first generation (Code == uuid)
        first_gen_data <- uuid_data %>%
          filter(Code == uuid)
        
        if (nrow(first_gen_data) > 0) {
          avg_first <- sapply(first_gen_data[, c("pH", "EC dS/m",
                                                 "I Relative humidity (RH) %",
                                                 "Internal T°")],
                              function(x) mean(as.numeric(x), na.rm = TRUE))
          feature_vector[["pH2"]]                         <- avg_first[["pH"]]
          feature_vector[["EC dS/m2"]]                    <- avg_first[["EC dS/m"]]
          feature_vector[["I Relative humidity (RH) %2"]] <- avg_first[["I Relative humidity (RH) %"]]
          feature_vector[["Internal T°2"]]                <- avg_first[["Internal T°"]]
        }
        
        # second generation: first 2 segments
        segments <- strsplit(code, "-", fixed = TRUE)[[1]]
        second_gen_code <- paste(segments[1:2], collapse = "-")
        second_gen_data <- uuid_data %>%
          filter(Code == second_gen_code)
        
        if (nrow(second_gen_data) > 0) {
          avg_second <- sapply(second_gen_data[, c("pH", "EC dS/m",
                                                   "I Relative humidity (RH) %",
                                                   "Internal T°")],
                               function(x) mean(as.numeric(x), na.rm = TRUE))
          feature_vector[["pH3"]]                         <- avg_second[["pH"]]
          feature_vector[["EC dS/m3"]]                    <- avg_second[["EC dS/m"]]
          feature_vector[["I Relative humidity (RH) %3"]] <- avg_second[["I Relative humidity (RH) %"]]
          feature_vector[["Internal T°3"]]                <- avg_second[["Internal T°"]]
        }
        
        # third generation: first 3 segments
        third_gen_code <- paste(segments[1:3], collapse = "-")
        third_gen_data <- uuid_data %>%
          filter(Code == third_gen_code)
        
        if (nrow(third_gen_data) > 0) {
          avg_third <- sapply(third_gen_data[, c("pH", "EC dS/m",
                                                 "I Relative humidity (RH) %",
                                                 "Internal T°")],
                              function(x) mean(as.numeric(x), na.rm = TRUE))
          feature_vector[["pH4"]]                         <- avg_third[["pH"]]
          feature_vector[["EC dS/m4"]]                    <- avg_third[["EC dS/m"]]
          feature_vector[["I Relative humidity (RH) %4"]] <- avg_third[["I Relative humidity (RH) %"]]
          feature_vector[["Internal T°4"]]                <- avg_third[["Internal T°"]]
        }
        
        # fourth generation: full code
        fourth_gen_code <- code
        fourth_gen_data <- uuid_data %>%
          filter(Code == fourth_gen_code)
        
        if (nrow(fourth_gen_data) > 0) {
          avg_fourth <- sapply(fourth_gen_data[, c("pH", "EC dS/m",
                                                   "I Relative humidity (RH) %",
                                                   "Internal T°")],
                               function(x) mean(as.numeric(x), na.rm = TRUE))
          feature_vector[["pH5"]]                         <- avg_fourth[["pH"]]
          feature_vector[["EC dS/m5"]]                    <- avg_fourth[["EC dS/m"]]
          feature_vector[["I Relative humidity (RH) %5"]] <- avg_fourth[["I Relative humidity (RH) %"]]
          feature_vector[["Internal T°5"]]                <- avg_fourth[["Internal T°"]]
        }
      }
      
      # -------------------------------------------------------------------------
      # 7.4 add static values for this UUID (Crop, Sort, etc.)
      # -------------------------------------------------------------------------
      
      static_values <- uuid_data[1, c("Crop", "Sort", 
                                      "Nutrient Solution",
                                      "Soil Type",
                                      "Light type")]
      
      feature_vector[["Crop"]]              <- static_values[["Crop"]]
      feature_vector[["Sort"]]              <- static_values[["Sort"]]
      feature_vector[["Nutrient Solution"]] <- static_values[["Nutrient Solution"]]
      feature_vector[["Soil Type"]]         <- static_values[["Soil Type"]]
      feature_vector[["Light type"]]        <- static_values[["Light type"]]
      
      # -------------------------------------------------------------------------
      # 7.5 store this feature vector in the list
      # -------------------------------------------------------------------------
      
      all_feature_vectors[[fv_index]] <- feature_vector
      fv_index <- fv_index + 1
    }
  }
}

# -------------------------------------------------------------------------------
# 8. combine all feature vectors into a single data frame
# -------------------------------------------------------------------------------

new_table <- bind_rows(all_feature_vectors)

# -------------------------------------------------------------------------------
# 9. remove rows with specific UUIDs (inherent errors)
# those had some wild feature values that are the result of erroneous manual input
# -------------------------------------------------------------------------------

uuids_to_remove <- c("V00381", "V00382", "V00378",
                     "V00489", "V01003", "V01006")

new_table <- new_table %>%
  filter(!UUID %in% uuids_to_remove)

# -------------------------------------------------------------------------------
# 10. convert numeric-like columns to numeric ignoring certain columns
# -------------------------------------------------------------------------------

ignore_columns <- c("UUID", "Code", "Crop", "Sort",
                    "Nutrient Solution", "Soil Type", "Light type")

for (col_name in colnames(new_table)) {
  if (!(col_name %in% ignore_columns)) {
    new_table[[col_name]] <- new_table[[col_name]] %>%
      as.character() %>%
      gsub(",", ".", .) %>%
      as.numeric()
  }
}

# -------------------------------------------------------------------------------
# 11. save the final processed dataset ("Final_Dataset.xlsx")
# -------------------------------------------------------------------------------

write.xlsx(new_table, final_dataset_path, rowNames = FALSE)

# -------------------------------------------------------------------------------
# 12. reorder columns: move last readings before "Crop"
# -------------------------------------------------------------------------------

# load the dataset we just saved
data_reorder <- read_excel(final_dataset_path)

# columns to move
columns_last_readings <- c("pH6", "EC dS/m6",
                           "I Relative humidity (RH) %6",
                           "Internal T°6")

# current column order
cols <- names(data_reorder)

# remove the last-reading columns from their current positions
cols_no_last <- setdiff(cols, columns_last_readings)

# position of "Crop" in this reduced vector
crop_pos <- which(cols_no_last == "Crop")

# insert the last-reading columns *before* "Crop"
new_order <- append(cols_no_last, columns_last_readings, after = crop_pos - 1)

# reorder the data
data_reordered <- data_reorder[, new_order]

# overwrite the same Final_Dataset.xlsx with reordered columns
write.xlsx(data_reordered, final_dataset_path, rowNames = FALSE)

