# Load necessary library
library(dplyr)

# Define directory containing the tsv files
dir <- "/home/admin/Desktop/Analysis/harmyASL/Data/Raw/asl/volumes/Y4Y5/"

# Define output directory
output_dir <- "/home/admin/Desktop/Analysis/harmyASL/Data/Processed/ASL/Y4Y5"  # Replace this with your desired output directory


# Define file patterns for CBF, ATT, and Tex
patterns <- list(
        CBF = "mean_qCBF_StandardSpace_.*\\.tsv",
        #ATT = "mean_ATT_StandardSpace_.*\\.tsv",
        #Tex = "mean_Tex_StandardSpace_.*\\.tsv",
        scov = "CoV_qCBF_StandardSpace_.*\\.tsv"
        
)

# Define sets to process "Hammers"
sets <- c("Tatu", "TotalGM", "DeepWM")

# Define columns to exclude from renaming
exclude_columns <- c("LongitudinalTimePoint", "SubjectNList", "Site", "GM_vol", "WM_vol", "CSF_vol", 
                     "GM_ICVRatio", "GMWM_ICVRatio", "WMH_vol", "WMH_count", "MeanMotion", 
                     "participant_id", "session")

# Function to process files for a given modality (CBF, ATT, Tex)
process_files <- function(pattern, modality, output_dir) {
        # Create the output directory if it doesn't exist
        if (!dir.exists(output_dir)) {
                dir.create(output_dir, recursive = TRUE)  # Create the directory, including any parent directories if necessary
        }
        
        # List all files matching the pattern
        files <- list.files(dir, pattern = pattern, full.names = TRUE)
        
        # Initialize empty lists to store combined data for PVC0 and PVC2
        combined_pvc0_list <- list()
        combined_pvc2_list <- list()
        
        # Loop through each set (Hammers, MNI, TotalGM, DeepWM)
        for (set in sets) {
                # Filter files for the current set and PVC type
                set_pvc0 <- grep(paste0(set, ".*PVC0"), files, value = TRUE)
                set_pvc2 <- grep(paste0(set, ".*PVC2"), files, value = TRUE)
                
                # If PVC0 files are found, read them
                if (length(set_pvc0) > 0) {
                        set_pvc0_df <- read.delim(set_pvc0)
                        
                        # Add prefixes and suffixes to the columns (skip prefix for TotalGM and DeepWM)
                        if (set %in% c("Hammers", "MNI")) {
                                set_pvc0_df <- set_pvc0_df %>%
                                        rename_with(~ paste0(tolower(set), "_", ., "_pvc0"), 
                                                    .cols = setdiff(names(set_pvc0_df), exclude_columns))
                        } else {
                                set_pvc0_df <- set_pvc0_df %>%
                                        rename_with(~ paste0(., "_pvc0"), .cols = setdiff(names(set_pvc0_df), exclude_columns))
                        }
                        # Append the processed PVC0 dataframe to the list
                        combined_pvc0_list[[set]] <- set_pvc0_df
                }
                
                # If PVC2 files are found, read them
                if (length(set_pvc2) > 0) {
                        set_pvc2_df <- read.delim(set_pvc2)
                        
                        # Add prefixes and suffixes to the columns (skip prefix for TotalGM and DeepWM)
                        if (set %in% c("Hammers", "MNI")) {
                                set_pvc2_df <- set_pvc2_df %>%
                                        rename_with(~ paste0(tolower(set), "_", ., "_pvc2"), 
                                                    .cols = setdiff(names(set_pvc2_df), exclude_columns))
                        } else {
                                set_pvc2_df <- set_pvc2_df %>%
                                        rename_with(~ paste0(., "_pvc2"), .cols = setdiff(names(set_pvc2_df), exclude_columns))
                        }
                        # Append the processed PVC2 dataframe to the list
                        combined_pvc2_list[[set]] <- set_pvc2_df
                }
        }
        
        # If there were any PVC0 files, combine them
        if (length(combined_pvc0_list) > 0) {
                combined_pvc0 <- Reduce(function(x, y) inner_join(x, y, by = exclude_columns), combined_pvc0_list)
                combined_pvc0 <- combined_pvc0 %>%
                        rename_with(~ paste0(modality, "_", .), .cols = setdiff(names(combined_pvc0), exclude_columns)) %>%
                        rename_with(tolower)  # Convert all column names to lowercase
                # Save the combined PVC0 data to the output directory
                write.table(combined_pvc0, file = file.path(output_dir, paste0("combined_", modality, "_pvc0.tsv")), 
                            sep = "\t", row.names = FALSE, quote = FALSE)
        }
        
        # If there were any PVC2 files, combine them
        if (length(combined_pvc2_list) > 0) {
                combined_pvc2 <- Reduce(function(x, y) inner_join(x, y, by = exclude_columns), combined_pvc2_list)
                combined_pvc2 <- combined_pvc2 %>%
                        rename_with(~ paste0(modality, "_", .), .cols = setdiff(names(combined_pvc2), exclude_columns)) %>%
                        rename_with(tolower)  # Convert all column names to lowercase
                # Save the combined PVC2 data to the output directory
                write.table(combined_pvc2, file = file.path(output_dir, paste0("combined_", modality, "_pvc2.tsv")), 
                            sep = "\t", row.names = FALSE, quote = FALSE)
        }
}


# Process each modality (CBF, ATT, Tex)
for (modality in names(patterns)) {
        process_files(patterns[[modality]], modality, output_dir)
}

