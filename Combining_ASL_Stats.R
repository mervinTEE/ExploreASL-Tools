# Load necessary library
library(dplyr)

# Define directory containing the tsv files
dir <- "/home/admin/Desktop/Analysis/NEURO-ASH/Data/Raw"

# Define file patterns for CBF, ATT, and Tex
patterns <- list(
        CBF = "mean_qCBF_StandardSpace_.*\\.tsv",
        ATT = "mean_ATT_StandardSpace_.*\\.tsv",
        Tex = "mean_Tex_StandardSpace_.*\\.tsv"
)

# Define sets to process
sets <- c("Hammers", "MNI", "TotalGM", "DeepWM")

# Define columns to exclude from renaming
exclude_columns <- c("LongitudinalTimePoint", "SubjectNList", "Site", "GM_vol", "WM_vol", "CSF_vol", 
                     "GM_ICVRatio", "GMWM_ICVRatio", "WMH_vol", "WMH_count", "MeanMotion", 
                     "participant_id", "session")

# Function to process files for a given modality (CBF, ATT, Tex)
process_files <- function(pattern, modality) {
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
                
                # Read the files
                set_pvc0_df <- read.delim(set_pvc0)
                set_pvc2_df <- read.delim(set_pvc2)
                
                # Add prefixes and suffixes to the columns (skip prefix for TotalGM and DeepWM)
                if (set %in% c("Hammers", "MNI")) {
                        set_pvc0_df <- set_pvc0_df %>%
                                rename_with(~ paste0(tolower(set), "_", ., "_pvc0"), 
                                            .cols = setdiff(names(set_pvc0_df), exclude_columns))
                        set_pvc2_df <- set_pvc2_df %>%
                                rename_with(~ paste0(tolower(set), "_", ., "_pvc2"), 
                                            .cols = setdiff(names(set_pvc2_df), exclude_columns))
                } else {
                        # Only add suffix for TotalGM and DeepWM
                        set_pvc0_df <- set_pvc0_df %>%
                                rename_with(~ paste0(., "_pvc0"), .cols = setdiff(names(set_pvc0_df), exclude_columns))
                        set_pvc2_df <- set_pvc2_df %>%
                                rename_with(~ paste0(., "_pvc2"), .cols = setdiff(names(set_pvc2_df), exclude_columns))
                }
                
                # Append the processed dataframes to the lists
                combined_pvc0_list[[set]] <- set_pvc0_df
                combined_pvc2_list[[set]] <- set_pvc2_df
        }
        
        # Combine all sets for PVC0 and PVC2
        combined_pvc0 <- Reduce(function(x, y) inner_join(x, y, by = exclude_columns), combined_pvc0_list)
        combined_pvc2 <- Reduce(function(x, y) inner_join(x, y, by = exclude_columns), combined_pvc2_list)
        
        # Add suffix to all columns in the combined tables (except excluded columns)
        combined_pvc0 <- combined_pvc0 %>%
                rename_with(~ paste0(modality, "_", .), .cols = setdiff(names(combined_pvc0), exclude_columns)) %>%
                rename_with(tolower)  # Convert all column names to lowercase
        combined_pvc2 <- combined_pvc2 %>%
                rename_with(~ paste0(modality, "_", .), .cols = setdiff(names(combined_pvc2), exclude_columns)) %>%
                rename_with(tolower)  # Convert all column names to lowercase
        
        # Save the combined dataframes to new TSV files
        write.table(combined_pvc0, file = file.path(dir, paste0("combined_", modality, "_pvc0.tsv")), 
                    sep = "\t", row.names = FALSE, quote = FALSE)
        write.table(combined_pvc2, file = file.path(dir, paste0("combined_", modality, "_pvc2.tsv")), 
                    sep = "\t", row.names = FALSE, quote = FALSE)
}

# Process each modality (CBF, ATT, Tex)
for (modality in names(patterns)) {
        process_files(patterns[[modality]], modality)
}