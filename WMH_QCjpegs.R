# Load necessary libraries
library(oro.nifti)
library(ggplot2)
library(gridExtra)
library(grid)
library(png)  # for saving via ggsave
library(RColorBrewer)
library(dplyr)
library(magick)  # for rotating images

# Define function to generate WMH QC JPEG
generate_wmh_qc_jpeg <- function(flair_path, wmh_path, output_path, num_slices = 16) {
        
        # Load NIfTI files
        flair_nii <- readNIfTI(flair_path, reorient = FALSE)
        wmh_nii <- readNIfTI(wmh_path, reorient = FALSE)
        
        # Check dimensions match
        if (!all(dim(flair_nii) == dim(wmh_nii))) {
                stop("FLAIR and WMH volumes must have the same dimensions.")
        }
        
        # Determine slices
        z_slices <- round(seq(1, dim(flair_nii)[3], length.out = num_slices))
        plots <- list()
        
        # Loop over slices and create plots
        for (i in seq_along(z_slices)) {
                z <- z_slices[i]
                flair_slice <- t(flair_nii[,,z])
                wmh_slice <- t(wmh_nii[,,z])
                
                # Prepare data for ggplot
                df <- expand.grid(x = 1:nrow(flair_slice), y = 1:ncol(flair_slice))
                df$flair <- as.vector(flair_slice)
                df$wmh <- as.vector(wmh_slice)
                
                # Create plot
                p <- ggplot(df, aes(x, y)) +
                        geom_raster(aes(fill = flair)) +
                        scale_fill_gradient(low = "black", high = "white") +
                        new_scale("fill") +
                        geom_raster(data = subset(df, wmh > 0), aes(fill = wmh), alpha = 0.9) +
                        scale_fill_gradient(low = NA, high = "red", na.value = NA) +
                        coord_fixed() +
                        theme_void() +
                        theme(legend.position = "none")
                
                # Save plot to list
                plots[[i]] <- p
        }
        
        # Save as JPEG
        jpeg(output_path, width = 2000, height = 2000, quality = 90, res = 150)
        grid.arrange(grobs = plots, ncol = 4)
        dev.off()
        
        message("Saved: ", output_path)
}

# File paths for FLAIR and WMH NIfTI images
flair_path <- "/mnt/hdd/MT/HARMY/HARMY_WMH/derivatives/LSTAI_Y2/sub-HD001/temp/FLAIR.nii.gz"
wmh_path <- "/mnt/hdd/MT/HARMY/HARMY_WMH/derivatives/LSTAI_Y2/sub-HD001/temp/WMH.nii.gz"
output_path <- "/home/admin/Downloads/wmh_qc_jpeg.jpg"

# Generate WMH QC JPEG
generate_wmh_qc_jpeg(flair_path, wmh_path, output_path, num_slices = 20)

# Rotate the saved image 90 degrees clockwise using magick
image <- image_read(output_path)
image_rotated <- image_rotate(image, 90)

# Save the rotated image
image_write(image_rotated, "/home/admin/Downloads/wmh_qc_jpeg_rotated.jpg")

# Optionally view the rotated image
image_rotated %>% print()



###### Looping Below ###########

# Define the root directory containing the subject data
root_dir <- "/mnt/hdd/MT/HARMY/HARMY_WMH/derivatives/LSTAI_Y2"

# Define the output QC folder
output_folder <- "/mnt/hdd/MT/HARMY/HARMY_WMH/derivatives/LSTAI_Y2/QC"

# List all subject directories in the root directory
subject_dirs <- list.dirs(root_dir, recursive = FALSE)

# Loop through each subject directory
for (subject_dir in subject_dirs) {
        
        # Check if the directory contains "sub-" (indicating it's a subject folder)
        if (grepl("sub-", subject_dir)) {
                
                # Extract subject ID (sub-HDxxx)
                subid <- basename(subject_dir)
                
                # Define paths to FLAIR and WMH NIfTI files
                flair_path <- file.path(subject_dir, "temp", paste0("sub-X_ses-Y_space-mni_desc-stripped_FLAIR.nii.gz"))
                wmh_path <- file.path(subject_dir, "temp", paste0("sub-X_ses-Y_space-mni_seg-lst.nii.gz"))
                
                # Define the output JPEG file path with subject ID
                output_jpeg <- file.path(output_folder, paste0(subid, "_wmh_qc.jpg"))
                
                # Check if both FLAIR and WMH files exist before proceeding
                if (file.exists(flair_path) && file.exists(wmh_path)) {
                        
                        # Generate and save the WMH QC JPEG
                        generate_wmh_qc_jpeg(flair_path, wmh_path, output_jpeg, num_slices = 25)
                        
                        # Optionally, rotate the saved image 90 degrees clockwise
                        image <- image_read(output_jpeg)
                        image_rotated <- image_rotate(image, 90)
                        
                        # Save the rotated image
                        rotated_output_jpeg <- file.path(output_folder, paste0(subid, "_wmh_qc_rotated.jpg"))
                        image_write(image_rotated, rotated_output_jpeg)
                        
                        # Optional: View the rotated image
                        #image_rotated %>% print()
                } else {
                        message("Missing files for subject: ", subid)
                }
        }
}

message("Processing complete.")
