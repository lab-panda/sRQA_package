###################################################################################
# PART I  - Import .1D files and concatenate
###################################################################################
import pandas as pd
import glob
import os

###################################################################################
# Directory to files
directory_path = "D:/Projects/hbn/movie_vs_rest/Files/paired"

# Import and concatenate       
all_dataframes = []
for file_path in glob.glob(os.path.join(directory_path, "*.1D")):
    data = pd.read_csv(file_path, header=None, comment='#')
    data.insert(0, 'filename', os.path.basename(file_path))  # Keeps filename!
    all_dataframes.append(data)
combined_data = pd.concat(all_dataframes, ignore_index=True)      

###################################################################################
# PART 2- Add atlas labels and appropriate variables
###################################################################################
# Re-label with Schaefer atlas labels
from nilearn import datasets

# Fetch the Schaefer atlas
schaefer_atlas = datasets.fetch_atlas_schaefer_2018(n_rois=200, yeo_networks=17, resolution_mm=2)

# Extract labels and convert from bytes to string
labels = schaefer_atlas.labels
combined_data.columns = ['filename'] + labels[1:]

# Extract participant ID from filename
# This will extract everything between 'sub-' and the first '_'
combined_data['participant_id'] = combined_data['filename'].str.extract(r'sub-([^_]+)')
combined_data['participant_id'].nunique()
# Extract task information from filename
combined_data['task'] = combined_data['filename'].str.extract(r'task-(rest|movieDM)')

# Save to .csv
combined_data.to_csv("D:/Projects/hbn/movie_vs_rest/Files/movie_vs_rest_scans_111725.csv", index=False)