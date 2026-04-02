#%%
import numpy as np
import matplotlib.pyplot as plt
from nilearn import datasets, plotting, image
from nilearn.image import load_img, new_img_like
import nibabel as nib

# Load the Schaefer atlas (200 parcels, 17 networks)
schaefer_atlas = datasets.fetch_atlas_schaefer_2018(
    n_rois=200, 
    yeo_networks=17,
    resolution_mm=2
)

# Load the atlas image
atlas_img = load_img(schaefer_atlas.maps)
atlas_data = atlas_img.get_fdata()

# Get network labels
network_labels = schaefer_atlas.labels

#####################################################################
# DAN (A)
#####################################################################
dan_a_indices = []
dan_a_labels = []

# Identifying Dorsal Attention Network A regions:
for i, label in enumerate(network_labels):
    # Handle both string and bytes labels
    if isinstance(label, bytes):
        label_str = label.decode('utf-8')
    else:
        label_str = str(label)
    
    # Search for DorsAttnA (not DorsAttnB)
    if 'DorsAttnA' in label_str and 'DorsAttnB' not in label_str:
        dan_a_indices.append(i + 1)  # Atlas indices start from 1
        dan_a_labels.append(label_str)
        print(f"Region {i+1:3d}: {label_str}")

# Create a mask for DAN-A regions only
dan_a_mask = np.zeros_like(atlas_data)
for dan_idx in dan_a_indices:
    dan_a_mask[atlas_data == dan_idx] = dan_idx

# Create a new image with only DAN-A regions
dan_a_img = new_img_like(atlas_img, dan_a_mask)

# Create glass brain visualization of DAN-A only
plt.figure(figsize=(16, 10))

dan_a_fig = plotting.plot_glass_brain(
    dan_a_img,
    display_mode='xz',  # Sagittal and axial views
    colorbar=False,
    cmap='green_transparent',
    threshold=0.5,
    title='Dorsal Attention Network A',
    plot_abs=False,
    vmin=0.5,
    vmax=1.5,
    black_bg=False
)

plt.tight_layout()
plt.show()

# Don't need dpi for svg, but kept it for other formats
dan_a_fig.savefig('D:/Projects/hbn/movie_vs_rest/figs/dan_a_glassbrain.pdf', dpi=300)

#####################################################################
# DAN (B)
#####################################################################
dan_b_indices = []
dan_b_labels = []

# Identifying Dorsal Attention Network B regions:
for i, label in enumerate(network_labels):
    # Handle both string and bytes labels
    if isinstance(label, bytes):
        label_str = label.decode('utf-8')
    else:
        label_str = str(label)
    
    # Search for DorsAttnB
    if 'DorsAttnB' in label_str:
        dan_b_indices.append(i + 1)  # Atlas indices start from 1
        dan_b_labels.append(label_str)
        print(f"Region {i+1:3d}: {label_str}")

# Create a mask for DAN-B regions only
dan_b_mask = np.zeros_like(atlas_data)
for dan_idx in dan_b_indices:
    dan_b_mask[atlas_data == dan_idx] = dan_idx

# Create a new image with only DAN-B regions
dan_b_img = new_img_like(atlas_img, dan_b_mask)

# Create glass brain visualization of DAN-B only
plt.figure(figsize=(16, 10))

dan_b_fig = plotting.plot_glass_brain(
    dan_b_img,
    display_mode='xz',  # left, right, top, front views
    colorbar=False,
    cmap='blue_transparent',
    threshold=0.5,
    title='Dorsal Attention Network B',
    plot_abs=False,
    vmin=0.5,
    vmax=1.5,
    black_bg=False
)

plt.tight_layout()
plt.show()

dan_b_fig.savefig('D:/Projects/hbn/movie_vs_rest/figs/dan_b_glassbrain.pdf', dpi=300)