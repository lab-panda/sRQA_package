import os
import shutil
import pandas as pd
from pathlib import Path
from collections import defaultdict

# This is to copy pulled cpac files from the cloned directory 
# I am only pulling files where subjects have both rest and movie conditions

def copy_1D_files():
    who = pd.read_csv("C:/Users/ac5867/Desktop/Projects/movie_vs_rest/pull_cpac/nt_folders.csv")
    subjects = who["folder"]
    source_dir = "C:/Users/ac5867/HBN_CPAC"
    destination_dir = "C:/Users/ac5867/Desktop/Projects/movie_vs_rest/Files/paired"
    
    Path(destination_dir).mkdir(parents=True, exist_ok=True)
    subject_files = defaultdict(list)
    
    # Find all qualifying files
    for root, dirs, files in os.walk(source_dir):
        current_subject = None
        for subject in subjects:
            if subject in root:
                current_subject = subject
                break
        
        if current_subject:
            for file in files:
                if (file.endswith('.1D') and 
                    ('rest_run-1' in file.lower() or 'moviedm' in file.lower()) and 
                    'schaefer2018p200n17' in file.lower() and
                    '36parameter' in file.lower() and
                    'acq-variant' not in file.lower()):
                    
                    source_file = os.path.join(root, file)
                    subject_files[current_subject].append((source_file, file))
    
    # Copy only subjects with both file types
    for subject, files_list in subject_files.items():
        has_rest = any('rest_run-1' in file[1].lower() for file in files_list)
        has_movie = any('moviedm' in file[1].lower() for file in files_list)
        
        if has_rest and has_movie:
            for source_file, filename in files_list:
                dest_file = os.path.join(destination_dir, filename)
                shutil.copy2(source_file, dest_file)

if __name__ == "__main__":
    copy_1D_files()