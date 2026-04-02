## Data Source

R-R intervals were derived from the MIT-BIH Atrial Fibrillation Database:
https://physionet.org/content/afdb/1.0.0/

## Files

| File | Description |
|------|-------------|
| `batch_process_afib.py` | Extracts R-R intervals from the MIT-BIH AF database and saves them as labeled 200-beat windows |
| `windowed.zip` | Pre-processed windowed R-R interval CSVs (output of the Python script) |
| `ECG_pipeline.R` | Main analysis: symbolic RQA, mixed effects models, and XGBoost classification |
| `symbolization_plots.R` | Generates example symbolization and recurrence plot figures |
| `final_model_010826.rds` | Trained XGBoost model |
| `testset_final_model_010826.csv` | Test split used for reported results |
| `trainset_final_model_010826.csv` | Train split used for reported results |

## Reproducing the Results

### From scratch

1. Download the MIT-BIH Atrial Fibrillation Database from PhysioNet
2. Run `batch_process_afib.py` to generate windowed R-R interval CSVs
3. Run `ECG_pipeline.R` — Parts 1-3 will run from the newly generated CSVs
4. Part 4 (commented out) shows how the classifier was trained
5. Part 5 requires the provided model and data splits to reproduce the reported classification results

### Using provided files

1. Unzip `windowed.zip`
2. Run `ECG_pipeline.R` — Parts 1-3 run the sRQA and mixed models from the windowed CSVs
3. Part 4 (commented out) shows how the classifier was trained
4. Part 5 loads the provided model and data splits to reproduce the classification results

All file paths are hardcoded and will need to be updated to match your local directory structure.
