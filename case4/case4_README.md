## Data Source
Videos must be requested and downloaded directly from the Miami University Deception Detection Database (MU3D). 
Lloyd, E. P., et al. (2019). Miami University Deception Detection Database. Behavior Research Methods, 51, 429–439. https://pubmed.ncbi.nlm.nih.gov/29869221/

## Files
| File | Description |
|------|-------------|
| `01_transcription.py` | Extract audio from MU3D videos and transcribe with Whisper (OpenAI), producing word-level timestamps and pause durations |
| `02_cleaning_and_srqa.R` | Clean transcription data, merge participant demographics, binarize pauses, and compute 14 sRQA metrics per condition |
| `03_statistical_analysis_part1.R` | Two-way mixed-effects models (Veracity × Valence), ordered differences plot, and emmeans interaction plots |
| `04_statistical_analysis_part2.R` | Mixed-effects models (Valence × Sex), ordered differences plot, and emmeans interaction plots  |
| `05_xgboost.R` | XGBoost (DART) with LOOCV and Hyperband tuning for truth vs. lie classification; ROC curve |
| `deception_srqa_with_pause_1120_final.csv` | Analysis-ready dataset: 4 rows per participant (lie/truth × positive/negative) with 14 sRQA metrics |
| `final_analysis.csv` | MU3D participant demographics and experimental conditions (used in Step 3) |

## Reproducing the Results
1. Request and download the MU3D video files from Miami University.
2. Run `01_transcription.py` to extract audio and transcribe videos (direct local path to MU3D video folder). Produces `mu3d_sequential_timestamps.csv`.
3. Run `02_cleaning_and_srqa.R` to clean transcriptions, merge with `final_analysis.csv`, and compute sRQA metrics. Produces `deception_srqa_with_pause_1120_final.csv` which is our main feature table.
4. Run `03_statistical_analysis_part1.R` to fit Veracity × Valence models and produce s plot, 4 interaction plots.
5. Run `04_statistical_analysis_part2.R` to fit Veracity × Valence × Sex models and produce s plot, 3 interaction plots.
6. Run `05_xgboost.R` to train and evaluate the XGBoost classifier and produce ROC curve figure

All file paths are hardcoded and will need to be updated to match your local directory structure.
