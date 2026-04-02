import wfdb
import numpy as np
import os
import pandas as pd
from collections import Counter
import glob

# Known issues with specific records
RECORD_NOTES = {
    '00735': 'Signals unavailable (but QRS annotations are fine)',
    '03665': 'Signals unavailable (but QRS annotations are fine)',
    '04043': 'Block 39 is unreadable',
    '04936': 'Only record with signals previously available',
    '05091': 'Corrected QRS annotations available (qrsc)',
    '06453': 'Recording ends after about 9 hours, 15 minutes',
    '08378': 'No start time',
    '08405': 'No start time; block 1067 is unreadable',
    '08434': 'Blocks 648, 857, and 894 are unreadable',
    '08455': 'No start time'
}


def get_available_records(data_dir):
    # Find all records that have .atr and .qrs/.qrsc files
    atr_files = glob.glob(os.path.join(data_dir, '*.atr'))
    record_names = [os.path.basename(f).replace('.atr', '') for f in atr_files]
    
    available_records = {}
    
    for record_name in record_names:
        # Check for corrected annotations first
        qrsc_path = os.path.join(data_dir, f'{record_name}.qrsc')
        qrs_path = os.path.join(data_dir, f'{record_name}.qrs')
        
        if os.path.exists(qrsc_path):
            available_records[record_name] = 'qrsc'
            if record_name in RECORD_NOTES:
                print(f"Note for {record_name}: {RECORD_NOTES[record_name]}")
        elif os.path.exists(qrs_path):
            available_records[record_name] = 'qrs'
            if record_name in RECORD_NOTES:
                print(f"Note for {record_name}: {RECORD_NOTES[record_name]}")
        else:
            print(f"Warning: No QRS annotations found for {record_name}, skipping...")
    
    return available_records


def get_rhythm_at_sample(atr_annotations, sample_idx):
    # Get the rhythm type at a given sample index
    current_rhythm = '(N'  # Default to normal
    
    for i, sample in enumerate(atr_annotations.sample):
        if sample > sample_idx:
            break
        if atr_annotations.aux_note[i] in ['(AFIB', '(AFL', '(J', '(N']:
            current_rhythm = atr_annotations.aux_note[i]
    
    return current_rhythm


def process_single_record(record_path, record_name, qrs_annotator='qrs', window_size=200):
    # Process a single record and extract windowed RR intervals with labels
    original_dir = os.getcwd()
    os.chdir(record_path)
    
    try:
        atr = wfdb.rdann(record_name, 'atr')
        qrs = wfdb.rdann(record_name, qrs_annotator)
        
        print(f"Processing {record_name} using {qrs_annotator} annotations...")
        
        # R-peaks and RR intervals
        r_peaks = qrs.sample
        rr_intervals = np.diff(r_peaks)
        
        # Label each RR interval based on rhythm
        rhythm_labels = []
        for i in range(len(rr_intervals)):
            beat_location = r_peaks[i+1]
            rhythm_type = get_rhythm_at_sample(atr, beat_location)
            
            if rhythm_type == '(AFIB':
                rhythm_labels.append('AF')
            elif rhythm_type == '(N':
                rhythm_labels.append('Normal')
            elif rhythm_type == '(AFL':
                rhythm_labels.append('AFL')
            elif rhythm_type == '(J':
                rhythm_labels.append('J')
            else:
                rhythm_labels.append('Unknown')
        
        # Create windows
        num_windows = len(rr_intervals) // window_size
        windows_data = []
        
        for i in range(num_windows):
            start_idx = i * window_size
            end_idx = start_idx + window_size
            
            window_rr = rr_intervals[start_idx:end_idx]
            window_rhythm_labels = rhythm_labels[start_idx:end_idx]
            
            # Majority vote for window label
            label_counts = Counter(window_rhythm_labels)
            total_count = len(window_rhythm_labels)
            
            af_count = label_counts.get('AF', 0)
            normal_count = label_counts.get('Normal', 0)
            afl_count = label_counts.get('AFL', 0)
            j_count = label_counts.get('J', 0)
            
            if af_count > total_count / 2:
                window_label = 'AF'
            elif normal_count > total_count / 2:
                window_label = 'Normal'
            else:
                window_label = 'Other'
            
            windows_data.append({
                'record': record_name,
                'window_idx': i,
                'rr_intervals': window_rr,
                'label': window_label,
                'af_count': af_count,
                'normal_count': normal_count,
                'afl_count': afl_count,
                'j_count': j_count,
                'qrs_annotator': qrs_annotator
            })
        
        print(f"  Created {num_windows} windows from {record_name}")
        os.chdir(original_dir)
        return windows_data
        
    except Exception as e:
        print(f"Error processing {record_name}: {str(e)}")
        os.chdir(original_dir)
        return []


def get_label_code(label):
    # Short code for filename
    label_map = {'AF': 'AF', 'Normal': 'N', 'Other': 'O'}
    return label_map.get(label, 'O')


def process_all_records(data_dir, output_dir, window_size=200):
    # Process all records and save windowed RR intervals
    os.makedirs(output_dir, exist_ok=True)
    
    print("Scanning for available records...")
    available_records = get_available_records(data_dir)
    
    print(f"\nFound {len(available_records)} records with QRS annotations")
    print(f"  qrsc: {sum(1 for v in available_records.values() if v == 'qrsc')}")
    print(f"  qrs: {sum(1 for v in available_records.values() if v == 'qrs')}")
    print()
    
    all_windows = []
    record_stats = []
    
    for record_name in sorted(available_records.keys()):
        qrs_annotator = available_records[record_name]
        windows_data = process_single_record(data_dir, record_name, qrs_annotator, window_size)
        
        if windows_data:
            all_windows.extend(windows_data)
            
            window_labels = [w['label'] for w in windows_data]
            stats = {
                'record': record_name,
                'qrs_annotator': qrs_annotator,
                'total_windows': len(windows_data),
                'af_windows': window_labels.count('AF'),
                'normal_windows': window_labels.count('Normal'),
                'other_windows': window_labels.count('Other')
            }
            record_stats.append(stats)
            
            # Save individual window CSVs
            for window_data in windows_data:
                label_code = get_label_code(window_data['label'])
                filename = f"{record_name}_window{window_data['window_idx']}_{label_code}.csv"
                filepath = os.path.join(output_dir, filename)
                
                df = pd.DataFrame({'rr_interval': window_data['rr_intervals']})
                df.to_csv(filepath, index=False, header=True)
    
    # Summary
    if record_stats:
        stats_df = pd.DataFrame(record_stats)
        print(stats_df.to_string(index=False))
        print(f"\nTotal records: {len(record_stats)}")
        print(f"Total windows: {len(all_windows)}")
        print(f"  AF: {sum(stats_df['af_windows'])} ({sum(stats_df['af_windows'])/len(all_windows)*100:.1f}%)")
        print(f"  Normal: {sum(stats_df['normal_windows'])} ({sum(stats_df['normal_windows'])/len(all_windows)*100:.1f}%)")
        print(f"  Other: {sum(stats_df['other_windows'])} ({sum(stats_df['other_windows'])/len(all_windows)*100:.1f}%)")
    else:
        print("No records were successfully processed.")
    
    return all_windows, record_stats


if __name__ == "__main__":
    data_dir = 'D:/Projects/Physionet/mit-bih-atrial-fibrillation-database-1.0.0/files'
    output_dir = 'D:/Projects/Physionet/mit-bih-atrial-fibrillation-database-1.0.0/windowed'
    
    all_windows, stats = process_all_records(data_dir, output_dir, window_size=200)