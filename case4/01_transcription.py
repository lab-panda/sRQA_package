# ===============================
# 01_transcription.py
# Audio extraction and word-level transcription using Whisper
# Converts MU3D videos → audio → word-by-word timestamps with pause durations
# Output: mu3d_sequential_timestamps.csv
# ===============================

import os
import whisper_timestamped as whisper
import pandas as pd
from moviepy.editor import VideoFileClip

# ===============================
# SET PATHS 
base_folder = "/path/to/MU3D-Package/Videos"  # UPDATE to your local MU3D video directory
video_folder = base_folder
audio_folder = os.path.join(base_folder, "audios")
output_csv_path = os.path.join(base_folder, "mu3d_sequential_timestamps.csv")

# ===============================
# STEP 1: EXTRACT AUDIO FROM VIDEOS
successful_extractions = 0

for video_file in video_files:
    video_path = os.path.join(video_folder, video_file)
    audio_filename = f"{os.path.splitext(video_file)[0]}.wav"
    audio_path = os.path.join(audio_folder, audio_filename)  #saving inside thesis_audios folder

    print(f"\nExtracting audio from: {video_file}")

    #put video into memory so we can access its contents 
    video = VideoFileClip(video_path)

    #check the audio and extract
    if video.audio is not None:
        video.audio.write_audiofile(audio_path, verbose=False, logger=None)
        successful_extractions += 1

    else:
        print(f"NO! Warning: No audio found in {video_file}, skipping...")

print(f"\nALL DONE! Extracted audio from {successful_extractions}/{len(video_files)} videos.")

# ===============================
# STEP 2: TRANSCRIBE WITH WORD-LEVEL TIMESTAMPS
#whisper model 
model = whisper.load_model("base")

#check for existing files to resume processing
completed_videos = set()
if os.path.exists(output_csv_path):
    existing_df = pd.read_csv(output_csv_path)
    completed_videos = set(existing_df["Video Filename"].tolist())

#make sure audio folder exists
if not os.path.exists(audio_folder):
    print(f"NO! Error: Audio folder {audio_folder} not found!")
else:
    all_results = []
    
    #processing each audio file
    for audio_file in os.listdir(audio_folder):
        if audio_file.endswith(".wav") and audio_file not in completed_videos:
            audio_path = os.path.join(audio_folder, audio_file)
            print(f"\nTranscribing: {audio_file}")
            
            try:
                #load and transcribe with word by word timestamps
                audio = whisper.load_audio(audio_path)
                result = whisper.transcribe(
                    model, audio, 
                    beam_size=5, best_of=5, language="en", 
                    condition_on_previous_text=False,
                    temperature=(0.0, 0.2, 0.4, 0.6, 0.8, 1.0), 
                    detect_disfluencies=True
                )
                
                #extract word by word data with pauses
                previous_word_end = 0
                
                for segment in result.get('segments', []):
                    segment_start = segment.get('start', 0)
                    segment_end = segment.get('end', 0)
                    segment_text = segment.get('text', '').strip()
                    
                    #process words in this segment
                    for word_info in segment.get('words', []):
                        word = word_info.get('text', '').strip()
                        word_start = word_info.get('start', 0)
                        word_end = word_info.get('end', 0)
                        
                        #calculat pause before word
                        pause_duration = word_start - previous_word_end if previous_word_end > 0 else 0
                        
                        #add row to results
                        all_results.append({
                            'Video Filename': audio_file,
                            'Segment Start': segment_start,
                            'Segment End': segment_end,
                            'Segment Text': segment_text,
                            'Word': word,
                            'Word Start': word_start,
                            'Word End': word_end,
                            'Pause Before Word': pause_duration
                        })
                        
                        previous_word_end = word_end
                
                print(f"YAY! Transcription complete for: {audio_file}")
                
                #save after each file
                df = pd.DataFrame(all_results)
                df.to_csv(output_csv_path, mode='a', 
                         header=not os.path.exists(output_csv_path), index=False)
                all_results = []  #clear to save memory
                
            except Exception as e:
                print(f"NO! Error processing {audio_file}: {e}")
    
    print(f"YAY! All results saved to: {output_csv_path}")

