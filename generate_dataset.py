"""
Dataset Generation for Token Prediction Model

This script processes text files (ABAP and Markdown) to create a training dataset
for token prediction models. It extracts linguistic features and calculates actual
token counts using both GPT-4 and Mistral tokenizers.

Usage:
    python generate_dataset.py [--input-dirs DIR1 DIR2 ...] [--output OUTPUT_FILE]
"""

import os
import sys
import argparse
from typing import List, Tuple
import pandas as pd
from tqdm import tqdm

from utils import (
    count_tokens_gpt4, 
    count_tokens_mistral,
    extract_text_features,
    predict_tokens_gpt4,
    predict_tokens_mistral
)


def process_text_file(file_path: str) -> dict:
    """
    Process a single text file and extract all relevant features and token counts.
    
    Args:
        file_path: Path to the text file
        
    Returns:
        Dictionary containing file metadata, features, and token counts
    """
    try:
        with open(file_path, 'r', encoding='utf-8') as file:
            text = file.read()
    except Exception as e:
        print(f"Error reading {file_path}: {e}")
        return None
    
    # Extract features
    features = extract_text_features(text)
    (text_length, word_count, punctuation_count, number_count, 
     whitespace_count, line_count, sentence_count) = features
    
    # Calculate actual token counts
    gpt4_tokens = count_tokens_gpt4(text)
    mistral_tokens = count_tokens_mistral(text)
    
    # Generate predictions
    predicted_gpt4 = predict_tokens_gpt4(*features)
    predicted_mistral = predict_tokens_mistral(*features)
    
    # Calculate prediction differences
    diff_gpt4 = gpt4_tokens - predicted_gpt4
    diff_mistral = mistral_tokens - predicted_mistral
    
    return {
        'filename': os.path.basename(file_path),
        'file_path': file_path,
        'gpt4_tokens': gpt4_tokens,
        'mistral_tokens': mistral_tokens,
        'text_length': text_length,
        'word_count': word_count,
        'punctuation_count': punctuation_count,
        'number_count': number_count,
        'whitespace_count': whitespace_count,
        'line_count': line_count,
        'sentence_count': sentence_count,
        'predicted_gpt4': predicted_gpt4,
        'predicted_mistral': predicted_mistral,
        'diff_gpt4': diff_gpt4,
        'diff_mistral': diff_mistral
    }


def scan_directories(directories: List[str], extensions: List[str] = None) -> List[str]:
    """
    Scan directories for files with specified extensions.
    
    Args:
        directories: List of directory paths to scan
        extensions: List of file extensions to include (e.g., ['.abap', '.md'])
        
    Returns:
        List of file paths matching the criteria
    """
    if extensions is None:
        extensions = ['.abap', '.md']
    
    file_paths = []
    
    for directory in directories:
        if not os.path.exists(directory):
            print(f"Warning: Directory '{directory}' does not exist, skipping...")
            continue
            
        for root, _, files in os.walk(directory):
            for file in files:
                if any(file.endswith(ext) for ext in extensions):
                    file_paths.append(os.path.join(root, file))
    
    return file_paths


def generate_dataset(input_directories: List[str], 
                    output_file: str,
                    file_extensions: List[str] = None) -> pd.DataFrame:
    """
    Generate a dataset by processing multiple directories of text files.
    
    Args:
        input_directories: List of directories containing text files
        output_file: Path for the output TSV file
        file_extensions: List of file extensions to process
        
    Returns:
        DataFrame containing the generated dataset
    """
    print("Token Prediction Dataset Generator")
    print("=" * 50)
    
    # Scan for files
    print(f"\nScanning directories: {', '.join(input_directories)}")
    file_paths = scan_directories(input_directories, file_extensions)
    print(f"Found {len(file_paths)} files to process")
    
    if not file_paths:
        print("No files found to process!")
        return None
    
    # Process files
    print("\nProcessing files...")
    results = []
    
    for file_path in tqdm(file_paths, desc="Processing"):
        result = process_text_file(file_path)
        if result and result['gpt4_tokens'] > 0:  # Only include files with content
            results.append(result)
    
    # Create DataFrame
    df = pd.DataFrame(results)
    
    # Sort by filename
    df = df.sort_values('filename')
    
    # Save to TSV
    print(f"\nSaving dataset to: {output_file}")
    os.makedirs(os.path.dirname(output_file), exist_ok=True)
    df.to_csv(output_file, sep='\t', index=False)
    
    # Display statistics
    print("\nDataset Statistics:")
    print(f"Total files processed: {len(df)}")
    print(f"Average GPT-4 tokens per file: {df['gpt4_tokens'].mean():.1f}")
    print(f"Average Mistral tokens per file: {df['mistral_tokens'].mean():.1f}")
    print(f"GPT-4 prediction MAE: {df['diff_gpt4'].abs().mean():.1f}")
    print(f"Mistral prediction MAE: {df['diff_mistral'].abs().mean():.1f}")
    
    return df


def main():
    """Main entry point for the script."""
    parser = argparse.ArgumentParser(
        description="Generate training dataset for token prediction models"
    )
    parser.add_argument(
        '--input-dirs', 
        nargs='+',
        #default=['./_abap_code/'],
        default=['./_your_specific_dataset/'],
        help='Input directories containing text files (default: ./_abap_code/)'
    )
    parser.add_argument(
        '--output',
        default='./_predictoken/dataset.tsv',
        help='Output TSV file path (default: ./_predictoken/dataset.tsv)'
    )
    parser.add_argument(
        '--extensions',
        nargs='+',
        default=['.abap', '.md'],
        help='File extensions to process (default: .abap .md)'
    )
    
    args = parser.parse_args()
    
    try:
        df = generate_dataset(
            input_directories=args.input_dirs,
            output_file=args.output,
            file_extensions=args.extensions
        )
        
        if df is not None:
            print(f"\nDataset generation complete! Saved {len(df)} records.")
            
    except KeyboardInterrupt:
        print("\n\nProcess interrupted by user.")
        sys.exit(1)
    except Exception as e:
        print(f"\nError: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()