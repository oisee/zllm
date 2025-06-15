"""
Model Training for Token Prediction

This script trains linear regression models to predict token counts for both
GPT-4 and Mistral tokenizers based on text features. The models achieve
high accuracy (R² > 0.997) and are saved for future use.

Usage:
    python train_models.py [--input INPUT_FILE] [--output-dir OUTPUT_DIR]
"""

import os
import sys
import argparse
import json
from datetime import datetime
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from joblib import dump


# Feature columns used for training
FEATURE_COLUMNS = [
    'text_length', 'word_count', 'punctuation_count', 
    'number_count', 'whitespace_count', 'line_count', 'sentence_count'
]


def evaluate_model(model, X_test, y_test, model_name: str) -> dict:
    """
    Evaluate a trained model and return metrics.
    
    Args:
        model: Trained scikit-learn model
        X_test: Test features
        y_test: Test labels
        model_name: Name of the model for display
        
    Returns:
        Dictionary containing evaluation metrics
    """
    y_pred = model.predict(X_test)
    
    mae = mean_absolute_error(y_test, y_pred)
    mse = mean_squared_error(y_test, y_pred)
    rmse = np.sqrt(mse)
    r2 = r2_score(y_test, y_pred)
    
    print(f"\n{model_name} Model Performance:")
    print(f"  Mean Absolute Error (MAE): {mae:.2f} tokens")
    print(f"  Root Mean Square Error (RMSE): {rmse:.2f} tokens")
    print(f"  R-squared (R²): {r2:.6f}")
    
    return {
        'mae': mae,
        'mse': mse,
        'rmse': rmse,
        'r2': r2
    }


def train_single_model(X, y, model_name: str, test_size: float = 0.2) -> dict:
    """
    Train a single linear regression model.
    
    Args:
        X: Feature matrix
        y: Target values
        model_name: Name of the model
        test_size: Fraction of data to use for testing
        
    Returns:
        Dictionary containing model, metrics, and coefficients
    """
    print(f"\nTraining {model_name} model...")
    print(f"Dataset size: {len(X)} samples")
    
    # Split data
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=test_size, random_state=42
    )
    
    # Train model
    model = LinearRegression()
    model.fit(X_train, y_train)
    
    # Evaluate
    metrics = evaluate_model(model, X_test, y_test, model_name)
    
    # Extract coefficients
    coefficients = model.coef_
    intercept = model.intercept_
    
    print(f"\nModel Coefficients:")
    for feature, coef in zip(FEATURE_COLUMNS, coefficients):
        print(f"  {feature}: {coef:.8f}")
    print(f"  Intercept: {intercept:.8f}")
    
    return {
        'model': model,
        'metrics': metrics,
        'coefficients': coefficients.tolist(),
        'intercept': float(intercept),
        'feature_names': FEATURE_COLUMNS,
        'training_samples': len(X_train),
        'test_samples': len(X_test)
    }


def generate_python_function(model_info: dict, model_name: str) -> str:
    """
    Generate a Python function from model coefficients.
    
    Args:
        model_info: Dictionary containing model information
        model_name: Name of the model (gpt4 or mistral)
        
    Returns:
        Python function as a string
    """
    func_template = f'''def predict_tokens_{model_name}(text_length: int, word_count: int, punctuation_count: int,
                          number_count: int, whitespace_count: int, line_count: int,
                          sentence_count: int) -> int:
    """
    Predict {model_name.upper()} token count using pre-trained linear regression model.
    
    Model Performance:
    - R²: {model_info["metrics"]["r2"]:.6f}
    - MAE: {model_info["metrics"]["mae"]:.2f} tokens
    
    Returns:
        Predicted token count (rounded up)
    """
    import math
    
    features = [text_length, word_count, punctuation_count, number_count,
                whitespace_count, line_count, sentence_count]
    coefficients = {model_info["coefficients"]}
    intercept = {model_info["intercept"]}
    
    prediction = intercept + sum(f * c for f, c in zip(features, coefficients))
    return math.ceil(prediction)
'''
    return func_template


def generate_abap_code(models_info: dict) -> str:
    """
    Generate ABAP code for model implementation.
    
    Args:
        models_info: Dictionary containing model information for all models
        
    Returns:
        ABAP code as a string
    """
    abap_code = """*----------------------------------------------------------------------*
*  Token Prediction Models - ABAP Implementation
*  Generated on: {timestamp}
*----------------------------------------------------------------------*
CLASS zcl_token_predictor DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_coefficients,
             text_length  TYPE f,
             words        TYPE f,
             punctuations TYPE f,
             numbers      TYPE f,
             whitespaces  TYPE f,
             lines        TYPE f,
             sentences    TYPE f,
             intercept    TYPE f,
           END OF ty_coefficients.
    
    CLASS-DATA: gs_co_gpt4    TYPE ty_coefficients,
                gs_co_mistral TYPE ty_coefficients.
    
    CLASS-METHODS: class_constructor,
                   predict_tokens_gpt4
                     IMPORTING iv_text         TYPE string
                     RETURNING VALUE(rv_tokens) TYPE i,
                   predict_tokens_mistral
                     IMPORTING iv_text         TYPE string
                     RETURNING VALUE(rv_tokens) TYPE i.
    
  PRIVATE SECTION.
    CLASS-METHODS: extract_features
                     IMPORTING iv_text            TYPE string
                     EXPORTING ev_text_length     TYPE i
                               ev_words           TYPE i
                               ev_punctuations    TYPE i
                               ev_numbers         TYPE i
                               ev_whitespaces     TYPE i
                               ev_lines           TYPE i
                               ev_sentences       TYPE i.
ENDCLASS.

CLASS zcl_token_predictor IMPLEMENTATION.
  METHOD class_constructor.
""".format(timestamp=datetime.now().strftime("%Y-%m-%d %H:%M:%S"))
    
    for model_name, info in models_info.items():
        model_var = f"gs_co_{model_name}"
        coeffs = info['coefficients']
        abap_code += f"""
    * {model_name.upper()} Model Coefficients
    * R²: {info['metrics']['r2']:.6f}, MAE: {info['metrics']['mae']:.2f}
    {model_var} = VALUE #(
      text_length  = '{coeffs[0]:.8f}'
      words        = '{coeffs[1]:.8f}'
      punctuations = '{coeffs[2]:.8f}'
      numbers      = '{coeffs[3]:.8f}'
      whitespaces  = '{coeffs[4]:.8f}'
      lines        = '{coeffs[5]:.8f}'
      sentences    = '{coeffs[6]:.8f}'
      intercept    = '{info['intercept']:.8f}'
    ).
"""
    
    abap_code += """
  ENDMETHOD.
  
  METHOD predict_tokens_gpt4.
    DATA: lv_text_length  TYPE i,
          lv_words        TYPE i,
          lv_punctuations TYPE i,
          lv_numbers      TYPE i,
          lv_whitespaces  TYPE i,
          lv_lines        TYPE i,
          lv_sentences    TYPE i,
          lv_prediction   TYPE f.
    
    extract_features( EXPORTING iv_text = iv_text
                      IMPORTING ev_text_length = lv_text_length
                                ev_words = lv_words
                                ev_punctuations = lv_punctuations
                                ev_numbers = lv_numbers
                                ev_whitespaces = lv_whitespaces
                                ev_lines = lv_lines
                                ev_sentences = lv_sentences ).
    
    lv_prediction = gs_co_gpt4-intercept
                  + lv_text_length * gs_co_gpt4-text_length
                  + lv_words * gs_co_gpt4-words
                  + lv_punctuations * gs_co_gpt4-punctuations
                  + lv_numbers * gs_co_gpt4-numbers
                  + lv_whitespaces * gs_co_gpt4-whitespaces
                  + lv_lines * gs_co_gpt4-lines
                  + lv_sentences * gs_co_gpt4-sentences.
    
    rv_tokens = ceil( lv_prediction ).
  ENDMETHOD.
  
  METHOD predict_tokens_mistral.
    DATA: lv_text_length  TYPE i,
          lv_words        TYPE i,
          lv_punctuations TYPE i,
          lv_numbers      TYPE i,
          lv_whitespaces  TYPE i,
          lv_lines        TYPE i,
          lv_sentences    TYPE i,
          lv_prediction   TYPE f.
    
    extract_features( EXPORTING iv_text = iv_text
                      IMPORTING ev_text_length = lv_text_length
                                ev_words = lv_words
                                ev_punctuations = lv_punctuations
                                ev_numbers = lv_numbers
                                ev_whitespaces = lv_whitespaces
                                ev_lines = lv_lines
                                ev_sentences = lv_sentences ).
    
    lv_prediction = gs_co_mistral-intercept
                  + lv_text_length * gs_co_mistral-text_length
                  + lv_words * gs_co_mistral-words
                  + lv_punctuations * gs_co_mistral-punctuations
                  + lv_numbers * gs_co_mistral-numbers
                  + lv_whitespaces * gs_co_mistral-whitespaces
                  + lv_lines * gs_co_mistral-lines
                  + lv_sentences * gs_co_mistral-sentences.
    
    rv_tokens = ceil( lv_prediction ).
  ENDMETHOD.
  
  METHOD extract_features.
    " Implementation would extract the same features as Python version
    " This is a placeholder - actual implementation depends on ABAP version
  ENDMETHOD.
ENDCLASS.
"""
    
    return abap_code


def train_all_models(input_file: str, output_dir: str) -> dict:
    """
    Train models for all supported tokenizers.
    
    Args:
        input_file: Path to the training dataset (TSV file)
        output_dir: Directory to save trained models and generated code
        
    Returns:
        Dictionary containing all model information
    """
    print("Token Prediction Model Training")
    print("=" * 50)
    
    # Load dataset
    print(f"\nLoading dataset from: {input_file}")
    df = pd.read_csv(input_file, sep='\t')
    print(f"Loaded {len(df)} samples")
    
    # Check for required columns
    required_cols = FEATURE_COLUMNS + ['gpt4_tokens', 'mistral_tokens']
    missing_cols = [col for col in required_cols if col not in df.columns]
    
    if missing_cols:
        # Try alternative column names
        alt_mappings = {
            'gpt4_tokens': 'Token Count',
            'mistral_tokens': 'Mistral Token Count',
            'text_length': 'Text Length',
            'word_count': 'Word Count',
            'punctuation_count': 'Punctuation Count',
            'number_count': 'Number Count',
            'whitespace_count': 'Whitespace Count',
            'line_count': 'Lines',
            'sentence_count': 'Sentence Count'
        }
        
        for new_col, old_col in alt_mappings.items():
            if old_col in df.columns and new_col not in df.columns:
                df[new_col] = df[old_col]
    
    # Prepare features
    X = df[FEATURE_COLUMNS]
    
    # Train models
    models_info = {}
    
    # GPT-4 model
    if 'gpt4_tokens' in df.columns or 'Token Count' in df.columns:
        y_gpt4 = df.get('gpt4_tokens', df.get('Token Count'))
        models_info['gpt4'] = train_single_model(X, y_gpt4, "GPT-4")
    
    # Mistral model
    if 'mistral_tokens' in df.columns or 'Mistral Token Count' in df.columns:
        y_mistral = df.get('mistral_tokens', df.get('Mistral Token Count'))
        models_info['mistral'] = train_single_model(X, y_mistral, "Mistral")
    
    # Save models
    print(f"\nSaving models to: {output_dir}")
    os.makedirs(output_dir, exist_ok=True)
    
    for model_name, info in models_info.items():
        model_path = os.path.join(output_dir, f'{model_name}_model.joblib')
        dump(info['model'], model_path)
        print(f"  Saved {model_name} model to: {model_path}")
    
    # Save model information
    info_path = os.path.join(output_dir, 'model_info.json')
    info_data = {
        name: {k: v for k, v in info.items() if k != 'model'}
        for name, info in models_info.items()
    }
    info_data['training_date'] = datetime.now().isoformat()
    info_data['training_file'] = input_file
    
    with open(info_path, 'w') as f:
        json.dump(info_data, f, indent=2)
    print(f"  Saved model info to: {info_path}")
    
    # Generate Python code
    python_code = "# Auto-generated token prediction functions\n\n"
    for model_name, info in models_info.items():
        python_code += generate_python_function(info, model_name) + "\n\n"
    
    python_path = os.path.join(output_dir, 'prediction_functions.py')
    with open(python_path, 'w') as f:
        f.write(python_code)
    print(f"  Generated Python functions: {python_path}")
    
    # Generate ABAP code
    abap_code = generate_abap_code(models_info)
    abap_path = os.path.join(output_dir, 'token_predictor.abap')
    with open(abap_path, 'w') as f:
        f.write(abap_code)
    print(f"  Generated ABAP code: {abap_path}")
    
    return models_info


def main():
    """Main entry point for the script."""
    parser = argparse.ArgumentParser(
        description="Train token prediction models"
    )
    parser.add_argument(
        '--input',
        default='./_predictoken/stats_4_training.tsv',
        help='Input training dataset (TSV file)'
    )
    parser.add_argument(
        '--output-dir',
        default='./_predictoken/models/',
        help='Output directory for models and generated code'
    )
    
    args = parser.parse_args()
    
    try:
        if not os.path.exists(args.input):
            print(f"Error: Input file '{args.input}' not found!")
            sys.exit(1)
        
        models_info = train_all_models(args.input, args.output_dir)
        
        print("\n" + "=" * 50)
        print("Training complete!")
        print(f"Trained {len(models_info)} models")
        
    except KeyboardInterrupt:
        print("\n\nProcess interrupted by user.")
        sys.exit(1)
    except Exception as e:
        print(f"\nError: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()