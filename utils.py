"""
Utility functions for the Token Prediction Model.

This module contains shared functions for feature extraction, token counting,
and text analysis used across the token prediction pipeline.
"""

import math
import tiktoken
import keras_nlp


def count_tokens_gpt4(text: str) -> int:
    """
    Count tokens in text using OpenAI's GPT-4 tokenizer.
    
    Args:
        text: The input text to tokenize
        
    Returns:
        Number of tokens in the text
    """
    enc = tiktoken.encoding_for_model("gpt-4")
    num_tokens = len(enc.encode(text))
    return num_tokens


def count_tokens_mistral(text: str) -> int:
    """
    Count tokens in text using Mistral's tokenizer.
    
    Args:
        text: The input text to tokenize
        
    Returns:
        Number of tokens in the text
    """
    tokenizer = keras_nlp.models.MistralTokenizer.from_preset(
        "mistral_instruct_7b_en",
    )
    tokens = tokenizer(text)
    return len(tokens)


def extract_text_features(text: str) -> tuple:
    """
    Extract linguistic features from text for token prediction.
    
    This function analyzes text and extracts 7 key features that correlate
    with token count across different tokenizers.
    
    Args:
        text: The input text to analyze
        
    Returns:
        A tuple containing:
        - text_length: Total character count
        - word_count: Number of space-separated words
        - punctuation_count: Count of punctuation marks (.,;:!?'")
        - number_count: Count of digit characters
        - whitespace_count: Count of all whitespace characters
        - line_count: Number of newline characters
        - sentence_count: Approximate count of sentences (based on .!?)
    """
    text_length = len(text)
    word_count = len(text.split())
    punctuation_count = sum(c in '.,;:!?\'"' for c in text)
    number_count = sum(c.isdigit() for c in text)
    whitespace_count = sum(c.isspace() for c in text)
    line_count = text.count('\n')
    
    # Approximate sentence count by counting sentence-ending punctuation
    sentence_delimiters = ['.', '?', '!']
    sentence_count = sum(text.count(delimiter) for delimiter in sentence_delimiters)
    
    return (text_length, word_count, punctuation_count, number_count, 
            whitespace_count, line_count, sentence_count)


def predict_tokens_gpt4(text_length: int, words: int, punctuations: int, 
                       numbers: int, whitespaces: int, lines: int, 
                       sentences: int) -> int:
    """
    Predict GPT-4 token count using a pre-trained linear regression model.
    
    This function uses coefficients from a model trained on a diverse dataset
    of ABAP and Markdown files. The model achieves R² > 0.997.
    
    Args:
        text_length: Total character count
        words: Number of words
        punctuations: Count of punctuation marks
        numbers: Count of digit characters
        whitespaces: Count of whitespace characters
        lines: Number of newline characters
        sentences: Approximate sentence count
        
    Returns:
        Predicted token count (rounded up to nearest integer)
    """
    # Model coefficients trained on diverse text data
    args = [text_length, words, punctuations, numbers, whitespaces, lines, sentences]
    coefficients = [0.09962379, 0.41045692, 0.94309167, 1.1346075, 
                   -0.05270366, 3.94760669, -1.49832381]
    intercept = -37.79047528292131
    
    predicted_tokens = intercept + sum(arg * coef for arg, coef in zip(args, coefficients))
    return math.ceil(predicted_tokens)


def predict_tokens_mistral(text_length: int, words: int, punctuations: int, 
                          numbers: int, whitespaces: int, lines: int, 
                          sentences: int) -> int:
    """
    Predict Mistral token count using a pre-trained linear regression model.
    
    This function uses coefficients from a model trained on a diverse dataset
    of ABAP and Markdown files. The model achieves R² > 0.997.
    
    Args:
        text_length: Total character count
        words: Number of words
        punctuations: Count of punctuation marks
        numbers: Count of digit characters
        whitespaces: Count of whitespace characters
        lines: Number of newline characters
        sentences: Approximate sentence count
        
    Returns:
        Predicted token count (rounded up to nearest integer)
    """
    # Model coefficients trained on diverse text data
    args = [text_length, words, punctuations, numbers, whitespaces, lines, sentences]
    coefficients = [0.21993344, -0.30867016, 0.53583886, 1.75223606, 
                   -0.06621309, 5.21460546, 0.05222465]
    intercept = -60.45156056006181
    
    predicted_tokens = intercept + sum(arg * coef for arg, coef in zip(args, coefficients))
    return math.ceil(predicted_tokens)


def predict_tokens_from_text(text: str, model: str = "gpt4") -> dict:
    """
    Predict token count directly from text input.
    
    Args:
        text: The input text to analyze
        model: Target model - "gpt4" or "mistral"
        
    Returns:
        Dictionary containing:
        - predicted_tokens: The predicted token count
        - features: Dictionary of extracted features
        - model: The model used for prediction
    """
    features = extract_text_features(text)
    feature_names = ["text_length", "word_count", "punctuation_count", 
                    "number_count", "whitespace_count", "line_count", 
                    "sentence_count"]
    feature_dict = dict(zip(feature_names, features))
    
    if model.lower() == "gpt4":
        predicted = predict_tokens_gpt4(*features)
    elif model.lower() == "mistral":
        predicted = predict_tokens_mistral(*features)
    else:
        raise ValueError(f"Unknown model: {model}. Choose 'gpt4' or 'mistral'")
    
    return {
        "predicted_tokens": predicted,
        "features": feature_dict,
        "model": model
    }