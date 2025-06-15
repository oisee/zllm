"""
Token Prediction CLI Tool

This script provides a command-line interface for predicting token counts
for text input using pre-trained models. It supports both GPT-4 and Mistral
tokenizers and can process individual text strings or files.

Usage:
    python predict.py "Your text here" --model gpt4
    python predict.py --file document.txt --model mistral
    python predict.py --interactive
"""

import sys
import argparse
from typing import Optional
import json

from utils import (
    extract_text_features,
    predict_tokens_gpt4,
    predict_tokens_mistral,
    predict_tokens_from_text,
    count_tokens_gpt4,
    count_tokens_mistral
)


def predict_text(text: str, model: str = "gpt4", show_features: bool = False, 
                compare_actual: bool = False) -> dict:
    """
    Predict token count for given text.
    
    Args:
        text: Input text to analyze
        model: Model to use (gpt4 or mistral)
        show_features: Whether to display extracted features
        compare_actual: Whether to calculate and show actual token count
        
    Returns:
        Dictionary with prediction results
    """
    # Get prediction
    result = predict_tokens_from_text(text, model)
    
    # Add actual token count if requested
    if compare_actual:
        try:
            if model.lower() == "gpt4":
                actual_tokens = count_tokens_gpt4(text)
            else:
                actual_tokens = count_tokens_mistral(text)
            
            result['actual_tokens'] = actual_tokens
            result['difference'] = actual_tokens - result['predicted_tokens']
            result['accuracy'] = (1 - abs(result['difference']) / actual_tokens) * 100
        except Exception as e:
            result['actual_tokens'] = f"Error: {e}"
    
    return result


def format_result(result: dict, verbose: bool = False) -> str:
    """
    Format prediction result for display.
    
    Args:
        result: Dictionary containing prediction results
        verbose: Whether to show detailed output
        
    Returns:
        Formatted string for display
    """
    output = []
    
    # Basic prediction
    output.append(f"Predicted {result['model'].upper()} tokens: {result['predicted_tokens']}")
    
    # Actual comparison if available
    if 'actual_tokens' in result and isinstance(result['actual_tokens'], int):
        output.append(f"Actual tokens: {result['actual_tokens']}")
        output.append(f"Difference: {result['difference']:+d} ({result['accuracy']:.1f}% accurate)")
    
    # Features if verbose
    if verbose and 'features' in result:
        output.append("\nExtracted features:")
        for feature, value in result['features'].items():
            output.append(f"  {feature}: {value}")
    
    return "\n".join(output)


def predict_file(file_path: str, model: str = "gpt4", **kwargs) -> dict:
    """
    Predict token count for text in a file.
    
    Args:
        file_path: Path to the text file
        model: Model to use
        **kwargs: Additional arguments passed to predict_text
        
    Returns:
        Dictionary with prediction results
    """
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            text = f.read()
        
        result = predict_text(text, model, **kwargs)
        result['file_path'] = file_path
        return result
        
    except Exception as e:
        return {'error': str(e), 'file_path': file_path}


def interactive_mode():
    """Run the tool in interactive mode."""
    print("Token Prediction Tool - Interactive Mode")
    print("Type 'quit' or 'exit' to stop, 'help' for commands")
    print("-" * 50)
    
    while True:
        try:
            text = input("\nEnter text (or command): ").strip()
            
            if text.lower() in ['quit', 'exit']:
                break
            elif text.lower() == 'help':
                print("\nCommands:")
                print("  quit/exit - Exit interactive mode")
                print("  model:gpt4 - Switch to GPT-4 model")
                print("  model:mistral - Switch to Mistral model")
                print("  features:on/off - Show/hide feature extraction")
                print("  compare:on/off - Show/hide actual token comparison")
                continue
            elif text.startswith('model:'):
                model = text.split(':', 1)[1]
                if model in ['gpt4', 'mistral']:
                    interactive_mode.model = model
                    print(f"Switched to {model.upper()} model")
                continue
            elif text.startswith('features:'):
                interactive_mode.show_features = text.endswith('on')
                print(f"Feature display: {'on' if interactive_mode.show_features else 'off'}")
                continue
            elif text.startswith('compare:'):
                interactive_mode.compare = text.endswith('on')
                print(f"Actual comparison: {'on' if interactive_mode.compare else 'off'}")
                continue
            
            if text:
                result = predict_text(
                    text, 
                    getattr(interactive_mode, 'model', 'gpt4'),
                    show_features=getattr(interactive_mode, 'show_features', False),
                    compare_actual=getattr(interactive_mode, 'compare', False)
                )
                print("\n" + format_result(result, verbose=interactive_mode.show_features))
                
        except KeyboardInterrupt:
            print("\n\nExiting...")
            break
        except Exception as e:
            print(f"Error: {e}")


def main():
    """Main entry point for the CLI tool."""
    parser = argparse.ArgumentParser(
        description="Predict token counts for text using pre-trained models",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python predict.py "Hello, world!" --model gpt4
  python predict.py --file document.txt --model mistral --compare
  python predict.py --interactive
  echo "Some text" | python predict.py --stdin
        """
    )
    
    # Input options
    input_group = parser.add_mutually_exclusive_group()
    input_group.add_argument(
        'text',
        nargs='?',
        help='Text to analyze (if not using --file or --stdin)'
    )
    input_group.add_argument(
        '--file', '-f',
        help='Path to text file to analyze'
    )
    input_group.add_argument(
        '--stdin',
        action='store_true',
        help='Read text from standard input'
    )
    input_group.add_argument(
        '--interactive', '-i',
        action='store_true',
        help='Run in interactive mode'
    )
    
    # Model options
    parser.add_argument(
        '--model', '-m',
        choices=['gpt4', 'mistral'],
        default='gpt4',
        help='Model to use for prediction (default: gpt4)'
    )
    
    # Output options
    parser.add_argument(
        '--compare', '-c',
        action='store_true',
        help='Compare with actual token count'
    )
    parser.add_argument(
        '--features',
        action='store_true',
        help='Show extracted text features'
    )
    parser.add_argument(
        '--json',
        action='store_true',
        help='Output results as JSON'
    )
    
    args = parser.parse_args()
    
    # Interactive mode
    if args.interactive:
        interactive_mode()
        return
    
    # Get input text
    if args.stdin:
        text = sys.stdin.read()
    elif args.file:
        result = predict_file(
            args.file, 
            args.model,
            show_features=args.features,
            compare_actual=args.compare
        )
        if 'error' in result:
            print(f"Error reading file: {result['error']}")
            sys.exit(1)
        
        if args.json:
            print(json.dumps(result, indent=2))
        else:
            print(format_result(result, verbose=args.features))
        return
    elif args.text:
        text = args.text
    else:
        parser.print_help()
        sys.exit(1)
    
    # Make prediction
    result = predict_text(
        text, 
        args.model,
        show_features=args.features,
        compare_actual=args.compare
    )
    
    # Output results
    if args.json:
        print(json.dumps(result, indent=2))
    else:
        print(format_result(result, verbose=args.features))


if __name__ == "__main__":
    main()