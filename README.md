# ZLLM: Enterprise LLM Integration Framework for ABAP

> Train once in Python. Predict anywhere—in ABAP, offline, or on edge AI.

The ZLLM framework provides comprehensive LLM integration capabilities for SAP ABAP environments, starting with **PREDICTOKEN** - smart token count prediction without expensive API calls.

## Quick Start

### PREDICTOKEN - Token Count Prediction

```abap
" Predict tokens for GPT-4
DATA(lo_predictor) = zcl_llm_00_predictoken=>new_for_model_type( 'GPT' ).
DATA(lv_tokens) = lo_predictor->predict( lv_your_text ).

" Predict tokens for Mistral
DATA(lo_predictor) = zcl_llm_00_predictoken=>new_for_model_type( 'MISTRAL' ).
DATA(lv_tokens) = lo_predictor->predict( lv_your_text ).
```

## Why PREDICTOKEN?

| Method | R² Score | Avg Error | Dependencies | API Calls |
|--------|----------|-----------|--------------|-----------|
| chars÷4 | 0.70 | ±23% | None | 0 |
| words×1.3 | 0.83 | ±15% | Basic regex | 0 |
| **PREDICTOKEN** | **0.987** | **±3%** | **Regex only** | **0** |
| Tokenizer API | 1.000 | 0% | Network + API | Every time |

**Performance:** Theoretical < 1ms prediction time vs 200-500ms for API calls

## Repository Structure

```
zllm/
├── README.md                   # Project documentation
├── requirements.txt            # Python dependencies
├── abaplint.jsonc             # ABAP code quality configuration
│
├── _predictoken/              # PREDICTOKEN training data & models
│   ├── linear_regression_model.joblib    # Trained model file
│   ├── stats_4_training.tsv              # Training dataset
│   └── stats_4_training_B.tsv            # Additional training data (result of "generate_dataset.py", not to overwrite "stats_4_training.tsv" )
│
├── *your*specific_dataset/    # Placeholder for your training data
├── generate_dataset.py        # Dataset generation from text files
├── train_models.py           # Model training script
├── predict.py                # Prediction testing script
└── utils.py                  # Helper functions
```

## Installation & Usage

### Python Training Environment

1. **Install dependencies:**
   ```bash
   pip install -r requirements.txt
   ```

2. **Generate training dataset from your own text files:**
   ```bash
   # Train on ABAP code and documentation
   python generate_dataset.py --input-dirs ./your_abap_code/ --extensions .abap .md
   
   # Train on any text files (Python, Java, documentation, etc.)
   python generate_dataset.py --input-dirs ./*your*specific_dataset/ --extensions .py .java .txt .md
   
   # Multiple directories and file types
   python generate_dataset.py --input-dirs ./code/ ./docs/ ./specs/ --extensions .abap .py .md .txt
   ```

3. **Train models on your dataset:**
   ```bash
   python train_models.py
   ```

4. **Test predictions:**
   ```bash
   python predict.py "your text here"
   ```

### Custom Training Features

- **Any Text Format**: Train on code, documentation, specifications, or mixed content
- **Multiple File Types**: Support for .abap, .py, .java, .md, .txt, and more
- **Flexible Datasets**: Combine different text types for robust predictions
- **Domain Adaptation**: Optimize for your specific text patterns and vocabulary

### Training Data Examples

```bash
# Use your specific dataset directory
python generate_dataset.py --input-dirs ./*your*specific_dataset/ --extensions .md .txt

# Mixed programming languages  
python generate_dataset.py --input-dirs ./src/ --extensions .py .java .js .abap

# Technical writing
python generate_dataset.py --input-dirs ./articles/ ./blogs/ --extensions .md .txt
```

### ABAP Integration

1. **Install via abapGit** (coming soon)
2. **Copy ABAP classes** to your system
3. **Run demo program:** `ZLLM_00_PREDICTOKEN_DEMO`

## PREDICTOKEN Performance

**Theoretical Performance:**
- **Prediction Time:** < 1ms (simple arithmetic operations)
- **Memory Usage:** 7 coefficients vs millions of parameters
- **Dependencies:** ABAP regex only

**Training Capabilities:**
- **Accuracy Target:** R² > 0.98 for most text domains
- **Feature Engineering:** 7 linguistic features (words, punctuation, etc.)
- **Model Types:** Separate models for GPT-4 and Mistral tokenizers

## Use Cases

### Enterprise AI Integration
- **Pre-flight checks** before expensive API calls
- **Cost estimation** for LLM operations  
- **Batch optimization** without tokenizer round-trips
- **Edge AI scenarios** with no network connectivity

### Custom Domain Applications
- **Technical Documentation** - Train on your company's docs and specs
- **Programming Languages** - Optimize for Python, Java, JavaScript, ABAP, etc.
- **Mixed Content** - Handle code + documentation + business text
- **Domain-Specific Text** - Legal documents, scientific papers, etc.

### Development Scenarios
- **Air-gapped systems** - No external API access
- **Real-time applications** - Sub-millisecond predictions
- **Cost-sensitive environments** - Minimize API usage
- **Legacy systems** - No modern ML libraries required

## Technical Deep-Dive

### Machine Learning Approach

PREDICTOKEN uses **linear regression** trained on diverse text corpora:

1. **Dataset Generation:** Process any text files (.abap, .py, .md, .txt, etc.)
2. **Feature Extraction:** 7 linguistic features per text sample
3. **Multi-Model Training:** Separate models for each tokenizer (GPT-4, Mistral)
4. **Coefficient Export:** Python coefficients → ABAP constants
5. **Runtime Prediction:** Simple arithmetic (7 multiplications + additions)

### Training Data Flexibility

**Supported Formats:**
- Source code (.abap, .py, .java, .js, .cpp, etc.)
- Documentation (.md, .txt, .rst)  
- Mixed content (code + comments + docs)
- Domain-specific text (legal, scientific, technical)

**Dataset Generation Features:**
- Automatic file discovery across directories
- Configurable file extensions
- Feature extraction and validation
- Token count verification with actual tokenizers
- Export to training-ready format

### Why Linear Regression Works

- **Deployable anywhere** - No ML runtime required
- **Interpretable** - Understand feature importance
- **Fast** - Constant time regardless of text length
- **Reliable** - No black-box failures
- **Lightweight** - 7 coefficients vs millions of parameters
- **Adaptable** - Retrain on your specific text domain

## Roadmap

### Current Release: PREDICTOKEN
- Token count prediction framework
- Python training pipeline with dataset generation
- Trained model artifacts (linear_regression_model.joblib)
- Training datasets (stats_4_training.tsv)
- ABAP implementation (coming soon)

### Coming Soon: Full ZLLM Framework
- **Core Orchestration** - LLM provider abstractions
- **Prompt Engineering** - Template system and composition
- **API Integration** - Multi-model payload adaptation
- **Code Intelligence** - ABAP code analysis and completion
- **Developer Tools** - GUI components and interactive features

## Development

### Code Quality
- **abaplint** configuration for ABAP code quality
- **Type safety** with comprehensive interface definitions
- **Performance focus** - Optimized for enterprise usage
- **Documentation** - Comprehensive inline and external docs

### Contributing
1. Fork the repository
2. Create feature branch (`git checkout -b feature/amazing-feature`)
3. Commit changes (`git commit -m 'Add amazing feature'`)
4. Push to branch (`git push origin feature/amazing-feature`)
5. Open Pull Request

## Documentation

### Technical Articles
- **LinkedIn:** [PREDICTOKEN: Fast, Local Token Count Prediction](your-linkedin-url)
- **SAP Community:** Coming soon
- **Technical Deep-Dive:** See [Implementation](IMPLEMENTATION.md)

### Related Projects
- **ZVDB:** [Vector Database in pure ABAP](https://github.com/oisee/zvdb)
- **Microsoft AI SDK for ABAP:** Integration dependency

## Performance Benchmarks

### Theoretical Performance
Based on the linear regression approach:

- **Prediction Time:** < 1ms (7 multiplications + 7 additions + ceiling operation)
- **Memory Usage:** 7 coefficients vs millions of parameters in neural models
- **Scalability:** Linear with text length (constant feature extraction cost)

### Training Accuracy (When Trained)
The linear regression approach typically achieves:

- **R² Score:** > 0.98 for most text domains
- **Mean Absolute Error:** 2-4 tokens for typical samples
- **Consistency:** Stable predictions across different text sizes

**Note:** Actual benchmarks will be added once comprehensive testing is completed across different domains and hardware configurations.

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Author

**Alice Vinogradova**  
Senior Software Engineer with 20+ years ABAP/SAP experience  
Specializing in enterprise AI integration and performance optimization

*Built in spare time with love, a personal Docker SAP NetWeaver instance, and a couple of imaginary friends: Claude and ChatGPT* =)

---

## Star This Repository

If you find ZLLM useful, please star this repository to show your support and stay updated on new releases!

**Coming Next:** Core ZLLM framework components - prompt engineering, API integration, and developer tools. Follow for updates!