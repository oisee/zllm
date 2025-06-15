# PREDICTOKEN: Fast, Local Token Count Prediction for Enterprise AI Systems

*Train once in Python. Predict anywhere—in ABAP, offline, or on edge AI. How do you predict LLM token consumption without expensive API calls? Here's a machine learning approach that brings token count prediction directly into ABAP - and why this matters for any environment building LLM integrations.*

Hello, my name is Alice, and I've been working with ABAP/SAP for about two decades with a lifelong passion for performance optimization. Today I want to share a technical solution I developed about a year ago when LLM API costs were significantly higher and token prediction was even more crucial than today.

Let me show you =)

## TL;DR:
**GitHub**: [oisee/zllm](https://github.com/oisee/zllm) (see ZCL_LLM_00_PREDICTOKEN)  
**The Problem**: How to predict token counts without calling expensive tokenizer APIs  
**The Solution**: Linear regression trained on ABAP corpus, generating pure ABAP prediction code  
**The Innovation**: Machine learning model deployment in environments with zero native ML libraries  
**Use Cases**: SAP/ABAP systems, edge AI, air-gapped environments, cost-sensitive pipelines

## What Problem Are We Trying to Solve?

When building LLM integrations, especially in enterprise environments, you constantly face this dilemma:

**"How many tokens will this text consume?"**

The traditional approaches all have significant drawbacks:

1. **Call the tokenizer API first** - Adds 200-500ms latency + network overhead + additional cost
2. **Conservative estimation** - Wastes 30-50% of available context window  
3. **Character-based approximation** - Wildly inaccurate (can be off by 40%+)
4. **Send and pray** - Results in 10-15% failed requests and wasted API calls

This was especially painful ~1 year ago when:
- LLM API costs were significantly higher
- Every failed request due to token limits was expensive
- Context windows were smaller, making efficiency crucial
- Enterprise budgets for AI experimentation were tighter

For ABAP environments, this problem is amplified because **there are no native tokenizer libraries**. You can't just `pip install tiktoken` - you're stuck with external API calls for everything.

## Why This Matters Beyond ABAP

While I built this for SAP systems, the core challenge applies everywhere:

**Edge AI devices** - Limited connectivity, need local token prediction  
**Cost-sensitive environments** - Every API call matters  
**Real-time systems** - Can't afford tokenizer API latency  
**Air-gapped systems** - No external API access allowed  
**Legacy environments** - No modern ML libraries available

The technique is portable. As long as your environment supports basic string operations and math, this can be deployed without ML runtimes or external APIs.

The fundamental question: **Can you predict token counts using only basic math and text processing?**

## Validation: GOOD ENOUGH

The practicality of PREDICTOKEN is validated based on these criteria:

**Accuracy**: Prediction error should be < 10% for typical text (< 5% for code)  
**Performance**: Prediction time should be negligible (< 1ms)  
**Overestimation Bias**: Slight intentional overestimation (~2-3%) is built-in to avoid payload cutoff errors  
**Zero Dependencies**: Must work in any environment with basic arithmetic

These criteria ensure the system provides practical value without requiring external services or complex dependencies.

## Understanding the Data

Let's examine what we're predicting:

**Input**: Any text (ABAP code, documentation, mixed content)  
**Output**: Token count for specific LLM tokenizers (GPT, Mistral, etc.)  
**Challenge**: Different tokenizers produce different counts for identical input

Example with ABAP code:

```abap
" This ABAP code snippet:
DATA: lv_customer_id TYPE kunnr,
      lt_orders TYPE TABLE OF vbak.

SELECT * FROM vbak 
  INTO TABLE lt_orders
  WHERE kunnr = lv_customer_id.
```

**GPT-4 Tokenizer**: ~31 tokens  
**Mistral Tokenizer**: ~35 tokens  
**Character Count**: 156 characters  
**Word Count**: 15 words

Simple character/word ratios would give you estimates between 20-40 tokens - not precise enough for reliable payload sizing.

## The Machine Learning Approach

Instead of trying to reverse-engineer complex tokenizer algorithms, I took a feature-based regression approach:

### Feature Engineering

```abap
TYPES:
  BEGIN OF ts_features,
    text_length  TYPE i,     " Total character count
    words        TYPE i,     " Word boundaries (\w+'*\w*|\w)
    punctuations TYPE i,     " [.,;:!?'"] count
    numbers      TYPE i,     " \d count  
    whitespaces  TYPE i,     " \s count
    lines        TYPE i,     " Newline count + 1
    sentences    TYPE i,     " [.!?]\s* count + 1
  END OF ts_features.
```

These features capture the linguistic patterns that correlate with tokenization behavior:

- **Text length**: Base correlation with token count
- **Words**: Primary tokenization units
- **Punctuation**: Often becomes separate tokens  
- **Numbers**: Special tokenization rules
- **Whitespace**: Token boundary indicators
- **Lines**: Important for code structure
- **Sentences**: Semantic boundaries matter

### Training Pipeline

The training happens in Python, but deployment is pure ABAP:

1. **Corpus Collection**: Collected diverse ABAP code samples and documentation
2. **Ground Truth Generation**: Called actual tokenizer APIs to get true counts
3. **Feature Extraction**: Computed the 7 features for each sample
4. **Linear Regression**: Trained separate models per tokenizer
5. **Coefficient Export**: Generated ABAP constants from Python coefficients

The key insight: **Linear regression is simple enough to implement anywhere**.

## Implementation: From Python Training to ABAP Deployment

Here's the beautiful part. Python training produces these regression coefficients:

```python
# GPT-4 Model Training Results:
# Coefficients: [ 0.0610605   0.5011021   0.93815106  1.16759457 
#                -0.01792212  4.63131677 -1.67103032]
# Intercept: -28.82816309097143
# R²: 0.987 (excellent fit)

# Mistral Model Training Results:
# Coefficients: [ 0.18695786 -0.28615407  0.52599872  1.80894008 
#                -0.02379413  5.65761217  0.34037584]
# Intercept: -56.52809407193536
# R²: 0.981 (excellent fit)
```

Which automatically generate ABAP constants:

```abap
METHOD class_constructor.
  " Auto-generated from Python training
  gs_co_gpt = VALUE #(
    text_length  = '0.0610605'
    words        = '0.5011021'
    punctuations = '0.93815106'
    numbers      = '1.16759457'
    whitespaces  = '-0.01792212'
    lines        = '4.63131677'
    sentences    = '-1.67103032'
    intercept    = '-28.82816309097143'
  ).
  
  gs_co_mistral = VALUE #(
    text_length  = '0.18695786'
    words        = '-0.28615407'
    punctuations = '0.52599872'
    numbers      = '1.80894008'
    whitespaces  = '-0.02379413'
    lines        = '5.65761217'
    sentences    = '0.34037584'
    intercept    = '-56.52809407193536'
  ).
ENDMETHOD.
```

## Real-Time Prediction: Seven Operations

The actual prediction is remarkably simple:

```abap
METHOD predict_tokens.
  DATA lv_sum TYPE decfloat34.
  
  " Linear regression formula
  lv_sum = ceil(
    ms_-intercept +
    ms_-text_length  * is_-text_length +
    ms_-words        * is_-words +
    ms_-punctuations * is_-punctuations +
    ms_-numbers      * is_-numbers +
    ms_-whitespaces  * is_-whitespaces +
    ms_-lines        * is_-lines +
    ms_-sentences    * is_-sentences
  ).
  
  " Ensure minimum reasonable prediction
  rv_ = COND #(
    WHEN lv_sum > mv_min THEN lv_sum
    ELSE mv_min
  ).
ENDMETHOD.
```

**Seven multiplications, seven additions, one ceiling operation**. That's it.

Execution time: **< 1ms** on any modern system.

## Feature Extraction: The Real Work

The computational work happens in feature extraction:

```abap
METHOD extract_features.
  rs_-text_length = strlen( iv_ ).
  
  " Word Count using regex
  DATA word_regex TYPE string VALUE `(\w+'*\w*|\w)`.
  rs_-words = count( val = iv_ regex = word_regex ).
  
  " Punctuation Count  
  DATA punc_regex TYPE string VALUE `[.,;:!?'"]`.
  rs_-punctuations = count( val = iv_ regex = punc_regex ).
  
  " Number Count
  DATA number_regex TYPE string VALUE `\d`.
  rs_-numbers = count( val = iv_ regex = number_regex ).
  
  " Whitespace Count
  DATA whitespace_regex TYPE string VALUE `\s`.
  rs_-whitespaces = count( val = iv_ regex = whitespace_regex ).
  
  " Line count
  rs_-lines = count( val = iv_ sub = cl_abap_char_utilities=>newline ) + 1.
  
  " Sentence Count
  DATA sentence_regex TYPE string VALUE `(\.s*|\?s*|!s*)`.
  rs_-sentences = count( val = iv_ regex = sentence_regex ) + 1.
ENDMETHOD.
```

Even this is fast - regex operations complete in microseconds for typical text sizes.

## Why Linear Regression Works

You might wonder: "Why not use a neural network or transformer?"

For token count prediction, linear regression has unique advantages:

1. **Deployable Anywhere**: No ML runtime required
2. **Interpretable**: You can see which features matter most  
3. **Fast**: Constant-time prediction regardless of text length
4. **Reliable**: No black-box failures or edge cases
5. **Small**: Seven coefficients vs millions of parameters

The R² values (0.987 for GPT, 0.981 for Mistral) show that linear relationships capture most of the variance in token counts. While not perfect on edge cases (e.g., emojis, code-mixed text), it achieves excellent accuracy on structured and typical enterprise inputs like ABAP and documentation.

## Model Adaptation Strategy

The system supports multiple tokenizers through a simple factory pattern:

```abap
" Get predictor for specific model
DATA(lo_predictor) = zcl_llm_00_predictoken=>new_for_model_type( 
  iv_ = zcl_llm_00_predictoken=>gc_llm_type-gpt 
).

" Or auto-detect from LLM configuration  
DATA(lo_predictor) = zcl_llm_00_predictoken=>new_for_llm( io_llm ).

" Make prediction
DATA(lv_tokens) = lo_predictor->predict( lv_text ).
```

Adding support for new tokenizers requires:
1. Collect training samples
2. Run Python regression training  
3. Add coefficients to ABAP constants
4. Update factory method

## Practical Applications

This approach enables several enterprise AI patterns:

**Smart Context Management**: Predict if additional context will fit  
**Batch Optimization**: Group requests optimally without exceeding limits  
**Cost Estimation**: Calculate API costs before making calls  
**Fallback Strategies**: Switch to smaller models when content is too large  
**Performance Optimization**: Avoid expensive tokenizer API calls

For edge AI or resource-constrained environments, this pattern could be even more valuable than in traditional cloud setups.

## Stats and Performance

**Accuracy**: 
- GPT models: < 5% error on 95% of samples
- Mistral models: < 7% error on 95% of samples  
- Overestimation bias: 2-3% (intentional safety margin)

**Performance**:
- Feature extraction: ~10-50μs (depending on text size)
- Prediction calculation: ~1μs
- Total overhead: Negligible compared to any network operation

**Memory footprint**: 
- Model coefficients: ~200 bytes
- Runtime overhead: Minimal (no caching needed)

## Future Enhancements

The framework could be extended for:

**Multi-language support**: Train on different programming languages  
**Domain-specific models**: Optimize for documentation vs code vs data  
**Dynamic retraining**: Update coefficients based on new tokenizer versions  
**Ensemble methods**: Combine multiple predictors for better accuracy

## Why This Matters

PREDICTOKEN demonstrates a broader principle: **You don't always need complex ML infrastructure to solve ML problems**.

Sometimes the right answer is:
- Train sophisticated models where you have resources
- Deploy simple models where you need reliability
- Bridge the gap with smart feature engineering

For enterprise environments with strict dependencies, air-gapped systems, or edge devices, this pattern of "train complex, deploy simple" could unlock many AI capabilities that would otherwise be impossible.

## Conclusion

PREDICTOKEN shows that effective AI integration doesn't require cutting-edge infrastructure everywhere. By training regression models offline and deploying them as simple arithmetic, you can bring intelligent token prediction to any environment.

Whether you're working with ABAP, embedded systems, legacy platforms, or just want to minimize API dependencies, the principle remains: **smart feature engineering + simple models can solve real problems**.

While this module is part of the larger ZLLM framework (to be released), it's designed to run standalone. The code is available as part of the ZLLM framework. Feel free to adapt the approach for your own tokenizer prediction challenges!

---

**About the Author:**

Alice Vinogradova is a Senior Software Engineer with two decades of ABAP/SAP experience and a passion for bridging traditional enterprise systems with modern AI capabilities. She specializes in performance optimization and practical AI integration patterns for conservative enterprise environments.

---

**P.S.**

(Brought to you by me in my spare time over weekends, my personal NetWeaver 2022 (Docker) SAP S/4HANA instance, and a couple of imaginary friends: Claude and ChatGPT =)

This continues my weekend exploration of enterprise AI in ABAP—previously I built [ZVDB](https://github.com/oisee/zvdb), a [pure ABAP vector database](https://www.linkedin.com/pulse/vector-db-pure-abap-any-alice-vinogradova-znjbe/) that achieved remarkable performance without external dependencies.

I originally wanted to release ZLLM about a year ago, but it always seemed "not good enough" for public eyes. Despite many weekend attempts to clean it up for release, I kept preferring to "just improve it a bit and add more features"—the classic developer perfectionism trap =)

So now I've decided to break the cycle: release ZLLM incrementally by submodules in the [oisee/zllm](https://github.com/oisee/zllm) repository, gradually filling the gaps. Better to share working solutions than perfect ones that never see daylight!