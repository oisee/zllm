# ZLLM Framework Guide

## Table of Contents
1. [Introduction](#introduction)
2. [Quick Start](#quick-start)
3. [Core Concepts](#core-concepts)
4. [Template Engine](#template-engine)
5. [Steps and Flows](#steps-and-flows)
6. [Advanced Features](#advanced-features)
7. [Demo Programs](#demo-programs)
8. [Best Practices](#best-practices)

## Introduction

The ZLLM Framework is a sophisticated "LangChain-lite" implementation for ABAP/SAP systems. It provides a complete toolkit for building LLM-powered applications with features like:

- **Lazy Execution**: Non-blocking async operations
- **Flow Orchestration**: Chain multiple LLM calls with result propagation
- **Template Engine**: Powerful pattern system for prompt engineering
- **Cache System**: Built-in caching for performance
- **Load Balancing**: Intelligent routing between multiple LLM models
- **Parallel Processing**: Execute multiple operations concurrently

## Quick Start

### Basic LLM Query

```abap
" Create LLM instance
DATA(lo_llm) = zcl_llm=>new( 'DEFAULT.ENV' ).

" Create a simple step
DATA(lo_step) = zcl_llm_00_step_lazy=>new_from_string(
  iv_usr = 'Explain quantum computing in simple terms'
  io_llm = lo_llm
).

" Execute and get result
DATA(lr_result) = lo_step->exec( ).
```

### Two-Step Flow

```abap
" Step 1: Generate content
DATA(lo_step1) = zcl_llm_00_step_lazy=>new_from_string(
  iv_usr = 'Write a haiku about programming'
  io_llm = lo_llm
).

" Step 2: Analyze previous result
DATA(lo_step2) = zcl_llm_00_step_lazy=>new_from_string(
  iv_usr = 'Explain the meaning of this haiku: {T}'
  io_llm = lo_llm
).

" Create flow
DATA(lo_flow) = zcl_llm_00_flow_lazy=>new(
  VALUE #( ( lo_step1 ) ( lo_step2 ) )
).

" Execute flow
DATA(lo_result) = lo_flow->exec( ).
```

## Core Concepts

### 1. Lazy Execution Model

The framework uses lazy evaluation for optimal performance:

```abap
" Start operation (non-blocking)
DATA(lo_result) = lo_step->start( ir_input ).

" Do other work...

" Collect result when needed (blocking)
DATA(lr_data) = lo_result->collect( ).
```

### 2. Steps

Steps are the basic unit of LLM interaction:

```abap
" From string
DATA(lo_step) = zcl_llm_00_step_lazy=>new_from_string(
  iv_usr = 'Your prompt'
  iv_sys = 'System prompt (optional)'
  io_llm = lo_llm
).

" From pattern
DATA(lo_pattern) = zcl_llm_00_pat=>new( 'Process this data: {T-FIELD}' ).
DATA(lo_step) = zcl_llm_00_step_lazy=>new_from_pat(
  io_pat_usr = lo_pattern
  io_llm     = lo_llm
).

" From formula (combines system and user prompts)
DATA(lo_formula) = zcl_llm_00_formula=>new_from_usr_and_sys(
  iv_user   = 'User prompt with {T}'
  iv_system = 'You are an expert assistant'
  iv_name   = 'my_formula'
).
DATA(lo_step) = zcl_llm_00_step_lazy=>new_from_formula(
  io_ = lo_formula
  io_llm = lo_llm
).
```

### 3. Flows

Flows chain multiple steps together:

```abap
" Sequential flow
DATA(lo_flow) = zcl_llm_00_flow_lazy=>new(
  VALUE #(
    ( lo_step1 )
    ( lo_step2 )
    ( lo_step3 )
  )
).

" From pattern list
DATA(lo_pat_list) = zcl_llm_00_pat_list=>new( ).
lo_pat_list->add( zcl_llm_00_pat=>new( 'First prompt' ) ).
lo_pat_list->add( zcl_llm_00_pat=>new( 'Second prompt using {T}' ) ).

DATA(lo_flow) = zcl_llm_00_flow_lazy=>new_from_pat_list(
  io_pat_list = lo_pat_list
  io_llm      = lo_llm
).
```

### 4. Composability

Flows can be used as steps in higher-order flows:

```abap
" Create sub-flows
DATA(lo_analysis_flow) = zcl_llm_00_flow_lazy=>new( VALUE #(
  ( lo_extract_entities )
  ( lo_sentiment_analysis )
) ).

DATA(lo_summary_flow) = zcl_llm_00_flow_lazy=>new( VALUE #(
  ( lo_summarize )
  ( lo_translate )
) ).

" Compose into main flow
DATA(lo_main_flow) = zcl_llm_00_flow_lazy=>new( VALUE #(
  ( lo_analysis_flow->to_step( ) )
  ( lo_summary_flow->to_step( ) )
  ( lo_final_report )
) ).
```

## Template Engine

The template engine is the heart of result propagation between steps.

### Basic Substitution

```abap
DATA(lo_pat) = zcl_llm_00_pat=>new( 'Hello {T}!' ).
DATA(lv_result) = lo_pat->apply( REF #( 'World' ) ).
" Result: Hello World!
```

### Structure Access

```abap
" For structure: BEGIN OF ls_data, name TYPE string, age TYPE i, END OF ls_data.
DATA(lo_pat) = zcl_llm_00_pat=>new( 'Name: {T-NAME}, Age: {T-AGE}' ).
DATA(lv_result) = lo_pat->apply( REF #( ls_data ) ).
```

### Deep Structure Navigation

```abap
" Nested structures
DATA(lo_pat) = zcl_llm_00_pat=>new( 
  'Customer: {T-CUSTOMER-NAME}, Order: {T-ORDER-ID}' 
).

" JSON paths
DATA(lo_pat) = zcl_llm_00_pat=>new(
  'Characters: {T-CHARACTERS-NAME}, {T-CHARACTERS-ROLE}'
).
```

### Table Processing

The template engine automatically handles tables:

```abap
" Pattern with table placeholders
DATA(lo_pat) = zcl_llm_00_pat=>new( '| {T-COL1} | {T-COL2} |' ).

" Applied to table produces:
" | Value1A | Value2A |
" | Value1B | Value2B |
" | Value1C | Value2C |
```

### JSON Integration

Complex structures are automatically converted to JSON:

```abap
DATA(lo_step) = zcl_llm_00_step_lazy=>new_from_string(
  iv_usr = 'Analyze this data: {T}'
  io_llm = lo_llm
  iv_detect_json = 'X'  " Auto-detect and format JSON
).
```

## Steps and Flows

### Sequential Processing

```abap
" Step 1: Extract information
DATA(lo_extract) = zcl_llm_00_step_lazy=>new_from_string(
  iv_usr = 'Extract key points from this text: {T}'
  iv_sys = 'Return as JSON array'
  io_llm = lo_llm
).

" Step 2: Expand each point
DATA(lo_expand) = zcl_llm_00_step_lazy=>new_from_string(
  iv_usr = 'For each point in {T}, provide detailed explanation'
  io_llm = lo_llm
).

" Step 3: Generate summary
DATA(lo_summary) = zcl_llm_00_step_lazy=>new_from_string(
  iv_usr = 'Create executive summary from: {T}'
  io_llm = lo_llm
).

" Create flow
DATA(lo_flow) = zcl_llm_00_flow_lazy=>new(
  VALUE #( ( lo_extract ) ( lo_expand ) ( lo_summary ) )
).
```

### Parallel Processing

```abap
" Process multiple items in parallel
DATA(lo_parallel) = zcl_llm_00_step_lazy_parallel=>new(
  io_step = lo_analysis_step
  io_llm  = lo_llm
).

" Execute on table of inputs
DATA(lo_result) = lo_parallel->start( REF #( lt_documents ) ).
```

### Conditional Flows

```abap
" Dynamic flow construction based on conditions
DATA(lt_steps) = VALUE zif_llm_00_step_lazy=>tt_steps( ).

IF lv_need_translation = abap_true.
  APPEND lo_translate_step TO lt_steps.
ENDIF.

IF lv_need_summary = abap_true.
  APPEND lo_summary_step TO lt_steps.
ENDIF.

DATA(lo_flow) = zcl_llm_00_flow_lazy=>new( lt_steps ).
```

## Advanced Features

### 1. Cache System

```abap
" Create cache with custom seed
DATA(lo_cache) = zcl_llm_00_cache=>new( iv_seed = 12345 ).

" Create cached LLM
DATA(lo_llm_cached) = zcl_llm=>new( 
  iv_config = 'DEFAULT.ENV'
  io_cache  = lo_cache
).

" Subsequent identical requests will use cache
```

### 2. Load Balancing

Route requests to different models based on complexity:

```abap
" Create models
DATA(lo_llm_mini) = zcl_llm=>new( 'MINI.ENV' ).   " For simple tasks
DATA(lo_llm_maxi) = zcl_llm=>new( 'MAXI.ENV' ).   " For complex tasks

" Create composite with threshold
DATA(lo_llm_smart) = zcl_llm_00_llm_lazy_composite=>new(
  io_llm       = lo_llm_mini
  io_llm_exp   = lo_llm_maxi
  iv_threshold = 1000  " Token threshold
).

" Automatically routes to appropriate model
```

### 3. Token Prediction

Estimate token usage without calling LLM:

```abap
" Get token prediction
DATA(lv_tokens) = zcl_llm_00_predictoken=>predict_tokens_gpt4( lv_text ).

" Use for routing decisions
IF lv_tokens < 500.
  lo_llm = lo_llm_mini.
ELSE.
  lo_llm = lo_llm_maxi.
ENDIF.
```

### 4. Environment Configuration

```abap
" Load from .env file
DATA(lo_dotenv) = zcl_llm_00_dotenv=>new( 'DEFAULT.ENV' ).

" Access configuration
DATA(lv_model) = lo_dotenv->get( 'MODEL' ).
DATA(lv_temp)  = lo_dotenv->get( 'TEMPERATURE' ).
DATA(lv_limit) = lo_dotenv->get( 'MAX_TOKENS' ).
```

### 5. Custom Patterns

```abap
" Create pattern with custom delimiters
DATA(lo_pat) = zcl_llm_00_pat=>new(
  iv_             = 'Process <<T-DATA>>'
  iv_start_string = '<<'
  iv_end_string   = '>>'
  iv_root         = 'T'
).
```

## Demo Programs

### 1. ZLLM_00_ONBOARD - Setup and Testing

Run this first to set up your environment:
- Creates .env configuration files
- Tests LLM connectivity
- Validates different model variants

### 2. ZLLM_00_FLOW_DEMO - Basic Flow Example

Simple two-step flow demonstration:
- Generates creative content
- Processes and transforms it

### 3. ZLLM_00_REPL - Interactive Development

Advanced interactive environment:
- Four-step story generation workflow
- Pattern testing and debugging
- Cache integration
- HTML preview

### 4. ZLLM_00_SYNC - File Synchronization

Virtual filesystem sync with local folders (implementation details vary).

## Best Practices

### 1. Error Handling

```abap
TRY.
    DATA(lr_result) = lo_step->exec( ).
  CATCH zcx_llm INTO DATA(lx_error).
    " Handle error
    MESSAGE lx_error->get_text( ) TYPE 'E'.
ENDTRY.
```

### 2. Pattern Design

- Use meaningful placeholder names: `{T-CUSTOMER_NAME}` not `{T-X}`
- Keep patterns readable and maintainable
- Test patterns with sample data before using in flows

### 3. Performance Optimization

- Use caching for repeated queries
- Leverage parallel processing for batch operations
- Choose appropriate models based on task complexity
- Monitor token usage with prediction

### 4. Flow Design

- Keep steps focused on single responsibilities
- Use meaningful step names for debugging
- Test steps individually before combining
- Consider reusability when designing flows

### 5. Debugging

```abap
" Enable debug mode in REPL
" Use trace interfaces for execution monitoring
" Check step results at each stage
```

## Conclusion

The ZLLM Framework provides a powerful, enterprise-ready solution for integrating LLMs into SAP systems. Its lazy execution model, sophisticated template engine, and composable architecture make it ideal for building complex AI-powered workflows while maintaining clean, maintainable code.

For more examples, explore the demo programs and experiment with different patterns and flows to discover the full potential of the framework.