CLASS zcl_llm_00_predictoken DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    types: string_t TYPE  Zif_llm_00_types=>string_t.
    TYPES:
      BEGIN OF ts_coeff,
        text_length  TYPE decfloat16,
        words        TYPE decfloat16,
        punctuations TYPE decfloat16,
        numbers      TYPE decfloat16,
        whitespaces  TYPE decfloat16,
        lines        TYPE decfloat16,
        sentences    TYPE decfloat16,
        intercept    TYPE decfloat16,
      END OF ts_coeff .
    TYPES:
      BEGIN OF ts_features,
        text_length  TYPE i,
        words        TYPE i,
        punctuations TYPE i,
        numbers      TYPE i,
        whitespaces  TYPE i,
        lines        TYPE i,
        sentences    TYPE i,
      END OF ts_features .

    CONSTANTS:
      BEGIN OF gc_llm_type,
        gpt           TYPE string VALUE 'GPT',
        mistral       TYPE string VALUE 'MISTRAL',
        gpt_reasoning TYPE string value 'GPT_REASONING',
      END OF gc_llm_type .

    CLASS-METHODS class_constructor .
    CLASS-METHODS new
      IMPORTING
        !is_       TYPE ts_coeff OPTIONAL
        !iv_min    TYPE i OPTIONAL
          PREFERRED PARAMETER !is_
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_predictoken .
    CLASS-METHODS new_for_model_type
      IMPORTING
        !iv_       TYPE string DEFAULT gc_llm_type-gpt
        !iv_min    TYPE i OPTIONAL
          PREFERRED PARAMETER !iv_
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_predictoken .
    METHODS predict
      IMPORTING
        !iv_       TYPE string
      RETURNING
        VALUE(rv_) TYPE i .
    METHODS predict_for_itab
      IMPORTING
        !it_       TYPE string_t
      RETURNING
        VALUE(rv_) TYPE i .
    CLASS-METHODS extract_features
      IMPORTING
        !iv_       TYPE string
      RETURNING
        VALUE(rs_) TYPE ts_features .
    CLASS-METHODS new_for_llm
      IMPORTING
        !io_llm    TYPE REF TO zif_llm_00_llm_lazy
        !iv_min    TYPE i OPTIONAL
          PREFERRED PARAMETER !io_llm
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_predictoken .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_ TYPE ts_coeff .
    DATA mv_min TYPE i .  "minimal prediction (max( abs (intercept), 1 )
    CLASS-DATA gs_co_gpt TYPE ts_coeff .
    CLASS-DATA gs_co_mistral TYPE ts_coeff .

    METHODS constructor
      IMPORTING
        !is_    TYPE ts_coeff
        !iv_min TYPE i .
    METHODS predict_tokens
      IMPORTING
        !is_       TYPE ts_features
      RETURNING
        VALUE(rv_) TYPE i .
ENDCLASS.



CLASS ZCL_LLM_00_PREDICTOKEN IMPLEMENTATION.


  METHOD class_constructor.
* Coefficients: [ 0.0610605   0.5011021   0.93815106  1.16759457 -0.01792212  4.63131677 -1.67103032]
* Intercept: -28.82816309097143
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

* Coefficients: [ 0.18695786 -0.28615407 0.52599872 1.80894008 -0.02379413 5.65761217 0.34037584]
* Intercept: -56.52809407193536

    gs_co_mistral  = VALUE #(
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


  METHOD constructor.
    ms_    = is_.

    IF iv_min IS INITIAL.
      mv_min = COND i(
        WHEN abs( ms_-intercept ) > 0 THEN abs( ms_-intercept )
        ELSE 1
      ).
    ELSE.
      mv_min = iv_min.
    ENDIF.
  ENDMETHOD.


  METHOD extract_features.
    rs_-text_length = strlen( iv_ ).

    " Word Count
    DATA word_regex TYPE string VALUE `(\w+'*\w*|\w)`.
    rs_-words = count( val = iv_ regex = word_regex ).

    " Punctuation Count
    "DATA punc_regex TYPE string VALUE `[,.!?']`.
    DATA punc_regex TYPE string VALUE  `[.,;:!?'"]`.
    rs_-punctuations = count( val = iv_ regex = punc_regex ).

    " Number Count
    DATA number_regex TYPE string VALUE `\d`.
    rs_-numbers = count( val = iv_ regex = number_regex ).

    " Whitespace Count
    DATA whitespace_regex TYPE string VALUE `\s`.
    rs_-whitespaces = count( val = iv_ regex = whitespace_regex ).
    "calculate lines in text
    rs_-lines = count( val = iv_ sub = cl_abap_char_utilities=>newline ) + 1.
*    FIND ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN iv_ MATCH COUNT DATA(lv_lines).
*    rs_-lines = lv_lines + 1.

    " Sentence Count
    DATA sentence_regex TYPE string VALUE `(\.\s*|\?\s*|!\s*)`.
    rs_-sentences = count( val = iv_ regex = sentence_regex ) + 1.

  ENDMETHOD.


  METHOD new.
*--------------------------------------------------------------------*
    IF is_ IS NOT SUPPLIED.
      DATA(ls_) = gs_co_gpt.
    ELSE.
      ls_ = is_.
    ENDIF.
*--------------------------------------------------------------------*
    ro_ = NEW #(
      is_    = ls_
      iv_min = iv_min
    ).
  ENDMETHOD.


  METHOD new_for_llm.
    ro_ = new_for_model_type(
      iv_    = io_llm->get_config( )-model_type
      iv_min = iv_min
    ).
  ENDMETHOD.


  METHOD new_for_model_type.
    CASE iv_.
      WHEN gc_llm_type-mistral.
        ro_ = new(
          is_    = gs_co_mistral
          iv_min = iv_min
        ).
      WHEN gc_llm_type-gpt or gc_llm_type-gpt_reasoning.
        ro_ = new(
          is_    = gs_co_gpt
          iv_min = iv_min ).
      WHEN OTHERS.
        ro_ = new(
          is_    = gs_co_gpt
          iv_min = iv_min ).
    ENDCASE.
  ENDMETHOD.


  METHOD predict.
    DATA(ls_feat) = me->extract_features( iv_ ).
    rv_ = me->predict_tokens( ls_feat ).
  ENDMETHOD.


  METHOD predict_for_itab.
    DATA lv_ TYPE string.
    LOOP AT it_ REFERENCE INTO DATA(lr_).
      lv_ = COND #( WHEN sy-index = 1 THEN lr_->*
                    ELSE lv_ && cl_abap_char_utilities=>newline ).
    ENDLOOP.
    rv_ = me->predict( lv_ ).
  ENDMETHOD.


  METHOD predict_tokens.
    DATA lv_sum TYPE decfloat34.
    lv_sum  = ceil(
      ms_-intercept +
      ms_-text_length  * is_-text_length +
      ms_-words        * is_-words +
      ms_-punctuations * is_-punctuations +
      ms_-numbers      * is_-numbers +
      ms_-whitespaces  * is_-whitespaces +
      ms_-lines        * is_-lines +
      ms_-sentences    * is_-sentences
    ).

    rv_ = COND #(
      WHEN lv_sum > mv_min THEN lv_sum
      ELSE mv_min
    ).

  ENDMETHOD.
ENDCLASS.
