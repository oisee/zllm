CLASS zcl_llm_00_text_format DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .
  "~ Class definition for ZCL_LLM_00_TEXT_FORMAT, which is a public, final class with private instantiation to control object creation.
  "~ This ABAP class ZCL_LLM_00_TEXT_FORMAT is designed for text formatting.
* It provides methods to format text sequences into lines with specified maximum lengths, add comments to each line,
* and check for sentence-ending punctuations. The class ensures controlled instantiation through a private constructor and a static NEW method.
* Key inputs include text sequences and optional maximum lengths, while outputs are formatted strings or tables of strings.
* The class is essential for applications requiring structured text formatting and commenting.

  PUBLIC SECTION.

    "~ Constant for newline character, used to separate lines in formatted text.
    CONSTANTS lc_n TYPE string VALUE cl_abap_char_utilities=>newline ##NO_TEXT.

    CLASS-METHODS new                                        "~ Static method to create a new instance of ZCL_LLM_00_TEXT_FORMAT with an optional maximum length parameter.
      IMPORTING
        !iv_max_len TYPE i DEFAULT 60
      RETURNING
        VALUE(ro_)  TYPE REF TO zcl_llm_00_text_format .
    METHODS format                                           "~ Method to format a given text sequence into a single string with line breaks.
      IMPORTING
        !iv_       TYPE csequence
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS is_eos                                           "~ Method to check if a given character sequence ends with a sentence-ending punctuation (e.g., '.', '!', '?').
      IMPORTING
        !iv_       TYPE csequence
      RETURNING
        VALUE(rv_) TYPE sap_bool .
    METHODS format_t                                         "~ Method to format a given text sequence into a table of strings, each representing a line.
      IMPORTING
        !iv_       TYPE csequence
      RETURNING
        VALUE(rt_) TYPE zcl_llm=>string_t .
    METHODS format_and_comment_t                             "~ Method to format a given text sequence into a table of strings with each line prefixed by a specified string.
      IMPORTING
        !iv_       TYPE csequence
        !iv_prefix TYPE string DEFAULT `"~ `
      RETURNING
        VALUE(rt_) TYPE zcl_llm=>string_t .
    METHODS format_and_comment                               "~ Method to format a given text sequence into a single string with line breaks and prefixed lines.
      IMPORTING
        !iv_       TYPE csequence
        !iv_prefix TYPE string DEFAULT `"~ `
      RETURNING
        VALUE(rv_) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_max_len TYPE i .

    METHODS constructor                                      "~ Constructor method to initialize the maximum length for text formatting.
      IMPORTING
        !iv_max_len TYPE i .
ENDCLASS.



CLASS zcl_llm_00_text_format IMPLEMENTATION.


  METHOD constructor.
    "~ Constructor method to initialize the instance variable mv_max_len with the provided maximum length value.
    " Constructor method to initialize the instance variables with the provided values.
    mv_max_len = iv_max_len.  " Set the maximum length
  ENDMETHOD.


  METHOD format.
    "~ Implementation of the FORMAT method to convert a text sequence into a formatted string with line breaks.

    rv_ = concat_lines_of( table = format_t( iv_ ) sep = lc_n ).       "~ Call to the FORMAT_T method to format the text sequence into a table of strings, then concatenate them into a single string with line breaks.

  ENDMETHOD.


  METHOD format_and_comment.
    "~ Implementation of the FORMAT_AND_COMMENT method to format a text sequence and add comments to each line.

    rv_ = concat_lines_of( "~ Call to the FORMAT_AND_COMMENT_T method to format the text sequence into a table of strings with comments.
      table = format_and_comment_t( iv_ = iv_ iv_prefix = iv_prefix )  "~ Pass the formatted table to CONCAT_LINES_OF to concatenate the lines into a single string with line breaks.
      sep   = lc_n
    ).

  ENDMETHOD.


  METHOD format_and_comment_t.
    "~ Implementation of the FORMAT_AND_COMMENT_T method to format a text sequence into a table of strings with prefixed comments.

    rt_ = format_t( iv_ ).                                 "~ Call to the FORMAT_T method to format the text sequence into a table of strings.
    "~ Loop through each line in the formatted table and add the prefix to each line.
    LOOP AT rt_ REFERENCE INTO DATA(lr_).
      lr_->* = iv_prefix && lr_->*.
    ENDLOOP.

  ENDMETHOD.


  METHOD format_t.
    "~ Implementation of the FORMAT_T method to split a text sequence into words and format them into lines based on the maximum length.
    DATA: lt_word         TYPE TABLE OF string,
          lv_current_line TYPE string,
          lv_current_len  TYPE i.

    "~ Split the input text sequence into words and store them in a table.
    SPLIT iv_ AT space INTO TABLE lt_word.

    "~ Loop through each word in the table to format them into lines.
    LOOP AT lt_word INTO DATA(lv_word).
      DATA(lv_word_len) = strlen( lv_word ).               "~ Calculate the length of the current word.

      " Handle long words (identifiers)
      "~ Check if the current word exceeds the maximum length.
      IF lv_word_len > mv_max_len.
        "~ If the current line is not empty, append it to the result table before adding the long word.
        IF lv_current_line IS NOT INITIAL.
          APPEND lv_current_line TO rt_.
        ENDIF.
        APPEND lv_word TO rt_.
        CLEAR: lv_current_line, lv_current_len.
        "~ Continue to the next word after handling a long word.
        CONTINUE.
      ENDIF.

      " Check if adding the word exceeds the max length
      "~ Check if adding the current word would exceed the maximum line length.
      IF lv_current_len + lv_word_len + 1 > mv_max_len.
        APPEND lv_current_line TO rt_.
        CLEAR: lv_current_line, lv_current_len.
      ENDIF.

      " Add word to current line
      "~ If the current line is not empty, add the current word to it; otherwise, start a new line with the current word.
      IF lv_current_line IS NOT INITIAL.
        lv_current_line = lv_current_line && ` ` && lv_word.
      ELSE.
        "~ Start a new line with the current word if the current line is empty.
        lv_current_line = lv_word.
      ENDIF.

      lv_current_len  = strlen( lv_current_line ).         "~ Update the length of the current line after adding the word.
    ENDLOOP.
*--------------------------------------------------------------------*
    " Add any remaining words
    "~ Append any remaining words to the result table after the loop.
    IF lv_current_line IS NOT INITIAL.
      APPEND lv_current_line TO rt_.
    ENDIF.

  ENDMETHOD.


  METHOD is_eos.
    "~ Implementation of the IS_EOS method to check if a text sequence ends with a sentence-ending punctuation.

    "~ Conditional expression to determine if the text sequence ends with a sentence-ending punctuation.
    rv_ = COND #(
 "~ Check if the text sequence contains any of the sentence-ending punctuations.
      WHEN iv_ CA '!.?' THEN abap_true
 "~ Return false if the text sequence does not end with a sentence-ending punctuation.
      ELSE abap_false
    ).

  ENDMETHOD.


  METHOD new.
    "~ Implementation of the NEW method to create a new instance of ZCL_LLM_00_TEXT_FORMAT.
    "~ Create a new instance of ZCL_LLM_00_TEXT_FORMAT with the provided maximum length.
    ro_ = NEW zcl_llm_00_text_format( iv_max_len ).
  ENDMETHOD.
ENDCLASS.
