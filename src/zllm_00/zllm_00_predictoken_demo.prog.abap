*&---------------------------------------------------------------------*
*& Report ZLLM_00_PREDICTOKEN_DEMO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zllm_00_predictoken_demo.

SELECTION-SCREEN BEGIN OF BLOCK b10.

  PARAMETERS: p_r1 RADIOBUTTON GROUP r1 DEFAULT 'X'.
  PARAMETERS: p_r2 RADIOBUTTON GROUP r1.

SELECTION-SCREEN END OF BLOCK b10.

AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'ONLI'.
      PERFORM go.
    WHEN OTHERS.
  ENDCASE.


FORM go.
  DATA: lt_query  TYPE STANDARD TABLE OF string.

  CALL FUNCTION 'TERM_CONTROL_EDIT'
    EXPORTING
      titel          = `Enter your query`
      langu          = sy-langu
    TABLES
      textlines      = lt_query
    EXCEPTIONS
      user_cancelled = 1
      OTHERS         = 2.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  DATA(lo_predictoken) = zcl_llm_00_predictoken=>new_for_model_type(
    iv_    = COND #( WHEN p_r1 = 'X' THEN 'GPT'
                     ELSE 'MISTRAL' )
  ).

*  APPEND LINES OF lt_query to lt_query.
*  APPEND LINES OF lt_query to lt_query.
*  APPEND LINES OF lt_query to lt_query.
*  APPEND LINES OF lt_query to lt_query.

  data(lv_tokens) = lo_predictoken->predict_for_itab( lt_query ) .

  cl_demo_output=>write( |Tokens predicted: { lv_tokens }| ).
  cl_demo_output=>display( lt_query ).

ENDFORM.
