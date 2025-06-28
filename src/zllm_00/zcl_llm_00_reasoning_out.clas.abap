CLASS zcl_llm_00_reasoning_out DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_types.
    INTERFACES zif_llm_00_json .

    ALIASES to_json
      FOR zif_llm_00_json~to_json .

    TYPES ts_ TYPE zif_llm_00_types=>ts_reasoning_out.

    CLASS-METHODS new
      IMPORTING
        !is_       TYPE ts_
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_reasoning_out
      RAISING
        zcx_s .
    CLASS-METHODS new_from_json
      IMPORTING
        !iv_       TYPE string
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_reasoning_out
      RAISING
        zcx_s.
    METHODS get_
      RETURNING
        VALUE(rs_) TYPE ts_ .
    METHODS get_reply
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS get_reasoning_effort
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS get_usage
      RETURNING
        VALUE(rs_) TYPE zif_llm_00_types=>ts_reasoning_usage .
    METHODS is_completed
      RETURNING
        VALUE(rv_) TYPE sap_bool .
    METHODS has_error
      RETURNING
        VALUE(rv_) TYPE sap_bool .
    METHODS get_error
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS get_reasoning_tokens
      RETURNING
        VALUE(rv_) TYPE i .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA gv_msg TYPE string .

    METHODS constructor
      IMPORTING
        !is_ TYPE ts_
      RAISING
        zcx_s .
    DATA: ms_ TYPE ts_.
ENDCLASS.



CLASS ZCL_LLM_00_REASONING_OUT IMPLEMENTATION.


  METHOD constructor.
    ms_ = is_.
  ENDMETHOD.


  METHOD get_.
    rs_ = ms_.
  ENDMETHOD.


  METHOD get_error.
    rv_ = ms_-error.
  ENDMETHOD.


  METHOD get_reasoning_effort.
    rv_ = ms_-reasoning-effort.
  ENDMETHOD.


  METHOD get_reasoning_tokens.
    rv_ = ms_-usage-output_tokens_details-reasoning_tokens.
  ENDMETHOD.


  METHOD get_reply.
    " Find the message type output item
    LOOP AT ms_-output INTO DATA(ls_output) WHERE type = 'message'.
      " Get the first text content from the message
      rv_ = VALUE #( ls_output-content[ 1 ]-text OPTIONAL ).
      RETURN.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_usage.
    rs_ = ms_-usage.
  ENDMETHOD.


  METHOD has_error.
    rv_ = boolc( ms_-error IS NOT INITIAL ).
  ENDMETHOD.


  METHOD is_completed.
    DATA(lv_status) = to_lower( ms_-status ).
    rv_ = boolc( lv_status = 'completed' ).
  ENDMETHOD.


  METHOD new.
    ro_ = NEW #( is_ ).
  ENDMETHOD.


  METHOD new_from_json.
    DATA: ls_ TYPE ts_.
    zcl_llm_00_json=>from_json(
      EXPORTING json = iv_
      CHANGING  data = ls_
    ).
    ro_ = NEW #( ls_ ).

    IF ls_-status IS INITIAL.
      RETURN.
    ENDIF.

    IF line_exists( ls_-output[ type = 'message' ] ).
      data(lv_has_message) = abap_true.
    ENDIF.

    IF lv_has_message = abap_false AND ls_-error IS INITIAL.
      MESSAGE w000(zcx_) WITH 'No message output found' INTO DATA(lv_msg).
      zcx_s=>raise( sy ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_llm_00_json~to_json.
    rv_ = zcl_llm_00_json=>to_json( ms_ ).
  ENDMETHOD.
ENDCLASS.
