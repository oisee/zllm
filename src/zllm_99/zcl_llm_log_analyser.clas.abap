class ZCL_LLM_LOG_ANALYSER definition
  public
  create protected .

public section.

  interfaces zif_llm_log_analyser .

  aliases GET_WORST_BAPIRET2
    for zif_llm_log_analyser~GET_WORST_BAPIRET2 .
  aliases GET_WORST_MESSAGE
    for zif_llm_log_analyser~GET_WORST_MESSAGE .
  aliases GET_WORST_MESSAGE_TYPE
    for zif_llm_log_analyser~GET_WORST_MESSAGE_TYPE .
  aliases HAS_ERROR
    for zif_llm_log_analyser~HAS_ERROR .
  aliases HAS_SUCCESS_ONLY
    for zif_llm_log_analyser~HAS_SUCCESS_ONLY .
  aliases HAS_WARNING_OR_BETTER
    for zif_llm_log_analyser~HAS_WARNING_OR_BETTER .

  class-methods NEW
    importing
      !IO_LOG type ref to zif_llm_log
    returning
      value(RO_) type ref to ZCL_LLM_LOG_ANALYSER .
  PROTECTED SECTION.
    DATA: mo_log TYPE REF TO zif_llm_log.
    METHODS constructor IMPORTING io_log TYPE REF TO zif_llm_log.
private section.

  aliases HAS_ANY
    for zif_llm_log_analyser~HAS_ANY .
  aliases HAS_WARNING
    for zif_llm_log_analyser~HAS_WARNING .
ENDCLASS.



CLASS ZCL_LLM_LOG_ANALYSER IMPLEMENTATION.


  METHOD constructor.
    mo_log = io_log.
  ENDMETHOD.


  METHOD new.
    ro_ = NEW #( io_log ).
  ENDMETHOD.


  METHOD zif_llm_log_analyser~get_worst_bapiret2.
    DATA(ls_) = me->zif_llm_log_analyser~get_worst_message(  ).
    rs_-type    = ls_-msgty.
    rs_-id         = ls_-msgid.
    rs_-number     = ls_-msgno.
    rs_-message_v1 = ls_-msgv1.
    rs_-message_v2 = ls_-msgv2.
    rs_-message_v3 = ls_-msgv3.
    rs_-message_v4 = ls_-msgv4.
  ENDMETHOD.


  METHOD zif_llm_log_analyser~get_worst_message.
    DATA(lt_msg) = mo_log->get_flatten_table(  ).
    READ TABLE lt_msg INTO rs_ WITH KEY msgty = 'X'.
    CHECK sy-subrc NE 0.
    READ TABLE lt_msg INTO rs_ WITH KEY msgty = 'A'.
    CHECK sy-subrc NE 0.
    READ TABLE lt_msg INTO rs_ WITH KEY msgty = 'E'.
    CHECK sy-subrc NE 0.
    READ TABLE lt_msg INTO rs_ WITH KEY msgty = 'W'.
    CHECK sy-subrc NE 0.
    READ TABLE lt_msg INTO rs_ WITH KEY msgty = 'I'.
    CHECK sy-subrc NE 0.
    READ TABLE lt_msg INTO rs_ WITH KEY msgty = 'S'.
    CHECK sy-subrc NE 0.

  ENDMETHOD.


  METHOD zif_llm_log_analyser~get_worst_message_type.
    DATA(ls_msg) = me->zif_llm_log_analyser~get_worst_message(  ).
    rv_ = ls_msg-msgty.
  ENDMETHOD.


  METHOD zif_llm_log_analyser~has_any.

    DATA(lt_msg) = mo_log->get_flatten_table( ).

    IF lt_msg IS INITIAL.
      RETURN.
    ENDIF.

    rv_ = abap_true.
  ENDMETHOD.


  METHOD zif_llm_log_analyser~has_error.
    CASE me->zif_llm_log_analyser~get_worst_message_type(  ).
      WHEN 'X' OR 'A' OR 'E'.
        rv_ = abap_true.
      WHEN OTHERS.
        CLEAR rv_.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_llm_log_analyser~has_success_only.
    CASE me->zif_llm_log_analyser~get_worst_message_type(  ).
      WHEN 'S'.
        rv_ = abap_true.
      WHEN OTHERS.
        CLEAR rv_.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_llm_log_analyser~has_warning.
    CASE me->zif_llm_log_analyser~get_worst_message_type(  ).
      WHEN 'W'.
        rv_ = abap_true.
      WHEN OTHERS.
        CLEAR rv_.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_llm_log_analyser~has_warning_or_better.
    CASE me->zif_llm_log_analyser~get_worst_message_type(  ).
      WHEN 'W' OR 'I' OR 'S'.
        rv_ = abap_true.
      WHEN OTHERS.
        CLEAR rv_.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_llm_log_analyser~if_any_show_most_severe_as.

    IF me->has_any( ).
      DATA(ls_err) = me->get_worst_bapiret2( ).
      DATA(lv_type) = COND #( WHEN iv_type IS INITIAL THEN COND sy-msgty( WHEN ls_err-type IS INITIAL THEN 'W'
                                                                          ELSE ls_err-type )
                              ELSE iv_type ).
      MESSAGE ID ls_err-id TYPE lv_type NUMBER ls_err-number WITH ls_err-message_v1 ls_err-message_v2 ls_err-message_v3 ls_err-message_v4.
    ENDIF.

  ENDMETHOD.


  METHOD zif_llm_log_analyser~IF_ERROR_SHOW_MOST_SEVERE_AS.

    IF me->has_error( ).
      DATA(ls_err) = me->get_worst_bapiret2( ).
      DATA(lv_type) = COND #( WHEN iv_type IS INITIAL THEN ls_err-type
                              ELSE iv_type ).
      MESSAGE ID ls_err-id TYPE lv_type
              NUMBER ls_err-number WITH ls_err-message_v1 ls_err-message_v2 ls_err-message_v3 ls_err-message_v4.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
