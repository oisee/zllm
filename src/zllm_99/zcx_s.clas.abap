CLASS zcx_s DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PROTECTED .

  PUBLIC SECTION.
    TYPE-POOLS abap .

    INTERFACES if_t100_message .
    INTERFACES zif_llm_log .
    INTERFACES zif_llm_logger .

    ALIASES add FOR zif_llm_logger~add.
    ALIASES a FOR zif_llm_logger~a.
    ALIASES e FOR zif_llm_logger~e.
    ALIASES w FOR zif_llm_logger~w.
    ALIASES i FOR zif_llm_logger~i.
    ALIASES s FOR zif_llm_logger~s.
    ALIASES display FOR zif_llm_logger~display.
    ALIASES get_bapiret2_table FOR zif_llm_log~get_bapiret2_table .
    ALIASES get_flatten_table  FOR zif_llm_log~get_flatten_table .
    ALIASES get_message_table  FOR zif_llm_log~get_message_table .
    ALIASES get_converter      FOR zif_llm_log~get_converter .
    ALIASES get_analyser       FOR zif_llm_log~get_analyser .

    CLASS-METHODS new
      IMPORTING
        !io_       TYPE any OPTIONAL
        !iv_type   TYPE symsgty OPTIONAL
          PREFERRED PARAMETER io_
      RETURNING
        VALUE(ro_) TYPE REF TO zcx_s
      RAISING
        zcx_s .
    CLASS-METHODS raise
      IMPORTING
        !io_       TYPE any OPTIONAL
        !iv_type   TYPE symsgty OPTIONAL
          PREFERRED PARAMETER io_
      RETURNING
        VALUE(ro_) TYPE REF TO zcx_s
      RAISING
        zcx_s .
    METHODS throw
      RAISING
        zcx_s .

    METHODS if_message~get_longtext
        REDEFINITION .
    METHODS if_message~get_text
        REDEFINITION .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF hrpad_message_field_list_alike,
        scrrprfd TYPE scrrprfd.
    TYPES: END OF hrpad_message_field_list_alike .
    TYPES:
      BEGIN OF hrpad_message_alike,
        cause(32)    TYPE c,              "original: hrpad_message_cause
        detail_level TYPE ballevel.
        INCLUDE TYPE symsg .
      TYPES: field_list   TYPE STANDARD TABLE OF hrpad_message_field_list_alike
                     WITH NON-UNIQUE KEY scrrprfd.
    TYPES: END OF hrpad_message_alike .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !gv_msg   TYPE string OPTIONAL .
  PRIVATE SECTION.
    DATA minilog TYPE REF TO zcl_minilog.
    DATA logger_delegate TYPE REF TO zif_llm_logger.
    DATA log_delegate TYPE REF TO zif_llm_log.
ENDCLASS.



CLASS ZCX_S IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD if_message~get_longtext.
    result = super->if_message~get_longtext( preserve_newlines ).
  ENDMETHOD.


  METHOD if_message~get_text.
    result = me->minilog->if_message~get_text(  ).
  ENDMETHOD.


  METHOD new.
    ro_ = NEW #(  ).
    ro_->minilog = zcl_minilog=>new(  ).
    ro_->log_delegate = ro_->minilog.
    ro_->logger_delegate = ro_->minilog.
  ENDMETHOD.


  METHOD raise.
    ro_ = new(  ).
    ro_->zif_llm_logger~add( io_     = io_
                         iv_type = iv_type  ).

    RAISE EXCEPTION ro_.
  ENDMETHOD.


  METHOD throw.

    RAISE EXCEPTION me.

  ENDMETHOD.


  METHOD zif_llm_logger~a.
    me->logger_delegate->a( io_ ).
    ro_ = me.
  ENDMETHOD.


  METHOD zif_llm_logger~add.
    me->logger_delegate->add( io_     = io_
                              iv_type = iv_type ).
    ro_ = me.
  ENDMETHOD.


  METHOD zif_llm_logger~display.
    me->logger_delegate->display(  ).
  ENDMETHOD.


  METHOD zif_llm_logger~e.
    me->logger_delegate->e( io_ ).
    ro_ = me.
  ENDMETHOD.


  METHOD zif_llm_logger~i.
    me->logger_delegate->i( io_ ).
    ro_ = me.
  ENDMETHOD.


  METHOD zif_llm_logger~s.
    me->logger_delegate->s( io_ ).
    ro_ = me.
  ENDMETHOD.


  METHOD zif_llm_logger~w.
    me->logger_delegate->w( io_ ).
    ro_ = me.
  ENDMETHOD.


  METHOD zif_llm_log~get_analyser.
    ro_ = zcl_llm_log_analyser=>new( me ).
  ENDMETHOD.


  METHOD zif_llm_log~get_bapiret2_table.
    rt_ = me->log_delegate->get_bapiret2_table( ).
  ENDMETHOD.


  METHOD zif_llm_log~get_converter.
    ro_ = zcl_llm_log_converter=>new( me ).
  ENDMETHOD.


  METHOD zif_llm_log~get_flatten_table.
    rt_ = me->log_delegate->get_flatten_table( ).
  ENDMETHOD.


  METHOD zif_llm_log~get_message_table.
    rt_ = me->log_delegate->get_message_table( ).
  ENDMETHOD.
ENDCLASS.
