INTERFACE zif_llm_log
  PUBLIC .


  TYPES:
    BEGIN OF ts_msg,
      free_text_msg TYPE string,
      exception     TYPE REF TO cx_root,
      callback_fm   TYPE string,
      importance    TYPE balprobcl.
      INCLUDE       TYPE symsg.
  TYPES: END OF ts_msg .
  TYPES:
    tt_msg TYPE STANDARD TABLE OF ts_msg WITH DEFAULT KEY .

  DATA gv_msg TYPE string .
  DATA mt_msg TYPE tt_msg .

  METHODS get_message_table
    RETURNING
      VALUE(rt_) TYPE tt_msg .
  METHODS get_flatten_table
    RETURNING
      VALUE(rt_) TYPE tt_msg .
  METHODS get_bapiret2_table
    RETURNING
      VALUE(rt_) TYPE bapiret2_t .

  METHODS get_converter
    RETURNING
      VALUE(ro_) TYPE REF TO zif_llm_log_converter.

  METHODS get_analyser
    RETURNING
      VALUE(ro_) TYPE REF TO zif_llm_log_analyser.

ENDINTERFACE.
