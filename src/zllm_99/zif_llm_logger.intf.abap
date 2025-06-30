INTERFACE zif_llm_logger
  PUBLIC .

  types: ts_msg TYPE zif_llm_log=>ts_msg.
  types: tt_msg TYPE zif_llm_log=>tt_msg.

  DATA gv_msg TYPE string .
  DATA mt_msg TYPE tt_msg .

  METHODS display
    IMPORTING iv_mode TYPE csequence OPTIONAL.

  METHODS add
    IMPORTING
      !io_            TYPE any       OPTIONAL
      !iv_type        TYPE symsgty   OPTIONAL
      !is_context     TYPE simple    OPTIONAL
      !iv_callback_fm TYPE csequence OPTIONAL
      !iv_importance  TYPE balprobcl OPTIONAL
        PREFERRED PARAMETER io_
    RETURNING
      VALUE(ro_)      TYPE REF TO zif_llm_logger .
  METHODS a
    IMPORTING
      !io_       TYPE any OPTIONAL
      !iv_type   TYPE symsgty DEFAULT 'A'
      !is_context     TYPE simple    OPTIONAL
      !iv_callback_fm TYPE csequence OPTIONAL
      !iv_importance  TYPE balprobcl OPTIONAL
        PREFERRED PARAMETER io_
    RETURNING
      VALUE(ro_) TYPE REF TO zif_llm_logger .
  METHODS e
    IMPORTING
      !io_       TYPE any OPTIONAL
      !iv_type   TYPE symsgty DEFAULT 'E'
      !is_context     TYPE simple    OPTIONAL
      !iv_callback_fm TYPE csequence OPTIONAL
      !iv_importance  TYPE balprobcl OPTIONAL
        PREFERRED PARAMETER io_
    RETURNING
      VALUE(ro_) TYPE REF TO zif_llm_logger .
  METHODS w
    IMPORTING
      !io_       TYPE any OPTIONAL
      !iv_type   TYPE symsgty DEFAULT 'W'
      !is_context     TYPE simple    OPTIONAL
      !iv_callback_fm TYPE csequence OPTIONAL
      !iv_importance  TYPE balprobcl OPTIONAL
        PREFERRED PARAMETER io_
    RETURNING
      VALUE(ro_) TYPE REF TO zif_llm_logger .
  METHODS i
    IMPORTING
      !io_       TYPE any OPTIONAL
      !iv_type   TYPE symsgty DEFAULT 'I'
      !is_context     TYPE simple    OPTIONAL
      !iv_callback_fm TYPE csequence OPTIONAL
      !iv_importance  TYPE balprobcl OPTIONAL
        PREFERRED PARAMETER io_
    RETURNING
      VALUE(ro_) TYPE REF TO zif_llm_logger .
  METHODS s
    IMPORTING
      !io_       TYPE any OPTIONAL
      !iv_type   TYPE symsgty DEFAULT 'S'
      !is_context     TYPE simple    OPTIONAL
      !iv_callback_fm TYPE csequence OPTIONAL
      !iv_importance  TYPE balprobcl OPTIONAL
        PREFERRED PARAMETER io_
    RETURNING
      VALUE(ro_) TYPE REF TO zif_llm_logger .
ENDINTERFACE.
