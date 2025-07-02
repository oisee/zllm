CLASS zcl_llm_00_ted_event_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS new
      IMPORTING
        !io_ted    TYPE REF TO cl_gui_textedit
        !ir_text   TYPE REF TO string
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_ted_event_handler .
    METHODS update_from_ref IMPORTING !ir_text TYPE REF TO string.
    METHODS handle_f1
      FOR EVENT f1 OF cl_gui_textedit
      IMPORTING
        !sender .
    METHODS handle_double_click
      FOR EVENT dblclick OF cl_gui_textedit
      IMPORTING
        !sender .
    METHODS sync_text
      RETURNING
        VALUE(rv_) TYPE sap_bool .
    METHODS set_text
      IMPORTING
        !iv_ TYPE string .
    METHODS set_modified_to
      IMPORTING
        !iv_ TYPE sap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mo_ted  TYPE REF TO cl_gui_textedit.
    DATA: mr_text TYPE REF TO string.

ENDCLASS.



CLASS ZCL_LLM_00_TED_EVENT_HANDLER IMPLEMENTATION.


  METHOD handle_double_click.
    cl_gui_cfw=>set_new_ok_code( EXPORTING new_code = 'ZDBLCLK' ).
  ENDMETHOD.


  METHOD handle_f1.
* show help on markdown
    cl_gui_cfw=>set_new_ok_code( EXPORTING new_code = 'F1' ).
    cl_gui_cfw=>flush( ).
  ENDMETHOD.


  METHOD new.
    ro_ = NEW #( ).
    ro_->mo_ted  = io_ted.
    ro_->mr_text = ir_text.
  ENDMETHOD.


  METHOD set_modified_to.

    IF iv_ = abap_true.
      DATA(lv_) = 1.
    ENDIF.

    mo_ted->set_textmodified_status(
      EXPORTING
        status                 = lv_
      EXCEPTIONS
        error_cntl_call_method = 1     " Error while setting property of TextEdit control
    ).
    IF sy-subrc <> 0.
      "zcl_cpu=>ok( ).
    ENDIF.

  ENDMETHOD.


  METHOD set_text.

    mr_text->* = iv_.
    mo_ted->set_textstream(
      EXPORTING
        text                   = mr_text->* " Text as String with Carriage Returns and Linefeeds
      EXCEPTIONS
        error_cntl_call_method = 1    " Error Calling COM Method
        not_supported_by_gui   = 2    " Method is not supported by installed GUI
    ).
    IF sy-subrc <> 0.
      "zcl_cpu=>ok( ).
    ENDIF.

  ENDMETHOD.


  METHOD sync_text.
    IF mo_ted IS NOT BOUND.
      RETURN.
    ENDIF.
    IF mr_text IS NOT BOUND.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------
    mo_ted->get_textstream(
      IMPORTING
        text                   = mr_text->*  " Text as String with Carriage Returns and Linefeeds
        is_modified            = DATA(lv_is_modified) " modify status of text
      EXCEPTIONS
        error_cntl_call_method = 1           " Error while retrieving a property from TextEdit control
        not_supported_by_gui   = 2           " Method is not supported by installed GUI
        OTHERS                 = 3
    ).
    IF sy-subrc <> 0.
      "zcl_cpu=>ok( ).
    ENDIF.

    IF lv_is_modified = 1.
      rv_ = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD update_from_ref.
    mr_text = ir_text.
    mo_ted->set_textstream(
      EXPORTING
        text                   = mr_text->* " Text as String with Carriage Returns and Linefeeds
      EXCEPTIONS
        error_cntl_call_method = 1    " Error Calling COM Method
        not_supported_by_gui   = 2    " Method is not supported by installed GUI
    ).
    IF sy-subrc <> 0.
      "zcl_cpu=>ok( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
