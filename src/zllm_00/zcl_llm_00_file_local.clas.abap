class ZCL_LLM_00_FILE_LOCAL definition
  public
  inheriting from ZCL_LLM_00_FILE
  create private .

public section.

  methods CONSTRUCTOR
    importing
      !IV_ type STRING .
  class-methods NEW
    importing
      !IV_ type STRING
    returning
      value(RO_) type ref to ZIF_LLM_00_FILE .

  methods ZIF_LLM_00_FILE~GET_XSTRING
    redefinition .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_LLM_00_FILE_LOCAL IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mv_path = iv_.
    FIND ALL OCCURRENCES OF '/' IN mv_path MATCH OFFSET DATA(lv_last_del_1).
    FIND ALL OCCURRENCES OF '\' IN mv_path MATCH OFFSET DATA(lv_last_del_2).
    DATA(lv_last_del) = COND i( WHEN lv_last_del_1 > lv_last_del_2 THEN lv_last_del_1
                                ELSE lv_last_del_2 ).

    IF lv_last_del = 0.
      me->mv_name = mv_path.
    ELSE.
      DATA(lv_off) = lv_last_del + 1.
      DATA(lv_name_len) = strlen( mv_path ) - lv_off.
      mv_name = mv_path+lv_off(lv_name_len).
    ENDIF.
  ENDMETHOD.


  METHOD new.
    ro_ ?= NEW zcl_llm_00_file_local( iv_ ).
  ENDMETHOD.


  METHOD zif_llm_00_file~get_xstring.
    DATA: lt_ TYPE STANDARD TABLE OF raw256.
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = mv_path
        filetype                = 'BIN'
      IMPORTING
        filelength              = DATA(lv_len)
      CHANGING
        data_tab                = lt_
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_len
      IMPORTING
        buffer       = rv_
      TABLES
        binary_tab   = lt_.

  ENDMETHOD.
ENDCLASS.
