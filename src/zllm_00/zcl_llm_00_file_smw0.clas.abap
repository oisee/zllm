CLASS zcl_llm_00_file_smw0 DEFINITION
  PUBLIC
  INHERITING FROM zcl_llm_00_file
  CREATE PRIVATE .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !is_      TYPE wwwdatatab
        !io_codec TYPE REF TO zif_llm_00_codec .
    CLASS-METHODS new
      IMPORTING
        !iv_       TYPE string
        !io_codec  TYPE REF TO zif_llm_00_codec OPTIONAL
      RETURNING
        VALUE(ro_) TYPE REF TO zif_llm_00_file .

    METHODS zif_llm_00_file~get_xstring
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_ TYPE wwwdatatab .
    DATA mt_ TYPE STANDARD TABLE OF wwwparams .
    DATA mo_codec TYPE REF TO zif_llm_00_codec.
ENDCLASS.



CLASS ZCL_LLM_00_FILE_SMW0 IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
*--------------------------------------------------------------------*
    mo_codec = io_codec.
    ms_ = is_.
    SELECT *
      FROM wwwparams
      WHERE relid = @ms_-relid AND
            objid = @ms_-objid
      INTO TABLE @mt_.

    me->mv_path = condense( VALUE #( mt_[ name = 'filename' ]-value OPTIONAL ) &&
                            VALUE #( mt_[ name = 'fileextension' ]-value OPTIONAL ) ).

    "   me->mv_name = me->extract_name_from_path( mv_path ).
    me->mv_name = ms_-objid.
  ENDMETHOD.


  METHOD new.
    IF io_codec IS BOUND.
      DATA(lo_codec) = io_codec.
    ELSE.
*     lo_codec = zcl_llm_00_codec_mock=>new( ).
      lo_codec = zcl_llm_00_codec=>new( ).
    ENDIF.

    ro_ ?= NEW zcl_llm_00_file_smw0(
      is_      = VALUE #( relid = 'MI' objid = iv_ )
      io_codec = lo_codec
    ).
  ENDMETHOD.


  METHOD zif_llm_00_file~get_xstring.
    DATA: lt_ TYPE w3mimetabtype.

    " Read by key
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key    = ms_
      TABLES
        mime   = lt_
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lv_len) = VALUE i( mt_[ name = 'filesize' ]-value OPTIONAL ).

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_len
      IMPORTING
        buffer       = rv_
      TABLES
        binary_tab   = lt_.

    IF mo_codec IS BOUND.
      rv_ = mo_codec->decode( rv_ ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
