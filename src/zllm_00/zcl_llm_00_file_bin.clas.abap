class ZCL_LLM_00_FILE_BIN definition
  public
  inheriting from ZCL_LLM_00_FILE
  create private .

public section.

  methods CONSTRUCTOR
    importing
      !IV_BIN type STRING
      !IV_NAME type STRING
      !IO_CODEC type ref to ZIF_LLM_00_CODEC .
  class-methods NEW
    importing
      !IV_BIN type STRING default ''
      !IV_NAME type STRING
      !IO_CODEC type ref to ZIF_LLM_00_CODEC optional
    returning
      value(RO_) type ref to ZIF_LLM_00_FILE .

  methods ZIF_LLM_00_FILE~GET_XSTRING
    redefinition .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_ TYPE zllm_00_bin.
    DATA mo_codec TYPE REF TO zif_llm_00_codec.
ENDCLASS.



CLASS ZCL_LLM_00_FILE_BIN IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
*--------------------------------------------------------------------*
    mo_codec = io_codec.
    SELECT SINGLE bin, name
      FROM zllm_00_bin
      WHERE bin = @iv_bin AND
            name = @iv_name
      INTO CORRESPONDING FIELDS OF @ms_.

    me->mv_path = ms_-name.
    me->mv_name = ms_-name.
  ENDMETHOD.


  METHOD new.
    IF io_codec IS BOUND.
      DATA(lo_codec) = io_codec.
    ELSE.
*     lo_codec = zcl_llm_00_codec_mock=>new( ).
      lo_codec = zcl_llm_00_codec=>new( ).
    ENDIF.

    ro_ ?= NEW zcl_llm_00_file_bin(
      iv_bin   = iv_bin
      iv_name  = iv_name
      io_codec = lo_codec
    ).
  ENDMETHOD.


  METHOD zif_llm_00_file~get_xstring.

    SELECT SINGLE v
      FROM zllm_00_bin
      WHERE bin  = @ms_-bin AND
            name = @ms_-name
      INTO @rv_.

    IF sy-subrc ne 0.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    IF mo_codec IS BOUND.
      rv_ = mo_codec->decode( rv_ ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
