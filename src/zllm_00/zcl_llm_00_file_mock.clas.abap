class ZCL_LLM_00_FILE_MOCK definition
  public
  inheriting from ZCL_LLM_00_FILE
  create private .

public section.

  methods CONSTRUCTOR
    importing
      !IV_CONTENT type STRING
      !IV_PATH type STRING optional
    preferred parameter IV_CONTENT .
  class-methods NEW
    importing
      !IV_CONTENT type STRING
      !IV_PATH type STRING optional
    preferred parameter IV_CONTENT
    returning
      value(RO_) type ref to ZIF_LLM_00_FILE .

  methods ZIF_LLM_00_FILE~GET_STRING
    redefinition .
  methods ZIF_LLM_00_FILE~GET_XSTRING
    redefinition .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mv_content TYPE string.

ENDCLASS.



CLASS ZCL_LLM_00_FILE_MOCK IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mv_path = iv_path.
    mv_name = me->extract_name_from_path( mv_path ).
    mv_content = iv_content.
  ENDMETHOD.


  METHOD new.
    ro_ ?= NEW zcl_llm_00_file_mock(
      iv_content = iv_content
      iv_path    = iv_path
    ).
  ENDMETHOD.


  METHOD zif_llm_00_file~get_string.
    rv_ = mv_content.
  ENDMETHOD.


  METHOD zif_llm_00_file~get_xstring.
    rv_ = cl_abap_codepage=>convert_to( source = mv_content codepage = `UTF-8` ).
  ENDMETHOD.
ENDCLASS.
