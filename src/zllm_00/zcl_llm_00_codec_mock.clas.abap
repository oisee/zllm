class ZCL_LLM_00_CODEC_MOCK definition
  public
  final
  create private .

public section.

  interfaces ZIF_LLM_00_CODEC .

  class-methods NEW
    returning
      value(RO_) type ref to ZIF_LLM_00_CODEC .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_LLM_00_CODEC_MOCK IMPLEMENTATION.


  METHOD new.
    ro_ ?= NEW zcl_llm_00_codec_mock( ).
  ENDMETHOD.


  METHOD zif_llm_00_codec~decode.
    rv_ = iv_.
  ENDMETHOD.


  METHOD zif_llm_00_codec~encode.
    rv_ = iv_.
  ENDMETHOD.
ENDCLASS.
