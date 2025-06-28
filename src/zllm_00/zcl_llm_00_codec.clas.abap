class ZCL_LLM_00_CODEC definition
  public
  final
  create private .

public section.

  interfaces ZIF_LLM_00_CODEC .

  methods CONSTRUCTOR
    importing
      !IV_ type XSTRING .
  class-methods NEW
    importing
      !IV_ type XSTRING optional
    returning
      value(RO_) type ref to ZIF_LLM_00_CODEC .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mv_ TYPE xstring.
    DATA: mv_seed TYPE i.
ENDCLASS.



CLASS ZCL_LLM_00_CODEC IMPLEMENTATION.


  METHOD constructor.
    mv_ = iv_.
    mv_seed = CONV i( mv_+0(2) ).
  ENDMETHOD.


  METHOD new.
    DATA: lv_val TYPE xuvalue.
    IF iv_ IS NOT INITIAL.
      "DATA(lv_) = iv_.
      DATA(lv_) = CONV xstring( zcl_llm=>xstring_hash( iv_ ) ).
    ELSE.
      GET PARAMETER ID 'ZLLM_CODEC' FIELD lv_val.
      DATA(lv_str) = CONV string( lv_val ).
*    IF strlen( lv_val ) < 4.
*      lv_val = lv_val && '0000'.
*      lv_val = lv_val+0(4).
*    ENDIF.
      "lv_ = CONV xstring( lv_val ).
      lv_ = CONV xstring( zcl_llm=>string_hash( lv_str ) ).
    ENDIF.
    ro_ ?= NEW zcl_llm_00_codec( lv_ ).

  ENDMETHOD.


  METHOD zif_llm_00_codec~decode.
    rv_ = me->zif_llm_00_codec~encode( iv_ ).
  ENDMETHOD.


  METHOD zif_llm_00_codec~encode.
    DATA(lv_mv_len) = xstrlen( mv_ ).
    DATA(lv_len) = xstrlen( iv_ ).
    DATA(lo_) = cl_abap_random=>create( mv_seed ).
    DATA(lv_position) = 0.
    DO lv_len TIMES.
      DATA(lv_circular) = lv_position MOD lv_mv_len.
      DATA(lv_random) = lo_->int( ).
      DATA: lv_b TYPE x.
      lv_b = lv_random.
      DATA(lv_a) = iv_+lv_position(1) BIT-XOR lv_b.
      lv_a = lv_a BIT-XOR mv_+lv_circular(1).
      rv_ = rv_ && lv_a.
      ADD 1 TO lv_position.
    ENDDO.

  ENDMETHOD.
ENDCLASS.
