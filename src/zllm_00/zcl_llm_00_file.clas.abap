class ZCL_LLM_00_FILE definition
  public
  abstract
  create public .

public section.

  interfaces ZIF_LLM_00_FILE .
  PROTECTED SECTION.

    METHODS extract_name_from_path
      IMPORTING
        !iv_       TYPE string
      RETURNING
        VALUE(rv_) TYPE string .
    DATA mv_path TYPE string .
    DATA mv_name TYPE string .
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_LLM_00_FILE IMPLEMENTATION.


  METHOD extract_name_from_path.
    rv_ = iv_.
    IF iv_ IS INITIAL.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    FIND ALL OCCURRENCES OF '/' IN iv_ MATCH OFFSET DATA(lv_last_del_1).
    FIND ALL OCCURRENCES OF '\' IN iv_ MATCH OFFSET DATA(lv_last_del_2).
    DATA(lv_last_del) = COND i( WHEN lv_last_del_1 > lv_last_del_2 THEN lv_last_del_1
                                ELSE lv_last_del_2 ).
    IF lv_last_del = 0 AND
       iv_+lv_last_del(1) CN '/\'.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA(lv_off) = lv_last_del + 1.
    DATA(lv_name_len) = strlen( iv_ ) - lv_off.
    rv_ = iv_+lv_off(lv_name_len).
  ENDMETHOD.


  METHOD zif_llm_00_file~get_name.
    rv_ = mv_name.
  ENDMETHOD.


  METHOD zif_llm_00_file~get_string.
    DATA: lo_conv  TYPE REF TO cl_abap_conv_in_ce.
    lo_conv = cl_abap_conv_in_ce=>create(
      "encoding = '4110'
      input = me->zif_llm_00_file~get_xstring( )
    ).
    TRY.
        lo_conv->read( IMPORTING data = rv_ ).
      CATCH cx_sy_conversion_codepage INTO DATA(lx_).
    ENDTRY.
  ENDMETHOD.


  method ZIF_LLM_00_FILE~GET_XSTRING.
  endmethod.


  METHOD zif_llm_00_file~to_o_string.
    ro_ = zcl_llm_00_string=>new_from_string( me->zif_llm_00_file~get_string( ) ).
  ENDMETHOD.
ENDCLASS.
