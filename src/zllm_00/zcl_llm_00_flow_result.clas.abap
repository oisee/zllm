CLASS zcl_llm_00_flow_result DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_string .
    INTERFACES zif_llm_00_step_result .

    TYPES:
      tt_ TYPE STANDARD TABLE OF REF TO zif_llm_00_step_result WITH DEFAULT KEY .

    CLASS-METHODS new
      IMPORTING
        !it_       TYPE tt_
      RETURNING
        VALUE(ro_) TYPE REF TO zif_llm_00_step_result .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS constructor
      IMPORTING
        it_ TYPE tt_.

    DATA: mt_          TYPE tt_.
    DATA: mo_chat_out  TYPE REF TO zcl_llm_00_chat_out.
    DATA: mo_embed_out TYPE REF TO zcl_llm_00_embed_out.

    DATA: mv_res TYPE string.
    DATA: mt_res TYPE STANDARD TABLE OF REF TO data.
    DATA: mr_res TYPE REF TO data.
    DATA: mt_chat_res  TYPE TABLE OF REF TO zcl_llm_00_chat_out.
    DATA: mt_embed_res TYPE TABLE OF REF TO zcl_llm_00_embed_out.

ENDCLASS.



CLASS ZCL_LLM_00_FLOW_RESULT IMPLEMENTATION.


  METHOD constructor.
    mt_ = it_.
  ENDMETHOD.


  METHOD new.
    ro_ ?= NEW zcl_llm_00_flow_result(
      it_    = it_
    ).
  ENDMETHOD.


  METHOD zif_llm_00_step_result~collect.
    IF mr_res IS BOUND.
      rr_ = mr_res.
      RETURN.
    ENDIF.
    LOOP AT mt_ INTO DATA(lo_).
      APPEND lo_->collect( ) TO mt_res.
    ENDLOOP.
    mr_res = REF #( mt_res ).
    mr_res = zcl_llm_00_json=>flatten_dref( mr_res ).
    rr_    = mr_res.
  ENDMETHOD.


  METHOD zif_llm_00_string~predict_tokens.
    DATA(lv_) = me->zif_llm_00_string~to_string( ).
    IF lv_ IS INITIAL.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA(lo_string) = zcl_llm_00_string=>new( lv_ ).
    rv_ = lo_string->predict_tokens( ).
  ENDMETHOD.


  METHOD zif_llm_00_string~to_string.
*    TRY.
*        IF mr_res IS NOT BOUND.
*          mr_res = me->zif_llm_00_step_result~collect( ).
*        ENDIF.
*      CATCH zcx_s INTO DATA(lx_s).
*        zcl_cpu=>ok( lx_s ).
*    ENDTRY.
*    rv_ = zcl_llm_00_string=>new( mr_res )->to_string( ).

    IF mv_res IS NOT INITIAL.
      rv_ = mv_res.
      RETURN.
    ENDIF.
    TRY.
        IF mr_res IS NOT BOUND.
          mr_res = me->zif_llm_00_step_result~collect( ).
        ENDIF.
      CATCH zcx_s INTO DATA(lx_s).
        "zcl_cpu=>ok( lx_s ).
    ENDTRY.
    mv_res = zcl_llm_00_string=>new( mr_res )->to_string( ).
    rv_ = mv_res.

  ENDMETHOD.


  method ZIF_LLM_00_STEP_RESULT~IS_JSON.

    data(lr_) = me->zif_llm_00_step_result~collect( ).
    rv_ = zcl_llm_00_json=>is_structure( lr_ ).

  endmethod.
ENDCLASS.
