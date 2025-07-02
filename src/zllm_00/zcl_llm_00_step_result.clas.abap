class ZCL_LLM_00_STEP_RESULT definition
  public
  final
  create private .

public section.

  interfaces ZIF_LLM_00_STRING .
  interfaces ZIF_LLM_00_STEP_RESULT .

  class-methods NEW
    importing
      !IO_LLM type ref to ZIF_LLM_00_LLM_LAZY
      !IO_JSON type ref to ZIF_LLM_00_JSON
      !IV_K type STRING optional
      !IV_JSON_EXPECTED type SAP_BOOL optional
    returning
      value(RO_) type ref to ZIF_LLM_00_STEP_RESULT
    raising
      ZCX_S .
  class-methods NEW_FROM_HTTP
    importing
      !IO_HTTP type ref to IF_HTTP_CLIENT
      !IO_LLM type ref to ZIF_LLM_00_LLM_LAZY
      !IV_JSON_EXPECTED type SAP_BOOL optional
    returning
      value(RO_) type ref to ZIF_LLM_00_STEP_RESULT
    raising
      ZCX_S .
  class-methods NEW_FROM_STRING
    importing
      !IV_ type STRING
      !IV_JSON_EXPECTED type SAP_BOOL optional
    returning
      value(RO_) type ref to ZIF_LLM_00_STEP_RESULT
    raising
      ZCX_S .
  class-methods NEW_FROM_TABLE
    importing
      !IT_ type ANY TABLE
      !IV_JSON_EXPECTED type SAP_BOOL optional
    returning
      value(RO_) type ref to ZIF_LLM_00_STEP_RESULT
    raising
      ZCX_S .
  class-methods _DEBUG
    importing
      !IV_ type SAP_BOOL .
  class-methods NEW_FROM_STRUCTURE
    importing
      !IS_ type ANY
      !IV_JSON_EXPECTED type SAP_BOOL default 'X'
    returning
      value(RO_) type ref to ZIF_LLM_00_STEP_RESULT
    raising
      ZCX_S .
  PROTECTED SECTION.
private section.

  class-data MV_DEBUG type SAP_BOOL .
  data MO_JSON type ref to ZIF_LLM_00_JSON .
*    DATA mo_http      TYPE REF TO if_http_client .
  data MO_LLM type ref to ZIF_LLM_00_LLM_LAZY .
  data MV_RES type STRING .
  data MV_RES_JSON type STRING .
  data MR_RES type ref to DATA .
  data MV_JSON_EXPECTED type SAP_BOOL .
  data MO_LLM_RESPONSE type ref to ZIF_LLM_00_LLM_RESPONSE .

  methods CONSTRUCTOR
    importing
      !IO_HTTP type ref to IF_HTTP_CLIENT optional
      !IO_JSON type ref to ZIF_LLM_00_JSON optional
      !IO_LLM type ref to ZIF_LLM_00_LLM_LAZY optional
      !IV_K type STRING optional
      !IV_JSON_EXPECTED type SAP_BOOL optional
      !IV_V type STRING optional
    raising
      ZCX_S .
ENDCLASS.



CLASS ZCL_LLM_00_STEP_RESULT IMPLEMENTATION.


  METHOD constructor.
*   mo_http          = io_http.
    mo_llm           = io_llm.
    mo_json          = io_json.
    mv_json_expected = iv_json_expected.

    IF iv_v IS SUPPLIED.
      mv_res = iv_v.
      mr_res = REF #( mv_res ).
    ENDIF.

    IF mo_llm IS BOUND.
      mo_llm_response = mo_llm->q(
        io_  = mo_json
        iv_k = iv_k
      ).
    ENDIF.
  ENDMETHOD.


  METHOD new.
    ro_ ?= NEW zcl_llm_00_step_result(
      iv_k    = iv_k
      io_json = io_json
      io_llm  = io_llm
      iv_json_expected = iv_json_expected
    ).
  ENDMETHOD.


  METHOD new_from_http.
    ro_ ?= NEW zcl_llm_00_step_result(
      io_http = io_http
      io_llm  = io_llm
      iv_json_expected = iv_json_expected
    ).
  ENDMETHOD.


  METHOD NEW_FROM_STRING.
    ro_ ?= NEW zcl_llm_00_step_result(
      iv_k             = iv_
      iv_v             = iv_
      iv_json_expected = iv_json_expected
    ).
  ENDMETHOD.


  METHOD new_from_table.
    DATA(lo_) = NEW zcl_llm_00_step_result(
      iv_json_expected = iv_json_expected
    ).
    lo_->mr_res = REF #( it_ ).
    ro_ ?= lo_.
  ENDMETHOD.


  METHOD zif_llm_00_step_result~collect.
    IF mr_res IS BOUND.
      rr_ = mr_res.
      RETURN.
    ENDIF.

    data(lo_payload_adapter) = mo_llm->get_payload_adapter( ).
    mv_res = lo_payload_adapter->output( mo_llm_response->v( ) ).

*    DATA(lo_cc) = me->zif_llm_00_step_result~collect_chat( ).
*    mv_res = lo_cc->get_reply( ).

    IF mv_json_expected = abap_true.
      IF mv_debug = abap_true.
        cl_demo_output=>display( mv_res ).
      ENDIF.
      mr_res = zcl_llm_00_json=>generate( mv_res ).
      IF zcl_llm_00_json=>is_structure( mr_res  ) AND
         zcl_llm_00_json=>has_field( mr_res ).
        IF zcl_llm_00_json=>field_unpacked(
            EXPORTING ir_ = mr_res
            IMPORTING er_ = mr_res
        ).
        ENDIF.
      ENDIF.
      mr_res = zcl_llm_00_json=>flatten_dref( mr_res ).
    ELSE.
      mr_res = REF #( mv_res ).
    ENDIF.

    rr_ = mr_res.
  ENDMETHOD.


  METHOD zif_llm_00_step_result~collect_embed.
*    DATA(lv_json) =  mo_llm->receive( mo_http ).
    DATA(lv_json) = mo_llm_response->v( ).
    ro_ = zcl_llm_00_embed_out=>new_from_json( lv_json ).
  ENDMETHOD.


  METHOD zif_llm_00_step_result~collect_raw.
    "mv_res_json = mo_llm->receive( mo_http ).
    mv_res_json = mo_llm_response->v( ).
    rv_ = mv_res_json.
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
    IF mv_res IS NOT INITIAL.
      rv_ = mv_res.
      RETURN.
    ENDIF.
    TRY.
        IF mr_res IS NOT BOUND.
          mr_res = me->zif_llm_00_step_result~collect( ).
        ENDIF.
      CATCH zcx_s INTO DATA(lx_s).
        cl_demo_output=>display( lx_s->get_text( ) && zif_llm=>n && lx_s->get_longtext( ) ).
        "zcl_cpu=>ok( lx_s ).
    ENDTRY.
    mv_res = zcl_llm_00_string=>new( mr_res )->to_string( ).
    rv_ = mv_res.
  ENDMETHOD.


  METHOD _debug.
    mv_debug = iv_.
  ENDMETHOD.


  METHOD NEW_FROM_STRUCTURE.
    DATA(lo_) = NEW zcl_llm_00_step_result(
      iv_json_expected = iv_json_expected
    ).
    lo_->mr_res = REF #( is_ ).
    ro_ ?= lo_.
  ENDMETHOD.


  method ZIF_LLM_00_STEP_RESULT~IS_JSON.

    rv_ = mv_json_expected.

  endmethod.
ENDCLASS.
