CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_llm_00_step_result DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zif_llm_00_step_result.  "class under test

    METHODS: setup.

    METHODS: collect FOR TESTING.
    METHODS: collect_chat FOR TESTING.
    METHODS: collect_embed FOR TESTING.
    METHODS: collect_raw FOR TESTING.
    METHODS: new FOR TESTING.
    METHODS: new_from_http FOR TESTING.
    METHODS: new_from_sting FOR TESTING.
    METHODS: new_from_table FOR TESTING.
    METHODS: _debug FOR TESTING.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.

  METHOD setup.
    mo_cut = zcl_llm_00_step_result=>new_from_string( 'Hi!' ).

*    DATA io_http TYPE REF TO if_http_client.
*    DATA io_json TYPE REF TO zif_llm_00_json.
*    DATA io_llm TYPE REF TO zif_llm_00_llm_lazy.
*    DATA iv_k TYPE string.
*    DATA iv_json_expected TYPE sap_bool.
*
*    CREATE OBJECT mo_cut
**     EXPORTING
**       IO_HTTP = io_Http
**       IO_JSON = io_Json
**       IO_LLM = io_Llm
**       IV_K = iv_K
**       IV_JSON_EXPECTED = iv_Json_Expected
*      .
  ENDMETHOD.


  METHOD collect.
*    DATA rr_ TYPE REF TO data.
*    rr_ = f_cut->zif_llm_00_step_result~collect(  ).
  ENDMETHOD.


  METHOD collect_chat.
*    DATA ro_ TYPE REF TO zcl_llm_00_chat_out.
*    ro_ = f_cut->zif_llm_00_step_result~collect_chat(  ).
  ENDMETHOD.


  METHOD collect_embed.
*    DATA ro_ TYPE REF TO zcl_llm_00_embed_out.
*    ro_ = f_cut->zif_llm_00_step_result~collect_embed(  ).
  ENDMETHOD.


  METHOD collect_raw.
*    DATA rv_ TYPE string.
*    rv_ = f_cut->zif_llm_00_step_result~collect_raw(  ).
  ENDMETHOD.


  METHOD new.

  ENDMETHOD.


  METHOD new_from_http.

  ENDMETHOD.


  METHOD new_from_sting.
    data(lo_) = zcl_llm_00_step_result=>new_from_string( 'Hi!' ).
    data(lv_res) = lo_->to_string( ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lv_res
        exp = 'Hi!'
    ).
  ENDMETHOD.


  METHOD new_from_table.
  ENDMETHOD.


  METHOD _debug.
  ENDMETHOD.



ENDCLASS.
