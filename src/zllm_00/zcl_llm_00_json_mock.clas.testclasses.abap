CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_llm_00_json_mock DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zif_llm_00_json.

    METHODS: setup FOR TESTING RAISING cx_static_check.
    METHODS: test_new FOR TESTING.
    METHODS: test_get_json FOR TESTING.

ENDCLASS.

CLASS lcl_ IMPLEMENTATION.
  METHOD setup.
    mo_cut = zcl_llm_00_json_mock=>new( iv_ = '{"key":"value"}' ).
  ENDMETHOD.

  METHOD test_new.
    DATA(lo_json) = zcl_llm_00_json_mock=>new( iv_ = '{"new_key":"new_value"}' ).
    cl_abap_unit_assert=>assert_not_initial( act = lo_json ).
  ENDMETHOD.

  METHOD test_get_json.
    DATA(lv_json) = mo_cut->to_json( ).
    cl_abap_unit_assert=>assert_equals( act = lv_json exp = '{"key":"value"}' ).
  ENDMETHOD.

ENDCLASS.
