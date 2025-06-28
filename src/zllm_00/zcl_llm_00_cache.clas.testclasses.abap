CLASS lcl_ DEFINITION DEFERRED.

CLASS zcl_llm_00_cache DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zif_llm_00_cache.

    METHODS setup.
    METHODS teardown.
    METHODS test_new FOR TESTING.
    METHODS test_clear_cache FOR TESTING.
    METHODS test_clear_cache_for_seed FOR TESTING.
    METHODS test_get FOR TESTING.
    METHODS test_invalidate FOR TESTING.
    METHODS test_put FOR TESTING.
    METHODS test_set_default_seed FOR TESTING.
ENDCLASS.

CLASS lcl_ IMPLEMENTATION.

  METHOD setup.
    mo_cut = zcl_llm_00_cache=>new( ).
  ENDMETHOD.

  METHOD teardown.
    CLEAR: mo_cut.
  ENDMETHOD.

  METHOD test_new.
    DATA(lo_cut) = zcl_llm_00_cache=>new( ).
    cl_abap_unit_assert=>assert_not_initial( act = lo_cut ).
  ENDMETHOD.

  METHOD test_clear_cache.
    "Add some data to the cache
    mo_cut->put( k = 'test_key' v = 'test_value' ).
    "Clear the cache
    mo_cut->clear( ).
    "Try to get the data from the cache
    DATA(lv_result) = mo_cut->get( 'test_key' ).
    "Assert that the cache is empty
    cl_abap_unit_assert=>assert_initial( act = lv_result ).
  ENDMETHOD.

  METHOD test_clear_cache_for_seed.
    "Add some data to the cache
    mo_cut->put( k = 'test_key' v = 'test_value' ).
    "Clear the cache for a specific seed
    mo_cut->clear_for_seed( 1 ).
    "Try to get the data from the cache
    DATA(lv_result) = mo_cut->get( 'test_key' ).
    "Assert that the data is still in the cache
    cl_abap_unit_assert=>assert_not_initial( act = lv_result ).
  ENDMETHOD.

  METHOD test_get.
    "Add some data to the cache
    mo_cut->put( k = 'test_key' v = 'test_value' ).
    "Get the data from the cache
    DATA(lv_result) = mo_cut->get( 'test_key' ).
    "Assert that the data is correct
    cl_abap_unit_assert=>assert_equals( act = lv_result exp = 'test_value' ).
  ENDMETHOD.

  METHOD test_invalidate.
    "Add some data to the cache
    mo_cut->put( k = 'test_key' v = 'test_value' ).
    "Invalidate the data in the cache
    mo_cut->invalidate( 'test_key' ).
    "Try to get the data from the cache
    DATA(lv_result) = mo_cut->get( 'test_key' ).
    "Assert that the data is not in the cache
    cl_abap_unit_assert=>assert_initial( act = lv_result ).
  ENDMETHOD.

  METHOD test_put.
    "Add some data to the cache
    mo_cut->put( k = 'test_key' v = 'test_value' ).
    "Get the data from the cache
    DATA(lv_result) = mo_cut->get( 'test_key' ).
    "Assert that the data is correct
    cl_abap_unit_assert=>assert_equals( act = lv_result exp = 'test_value' ).
  ENDMETHOD.

  METHOD test_set_default_seed.
    "Set the default seed
    zcl_llm_00_cache=>_set_default_seed( 1 ).
    "Assert that the default seed is correct
    cl_abap_unit_assert=>assert_equals( act = zcl_llm_00_cache=>gv_seed exp = 1 ).
  ENDMETHOD.

ENDCLASS.
