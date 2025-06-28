CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_LLM_00_function DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      cut TYPE REF TO zcl_LLM_00_function.  "class under test

    METHODS: setup.
    METHODS: get_json FOR TESTING.
    METHODS: get_desc FOR TESTING.
    METHODS: invoke FOR TESTING.
    METHODS: new FOR TESTING.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.

  METHOD setup.
    cut = zcl_LLM_00_function=>new( 'ZOAI_01_TEST_04_PARAMETER'  ).
  ENDMETHOD.

  METHOD get_json.
    DATA rv_ TYPE string.
    rv_ = cut->zif_llm_00_json~to_json(  ).
*    RETURN.
    cl_demo_output=>display_json( rv_ ).
  ENDMETHOD.


  METHOD get_desc.
    DATA rv_ok TYPE sap_bool.
    rv_ok = cut->get_desc(  ).
  ENDMETHOD.


  METHOD invoke.
    cut->zif_llm_00_function~invoke(  ).
  ENDMETHOD.


  METHOD new.
    DATA ls_ TYPE zif_llm_00_function=>ts_par.
    DATA(lo_) = zcl_llm_00_parameters=>new( ls_ ).
  ENDMETHOD.

ENDCLASS.
