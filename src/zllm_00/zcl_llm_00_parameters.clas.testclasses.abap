CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_llm_00_parameters DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      cut TYPE REF TO zcl_llm_00_parameters.  "class under test

    METHODS: setup.
    METHODS: get_json FOR TESTING.
    METHODS: new FOR TESTING.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.

  METHOD setup.
    DATA: ls_ TYPE zcl_llm_00_parameters=>ts_par.
    cut = zcl_llm_00_parameters=>new( ls_ ).
  ENDMETHOD.


  METHOD get_json.
    DATA rv_ TYPE string.
    rv_ = cut->zif_llm_00_json~to_json(  ).

*    RETURN.

    cl_demo_output=>display_json( rv_ ).

  ENDMETHOD.



  METHOD new.

    DATA: ls_ TYPE zcl_llm_00_parameters=>ts_par.
    cut = zcl_llm_00_parameters=>new( ls_ ).

  ENDMETHOD.

ENDCLASS.
