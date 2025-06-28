CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_LLM_00_kv DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      cut TYPE REF TO zcl_LLM_00_kv.  "class under test

    METHODS: setup.
    METHODS: get_json FOR TESTING.
    METHODS: new FOR TESTING.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.

  METHOD setup.
    cut = zcl_LLM_00_kv=>new( ).
  ENDMETHOD.


  METHOD get_json.
    DATA rv_ TYPE string.
    cut->append( VALUE #( k = 'Key' v = 'Value' ) ).
    rv_ = cut->zif_llm_00_json~to_json(  ).

*    RETURN.

    cl_demo_output=>display_json( rv_ ).

  ENDMETHOD.



  METHOD new.

    cut = zcl_LLM_00_kv=>new( ).

  ENDMETHOD.

ENDCLASS.
