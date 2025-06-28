CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_llm_00_reasoning_out DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      cut TYPE REF TO zcl_llm_00_reasoning_in.  "class under test

    METHODS: setup.
    METHODS: to_json FOR TESTING.
    METHODS: new FOR TESTING.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.

  METHOD setup.
    DATA: ls_ TYPE zcl_llm_00_reasoning_in=>ts_.
    cut = zcl_llm_00_reasoning_in=>new( ls_ ).
  ENDMETHOD.


  METHOD to_json.
    DATA(lo_in) = zcl_llm_00_chat_in=>new( VALUE #(
      model    = 'gpt-4'
      messages = VALUE #( ( ) ( ) )
      response_format = zcl_llm_00_kv=>new(
        VALUE #( ( k = 'type' v = 'json_object' ) )
      )
      functions = zcl_llm_00_array=>new( VALUE #( ( zcl_llm_00_function=>new( 'ZOAI_01_TEST_01_PARAMETER' ) ) ) )
    ) ).

    data(lv_) = lo_in->zif_llm_00_json~to_json( ).

*    RETURN.

    cl_demo_output=>display_json( lv_ ).

  ENDMETHOD.



  METHOD new.

    DATA: ls_ TYPE zcl_llm_00_reasoning_in=>ts_.
    cut = zcl_llm_00_reasoning_in=>new( ls_ ).

  ENDMETHOD.


ENDCLASS.
