CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_llm_00_llm_lazy_mock DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*  PRIVATE SECTION.
*    DATA:
*      cut TYPE REF TO zcl_llm_00_llm_async.  "class under test
*
*    METHODS: setup.
*    METHODS: chat_completions         FOR TESTING.
*    METHODS: embeddings FOR TESTING.
*    METHODS: function_call            FOR TESTING.
*    METHODS: function_call_04         FOR TESTING.
*    METHODS: function_call_and_invoke FOR TESTING.
*    METHODS: new FOR TESTING.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.
*
*  METHOD setup.
*    cut = zcl_llm_00_llm_async=>new( zcl_llm_00_dotenv=>new_from_path( 'C:\TEMP\@LLM\local.env' )->get_config( ) ).
*  ENDMETHOD.
*
*  METHOD embeddings.
*    DATA(lo_in) = zcl_llm_00_embed_in=>new( VALUE #(
*      input = 'What do you know about HOMELANDER? What is your strategy to defeat him (or, at least, disarm )?'
*    ) ).
*    DATA: lv_json TYPE string.
*    DATA(lo_e) = cut->embeddings( EXPORTING io_ = lo_in  IMPORTING ev_json = lv_json ).
*
*    cl_demo_output=>write( '-- embed' ).
*    cl_demo_output=>write( lo_e->get_json( ) ).
*    cl_demo_output=>write( lo_e->get_( ) ).
*    cl_demo_output=>display( ).
*
*  ENDMETHOD.
*
*
*  METHOD chat_completions.
*    RETURN.
*    DATA(lo_in) = zcl_llm_00_chat_in=>new( VALUE #(
*      messages = VALUE #(
*        ( role = 'system' content = 'You are superwoman!'  )
*        ( role = 'user'   content = 'What do you know about HOMELANDER? What is your strategy to defeat him (or, at least, disarm )?'  ) )
*      functions = zcl_llm_00_array=>new( VALUE #( ( zcl_llm_00_function=>new( 'ZOAI_01_TEST_01_PARAMETER' ) ) ) )
*    ) ).
*
*    DATA(lo_cc) = cut->chat_completions( lo_in ).
*
*    cl_demo_output=>write( '-- chat_completions' ).
*    cl_demo_output=>write( lo_cc->get_json( ) ).
*    cl_demo_output=>write( lo_cc->get_reply( ) ).
*
*  ENDMETHOD.
*
*  METHOD function_call.
*
*    DATA(lo_in) = zcl_llm_00_chat_in=>new( VALUE #(
*      messages = VALUE #( ( role = 'system' content = 'You are superwoman!'  )
*                          ( role = 'user' content = 'Call a function with three parameters please (pass any fun data)' ) )
*      functions = zcl_llm_00_array=>new( VALUE #( ( zcl_llm_00_function=>new( 'ZOAI_01_TEST_01_PARAMETER' ) )
*                                                  ( zcl_llm_00_function=>new( 'ZOAI_01_TEST_03_PARAMETER' ) )
*                                                )
*                                       )
*    ) ).
*
*    DATA(lo_cc_o1) = cut->chat_completions( lo_in ).
*
*    IF lo_cc_o1->is_function_call( ).
*      DATA(lo_fm) = lo_cc_o1->get_function( ).
*      "lo_fm->invoke( ).
*    ENDIF.
*
*
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act                  = lo_cc_o1->is_function_call( )
*        exp                  = 'X'
*    ).
*
*    cl_demo_output=>write( '-- function call ' ).
*    cl_demo_output=>write( lo_cc_o1->get_json( ) ).
*    cl_demo_output=>display( ).
*
*  ENDMETHOD.
*
*  METHOD function_call_04.
*
*    DATA(lo_in) = zcl_llm_00_chat_in=>new( VALUE #(
*      messages = VALUE #( ( role = 'system' content = 'You are superwoman!'  )
*                          ( role = 'user' content = 'Call a function ZOAI_01_TEST_04_PARAMETER with four parameters please. Pass fun data, to repeat the sting 7 times' ) )
*      functions = zcl_llm_00_array=>new( VALUE #( ( zcl_llm_00_function=>new( 'ZOAI_01_TEST_04_PARAMETER' ) )
*                                                  ( zcl_llm_00_function=>new( 'ZOAI_01_TEST_01_PARAMETER' ) )
*                                                )
*                                       )
*    ) ).
*
*    DATA(lo_cc_o1) = cut->chat_completions( lo_in ).
*
*    IF lo_cc_o1->is_function_call( ).
*      DATA(lo_fm) = lo_cc_o1->get_function( ).
*      lo_fm->invoke( ).
*    ENDIF.
*
**    cl_abap_unit_assert=>assert_equals(
**      EXPORTING
**        act                  = lo_cc_o1->is_function_call( )
**        exp                  = 'X'
**    ).
*
*    cl_demo_output=>write( '-- function call ' ).
*    cl_demo_output=>write( lo_cc_o1->get_json( ) ).
*    cl_demo_output=>display( ).
*
*  ENDMETHOD.
*
*  METHOD function_call_and_invoke.
*    RETURN.
*
*
*    DATA(lo_in) = zcl_llm_00_chat_in=>new( VALUE #(
*      messages = VALUE #( ( role = 'system' content = 'You are superwoman!'  )
*                          ( role = 'user' content = 'Call a function with three parameters please (pass any fun data)' ) )
*      functions = zcl_llm_00_array=>new( VALUE #( ( zcl_llm_00_function=>new( 'ZOAI_01_TEST_01_PARAMETER' ) )
*                                                  ( zcl_llm_00_function=>new( 'ZOAI_01_TEST_03_PARAMETER' ) )
*                                                )
*                                       )
*    ) ).
*
*    DATA(lo_cc_o1) = cut->chat_completions( lo_in ).
*
*    IF lo_cc_o1->is_function_call( ).
*      DATA(lo_fm) = lo_cc_o1->get_function( ).
*      "lo_fm->invoke( ).
*    ENDIF.
*
*
**    cl_abap_unit_assert=>assert_equals(
**      EXPORTING
**        act                  = lo_cc_o1->is_function_call( )
**        exp                  = 'X'
**    ).
*
*    cl_demo_output=>write( ' JSON ' ).
*    cl_demo_output=>write( lo_cc_o1->get_json( ) ).
*    cl_demo_output=>display( ).
*
*  ENDMETHOD.
*
*  METHOD new.
*
*  ENDMETHOD.

ENDCLASS.
