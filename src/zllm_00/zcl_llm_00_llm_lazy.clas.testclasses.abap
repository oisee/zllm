CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_llm_00_llm_lazy DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zif_llm_00_llm_lazy.  "class under test
    DATA:
      mo_fl TYPE REF TO zif_llm_00_file_list.

    METHODS: setup.
    METHODS: cc               FOR TESTING. "chat completions
    METHODS: cc_local         FOR TESTING. "chat completions
    METHODS: cc_azure         FOR TESTING. "chat completions
    METHODS: cc_azure2        FOR TESTING. "chat completions
    METHODS: cc_azure_o4      FOR TESTING. "chat completions
    METHODS: cc_cache         FOR TESTING. "chat completions
    METHODS: cc_cache_bypass  FOR TESTING. "chat completions
    METHODS: cc_json_forced   FOR TESTING.
    METHODS: cc_json_detected FOR TESTING.
    METHODS: cc_json_explicit FOR TESTING.

*   METHODS: embeddings FOR TESTING.
*   METHODS: function_call            FOR TESTING.
*   METHODS: function_call_04         FOR TESTING.
*   METHODS: function_call_and_invoke FOR TESTING.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.

  METHOD setup.
    mo_fl = zcl_llm_00_file_list_bin=>new_from_bin(
      iv_bin  = '$ZLLM_' && sy-uname
      iv_mask = '*.env;*.md'
    ).
    DATA(lo_env) = mo_fl->get_by_name( 'DEFAULT.ENV' ).
    DATA(lo_cache) = zcl_llm_00_cache=>new(
      iv_seed = 0
    ).
    mo_cut = zcl_llm_00_llm_lazy=>new_from_file(
      io_      = lo_env
      io_cache = lo_cache
    ).
  ENDMETHOD.

  METHOD cc.

    DATA(lo_nya) = zcl_llm_00_formula=>new_from_name(
      io_fl   = mo_fl
      iv_name = 'nya'
*     iv_prefix  = '{'
*     iv_postfix = '}'
*     iv_root = 'T'
    ).

    DATA(lt_msg) = VALUE zcl_llm_00_chat_in=>zif_llm_00_types~tt_message_in(
      ( role = 'system' content = lo_nya->get_sys( )->apply( REF #( '' ) ) )
      ( role = 'user'   content = lo_nya->get_usr( )->apply( REF #( '' ) ) )
    ).

*    cl_demo_output=>write( zcl_llm_00_json=>to_json( lt_msg ) ).
*    cl_demo_output=>display( ).

    DATA(ls_in) = VALUE zif_llm_00_types=>ts_chat_in(
      model    = mo_cut->get_config( )-model_name
      messages = lt_msg
    ).

    DATA(lo_in) = zcl_llm_00_chat_in=>new( ls_in ).

*    cl_demo_output=>write( lo_in->zif_llm_00_json~to_json( ) ).
*    cl_demo_output=>display( ).

    DATA(lo_response) = mo_cut->q( lo_in ).
    DATA(lv_a) = mo_cut->a( lo_response ).

    cl_demo_output=>write( lv_a ).
    cl_demo_output=>display( ).

  ENDMETHOD.

  METHOD cc_local.
    DATA(lo_nya) = zcl_llm_00_formula=>new_from_name(
      io_fl   = mo_fl
      iv_name = 'nya'
*     iv_prefix  = '{'
*     iv_postfix = '}'
*     iv_root = 'T'
    ).

    DATA(lt_msg) = VALUE zcl_llm_00_chat_in=>zif_llm_00_types~tt_message_in(
      ( role = 'system' content = lo_nya->get_sys( )->apply( REF #( '' ) ) )

      ( role = 'user'   content = lo_nya->get_usr( )->apply( REF #( '' ) ) )
    ).

*    cl_demo_output=>write( zcl_llm_00_json=>to_json( lt_msg ) ).
*    cl_demo_output=>display( ).

    DATA(ls_in) = VALUE zif_llm_00_types=>ts_chat_in(
      model    = mo_cut->get_config( )-model_name
      messages = lt_msg
    ).

    DATA(lo_in) = zcl_llm_00_chat_in=>new( ls_in ).

*    cl_demo_output=>write( lo_in->zif_llm_00_json~to_json( ) ).
*    cl_demo_output=>display( ).

    DATA(lo_env) = mo_fl->get_by_name( 'DEFAULT.ENV' )." <-
    DATA(lo_cache) = zcl_llm_00_cache=>new(
      iv_seed = 0
    ).
    DATA(lo_llm) = zcl_llm_00_llm_lazy=>new_from_file(
      io_      = lo_env
      io_cache = lo_cache
    ).

    DATA(lo_response) = lo_llm->q( lo_in ).
    DATA(lv_a) = lo_llm->a( lo_response ).

    cl_demo_output=>write( lv_a ).
    cl_demo_output=>display( ).
  ENDMETHOD.

  METHOD cc_azure.
    DATA(lo_nya) = zcl_llm_00_formula=>new_from_name(
      io_fl   = mo_fl
      iv_name = 'nya'
*     iv_prefix  = '{'
*     iv_postfix = '}'
*     iv_root = 'T'
    ).

    DATA(lt_msg) = VALUE zcl_llm_00_chat_in=>zif_llm_00_types~tt_message_in(
      ( role = 'system' content = lo_nya->get_sys( )->apply( REF #( '' ) ) )

      ( role = 'user'   content = lo_nya->get_usr( )->apply( REF #( '' ) ) )
    ).

*    cl_demo_output=>write( zcl_llm_00_json=>to_json( lt_msg ) ).
*    cl_demo_output=>display( ).

    DATA(ls_in) = VALUE zif_llm_00_types=>ts_chat_in(
      model    = mo_cut->get_config( )-model_name
      messages = lt_msg
    ).

    DATA(lo_in) = zcl_llm_00_chat_in=>new( ls_in ).

*    cl_demo_output=>write( lo_in->zif_llm_00_json~to_json( ) ).
*    cl_demo_output=>display( ).

    DATA(lo_env) = mo_fl->get_by_name( 'DEFAULT.ENV' )." <-
    DATA(lo_cache) = zcl_llm_00_cache=>new(
      iv_seed = 0
    ).
    DATA(lo_llm) = zcl_llm_00_llm_lazy=>new_from_file(
      io_      = lo_env
      io_cache = lo_cache
    ).

    DATA(lo_response) = lo_llm->q( lo_in ).
    DATA(lv_a) = lo_llm->a( lo_response ).

    cl_demo_output=>write( lv_a ).
    cl_demo_output=>display( ).
  ENDMETHOD.

  METHOD cc_azure2.
    DATA(lo_nya) = zcl_llm_00_formula=>new_from_name(
      io_fl   = mo_fl
      iv_name = 'nya'
*     iv_prefix  = '{'
*     iv_postfix = '}'
*     iv_root = 'T'
    ).

    DATA(lt_msg) = VALUE zcl_llm_00_chat_in=>zif_llm_00_types~tt_message_in(
      ( role = 'system' content = lo_nya->get_sys( )->apply( REF #( '' ) ) )

      ( role = 'user'   content = lo_nya->get_usr( )->apply( REF #( '' ) ) )
    ).

*    cl_demo_output=>write( zcl_llm_00_json=>to_json( lt_msg ) ).
*    cl_demo_output=>display( ).

    DATA(ls_in) = VALUE zif_llm_00_types=>ts_chat_in(
      model    = mo_cut->get_config( )-model_name
      messages = lt_msg
    ).

    DATA(lo_in) = zcl_llm_00_chat_in=>new( ls_in ).
*
*    cl_demo_output=>write( lo_in->zif_llm_00_json~to_json( ) ).
*    cl_demo_output=>display( ).

    DATA(lo_env) = mo_fl->get_by_name( 'DEFAULT.ENV' )." <-
    DATA(lo_cache) = zcl_llm_00_cache=>new(
      iv_seed = 0
    ).
    DATA(lo_llm) = zcl_llm_00_llm_lazy=>new_from_file(
      io_      = lo_env
      io_cache = lo_cache
    ).

    DATA(lo_response) = lo_llm->q( lo_in ).
    DATA(lv_a) = lo_llm->a( lo_response ).

    cl_demo_output=>write( lv_a ).
    cl_demo_output=>display( ).
  ENDMETHOD.

  METHOD cc_azure_o4.
    DATA(lo_env) = mo_fl->get_by_name( 'DEFAULT-DEEP.ENV' )." <-
    DATA(lo_cache) = zcl_llm_00_cache=>new(
      iv_seed = 0
    ).
    DATA(lo_llm) = zcl_llm_00_llm_lazy=>new_from_file(
      io_      = lo_env
      io_cache = lo_cache
    ).

*    DATA(lo_nya) = zcl_llm_00_formula=>new_from_name(
*      io_fl   = mo_fl
*      iv_name = 'CLASS_TO_TEST'
**     iv_prefix  = '{'
**     iv_postfix = '}'
**     iv_root = 'T'
*    ).

    DATA(lt_msg) = VALUE zcl_llm_00_chat_in=>zif_llm_00_types~tt_message_in(
      ( role = 'user'   content = 'How can you help me?' )
    ).

*    cl_demo_output=>write( zcl_llm_00_json=>to_json( lt_msg ) ).
*    cl_demo_output=>display( ).

    DATA(ls_in) = VALUE zif_llm_00_types=>ts_reasoning(
      model    = lo_llm->get_config( )-model_name
      input    = lt_msg
    ).

    DATA(lo_in) = zcl_llm_00_reasoning_in=>new( ls_in ).
*
*    cl_demo_output=>write( lo_in->zif_llm_00_json~to_json( ) ).
*    cl_demo_output=>display( ).


    DATA(lo_response) = lo_llm->q( lo_in ).
    DATA(lv_a) = lo_llm->a( lo_response ).

    cl_demo_output=>write( lv_a ).
    cl_demo_output=>display( ).
  ENDMETHOD.

  METHOD cc_cache.

    DATA(lo_nya) = zcl_llm_00_formula=>new_from_name(
      io_fl   = mo_fl
      iv_name = 'nya'
*     iv_prefix  = '{'
*     iv_postfix = '}'
*     iv_root = 'T'
    ).

    DATA(lt_msg) = VALUE zcl_llm_00_chat_in=>zif_llm_00_types~tt_message_in(
      ( role = 'system' content = lo_nya->get_sys( )->apply( REF #( '' ) ) )
      ( role = 'user'   content = lo_nya->get_usr( )->apply( REF #( '' ) ) )
    ).

*    cl_demo_output=>write( zcl_llm_00_json=>to_json( lt_msg ) ).
*    cl_demo_output=>display( ).

    DATA(ls_in) = VALUE zif_llm_00_types=>ts_chat_in(
      model    = mo_cut->get_config( )-model_name
      messages = lt_msg
    ).

    DATA(lo_in) = zcl_llm_00_chat_in=>new( ls_in ).
*
*    cl_demo_output=>write( lo_in->zif_llm_00_json~to_json( ) ).
*    cl_demo_output=>display( ).

    DATA(lo_env)   = mo_fl->get_by_name( 'DEFAULT.ENV' )." <-
    DATA(lo_cache) = zcl_llm_00_cache=>new( iv_seed = 111 ).
    DATA(lo_llm) = zcl_llm_00_llm_lazy=>new_from_file(
      io_      = lo_env
      io_cache = lo_cache
    ).

    DATA(lo_response)   = lo_llm->q( lo_in ).
    DATA(lv_a) = lo_llm->a( lo_response ).

    DATA(lo_response_2) = lo_llm->q( lo_in ).
    DATA(lv_a_2) = lo_llm->a( lo_response_2 ).

    cl_demo_output=>write( lv_a ).
    cl_demo_output=>write( lv_a_2 ).
    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lv_a
        exp = lv_a_2
    ).
  ENDMETHOD.

  METHOD cc_cache_bypass.
    DATA(lo_nya) = zcl_llm_00_formula=>new_from_name(
      io_fl   = mo_fl
      iv_name = 'nya'
*     iv_prefix  = '{'
*     iv_postfix = '}'
*     iv_root = 'T'
    ).

    DATA(lt_msg) = VALUE zcl_llm_00_chat_in=>zif_llm_00_types~tt_message_in(
      ( role = 'system' content = lo_nya->get_sys( )->apply( REF #( '' ) ) )
      ( role = 'user'   content = lo_nya->get_usr( )->apply( REF #( '' ) ) )
    ).

*    cl_demo_output=>write( zcl_llm_00_json=>to_json( lt_msg ) ).
*    cl_demo_output=>display( ).

    DATA(ls_in) = VALUE zif_llm_00_types=>ts_chat_in(
      model    = mo_cut->get_config( )-model_name
      messages = lt_msg
    ).

    DATA(lo_in) = zcl_llm_00_chat_in=>new( ls_in ).
*
*    cl_demo_output=>write( lo_in->zif_llm_00_json~to_json( ) ).
*    cl_demo_output=>display( ).

    DATA(lo_env)   = mo_fl->get_by_name( 'DEFAULT.ENV' )." <-
    DATA(lo_cache) = zcl_llm_00_cache_never=>new( ).
    DATA(lo_llm) = zcl_llm_00_llm_lazy=>new_from_file(
      io_      = lo_env
      io_cache = lo_cache
    ).

    DATA(lo_response)   = lo_llm->q( lo_in ).
    DATA(lv_a) = lo_llm->a( lo_response ).

    DATA(lo_response_2) = lo_llm->q( lo_in ).
    DATA(lv_a_2) = lo_llm->a( lo_response_2 ).

    cl_demo_output=>write( lv_a ).
    cl_demo_output=>write( lv_a_2 ).
    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_differs(
      EXPORTING
        act = lv_a
        exp = lv_a_2
    ).

  ENDMETHOD.

*
*  METHOD embeddings.
*    DATA(lo_in) = zcl_llm_00_embed_in=>new( VALUE #(
*      input = 'What do you know about HOMELANDER? What is your strategy to defeat him (or, at least, disarm )?'
*    ) ).
*    DATA: lv_json TYPE string.
*    DATA(lo_e) = mo_cut->embeddings( EXPORTING io_ = lo_in  IMPORTING ev_json = lv_json ).
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
*    DATA(lo_cc) = mo_cut->chat_completions( lo_in ).
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
*    DATA(lo_cc_o1) = mo_cut->chat_completions( lo_in ).
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
*    DATA(lo_cc_o1) = mo_cut->chat_completions( lo_in ).
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
*    DATA(lo_cc_o1) = mo_cut->chat_completions( lo_in ).
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

  METHOD cc_json_forced.
    DATA(lo_nya) = zcl_llm_00_formula=>new_from_name(
      io_fl   = mo_fl
      iv_name = 'nya'
*     iv_prefix  = '{'
*     iv_postfix = '}'
*     iv_root = 'T'
    ).

    DATA(lt_msg) = VALUE zcl_llm_00_chat_in=>zif_llm_00_types~tt_message_in(
      ( role = 'system' content = lo_nya->get_sys( )->apply( REF #( '' ) ) )
      ( role = 'user'   content = lo_nya->get_usr( )->apply( REF #( '' ) ) )
    ).

*    cl_demo_output=>write( zcl_llm_00_json=>to_json( lt_msg ) ).
*    cl_demo_output=>display( ).

    DATA(ls_in) = VALUE zif_llm_00_types=>ts_chat_in(
      model    = mo_cut->get_config( )-model_name
      messages = lt_msg
    ).

    DATA(lo_in) = zcl_llm_00_chat_in=>new( ls_in ).
*
*    cl_demo_output=>write( lo_in->zif_llm_00_json~to_json( ) ).
*    cl_demo_output=>display( ).

    DATA(lo_env)   = mo_fl->get_by_name( 'DEFAULT.ENV' )." <-
    DATA(lo_cache) = zcl_llm_00_cache_never=>new( ).
    DATA(lo_llm) = zcl_llm_00_llm_lazy=>new_from_file(
      io_      = lo_env
      io_cache = lo_cache
    ).

    DATA(lo_response)   = lo_llm->q( lo_in ).
    DATA(lv_a) = lo_llm->a( lo_response ).

    DATA(lo_response_2) = lo_llm->q( lo_in ).
    DATA(lv_a_2) = lo_llm->a( lo_response_2 ).

    cl_demo_output=>write( lv_a ).
    cl_demo_output=>write( lv_a_2 ).
    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_differs(
      EXPORTING
        act = lv_a
        exp = lv_a_2
    ).


  ENDMETHOD.

  METHOD cc_json_detected.

  ENDMETHOD.

  METHOD cc_json_explicit.
    DATA(lo_nya) = zcl_llm_00_formula=>new_from_name(
      io_fl   = mo_fl
      iv_name = 'QNA'
*     iv_prefix  = '{'
*     iv_postfix = '}'
*     iv_root = 'T'
    ).

    DATA(lt_msg) = VALUE zcl_llm_00_chat_in=>zif_llm_00_types~tt_message_in(
      ( role = 'system' content = lo_nya->get_sys( )->apply( REF #( '' ) ) )
      ( role = 'user'   content = lo_nya->get_usr( )->apply( REF #( '' ) ) )
    ).

*    cl_demo_output=>write( zcl_llm_00_json=>to_json( lt_msg ) ).
*    cl_demo_output=>display( ).

    DATA(ls_in) = VALUE zif_llm_00_types=>ts_chat_in(
      model    = mo_cut->get_config( )-model_name
      messages = lt_msg
    ).

    DATA(lo_in) = zcl_llm_00_chat_in=>new( ls_in ).
*
*    cl_demo_output=>write( lo_in->zif_llm_00_json~to_json( ) ).
*    cl_demo_output=>display( ).

    DATA(lo_env)   = mo_fl->get_by_name( 'DEFAULT.ENV' )." <-
    DATA(lo_cache) = zcl_llm_00_cache_never=>new( ).
    DATA(lo_llm) = zcl_llm_00_llm_lazy=>new_from_file(
      io_      = lo_env
      io_cache = lo_cache
    ).

    DATA(lo_response)   = lo_llm->q( lo_in ).
    DATA(lv_a) = lo_llm->a( lo_response ).

    DATA(lo_response_2) = lo_llm->q( lo_in ).
    DATA(lv_a_2) = lo_llm->a( lo_response_2 ).

    cl_demo_output=>write( lv_a ).
    cl_demo_output=>write( lv_a_2 ).
    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_differs(
      EXPORTING
        act = lv_a
        exp = lv_a_2
    ).


  ENDMETHOD.
ENDCLASS.
