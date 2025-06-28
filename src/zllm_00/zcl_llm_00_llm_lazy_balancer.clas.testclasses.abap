*CLASS lcl_ DEFINITION DEFERRED.
*CLASS zcl_llm_00_llm_lazy_composite DEFINITION LOCAL FRIENDS lcl_.
*
*CLASS lcl_ DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    DATA: mo_cut TYPE REF TO zif_llm_00_llm_lazy.
*
*    CLASS-METHODS
*      class_setup.
*    METHODS:
*      setup,
*      test_new FOR TESTING,
*      test_chat_send_below_threshold FOR TESTING,
*      test_chat_send_above_threshold FOR TESTING,
*      test_chat_receive FOR TESTING,
*      test_embed_send FOR TESTING,
*      test_embed_receive FOR TESTING,
*      test_get_model_name FOR TESTING,
*      test_get_max_token FOR TESTING,
*      test_get_model_type FOR TESTING,
*      test_get_split_limit FOR TESTING.
*
*    CLASS-DATA go_llm     TYPE ref to zif_llm_00_llm_lazy.
*    CLASS-DATA go_llm_exp TYPE ref to zif_llm_00_llm_lazy.
*    CLASS-DATA go_fl      TYPE ref to zif_llm_00_file_list.
*    " Declaration of a mock class for zcl_llm_00_llm_lazy to be used in unit tests
*
*ENDCLASS.
*
*CLASS lcl_ IMPLEMENTATION.
*
*  METHOD class_setup.
*    go_fl = zcl_llm_00_file_list_local=>new_from_folder(
*      iv_     = 'C:\TEMP\@ZLLM'
*      iv_mask = '*.env'
*     ).
*    DATA(lo_local_free) = go_fl->get_by_name( 'local_free.env' ).
*    DATA(lo_local_exp)  = go_fl->get_by_name( 'local_exp.env' ).
*    go_llm = zcl_llm_00_llm_lazy=>new_from_file( lo_local_free ).
*    go_llm_exp = zcl_llm_00_llm_lazy=>new_from_file( lo_local_exp ).
*  ENDMETHOD.
*
*  METHOD setup.
*    mo_cut = zcl_llm_00_llm_lazy_composite=>new(
*      io_llm       = go_llm
*      io_llm_exp   = go_llm_exp
*      iv_threshold = 100
*    ).
*  ENDMETHOD.
*
*  METHOD test_new.
*    cl_abap_unit_assert=>assert_not_initial( act = mo_cut ).
*  ENDMETHOD.
*
*  METHOD test_chat_send_below_threshold.
*    DATA(lo_input) = zcl_llm_00_json_mock=>new( iv_tokens = 50 ).
*    DATA(lv_result) = mo_cut->chat_send( lo_input ).
*    cl_abap_unit_assert=>assert_equals( exp = 'light' act = lv_result ).
*  ENDMETHOD.
*
*  METHOD test_chat_send_above_threshold.
*    DATA(lo_input) = zcl_llm_00_json_mock=>new( iv_tokens = 150 ).
*    DATA(lv_result) = mo_cut->chat_send( lo_input ).
*    cl_abap_unit_assert=>assert_equals( exp = 'expensive' act = lv_result ).
*  ENDMETHOD.
*
*  METHOD test_chat_receive.
*    DATA(lo_input) = NEW zcl_llm_00_json_mock( ).
*    DATA(lv_result) = mo_cut->chat_receive( lo_input ).
*    cl_abap_unit_assert=>assert_equals( exp = 'exp_received' act = lv_result ).
*  ENDMETHOD.
*
*  METHOD test_embed_send.
*    DATA(lo_input) = NEW zcl_llm_00_json_mock( ).
*    DATA(lv_result) = mo_cut->embed_send( lo_input ).
*    cl_abap_unit_assert=>assert_equals( exp = 'exp_embed_sent' act = lv_result ).
*  ENDMETHOD.
*
*  METHOD test_embed_receive.
*    DATA(lo_input) = NEW zcl_llm_00_json_mock( ).
*    DATA(lv_result) = mo_cut->embed_receive( lo_input ).
*    cl_abap_unit_assert=>assert_equals( exp = 'exp_embed_received' act = lv_result ).
*  ENDMETHOD.
*
*  METHOD test_get_model_name.
*    DATA(lv_model_name) = mo_cut->get_model_name( ).
*    cl_abap_unit_assert=>assert_equals( exp = 'exp_model' act = lv_model_name ).
*  ENDMETHOD.
*
*  METHOD test_get_max_token.
*    DATA(lv_max_token) = mo_cut->get_max_token( ).
*    cl_abap_unit_assert=>assert_equals( exp = 1024 act = lv_max_token ).
*  ENDMETHOD.
*
*  METHOD test_get_model_type.
*    DATA(lv_model_type) = mo_cut->get_model_type( ).
*    cl_abap_unit_assert=>assert_equals( exp = 'expensive_type' act = lv_model_type ).
*  ENDMETHOD.
*
*  METHOD test_get_split_limit.
*    DATA(lv_split_limit) = mo_cut->get_split_limit( ).
*    cl_abap_unit_assert=>assert_equals( exp = 512 act = lv_split_limit ).
*  ENDMETHOD.
*
*ENDCLASS.
*"$. endregion }
