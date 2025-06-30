*"* use this source file for your ABAP unit test classes
*"* use this source file for your ABAP unit test classes
CLASS ltc_blog DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.

  PRIVATE SECTION.
    DATA: gv_msg TYPE string.
    DATA: cut TYPE REF TO zcl_blog.
    METHODS one_msg_implicit FOR TESTING.
    METHODS one_msg_implicit_type_e FOR TESTING.
    METHODS one_msg_implicit_type_e_as_w FOR TESTING.
    METHODS one_msg_implicit_type_args FOR TESTING.
    METHODS one_msg_explicit FOR TESTING.
    METHODS one_msg_explicit_type FOR TESTING.
    METHODS one_logger FOR TESTING.
    METHODS one_exception FOR TESTING.
    METHODS one_exception_zif_log FOR TESTING.
    METHODS two_msg_implicit FOR TESTING.
    METHODS two_msg_implicit_order FOR TESTING.
    METHODS two_msg_chain_add_add FOR TESTING.
    METHODS test_display_default FOR TESTING.
    METHODS add_bapi_coru_return FOR TESTING.
    METHODS add_bapireturn FOR TESTING.
    METHODS setup.
ENDCLASS.

CLASS ltc_blog IMPLEMENTATION.

  METHOD setup.
    "given
    cut = zcl_blog=>new( object = 'ZCX_' auto_save = abap_false ).
    MESSAGE e000(zcx_) WITH 'A1' 'A2' 'A3' 'A4' INTO gv_msg.
  ENDMETHOD.
  METHOD one_msg_implicit.
    "when
    cut->add(  ).
    "then
    DATA(lt_msg) = cut->get_flatten_table(  ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_msg )
                                        exp = 1 ).
  ENDMETHOD.

  METHOD one_msg_implicit_type_e.
    "when
    cut->add(  ).
    "then
    DATA(lt_msg) = cut->get_flatten_table(  ).
    DATA(ls_msg) = VALUE #( lt_msg[ 1 ] OPTIONAL ).
    cl_abap_unit_assert=>assert_equals( act = ls_msg-msgty
                                        exp = 'E' ).
  ENDMETHOD.

  METHOD one_msg_implicit_type_e_as_w.
    "when
    cut->w(  ).
    "then
    DATA(lt_msg) = cut->get_flatten_table(  ).
    DATA(ls_msg) = VALUE #( lt_msg[ 1 ] OPTIONAL ).
    cl_abap_unit_assert=>assert_equals( act = ls_msg-msgty
                                        exp = 'W' ).
  ENDMETHOD.
  METHOD one_msg_implicit_type_args.
    "when
    cut->w(  ).
    "then
    DATA(lt_msg) = cut->get_flatten_table(  ).
    DATA(ls_msg) = VALUE #( lt_msg[ 1 ] OPTIONAL ).
    cl_abap_unit_assert=>assert_equals( act = ls_msg-msgv1
                                        exp = 'A1' ).
    cl_abap_unit_assert=>assert_equals( act = ls_msg-msgv2
                                        exp = 'A2' ).
    cl_abap_unit_assert=>assert_equals( act = ls_msg-msgv3
                                        exp = 'A3' ).
    cl_abap_unit_assert=>assert_equals( act = ls_msg-msgv4
                                        exp = 'A4' ).
  ENDMETHOD.
  METHOD two_msg_implicit.
    "when
    cut->add(  ).
    MESSAGE e001(zcx_) WITH 'A1' 'A2' 'A3' 'A4' INTO gv_msg.
    cut->add(  ).
    "then
    DATA(lt_msg) = cut->get_flatten_table(  ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_msg )
                                        exp = 2 ).
  ENDMETHOD.
  METHOD two_msg_implicit_order.
    cut->add(  ).
    MESSAGE e001(zcx_) WITH 'A1' 'A2' 'A3' 'A4' INTO gv_msg.
    cut->add(  ).
    "then
    DATA(lt_msg) = cut->get_flatten_table(  ).
    DATA(ls_msg_01) = VALUE #( lt_msg[ 1 ] OPTIONAL ).
    DATA(ls_msg_02) = VALUE #( lt_msg[ 2 ] OPTIONAL ).
    cl_abap_unit_assert=>assert_equals( act = ls_msg_01-msgno
                                        exp = '000' ).
    cl_abap_unit_assert=>assert_equals( act = ls_msg_02-msgno
                                        exp = '001' ).
  ENDMETHOD.

  METHOD one_msg_explicit.
    "when
    cut->add( sy ).
    "then
    DATA(lt_msg) = cut->get_flatten_table(  ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_msg )
                                        exp = 1 ).
  ENDMETHOD.

  METHOD one_msg_explicit_type.
    "when
    cut->add( io_ = sy iv_type = 'W' ).
    "then
    DATA(lt_msg) = cut->get_flatten_table(  ).
    DATA(ls_msg) = VALUE #( lt_msg[ 1 ] OPTIONAL ).
    cl_abap_unit_assert=>assert_equals( act = ls_msg-msgty
                                        exp = 'W' ).
  ENDMETHOD.

  METHOD two_msg_chain_add_add.
    "when
    cut->add( )->add( ).
    "then
    DATA(lt_msg) = cut->get_flatten_table(  ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_msg )
                                        exp = 2 ).

  ENDMETHOD.


  METHOD test_display_default.
    "when
    cut->add( )->add( ).
    "then
"   cut->display(  ). "disabled to prevent launching during TR release

  ENDMETHOD.


  METHOD one_logger.
    "when
    DATA(lo_log2) = zcl_minilog=>new(  ).
    MESSAGE e000(zcx_) WITH 'A1' 'A2' 'A3' 'A4' INTO gv_msg.
    lo_log2->add( sy ).
    cut->add( lo_log2 ).
    "then
    cl_abap_unit_assert=>assert_equals( act = cut->get_message_table(  )
                                        exp = lo_log2->get_message_table(  )
                                      ).
  ENDMETHOD.

  METHOD one_exception.
    DATA: lx_ TYPE REF TO cx_root.
    "when
    TRY.
*        MESSAGE e000(zcx_) WITH 'A1' 'A2' 'A3' 'A4' INTO gv_msg.
*        zcx_s=>raise(  ).
        RAISE EXCEPTION TYPE cx_ocs_error.
      CATCH cx_root INTO lx_.
        cut->add( lx_ ).
    ENDTRY.
    "then
    DATA: lt_msg TYPE zif_llm_log=>tt_msg.
    DATA: ls_msg TYPE zif_llm_log=>ts_msg.
    lt_msg = cut->get_message_table(  ).
    READ TABLE lt_msg INTO ls_msg INDEX 1.
    cl_abap_unit_assert=>assert_equals( act = ls_msg-msgid
                                        exp = 'TN' ).
  ENDMETHOD.

  METHOD one_exception_zif_log.
    DATA: lx_ TYPE REF TO zcx_s.
    "when
    TRY.
        MESSAGE e000(zcx_) WITH 'A1' 'A2' 'A3' 'A4' INTO gv_msg.
        zcx_s=>raise(  ).
      CATCH zcx_s INTO lx_.
        cut->add( lx_ ).
    ENDTRY.
    "then
    cl_abap_unit_assert=>assert_equals( act = cut->get_message_table( )
                                        exp = lx_->get_message_table( ) ).
  ENDMETHOD.

  METHOD add_bapi_coru_return.
    "when
    DATA: ls_ TYPE bapi_coru_return.
    ls_-id = 'ZCX_'.
    ls_-type = 'E'.
    ls_-number = '000'.
    ls_-message_v1 = '1'.
    ls_-message_v2 = '2'.
    ls_-message_v3 = '3'.
    ls_-message_v4 = '4'.
    cut->add( ls_ ).
    "then
    DATA(ls_act) = cut->get_analyser( )->get_worst_bapiret2( ).
    cl_abap_unit_assert=>assert_equals( act = ls_-message_v1
                                        exp = ls_act-message_v1
                                  ).
  ENDMETHOD.
  METHOD add_bapireturn.
    "when
    DATA: ls_ TYPE bapireturn.
    ls_-type = 'E'.
    ls_-code = 'W5037'.
    ls_-message_v1 = '1'.
    ls_-message_v2 = '2'.
    ls_-message_v3 = '3'.
    ls_-message_v4 = '4'.
    cut->add( ls_ ).
    "then
    DATA(ls_act) = cut->get_analyser( )->get_worst_bapiret2( ).
    cl_abap_unit_assert=>assert_equals( act = ls_-message_v1
                                        exp = ls_act-message_v1
                                  ).
  ENDMETHOD.


ENDCLASS.


CLASS lcl_ DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.
  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zcl_minilog.
    METHODS: setup FOR TESTING RAISING cx_static_check,
             test_new_instance FOR TESTING,
             test_add_message_implicitly FOR TESTING,
             test_add_message_explicitly FOR TESTING,
             test_add_message_with_type FOR TESTING,
             test_logging_exception FOR TESTING,
             test_add_bapireturn_structure FOR TESTING,
             test_get_flatten_table FOR TESTING,
             test_get_message_table FOR TESTING,
             test_integration_with_external FOR TESTING,
             test_message_order FOR TESTING,
             test_converter_functionality FOR TESTING,
             test_analyser_functionality FOR TESTING.

ENDCLASS.

CLASS lcl_ IMPLEMENTATION.

  METHOD setup.
    mo_cut = zcl_minilog=>new( ).
  ENDMETHOD.

  METHOD test_new_instance.
    DATA(lo_instance) = zcl_minilog=>new( ).
    cl_abap_unit_assert=>assert_not_initial( act = lo_instance ).
  ENDMETHOD.

  METHOD test_add_message_implicitly.
    mo_cut->add( 'Test message' ).
    DATA(lt_messages) = mo_cut->get_message_table( ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages ) exp = 1 ).
  ENDMETHOD.

  METHOD test_add_message_explicitly.
    mo_cut->add( io_ = 'Explicit message' iv_type = 'I' ).
    DATA(lt_messages) = mo_cut->get_message_table( ).
    DATA(ls_message) = lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = ls_message-free_text_msg exp = 'Explicit message' ).
    cl_abap_unit_assert=>assert_equals( act = ls_message-msgty exp = 'I' ).
  ENDMETHOD.

  METHOD test_add_message_with_type.
    mo_cut->e( 'Error message' ).
    DATA(lt_messages) = mo_cut->get_message_table( ).
    DATA(ls_message) = lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = ls_message-msgty exp = 'E' ).
  ENDMETHOD.

  METHOD test_logging_exception.
    TRY.
        zcx_s=>raise( 'kekek' ).
      CATCH cx_static_check INTO DATA(lx_exception).
        mo_cut->add( lx_exception ).
    ENDTRY.
    DATA(lt_messages) = mo_cut->get_message_table( ).
    cl_abap_unit_assert=>assert_not_initial( lt_messages ).
  ENDMETHOD.

  METHOD test_add_bapireturn_structure.
    DATA(ls_bapireturn) = VALUE bapireturn( type = 'S' message = 'Success' ).
    mo_cut->add( ls_bapireturn ).
    DATA(lt_messages) = mo_cut->get_message_table( ).
    cl_abap_unit_assert=>assert_not_initial( lt_messages ).
  ENDMETHOD.

  METHOD test_get_flatten_table.
    mo_cut->add( 'First message' ).
    mo_cut->e( 'Second message' ).
    DATA(lt_flat) = mo_cut->get_flatten_table( ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_flat ) exp = 2 ).
  ENDMETHOD.

  METHOD test_get_message_table.
    mo_cut->add( 'Message for table' ).
    DATA(lt_messages) = mo_cut->get_message_table( ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages ) exp = 1 ).
  ENDMETHOD.

  METHOD test_integration_with_external.
    DATA(lo_external_logger) = zcl_minilog=>new( ).
    lo_external_logger->add( 'External log message' ).
    mo_cut->add( lo_external_logger ).
    DATA(lt_messages) = mo_cut->get_message_table( ).
    cl_abap_unit_assert=>assert_not_initial( lt_messages ).
  ENDMETHOD.

  METHOD test_message_order.
    mo_cut->add( 'First' ).
    mo_cut->add( 'Second' ).
    DATA(lt_messages) = mo_cut->get_message_table( ).
    DATA(ls_first_message) = lt_messages[ 1 ].
    DATA(ls_second_message) = lt_messages[ 2 ].
    cl_abap_unit_assert=>assert_equals( act = ls_first_message-free_text_msg exp = 'First' ).
    cl_abap_unit_assert=>assert_equals( act = ls_second_message-free_text_msg exp = 'Second' ).
  ENDMETHOD.

  METHOD test_converter_functionality.
    DATA(lo_converter) = mo_cut->get_converter( ).
    cl_abap_unit_assert=>assert_not_initial( act = lo_converter ).
  ENDMETHOD.

  METHOD test_analyser_functionality.
    DATA(lo_analyser) = mo_cut->get_analyser( ).
    cl_abap_unit_assert=>assert_not_initial( act = lo_analyser ).
  ENDMETHOD.

ENDCLASS.
