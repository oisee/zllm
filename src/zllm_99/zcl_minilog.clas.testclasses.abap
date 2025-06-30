*"* use this source file for your ABAP unit test classes
*"* use this source file for your ABAP unit test classes
CLASS ltc_minilog DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.

  PRIVATE SECTION.
    DATA: gv_msg TYPE string.
    DATA: cut TYPE REF TO zcl_minilog.
    METHODS one_msg_implicit FOR TESTING.
    METHODS one_msg_implicit_type_e FOR TESTING.
    METHODS one_msg_implicit_type_e_as_w FOR TESTING.
    METHODS one_msg_implicit_type_args FOR TESTING.
    METHODS one_msg_explicit FOR TESTING.
    METHODS one_msg_explicit_type FOR TESTING.
    METHODS one_logger FOR TESTING.
    METHODS one_exception FOR TESTING.
    METHODS one_exception_zif_log FOR TESTING.
    METHODS one_exc_ignore_importance_ovr FOR TESTING.
    METHODS one_exc_respect_importance FOR TESTING.
    METHODS two_msg_implicit FOR TESTING.
    METHODS two_msg_implicit_order FOR TESTING.
    METHODS two_msg_chain_add_add FOR TESTING.
    METHODS add_bapi_coru_return FOR TESTING.
    METHODS add_bapireturn FOR TESTING.
    METHODS add_empty_log FOR TESTING.
    METHODS setup.
ENDCLASS.

CLASS ltc_minilog IMPLEMENTATION.

  METHOD setup.
    "given
    cut = zcl_minilog=>new(  ).
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

  METHOD one_exception.
    "when
    TRY.
*        MESSAGE e000(zcx_) WITH 'A1' 'A2' 'A3' 'A4' INTO gv_msg.
*        zcx_s=>raise(  ).
        RAISE EXCEPTION TYPE cx_ocs_error.
      CATCH cx_root INTO DATA(lx_).
        cut->add( lx_ ).
    ENDTRY.
    "then
    DATA(lt_msg) = cut->get_message_table(  ).
    DATA(ls_msg) = VALUE #( lt_msg[ 1 ] OPTIONAL ).
    cl_abap_unit_assert=>assert_equals( act = ls_msg-msgid
                                        exp = 'TN' ).
"@todo: add usual exception without interfaces
*    cl_abap_unit_assert=>assert_equals( act = ls_msg-exception
*                                        exp = lx_ ).
  ENDMETHOD.

  METHOD one_exception_zif_log.
    "when
    TRY.
        MESSAGE e000(zcx_) WITH 'A1' 'A2' 'A3' 'A4' INTO gv_msg.
        zcx_s=>raise(  ).
      CATCH zcx_s INTO DATA(lx_).
        cut->add( lx_ ).
    ENDTRY.
    "then
    cl_abap_unit_assert=>assert_equals( act = cut->get_message_table( )
                                        exp = lx_->get_message_table( ) ).
  ENDMETHOD.

  METHOD one_exc_ignore_importance_ovr.
    TRY.
        MESSAGE e000(zcx_) WITH 'A1' 'A2' 'A3' 'A4' INTO gv_msg.
        zcx_s=>raise(  ).
      CATCH zcx_s INTO DATA(lx_).
        cut->add( io_ = lx_ iv_importance = '1' ).
    ENDTRY.
    "then
    DATA(lt_msg) = cut->get_flatten_table(  ).
    DATA(ls_msg) = VALUE #( lt_msg[ 1 ] OPTIONAL ).
    cl_abap_unit_assert=>assert_equals( act = ls_msg-importance
                                        exp = '' ).

  ENDMETHOD.
  METHOD one_exc_respect_importance.
    TRY.
        MESSAGE e000(zcx_) WITH 'A1' 'A2' 'A3' 'A4' INTO gv_msg.
        zcx_s=>raise(  ).
      CATCH zcx_s INTO DATA(lx_).
        cut->add( lx_ ).
    ENDTRY.
    "then
    DATA(lt_msg) = cut->get_flatten_table(  ).
    DATA(ls_msg) = VALUE #( lt_msg[ 1 ] OPTIONAL ).
    cl_abap_unit_assert=>assert_equals( act = ls_msg-importance
                                        exp = '' ).

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

  METHOD add_empty_log.
    "when
    DATA(lo_log2) = zcl_minilog=>new(  ).
    cut->add( lo_log2 ).
    "then
    data: lt_ type zif_llm_log=>tt_msg.
    cl_abap_unit_assert=>assert_equals( act = cut->get_message_table(  )
                                        exp = lt_
                                      ).
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

CLASS ltc_minilog_cast DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.

  PRIVATE SECTION.
    DATA: gv_msg TYPE string.
    DATA: cut TYPE REF TO zcl_minilog.
    METHODS cast_to_logger FOR TESTING.
    METHODS cast_to_log FOR TESTING.
    METHODS cast_to_log_logger FOR TESTING.
    METHODS setup.
ENDCLASS.

CLASS ltc_minilog_cast IMPLEMENTATION.

  METHOD setup.
    "given
    cut = zcl_minilog=>new(  ).
    MESSAGE e000(zcx_) WITH 'A1' 'A2' 'A3' 'A4' INTO gv_msg.
  ENDMETHOD.

  METHOD cast_to_log.
    "when
    DATA: li_log TYPE REF TO zif_llm_log.
    li_log = cut.
    "then
*    data(lo_type) = cl_abap_typedescr=>describe_by_data( li_log ).
*    cl_abap_unit_assert=>assert_equals( act = lo_type->absolute_name
*                                        exp = '' ).
    li_log->get_bapiret2_table(  ).
  ENDMETHOD.

  METHOD cast_to_logger.
    "when
    DATA: li_logger TYPE REF TO zif_llm_logger.
    li_logger = cut.
    "then
*    data(lo_type) = cl_abap_typedescr=>describe_by_data( li_logger ).
*    cl_abap_unit_assert=>assert_equals( act = lo_type->absolute_name
*                                        exp = '' ).
    li_logger->add(  ).
  ENDMETHOD.

  METHOD cast_to_log_logger.
    "when
    DATA: li_log TYPE REF TO zif_llm_log.
    DATA: li_logger TYPE REF TO zif_llm_logger.
    li_log = cut.
    li_logger ?= li_log. "convert logger to log.
    li_logger->add(  )->add( ).
    "then
    DATA(lt_msg) = li_log->get_flatten_table(  ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_msg )
                                        exp = 2 ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_log_converter DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.

  PRIVATE SECTION.
    DATA: gv_msg TYPE string.
    DATA: cut TYPE REF TO zcl_minilog.
    METHODS convert_to_flatten_table FOR TESTING.
    METHODS convert_to_bapiret2_table FOR TESTING.
    METHODS convert_to_idoc_status_table FOR TESTING.
    METHODS setup.
ENDCLASS.


CLASS ltc_log_converter IMPLEMENTATION.

  METHOD setup.
    "given
    cut = zcl_minilog=>new(  ).
    MESSAGE e000(zcx_) WITH 'A1' 'A2' 'A3' 'A4' INTO gv_msg.
    cut->add( ).
    cut->add( sy ).
  ENDMETHOD.

  METHOD convert_to_flatten_table.
    "when
    DATA(lt_) = cut->get_converter( )->get_flatten_table( ).
    "then
    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( lt_ ).
    cl_abap_unit_assert=>assert_equals( act = lo_type->absolute_name
                                        exp = '\INTERFACE=ZIF_LOG_CONVERTER\TYPE=TT_MSG' ).
  ENDMETHOD.

  METHOD convert_to_bapiret2_table.
    "when
    DATA(lt_) = cut->get_converter( )->get_bapiret2_table( ).
    "then
    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( lt_ ).
    cl_abap_unit_assert=>assert_equals( act = lo_type->absolute_name
                                        exp = '\TYPE=BAPIRET2_T' ).
  ENDMETHOD.

  METHOD convert_to_idoc_status_table.
    "when
    DATA(lt_) = cut->get_converter( )->get_idoc_status_table(
                                      iv_docnum   = '012345'
                                      iv_status   = '53'
                                    ).
    "then
    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( lt_ ).
    cl_abap_unit_assert=>assert_equals( act = lo_type->absolute_name
                                        exp = '\TYPE=BDTIDOCSTA' ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_log_analyser DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.

  PRIVATE SECTION.
    DATA: gv_msg TYPE string.
    DATA: cut TYPE REF TO zcl_minilog.
    METHODS setup.

    METHODS analyse_has_error FOR TESTING.
    METHODS analyse_has_no_error FOR TESTING.
    METHODS analyse_has_success_only FOR TESTING.
    METHODS analyse_has_no_success_only FOR TESTING.
    METHODS analyse_has_warning_or_better FOR TESTING.
ENDCLASS.

CLASS ltc_log_analyser IMPLEMENTATION.

  METHOD setup.
    "given
    cut = zcl_minilog=>new(  ).
    MESSAGE s000(zcx_) WITH 'A1' 'A2' 'A3' 'A4' INTO gv_msg.
  ENDMETHOD.

  METHOD analyse_has_error.
    "when
    cut->add( sy ).
    cut->e( sy ).
    DATA(lv_) = cut->get_analyser( )->has_error( ).
    "then
    cl_abap_unit_assert=>assert_equals( act = lv_
                                        exp = abap_true ).
  ENDMETHOD.
  METHOD analyse_has_no_error.
    "when
    cut->add( sy ).
    cut->w( sy ).
    DATA(lv_) = cut->get_analyser( )->has_error( ).
    "then
    cl_abap_unit_assert=>assert_equals( act = lv_
                                        exp = abap_false ).
  ENDMETHOD.

  METHOD analyse_has_success_only.
    "when
    cut->s( sy ).
    cut->add( sy ).
    DATA(lv_) = cut->get_analyser( )->has_success_only( ).
    "then
    cl_abap_unit_assert=>assert_equals( act = lv_
                                        exp = abap_true ).
  ENDMETHOD.

  METHOD analyse_has_no_success_only.
    "when
    cut->s( sy ).
    cut->w( sy ).
    DATA(lv_) = cut->get_analyser( )->has_success_only( ).
    "then
    cl_abap_unit_assert=>assert_equals( act = lv_
                                        exp = abap_false ).
  ENDMETHOD.

  METHOD analyse_has_warning_or_better.
    "when
    cut->s( sy ).
    cut->w( sy ).
    DATA(lv_) = cut->get_analyser( )->has_warning_or_better( ).
    "then
    cl_abap_unit_assert=>assert_equals( act = lv_
                                        exp = abap_true ).
  ENDMETHOD.

ENDCLASS.
