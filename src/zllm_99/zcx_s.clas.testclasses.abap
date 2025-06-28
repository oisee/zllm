*"* use this source file for your ABAP unit test classes

CLASS ltc_zcx_s_instance DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.

  PRIVATE SECTION.
    DATA: gv_msg TYPE string.
    DATA: cut TYPE REF TO zcx_s.
    METHODS one_msg_implicit FOR TESTING RAISING zcx_s.
    METHODS one_msg_implicit_type_e FOR TESTING RAISING zcx_s.
    METHODS one_msg_implicit_type_e_as_w FOR TESTING RAISING zcx_s.
    METHODS one_msg_implicit_type_e_as_a FOR TESTING RAISING zcx_s.
    METHODS one_msg_implicit_type_e_as_i FOR TESTING RAISING zcx_s.
    METHODS one_msg_implicit_type_e_as_s FOR TESTING RAISING zcx_s.
    METHODS one_msg_implicit_type_args FOR TESTING RAISING zcx_s.
    METHODS one_msg_explicit FOR TESTING RAISING zcx_s.
    METHODS one_msg_explicit_type FOR TESTING RAISING zcx_s.
    METHODS one_msg_add_and_throw FOR TESTING RAISING zcx_s.
    METHODS two_msg_implicit FOR TESTING RAISING zcx_s.
    METHODS two_msg_implicit_order FOR TESTING RAISING zcx_s.
    METHODS two_msg_chain_add_add FOR TESTING RAISING zcx_s.
    METHODS setup RAISING zcx_s.
ENDCLASS.

CLASS ltc_zcx_s_instance IMPLEMENTATION.

  METHOD setup.
    "given
    cut = zcx_s=>new(  ).
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
  METHOD one_msg_implicit_type_e_as_a.
    "when
    cut->a(  ).
    "then
    DATA(lt_msg) = cut->get_flatten_table(  ).
    DATA(ls_msg) = VALUE #( lt_msg[ 1 ] OPTIONAL ).
    cl_abap_unit_assert=>assert_equals( act = ls_msg-msgty
                                        exp = 'A' ).
  ENDMETHOD.
  METHOD one_msg_implicit_type_e_as_i.
    "when
    cut->i(  ).
    "then
    DATA(lt_msg) = cut->get_flatten_table(  ).
    DATA(ls_msg) = VALUE #( lt_msg[ 1 ] OPTIONAL ).
    cl_abap_unit_assert=>assert_equals( act = ls_msg-msgty
                                        exp = 'I' ).
  ENDMETHOD.
  METHOD one_msg_implicit_type_e_as_s.
    "when
    cut->s(  ).
    "then
    DATA(lt_msg) = cut->get_flatten_table(  ).
    DATA(ls_msg) = VALUE #( lt_msg[ 1 ] OPTIONAL ).
    cl_abap_unit_assert=>assert_equals( act = ls_msg-msgty
                                        exp = 'S' ).
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
    "when
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

  METHOD one_msg_add_and_throw.
    "when
    cut->add( ).
    TRY.
        cut->throw( ).
      CATCH zcx_s INTO cut.
        MESSAGE e000(zcx_) INTO data(lv_msg).
    ENDTRY.
    "then
    DATA(lt_msg) = cut->get_flatten_table(  ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_msg )
                                        exp = 1 ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_zcx_s_static DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.

  PRIVATE SECTION.
    DATA: gv_msg TYPE string.
    DATA: cut TYPE REF TO zcx_s.
    METHODS one_msg_raise FOR TESTING.
    METHODS one_msg_raise_type_e FOR TESTING.
ENDCLASS.

CLASS ltc_zcx_s_static IMPLEMENTATION.

  METHOD one_msg_raise.
    "given
    MESSAGE e000(zcx_) WITH 'A1' 'A2' 'A3' 'A4' INTO gv_msg.
    "when
    TRY.
        zcx_s=>raise( ).
      CATCH zcx_s INTO DATA(cut).
        MESSAGE e000(zcx_) INTO DATA(lv_msg).
    ENDTRY.
    "then
    DATA(lt_msg) = cut->get_flatten_table(  ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_msg )
                                        exp = 1 ).
  ENDMETHOD.

  METHOD one_msg_raise_type_e.
    "given
    MESSAGE e000(zcx_) WITH 'A1' 'A2' 'A3' 'A4' INTO gv_msg.
    "when
    TRY.
        zcx_s=>raise( ).
      CATCH zcx_s INTO DATA(cut).
        MESSAGE e000(zcx_) INTO DATA(lv_msg).
    ENDTRY.
    "then
    DATA(lt_msg) = cut->get_flatten_table(  ).
    DATA(ls_msg) = VALUE #( lt_msg[ 1 ] OPTIONAL ).
    cl_abap_unit_assert=>assert_equals( act = ls_msg-msgty
                                        exp = 'E' ).
  ENDMETHOD.


ENDCLASS.
