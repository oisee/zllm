CLASS lcl_ DEFINITION DEFERRED.

*CLASS zcl_llm_00_pat DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.
  PRIVATE SECTION.
*    DATA: mo_cut TYPE REF TO zif_llm_00_pat.
*    METHODS: setup FOR TESTING RAISING cx_static_check,
*      ctor FOR TESTING,
*      new_inst FOR TESTING,
*      new_from_file FOR TESTING,
*      apply_pos FOR TESTING,
*      apply_neg_unbound FOR TESTING,
*      apply_neg_no_assign FOR TESTING,
*      new_neg_defaults FOR TESTING,
*      new_from_file_neg FOR TESTING.
*    TYPES: BEGIN OF ts_,
*             name  TYPE string,
*             item  TYPE string,
*             value TYPE string,
*           END OF ts_.
*    DATA: ms_ TYPE ts_.
*    DATA: mo_file TYPE REF TO zif_llm_00_file.
ENDCLASS.

CLASS lcl_ IMPLEMENTATION.
*
*  METHOD setup.
*    mo_file = zcl_llm_00_file_mock=>new(
*      'Hello {NAME}'
*    ).
*    mo_cut = zcl_llm_00_pat=>new_from_file(
*      io_        = mo_file
*      iv_prefix  = '{'
*      iv_postfix = '}'
*    ).
*  ENDMETHOD.
*
*  METHOD ctor.
*    DATA(lo_instance) = zcl_llm_00_pat=>new( iv_ = 'Test {ITEM}' iv_prefix = '[' iv_postfix = ']' ).
*    cl_abap_unit_assert=>assert_not_initial( act = lo_instance ).
*  ENDMETHOD.
*
*  METHOD new_inst.
*    DATA(lo_instance) = zcl_llm_00_pat=>new( iv_ = 'Dynamic {VALUE}' iv_prefix = '<' iv_postfix = '>' ).
*    cl_abap_unit_assert=>assert_not_initial( act = lo_instance ).
*  ENDMETHOD.
*
*  METHOD new_from_file.
*    DATA(lo_file) = zcl_llm_00_file_smw0=>new( 'ZLLM_00_TEST.TXT' ).
*    DATA(lo_instance) = zcl_llm_00_pat=>new_from_file(
*      io_ = lo_file
*      iv_prefix = '{'
*      iv_postfix = '}'
*    ).
*    DATA(lv_act) = lo_instance->apply( REF #( 'Keke' ) ).
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act  = lv_act
*        exp  = 'IN: Keke =)'
*    ).
*  ENDMETHOD.
*
*  METHOD apply_pos.
*    ms_-name = `World`.
*    DATA(lv_result) = mo_cut->apply( REF #( ms_ ) ).
*    cl_abap_unit_assert=>assert_equals( act = lv_result exp = 'Hello World' ).
*  ENDMETHOD.
*
*  METHOD apply_neg_unbound.
*    DATA: lr_ TYPE REF TO data.
*    DATA(lv_result) = mo_cut->apply( lr_ ).
*    DATA(lv_exp) = mo_file->get_string( ).
*    cl_abap_unit_assert=>assert_equals(
*      exp = lv_exp
*      act = lv_result
*    ).
*  ENDMETHOD.
*
*  METHOD apply_neg_no_assign.
*    DATA: lr_data TYPE REF TO data.
*    DATA(lv_result) = mo_cut->apply( ir_ = lr_data ).
*    DATA(lv_exp) = mo_file->get_string( ).
*    cl_abap_unit_assert=>assert_equals(
*      exp = lv_exp
*      act = lv_result
*    ).
*
*  ENDMETHOD.
*
*  METHOD new_neg_defaults.
*    DATA(lo_) = zcl_llm_00_pat=>new( iv_ = 'Without defaults' ).
*    DATA lo_instance TYPE REF TO zcl_llm_00_pat.
*    lo_instance ?= lo_.
*    cl_abap_unit_assert=>assert_equals( act = lo_instance->mv_prefix  exp = '{' ).
*    cl_abap_unit_assert=>assert_equals( act = lo_instance->mv_postfix exp = '}' ).
*  ENDMETHOD.
*
*  METHOD new_from_file_neg.
*    DATA(lo_file) = zcl_llm_00_file_smw0=>new( '' ).
*    DATA(lo_) = zcl_llm_00_pat=>new_from_file( io_ = lo_file ).
*    DATA lo_instance TYPE REF TO zcl_llm_00_pat.
*    lo_instance ?= lo_.
*    cl_abap_unit_assert=>assert_initial( act = lo_instance->mv_ ).
*  ENDMETHOD.

ENDCLASS.
