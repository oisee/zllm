CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_llm_00_file_list_bin DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zif_llm_00_file_list.  "class under test

    METHODS: setup.
    METHODS: all  FOR TESTING.
    METHODS: save FOR TESTING.
    METHODS: new  FOR TESTING.
    METHODS: new_from_package FOR TESTING.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.

  METHOD setup.
    mo_cut ?= zcl_llm_00_file_list_bin=>new_from_bin(
      iv_bin  = '$ZLLM_00'
      iv_mask = '*.TXT'
    ).
  ENDMETHOD.

  METHOD all.
    DATA(lt_) = mo_cut->get(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = lines( lt_ )
      exp   = '1'
    ).
  ENDMETHOD.

  METHOD save.
    DATA(lo_file) = zcl_llm_00_file_mock=>new(
      iv_content = 'TEST'
      iv_path    = 'BIN-TEST.TXT'
    ).

    mo_cut->save( lo_file ).

    DATA(lo_saved) = mo_cut->get_by_name( lo_file->get_name( ) ).
    DATA(lv_saved_content) = lo_saved->get_string( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_saved_content
      exp   = lo_file->get_string( )
    ).

    data(lo_cut) = zcl_llm_00_file_list_bin=>new_from_bin(
      iv_bin  = '$ZLLM_00'
      iv_mask = '*.TXT'
    ).

    DATA(lo_saved2) = lo_cut->get_by_name( lo_saved->get_name( ) ).
    DATA(lv_saved_content2) = lo_saved2->get_string( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_saved_content2
      exp   = lv_saved_content
    ).

  ENDMETHOD.


  METHOD new.
*    DATA it_ TYPE zif_llm_00_file_list=>tt_file.
*    DATA ro_ TYPE REF TO zif_llm_00_file_list.
*
*    ro_ = f_cut->new( it_ ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = ro_
*      exp   = ro_          "<--- please adapt expected value
*    " msg   = 'Testing value ro_'
**     level =
*    ).
  ENDMETHOD.


  METHOD new_from_package.
    DATA lo_ TYPE REF TO zif_llm_00_file_list.
    lo_ = zcl_llm_00_file_list_bin=>new_from_bin(
      iv_bin  = '$ZLLM_00'
      iv_mask = '*.TXT'
    ).
    DATA(lt_all) = lo_->get( ).
    cl_abap_unit_assert=>assert_equals(
      act   = lt_all
      exp   = lt_all
    ).
  ENDMETHOD.
ENDCLASS.
