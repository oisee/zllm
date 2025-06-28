CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_llm_00_formula_list DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*  PRIVATE SECTION.
*    DATA:
*      mo_cut TYPE REF TO ZIF_LLM_00_FORMULA_LIST.  "class under test
*
*    METHODS: setup.
*    METHODS: all FOR TESTING.
*    METHODS: next FOR TESTING.
*    METHODS: rewind FOR TESTING.
*    METHODS: new FOR TESTING.
*    METHODS: new_from_folder FOR TESTING.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.
*
*  METHOD setup.
*    mo_cut ?= ZIF_LLM_00_FORMULA_LIST=>new_from_folder(
*      iv_     = 'C:\TEMP\@zllm\'
*      iv_mask = '*.md'
*    ).
*  ENDMETHOD.
*
*  METHOD all.
*    DATA(lt_) = mo_cut->get(  ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lines( lt_ )
*      exp   = '3'
*    ).
*  ENDMETHOD.
*
*
*  METHOD next.
**    DATA(lo_) = mo_cut->next(  ).
**
**    cl_abap_unit_assert=>assert_bound( lo_ ).
*  ENDMETHOD.
*
*
*  METHOD rewind.
**    mo_cut->rewind(  ).
*
*  ENDMETHOD.
*
*
*  METHOD new.
**    DATA it_ TYPE zcl_llm_00_ptl=>tt_file.
**    DATA ro_ TYPE REF TO zcl_llm_00_ptl.
**
**    ro_ = f_cut->new( it_ ).
**
**    cl_abap_unit_assert=>assert_equals(
**      act   = ro_
**      exp   = ro_          "<--- please adapt expected value
**    " msg   = 'Testing value ro_'
***     level =
**    ).
*  ENDMETHOD.
*
*
*  METHOD new_from_folder.
*    DATA lo_ TYPE REF TO ZIF_LLM_00_FORMULA_LIST.
*    lo_ = ZIF_LLM_00_FORMULA_LIST=>new_from_folder(
*      iv_     = 'C:\TEMP\@ZLLM\'
*      iv_mask = '*.md'
*    ).
*    DATA(lt_all) = lo_->get( ).
*    cl_abap_unit_assert=>assert_equals(
*      act   = lt_all
*      exp   = lt_all
*    ).
*  ENDMETHOD.
ENDCLASS.
