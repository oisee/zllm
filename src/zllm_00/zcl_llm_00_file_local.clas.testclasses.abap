
CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zif_llm_00_file.  "class under test

    METHODS: setup.
    METHODS: teardown.
    METHODS: get_string FOR TESTING.
    METHODS: get_xstring FOR TESTING.

ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.

  METHOD setup.
    mo_cut ?= zcl_llm_00_file_local=>new( 'C:\TEMP\ZLLM_LOCAL.TXT' ).
  ENDMETHOD.


  METHOD teardown.

  ENDMETHOD.


  METHOD get_string.

    DATA rv_ TYPE string.

    rv_ = mo_cut->get_string(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_
      exp   = 'IN: {IN} =)'          "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.


  METHOD get_xstring.
    DATA rv_ TYPE xstring.
    rv_ = mo_cut->get_xstring(  ).
    cl_abap_unit_assert=>assert_equals(
      act   = rv_
      exp   = rv_          "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.


ENDCLASS.
