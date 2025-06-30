CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_llm_log_converter DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      cut TYPE REF TO zif_llm_log_converter.  "class under test

    METHODS: setup.
    METHODS: get_bapiret2_table FOR TESTING.
    METHODS: get_flatten_table FOR TESTING.
    METHODS: bapiret2_to_string FOR TESTING.
    METHODS: new FOR TESTING.
    METHODS: sy_to_bapiret2 FOR TESTING.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.

  METHOD setup.

    DATA(lo_log) = zcl_minilog=>new( ).
    lo_log->add( 'Hi!' ).
    cut = lo_log->get_converter( ).

  ENDMETHOD.


  METHOD get_bapiret2_table.

    DATA rt_ TYPE bapiret2_t.

    rt_ = cut->get_bapiret2_table(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = rt_
      exp   = rt_          "<--- please adapt expected value
    " msg   = 'Testing value rt_'
*     level =
    ).
  ENDMETHOD.


  METHOD get_flatten_table.

    DATA rt_ TYPE zif_llm_log_converter=>tt_msg.

    rt_ = cut->get_flatten_table(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = rt_
      exp   = rt_          "<--- please adapt expected value
    " msg   = 'Testing value rt_'
*     level =
    ).
  ENDMETHOD.


  METHOD bapiret2_to_string.

    DATA is_ TYPE bapiret2.
    DATA rv_ TYPE string.

    rv_ = zcl_llm_log_converter=>bapiret2_to_string( is_ ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_
      exp   = rv_          "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.


  METHOD new.

    DATA io_log TYPE REF TO zif_llm_log.
    DATA ro_ TYPE REF TO zcl_llm_log_converter.

    ro_ = zcl_llm_log_converter=>new( io_log ).

    cl_abap_unit_assert=>assert_equals(
      act   = ro_
      exp   = ro_          "<--- please adapt expected value
    " msg   = 'Testing value ro_'
*     level =
    ).
  ENDMETHOD.


  METHOD sy_to_bapiret2.

    DATA is_ TYPE syst.
    DATA rs_ TYPE bapiret2.

    rs_ = zcl_llm_log_converter=>sy_to_bapiret2( is_ ).

    cl_abap_unit_assert=>assert_equals(
      act   = rs_
      exp   = rs_          "<--- please adapt expected value
    " msg   = 'Testing value rs_'
*     level =
    ).
  ENDMETHOD.




ENDCLASS.
