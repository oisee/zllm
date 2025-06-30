*CLASS lcl_ DEFINITION DEFERRED.
*CLASS zcl_log_analyser DEFINITION LOCAL FRIENDS lcl_.
*
*CLASS lcl_ DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS
*.
*  PRIVATE SECTION.
*    DATA:
*      cut TYPE REF TO zif_log_analyser.  "class under test
*
*    METHODS: setup.
*    METHODS: has_any FOR TESTING.
*    METHODS: new FOR TESTING.
*ENDCLASS.       "lcl_
*
*
*CLASS lcl_ IMPLEMENTATION.
*  METHOD setup.
*
*    DATA(lo_log) = zcl_minilog=>new( ).
*    lo_log->add( 'Hi!' ).
*    cut = lo_log->get_analyser( ).
*
*  ENDMETHOD.
*
*  METHOD has_any.
*    DATA rv_ TYPE abap_bool.
*    rv_ = cut->has_any( ).
*    cl_abap_unit_assert=>assert_equals(
*      act   = rv_
*      exp   = abap_true   "<--- please adapt expected value
*    " msg   = 'Testing value rv_'
**     level =
*    ).
*  ENDMETHOD.
*
*
*  METHOD new.
*    DATA lo_log TYPE REF TO zif_log.
*    DATA ro_ TYPE REF TO zcl_log_analyser.
*
*    lo_log = zcl_minilog=>new( ).
*    ro_ = zcl_log_analyser=>new( lo_log ).
*
*  ENDMETHOD.
*
*ENDCLASS.
CLASS lcl_uat DEFINITION DEFERRED.
CLASS zcl_llm_log_analyser DEFINITION LOCAL FRIENDS lcl_uat.

CLASS lcl_uat DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zif_llm_log_analyser.  "class under test
    METHODs: setup.
    METHODS: get_worst_bapiret2 FOR TESTING.
    METHODS: get_worst_message FOR TESTING.
    METHODS: get_worst_message_type FOR TESTING.
    METHODS: has_any FOR TESTING.
    METHODS: has_error FOR TESTING.
    METHODS: has_success_only FOR TESTING.
    METHODS: has_warning FOR TESTING.
    METHODS: has_warning_or_better FOR TESTING.
    METHODS: if_any_show_most_severe_as FOR TESTING.
    METHODS: if_error_show_most_severe_as FOR TESTING.
    METHODS: new FOR TESTING.
ENDCLASS.       "lcl_Uat


CLASS lcl_uat IMPLEMENTATION.

  METHOD setup.
    DATA(lo_log) = zcl_minilog=>new( ).
    lo_log->add( 'Hi!' ).
    f_cut = lo_log->get_analyser( ).

  ENDMETHOD.

  METHOD get_worst_bapiret2.

    DATA rs_ TYPE bapiret2.

    rs_ = f_cut->get_worst_bapiret2(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = rs_
      exp   = rs_          "<--- please adapt expected value
    " msg   = 'Testing value rs_'
*     level =
    ).
  ENDMETHOD.


  METHOD get_worst_message.

    DATA rs_ TYPE zif_llm_log_analyser=>ts_msg.

    rs_ = f_cut->get_worst_message(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = rs_
      exp   = rs_          "<--- please adapt expected value
    " msg   = 'Testing value rs_'
*     level =
    ).
  ENDMETHOD.


  METHOD get_worst_message_type.

    DATA rv_ TYPE symsgty.

    rv_ = f_cut->get_worst_message_type(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_
      exp   = rv_          "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.


  METHOD has_any.

    DATA rv_ TYPE abap_bool.

    rv_ = f_cut->has_any(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_
      exp   = rv_          "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.


  METHOD has_error.

    DATA rv_ TYPE abap_bool.

    rv_ = f_cut->has_error(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_
      exp   = rv_          "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.


  METHOD has_success_only.

    DATA rv_ TYPE abap_bool.

    rv_ = f_cut->has_success_only(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_
      exp   = rv_          "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.


  METHOD has_warning.

    DATA rv_ TYPE abap_bool.

    rv_ = f_cut->has_warning(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_
      exp   = rv_          "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.


  METHOD has_warning_or_better.

    DATA rv_ TYPE abap_bool.

    rv_ = f_cut->has_warning_or_better(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_
      exp   = rv_          "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.


  METHOD if_any_show_most_severe_as.

    DATA iv_type TYPE syst_msgty.

    f_cut->if_any_show_most_severe_as( iv_type ).

  ENDMETHOD.


  METHOD if_error_show_most_severe_as.

    DATA iv_type TYPE syst_msgty.

    f_cut->if_error_show_most_severe_as( iv_type ).

  ENDMETHOD.


  METHOD new.

    DATA io_log TYPE REF TO zif_llm_log.
    DATA ro_ TYPE REF TO zcl_llm_log_analyser.

    ro_ = zcl_llm_log_analyser=>new( io_log ).

    cl_abap_unit_assert=>assert_equals(
      act   = ro_
      exp   = ro_          "<--- please adapt expected value
    " msg   = 'Testing value ro_'
*     level =
    ).
  ENDMETHOD.




ENDCLASS.
