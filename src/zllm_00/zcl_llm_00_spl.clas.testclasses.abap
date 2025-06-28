CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_llm_00_spl DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      cut TYPE REF TO zcl_llm_00_spl.  "class under test

    METHODS: setup.
    METHODS: teardown.

    METHODS: adjust          FOR TESTING.
    METHODS: get_leafs       FOR TESTING.
    METHODS: get_nodes       FOR TESTING.

    METHODS: new             FOR TESTING.
    METHODS: split           FOR TESTING.
    METHODS: split_flex_into FOR TESTING.
    METHODS: split_h_into    FOR TESTING.
    METHODS: split_v_into    FOR TESTING.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.

  METHOD setup.
    cut = zcl_llm_00_spl=>new(
      io_     = cl_gui_container=>default_screen
      iv_mode = 'T'
    ).
  ENDMETHOD.

  METHOD teardown.



  ENDMETHOD.


  METHOD adjust.

    DATA is_ TYPE zcl_llm_00_spl=>ts_.
    DATA ct_ TYPE zcl_llm_00_spl=>tt_.
    DATA rs_ TYPE zcl_llm_00_spl=>ts_.

    rs_ = cut->adjust(
      EXPORTING
        is_ = is_
      CHANGING
        ct_ = ct_ ).

    cl_abap_unit_assert=>assert_equals(
      act = ct_
      exp = ct_          "<--- please adapt expected value
    " msg = 'Testing value ct_'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act = rs_
      exp = rs_          "<--- please adapt expected value
    " msg = 'Testing value rs_'
*     level =
    ).
  ENDMETHOD.


  METHOD get_leafs.

    DATA rt_ TYPE zcl_llm_00_spl=>tt_.

    rt_ = cut->get_leafs(  ).

    cl_abap_unit_assert=>assert_equals(
      act = rt_
      exp = rt_          "<--- please adapt expected value
    " msg = 'Testing value rt_'
*     level =
    ).
  ENDMETHOD.


  METHOD get_nodes.

    DATA rt_ TYPE zcl_llm_00_spl=>tt_.

    rt_ = cut->get_nodes(  ).

    cl_abap_unit_assert=>assert_equals(
      act = rt_
      exp = rt_          "<--- please adapt expected value
    " msg = 'Testing value rt_'
*     level =
    ).
  ENDMETHOD.

  METHOD new.

    DATA io_ TYPE REF TO cl_gui_container.
    DATA iv_mode TYPE string.
    DATA ro_ TYPE REF TO zcl_llm_00_spl.

    ro_ = zcl_llm_00_spl=>new(
*       IO_ = io_
*       IV_MODE = iv_Mode
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ro_
      exp = ro_          "<--- please adapt expected value
    " msg = 'Testing value ro_'
*     level =
    ).
  ENDMETHOD.


  METHOD split.

    DATA iv_ TYPE string.
    DATA rt_ TYPE zcl_llm_00_spl=>tt_.

    rt_ = cut->split( iv_ ).

    cl_abap_unit_assert=>assert_equals(
      act = rt_
      exp = rt_          "<--- please adapt expected value
    " msg = 'Testing value rt_'
*     level =
    ).
  ENDMETHOD.



  METHOD split_flex_into.

    DATA iv_ TYPE i.
    DATA iv_split_vertical_first TYPE sap_bool.
    DATA iv_threshold TYPE i.
    DATA rt_ TYPE zcl_llm_00_spl=>tt_.

    rt_ = cut->split_flex_into(
      iv_ = iv_
*     IV_SPLIT_VERTICAL_FIRST = iv_Split_Vertical_First
*     IV_THRESHOLD = iv_Threshold
    ).

    cl_abap_unit_assert=>assert_equals(
      act = rt_
      exp = rt_          "<--- please adapt expected value
    " msg = 'Testing value rt_'
*     level =
    ).
  ENDMETHOD.


  METHOD split_h_into.

    DATA iv_ TYPE i.
    DATA rt_ TYPE zcl_llm_00_spl=>tt_.

    rt_ = cut->split_h_into( iv_ ).

    cl_abap_unit_assert=>assert_equals(
      act = rt_
      exp = rt_          "<--- please adapt expected value
    " msg = 'Testing value rt_'
*     level =
    ).
  ENDMETHOD.

  METHOD split_v_into.

    DATA iv_ TYPE i.
    DATA rt_ TYPE zcl_llm_00_spl=>tt_.

    rt_ = cut->split_v_into( iv_ ).

    cl_abap_unit_assert=>assert_equals(
      act = rt_
      exp = rt_          "<--- please adapt expected value
    " msg = 'Testing value rt_'
*     level =
    ).
  ENDMETHOD.

ENDCLASS.
