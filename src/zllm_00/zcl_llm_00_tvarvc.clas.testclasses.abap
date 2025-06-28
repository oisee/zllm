
CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.

  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_llm_00_tvarvc.  "class under test

    METHODS: get_impossible_range     FOR TESTING.
    METHODS: get_parameter_by_name    FOR TESTING.
    METHODS: get_range_by_name        FOR TESTING.
    METHODS: get_default_from_a_value FOR TESTING.
    METHODS: get_default_from_a_range FOR TESTING.

*    METHODS: new_for_postfix for testing.
*    METHODS: p_post for testing.
*    METHODS: p_lang_en for testing.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.

  METHOD get_impossible_range.
    DATA(ltr_)    = zcl_llm_00_tvarvc=>get_impossible_range(  ).
    DATA(ltr_exp) = VALUE zcl_llm_00_tvarvc=>ttr_(
      ( sign = 'E' option = 'CP' low = '*' )
    ).
    cl_abap_unit_assert=>assert_equals(
      act = ltr_    "
      exp = ltr_exp "
    ).
  ENDMETHOD.

  METHOD get_parameter_by_name.

    DATA iv_name TYPE rvari_vnam.
    DATA iv_default TYPE tvarv_val.
    DATA rv_ TYPE tvarv_val.

    rv_ = zcl_llm_00_tvarvc=>get_parameter_by_name(
*       IV_NAME = iv_Name
*       IV_DEFAULT = iv_Default
    ).
  ENDMETHOD.


  METHOD get_range_by_name.
    DATA(ltr_) = zcl_llm_00_tvarvc=>get_range_by_name(
       iv_name = 'ZCOL_100_MICS_STRAT'
    ).
    cl_abap_unit_assert=>assert_not_initial( ltr_ ).
  ENDMETHOD.

  METHOD get_default_from_a_value.
    DATA(ltr_) = zcl_llm_00_tvarvc=>get_range_by_name(
       iv_name    = 'ZQPWOEIRUTYALSKDJFHGZMXNCBV102'
       iv_default = 'V'
    ).
    DATA(ltr_exp) = VALUE zcl_llm_00_tvarvc=>ttr_(
      ( sign = 'I' option = 'EQ' low = 'V' )
    ).
    cl_abap_unit_assert=>assert_not_initial( ltr_ ).
  ENDMETHOD.

  METHOD get_default_from_a_range.
    DATA(ltr_) = VALUE zcl_llm_00_tvarvc=>ttr_(
      (                          low = '1' )
      (            option = 'EQ' low = '2' )
      ( sign = 'I'               low = '3' )
      ( sign = 'I' option = 'EQ' low = '4' )
    ).
    DATA(ltr_act) = zcl_llm_00_tvarvc=>get_range_by_name(
       iv_name     = 'ZQPWOEIRUTYALSKDJFHGZMXNCBV102'
       itr_default = ltr_
    ).
    DATA(ltr_exp) = VALUE zcl_llm_00_tvarvc=>ttr_(
      ( sign = 'I' option = 'EQ' low = '1' )
      ( sign = 'I' option = 'EQ' low = '2' )
      ( sign = 'I' option = 'EQ' low = '3' )
      ( sign = 'I' option = 'EQ' low = '4' )
    ).
    cl_abap_unit_assert=>assert_equals(
      act = ltr_act "
      exp = ltr_exp "
    ).
  ENDMETHOD.

ENDCLASS.
