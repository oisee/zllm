CLASS lcl_ DEFINITION DEFERRED.

CLASS zcl_llm_00_predictoken DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.
  PRIVATE SECTION.
    TYPES: string_t TYPE zif_llm_00_types=>string_t.
    DATA: mo_cut TYPE REF TO zcl_llm_00_predictoken.
    CLASS-DATA: gs_coeff TYPE zcl_llm_00_predictoken=>ts_coeff.
    METHODS: setup,
      test_constructor       FOR TESTING,
      test_class_constructor FOR TESTING,
      new_instance_creation  FOR TESTING,
      new_for_model_instance FOR TESTING,
      predict_single         FOR TESTING,
      predict_for_itab       FOR TESTING,
      extract_features       FOR TESTING,
      minimal_prediction     FOR TESTING.

    DATA: mv_in TYPE string.
ENDCLASS.

CLASS lcl_ IMPLEMENTATION.
  METHOD setup.
    mo_cut = zcl_llm_00_predictoken=>new( ).
    DO 84 TIMES.
      mv_in = mv_in && `This is a test. `.
    ENDDO.
  ENDMETHOD.

  METHOD test_constructor.
    cl_abap_unit_assert=>assert_not_initial( act = mo_cut ).
  ENDMETHOD.

  METHOD test_class_constructor.
    cl_abap_unit_assert=>assert_not_initial( act = zcl_llm_00_predictoken=>gs_co_gpt ).
    cl_abap_unit_assert=>assert_not_initial( act = zcl_llm_00_predictoken=>gs_co_mistral ).
  ENDMETHOD.

  METHOD new_instance_creation.
    DATA(lo_instance) = zcl_llm_00_predictoken=>new( gs_coeff ).
    cl_abap_unit_assert=>assert_not_initial( act = lo_instance ).
  ENDMETHOD.

  METHOD new_for_model_instance.
    DATA(lo_instance_gpt) = zcl_llm_00_predictoken=>new_for_model_type( iv_ = 'GPT' ).
    DATA(lo_instance_mistral) = zcl_llm_00_predictoken=>new_for_model_type( iv_ = 'MISTRAL' ).

    cl_abap_unit_assert=>assert_not_initial( act = lo_instance_gpt ).
    cl_abap_unit_assert=>assert_not_initial( act = lo_instance_mistral ).
  ENDMETHOD.

  METHOD predict_single.
    DATA(lo_instance) = zcl_llm_00_predictoken=>new_for_model_type( iv_ = 'GPT' ).
    DATA(lv_result) = lo_instance->predict(
      iv_ = mv_in
    ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lv_result
        exp = 157
    ).
  ENDMETHOD.

  METHOD predict_for_itab.
    DATA(lo_instance) = zcl_llm_00_predictoken=>new_for_model_type( iv_ = 'GPT' ).
    DATA(lt_text) = VALUE string_t( ( `This is a test.` ) ( `Another test.` ) ).
    DATA(lv_result) = lo_instance->predict_for_itab( it_ = lt_text ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lv_result
        exp = ceil( abs( mo_cut->ms_-intercept ) )
    ).
  ENDMETHOD.

  METHOD extract_features.
    DATA(ls_features) = zcl_llm_00_predictoken=>extract_features( iv_ = 'This is a test. For real' ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = ls_features-whitespaces
        exp = 5
    ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = ls_features-lines
        exp = 1
    ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = ls_features-words
        exp = 6
    ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = ls_features-punctuations
        exp = 1
    ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = ls_features-sentences
        exp = 2
    ).
  ENDMETHOD.

  METHOD minimal_prediction.
    DATA(lv_act) = mo_cut->predict( '' ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lv_act
        exp = ceil( abs( mo_cut->ms_-intercept ) )
    ).
*--------------------------------------------------------------------*
    DATA(lv_min) = 10.
    DATA(lo_cut) = zcl_llm_00_predictoken=>new_for_model_type(
      iv_min = lv_min
    ).
    DATA(lv_act2) = lo_cut->predict( '' ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lv_act2
        exp = lv_min
    ).
  ENDMETHOD.

ENDCLASS.
