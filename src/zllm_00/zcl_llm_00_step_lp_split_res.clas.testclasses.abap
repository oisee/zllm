CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_llm_00_step_lp_split_res DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.

  PRIVATE SECTION.
    TYPES: string_t TYPE Zif_LLM_00_types=>string_t.
    DATA:
      mo_cut TYPE REF TO zif_llm_00_step_result,         "class under test
      mo_    TYPE REF TO zcl_llm_00_step_lp_split_res .  "class under test

    DATA: mo_llm TYPE REF TO zif_llm_00_llm_lazy.

    METHODS: setup.
    METHODS: predict_tokens    FOR TESTING.
    METHODS: to_string         FOR TESTING.
    METHODS: new               FOR TESTING.
    METHODS: new_from_sting    FOR TESTING.
    METHODS: new_from_table    FOR TESTING.
    METHODS: split_at          FOR TESTING.
    METHODS: split_step_result FOR TESTING.
    METHODS: split_string      FOR TESTING.
    METHODS: split_string_in_two_and_adjust FOR TESTING.
    METHODS: split_table       FOR TESTING.
    METHODS: tokens            FOR TESTING.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.

  METHOD setup.
    DATA iv_ TYPE string.
*   DATA it_ TYPE string_t.
*   DATA io_ TYPE REF TO zif_llm_00_step_result.
*   DATA lo_llm TYPE REF TO zif_llm_00_llm_lazy.

    mo_llm = zcl_llm_00_llm_lazy_mock=>new(
      VALUE #( api_model = 'GPT-4O' api_max_token = 16000 api_token_split_limit = 100 )
    ).
    mo_cut =  zcl_llm_00_step_lp_split_res=>new_from_string(
      iv_ =
      `# Header                                                                                                       ` && zif_llm=>n &&
      ` quazimordo ?                                                                                                  ` && zif_llm=>n &&
      ` stupid text test text test                          stupid text test text test                                ` && zif_llm=>n &&
      `                           stupid text test text test                          stupid text test text test      ` && zif_llm=>n &&
      `## Header 2                                                                                                    ` && zif_llm=>n &&
      `### Header 3                                                                                                   ` && zif_llm=>n &&
      ` quazimordo ?                                                                                                  ` && zif_llm=>n &&
      ` stupid text test text test                                                                                    ` && zif_llm=>n &&
      `                                                                                                               ` && zif_llm=>n &&
      ` stupid text test text test                          stupid text test text test                                ` && zif_llm=>n &&
      `                           stupid text test text test                          stupid text test text test      ` && zif_llm=>n &&
      `## Header 2                                                                                                    ` && zif_llm=>n &&
      ` stupid text test text test                          stupid text test text test                                ` && zif_llm=>n &&
      `                           stupid text test text test                          stupid text test text test      ` && zif_llm=>n &&
      `# Header                                                                                                       ` && zif_llm=>n &&
      ` quazimordo ?                                                                                                  ` && zif_llm=>n &&
      ` stupid text test text test                                                                                    ` && zif_llm=>n &&
      `                                                                                                               ` && zif_llm=>n &&
      ` stupid text test text test                          stupid text test text test                                ` && zif_llm=>n &&
      `                           stupid text test text test                          stupid text test text test      ` && zif_llm=>n &&
      `## Header 2                                                                                                    ` && zif_llm=>n &&
      ` quazimordo ?                                                                                                  ` && zif_llm=>n &&
      ` stupid text test text test                                                                                    ` && zif_llm=>n &&
      `                                                                                                               ` && zif_llm=>n &&
      `                                                                                                               `
      io_llm = mo_llm
    ).
    mo_ ?= mo_cut.

  ENDMETHOD.

  METHOD predict_tokens.
    DATA rv_ TYPE i.
    rv_ = mo_cut->predict_tokens( ).
    cl_abap_unit_assert=>assert_equals(
      act   = rv_
      exp   = rv_          "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.


  METHOD to_string.
    DATA rv_ TYPE string.
    rv_ = mo_cut->to_string(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_
      exp   = rv_          "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.


  METHOD new.
*    DATA ir_ TYPE REF TO data.
*    DATA io_ TYPE REF TO zif_llm_00_step_result.
*    DATA io_llm TYPE REF TO zif_llm_00_llm_lazy.
*    DATA ro_ TYPE REF TO zif_llm_00_step_result.
*
*    ro_ = zcl_llm_00_step_lp_split_res=>new(
**       IR_ = ir_
**       IO_ = io_
*        io_llm = io_llm ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = ro_
*      exp   = ro_          "<--- please adapt expected value
*    " msg   = 'Testing value ro_'
**     level =
*    ).
  ENDMETHOD.


  METHOD new_from_sting.

    DATA io_ TYPE REF TO zif_llm_00_step_result.
    DATA ro_ TYPE REF TO zif_llm_00_step_result.

    ro_ = zcl_llm_00_step_lp_split_res=>new_from_string(
        iv_    = `What if? What if? What if? What if? What if? What if? What if? What if? What if? What if? What if? What if? What if? What if? What if?`
        io_llm = mo_llm ).

    cl_abap_unit_assert=>assert_equals(
      act   = ro_
      exp   = ro_          "<--- please adapt expected value
    " msg   = 'Testing value ro_'
*     level =
    ).
  ENDMETHOD.


  METHOD new_from_table.

    DATA io_ TYPE REF TO zif_llm_00_step_result.
    DATA ro_ TYPE REF TO zif_llm_00_step_result.

    ro_ = zcl_llm_00_step_lp_split_res=>new_from_table(
        it_    = VALUE #(
          ( `What if? What if? What if? What if? What if? What if? What if? What if? What if? What if? What if? What if? What if? What if? What if?` )
          ( `Yaaay! Yaaay! Yaaay! Yaaay! Yaaay! Yaaay! Yaaay! Yaaay! Yaaay! Yaaay! Yaaay! Yaaay! Yaaay! Yaaay! Yaaay! Yaaay! Yaaay! Yaaay! Yaaay!` )
        )
        io_llm = mo_llm ).

    cl_abap_unit_assert=>assert_equals(
      act   = ro_
      exp   = ro_          "<--- please adapt expected value
    " msg   = 'Testing value ro_'
*     level =
    ).
  ENDMETHOD.


  METHOD split_at.
    DATA iv_ TYPE string.
    DATA iv_offset TYPE i.
    DATA rt_ TYPE string_t.

    rt_ = mo_->split_at(
        iv_ = `Hi, we are testing the split`
        iv_offset = 4 ).

    cl_abap_unit_assert=>assert_equals(
      act   = rt_[ 2 ]
      exp   = `we are testing the split`
    ).
  ENDMETHOD.


  METHOD split_step_result.
*    DATA io_ TYPE REF TO zif_llm_00_step_result.
*    DATA rt_ TYPE string_t.
*
*    rt_ = mo_cut->split_step_result( io_ ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = rt_
*      exp   = rt_          "<--- please adapt expected value
*    " msg   = 'Testing value rt_'
**     level =
*    ).
  ENDMETHOD.


  METHOD split_string.

    DATA iv_ TYPE string.
    DATA iv_offset TYPE i.
    DATA rt_ TYPE string_t.

    rt_ = mo_->split_at(
        iv_ = `Hi, we are testing the split`
        iv_offset = 4 ).

    cl_abap_unit_assert=>assert_equals(
      act   = rt_[ 2 ]
      exp   = `we are testing the split`
    ).
  ENDMETHOD.


  METHOD split_string_in_two_and_adjust.

    DATA(lt_) = mo_cut->collect( ).

*    DATA iv_ TYPE string.
*    DATA rt_ TYPE string_t.
*
*    rt_ = mo_cut->split_string_in_two_and_adjust( iv_ ).
*
    cl_abap_unit_assert=>assert_equals(
      act   = lt_
      exp   = lt_          "<--- please adapt expected value
    " msg   = 'Testing value rt_'
*     level =
    ).
  ENDMETHOD.


  METHOD split_table.
*
*    DATA it_ TYPE string_t.
*    DATA rt_ TYPE string_t.
*
*    rt_ = mo_cut->split_table( it_ ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = rt_
*      exp   = rt_          "<--- please adapt expected value
*    " msg   = 'Testing value rt_'
**     level =
*    ).
  ENDMETHOD.


  METHOD tokens.

    DATA iv_ TYPE string.
    DATA rv_ TYPE i.

    rv_ = mo_->tokens( `kekeke kokoko zozozo =)` ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_
      exp   = rv_          "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.

ENDCLASS.
