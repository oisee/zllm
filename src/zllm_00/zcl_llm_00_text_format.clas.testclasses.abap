CLASS ltcl_llm_00_text_format DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS: lc_n TYPE string VALUE cl_abap_char_utilities=>newline.
    DATA:
      mo_cut TYPE REF TO zcl_llm_00_text_format.  " Class Under Test

    METHODS:
      setup,
      test_default_config FOR TESTING,
      test_custom_config FOR TESTING,
      test_long_word FOR TESTING,
      test_sentence_break FOR TESTING,
      test_mixed_scenarios FOR TESTING,
      test_is_eos FOR TESTING.

ENDCLASS.

CLASS ltcl_llm_00_text_format IMPLEMENTATION.

  METHOD setup.
    " Create an instance of the class under test with default configuration
    mo_cut = zcl_llm_00_text_format=>new( ).
  ENDMETHOD.

  METHOD test_default_config.
    mo_cut = zcl_llm_00_text_format=>new( 60 ).
    DATA(lv_input)    = 'This is a test string that should be wrapped at sixty characters as per the default configuration.'.
    DATA(lv_expected) = |This is a test string that should be wrapped at sixty| && lc_n &&
                        |characters as per the default configuration.|.

    DATA(lv_result) = mo_cut->format( lv_input ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = lv_expected
      msg = 'Default configuration test failed'
    ).
  ENDMETHOD.

  METHOD test_custom_config.
    DATA(lo_custom_cut) = zcl_llm_00_text_format=>new( 20 ).

    DATA(lv_input) =    'This is a test with custom configuration.'.
    DATA(lv_expected) = |This is a test with| && lc_n &&
                        |custom| && lc_n &&
                        |configuration.|.

    DATA(lv_result) = lo_custom_cut->format( lv_input ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = lv_expected
      msg = 'Custom configuration test failed'
    ).
  ENDMETHOD.

  METHOD test_long_word.
    DATA(lv_input)    = 'This string contains a very_long_identifier_that_exceeds_the_maximum_length and should be on its own line.'.
    DATA(lv_expected) = |This string contains a| && lc_n &&
                        |very_long_identifier_that_exceeds_the_maximum_length and| && lc_n &&
                        |should be on its own line.|.

    DATA(lv_result) = mo_cut->format( lv_input ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = lv_expected
      msg = 'Long word handling test failed'
    ).
  ENDMETHOD.

  METHOD test_sentence_break.
    DATA(lo_custom_cut) = zcl_llm_00_text_format=>new( 30 ).

    DATA(lv_input)    = 'This is the first sentence. This is the second sentence that should trigger a line break due to length.'.
    DATA(lv_expected) = |This is the first sentence.| && lc_n &&
                        |This is the second sentence| && lc_n &&
                        |that should trigger a line | && lc_n &&
                        |break due to length.|.

    DATA(lv_result) = lo_custom_cut->format( lv_input ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = lv_expected
      msg = 'Sentence break test failed'
    ).
  ENDMETHOD.

  METHOD test_mixed_scenarios.
    DATA(mo_cut) = zcl_llm_00_text_format=>new( 54 ).

    DATA(lv_input) = |Short sentence. A very_long_identifier_that_exceeds_the_maximum_length. Another short sentence. | &&
                     |This sentence is quite long and should be wrapped to the next line due to length restrictions.|.
    DATA(lv_expected) = |Short sentence. A| && lc_n &&
                        |very_long_identifier_that_exceeds_the_maximum_length.| && lc_n &&
                        |Another short sentence. This sentence is quite long|   && lc_n &&
                        |and should be wrapped to the next line due to length|  && lc_n &&
                        |restrictions.|.

    DATA(lv_result) = mo_cut->format( lv_input ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = lv_expected
      msg = 'Mixed scenarios test failed'
    ).
  ENDMETHOD.

  METHOD test_is_eos.
    cl_abap_unit_assert=>assert_true(
      act = mo_cut->is_eos( 'sentence.' )
      msg = 'End of sentence with period not detected'
    ).

    cl_abap_unit_assert=>assert_true(
      act = mo_cut->is_eos( 'question?' )
      msg = 'End of sentence with question mark not detected'
    ).

    cl_abap_unit_assert=>assert_true(
      act = mo_cut->is_eos( 'exclamation!' )
      msg = 'End of sentence with exclamation mark not detected'
    ).

    cl_abap_unit_assert=>assert_false(
      act = mo_cut->is_eos( 'not end of sentence' )
      msg = 'False positive for end of sentence'
    ).
  ENDMETHOD.

ENDCLASS.
