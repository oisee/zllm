
CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zif_llm_00_codec.  "class under test

    METHODS: setup.
    METHODS: teardown.
    METHODS: decode FOR TESTING.
    METHODS: encode FOR TESTING.
    METHODS: new FOR TESTING.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.

  METHOD setup.
    " Instantiate the class under test
    mo_cut = zcl_llm_00_codec=>new( '4242' ).

  ENDMETHOD.


  METHOD teardown.
  ENDMETHOD.


  METHOD decode.

    DATA iv_ TYPE xstring.
    DATA rv_ TYPE xstring.

    "rv_ = mo_cut->decode( '5022BEFBEF75D6EF345B' ).
    rv_ = mo_cut->decode( '1260FCB9AD3794AD7619' ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_
      exp   = '00112233445566778899'
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.


  METHOD encode.

    DATA lv_ TYPE xstring.
    DATA rv_ TYPE xstring.

    rv_ = mo_cut->encode( '00112233445566778899' ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_
"     exp   = '5022BEFBEF75D6EF345B'
      exp   = '1260FCB9AD3794AD7619'
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.


  METHOD new.

    DATA iv_ TYPE xstring.
    DATA ro_ TYPE REF TO zif_llm_00_codec.

    ro_ = zcl_llm_00_codec=>new( iv_ ).

    cl_abap_unit_assert=>assert_equals(
      act   = ro_
      exp   = ro_          "<--- please adapt expected value
    " msg   = 'Testing value ro_'
*     level =
    ).
  ENDMETHOD.




ENDCLASS.
