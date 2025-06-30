CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_llm_00_list DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    TYPES: ttr_       TYPE zcl_llm_00_list=>ttr_.
    TYPES: ttr_tvarvc TYPE zcl_llm_00_list=>ttr_tvarvc.
  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_llm_00_list.  "class under test
    METHODS: setup.
    METHODS: new FOR TESTING.
    METHODS: new_from_string FOR TESTING.
    METHODS: r FOR TESTING.
    METHODS: r_t FOR TESTING.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.
  METHOD setup.
    mo_cut = zcl_llm_00_list=>new_from_string( 'keke;!keke*.md;olol*.env' ).
  ENDMETHOD.

  METHOD new.
    DATA(lo_) = zcl_llm_00_list=>new( VALUE #(
      ( `!keke*0.txt` )
      ( `keke*.txt` )
      ( `keke+.txt` )
      ( `keke.txt` )
    ) ).

    DATA(ltr_) = lo_->r( ).
    cl_abap_unit_assert=>assert_equals(
      act   = ltr_
      exp   = VALUE ttr_(
        ( sign = 'E' option = 'CP' low = 'keke*0.txt' )
        ( sign = 'I' option = 'CP' low = 'keke*.txt' )
        ( sign = 'I' option = 'CP' low = 'keke+.txt' )
        ( sign = 'I' option = 'EQ' low = 'keke.txt' )
      )
    ).
  ENDMETHOD.


  METHOD new_from_string.

    DATA(lo_) = zcl_llm_00_list=>new_from_string( `!keke*0.txt;keke*.txt;keke+.txt;keke.txt` ).

    DATA(ltr_) = lo_->r( ).
    cl_abap_unit_assert=>assert_equals(
      act   = ltr_
      exp   = VALUE ttr_(
        ( sign = 'E' option = 'CP' low = 'keke*0.txt' )
        ( sign = 'I' option = 'CP' low = 'keke*.txt' )
        ( sign = 'I' option = 'CP' low = 'keke+.txt' )
        ( sign = 'I' option = 'EQ' low = 'keke.txt' )
      )
    ).
  ENDMETHOD.


  METHOD r.
    "'keke;!keke*.md;olol*.env'

    DATA(ltr_) = mo_cut->r( ).
    cl_abap_unit_assert=>assert_equals(
      act   = ltr_
      exp   = VALUE ttr_(
        ( sign = 'I' option = 'EQ' low = 'keke' )
        ( sign = 'E' option = 'CP' low = 'keke*.md' )
        ( sign = 'I' option = 'CP' low = 'olol*.env' )
      )
    ).

  ENDMETHOD.


  METHOD r_t.
    "'keke;!keke*.md;olol*.env'

    DATA(ltr_) = mo_cut->r_t( ).
    cl_abap_unit_assert=>assert_equals(
      act   = ltr_
      exp   = VALUE ttr_tvarvc(
        ( sign = 'I' option = 'EQ' low = 'keke' )
        ( sign = 'E' option = 'CP' low = 'keke*.md' )
        ( sign = 'I' option = 'CP' low = 'olol*.env' )
      )
    ).

  ENDMETHOD.

ENDCLASS.
