CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_llm_00_slice DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    TYPES: tt_ TYPE STANDARD TABLE OF tadir.
    DATA: cut TYPE REF TO zcl_llm_00_slice.  "class under test
    DATA: mt_ TYPE tt_.
    METHODS setup.
    METHODS: new             FOR TESTING.
    METHODS: new_for_table   FOR TESTING.
    METHODS: new_for_table_7 FOR TESTING.
    METHODS: type_mismatch   FOR TESTING.
    METHODS: new_for_single  FOR TESTING.
    METHODS: new_for_two     FOR TESTING.

ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.
  METHOD setup.
    DO 37 TIMES.
      APPEND INITIAL LINE TO mt_ REFERENCE INTO DATA(lr_).
      lr_->pgmid = sy-index.
    ENDDO.
    cut = zcl_llm_00_slice=>new( it_      = mt_
                                  iv_slice = 10 ).
  ENDMETHOD.

  METHOD new.
    DATA: lt_ LIKE mt_.
    WHILE cut->next( CHANGING ct_ = lt_ ).

    ENDWHILE.
  ENDMETHOD.


  METHOD new_for_table.
    DATA: lt_ LIKE mt_.
    WHILE cut->next( CHANGING ct_ = lt_ ).

    ENDWHILE.
  ENDMETHOD.

  METHOD new_for_table_7.
    DATA: lt_tadir TYPE tt_.
    DO 7 TIMES.
      APPEND INITIAL LINE TO lt_tadir REFERENCE INTO DATA(lr_).
      lr_->pgmid = sy-index.
    ENDDO.
    DATA(lo_) = zcl_llm_00_slice=>new( it_      = lt_tadir
                                        iv_slice = 7 ).
    DATA: lt_ LIKE mt_.
    WHILE lo_->next( CHANGING ct_ = lt_ ).

    ENDWHILE.
  ENDMETHOD.

  METHOD new_for_single.
    DATA: lt_tadir TYPE tt_.
    DO 1 TIMES.
      APPEND INITIAL LINE TO lt_tadir REFERENCE INTO DATA(lr_).
      lr_->pgmid = sy-index.
    ENDDO.

    DATA(lo_) = zcl_llm_00_slice=>new( it_      = lt_tadir
                                        iv_slice = 1000 ).
    DATA: lt_ LIKE mt_.
    WHILE lo_->next( CHANGING ct_ = lt_ ).

    ENDWHILE.
  ENDMETHOD.
  METHOD new_for_two.
    DATA: lt_tadir TYPE tt_.
    DO 2 TIMES.
      APPEND INITIAL LINE TO lt_tadir REFERENCE INTO DATA(lr_).
      lr_->pgmid = sy-index.
    ENDDO.

    DATA(lo_) = zcl_llm_00_slice=>new( it_      = lt_tadir
                                        iv_slice = 1000 ).
    DATA: lt_ LIKE mt_.
    WHILE lo_->next( CHANGING ct_ = lt_ ).

    ENDWHILE.
  ENDMETHOD.

  METHOD type_mismatch.
*    DATA: lt_ TYPE STANDARD TABLE OF makt.
*    TRY.
*        WHILE cut->next( CHANGING ct_ = lt_ ).
*
*        ENDWHILE.
*      CATCH zcx_s INTO DATA(lx_s).
*        zcl_col_000_cpu=>no_issue( lx_s ).
*    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_2 DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zcl_llm_00_slice.
    DATA: mt_ TYPE STANDARD TABLE OF tadir.
    METHODS setup.
    METHODS: test_new           FOR TESTING,
             test_next          FOR TESTING,
             test_next_empty    FOR TESTING,
             test_next_single   FOR TESTING,
             test_next_multiple FOR TESTING.

ENDCLASS.

CLASS lcl_2 IMPLEMENTATION.

  METHOD setup.
    DO 37 TIMES.
      APPEND INITIAL LINE TO mt_ REFERENCE INTO DATA(lr_).
      lr_->pgmid = sy-index.
    ENDDO.
    mo_cut = zcl_llm_00_slice=>new( it_ = mt_ iv_slice = 10 ).
  ENDMETHOD.

  METHOD test_new.
    " Test the instantiation of the class using the factory method
    DATA(lo_instance) = zcl_llm_00_slice=>new( it_ = mt_ iv_slice = 10 ).
    cl_abap_unit_assert=>assert_not_initial( act = lo_instance ).
  ENDMETHOD.

  METHOD test_next.
    " Test the next method for general functionality
    DATA: lt_ LIKE mt_.
    WHILE mo_cut->next( CHANGING ct_ = lt_ ).
    ENDWHILE.
    "cl_abap_unit_assert=>assert_equals( act = lines( lt_ ) exp = 7 ).
  ENDMETHOD.

  METHOD test_next_empty.
    " Test the next method with an empty input table
    DATA: lt_empty TYPE STANDARD TABLE OF tadir.
    mo_cut = zcl_llm_00_slice=>new( it_ = lt_empty iv_slice = 10 ).
    DATA: lt_ LIKE mt_.
    WHILE mo_cut->next( CHANGING ct_ = lt_ ).
    ENDWHILE.
    cl_abap_unit_assert=>assert_equals( act = lines( lt_ ) exp = 0 ).
  ENDMETHOD.

  METHOD test_next_single.
    " Test the next method with a single entry in the input table
    DATA: lt_single TYPE STANDARD TABLE OF tadir.
    APPEND INITIAL LINE TO lt_single REFERENCE INTO DATA(lr_).
    lr_->pgmid = 1.
    mo_cut = zcl_llm_00_slice=>new( it_ = lt_single iv_slice = 10 ).
    DATA: lt_ LIKE mt_.
    WHILE mo_cut->next( CHANGING ct_ = lt_ ).
    ENDWHILE.
    "cl_abap_unit_assert=>assert_equals( act = lines( lt_ ) exp = 1 ).
  ENDMETHOD.

  METHOD test_next_multiple.
    " Test the next method with multiple entries in the input table
    DATA: lt_multiple TYPE STANDARD TABLE OF tadir.
    DO 20 TIMES.
      APPEND INITIAL LINE TO lt_multiple REFERENCE INTO DATA(lr_).
      lr_->pgmid = sy-index.
    ENDDO.
    mo_cut = zcl_llm_00_slice=>new( it_ = lt_multiple iv_slice = 5 ).
    DATA: lt_ LIKE mt_.
    WHILE mo_cut->next( CHANGING ct_ = lt_ ).
    ENDWHILE.
    "cl_abap_unit_assert=>assert_equals( act = lines( lt_ ) exp = 5 ).
  ENDMETHOD.

ENDCLASS.
