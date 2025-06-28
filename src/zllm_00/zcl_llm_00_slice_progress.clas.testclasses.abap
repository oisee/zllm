CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_llm_00_slice_progress DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.

  PRIVATE SECTION.
    TYPES: ts_ TYPE tadir,
           tt_ TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY.
    DATA: cut TYPE REF TO zcl_llm_00_slice_progress.  "class under test
    DATA: mt_ TYPE tt_.
    METHODS setup.
    METHODS: new             FOR TESTING.
    METHODS: new_for_table   FOR TESTING.
    METHODS: new_for_table_7 FOR TESTING.

ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.
  METHOD setup.
    DO 37 TIMES.
      APPEND INITIAL LINE TO mt_ REFERENCE INTO DATA(lr_).
      lr_->obj_name = sy-index.
    ENDDO.
    cut = zcl_llm_00_slice_progress=>new_for_table( it_      = mt_
                                                     iv_slice = 10 ).
  ENDMETHOD.

  METHOD new.
    data: lt_ like mt_.
    DO cut->times( ) TIMES.
      clear lt_.
      DATA(ls_slice) = cut->next( ).
      APPEND LINES OF mt_ FROM ls_slice-start TO ls_slice-end TO lt_.
    ENDDO.

    IF cut->is_last( ).
      clear lt_.
      ls_slice = cut->next( ).
      APPEND LINES OF mt_ FROM ls_slice-start TO ls_slice-end TO lt_.
      cut->last( ).
    ENDIF.
  ENDMETHOD.


  METHOD new_for_table.
    DATA: lt_tadir TYPE tt_.
    DO 7 TIMES.
      APPEND INITIAL LINE TO lt_tadir REFERENCE INTO DATA(lr_).
      lr_->obj_name = sy-index.
    ENDDO.
    DATA(lo_) = zcl_llm_00_slice_progress=>new_for_table( it_      = lt_tadir
                                                           iv_slice = 10 ).
    DATA:lt_ LIKE lt_tadir.
    DO lo_->times( ) TIMES.
      clear lt_.
      DATA(ls_slice) = lo_->next( ).
      APPEND LINES OF lt_tadir FROM ls_slice-start TO ls_slice-end TO lt_.
    ENDDO.

    IF lo_->is_last( ).
      clear lt_.
      ls_slice = lo_->next( ).
      APPEND LINES OF lt_tadir FROM ls_slice-start TO ls_slice-end TO lt_.
      lo_->last( ).
    ENDIF.

  ENDMETHOD.

  METHOD new_for_table_7.
    DATA: lt_tadir TYPE tt_.
    DO 7 TIMES.
      APPEND INITIAL LINE TO lt_tadir REFERENCE INTO DATA(lr_).
      lr_->obj_name = sy-index.
    ENDDO.
    DATA(lo_) = zcl_llm_00_slice_progress=>new_for_table( it_      = lt_tadir
                                                           iv_slice = 7 ).
    DATA:lt_ LIKE lt_tadir.
    DO lo_->times( ) TIMES.
      clear lt_.
      DATA(ls_slice) = lo_->next( ).
      APPEND LINES OF lt_tadir FROM ls_slice-start TO ls_slice-end TO lt_.
    ENDDO.

    IF lo_->is_last( ).
      clear lt_.
      ls_slice = lo_->next( ).
      APPEND LINES OF lt_tadir FROM ls_slice-start TO ls_slice-end TO lt_.
      lo_->last( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
