CLASS zcl_llm_00_slice_progress DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_slice,
        index TYPE i,
        start TYPE i,
        end   TYPE i,
      END OF ts_slice .

    CONSTANTS mc_max_step TYPE i VALUE 1000 ##NO_TEXT.
    CONSTANTS mc_default_table_chunks TYPE i VALUE 20 ##NO_TEXT.

    METHODS constructor
      IMPORTING
        !iv_total TYPE i
        !iv_text  TYPE string DEFAULT ''
        !iv_slice TYPE i DEFAULT 1000 .
    CLASS-METHODS new
      IMPORTING
        !iv_total  TYPE i
        !iv_text   TYPE string DEFAULT ''
        !iv_slice  TYPE i DEFAULT 1000
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_slice_progress .
    CLASS-METHODS new_for_table
      IMPORTING
        !it_       TYPE ANY TABLE
        !iv_text   TYPE string OPTIONAL
        !iv_slice  TYPE i DEFAULT 1000
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_slice_progress .
    METHODS times
      RETURNING
        VALUE(rv_) TYPE i .
    METHODS is_last
      RETURNING
        VALUE(rv_) TYPE sap_bool .
    METHODS next
      RETURNING
        VALUE(rs_) TYPE ts_slice .
    METHODS reset .
    METHODS last
      RETURNING
        VALUE(rv_) TYPE i .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_total TYPE i .
    DATA mv_index TYPE i .
    DATA mv_text TYPE string .

    DATA mv_times TYPE i.
    DATA mv_slice TYPE i.
    DATA mv_last_slice TYPE i.

    DATA:ms_      TYPE ts_slice.
ENDCLASS.



CLASS ZCL_LLM_00_SLICE_PROGRESS IMPLEMENTATION.


  METHOD constructor.

    mv_text  = iv_text.
    mv_total = iv_total.
    mv_slice = iv_slice.

    mv_times      = mv_total DIV mv_slice.
    mv_last_slice = mv_total MOD mv_slice.

  ENDMETHOD.


  METHOD is_last.
    rv_ = xsdbool( mv_last_slice > 0 ).
  ENDMETHOD.


  METHOD last.
    IF ms_-index >= mv_times.

      cl_progress_indicator=>progress_indicate(
        i_text      = |{ mv_text }:{ ms_-start }..{ mv_total }/{ mv_total }|
        i_total     = mv_total
        i_processed = mv_total
        i_output_immediately = abap_true ).

    ENDIF.
    rv_ = mv_index.
  ENDMETHOD.


  METHOD new.
    ro_ = NEW #(
      iv_total = iv_total
      iv_text  = iv_text
      iv_slice = iv_slice
    ).
  ENDMETHOD.


  METHOD new_for_table.

    DATA: lo_     TYPE REF TO cl_abap_typedescr,
          lv_head TYPE string,
          lv_tail TYPE string.

* Create a new instance of CL_ABAP_TABLEDESCR for the table you want to get the type name of
    lo_ = cl_abap_typedescr=>describe_by_data( it_ ).

* Get the type name of the table
    SPLIT lo_->absolute_name AT '=' INTO lv_head lv_tail. "\TYPE=MARA_TT"
    ro_ = NEW #(
      iv_total = lines( it_ )
      iv_text  = COND string( WHEN iv_text IS INITIAL THEN lv_tail
                              ELSE iv_text )
      iv_slice = iv_slice
    ).
  ENDMETHOD.


  METHOD next.
    ADD 1 TO mv_index.

    IF mv_index <= mv_times.
      rs_-start = ( mv_index - 1 ) * mv_slice + 1.
      rs_-end   = mv_index * mv_slice.
      rs_-index = mv_index.

    ELSE.
      rs_-start = 1 + mv_total DIV mv_slice * mv_slice.
      rs_-end   = mv_total.
      rs_-index = mv_index.
    ENDIF.

    ms_ = rs_.

    cl_progress_indicator=>progress_indicate(
      i_text      = |{ mv_text }:{ rs_-start }..{ rs_-end }/{ mv_total }|
      i_total     = mv_total
      i_processed = rs_-start
      i_output_immediately = abap_true ).

  ENDMETHOD.


  METHOD reset.
  ENDMETHOD.


  METHOD times.
    rv_ = mv_times.
  ENDMETHOD.
ENDCLASS.
