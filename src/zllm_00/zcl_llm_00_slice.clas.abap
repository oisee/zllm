CLASS zcl_llm_00_slice DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_slice,
        index TYPE i,
        start TYPE i,
        end   TYPE i,
      END OF ts_slice .
    TYPES:
      tt_slice TYPE STANDARD TABLE OF ts_slice .

    CLASS-METHODS new
      IMPORTING
        !it_       TYPE INDEX TABLE
        !iv_slice  TYPE i DEFAULT 1000
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_slice .
    METHODS constructor
      IMPORTING
        !it_      TYPE INDEX TABLE
        !iv_slice TYPE i DEFAULT 1000 .
    METHODS next
      CHANGING
        !ct_       TYPE INDEX TABLE
      RETURNING
        VALUE(rv_) TYPE sap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mr_              TYPE REF TO data.
*   DATA mr_current_slice TYPE REF TO data.

    DATA mv_lines TYPE i .
    DATA mv_index TYPE i .

    DATA mv_times TYPE i.
    DATA mv_slice TYPE i.
    DATA mv_last_slice TYPE i.

    DATA:ms_      TYPE ts_slice.
    DATA:mt_      TYPE tt_slice.
ENDCLASS.



CLASS ZCL_LLM_00_SLICE IMPLEMENTATION.


  METHOD constructor.
    mr_ = REF #( it_ ).
    mv_slice = COND i( WHEN iv_slice <= 0 THEN 1000
                       ELSE iv_slice ).
    mv_lines      = lines( it_ ).
    mv_times      = mv_lines DIV mv_slice.
    mv_last_slice = mv_lines MOD mv_slice.
*--------------------------------------------------------------------*
    DATA(lv_index) = 0.

    DO mv_times TIMES.
      ADD 1 TO lv_index.
      APPEND VALUE #( index = lv_index
                      start = ( lv_index - 1 ) * mv_slice + 1
                      end   = lv_index * mv_slice  )
        TO mt_.
    ENDDO.
    IF mv_last_slice > 0.
      ADD 1 TO lv_index.
      APPEND VALUE #( index = lv_index
                      start = nmax( val1 = ( mv_lines - mv_last_slice )
                                    val2 = 1 )
                      end   = mv_lines )
        TO mt_.
    ENDIF.

  ENDMETHOD.


  METHOD new.
    ro_ = NEW #( it_      = it_
                 iv_slice = iv_slice ).
  ENDMETHOD.


  METHOD next.
    ADD 1 TO mv_index.
    CLEAR ct_.
*   mr_current_slice = REF #( ct_ ).

    IF mv_index > lines( mt_ ).
      RETURN.
    ENDIF.

    ms_ = mt_[ mv_index ].
    FIELD-SYMBOLS: <fs_> TYPE INDEX TABLE.
    ASSIGN mr_->* TO <fs_>.

    APPEND LINES OF <fs_> FROM ms_-start TO ms_-end TO ct_.

    rv_ = abap_true.
  ENDMETHOD.
ENDCLASS.
