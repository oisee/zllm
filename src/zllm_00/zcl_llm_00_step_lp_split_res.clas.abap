CLASS zcl_llm_00_step_lp_split_res DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES: string_t TYPE Zif_LLM_00_types=>string_t.
    INTERFACES zif_llm_00_string .
    INTERFACES zif_llm_00_step_result .

    CLASS-METHODS new
      IMPORTING
        !ir_       TYPE REF TO data OPTIONAL
        !io_       TYPE REF TO zif_llm_00_step_result OPTIONAL
        !io_llm    TYPE REF TO zif_llm_00_llm_lazy
      RETURNING
        VALUE(ro_) TYPE REF TO zif_llm_00_step_result .
    CLASS-METHODS new_from_string
      IMPORTING
        !iv_       TYPE string
        !io_llm    TYPE REF TO zif_llm_00_llm_lazy
      RETURNING
        VALUE(ro_) TYPE REF TO zif_llm_00_step_result .
    CLASS-METHODS new_from_table
      IMPORTING
        !it_       TYPE string_t
        !io_llm    TYPE REF TO zif_llm_00_llm_lazy
      RETURNING
        VALUE(ro_) TYPE REF TO zif_llm_00_step_result .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS constructor
      IMPORTING
        !iv_    TYPE string                        OPTIONAL
        !it_    TYPE string_t                      OPTIONAL
        !io_    TYPE REF TO zif_llm_00_step_result OPTIONAL
        !io_llm TYPE REF TO zif_llm_00_llm_lazy    .

    DATA: mo_llm       TYPE REF TO zif_llm_00_llm_lazy.
    DATA: mv_res       TYPE string.
    DATA: mt_res       TYPE string_t.
    DATA: mr_res       TYPE REF TO data.
    DATA: mo_in        TYPE REF TO zif_llm_00_step_result.
    DATA: mv_in        TYPE string.
    DATA: mt_in        TYPE string_t.
    DATA: mo_predictoken TYPE REF TO zcl_llm_00_predictoken.

    DATA: mv_max_token   TYPE i.
    DATA: mv_split_limit TYPE i.

    METHODS split_on
      IMPORTING iv_        TYPE string
                iv_on      TYPE string
                iv_lim     TYPE i
                iv_low     TYPE i
                iv_inc     TYPE sap_bool OPTIONAL
      EXPORTING et_        TYPE string_t
      RETURNING VALUE(rv_) TYPE sap_bool.

    METHODS split_at
      IMPORTING iv_        TYPE string
                iv_offset  TYPE i
      RETURNING VALUE(rt_) TYPE string_t.

    METHODS split_string_in_two_and_adjust
      IMPORTING iv_        TYPE string
      RETURNING VALUE(rt_) TYPE string_t.

    METHODS split_string
      IMPORTING iv_        TYPE string
      RETURNING VALUE(rt_) TYPE string_t.

    METHODS split_table
      IMPORTING it_        TYPE string_t
      RETURNING VALUE(rt_) TYPE string_t.

    METHODS split_step_result
      IMPORTING io_        TYPE REF TO zif_llm_00_step_result
      RETURNING VALUE(rt_) TYPE string_t.

    METHODS tokens
      IMPORTING iv_        TYPE string
      RETURNING VALUE(rv_) TYPE i.

ENDCLASS.



CLASS ZCL_LLM_00_STEP_LP_SPLIT_RES IMPLEMENTATION.


  METHOD constructor.
    mv_in  = iv_.
    mo_in  = io_.
    mt_in  = it_.
    mo_llm = io_llm.
    mv_max_token   = mo_llm->get_config( )-max_token.
    mv_split_limit = mo_llm->get_config( )-split_limit.
    mo_predictoken = zcl_llm_00_predictoken=>new_for_llm( mo_llm ).
  ENDMETHOD.


  METHOD new.
    IF io_ IS SUPPLIED AND io_ IS BOUND.
      ro_ ?= NEW zcl_llm_00_step_lp_split_res(
        io_    = io_
        io_llm = io_llm
      ).
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    FIELD-SYMBOLS: <fs_any_t> TYPE string_t,
                   <fs_any>   TYPE any.
*--------------------------------------------------------------------*
    ASSIGN ir_->* TO <fs_any>.
    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( <fs_any> ).
    DATA(lv_type_kind) = lo_type->type_kind.
    DATA(lv_kind)      = lo_type->kind.
    CASE lv_kind.
      WHEN lo_type->kind_elem.
        IF lv_type_kind = lo_type->typekind_char  OR
           lv_type_kind = lo_type->typekind_clike OR
           lv_type_kind = lo_type->typekind_csequence.
          ro_ ?= zcl_llm_00_step_lp_split_res=>new_from_string(
                   iv_    = CONV #( <fs_any> )
                   io_llm = io_llm
                 ).
          RETURN.
        ELSEIF lv_type_kind = lo_type->typekind_string.
          ro_ ?= zcl_llm_00_step_lp_split_res=>new_from_string(
                   iv_    = <fs_any>
                   io_llm = io_llm
                 ).
          RETURN.
        ENDIF.

      WHEN lo_type->kind_ref.
        ro_ ?= zcl_llm_00_step_lp_split_res=>new(
                 ir_    = <fs_any>
                 io_llm = io_llm
               ).
        RETURN.
*      WHEN mo_type->kind_struct.
*        mr_ = REF #( io_ ).
      WHEN lo_type->kind_table.
        ASSIGN ir_->* TO <fs_any_t>.
        ro_ ?= zcl_llm_00_step_lp_split_res=>new_from_table(
                 it_    = <fs_any_t>
                 io_llm = io_llm
               ).
      WHEN OTHERS.
        ro_ ?= zcl_llm_00_step_lp_split_res=>new_from_string(
                 iv_    = ' '
                 io_llm = io_llm
               ).
    ENDCASE.
  ENDMETHOD.


  METHOD new_from_string.
    ro_ = NEW zcl_llm_00_step_lp_split_res(
       iv_    = iv_
       io_llm = io_llm
    ).
  ENDMETHOD.


  METHOD new_from_table.
    ro_ = NEW zcl_llm_00_step_lp_split_res(
       it_    = it_
       io_llm = io_llm
    ).
  ENDMETHOD.


  METHOD split_at.
    DATA(lv_len) = strlen( iv_ ).
    DATA(lv_tail_len) = lv_len - iv_offset.
    DATA(lv_head) = iv_+0(iv_offset).
    APPEND lv_head TO rt_.
    IF lv_tail_len > 0.
      DATA(lv_tail) = iv_+iv_offset(lv_tail_len).
      APPEND lv_tail TO rt_.
    ENDIF.
  ENDMETHOD.


  METHOD split_on.
    DATA(lv_add) = COND i(
      WHEN iv_inc IS INITIAL THEN 0
      ELSE strlen( iv_on )
    ).

    FIND ALL OCCURRENCES OF iv_on IN iv_(iv_lim) RESULTS DATA(lt_match).
    IF lt_match IS INITIAL.
      IF et_ IS REQUESTED.
*        CLEAR et_.
      ENDIF.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    SORT lt_match BY offset DESCENDING.
    DATA(lv_offset) = VALUE #( lt_match[ 1 ]-offset OPTIONAL ).
    IF lv_offset > iv_low.
      rv_ = abap_true.
      IF et_ IS REQUESTED.
        et_ = split_at(
          iv_ = iv_
          iv_offset = ( lv_offset + lv_add )
        ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD split_step_result.
    DATA(lv_) = io_->to_string( ).
    rt_ = me->split_string( lv_ ).
  ENDMETHOD.


  METHOD split_string.
    rt_ = VALUE #( ( iv_ ) ).
    DATA(lv_tokens) = mo_predictoken->predict( iv_ ).
    IF lv_tokens <= mv_split_limit.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    rt_ = me->split_table( rt_ ).
  ENDMETHOD.


  METHOD split_string_in_two_and_adjust.
    rt_ = VALUE #( ( iv_ ) ).
    DATA(lv_tokens) = mo_predictoken->predict( iv_ ).
    IF lv_tokens <= mv_split_limit OR lv_tokens = 0 OR mv_split_limit = 0.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA(lv_len) = strlen( iv_ ).
    DATA: lv_ratio TYPE f.
    DATA: lv_lim   TYPE i.
    DATA: lv_low   TYPE i.
    lv_ratio = mv_split_limit / lv_tokens.
    lv_lim = lv_len * lv_ratio.
    lv_low = lv_len * lv_ratio * '0.55' .
    IF lv_low = 0.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    IF me->split_on( EXPORTING
      iv_on  = zif_llm=>n && `# `      iv_lim = lv_lim  iv_low = lv_low iv_ = iv_ IMPORTING  et_ = rt_ ) = abap_true.
      RETURN.
    ENDIF.
    IF me->split_on( EXPORTING
      iv_on  = zif_llm=>n && `## `     iv_lim = lv_lim  iv_low = lv_low iv_ = iv_ IMPORTING  et_ = rt_ ) = abap_true.
      RETURN.
    ENDIF.
    IF me->split_on( EXPORTING
      iv_on  = zif_llm=>n && `### `    iv_lim = lv_lim  iv_low = lv_low iv_ = iv_ IMPORTING  et_ = rt_ ) = abap_true.
      RETURN.
    ENDIF.
    IF me->split_on( EXPORTING
      iv_on  = zif_llm=>n && `#### `   iv_lim = lv_lim  iv_low = lv_low iv_ = iv_ IMPORTING  et_ = rt_ ) = abap_true.
      RETURN.
    ENDIF.
    IF me->split_on( EXPORTING
      iv_on  = CONV #( zif_llm=>n )    iv_lim = lv_lim  iv_low = lv_low iv_ = iv_ iv_inc = 'X' IMPORTING  et_ = rt_ ) = abap_true.
      RETURN.
    ENDIF.
    IF me->split_on( EXPORTING
      iv_on  = `.`                     iv_lim = lv_lim  iv_low = lv_low iv_ = iv_ iv_inc = 'X' IMPORTING  et_ = rt_ ) = abap_true.
      RETURN.
    ENDIF.
    IF me->split_on( EXPORTING
      iv_on  = ` `                     iv_lim = lv_lim  iv_low = lv_low iv_ = iv_ IMPORTING  et_ = rt_ ) = abap_true.
      RETURN.
    ENDIF.

    rt_ = split_at( iv_ = iv_ iv_offset = lv_lim ).

    RETURN.
*
*    "header
*    DATA(lv_h1) = zif_llm=>n && `# `.
*    DATA(lv_h2) = zif_llm=>n && `## `.
*    DATA(lv_h3) = zif_llm=>n && `### `.
*    DATA(lv_h4) = zif_llm=>n && `#### `.
*    "newline
*    DATA(lv_n) = zif_llm=>n.
*    "sentence
*    DATA(lv_s) = `.`.
*    "word
*    DATA(lv_w) = ` `.
*    FIND ALL OCCURRENCES OF lv_h1 IN iv_(lv_lim) RESULTS DATA(lt_match_1).
*    FIND ALL OCCURRENCES OF lv_h2 IN iv_(lv_lim) RESULTS DATA(lt_match_2).
*    FIND ALL OCCURRENCES OF lv_h3 IN iv_(lv_lim) RESULTS DATA(lt_match_3).
*    FIND ALL OCCURRENCES OF lv_h4 IN iv_(lv_lim) RESULTS DATA(lt_match_4).
*    FIND ALL OCCURRENCES OF lv_n  IN iv_(lv_lim) RESULTS DATA(lt_match_n).
*    FIND ALL OCCURRENCES OF lv_s  IN iv_(lv_lim) RESULTS DATA(lt_match_s).
*    FIND ALL OCCURRENCES OF lv_w  IN iv_(lv_lim) RESULTS DATA(lt_match_w).
*
*    SORT lt_match_1 BY offset DESCENDING.
*    SORT lt_match_2 BY offset DESCENDING.
*    SORT lt_match_3 BY offset DESCENDING.
*    SORT lt_match_4 BY offset DESCENDING.
*    SORT lt_match_n BY offset DESCENDING.
*    SORT lt_match_s BY offset DESCENDING.
*    SORT lt_match_w BY offset DESCENDING.
*
*    DATA(lv_offset_1) = VALUE #( lt_match_1[ 1 ]-offset OPTIONAL ).
*    DATA(lv_offset_2) = VALUE #( lt_match_2[ 1 ]-offset OPTIONAL ).
*    DATA(lv_offset_3) = VALUE #( lt_match_3[ 1 ]-offset OPTIONAL ).
*    DATA(lv_offset_4) = VALUE #( lt_match_4[ 1 ]-offset OPTIONAL ).
*    DATA(lv_offset_n) = VALUE #( lt_match_n[ 1 ]-offset OPTIONAL ).
*    DATA(lv_offset_s) = VALUE #( lt_match_s[ 1 ]-offset OPTIONAL ).
*    DATA(lv_offset_w) = VALUE #( lt_match_w[ 1 ]-offset OPTIONAL ).
*
*    IF lv_offset_1 > lv_low.
*      rt_ = split_at( iv_ = iv_ iv_offset = lv_offset_1 ).
*      RETURN.
*    ENDIF.
*    IF lv_offset_2 > lv_low.
*      rt_ = split_at( iv_ = iv_ iv_offset = lv_offset_2 ).
*      RETURN.
*    ENDIF.
*    IF lv_offset_3 > lv_low.
*      rt_ = split_at( iv_ = iv_ iv_offset = lv_offset_3 ).
*      RETURN.
*    ENDIF.
*    IF lv_offset_4 > lv_low.
*      rt_ = split_at( iv_ = iv_ iv_offset = lv_offset_4 ).
*      RETURN.
*    ENDIF.
*    IF lv_offset_n > lv_low.
*      rt_ = split_at( iv_ = iv_ iv_offset = lv_offset_n + 1 ).
*      RETURN.
*    ENDIF.
*    IF lv_offset_s > lv_low.
*      rt_ = split_at( iv_ = iv_ iv_offset = lv_offset_s ).
*      RETURN.
*    ENDIF.
*    IF lv_offset_w > lv_low.
*      rt_ = split_at( iv_ = iv_ iv_offset = lv_offset_w ).
*      RETURN.
*    ENDIF.
*
*    rt_ = split_at( iv_ = iv_ iv_offset = lv_lim ).

  ENDMETHOD.


  METHOD split_table.
    DATA: lt_ LIKE rt_.
    LOOP AT it_ REFERENCE INTO DATA(lr_).
      IF tokens( lr_->* ) <= mv_split_limit.
        APPEND lr_->* TO lt_.
        CONTINUE.
      ENDIF.
      APPEND LINES OF me->split_string_in_two_and_adjust( lr_->* ) TO lt_.
    ENDLOOP.
    IF lines( lt_ ) = 1.
      rt_ = lt_.
      RETURN.
    ELSE.
      DATA: lt_head LIKE rt_.
      DATA: lt_tail LIKE rt_.
      APPEND LINES OF lt_ FROM 1 TO 1 TO lt_head.
      APPEND LINES OF lt_ FROM 2 TO lines( lt_ ) TO lt_tail.
      lt_tail = me->split_table( lt_tail  ).
      APPEND LINES OF lt_head TO rt_.
      APPEND LINES OF lt_tail TO rt_.
    ENDIF.
  ENDMETHOD.


  METHOD tokens.
    rv_ = mo_predictoken->predict( iv_ ).
  ENDMETHOD.


  METHOD zif_llm_00_step_result~collect.
    IF mr_res IS BOUND.
      rr_ = mr_res.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    IF mv_in IS NOT INITIAL.
      mt_res = me->split_string( mv_in ).
    ELSEIF mt_in IS NOT INITIAL.
      mt_res = me->split_table( mt_in ).
    ELSEIF mo_in IS BOUND.
      mt_res = me->split_step_result( mo_in ).
    ENDIF.
    mr_res = REF #( mt_res ).
    rr_ = mr_res.

  ENDMETHOD.


  METHOD zif_llm_00_string~predict_tokens.
    DATA(lv_) = me->zif_llm_00_string~to_string( ).
    IF lv_ IS INITIAL.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA(lo_string) = zcl_llm_00_string=>new( lv_ ).
    rv_ = lo_string->predict_tokens( ).
  ENDMETHOD.


  METHOD zif_llm_00_string~to_string.
    IF mv_res IS NOT INITIAL.
      rv_ = mv_res.
      RETURN.
    ENDIF.
    TRY.
        IF mr_res IS NOT BOUND.
          mr_res = me->zif_llm_00_step_result~collect( ).
        ENDIF.
      CATCH zcx_s INTO DATA(lx_s).
        "zcl_cpu=>ok( lx_s ).
    ENDTRY.
    mv_res = zcl_llm_00_string=>new( mr_res )->to_string( ).
    rv_ = mv_res.
  ENDMETHOD.


  METHOD zif_llm_00_step_result~is_json.
    DATA(lr_) = me->zif_llm_00_step_result~collect( ).
    rv_ = zcl_llm_00_json=>is_structure( lr_ ).
  ENDMETHOD.
ENDCLASS.
