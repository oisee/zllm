CLASS zcl_llm_00_pat DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    TYPES string_t TYPE zif_llm=>string_t.

    INTERFACES zif_llm_00_pat .

    ALIASES apply
      FOR zif_llm_00_pat~apply .
    ALIASES get_name
      FOR zif_llm_00_pat~get_name .

    TYPES:
      BEGIN OF ts_component,
        name    TYPE string,
        pattern TYPE string,
        offset  TYPE i,
        line    TYPE i,
        len     TYPE i,
      END OF ts_component .
    TYPES:
      tt_component TYPE STANDARD TABLE OF ts_component WITH KEY name .
    TYPES:
      BEGIN OF ts_scope,
        pre  TYPE string,
        t    TYPE string,
        post TYPE string,
        line TYPE i,
      END OF ts_scope .
    TYPES:
      BEGIN OF ts_multi_scope,
        pre   TYPE string,
        t_f   TYPE string,
        t_m   TYPE string,
        t_l   TYPE string,
        post  TYPE string,
        start TYPE i,
        end   TYPE i,
      END OF ts_multi_scope .
    TYPES:
      BEGIN OF ts_line,
        line  TYPE i,
        start TYPE i,
        end   TYPE i,
        len   TYPE i,
      END OF ts_line .
    TYPES:
      tt_line TYPE STANDARD TABLE OF ts_line WITH KEY line .

    CLASS-DATA gv_space TYPE string VALUE `                                                                                                    ` ##NO_TEXT.

    CLASS-METHODS class_constructor .
    CLASS-METHODS new
      IMPORTING
        !iv_        TYPE string
        !iv_prefix  TYPE string DEFAULT '{'
        !iv_postfix TYPE string DEFAULT '}'
        !iv_root    TYPE string DEFAULT 'T'
        !iv_name    TYPE string DEFAULT 'DEFAULT'
          PREFERRED PARAMETER iv_
      RETURNING
        VALUE(ro_)  TYPE REF TO zif_llm_00_pat .
    CLASS-METHODS new_for_fcat
      IMPORTING
        !iv_prefix  TYPE string DEFAULT '{'
        !iv_postfix TYPE string DEFAULT '}'
        !iv_root    TYPE string DEFAULT 'T'
        !it_        TYPE lvc_t_fcat
        !iv_name    TYPE string DEFAULT 'DEFAULT'
      RETURNING
        VALUE(ro_)  TYPE REF TO zif_llm_00_pat .
    CLASS-METHODS new_from_file
      IMPORTING
        !io_        TYPE REF TO zif_llm_00_file
        !iv_prefix  TYPE string DEFAULT '{'
        !iv_postfix TYPE string DEFAULT '}'
        !iv_root    TYPE string DEFAULT 'T'
        !iv_name    TYPE string OPTIONAL
          PREFERRED PARAMETER io_
      RETURNING
        VALUE(ro_)  TYPE REF TO zif_llm_00_pat .
    CLASS-METHODS new_from_name
      IMPORTING
        !io_fl      TYPE REF TO zif_llm_00_file_list
        !iv_prefix  TYPE string DEFAULT '{'
        !iv_postfix TYPE string DEFAULT '}'
        !iv_root    TYPE string DEFAULT 'T'
        !iv_name    TYPE string OPTIONAL
          PREFERRED PARAMETER io_fl
      RETURNING
        VALUE(ro_)  TYPE REF TO zif_llm_00_pat .
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mv_ TYPE string .
    DATA mv_res TYPE string .
    DATA mv_root TYPE string .
    DATA mv_prefix TYPE string .
    DATA mv_postfix TYPE string .
    DATA mv_name TYPE string .

    CLASS-METHODS space
      IMPORTING
        !iv_num    TYPE i
      RETURNING
        VALUE(rv_) TYPE string .
    CLASS-METHODS pad
      IMPORTING
        !iv_num    TYPE i
        !iv_       TYPE string OPTIONAL
      RETURNING
        VALUE(rv_) TYPE string .
*   data MT_LINE type TT_LINE .
    METHODS apply_dref
      IMPORTING
        !ir_       TYPE REF TO data
        !iv_root   TYPE string OPTIONAL
        !iv_ctx    TYPE string OPTIONAL
          PREFERRED PARAMETER ir_
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS apply_scalar
      IMPORTING
        !iv_       TYPE any
        !iv_root   TYPE string OPTIONAL
        !iv_ctx    TYPE string OPTIONAL
          PREFERRED PARAMETER iv_
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS apply_struct
      IMPORTING
        !is_       TYPE any
        !iv_root   TYPE string OPTIONAL
        !iv_ctx    TYPE string OPTIONAL
          PREFERRED PARAMETER is_
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS apply_table
      IMPORTING
        !it_       TYPE table
        !iv_root   TYPE string OPTIONAL
        !iv_ctx    TYPE string OPTIONAL
          PREFERRED PARAMETER it_
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS apply_table_of_dref
      IMPORTING
        !it_       TYPE table
        !iv_root   TYPE string OPTIONAL
        !iv_ctx    TYPE string OPTIONAL
          PREFERRED PARAMETER it_
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS apply_table_of_dref_to_scalar
      IMPORTING
        !it_       TYPE table
        !iv_root   TYPE string OPTIONAL
        !iv_ctx    TYPE string OPTIONAL
          PREFERRED PARAMETER it_
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS apply_table_of_scalar
      IMPORTING
        !it_       TYPE table
        !iv_root   TYPE string OPTIONAL
        !iv_ctx    TYPE string OPTIONAL
          PREFERRED PARAMETER it_
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS apply_table_of_scalar_bak
      IMPORTING
        !it_       TYPE table
        !iv_root   TYPE string OPTIONAL
        !iv_ctx    TYPE string OPTIONAL
          PREFERRED PARAMETER it_
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS apply_table_of_dref_to_struct
      IMPORTING
        !it_       TYPE table
        !iv_root   TYPE string OPTIONAL
        !iv_ctx    TYPE string OPTIONAL
          PREFERRED PARAMETER it_
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS apply_table_of_struct
      IMPORTING
        !it_       TYPE table
        !iv_root   TYPE string OPTIONAL
        !iv_ctx    TYPE string OPTIONAL
          PREFERRED PARAMETER it_
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS constructor
      IMPORTING
        !iv_        TYPE string
        !iv_prefix  TYPE string DEFAULT '{'
        !iv_postfix TYPE string DEFAULT '}'
        !iv_root    TYPE string DEFAULT 'T'
        !iv_name    TYPE string DEFAULT 'DEFAULT'
          PREFERRED PARAMETER iv_ .
    METHODS get_fields
      IMPORTING
        !it_       TYPE abap_compdescr_tab
        !iv_root   TYPE string
        !iv_ctx    TYPE string
      RETURNING
        VALUE(rt_) TYPE tt_component .
    METHODS get_scope_single_line
      IMPORTING
        !iv_ctx        TYPE string
        !iv_start      TYPE i
        !iv_end        TYPE i OPTIONAL
        !iv_first_line TYPE i
        !iv_last_line  TYPE i
      RETURNING
        VALUE(rs_)     TYPE ts_multi_scope .
    METHODS get_scope_multiline
      IMPORTING
        !iv_ctx        TYPE string
        !iv_start      TYPE i
        !iv_end        TYPE i OPTIONAL
        !iv_first_line TYPE i
        !iv_last_line  TYPE i
      RETURNING
        VALUE(rs_)     TYPE ts_multi_scope .
    METHODS get_offsets
      IMPORTING
        !iv_        TYPE string
        !iv_pattern TYPE csequence DEFAULT zif_llm=>n
          PREFERRED PARAMETER iv_
      RETURNING
        VALUE(rt_)  TYPE tt_line .
    METHODS get_line_by_offset
      IMPORTING
        !iv_       TYPE i
        !iv_ctx    TYPE string
      RETURNING
        VALUE(rv_) TYPE i .
    METHODS render_table_of_dref_to_scalar
      IMPORTING
        !it_          TYPE ANY TABLE
        !is_          TYPE ts_multi_scope
        !iv_root      TYPE string
        !iv_type_kind TYPE abap_typekind
      RETURNING
        VALUE(rv_)    TYPE string .
    METHODS render_table_of_scalar
      IMPORTING
        !it_          TYPE ANY TABLE
        !is_          TYPE ts_multi_scope
        !iv_root      TYPE string
        !iv_type_kind TYPE abap_typekind
      RETURNING
        VALUE(rv_)    TYPE string .
    METHODS render_table_of_struct
      IMPORTING
        !it_       TYPE ANY TABLE
        !is_       TYPE ts_multi_scope
        !iv_root   TYPE string
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS get_scope
      IMPORTING
        !it_       TYPE abap_compdescr_tab
        !iv_root   TYPE string
        !iv_ctx    TYPE string
      RETURNING
        VALUE(rs_) TYPE ts_multi_scope .
    METHODS get_scope_s
      IMPORTING
        !iv_root   TYPE string
        !iv_ctx    TYPE string
      RETURNING
        VALUE(rs_) TYPE ts_multi_scope .
    METHODS tab_to_string
      IMPORTING
        !it_       TYPE string_t
        !is_       TYPE ts_multi_scope
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS tab_to_string_s
      IMPORTING
        !it_       TYPE string_t
        !is_       TYPE ts_multi_scope
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS render_table_of_dref_to_struct
      IMPORTING
        !it_       TYPE ANY TABLE
        !is_       TYPE ts_multi_scope
        !iv_root   TYPE string
      RETURNING
        VALUE(rv_) TYPE string .
ENDCLASS.



CLASS ZCL_LLM_00_PAT IMPLEMENTATION.


  METHOD apply_dref.
    IF iv_ctx IS NOT SUPPLIED.
      rv_ = mv_res.
    ELSE.
      rv_ = iv_ctx.
    ENDIF.
    FIELD-SYMBOLS: <fs_f> TYPE any.

    IF ir_ IS NOT BOUND.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ASSIGN ir_->* TO <fs_f>.
    IF <fs_f> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lo_td TYPE REF TO cl_abap_typedescr.
    lo_td ?= cl_abap_typedescr=>describe_by_data( <fs_f> ).
    DATA(lv_root) = iv_root.
    CASE lo_td->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        rv_ = me->apply_scalar(
                 iv_     = <fs_f>
                 iv_root = lv_root
                 iv_ctx  = rv_
        ).
        RETURN.
      WHEN cl_abap_typedescr=>kind_struct.
        rv_ = me->apply_struct(
                 is_     = <fs_f>
                 iv_root = lv_root
                 iv_ctx  = rv_
        ).
        RETURN.
      WHEN cl_abap_typedescr=>kind_table.
        rv_ = me->apply_table(
                 it_     = <fs_f>
                 iv_root = lv_root
                 iv_ctx  = rv_
        ).
        RETURN.
      WHEN cl_abap_typedescr=>kind_ref.
        rv_ = me->apply_dref(
                 ir_     = <fs_f>
                 iv_root = lv_root
                 iv_ctx  = rv_
        ).
        RETURN.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

  ENDMETHOD.


  METHOD apply_scalar.
    IF iv_ctx IS NOT SUPPLIED.
      rv_ = mv_res.
    ELSE.
      rv_ = iv_ctx.
    ENDIF.
    DATA(lv_prefix) = to_upper(
      COND #( WHEN iv_root IS NOT INITIAL THEN |{ mv_prefix }{ iv_root }|
                              ELSE |{ mv_prefix }|
      )
    ).
    REPLACE ALL OCCURRENCES OF |{ lv_prefix }{ mv_postfix }| IN rv_ WITH iv_.
  ENDMETHOD.


  METHOD apply_struct.
    IF iv_ctx IS NOT SUPPLIED.
      rv_ = mv_res.
    ELSE.
      rv_ = iv_ctx.
    ENDIF.
    FIELD-SYMBOLS: <fs_s> TYPE any. "structure
    FIELD-SYMBOLS: <fs_f> TYPE any. "field

    IF is_ IS INITIAL.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ASSIGN is_ TO <fs_s>.
    IF <fs_s> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lo_sd TYPE REF TO cl_abap_structdescr.
    lo_sd ?= cl_abap_structdescr=>describe_by_data( <fs_s> ).
    LOOP AT lo_sd->components REFERENCE INTO DATA(lr_field).
      ASSIGN COMPONENT lr_field->name OF STRUCTURE <fs_s> TO <fs_f>.

      DATA: lo_fd TYPE REF TO cl_abap_typedescr.
      lo_fd ?= cl_abap_typedescr=>describe_by_data( <fs_f> ).
      DATA(lv_root) = |{ iv_root }-{ lr_field->name }|.

      CASE lo_fd->kind.
        WHEN cl_abap_typedescr=>kind_elem.
          rv_ = me->apply_scalar(
                   iv_     = <fs_f>
                   iv_root = lv_root
                   iv_ctx  = rv_
          ).
          CONTINUE.
        WHEN cl_abap_typedescr=>kind_struct.
          rv_ = me->apply_struct(
                   is_     = <fs_f>
                   iv_root = lv_root
                   iv_ctx  = rv_
          ).
          CONTINUE.
        WHEN cl_abap_typedescr=>kind_table.
          rv_ = me->apply_table(
                   it_     = <fs_f>
                   iv_root = lv_root
                   iv_ctx  = rv_
          ).
          CONTINUE.
        WHEN cl_abap_typedescr=>kind_ref.
          rv_ = me->apply_dref(
                   ir_     = <fs_f>
                   iv_root = lv_root
                   iv_ctx  = rv_
          ).
          CONTINUE.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD apply_table.
    IF iv_ctx IS NOT SUPPLIED.
      rv_ = mv_res.
    ELSE.
      rv_ = iv_ctx.
    ENDIF.
    IF it_ IS INITIAL.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS: <fs_s> TYPE any.       "structure
*--------------------------------------------------------------------*
* @todo redo from the type of line, otherwise - no data -> fields of table will remain intact in the template
    READ TABLE it_ ASSIGNING <fs_s> INDEX 1.
    IF <fs_s> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lo_ld TYPE REF TO cl_abap_typedescr.
    lo_ld ?= cl_abap_typedescr=>describe_by_data( <fs_s> ).
    CASE lo_ld->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        rv_ = me->apply_table_of_scalar(
                 it_     = it_
                 iv_root = iv_root
                 iv_ctx  = rv_
        ).
        RETURN.
      WHEN cl_abap_typedescr=>kind_struct.
        rv_ = me->apply_table_of_struct(
                 it_     = it_
                 iv_root = iv_root
                 iv_ctx  = rv_
        ).
        RETURN.
      WHEN cl_abap_typedescr=>kind_ref.
        rv_ = me->apply_table_of_dref(
                 it_     = it_
                 iv_root = iv_root
                 iv_ctx  = rv_
        ).
        RETURN.
      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.


  METHOD apply_table_of_dref.

    IF iv_ctx IS NOT SUPPLIED.
      rv_ = mv_res.
    ELSE.
      rv_ = iv_ctx.
    ENDIF.
    IF it_ IS INITIAL.
      RETURN.
    ENDIF.
    FIELD-SYMBOLS: <fs_r> TYPE any.       "ref to line
    FIELD-SYMBOLS: <fs_l> TYPE any.       "line
*--------------------------------------------------------------------*
    READ TABLE it_ ASSIGNING <fs_r> INDEX 1.
    IF <fs_r> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ASSIGN <fs_r>->* TO <fs_l>.
    IF <fs_l> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lo_ld TYPE REF TO cl_abap_typedescr.
    lo_ld ?= cl_abap_typedescr=>describe_by_data( <fs_l> ).

    CASE lo_ld->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        rv_ = me->apply_table_of_dref_to_scalar(
                 it_     = it_
                 iv_root = iv_root
                 iv_ctx  = rv_
        ).
        RETURN.
      WHEN cl_abap_typedescr=>kind_struct.
        rv_ = me->apply_table_of_dref_to_struct(
                 it_     = it_
                 iv_root = iv_root
                 iv_ctx  = rv_
        ).
        RETURN.
*      WHEN cl_abap_typedescr=>kind_ref.
*        rv_ = me->apply_table_of_dref(
*                 it_     = it_
*                 iv_root = iv_root
*                 iv_ctx  = rv_
*        ).
*        RETURN.
      WHEN OTHERS.
        RETURN.
    ENDCASE.
**--------------------------------------------------------------------*
*    DATA(ls_scope) = me->get_scope(
*      it_     = lo_sd->components
*      iv_root = iv_root
*      iv_ctx  = rv_
*    ).
**--------------------------------------------------------------------*
*    rv_ = me->render_table_multiline(
*      it_     = it_
*      is_     = ls_scope
*      iv_root = iv_root
*    ).
*--------------------------------------------------------------------*
*    DATA: lo_ld TYPE REF TO cl_abap_typedescr.
*    lo_ld ?= cl_abap_typedescr=>describe_by_data( <fs_s> ).
*    CASE lo_ld->kind.
*      WHEN cl_abap_typedescr=>kind_elem.
*        rv_ = me->apply_table_of_scalar(
*                 it_     = it_
*                 iv_root = iv_root
*                 iv_ctx  = rv_
*        ).
*        RETURN.
*      WHEN cl_abap_typedescr=>kind_struct.
*        rv_ = me->apply_table_of_struct(
*                 it_     = it_
*                 iv_root = iv_root
*                 iv_ctx  = rv_
*        ).
*        RETURN.
*      WHEN cl_abap_typedescr=>kind_ref.
*        rv_ = me->apply_table_of_dref(
*                 it_     = it_
*                 iv_root = iv_root
*                 iv_ctx  = rv_
*        ).
*        RETURN.
*      WHEN OTHERS.
*        RETURN.

  ENDMETHOD.


  METHOD apply_table_of_dref_to_scalar.
    IF iv_ctx IS NOT SUPPLIED.
      rv_ = mv_res.
    ELSE.
      rv_ = iv_ctx.
    ENDIF.
    IF it_ IS INITIAL.
      RETURN.
    ENDIF.
    FIELD-SYMBOLS: <fs_r> TYPE any.       "ref to line
    FIELD-SYMBOLS: <fs_l> TYPE any.       "line
*--------------------------------------------------------------------*
    READ TABLE it_ ASSIGNING <fs_r> INDEX 1.
    IF <fs_r> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ASSIGN <fs_r>->* TO <fs_l>.
    IF <fs_l> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lo_ld TYPE REF TO cl_abap_typedescr.
    lo_ld ?= cl_abap_typedescr=>describe_by_data( <fs_l> ).
*--------------------------------------------------------------------*
    DATA(ls_scope) = me->get_scope_s(
      iv_root = iv_root
      iv_ctx  = rv_
    ).
    IF ls_scope IS INITIAL.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    rv_ = me->render_table_of_dref_to_scalar(
      it_          = it_
      is_          = ls_scope
      iv_type_kind = lo_ld->type_kind
      iv_root = iv_root
    ).
  ENDMETHOD.


  METHOD apply_table_of_dref_to_struct.
    IF iv_ctx IS NOT SUPPLIED.
      rv_ = mv_res.
    ELSE.
      rv_ = iv_ctx.
    ENDIF.
    IF it_ IS INITIAL.
      RETURN.
    ENDIF.
    FIELD-SYMBOLS: <fs_r> TYPE any.       "ref to structure
    FIELD-SYMBOLS: <fs_s> TYPE any.       "structure
*--------------------------------------------------------------------*
    READ TABLE it_ ASSIGNING <fs_r> INDEX 1.
    IF <fs_r> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ASSIGN <fs_r>->* TO <fs_s>.
    IF <fs_s> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lo_sd TYPE REF TO cl_abap_structdescr.
    lo_sd ?= cl_abap_structdescr=>describe_by_data( <fs_s> ).
*--------------------------------------------------------------------*
    DATA(ls_scope) = me->get_scope(
      it_     = lo_sd->components
      iv_root = iv_root
      iv_ctx  = rv_
    ).
    IF ls_scope IS INITIAL.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    rv_ = me->render_table_of_dref_to_struct(
      it_     = it_
      is_     = ls_scope
      iv_root = iv_root
    ).
  ENDMETHOD.


  METHOD apply_table_of_scalar.
    IF iv_ctx IS NOT SUPPLIED.
      rv_ = mv_res.
    ELSE.
      rv_ = iv_ctx.
    ENDIF.
    IF it_ IS INITIAL.
      RETURN.
    ENDIF.
    FIELD-SYMBOLS: <fs_l> TYPE any.       "line
*--------------------------------------------------------------------*
    READ TABLE it_ ASSIGNING <fs_l> INDEX 1.
    IF <fs_l> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
    DATA: lo_ld TYPE REF TO cl_abap_typedescr.
    lo_ld ?= cl_abap_typedescr=>describe_by_data( <fs_l> ).
*--------------------------------------------------------------------*
    DATA(ls_scope) = me->get_scope_s(
      iv_root = iv_root
      iv_ctx  = rv_
    ).
    IF ls_scope IS INITIAL.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    rv_ = me->render_table_of_scalar(
      it_          = it_
      is_          = ls_scope
      iv_type_kind = lo_ld->type_kind
      iv_root = iv_root
    ).
  ENDMETHOD.


  METHOD apply_table_of_scalar_bak.
*    IF iv_ctx IS NOT SUPPLIED.
*      rv_ = mv_res.
*    ELSE.
*      rv_ = iv_ctx.
*    ENDIF.
*    IF it_ IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    FIELD-SYMBOLS: <fs_f> TYPE any.       "field
**--------------------------------------------------------------------*
*    READ TABLE it_ ASSIGNING <fs_f> INDEX 1.
*    IF <fs_f> IS NOT ASSIGNED.
*      RETURN.
*    ENDIF.
**--------------------------------------------------------------------*
*    DATA: lo_ld TYPE REF TO cl_abap_typedescr.
*    lo_ld ?= cl_abap_typedescr=>describe_by_data( <fs_f> ).
*
*    DATA(lv_pattern) = |{ mv_prefix }{ iv_root }{ mv_postfix }|.
*    FIND FIRST OCCURRENCE OF lv_pattern IN rv_ IGNORING CASE MATCH OFFSET DATA(lv_offset).
*    DATA(lv_line) = get_line_by_offset(
*      iv_ctx = rv_
*      iv_ = lv_offset
*    ).
*    DATA(ls_scope) = me->get_scope_scalar(
*      iv_ctx  = rv_
*      iv_line = lv_line
*    ).
*    DATA: lt_table TYPE string_t.
**--------------------------------------------------------------------*
*    IF lo_ld->type_kind = lo_ld->typekind_char      OR
*       lo_ld->type_kind = lo_ld->typekind_clike     OR
*       lo_ld->type_kind = lo_ld->typekind_csequence OR
*       lo_ld->type_kind = lo_ld->typekind_string.
*      LOOP AT it_ ASSIGNING <fs_f>.
*        APPEND me->apply_scalar(
*                   iv_     = <fs_f>
*                   iv_root = iv_root
*                   iv_ctx  = ls_scope-t
*        ) TO lt_table.
*      ENDLOOP.
*    ELSE.
*      LOOP AT it_ ASSIGNING <fs_f>.
*        FIELD-SYMBOLS: <fs_sub> TYPE any. "substitution field
*        DATA lv_sub TYPE text255.
*        WRITE <fs_f> TO lv_sub.
*        CONDENSE lv_sub.
*        ASSIGN lv_sub TO <fs_sub>.
*        APPEND me->apply_scalar(
*                   iv_     = <fs_sub>
*                   iv_root = iv_root
*                   iv_ctx  = ls_scope-t
*        ) TO lt_table.
*      ENDLOOP.
*    ENDIF.
**--------------------------------------------------------------------*
*    IF lt_table IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    rv_ = me->tab_to_string(
*      it_ = lt_table
*      is_ = VALUE #(
*        pre   = ls_scope-pre
*        post  = ls_scope-post
*        start = ls_scope-line
*      )
*    ).
  ENDMETHOD.


  METHOD apply_table_of_struct.
    IF iv_ctx IS NOT SUPPLIED.
      rv_ = mv_res.
    ELSE.
      rv_ = iv_ctx.
    ENDIF.
    IF it_ IS INITIAL.
      RETURN.
    ENDIF.
    FIELD-SYMBOLS: <fs_s> TYPE any.       "structure
    FIELD-SYMBOLS: <fs_f> TYPE any.       "field
*--------------------------------------------------------------------*
    READ TABLE it_ ASSIGNING <fs_s> INDEX 1.
    IF <fs_s> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lo_sd TYPE REF TO cl_abap_structdescr.
    lo_sd ?= cl_abap_structdescr=>describe_by_data( <fs_s> ).
*--------------------------------------------------------------------*
    DATA(ls_scope) = me->get_scope(
      it_     = lo_sd->components
      iv_root = iv_root
      iv_ctx  = rv_
    ).
    IF ls_scope IS INITIAL.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    rv_ = me->render_table_of_struct(
      it_     = it_
      is_     = ls_scope
      iv_root = iv_root
    ).
  ENDMETHOD.


  METHOD class_constructor.
    DO 4 TIMES.
      gv_space = gv_space && gv_space. "1600 spaces
    ENDDO.
  ENDMETHOD.


  METHOD constructor.
    mv_        = iv_.
    mv_res     = mv_.
    mv_prefix  = iv_prefix.
    mv_postfix = iv_postfix.
    mv_root    = iv_root.
    mv_name    = iv_name.
  ENDMETHOD.


  METHOD get_fields.
    LOOP AT it_ REFERENCE INTO DATA(lr_field).
      DATA(lv_pattern) = |{ mv_prefix }{ iv_root }-{ lr_field->name }{ mv_postfix }|.
      FIND ALL OCCURRENCES OF lv_pattern IN iv_ctx RESULTS DATA(lt_match).
      IF lt_match IS INITIAL.
        CONTINUE.
      ENDIF.
      "APPEND LINES OF lt_match_comp TO lt_match.
      LOOP AT lt_match REFERENCE INTO DATA(lr_match).
        APPEND VALUE ts_component(
          name    = lr_field->name
          pattern = lv_pattern
          offset  = lr_match->offset
          line    = get_line_by_offset(
            iv_ctx = iv_ctx
            iv_ = lr_match->offset
          )
          len     = lr_match->length
        ) TO rt_.
      ENDLOOP.
    ENDLOOP.

    SORT rt_ BY offset.
  ENDMETHOD.


  METHOD get_line_by_offset.
    DATA(lt_line) = get_offsets( iv_ctx ).
    LOOP AT lt_line REFERENCE INTO DATA(lr_line) WHERE start <= iv_ AND end >= iv_.
      rv_ = lr_line->line.
      RETURN.
    ENDLOOP.

*    IF mt_line IS INITIAL.
*      mt_line = get_offsets( iv_ctx ).
*    ENDIF.
*
*    LOOP AT mt_line REFERENCE INTO DATA(lr_line) WHERE start <= iv_ AND end >= iv_.
*      rv_ = lr_line->line.
*      RETURN.
*    ENDLOOP.
  ENDMETHOD.


  METHOD get_offsets.
    IF iv_ IS INITIAL.
      RETURN.
    ENDIF.
    FIND ALL OCCURRENCES OF iv_pattern IN iv_ RESULTS DATA(lt_match).
    IF lt_match IS INITIAL.
      "we have at least one line!
      APPEND VALUE #(
        line  = 1
        start = 0
        len   = strlen( iv_ )
        end   = strlen( iv_ )
      ) TO rt_.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA(lv_count)  = 1.
    DATA(lv_offset) = 0.
    LOOP AT lt_match REFERENCE INTO DATA(lr_match).
      APPEND VALUE ts_line(
        line  = lv_count
        start = lv_offset
        end   = lr_match->offset
        len   = lr_match->offset - lv_offset
      ) TO rt_.
      ADD 1 TO lv_count.
      lv_offset = lr_match->offset.
    ENDLOOP.
    IF lr_match->offset < strlen( iv_ ).
      APPEND VALUE #(
        line   = lv_count
        start  = lv_offset
        end    = strlen( iv_ )
        len    = strlen( iv_ ) - lv_offset
      ) TO rt_.
    ENDIF.
  ENDMETHOD.


  METHOD get_scope.
    DATA: lt_field TYPE tt_component.
    LOOP AT it_ REFERENCE INTO DATA(lr_comp).
      DATA(lv_pattern) = |{ mv_prefix }{ iv_root }-{ lr_comp->name }{ mv_postfix }|.
      FIND ALL OCCURRENCES OF lv_pattern IN iv_ctx RESULTS DATA(lt_match).
      IF lt_match IS INITIAL.
        CONTINUE.
      ENDIF.
      "APPEND LINES OF lt_match_comp TO lt_match.
      LOOP AT lt_match REFERENCE INTO DATA(lr_match).
        APPEND VALUE ts_component(
          name    = lr_comp->name
          pattern = lv_pattern
          offset  = lr_match->offset
          line    = get_line_by_offset(
            iv_ctx = iv_ctx
            iv_ = lr_match->offset
          )
          len     = lr_match->length
        ) TO lt_field.
      ENDLOOP.
    ENDLOOP.
    SORT lt_field BY offset.
    IF lt_field IS INITIAL.
      "default scope
      rs_-pre = iv_ctx.
      RETURN.
    ENDIF.

    DATA(ls_first) = lt_field[ 1 ].
    DATA(ls_last) =  lt_field[ lines( lt_field ) ].
    rs_ = me->get_scope_multiline(
      iv_ctx        = iv_ctx
      iv_start      = ls_first-offset
      iv_first_line = ls_first-line
      iv_end        = ls_last-offset + ls_last-len
      iv_last_line  = ls_last-line
    ).
  ENDMETHOD.


  METHOD get_scope_multiline.
    IF iv_ctx IS INITIAL.
      RETURN.
    ENDIF.
    IF iv_first_line IS INITIAL OR iv_last_line IS INITIAL.
      RETURN.
    ENDIF.
    IF iv_end IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_line) = get_offsets( iv_ctx ).

    IF iv_first_line > lines( lt_line ).
      RETURN.
    ENDIF.
    IF iv_first_line > iv_last_line.
      RETURN.
    ENDIF.

    IF iv_first_line = iv_last_line.
      rs_ = me->get_scope_single_line(
        iv_ctx        = iv_ctx
        iv_start      = iv_start
        iv_end        = iv_end
        iv_first_line = iv_first_line
        iv_last_line  = iv_last_line
      ).
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA(lv_clone_len) = iv_end - iv_start.
    rs_-start = iv_start.
    rs_-end   = iv_end.

    DATA(lv_ctx_len)  = strlen( iv_ctx ).
    DATA(lv_pre_len)  = iv_start.
    DATA(lv_post_len) = lv_ctx_len  - iv_end.
    rs_-pre  = iv_ctx+0(iv_start).
    rs_-post = iv_ctx+iv_end(lv_post_len).
*--------------------------------------------------------------------*
    DATA(ls_first)  = VALUE #( lt_line[ iv_first_line ] ).
    DATA(ls_last)   = VALUE #( lt_line[ iv_last_line  ] ).

    "try to enhance scope to the first pipe in and last pipe in the
    IF iv_first_line = 1.
      DATA(lv_pad_left_n)  = iv_start - ls_first-start . "-1
    ELSE.
      lv_pad_left_n        = iv_start - ls_first-start - 1.
    ENDIF.
    DATA(lv_pad_right_n)   = ls_last-end - iv_end.

    DATA(lv_first_pre_len) = iv_start - ls_first-start.
    DATA(lv_first_pre)     = iv_ctx+ls_first-start(lv_first_pre_len).
    DATA(lv_last_post_len) = ls_last-end - iv_end.
    DATA(lv_last_post)     = iv_ctx+iv_end(lv_last_post_len).

    FIND FIRST OCCURRENCE OF '|' IN lv_first_pre MATCH OFFSET DATA(lv_first_pipe).
    IF sy-subrc = 0.
      DATA(lv_first_pre_corr) = lv_first_pre_len - lv_first_pipe.
      FIND ALL OCCURRENCES OF '|' IN lv_last_post RESULTS DATA(lt_match).
      IF sy-subrc = 0 AND lt_match IS NOT INITIAL.
        DATA(ls_last_pipe)     = lt_match[ lines( lt_match ) ].
        DATA(lv_last_post_corr) = ls_last_pipe-offset + 1.

        DATA(lv_start) = iv_start      - lv_first_pre_corr.
        lv_clone_len   = lv_clone_len  + lv_first_pre_corr.
        lv_pad_left_n  = lv_pad_left_n - lv_first_pre_corr.
        rs_-pre  = iv_ctx+0(lv_start).

        DATA(lv_end)   = iv_end         + lv_last_post_corr.
        lv_clone_len   = lv_clone_len   + lv_last_post_corr.
        lv_pad_right_n = lv_pad_right_n - lv_last_post_corr.
        lv_post_len    = lv_post_len    - lv_last_post_corr.
        rs_-post = iv_ctx+lv_end(lv_post_len).
      ENDIF.
    ELSE.
      lv_start = iv_start.
    ENDIF.
*--------------------------------------------------------------------*
*    DATA(lv_post_len) = lv_ctx_len  - ls_last-end.
*    DATA(lv_t_len)    = ls_last-end - ls_first-start.

    DATA(lv_pad_left)  = me->space( lv_pad_left_n  ).
    DATA(lv_pad_rigth) = me->space( lv_pad_right_n ).
    DATA(lv_clone) = iv_ctx+lv_start(lv_clone_len).
*--------------------------------------------------------------------*
*    IF iv_first_line = 1.
*      rs_-t_m  = iv_ctx(lv_t_len).
*      rs_-post = iv_ctx+lv_t_len(lv_post_len).
*    ELSEIF iv_first_line = lines( lt_line ).
*      rs_-pre  = iv_ctx(lv_pre_len).
*      rs_-t_m  = iv_ctx+ls_first-start(lv_t_len).
*    ELSE.
*      rs_-pre  = iv_ctx+0(lv_pre_len).
*      rs_-t_m    = iv_ctx+ls_first-start(lv_t_len).
*      rs_-post = iv_ctx+ls_last-end(lv_post_len).
*    ENDIF.

    rs_-t_m = lv_pad_left && lv_clone && lv_pad_rigth && zif_llm=>n.
    rs_-t_f = lv_clone && lv_pad_rigth && zif_llm=>n.
    rs_-t_l = lv_pad_left && lv_clone.

  ENDMETHOD.


  METHOD get_scope_s.
    DATA: lt_field TYPE tt_component.
    DATA(lv_pattern) = |{ mv_prefix }{ iv_root }{ mv_postfix }|.
    FIND ALL OCCURRENCES OF lv_pattern IN iv_ctx RESULTS DATA(lt_match).
    IF lt_match IS INITIAL.
      RETURN.
    ENDIF.
    "APPEND LINES OF lt_match_comp TO lt_match.
    LOOP AT lt_match REFERENCE INTO DATA(lr_match).
      APPEND VALUE ts_component(
        name    = ''
        pattern = lv_pattern
        offset  = lr_match->offset
        line    = get_line_by_offset(
          iv_ctx = iv_ctx
          iv_ = lr_match->offset
        )
        len     = lr_match->length
      ) TO lt_field.
    ENDLOOP.

    SORT lt_field BY offset.
    IF lt_field IS INITIAL.
      "default scope
      rs_-pre = iv_ctx.
      RETURN.
    ENDIF.

    DATA(ls_first) = lt_field[ 1 ].
    DATA(ls_last) =  lt_field[ lines( lt_field ) ].
    rs_ = me->get_scope_multiline(
      iv_ctx        = iv_ctx
      iv_start      = ls_first-offset
      iv_first_line = ls_first-line
      iv_end        = ls_last-offset + ls_last-len
      iv_last_line  = ls_last-line
    ).
  ENDMETHOD.


  METHOD get_scope_single_line.
    IF iv_ctx IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lv_line) = me->get_line_by_offset(
      iv_ = iv_start
      iv_ctx = iv_ctx
    ).
    IF lv_line IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_line) = get_offsets( iv_ctx ).
    IF lv_line > lines( lt_line ).
      RETURN.
    ENDIF.
*   rs_-line  = iv_line.
    rs_-start = iv_start.
*--------------------------------------------------------------------*
    DATA(ls_line)     = VALUE #( lt_line[ lv_line ] ).
    DATA(lv_ctx_len)  = strlen( iv_ctx ).
    DATA(lv_post_len) = lv_ctx_len - ls_line-end.
    DATA(lv_pre_len)  = ls_line-start.

    IF lv_line = 1.
      DATA(lv_clone) = iv_ctx(ls_line-len).
      rs_-post = iv_ctx+ls_line-end(lv_post_len).
    ELSEIF lv_line = lines( lt_line ).
      rs_-pre  = iv_ctx(lv_pre_len).
      lv_clone  = iv_ctx+ls_line-start(ls_line-len).
    ELSE.
      rs_-pre  = iv_ctx+0(lv_pre_len).
      lv_clone = iv_ctx+ls_line-start(ls_line-len).
      rs_-post = iv_ctx+ls_line-end(lv_post_len).
    ENDIF.

    rs_-t_m = lv_clone.
    rs_-t_f = lv_clone.
    rs_-t_l = lv_clone.

  ENDMETHOD.


  METHOD new.

    ro_ = NEW zcl_llm_00_pat(
      iv_        = iv_
      iv_prefix  = iv_prefix
      iv_postfix = iv_postfix
      iv_root    = iv_root
      iv_name    = iv_name
    ).
  ENDMETHOD.


  METHOD new_for_fcat.
    DATA(lt_) = it_.
    DELETE lt_ WHERE no_out = 'X' OR tech = 'X'.
    SORT lt_ BY col_pos.
    DATA(lv_header) = ``.
    DATA(lv_separator) = ``.
    LOOP AT lt_ REFERENCE INTO DATA(lr_).
      lv_header = COND #(
        WHEN lv_header IS INITIAL THEN |\| { lr_->seltext } \||
        ELSE |{ lv_header } { lr_->seltext } \||
      ).
      DATA(lv_pad) = pad( iv_num = strlen( lr_->seltext ) iv_ = '-' ).
      lv_separator = COND #(
        WHEN lv_separator IS INITIAL THEN |\| { lv_pad } \||
        ELSE |{ lv_separator } { lv_pad } \||
      ).
    ENDLOOP.

    DATA(lv_prefix) = COND #( WHEN iv_root IS NOT INITIAL THEN |{ iv_prefix }{ iv_root }-|
                              ELSE |{ iv_prefix }|
    ).

    DATA(lv_table) = ``.
    LOOP AT lt_ REFERENCE INTO lr_.
      lv_table = COND #(
        WHEN lv_table IS INITIAL THEN |\| { lv_prefix }{ lr_->fieldname }{ iv_postfix } \||
        ELSE |{ lv_table } { lv_prefix }{ lr_->fieldname }{ iv_postfix } \||
      ).
    ENDLOOP.

    DATA(lv_template) = lv_header && zif_llm=>n && lv_separator && zif_llm=>n && lv_table.

    ro_ = NEW zcl_llm_00_pat(
      iv_        = lv_template
      iv_prefix  = iv_prefix
      iv_postfix = iv_postfix
      iv_root    = iv_root
    ).
  ENDMETHOD.


  METHOD new_from_file.
    ro_ = NEW zcl_llm_00_pat(
      iv_        = io_->get_string( )
      iv_prefix  = iv_prefix
      iv_postfix = iv_postfix
      iv_name    = iv_name
      iv_root    = iv_root
    ).
  ENDMETHOD.


  METHOD new_from_name.

    DATA(lo_file) = io_fl->get_by_name( iv_name && '.md' ).

    ro_ = NEW zcl_llm_00_pat(
      iv_        = lo_file->get_string( )
      iv_prefix  = iv_prefix
      iv_postfix = iv_postfix
      iv_name    = iv_name
      iv_root    = iv_root
    ).
  ENDMETHOD.


  METHOD pad.
    IF iv_num = 0.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DO iv_num TIMES.
      rv_ = rv_ && iv_.
    ENDDO.

  ENDMETHOD.


  METHOD render_table_of_dref_to_scalar.
    IF it_ IS INITIAL.
      rv_ = is_-pre && is_-post.
    ENDIF.
*--------------------------------------------------------------------*
    FIELD-SYMBOLS: <fs_r> TYPE any.       "ref to line
    FIELD-SYMBOLS: <fs_l> TYPE any.       "line
    DATA: lt_table TYPE string_t.
    DATA(lv_counter) = 0.
    IF iv_type_kind = cl_abap_typedescr=>typekind_char      OR
       iv_type_kind = cl_abap_typedescr=>typekind_clike     OR
       iv_type_kind = cl_abap_typedescr=>typekind_csequence OR
       iv_type_kind = cl_abap_typedescr=>typekind_string.
      LOOP AT it_ ASSIGNING <fs_r>.
        ASSIGN <fs_r>->* TO <fs_l>.
        IF <fs_l> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.
        ADD 1 TO lv_counter.
        IF lv_counter = 1.
          APPEND me->apply_scalar(
            iv_     = <fs_l>
            iv_root = iv_root
            iv_ctx  = is_-t_f
          ) TO lt_table.

        ELSEIF lv_counter = lines( it_ ).
          APPEND me->apply_scalar(
            iv_     = <fs_l>
            iv_root = iv_root
            iv_ctx  = is_-t_l
          ) TO lt_table.

        ELSE.
          APPEND me->apply_scalar(
            iv_     = <fs_l>
            iv_root = iv_root
            iv_ctx  = is_-t_m
          ) TO lt_table.
        ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT it_ ASSIGNING <fs_r>.
        ASSIGN <fs_r>->* TO <fs_l>.
        IF <fs_l> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.
        FIELD-SYMBOLS: <fs_sub> TYPE any. "substitution field
        DATA lv_sub TYPE text255.
        WRITE <fs_l> TO lv_sub.
        CONDENSE lv_sub.
        ASSIGN lv_sub TO <fs_sub>.
        IF <fs_sub> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.
        ADD 1 TO lv_counter.
        IF lv_counter = 1.
          APPEND me->apply_scalar(
            iv_     = <fs_sub>
            iv_root = iv_root
            iv_ctx  = is_-t_f
          ) TO lt_table.

        ELSEIF lv_counter = lines( it_ ).
          APPEND me->apply_scalar(
            iv_     = <fs_sub>
            iv_root = iv_root
            iv_ctx  = is_-t_l
          ) TO lt_table.

        ELSE.
          APPEND me->apply_scalar(
            iv_     = <fs_sub>
            iv_root = iv_root
            iv_ctx  = is_-t_m
          ) TO lt_table.
        ENDIF.
      ENDLOOP.
    ENDIF.
*--------------------------------------------------------------------*
    IF lt_table IS INITIAL.
      RETURN.
    ENDIF.
    rv_ = tab_to_string_s(
      it_ = lt_table
      is_ = is_
    ).
  ENDMETHOD.


  METHOD render_table_of_dref_to_struct.
    IF it_ IS INITIAL.
      rv_ = is_-pre && is_-post.
    ENDIF.
*--------------------------------------------------------------------*
    FIELD-SYMBOLS: <fs_r> TYPE any.       "ref to structure
    FIELD-SYMBOLS: <fs_s> TYPE any.       "structure
    DATA: lt_table TYPE string_t.
    DATA(lv_counter) = 0.
    LOOP AT it_ ASSIGNING <fs_r>.
      ASSIGN <fs_r>->* TO <fs_s>.
      IF <fs_s> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      ADD 1 TO lv_counter.
      IF lv_counter = 1.
        APPEND me->apply_struct(
           is_     = <fs_s>
           iv_root = iv_root
           iv_ctx  = is_-t_f
        ) TO lt_table.

      ELSEIF lv_counter = lines( it_ ).
        APPEND me->apply_struct(
          is_     = <fs_s>
          iv_root = iv_root
          iv_ctx  = is_-t_l
        ) TO lt_table.

      ELSE.
        APPEND me->apply_struct(
          is_     = <fs_s>
          iv_root = iv_root
          iv_ctx  = is_-t_m
        ) TO lt_table.
      ENDIF.
    ENDLOOP.
*--------------------------------------------------------------------*
    IF lt_table IS INITIAL.
      RETURN.
    ENDIF.
    rv_ = tab_to_string(
      it_ = lt_table
      is_ = is_
    ).

*    DATA(lv_table) = ``.
*    LOOP AT it_ REFERENCE INTO DATA(lr_).
*      DATA(lv_) = lr_->*.
*      DATA(lv_len) = strlen( lv_ ).
*      IF lv_ IS INITIAL.
*        CONTINUE.
*      ENDIF.
*      IF is_-start = 1 AND lv_table IS INITIAL.
*        lv_table = lv_table && lv_ && zcol_=>n.
*      ELSE.
*        lv_table = lv_table && lv_.
*      ENDIF.
*    ENDLOOP.
*
*    rv_ = is_-pre && lv_table && is_-post.
  ENDMETHOD.


  METHOD render_table_of_scalar.
    IF it_ IS INITIAL.
      rv_ = is_-pre && is_-post.
    ENDIF.
*--------------------------------------------------------------------*
    FIELD-SYMBOLS: <fs_l> TYPE any.       "line
    DATA: lt_table TYPE string_t.
    DATA(lv_counter) = 0.
    IF iv_type_kind = cl_abap_typedescr=>typekind_char      OR
       iv_type_kind = cl_abap_typedescr=>typekind_clike     OR
       iv_type_kind = cl_abap_typedescr=>typekind_csequence OR
       iv_type_kind = cl_abap_typedescr=>typekind_string.
      LOOP AT it_ ASSIGNING <fs_l>.
        ADD 1 TO lv_counter.
        IF lv_counter = 1.
          APPEND me->apply_scalar(
            iv_     = <fs_l>
            iv_root = iv_root
            iv_ctx  = is_-t_f
          ) TO lt_table.

        ELSEIF lv_counter = lines( it_ ).
          APPEND me->apply_scalar(
            iv_     = <fs_l>
            iv_root = iv_root
            iv_ctx  = is_-t_l
          ) TO lt_table.

        ELSE.
          APPEND me->apply_scalar(
            iv_     = <fs_l>
            iv_root = iv_root
            iv_ctx  = is_-t_m
          ) TO lt_table.
        ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT it_ ASSIGNING <fs_l>.
        FIELD-SYMBOLS: <fs_sub> TYPE any. "substitution field
        DATA lv_sub TYPE text255.
        WRITE <fs_l> TO lv_sub.
        CONDENSE lv_sub.
        ASSIGN lv_sub TO <fs_sub>.
        IF <fs_sub> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.
        ADD 1 TO lv_counter.
        IF lv_counter = 1.
          APPEND me->apply_scalar(
            iv_     = <fs_sub>
            iv_root = iv_root
            iv_ctx  = is_-t_f
          ) TO lt_table.

        ELSEIF lv_counter = lines( it_ ).
          APPEND me->apply_scalar(
            iv_     = <fs_sub>
            iv_root = iv_root
            iv_ctx  = is_-t_l
          ) TO lt_table.

        ELSE.
          APPEND me->apply_scalar(
            iv_     = <fs_sub>
            iv_root = iv_root
            iv_ctx  = is_-t_m
          ) TO lt_table.
        ENDIF.
      ENDLOOP.
    ENDIF.
*--------------------------------------------------------------------*
    IF lt_table IS INITIAL.
      RETURN.
    ENDIF.
    rv_ = tab_to_string_s(
      it_ = lt_table
      is_ = is_
    ).
  ENDMETHOD.


  METHOD render_table_of_struct.
    IF it_ IS INITIAL.
      rv_ = is_-pre && is_-post.
    ENDIF.
*--------------------------------------------------------------------*
    FIELD-SYMBOLS: <fs_s> TYPE any.       "structure
    DATA: lt_table TYPE string_t.
    DATA(lv_counter) = 0.
    LOOP AT it_ ASSIGNING <fs_s>.
      ADD 1 TO lv_counter.
      IF lv_counter = 1.
        APPEND me->apply_struct(
           is_     = <fs_s>
           iv_root = iv_root
           iv_ctx  = is_-t_f
        ) TO lt_table.

      ELSEIF lv_counter = lines( it_ ).
        APPEND me->apply_struct(
          is_     = <fs_s>
          iv_root = iv_root
          iv_ctx  = is_-t_l
        ) TO lt_table.

      ELSE.
        APPEND me->apply_struct(
          is_     = <fs_s>
          iv_root = iv_root
          iv_ctx  = is_-t_m
        ) TO lt_table.
      ENDIF.
    ENDLOOP.
*--------------------------------------------------------------------*
    IF lt_table IS INITIAL.
      RETURN.
    ENDIF.
    rv_ = tab_to_string(
      it_ = lt_table
      is_ = is_
    ).
  ENDMETHOD.


  METHOD space.
    IF iv_num = 0.
      RETURN.
    ENDIF.

    IF iv_num <= strlen( gv_space ).
      rv_ = gv_space+0(iv_num).
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    rv_ = gv_space.
    DATA(lv_) = iv_num - strlen( gv_space ).
    DO lv_ TIMES.
      rv_ = rv_ && ` `.
    ENDDO.

  ENDMETHOD.


  METHOD tab_to_string.
    IF it_ IS INITIAL.
      rv_ = is_-pre && is_-post.
    ENDIF.
*--------------------------------------------------------------------*
    DATA(lv_table) = ``.
    LOOP AT it_ REFERENCE INTO DATA(lr_).
      DATA(lv_) = lr_->*.
      DATA(lv_len) = strlen( lv_ ).
      IF lv_ IS INITIAL.
        CONTINUE.
      ENDIF.
      IF
*        is_-start = 0 AND lv_table IS INITIAL.
*        lv_table = lv_table && lv_ && zcol_=>n.
*      ELSEIF
        is_-start = 1 AND lv_table IS INITIAL.
        lv_table = lv_table && lv_ && zif_llm=>n.
      ELSE.
        lv_table = lv_table && lv_.
      ENDIF.
    ENDLOOP.

    rv_ = is_-pre && lv_table && is_-post.
  ENDMETHOD.


  METHOD tab_to_string_s.
    IF it_ IS INITIAL.
      rv_ = is_-pre && is_-post.
    ENDIF.
*--------------------------------------------------------------------*
    DATA(lv_table) = ``.
    LOOP AT it_ REFERENCE INTO DATA(lr_).
      DATA(lv_) = lr_->*.
      DATA(lv_len) = strlen( lv_ ).
      IF lv_ IS INITIAL.
        CONTINUE.
      ENDIF.
      IF
*        is_-start = 0 AND lv_table IS INITIAL.
*        lv_table = lv_table && lv_ && zcol_=>n.
*      ELSEIF
        is_-start = 0 AND lv_table IS INITIAL.
        lv_table = lv_table && lv_ && zif_llm=>n.
      ELSE.
        lv_table = lv_table && lv_.
      ENDIF.
    ENDLOOP.

    rv_ = is_-pre && lv_table && is_-post.
  ENDMETHOD.


  METHOD zif_llm_00_pat~apply.
    mv_res = mv_.
    IF iv_root IS SUPPLIED.
      DATA(lv_root) = iv_root.
    ELSE.
      lv_root = mv_root.
    ENDIF.
    rv_ = me->apply_dref(
            ir_     = ir_
            iv_root = lv_root
    ).
  ENDMETHOD.


  METHOD zif_llm_00_pat~get_name.
    rv_ = mv_name.
  ENDMETHOD.
ENDCLASS.
