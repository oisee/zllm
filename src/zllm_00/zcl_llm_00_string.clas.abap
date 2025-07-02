class ZCL_LLM_00_STRING definition
  public
  final
  create private .

public section.

  types: string_t type Zif_LLM_00_types=>string_t.
  interfaces ZIF_LLM_00_STRING .

  class-data GV_LLM_TYPE type STRING read-only value ZCL_LLM_00_PREDICTOKEN=>GC_LLM_TYPE-GPT ##NO_TEXT.   "'GPT' ##NO_TEXT.

  class-methods NEW
    importing
      !IO_ type ANY
    returning
      value(RO_) type ref to ZIF_LLM_00_STRING .
  class-methods SET_DEFAULT_LLM_TYPE
    importing
      !IV_ type STRING .
  class-methods NEW_FROM_STRING
    importing
      !IV_ type STRING
    returning
      value(RO_) type ref to ZIF_LLM_00_STRING .
  class-methods NEW_FROM_STRING_T
    importing
      !IT_ type STRING_T
    returning
      value(RO_) type ref to ZIF_LLM_00_STRING .
  class-methods _TO_STRING
    importing
      !IR_ type ref to DATA
    returning
      value(RV_) type STRING .
  class-methods _TO_STRING_NODEPEND
    importing
      !IR_ type ref to DATA
    returning
      value(RV_) type STRING .
  class-methods _TTS .
  PROTECTED SECTION.
private section.

  data MO_ type ref to OBJECT .
  data MR_ type ref to DATA .
  data MO_STRING type ref to ZIF_LLM_00_STRING .
  data MO_TYPE type ref to CL_ABAP_TYPEDESCR .
  data MV_TYPE_KIND type ABAP_TYPEKIND .
  data MV_KIND type ABAP_TYPECATEGORY .
  data MV_STRING type STRING .
  class-data GO_PREDICTOKEN type ref to ZCL_LLM_00_PREDICTOKEN .
  data MV_TOKENS type I .

  methods CONSTRUCTOR
    importing
      !IO_ type ANY optional
      !IV_ type STRING optional .
ENDCLASS.



CLASS ZCL_LLM_00_STRING IMPLEMENTATION.


  METHOD constructor.
    IF iv_ IS SUPPLIED.
      mv_string = iv_.
      mr_ = REF #( mv_string ).
      RETURN.
    ENDIF.

    FIELD-SYMBOLS: <fs_any_t> TYPE ANY TABLE,
                   <fs_any>   TYPE any.
*--------------------------------------------------------------------*
    mo_type = cl_abap_typedescr=>describe_by_data( io_ ).
    mv_type_kind = mo_type->type_kind.
    mv_kind      = mo_type->kind.
    CASE mv_kind.
      WHEN mo_type->kind_class.
        IF io_ IS INSTANCE OF zif_llm_00_string.
          mo_string ?= io_.
        ENDIF.
*      WHEN mo_type->kind_elem.
*        mr_ = REF #( io_ ).
*      WHEN mo_type->kind_ref.
*        mr_ = REF #( io_ ).
*      WHEN mo_type->kind_struct.
*        mr_ = REF #( io_ ).
*      WHEN mo_type->kind_table.
*        mr_ = REF #( io_ ).
      WHEN OTHERS.
        mr_ = REF #( io_ ).
    ENDCASE.

  ENDMETHOD.


  METHOD new.
    ro_ ?= NEW zcl_llm_00_string( io_ = io_ ).
  ENDMETHOD.


  METHOD new_from_string.
    ro_ ?= NEW zcl_llm_00_string( iv_ = iv_ ).
  ENDMETHOD.


  METHOD new_from_string_t.
    DATA(lv_) = ``.
    LOOP AT it_ REFERENCE INTO DATA(lr_).
      IF sy-tabix = 1.
        lv_ = lr_->*.
      ELSE.
        lv_ = lv_ && zif_llm=>n && lr_->*.
      ENDIF.
    ENDLOOP.

    ro_ ?= NEW zcl_llm_00_string( iv_ = lv_ ).
  ENDMETHOD.


  METHOD set_default_llm_type.
    gv_llm_type = iv_.
  ENDMETHOD.


  METHOD zif_llm_00_string~predict_tokens.
    IF mv_tokens NE 0.
      rv_ = mv_tokens.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    IF go_predictoken IS BOUND.
      mv_tokens = go_predictoken->predict( me->zif_llm_00_string~to_string( ) ).
      rv_ = mv_tokens.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    go_predictoken = zcl_llm_00_predictoken=>new_for_model_type( gv_llm_type ).
    mv_tokens = go_predictoken->predict( me->zif_llm_00_string~to_string( ) ).
    rv_ = mv_tokens.
  ENDMETHOD.


  METHOD zif_llm_00_string~to_string.
    IF mv_string IS NOT INITIAL.
      rv_ = mv_string.
      RETURN.
    ENDIF.
    IF mo_string IS BOUND.
      rv_ = mo_string->to_string( ).
      RETURN.
    ENDIF.
    rv_ = me->_to_string( mr_ ).
  ENDMETHOD.


  METHOD zif_llm_00_string~to_xstring.
    rv_ = cl_abap_codepage=>convert_to( source = me->zif_llm_00_string~to_string( ) codepage = `UTF-8` ).
  ENDMETHOD.


  METHOD _to_string.
    DATA: lr_a TYPE REF TO data.
    DATA: lr_b TYPE REF TO data.
    FIELD-SYMBOLS: <fs_a> TYPE any.
    FIELD-SYMBOLS: <fs_t1> TYPE STANDARD TABLE. "line
    FIELD-SYMBOLS: <fs_l1> TYPE any. "line
    FIELD-SYMBOLS: <fs_r1> TYPE REF TO data.
    FIELD-SYMBOLS: <fs_t2> TYPE STANDARD TABLE. "line
    FIELD-SYMBOLS: <fs_l2> TYPE any. "line
    FIELD-SYMBOLS: <fs_ta> TYPE STANDARD TABLE. "accumulator table for flattening
    FIELD-SYMBOLS: <fs_tb> TYPE STANDARD TABLE. "accumulator table for flattening
    DATA: lv_type TYPE abap_abstypename.
    ASSIGN ir_->* TO <fs_a>.
    IF <fs_a> IS NOT ASSIGNED.
      rv_ = ``.
      RETURN.
    ENDIF.
    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( <fs_a> ).
    CASE lo_type->kind.
      WHEN lo_type->kind_elem.
        rv_ = <fs_a>.
        RETURN.
      WHEN lo_type->kind_ref.
        rv_ = _to_string( <fs_a> ).
        RETURN.
      WHEN lo_type->kind_struct.
        rv_ = zcl_llm_00_json=>to_json( <fs_a> ).
        RETURN.
      WHEN lo_type->kind_table.
        IF <fs_a> IS INITIAL.
          RETURN.
        ENDIF.
*       <fs_a> =
        data(lr_) = zcl_llm_00_json=>flatten_dref( REF #( <fs_a> ) ).
        rv_ = zcl_llm_00_json=>to_json( lr_ ).
        RETURN.
    ENDCASE.
  ENDMETHOD.


  METHOD _TO_STRING_NODEPEND.
    data: lr_a TYPE REF TO data.
    data: lr_b TYPE REF TO data.
    FIELD-SYMBOLS: <fs_a> TYPE any.
    FIELD-SYMBOLS: <fs_t1> TYPE STANDARD TABLE. "line
    FIELD-SYMBOLS: <fs_l1> TYPE any. "line
    FIELD-SYMBOLS: <fs_r1> TYPE REF TO data.
    FIELD-SYMBOLS: <fs_t2> TYPE STANDARD TABLE. "line
    FIELD-SYMBOLS: <fs_l2> TYPE any. "line
    FIELD-SYMBOLS: <fs_ta> TYPE STANDARD TABLE. "accumulator table for flattening
    FIELD-SYMBOLS: <fs_tb> TYPE STANDARD TABLE. "accumulator table for flattening
    data: lv_type TYPE ABAP_ABSTYPENAME.
    ASSIGN ir_->* TO <fs_a>.
    IF <fs_a> IS NOT ASSIGNED.
      rv_ = ``.
      RETURN.
    ENDIF.
    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( <fs_a> ).
    CASE lo_type->kind.
      WHEN lo_type->kind_elem.
        rv_ = <fs_a>.
        RETURN.
      WHEN lo_type->kind_ref.
        rv_ = _to_string( <fs_a> ).
        RETURN.
      WHEN lo_type->kind_struct.
        rv_ = zcl_llm_00_json=>to_json( <fs_a> ).
        RETURN.
      WHEN lo_type->kind_table.
        IF <fs_a> IS INITIAL.
          RETURN.
        ENDIF.
        ASSIGN <fs_a> TO <fs_t1>.
        READ TABLE <fs_t1> ASSIGNING <fs_l1> INDEX 1.
        IF sy-subrc NE 0.
          RETURN.
        ENDIF.
        IF zcl_llm_00_json=>is_string( <fs_l1> ).
          rv_ = zcl_llm_00_pat=>new( '{T} ' && zif_llm=>n )->apply( ir_ ).
          RETURN.
        ELSEIF zcl_llm_00_json=>is_table(
            EXPORTING ir_ = <fs_l1>
            IMPORTING er_ = lr_a ev_type = lv_type
          ).
          LOOP AT <fs_t1> ASSIGNING <fs_l1>.
            IF sy-tabix = 1.
              ASSIGN lr_a->* TO <fs_ta>.
            ELSE.
              zcl_llm_00_json=>is_table(
                EXPORTING ir_ = <fs_l1>
                IMPORTING er_ = lr_b ev_type = lv_type
              ).
              ASSIGN lr_b->* TO <fs_tb>.
              APPEND LINES OF <fs_tb> TO <fs_ta>.
            ENDIF.
          ENDLOOP.
          rv_ = _to_string( REF #( <fs_ta> ) ).
        ELSE.
*          LOOP AT <fs_t> ASSIGNING <fs_l>.
*            IF sy-tabix = 1.
*              rv_ = me->_to_string( ref #( <fs_l> ) ).
*            ELSE.
*              rv_ = rv_ && me->_to_string( ref #( <fs_l> ) ).
*            ENDIF.
*          ENDLOOP.
          rv_ = zcl_llm_00_json=>to_json( <fs_t1> ).
        ENDIF.
        RETURN.
    ENDCASE.
  ENDMETHOD.


  method _TTS.
  endmethod.
ENDCLASS.
