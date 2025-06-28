CLASS zcl_llm_00_tvarvc DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    TYPES:
      ttr_ TYPE RANGE OF tvarvc-low .
    TYPES:
      tsr_ TYPE LINE OF ttr_ .

    CLASS-METHODS exists
      IMPORTING
        !iv_name   TYPE tvarvc-name
      RETURNING
        VALUE(rv_) TYPE abap_bool .
    CLASS-METHODS get_default_range
      IMPORTING
        !iv_default  TYPE tvarvc-low OPTIONAL
        !itr_default TYPE ttr_ OPTIONAL
      RETURNING
        VALUE(rtr_)  TYPE ttr_ .
    CLASS-METHODS get_impossible_range
      RETURNING
        VALUE(rtr_) TYPE ttr_ .
    CLASS-METHODS get_integer_by_name
      IMPORTING
        !iv_name    TYPE tvarvc-name
        !iv_default TYPE i DEFAULT 0
          PREFERRED PARAMETER iv_name
      RETURNING
        VALUE(rv_)  TYPE i .
    CLASS-METHODS get_package_by_prog
      IMPORTING
        !iv_prog   TYPE sy-cprog DEFAULT sy-cprog
      RETURNING
        VALUE(rv_) TYPE tadir-devclass .
    CLASS-METHODS get_parameter_by_name
      IMPORTING
        !iv_name    TYPE tvarvc-name
        !iv_default TYPE tvarvc-low OPTIONAL
          PREFERRED PARAMETER iv_name
      RETURNING
        VALUE(rv_)  TYPE tvarvc-low .
    CLASS-METHODS get_range_by_name
      IMPORTING
        !iv_name     TYPE tvarvc-name
        !iv_default  TYPE tvarvc-low OPTIONAL
        !itr_default TYPE ttr_ OPTIONAL
          PREFERRED PARAMETER iv_name
      RETURNING
        VALUE(rtr_)  TYPE ttr_ .
    CLASS-METHODS new
      RETURNING
        VALUE(ro_) TYPE REF TO ZCL_LLM_00_TVARVC .
    CLASS-METHODS new_for_context
      IMPORTING
        !iv_postfix          TYPE string DEFAULT ''
        !iv_fallback_postfix TYPE string DEFAULT ''
        !iv_prefix           TYPE string DEFAULT ''
          PREFERRED PARAMETER iv_postfix
      RETURNING
        VALUE(ro_)           TYPE REF TO ZCL_LLM_00_TVARVC .
    CLASS-METHODS new_for_language
      IMPORTING
        !iv_       TYPE sy-langu DEFAULT sy-langu
      RETURNING
        VALUE(ro_) TYPE REF TO ZCL_LLM_00_TVARVC .
    CLASS-METHODS new_for_package
      IMPORTING
        !iv_       TYPE tadir-devclass OPTIONAL
        !iv_prog   TYPE sy-cprog DEFAULT sy-cprog
          PREFERRED PARAMETER iv_
      RETURNING
        VALUE(ro_) TYPE REF TO ZCL_LLM_00_TVARVC .
    CLASS-METHODS new_for_postfix
      IMPORTING
        !iv_         TYPE string DEFAULT ''
        !iv_fallback TYPE string DEFAULT ''
          PREFERRED PARAMETER iv_
      RETURNING
        VALUE(ro_)   TYPE REF TO ZCL_LLM_00_TVARVC .
    CLASS-METHODS to_i_or_default
      IMPORTING
        !iv_        TYPE tvarvc-low
        !iv_default TYPE i DEFAULT 0
      RETURNING
        VALUE(rv_)  TYPE i .
    METHODS i
      IMPORTING
        !iv_name    TYPE tvarvc-name
        !iv_default TYPE i DEFAULT 0
          PREFERRED PARAMETER iv_name
      RETURNING
        VALUE(rv_)  TYPE i .
    METHODS ir
      RETURNING
        VALUE(rtr_) TYPE ttr_ .
    METHODS p
      IMPORTING
        !iv_name    TYPE tvarvc-name
        !iv_default TYPE tvarvc-low OPTIONAL
          PREFERRED PARAMETER iv_name
      RETURNING
        VALUE(rv_)  TYPE tvarvc-low .
    METHODS r
      IMPORTING
        !iv_name     TYPE tvarvc-name
        !iv_default  TYPE tvarvc-low OPTIONAL
        !itr_default TYPE ttr_ OPTIONAL
          PREFERRED PARAMETER iv_name
      RETURNING
        VALUE(rtr_)  TYPE ttr_ .
    CLASS-METHODS get_exclude_all
      RETURNING
        VALUE(rtr_) TYPE ttr_ .
    METHODS ea
      RETURNING
        VALUE(rtr_) TYPE ttr_ .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_with_respect_to_context TYPE abap_bool .
    DATA mv_prefix  TYPE string .
    DATA mv_postfix TYPE string .
    DATA mv_fallback_postfix TYPE string .
ENDCLASS.



CLASS ZCL_LLM_00_TVARVC IMPLEMENTATION.


  METHOD ea.
    rtr_ = get_impossible_range( ).
  ENDMETHOD.


  METHOD exists.
    SELECT SINGLE * FROM tvarvc INTO @DATA(ls_) WHERE name = @iv_name. "AND type = 'P'.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
    rv_ = abap_true.
  ENDMETHOD.


  METHOD get_default_range.
    rtr_ = itr_default.
    IF iv_default IS SUPPLIED AND
       iv_default IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = iv_default ) TO rtr_.
    ENDIF.

    LOOP AT rtr_ ASSIGNING FIELD-SYMBOL(<fsr_>) WHERE sign IS INITIAL OR option IS INITIAL.
      IF <fsr_>-sign IS INITIAL.
        <fsr_>-sign = 'I'.
      ENDIF.
      IF <fsr_>-option IS INITIAL.
        <fsr_>-option = 'EQ'.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_exclude_all.
    rtr_ = VALUE #( ( sign = 'E' option = 'CP' low = '*' ) ).
  ENDMETHOD.


  METHOD get_impossible_range.
    rtr_ = VALUE #( ( sign = 'E' option = 'CP' low = '*' ) ).
  ENDMETHOD.


  METHOD get_integer_by_name.
    rv_ = to_i_or_default(
            iv_        = get_parameter_by_name( iv_name )
            iv_default = iv_default
    ).
  ENDMETHOD.


  METHOD get_package_by_prog.

    SELECT SINGLE devclass
      FROM tadir
      WHERE obj_name = @iv_prog AND
            object   = 'PROG'
      INTO @rv_.

  ENDMETHOD.


  METHOD get_parameter_by_name.
    rv_ = iv_default.
    SELECT SINGLE low FROM tvarvc INTO rv_ WHERE name = iv_name AND type = 'P'.
  ENDMETHOD.


  METHOD get_range_by_name.
    SELECT sign, opti AS option, low, high FROM tvarvc
      INTO CORRESPONDING FIELDS OF TABLE @rtr_
      WHERE name = @iv_name
        AND type = 'S'.
    "AND low NE '' AND high ne ''.
*--------------------------------------------------------------------*
    IF rtr_ IS INITIAL.
      rtr_ = get_default_range(
        iv_default  = iv_default
        itr_default = itr_default
      ).
    ENDIF.
  ENDMETHOD.


  METHOD i.
    rv_ = to_i_or_default(
            iv_        = me->p( iv_name )
            iv_default = iv_default
    ).
  ENDMETHOD.


  METHOD ir.
    rtr_ = get_impossible_range( ).
  ENDMETHOD.


  METHOD new.

    ro_ = NEW #( ).

  ENDMETHOD.


  METHOD new_for_context.
    ro_ = NEW #( ).
    IF iv_prefix           IS INITIAL AND
       iv_postfix          IS INITIAL AND
       iv_fallback_postfix IS INITIAL.
      RETURN.
    ENDIF.
    ro_->mv_with_respect_to_context = abap_true.
    ro_->mv_prefix           = COND #( WHEN iv_prefix  IS NOT INITIAL THEN |{ iv_prefix }_|
                                       ELSE '' ).
    ro_->mv_postfix          = COND #( WHEN iv_postfix IS NOT INITIAL THEN |_{ iv_postfix }|
                                       ELSE '' ).
    ro_->mv_fallback_postfix = COND #( WHEN iv_fallback_postfix IS NOT INITIAL THEN |_{ iv_fallback_postfix }|
                                       ELSE '' ).
  ENDMETHOD.


  METHOD new_for_language.
    DATA(lv_lang) = cl_i18n_languages=>sap1_to_sap2( iv_ ).
    ro_ = ZCL_LLM_00_TVARVC=>new_for_postfix(
      iv_         = CONV #( lv_lang )
      iv_fallback = 'EN'
    ).
  ENDMETHOD.


  METHOD new_for_package.
    IF iv_ IS SUPPLIED.
      DATA(lv_package) = CONV string( iv_ ).
    ELSE.
      lv_package = get_package_by_prog( iv_prog ).
    ENDIF.

    ro_ = ZCL_LLM_00_TVARVC=>new_for_context(
*            iv_postfix          = ''
*            iv_fallback_postfix = ''
             iv_prefix           = lv_package
    ).

  ENDMETHOD.


  METHOD new_for_postfix.
    ro_ = ZCL_LLM_00_TVARVC=>new_for_context(
      iv_postfix          = iv_
      iv_fallback_postfix = iv_fallback
    ).
  ENDMETHOD.


  METHOD p.
    rv_ = iv_default.
    IF mv_with_respect_to_context IS INITIAL.
      rv_ = get_parameter_by_name( iv_name = iv_name iv_default = iv_default ).
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lv_name TYPE tvarvc-name.
    lv_name = |{ mv_prefix }{ iv_name }{ mv_postfix }|.
    IF me->exists( lv_name ).
      rv_ = get_parameter_by_name( iv_name = lv_name iv_default = iv_default ).
      RETURN.
    ENDIF.
    lv_name = |{ mv_prefix }{ iv_name }{ mv_fallback_postfix }|.
    IF me->exists( lv_name ).
      rv_ = get_parameter_by_name( iv_name = lv_name iv_default = iv_default ).
      RETURN.
    ENDIF.
    lv_name = |{ mv_prefix }{ iv_name }|.
    IF me->exists( lv_name ).
      rv_ = get_parameter_by_name( iv_name = lv_name iv_default = iv_default ).
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD r.
    IF mv_with_respect_to_context IS INITIAL.
      rtr_ = get_range_by_name( iv_name = iv_name iv_default  = iv_default itr_default = itr_default ).
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lv_name TYPE tvarvc-name.
    lv_name = |{ mv_prefix }{ iv_name }{ mv_postfix }|.
    IF me->exists( lv_name ).
      rtr_ = get_range_by_name( iv_name = lv_name iv_default  = iv_default itr_default = itr_default ).
      RETURN.
    ENDIF.
    lv_name = |{ mv_prefix }{ iv_name }{ mv_fallback_postfix }|.
    IF me->exists( lv_name ).
      rtr_ = get_range_by_name( iv_name = lv_name iv_default  = iv_default itr_default = itr_default ).
      RETURN.
    ENDIF.
    lv_name = |{ mv_prefix }{ iv_name }|.
    IF me->exists( lv_name ).
      rtr_ = get_range_by_name( iv_name = lv_name iv_default  = iv_default itr_default = itr_default ).
      RETURN.
    ENDIF.

*--------------------------------------------------------------------*
    IF rtr_ IS INITIAL.
      rtr_ = get_default_range(
        iv_default  = iv_default
        itr_default = itr_default
      ).
    ENDIF.

  ENDMETHOD.


  METHOD to_i_or_default.

    rv_ = iv_default.

    DATA(lv_) = condense( val = iv_  from = '.,' to = space ).
    SPLIT lv_ AT space INTO lv_ DATA(lv_tail).

    IF iv_ CO '0123456789'.
      rv_ = CONV #( lv_ ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
