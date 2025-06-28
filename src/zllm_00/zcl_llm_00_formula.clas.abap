class ZCL_LLM_00_FORMULA definition
  public
  final
  create private .

public section.

  interfaces ZIF_LLM_00_FORMULA .

  methods CONSTRUCTOR
    importing
      !IO_PAT_SYS type ref to ZIF_LLM_00_PAT
      !IO_PAT_USR type ref to ZIF_LLM_00_PAT
      !IV_PREFIX type STRING default '{'
      !IV_POSTFIX type STRING default '}'
      !IV_NAME type STRING default 'default'
      !IV_ROOT type STRING default 'T' .
  class-methods NEW_FROM_PATS
    importing
      !IO_PAT_SYS type ref to ZIF_LLM_00_PAT
      !IO_PAT_USR type ref to ZIF_LLM_00_PAT
      !IV_PREFIX type STRING default '{'
      !IV_POSTFIX type STRING default '}'
      !IV_NAME type STRING default 'default'
      !IV_ROOT type STRING default 'T'
    returning
      value(RO_) type ref to ZIF_LLM_00_FORMULA .
  class-methods NEW_FROM_FILES
    importing
      !IO_FILE_SYS type ref to ZIF_LLM_00_FILE
      !IO_FILE_USR type ref to ZIF_LLM_00_FILE
      !IV_PREFIX type STRING default '{'
      !IV_POSTFIX type STRING default '}'
      !IV_NAME type STRING default 'default'
      !IV_ROOT type STRING default 'T'
    returning
      value(RO_) type ref to ZIF_LLM_00_FORMULA .
  class-methods NEW_FROM_NAME
    importing
      !IO_FL type ref to ZIF_LLM_00_FILE_LIST
      !IV_NAME type STRING
      !IV_PREFIX type STRING default '{'
      !IV_POSTFIX type STRING default '}'
      !IV_ROOT type STRING default 'T'
    returning
      value(RO_) type ref to ZIF_LLM_00_FORMULA .
  class-methods NEW_FROM_USR_FILE
    importing
      !IO_FILE_USR type ref to ZIF_LLM_00_FILE
      !IV_PREFIX type STRING default '{'
      !IV_POSTFIX type STRING default '}'
      !IV_NAME type STRING default 'default'
      !IV_ROOT type STRING default 'T'
      !IV_SYSTEM type STRING
    returning
      value(RO_) type ref to ZIF_LLM_00_FORMULA .
  class-methods NEW_FROM_NAME_AND_SYS_FILE
    importing
      !IO_FL type ref to ZIF_LLM_00_FILE_LIST
      !IV_NAME type STRING
      !IV_PREFIX type STRING default '{'
      !IV_POSTFIX type STRING default '}'
      !IV_ROOT type STRING default 'T'
      !IO_FILE_SYS type ref to ZIF_LLM_00_FILE
    returning
      value(RO_) type ref to ZIF_LLM_00_FORMULA .
  class-methods NEW_FROM_NAME_AND_SYS
    importing
      !IO_FL type ref to ZIF_LLM_00_FILE_LIST
      !IV_NAME type STRING
      !IV_PREFIX type STRING default '{'
      !IV_POSTFIX type STRING default '}'
      !IV_ROOT type STRING default 'T'
      !IV_SYSTEM type STRING
    returning
      value(RO_) type ref to ZIF_LLM_00_FORMULA .
  class-methods NEW_FROM_USR_AND_SYS
    importing
      !IV_PREFIX type STRING default '{'
      !IV_POSTFIX type STRING default '}'
      !IV_ROOT type STRING default 'T'
      !IV_SYSTEM type STRING
      !IV_USER type STRING
      !IV_NAME type STRING
    returning
      value(RO_) type ref to ZIF_LLM_00_FORMULA .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mo_pat_sys TYPE REF TO zif_llm_00_pat,
          mo_pat_usr TYPE REF TO zif_llm_00_pat,
          mv_prefix  TYPE string,
          mv_postfix TYPE string,
          mv_name    TYPE string,
          mv_root    TYPE string.
    DATA: mo_file    TYPE REF TO zif_llm_00_file.
ENDCLASS.



CLASS ZCL_LLM_00_FORMULA IMPLEMENTATION.


  METHOD constructor.
    mo_pat_sys = io_pat_sys.
    mo_pat_usr = io_pat_usr.
    mv_prefix  = iv_prefix.
    mv_postfix = iv_postfix.
    mv_name    = iv_name.
  ENDMETHOD.


  METHOD new_from_files.

    ro_ = NEW zcl_llm_00_formula(
      io_pat_sys = zcl_llm_00_pat=>new_from_file(
                     io_        = io_file_sys
                     iv_prefix  = iv_prefix
                     iv_postfix = iv_postfix
                     iv_root    = iv_root
                     iv_name    = iv_name
                   )
      io_pat_usr = zcl_llm_00_pat=>new_from_file(
                     io_        = io_file_usr
                     iv_prefix  = iv_prefix
                     iv_postfix = iv_postfix
                     iv_root    = iv_root
                     iv_name    = iv_name
                   )
      iv_prefix  = iv_prefix
      iv_postfix = iv_postfix
      iv_name    = iv_name
    ).

  ENDMETHOD.


  METHOD new_from_name.
    DATA(lo_file_sys) = io_fl->get_by_name( |{ iv_name }.sys.md| ).
    IF lo_file_sys IS NOT BOUND.
      lo_file_sys = io_fl->get_by_name( |sys.md| ).
    ENDIF.
    IF lo_file_sys IS NOT BOUND.
      lo_file_sys = zcl_llm_00_file_mock=>new( iv_content = '' iv_path = 'blank.sys.md' ).
    ENDIF.
*--------------------------------------------------------------------*
    DATA(lo_file_usr) = io_fl->get_by_name( |{ iv_name }.usr.md| ).
    IF lo_file_usr IS NOT BOUND.
      lo_file_usr = io_fl->get_by_name( |usr.md| ).
    ENDIF.
    IF lo_file_usr IS NOT BOUND.
      lo_file_usr = zcl_llm_00_file_mock=>new( iv_content = '' iv_path = 'blank.usr.md' ).
    ENDIF.

    ro_ = zcl_llm_00_formula=>new_from_files(
      io_file_sys = lo_file_sys
      io_file_usr = lo_file_usr
      iv_prefix   = iv_prefix
      iv_postfix  = iv_postfix
      iv_name     = iv_name
      iv_root     = iv_root
    ).
  ENDMETHOD.


  METHOD NEW_FROM_NAME_AND_SYS.
    DATA(lo_file_sys) = zcl_llm_00_file_mock=>new(
                          iv_content = iv_system
                          iv_path    = 'adhoc.sys.md'
                        ).

    DATA(lo_file_usr) = io_fl->get_by_name( |{ iv_name }.usr.md| ).
    IF lo_file_usr IS NOT BOUND.
      lo_file_usr = io_fl->get_by_name( |default.usr.md| ).
    ENDIF.
    IF lo_file_usr IS NOT BOUND.
      lo_file_usr = zcl_llm_00_file_mock=>new( iv_content = '' iv_path = 'blank.usr.md' ).
    ENDIF.

    ro_ = zcl_llm_00_formula=>new_from_files(
      io_file_sys = lo_file_sys
      io_file_usr = lo_file_usr
      iv_prefix   = iv_prefix
      iv_postfix  = iv_postfix
      iv_name     = iv_name
      iv_root     = iv_root
    ).

  ENDMETHOD.


  METHOD NEW_FROM_NAME_AND_SYS_FILE.
    DATA(lo_file_usr) = io_fl->get_by_name( |{ iv_name }.usr.md| ).
    IF lo_file_usr IS NOT BOUND.
      lo_file_usr = io_fl->get_by_name( |default.usr.md| ).
    ENDIF.
    IF lo_file_usr IS NOT BOUND.
      lo_file_usr = zcl_llm_00_file_mock=>new( iv_content = '' iv_path = 'blank.usr.md' ).
    ENDIF.

    ro_ = zcl_llm_00_formula=>new_from_files(
      io_file_sys = io_file_sys
      io_file_usr = lo_file_usr
      iv_prefix   = iv_prefix
      iv_postfix  = iv_postfix
      iv_name     = iv_name
      iv_root     = iv_root
    ).

  ENDMETHOD.


  METHOD new_from_pats.

    ro_ = NEW zcl_llm_00_formula(
      io_pat_sys = io_pat_sys
      io_pat_usr = io_pat_usr
      iv_prefix  = iv_prefix
      iv_postfix = iv_postfix
      iv_name    = iv_name
    ).

  ENDMETHOD.


  METHOD new_from_usr_and_sys.
    DATA(lo_file_sys) = zcl_llm_00_file_mock=>new(
                          iv_content = iv_system
                          iv_path    = 'adhoc.sys.md'
                        ).
    DATA(lo_file_usr) = zcl_llm_00_file_mock=>new(
                          iv_content = iv_user
                          iv_path    = 'adhoc.usr.md'
                        ).

    ro_ = zcl_llm_00_formula=>new_from_files(
      io_file_sys = lo_file_sys
      io_file_usr = lo_file_usr
      iv_prefix   = iv_prefix
      iv_postfix  = iv_postfix
      iv_name     = iv_name
      iv_root     = iv_root
    ).

  ENDMETHOD.


  METHOD new_from_usr_file.

    ro_ = NEW zcl_llm_00_formula(
      io_pat_sys = zcl_llm_00_pat=>new(
                     iv_        = iv_system
                     iv_prefix  = iv_prefix
                     iv_postfix = iv_postfix
                     iv_root    = iv_root
                     iv_name    = 'adhoc.sys.md'
                   )
      io_pat_usr = zcl_llm_00_pat=>new_from_file(
                     io_        = io_file_usr
                     iv_prefix  = iv_prefix
                     iv_postfix = iv_postfix
                     iv_root    = iv_root
*                    iv_name    = iv_name
                   )
      iv_prefix  = iv_prefix
      iv_postfix = iv_postfix
      iv_name    = iv_name
    ).

  ENDMETHOD.


  METHOD zif_llm_00_formula~apply.
    rs_-sys = mo_pat_sys->apply(
      ir_     = ir_
      iv_root = mv_root
    ).
    rs_-usr = mo_pat_sys->apply(
      ir_     = ir_
      iv_root = mv_root
    ).
  ENDMETHOD.


  METHOD zif_llm_00_formula~get_name.
    rv_ = mv_name.
  ENDMETHOD.


  METHOD zif_llm_00_formula~get_sys.
    ro_ = mo_pat_sys.
  ENDMETHOD.


  METHOD zif_llm_00_formula~get_usr.
    ro_ = mo_pat_usr.
  ENDMETHOD.
ENDCLASS.
