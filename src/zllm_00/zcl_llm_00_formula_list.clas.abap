class ZCL_LLM_00_FORMULA_LIST definition
  public
  final
  create private .

public section.

  interfaces ZIF_LLM_00_FORMULA_LIST .

  aliases FILTER
    for ZIF_LLM_00_FORMULA_LIST~FILTER .
  aliases GET
    for ZIF_LLM_00_FORMULA_LIST~GET .
  aliases GET_BY_NAME
    for ZIF_LLM_00_FORMULA_LIST~GET_BY_NAME .
  aliases TS_FORMULA
    for ZIF_LLM_00_FORMULA_LIST~TS_FORMULA .
  aliases TT_FILE
    for ZIF_LLM_00_FORMULA_LIST~TT_FILE .
  aliases TT_FORMULA
    for ZIF_LLM_00_FORMULA_LIST~TT_FORMULA .

  class-methods NEW
    importing
      !IT_ type TT_FORMULA
    returning
      value(RO_) type ref to ZIF_LLM_00_FORMULA_LIST .
  class-methods NEW_FROM_FL
    importing
      !IO_FL type ref to ZIF_LLM_00_FILE_LIST
      !IV_PREFIX type STRING default '{'
      !IV_POSTFIX type STRING default '}'
      !IV_ROOT type STRING default 'T'
    returning
      value(RO_) type ref to ZIF_LLM_00_FORMULA_LIST .
  class-methods NEW_FROM_FOLDER
    importing
      !IV_ type STRING
      !IV_MASK type STRING default '*.*'
      !IO_MASK type ref to ZCL_LLM_00_LIST optional
      !IV_ROOT type STRING default 'T'
    preferred parameter IV_
    returning
      value(RO_) type ref to ZIF_LLM_00_FORMULA_LIST .
  class-methods NEW_FROM_PACKAGE
    importing
      !IV_ type STRING
      !IV_MASK type STRING default '*.*'
      !IO_MASK type ref to ZCL_LLM_00_LIST optional
      !IV_ROOT type STRING default 'T'
    preferred parameter IV_
    returning
      value(RO_) type ref to ZIF_LLM_00_FORMULA_LIST .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_      TYPE tt_formula .

    METHODS constructor
      IMPORTING
        !it_ TYPE tt_formula.
ENDCLASS.



CLASS ZCL_LLM_00_FORMULA_LIST IMPLEMENTATION.


  METHOD constructor.
    mt_ = it_.
    IF mt_ IS INITIAL.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD new.
    ro_ = NEW zcl_llm_00_formula_list( it_ ).
  ENDMETHOD.


  METHOD new_from_fl.
    DATA(lv_tail_len) = 6. "strlen('sys.md').
    DATA lt_formula TYPE tt_formula.
    DATA(lt_name) = io_fl->filter( '*.sys.md|*.usr.md|sys.md|usr.md' ).
    LOOP AT lt_name REFERENCE INTO DATA(lr_name).
      IF lr_name->name CP '*sys.md' AND
         lr_name->name CP '*usr.md'.
        DATA(lv_head_len) = strlen( lr_name->name ) - lv_tail_len.
        lr_name->name = to_upper( lr_name->name+0(lv_head_len) ).
      ENDIF.
    ENDLOOP.
*--------------------------------------------------------------------*
    SORT lt_name BY name.
    DELETE ADJACENT DUPLICATES FROM lt_name COMPARING name.
*   DELETE lt_name WHERE name is INITIAL.
*   DATA(lo_fl) = zcl_llm_00_file_list_local=>new( lt_name ).
*--------------------------------------------------------------------*
    LOOP AT lt_name REFERENCE INTO DATA(lr_).
      DATA(lo_fm) = zcl_llm_00_formula=>new_from_name(
        io_fl      = io_fl
        iv_name    = lr_name->name
        iv_prefix  = iv_prefix
        iv_postfix = iv_postfix
        iv_root    = iv_root
      ).
    ENDLOOP.

  ENDMETHOD.


  METHOD new_from_folder.
*    DATA lv_len TYPE i.
*    DATA lt_ TYPE STANDARD TABLE OF text1024.
*    cl_gui_frontend_services=>directory_list_files(
*      EXPORTING
*        directory                   = iv_            " Directory To Search
*"       filter                      = '*'
*        files_only                  = 'X'
*      CHANGING
*        file_table                  = lt_
*        count                       = lv_len       " Number of Files/Dir Found
*      EXCEPTIONS
*        cntl_error                  = 1                " Control error
*        directory_list_files_failed = 2                " Could not list files in the directory
*        wrong_parameter             = 3                " Incorrect parameter combination
*        error_no_gui                = 4                " No GUI available
*        not_supported_by_gui        = 5                " GUI does not support this
*    ).
*    IF sy-subrc <> 0 OR lt_ IS INITIAL.
*      DATA(lo_mock) = zcl_llm_00_file_mock=>new(
*                         iv_content = '{T}'
*                         iv_path    = 'MOCK'
*      ).
**      ro_ = zcl_llm_00_ptl=>new_from_fl(  ).
*      RETURN.
*    ENDIF.
**--------------------------------------------------------------------*
*    IF io_mask IS BOUND.
*      DATA(lo_mask) = io_mask.
*    ELSE.
*      lo_mask = zcl_llm_00_list=>new_from_string( iv_mask ).
*    ENDIF.
*    DATA(ltr_mask) = lo_mask->r( ).
**--------------------------------------------------------------------*
*    DATA: lt_file TYPE tt_file .
*    LOOP AT lt_ REFERENCE INTO DATA(lr_) WHERE table_line IN ltr_mask.
*      DATA(lo_file) = zcl_llm_00_file_local=>new( CONV #( |{ iv_ }\\{ lr_->* }| ) ).
*      APPEND VALUE #(
*        file = lo_file
*        name = lo_file->get_name( )
*      ) TO lt_file.
*    ENDLOOP.
*    SORT lt_file BY name.
*    ro_ = zcl_llm_00_pat_list=>new_from_fl( lt_file ).
  ENDMETHOD.


  METHOD new_from_package.
    IF io_mask IS BOUND.
      DATA(lo_mask) = io_mask.
    ELSE.
      lo_mask = zcl_llm_00_list=>new_from_string( to_upper( iv_mask ) ).
    ENDIF.
    DATA(ltr_mask) = lo_mask->r( ).
    SELECT *
      FROM tadir
      WHERE devclass = @iv_ AND
            object   = 'W3MI' AND
            obj_name IN @ltr_mask
      INTO TABLE @DATA(lt_).
    IF lt_ IS INITIAL.
      DATA(lo_mock) = zcl_llm_00_file_mock=>new(
                         iv_content = '{IN}'
                         iv_path    = 'MOCK'
      ).
      ro_ = zcl_llm_00_formula_list=>new_from_fl(
        zcl_llm_00_file_list_local=>new(
        VALUE #( ( file = lo_mock name = lo_mock->get_name( ) ) )
      ) ).
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lt_file TYPE tt_file.
    LOOP AT lt_ REFERENCE INTO DATA(lr_).
      DATA(lo_file) = zcl_llm_00_file_smw0=>new( CONV #( lr_->obj_name ) ).
      APPEND VALUE #(
        file = lo_file
        name = lo_file->get_name( )
      ) TO lt_file.
    ENDLOOP.
    SORT lt_file BY name.
    ro_ = zcl_llm_00_formula_list=>new_from_fl(
        zcl_llm_00_file_list_local=>new(
        VALUE #( ( file = lo_mock name = lo_mock->get_name( ) ) )
      )
    ).
  ENDMETHOD.


  METHOD zif_llm_00_formula_list~filter.
    IF io_ IS NOT BOUND.
      DATA(lo_list) = zcl_llm_00_list=>new_from_string( iv_ ).
    ELSE.
      lo_list = io_.
    ENDIF.
    DATA(ltr_) = lo_list->r( ).
    LOOP AT mt_ REFERENCE INTO DATA(lr_) WHERE name IN ltr_.
      APPEND lr_->* TO rt_.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_llm_00_formula_list~get.
    rt_ = mt_.
  ENDMETHOD.


  METHOD zif_llm_00_formula_list~get_by_name.
    ro_ = VALUE #( mt_[ name = iv_ ]-formula OPTIONAL ).
  ENDMETHOD.
ENDCLASS.
