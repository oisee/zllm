class ZCL_LLM_00_PAT_LIST definition
  public
  final
  create private .

public section.

  interfaces ZIF_LLM_00_PAT_LIST .

  aliases FILTER
    for ZIF_LLM_00_PAT_LIST~FILTER .
  aliases GET
    for ZIF_LLM_00_PAT_LIST~GET .
  aliases GET_BY_NAME
    for ZIF_LLM_00_PAT_LIST~GET_BY_NAME .
  aliases TS_PAT
    for ZIF_LLM_00_PAT_LIST~TS_PAT .
  aliases TT_FILE
    for ZIF_LLM_00_PAT_LIST~TT_FILE .
  aliases TT_PT
    for ZIF_LLM_00_PAT_LIST~TT_PAT .

  class-methods NEW
    importing
      !IT_ type TT_PT
    returning
      value(RO_) type ref to ZIF_LLM_00_PAT_LIST .
  class-methods NEW_FROM_FL
    importing
      !IT_ type TT_FILE
      !IV_PREFIX type STRING default '{'
      !IV_POSTFIX type STRING default '}'
    returning
      value(RO_) type ref to ZIF_LLM_00_PAT_LIST .
  class-methods NEW_FROM_FOLDER
    importing
      !IV_ type STRING
      !IV_MASK type STRING default '*.*'
      !IO_MASK type ref to ZCL_LLM_00_LIST optional
    preferred parameter IV_
    returning
      value(RO_) type ref to ZIF_LLM_00_PAT_LIST .
  class-methods NEW_FROM_PACKAGE
    importing
      !IV_ type STRING
      !IV_MASK type STRING default '*.*'
      !IO_MASK type ref to ZCL_LLM_00_LIST optional
    preferred parameter IV_
    returning
      value(RO_) type ref to ZIF_LLM_00_PAT_LIST .
  PROTECTED SECTION.
  PRIVATE SECTION.


    DATA mt_ TYPE tt_pt .
    DATA mv_index TYPE i .
    DATA mv_len TYPE i .

    METHODS constructor
      IMPORTING
        !it_ TYPE tt_pt .
ENDCLASS.



CLASS ZCL_LLM_00_PAT_LIST IMPLEMENTATION.


  METHOD constructor.
    mt_ = it_.
    IF mt_ IS INITIAL.
      RETURN.
    ENDIF.
    mv_index = 1.
    mv_len   = lines( mt_ ).
  ENDMETHOD.


  METHOD new.
    ro_ = NEW zcl_llm_00_pat_list( it_ ).
  ENDMETHOD.


  METHOD new_from_fl.
    DATA lt_pt TYPE tt_pt.
    LOOP AT it_ REFERENCE INTO DATA(lr_).
      DATA(lo_pt) = zcl_llm_00_pat=>new_from_file(
                        io_        = lr_->file
                        iv_prefix  = iv_prefix
                        iv_postfix = iv_postfix
                        iv_name    = lr_->name
                    ).
      APPEND VALUE #(
        pat   = lo_pt
        name = lo_pt->get_name( )
      ) TO lt_pt.
    ENDLOOP.
    SORT lt_pt BY name.
    ro_ = zcl_llm_00_pat_list=>new( lt_pt ).
  ENDMETHOD.


  METHOD new_from_folder.
    DATA lv_len TYPE i.
    DATA lt_ TYPE STANDARD TABLE OF text1024.
    cl_gui_frontend_services=>directory_list_files(
      EXPORTING
        directory                   = iv_            " Directory To Search
"       filter                      = '*'
        files_only                  = 'X'
      CHANGING
        file_table                  = lt_
        count                       = lv_len       " Number of Files/Dir Found
      EXCEPTIONS
        cntl_error                  = 1                " Control error
        directory_list_files_failed = 2                " Could not list files in the directory
        wrong_parameter             = 3                " Incorrect parameter combination
        error_no_gui                = 4                " No GUI available
        not_supported_by_gui        = 5                " GUI does not support this
    ).
    IF sy-subrc <> 0 OR lt_ IS INITIAL.
      DATA(lo_mock) = zcl_llm_00_file_mock=>new(
                         iv_content = '{IN}'
                         iv_path    = 'MOCK'
      ).
*      ro_ = zcl_llm_00_ptl=>new_from_fl(  ).
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    IF io_mask IS BOUND.
      DATA(lo_mask) = io_mask.
    ELSE.
      lo_mask = zcl_llm_00_list=>new_from_string( iv_mask ).
    ENDIF.
    DATA(ltr_mask) = lo_mask->r( ).
*--------------------------------------------------------------------*
    DATA: lt_file TYPE tt_file .
    LOOP AT lt_ REFERENCE INTO DATA(lr_) WHERE table_line IN ltr_mask.
      DATA(lo_file) = zcl_llm_00_file_local=>new( CONV #( |{ iv_ }\\{ lr_->* }| ) ).
      APPEND VALUE #(
        file = lo_file
        name = lo_file->get_name( )
      ) TO lt_file.
    ENDLOOP.
    SORT lt_file BY name.
    ro_ = zcl_llm_00_pat_list=>new_from_fl( lt_file ).
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
      ro_ = zcl_llm_00_pat_list=>new_from_fl( VALUE #( ( file = lo_mock name = lo_mock->get_name( ) ) ) ).
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
    ro_ = zcl_llm_00_pat_list=>new_from_fl( lt_file ).
  ENDMETHOD.


  METHOD zif_llm_00_pat_list~filter.
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


  METHOD zif_llm_00_pat_list~get.
    rt_ = mt_.
  ENDMETHOD.


  METHOD zif_llm_00_pat_list~get_by_name.
    ro_ = VALUE #( mt_[ name = iv_ ]-pat OPTIONAL ).
  ENDMETHOD.
ENDCLASS.
