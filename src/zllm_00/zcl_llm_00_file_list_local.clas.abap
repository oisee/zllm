CLASS zcl_llm_00_file_list_local DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_file_list .

    ALIASES tt_file
      FOR zif_llm_00_file_list~tt_file .

    CLASS-METHODS class_constructor .
    CLASS-METHODS new
      IMPORTING
        !it_       TYPE tt_file
        !iv_folder TYPE string OPTIONAL
      RETURNING
        VALUE(ro_) TYPE REF TO zif_llm_00_file_list .
    CLASS-METHODS new_from_folder
      IMPORTING
        !iv_       TYPE string
        !iv_mask   TYPE string DEFAULT '*.*'
        !io_mask   TYPE REF TO zcl_llm_00_list OPTIONAL
          PREFERRED PARAMETER iv_
      RETURNING
        VALUE(ro_) TYPE REF TO zif_llm_00_file_list .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS constructor
      IMPORTING
        it_       TYPE tt_file
        iv_folder TYPE string OPTIONAL.
    DATA: mt_ TYPE tt_file.
    DATA: mv_index TYPE i.
    DATA: mv_len   TYPE i.
    DATA: mv_folder TYPE string.

    CLASS-DATA: gv_default_path TYPE string.

ENDCLASS.



CLASS ZCL_LLM_00_FILE_LIST_LOCAL IMPLEMENTATION.


  METHOD class_constructor.

    gv_default_path = zcl_llm=>get_default_folder( ).

  ENDMETHOD.


  METHOD constructor.
    mv_folder = iv_folder.
    mt_ = it_.
    IF mt_ IS INITIAL.
      RETURN.
    ENDIF.
    mv_index = 1.
    mv_len   = lines( mt_ ).
  ENDMETHOD.


  METHOD new.
    IF iv_folder IS SUPPLIED.
      DATA(lv_folder) = iv_folder.
    ELSE.
      lv_folder = gv_default_path.
    ENDIF.
    ro_ = NEW zcl_llm_00_file_list_local(
      it_ = it_
      iv_folder = lv_folder
    ).
  ENDMETHOD.


  METHOD new_from_folder.
    DATA lv_len TYPE i.
    DATA lt_ TYPE STANDARD TABLE OF text1024.
    cl_gui_frontend_services=>directory_list_files(
      EXPORTING
        directory                   = iv_            " Directory To Search
*       filter                      = iv_mask
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
      ro_ = NEW zcl_llm_00_file_list_local(
        iv_folder = iv_
        it_       = VALUE #(  )
      ).
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
    DATA: lt_file TYPE tt_file.
    LOOP AT lt_ REFERENCE INTO DATA(lr_) WHERE table_line IN ltr_mask.
      DATA(lo_file) = zcl_llm_00_file_local=>new( CONV #( |{ iv_ }\\{ lr_->* }| ) ).
      APPEND VALUE #(
        file = lo_file
        name = lo_file->get_name( )
      ) TO lt_file.
    ENDLOOP.
    SORT lt_file BY name.
    ro_ = NEW zcl_llm_00_file_list_local(
      iv_folder = iv_
      it_       = lt_file
    ).
  ENDMETHOD.


  METHOD zif_llm_00_file_list~filter.
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


  METHOD zif_llm_00_file_list~get.
    rt_ = mt_.
  ENDMETHOD.


  METHOD zif_llm_00_file_list~get_by_name.
    ro_ = VALUE #( mt_[ name = iv_ ]-file OPTIONAL ).
    if ro_ is NOT BOUND.
      ro_ = VALUE #( mt_[ name = to_upper( iv_ ) ]-file OPTIONAL ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_llm_00_file_list~save.
    DATA(lv_name) = io_->get_name( ).
    DATA(lv_path) = mv_folder && lv_name.
    DATA(lv_x)    = io_->get_xstring( ).
    DATA(lv_x_len) = xstrlen( lv_x ).
    DATA(lt_solix) = zcl_llm=>xstring_to_mime( lv_x ).

    cl_gui_frontend_services=>gui_download(
         EXPORTING
           bin_filesize = lv_x_len
           filename     = lv_path
           filetype     = 'BIN'
         CHANGING
           data_tab     = lt_solix
         EXCEPTIONS
           OTHERS       = 1 ).

*--------------------------------------------------------------------*
    " Append the updated file to the list
    DELETE mt_ WHERE name = lv_name.
    APPEND VALUE #( name = lv_name file = io_ ) TO mt_.
  ENDMETHOD.
ENDCLASS.
