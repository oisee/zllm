CLASS zcl_llm_00_file_list_smw0 DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_file_list .

    ALIASES tt_file
      FOR zif_llm_00_file_list~tt_file .

    CLASS-METHODS new
      IMPORTING
        !it_          TYPE tt_file
        !iv_package   TYPE string DEFAULT '$ZLLM_03_YAAC'
        !iv_namespace TYPE string OPTIONAL
      RETURNING
        VALUE(ro_)    TYPE REF TO zif_llm_00_file_list .
    CLASS-METHODS new_from_package
      IMPORTING
        !iv_          TYPE string
        !iv_mask      TYPE string DEFAULT '*.*'
        !io_mask      TYPE REF TO zcl_llm_00_list OPTIONAL
        !iv_namespace TYPE string OPTIONAL
        !io_codec     TYPE REF TO zif_llm_00_codec OPTIONAL
          PREFERRED PARAMETER iv_
      RETURNING
        VALUE(ro_)    TYPE REF TO zif_llm_00_file_list
      RAISING
        RESUMABLE(zcx_s) .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_        TYPE tt_file .
    DATA mv_index   TYPE i .
    DATA mv_len     TYPE i .
    DATA mv_ns      TYPE string. "namespace
    DATA mv_ns_len  TYPE string.
    DATA mv_package TYPE string.

    METHODS constructor
      IMPORTING
        !it_          TYPE tt_file
        !iv_package   TYPE string
        !iv_namespace TYPE string .

ENDCLASS.



CLASS ZCL_LLM_00_FILE_LIST_SMW0 IMPLEMENTATION.


  METHOD constructor.
    mv_package = iv_package.
    mt_ = it_.
    IF mt_ IS INITIAL.
      RETURN.
    ENDIF.
    mv_index = 1.
    mv_len   = lines( mt_ ).
    mv_ns    = iv_namespace.
    mv_ns_len = strlen( mv_ns ).
  ENDMETHOD.


  METHOD new.
    ro_ = NEW zcl_llm_00_file_list_smw0(
     it_          = it_
     iv_package   = iv_package
     iv_namespace = iv_namespace
   ).
  ENDMETHOD.


  METHOD new_from_package.

    IF io_mask IS BOUND.
      DATA(lo_mask) = io_mask.
    ELSE.
      lo_mask = zcl_llm_00_list=>new_from_string( to_upper( iv_mask ) ).
    ENDIF.
    DATA(ltr_mask) = lo_mask->r( ).

    SELECT a~objid AS obj_name
      FROM wwwdata AS a INNER JOIN tadir AS b
        ON a~objid = b~obj_name
      INTO TABLE @DATA(lt_)
      WHERE "f~text    IN so_text
            a~srtf2    = 0
        AND a~relid    = 'MI'
        AND b~pgmid    = 'R3TR'
        AND b~object   = 'W3MI'
        AND b~devclass = @iv_
        AND b~obj_name IN @ltr_mask.
* SORT wwwdata_tab BY devclass objid ASCENDING.
* SELECT * FROM tadir WHERE devclass =  @iv_ AND object   =  'W3MI' AND obj_name IN @ltr_mask      INTO TABLE @DATA(lt_).

    IF lt_ IS INITIAL.
      ro_ = NEW zcl_llm_00_file_list_smw0(
        it_          = VALUE #( )
        iv_package   = iv_
        iv_namespace = iv_namespace
      ).
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lt_file TYPE tt_file.
    LOOP AT lt_ REFERENCE INTO DATA(lr_).
      DATA(lo_file) = zcl_llm_00_file_smw0=>new(
                        iv_      = CONV #( lr_->obj_name )
                        io_codec = io_codec
                      ).
      APPEND VALUE #(
        file = lo_file
        name = lo_file->get_name( )
      ) TO lt_file.
    ENDLOOP.
    SORT lt_file BY name.
    ro_ = NEW zcl_llm_00_file_list_smw0(
      it_          = lt_file
      iv_package   = iv_
      iv_namespace = iv_namespace
    ).

  ENDMETHOD.


  METHOD zif_llm_00_file_list~filter.
    IF io_ IS NOT BOUND.
      DATA(lo_list) = zcl_llm_00_list=>new_from_string( iv_ ).
    ELSE.
      lo_list = io_.
    ENDIF.
    DATA(ltr_) = lo_list->r( ).
    DATA(lt_) = mt_.
    IF mv_ns IS NOT INITIAL.
      LOOP AT lt_ REFERENCE INTO DATA(lr_).
        lr_->name = to_upper( lr_->name+mv_ns_len ).
      ENDLOOP.
      LOOP AT lt_ REFERENCE INTO lr_ WHERE name IN ltr_.
        APPEND lr_->* TO rt_.
      ENDLOOP.
      RETURN.
    ELSE.
      LOOP AT mt_ REFERENCE INTO lr_ WHERE name IN ltr_.
        APPEND lr_->* TO rt_.
      ENDLOOP.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD zif_llm_00_file_list~get.
    rt_ = mt_.
  ENDMETHOD.


  METHOD zif_llm_00_file_list~get_by_name.
    DATA(lv_name) = to_upper( mv_ns && iv_ ).
    ro_ = VALUE #( mt_[ name = lv_name ]-file OPTIONAL ).
  ENDMETHOD.


  METHOD zif_llm_00_file_list~save.
    DATA(lv_name) = io_->get_name( ).
    DATA(lv_text) = CONV text255( io_->get_name( ) ).
    DATA(lv_x)    = io_->get_xstring( ).
    DATA(lo_codec) = zcl_llm_00_codec=>new( ).
    lv_x = lo_codec->encode( lv_x ).
    DATA(lv_len)  = xstrlen( lv_x ).
*--------------------------------------------------------------------*
    DATA(lt_mime) = zcl_llm=>xstring_to_mime( lv_x ).
*--------------------------------------------------------------------*
    DATA(ls_key) = VALUE wwwdatatab(
      objid    = mv_ns && to_upper( lv_name )
      relid    = 'MI'
      chname   = sy-uname
      devclass = mv_package
    ).

    DATA lt_html TYPE STANDARD TABLE OF w3html.
    CALL FUNCTION 'EXPORT_WEB_OBJECT'
      EXPORTING
        object_id          = ls_key-objid
        mimetype           = 'text/markdown'
        text               = lv_text
        size               = lv_len
        devclass           = ls_key-devclass
      TABLES
        html               = lt_html
        mime               = lt_mime
      EXCEPTIONS
        size_not_specified = 1
        object_locked      = 2.

    COMMIT WORK.
*--------------------------------------------------------------------*
    " Append the updated file to the list
    DELETE mt_ WHERE name = lv_name.
    lv_name = to_upper( lv_name ).
    DELETE mt_ WHERE name = lv_name.
    APPEND VALUE #( name = mv_ns && lv_name file = io_ ) TO mt_.
  ENDMETHOD.
ENDCLASS.
