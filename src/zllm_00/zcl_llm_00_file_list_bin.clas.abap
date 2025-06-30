CLASS zcl_llm_00_file_list_bin DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_file_list .

    ALIASES tt_file
      FOR zif_llm_00_file_list~tt_file .

    CLASS-METHODS new
      IMPORTING
        !iv_bin       TYPE string
        !it_          TYPE tt_file
        !iv_namespace TYPE string OPTIONAL
      RETURNING
        VALUE(ro_)    TYPE REF TO zif_llm_00_file_list .
    CLASS-METHODS new_from_bin
      IMPORTING
        !iv_bin    TYPE string
        !iv_mask   TYPE string DEFAULT '*.*'
        !io_mask   TYPE REF TO zcl_llm_00_list OPTIONAL
        !io_codec  TYPE REF TO zif_llm_00_codec OPTIONAL
      RETURNING
        VALUE(ro_) TYPE REF TO zif_llm_00_file_list
      RAISING
        RESUMABLE(zcx_s) .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_ TYPE tt_file .
    DATA mv_bin TYPE string.
    METHODS constructor
      IMPORTING
        !iv_bin TYPE string
        !it_    TYPE tt_file
      .
    METHODS commit.
ENDCLASS.



CLASS ZCL_LLM_00_FILE_LIST_BIN IMPLEMENTATION.


  METHOD commit.
    COMMIT WORK.
  ENDMETHOD.


  METHOD constructor.
    mv_bin = iv_bin.
    mt_ = it_.
  ENDMETHOD.


  METHOD new.
    ro_ = NEW zcl_llm_00_file_list_bin(
     iv_bin       = iv_bin
     it_          = it_
   ).
  ENDMETHOD.


  METHOD new_from_bin.

    IF io_mask IS BOUND.
      DATA(lo_mask) = io_mask.
    ELSE.
      lo_mask = zcl_llm_00_list=>new_from_string( to_upper( iv_mask ) ).
    ENDIF.
    DATA(ltr_mask) = lo_mask->r( ).

    SELECT *
      FROM zllm_00_bin
      INTO TABLE @DATA(lt_)
      WHERE bin        = @iv_bin
        AND name IN @ltr_mask
      ORDER BY bin, name.

    IF lt_ IS INITIAL.
      ro_ = NEW zcl_llm_00_file_list_bin(
        iv_bin       = iv_bin
        it_          = VALUE #( )
      ).
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lt_file TYPE tt_file.
    LOOP AT lt_ REFERENCE INTO DATA(lr_).
      DATA(lo_file) = zcl_llm_00_file_bin=>new(
                        iv_bin   = iv_bin
                        iv_name  = CONV #( lr_->name )
                        io_codec = io_codec
                      ).
      APPEND VALUE #(
        file = lo_file
        name = lo_file->get_name( )
      ) TO lt_file.
    ENDLOOP.
    SORT lt_file BY name.
    ro_ = NEW zcl_llm_00_file_list_bin(
      iv_bin       = iv_bin
      it_          = lt_file
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
    LOOP AT mt_ REFERENCE INTO DATA(lr_) WHERE name IN ltr_.
      APPEND lr_->* TO rt_.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_llm_00_file_list~get.
    rt_ = mt_.
  ENDMETHOD.


  METHOD zif_llm_00_file_list~get_by_name.
    DATA(lv_name) = to_upper( iv_ ).
    ro_ = VALUE #( mt_[ name = lv_name ]-file OPTIONAL ).
  ENDMETHOD.


  METHOD zif_llm_00_file_list~save.
    DATA(ls_) = VALUE zllm_00_bin(
        bin = mv_bin
        name = io_->get_name( )
        v    = io_->get_xstring( )
        cdate = sy-datum
    ).

    DATA(lo_codec) = zcl_llm_00_codec=>new( ).
    ls_-v = lo_codec->encode( ls_-v ).
*--------------------------------------------------------------------*
    MODIFY zllm_00_bin FROM @ls_.
    me->commit( ).
*--------------------------------------------------------------------*
    " Append the updated file to the list
    DELETE mt_ WHERE name = ls_-name.
    APPEND VALUE #( name = ls_-name file = io_ ) TO mt_.
  ENDMETHOD.
ENDCLASS.
