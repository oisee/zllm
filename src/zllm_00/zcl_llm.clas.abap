CLASS zcl_llm DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES string_t TYPE zif_llm_00_types=>string_t .

    CLASS-METHODS class_constructor .
    CLASS-METHODS guid
      RETURNING
        VALUE(rv_) TYPE guid .
    CLASS-METHODS tab_to_string
      IMPORTING
        !it_       TYPE string_t
      RETURNING
        VALUE(rv_) TYPE string .
    CLASS-METHODS string_to_tab
      IMPORTING
        !iv_       TYPE string
      RETURNING
        VALUE(rt_) TYPE string_t .
    CLASS-METHODS predict_tokens_for_string
      IMPORTING
        !iv_         TYPE string
        !io_         TYPE REF TO zcl_llm_00_predictoken OPTIONAL
        !iv_llm_type TYPE string DEFAULT 'GPT'
      RETURNING
        VALUE(rv_)   TYPE i .
    CLASS-METHODS predict_tokens_for_tab
      IMPORTING
        !it_           TYPE string_t
        !io_           TYPE REF TO zcl_llm_00_predictoken OPTIONAL
        !iv_model_type TYPE string DEFAULT 'GPT'
      RETURNING
        VALUE(rv_)     TYPE i .
    CLASS-METHODS string_hash
      IMPORTING
        !iv_       TYPE string
      RETURNING
        VALUE(rv_) TYPE hash160 .
    CLASS-METHODS string_to_xstring
      IMPORTING
        !iv_       TYPE string
      RETURNING
        VALUE(rv_) TYPE xstring .
    CLASS-METHODS xstring_to_mime
      IMPORTING
        !iv_       TYPE xstring
      RETURNING
        VALUE(rt_) TYPE w3mimetabtype .
    CLASS-METHODS xstring_to_string
      IMPORTING
        !iv_       TYPE xstring
      RETURNING
        VALUE(rv_) TYPE string .
    CLASS-METHODS xstring_hash
      IMPORTING
        !iv_       TYPE xstring
      RETURNING
        VALUE(rv_) TYPE hash160 .
    CLASS-METHODS tstring_to_xstring
      IMPORTING
        !it_       TYPE string_t
      RETURNING
        VALUE(rv_) TYPE xstring .
    CLASS-METHODS tstring_hash
      IMPORTING
        !it_       TYPE stringtab OPTIONAL
      RETURNING
        VALUE(rv_) TYPE hash160 .
    CLASS-METHODS get_default_folder
      RETURNING
        VALUE(rv_) TYPE string .
    CLASS-METHODS rand_int
      IMPORTING
        !iv_       TYPE i DEFAULT 100
      RETURNING
        VALUE(rv_) TYPE i .

    CLASS-METHODS new
      IMPORTING
        !iv_       TYPE string DEFAULT 'DEFAULT.ENV'
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm
      RAISING
        zcx_s .
    METHODS get_llm
      IMPORTING
        !iv_          TYPE string OPTIONAL
      RETURNING
        VALUE(ro_llm) TYPE REF TO zif_llm_00_llm_lazy
      RAISING
        zcx_s .
    METHODS get_step
      IMPORTING
        !iv_           TYPE string
        !io_llm        TYPE REF TO zif_llm_00_llm_lazy OPTIONAL
      RETURNING
        VALUE(ro_step) TYPE REF TO zif_llm_00_step_lazy
      RAISING
        zcx_s .
    METHODS get_flow
      IMPORTING
        !it_           TYPE stringtab
      EXPORTING
        !eo_flow       TYPE REF TO zif_llm_00_flow_lazy
      RETURNING
        VALUE(ro_step) TYPE REF TO zif_llm_00_step_lazy
      RAISING
        zcx_s .
    METHODS get_step_parallel
      IMPORTING
        !iv_           TYPE string
      RETURNING
        VALUE(ro_step) TYPE REF TO zif_llm_00_step_lazy
      RAISING
        zcx_s .
    METHODS get_pat
      IMPORTING
        !iv_          TYPE string
      RETURNING
        VALUE(ro_pat) TYPE REF TO zif_llm_00_pat
      RAISING
        zcx_s .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS get_predictoken
      IMPORTING
        !iv_       TYPE string DEFAULT 'GPT'
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_predictoken .

    CLASS-DATA: go_predictoken TYPE REF TO zcl_llm_00_predictoken.
    CLASS-DATA gv_bin TYPE string .
    CLASS-DATA go_fl TYPE REF TO zif_llm_00_file_list .
    CLASS-DATA go_cache TYPE REF TO zif_llm_00_cache .
    CLASS-DATA go_llm TYPE REF TO zif_llm_00_llm_lazy .
    CLASS-DATA go_llm_4o TYPE REF TO zif_llm_00_llm_lazy .
    CLASS-DATA go_llm_41 TYPE REF TO zif_llm_00_llm_lazy .
    CLASS-DATA go_llm_4ob TYPE REF TO zif_llm_00_llm_lazy .
    CLASS-DATA go_llm_41b TYPE REF TO zif_llm_00_llm_lazy .
    DATA mo_fl TYPE REF TO zif_llm_00_file_list .
    DATA mo_cache TYPE REF TO zif_llm_00_cache .
    DATA mo_llm TYPE REF TO zif_llm_00_llm_lazy .
    DATA mo_step TYPE REF TO zif_llm_00_step_lazy .
    DATA mo_flow TYPE REF TO zif_llm_00_flow_lazy .
    DATA mo_formula TYPE REF TO zif_llm_00_formula .
    DATA:
      mt_step TYPE STANDARD TABLE OF REF TO zif_llm_00_step_lazy WITH DEFAULT KEY .

    METHODS constructor
      IMPORTING
        !iv_ TYPE string
      RAISING
        zcx_s .
ENDCLASS.



CLASS ZCL_LLM IMPLEMENTATION.


  METHOD get_default_folder.

    cl_gui_frontend_services=>get_platform(
      RECEIVING
        platform             = DATA(lv_os)
      EXCEPTIONS
        error_no_gui         = 1                " No GUI available
        cntl_error           = 2                " Control error
        not_supported_by_gui = 3                " GUI does not support this
        OTHERS               = 4
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    "@TODO: replace with STVARV
    CASE lv_os.
      WHEN cl_gui_frontend_services=>platform_windowsxp.
        rv_ = '%USERPROFILE%\'.
        "rv_ = 'C:\Temp\.env'.
      WHEN cl_gui_frontend_services=>platform_linux.
        rv_ = '$HOME/'.
      WHEN cl_gui_frontend_services=>platform_macosx OR
           cl_gui_frontend_services=>platform_mac.
        rv_ = '$HOME/'.
      WHEN OTHERS.
        rv_ = '$HOME/'.
    ENDCASE.

  ENDMETHOD.


  METHOD get_predictoken.
    IF go_predictoken IS NOT BOUND.
      go_predictoken = zcl_llm_00_predictoken=>new_for_model_type( iv_ ).
    ENDIF.
    ro_ = go_predictoken.
  ENDMETHOD.


  METHOD predict_tokens_for_string.
    DATA: lo_ LIKE io_.
    IF io_ IS NOT BOUND.
      lo_ = get_predictoken( iv_llm_type ).
    ELSE.
      lo_ = io_.
    ENDIF.
    rv_ = lo_->predict( iv_ ).

  ENDMETHOD.


  METHOD predict_tokens_for_tab.
    DATA: lo_ LIKE io_.
    IF io_ IS NOT BOUND.
      lo_ = get_predictoken( iv_model_type ).
    ELSE.
      lo_ = io_.
    ENDIF.
    DATA(lv_) = tab_to_string( it_ ).
    rv_ = lo_->predict( lv_ ).

  ENDMETHOD.


  METHOD string_hash.

    CALL FUNCTION 'CALCULATE_HASH_FOR_CHAR'
      EXPORTING
        data   = iv_
      IMPORTING
        hash   = rv_
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD string_to_tab.
    DATA(lv_) = iv_.
    REPLACE ALL OCCURRENCES OF zif_llm=>cr_lf IN lv_ WITH zif_llm=>n.
    SPLIT lv_ AT zif_llm=>n INTO TABLE rt_.
  ENDMETHOD.


  METHOD string_to_xstring.
    rv_ = cl_abap_codepage=>convert_to( source = iv_ codepage = `UTF-8` ).
  ENDMETHOD.


  METHOD tab_to_string.
    LOOP AT it_ REFERENCE INTO DATA(lr_).
      IF sy-tabix = 1.
        rv_ = lr_->*.
        CONTINUE.
      ENDIF.
      rv_ = rv_ && zif_llm=>n && lr_->*.
    ENDLOOP.
  ENDMETHOD.


  METHOD tstring_hash.
    DATA lv_ TYPE string.
    CONCATENATE LINES OF it_ INTO lv_ SEPARATED BY zif_llm=>n.

    CALL FUNCTION 'CALCULATE_HASH_FOR_CHAR'
      EXPORTING
        data   = lv_
      IMPORTING
        hash   = rv_
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD tstring_to_xstring.
    rv_ = cl_abap_codepage=>convert_to( source = tab_to_string( it_ ) codepage = `UTF-8` ).
  ENDMETHOD.


  METHOD xstring_hash.
    CALL FUNCTION 'CALCULATE_HASH_FOR_RAW'
      EXPORTING
        data   = iv_
      IMPORTING
        hash   = rv_
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD xstring_to_mime.
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer          = iv_
        append_to_table = ' '
*     IMPORTING
*       output_length   = output_length
      TABLES
        binary_tab      = rt_.
  ENDMETHOD.


  METHOD xstring_to_string.
    DATA: lo_conv  TYPE REF TO cl_abap_conv_in_ce.
    lo_conv = cl_abap_conv_in_ce=>create(
      "encoding = '4110'
      input = iv_
    ).
    lo_conv->read(
      IMPORTING
        data = rv_ ).

  ENDMETHOD.


  METHOD guid.
    TRY.
        rv_ = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error INTO DATA(lx_).
        GET TIME STAMP FIELD DATA(lv_).
        rv_ = lv_.
    ENDTRY.
  ENDMETHOD.


  METHOD class_constructor.
  ENDMETHOD.


  METHOD rand_int.
    rv_ = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( )
      min  = 0
      max  = iv_ )->get_next( ).

  ENDMETHOD.


  METHOD constructor.
    IF go_llm IS BOUND AND go_fl IS BOUND AND go_cache IS BOUND.
      mo_llm   = go_llm.
      mo_fl    = go_fl.
      mo_cache = go_cache.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA(lo_codec) = zcl_llm_00_codec=>new( ). "default personal codec
    gv_bin = `$ZLLM_` && sy-uname.
    SELECT SINGLE *
      FROM zllm_00_bin
      WHERE bin = @gv_bin
      INTO @DATA(ls_bin).

    IF sy-subrc NE 0.
      gv_bin = `$ZLLM`.
      SELECT SINGLE *
        FROM zllm_00_bin
        WHERE bin = @gv_bin
        INTO @ls_bin.

      lo_codec = zcl_llm_00_codec=>new( `` ). "default generic codec
      IF sy-subrc NE 0.
        RETURN.
      ENDIF.
    ENDIF.
*--------------------------------------------------------------------*
    TRY.
        go_fl = zcl_llm_00_file_list_bin=>new_from_bin( "~ Initialize the file list component using the zcl_llm_00_file_list_bin class with the retrieved binary data.
          iv_bin  = gv_bin
          iv_mask = '*.env;*.md'
        ).
        go_cache = zcl_llm_00_cache=>new( ).                   "~ Initialize the cache component using the zcl_llm_00_cache class for efficient data retrieval.
        go_llm = zcl_llm_00_llm_lazy=>new_from_file( "~ Initialize the LLM component using the zcl_llm_00_llm_lazy class with the specified file and cache.
          io_      = go_fl->get_by_name( iv_ )
          io_cache = go_cache
        ).

*        go_llm_4o = zcl_llm_00_llm_lazy=>new_from_file( "~ Initialize the LLM component using the zcl_llm_00_llm_lazy class with the specified file and cache.
*          io_      = go_fl->get_by_name( 'azure-gpt40.env' )
*          io_cache = go_cache
*        ).
**        go_llm_41 = zcl_llm_00_llm_lazy=>new_from_file( "~ Initialize the LLM component using the zcl_llm_00_llm_lazy class with the specified file and cache.
**          io_      = go_fl->get_by_name( 'azure-gpt41.env' )
**          io_cache = go_cache
**        ).
*        go_llm_4ob = zcl_llm_00_llm_lazy=>new_from_file( "~ Initialize the LLM component using the zcl_llm_00_llm_lazy class with the specified file and cache.
*          io_      = go_fl->get_by_name( 'azure-gpt40b.env' )
*          io_cache = go_cache
*        ).
**        go_llm_41b = zcl_llm_00_llm_lazy=>new_from_file( "~ Initialize the LLM component using the zcl_llm_00_llm_lazy class with the specified file and cache.
**          io_      = go_fl->get_by_name( 'azure-gpt41b.env' )
**          io_cache = go_cache
**        ).
*
*        go_llm = zcl_llm_00_llm_lazy_balancer=>new(
*          VALUE #(
*          ( go_llm_4o )
**          ( go_llm_41 )
*          ( go_llm_4ob )
**          ( go_llm_41b )
*        )
*        ).

      CATCH zcx_s INTO DATA(lcx_s).
        MESSAGE e000(zcx_) WITH 'Error Getting API Keys' INTO DATA(lv_msg).
        zcx_s=>raise( sy ).
    ENDTRY.

    mo_llm   = go_llm.
    mo_fl    = go_fl.
    mo_cache = go_cache.

  ENDMETHOD.


  METHOD get_flow.

    CLEAR mt_step.
    LOOP AT it_ REFERENCE INTO DATA(lr_) WHERE table_line IS NOT INITIAL.
      DATA(lo_formula) = zcl_llm_00_formula=>new_from_name(
        io_fl   = mo_fl
        iv_name = lr_->*
      ).
      DATA(lo_step) = zcl_llm_00_step_lazy=>new_from_formula(
        io_    = lo_formula
        io_llm = mo_llm
      ).
      APPEND lo_step TO mt_step.
    ENDLOOP.
*--------------------------------------------------------------------*
    mo_flow = zcl_llm_00_flow_lazy=>new(
      mt_step
      ).

    ro_step ?= mo_flow.
    IF eo_flow IS REQUESTED.
      eo_flow  = mo_flow.
    ENDIF.

  ENDMETHOD.


  METHOD get_llm.
    IF iv_ IS SUPPLIED AND iv_ IS NOT INITIAL.
      ro_llm = zcl_llm_00_llm_lazy=>new_from_file( "~ Initialize the LLM component using the zcl_llm_00_llm_lazy class with the specified file and cache.
        io_      = mo_fl->get_by_name( iv_ )
        io_cache = mo_cache
      ).
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ro_llm = mo_llm.
  ENDMETHOD.


  METHOD get_step.

    mo_formula = zcl_llm_00_formula=>new_from_name( "~ Initialize the formula component using the zcl_llm_00_formula class with the specified name.
      io_fl   = mo_fl
      iv_name = iv_
    ).

    IF io_llm IS SUPPLIED AND io_llm IS BOUND.
      mo_step = zcl_llm_00_step_lazy=>new_from_formula( "~ Initialize the step component using the zcl_llm_00_step_lazy class with the formula and LLM components.
        io_            = mo_formula
        io_llm         = io_llm
        iv_detect_json = 'X'
      ).
    ELSE.
      mo_step = zcl_llm_00_step_lazy=>new_from_formula( "~ Initialize the step component using the zcl_llm_00_step_lazy class with the formula and LLM components.
        io_            = mo_formula
        io_llm         = mo_llm
        iv_detect_json = 'X'
      ).
    ENDIF.

    " this flow is from one step
    mo_flow = zcl_llm_00_flow_lazy=>new( VALUE #(          "~ Initialize the flow component using the zcl_llm_00_flow_lazy class with the specified step.
      ( mo_step )
    ) ).

    ro_step ?= mo_flow.

  ENDMETHOD.


  METHOD new.
    ro_ = NEW zcl_llm( iv_ ).
  ENDMETHOD.


  METHOD get_step_parallel.

    mo_formula = zcl_llm_00_formula=>new_from_name( "~ Initialize the formula component using the zcl_llm_00_formula class with the specified name.
      io_fl   = mo_fl
      iv_name = iv_
    ).

    mo_step = zcl_llm_00_step_lazy_parallel=>new_from_formula( "~ Initialize the step component using the zcl_llm_00_step_lazy class with the formula and LLM components.
      io_    = mo_formula
      io_llm = mo_llm
    ).

    ro_step ?= mo_step.

  ENDMETHOD.


  METHOD get_pat.

    ro_pat = zcl_llm_00_pat=>new_from_name(
      io_fl   = mo_fl
      iv_name = iv_
    ).

  ENDMETHOD.
ENDCLASS.
