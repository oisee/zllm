class ZCL_LLM_00_FUNCTION definition
  public
  final
  create private .

public section.

  interfaces ZIF_LLM_00_TYPES .
  interfaces ZIF_LLM_00_JSON .
  interfaces ZIF_LLM_00_FUNCTION .

  aliases TO_JSON
    for ZIF_LLM_00_JSON~TO_JSON .
  aliases TS_PAR
    for ZIF_LLM_00_FUNCTION~TS_PAR .

  types TS_ type ZIF_LLM_00_TYPES=>TS_FUNCTION .
  types TY_FM type TFDIR-FUNCNAME .  "RS38L_FNAM funcabap .

  class-methods NEW
    importing
      !IV_ type TY_FM
    returning
      value(RO_) type ref to ZCL_LLM_00_FUNCTION
    raising
      ZCX_S .
  methods GET_PARAMETERS
    returning
      value(RS_) type TS_PAR .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_fm TYPE ty_fm .
    DATA mv_functionname TYPE rs38l-name .
    DATA mv_global_flag TYPE rs38l-global .
    DATA mv_remote_call TYPE rs38l-remote .
    DATA mv_update_task TYPE rs38l-utask .
    DATA mv_short_text TYPE tftit-stext .
    DATA mv_function_pool TYPE rs38l-area .
    DATA mv_remote_basxml_supported TYPE rs38l-basxml_enabled .
    DATA mt_new_source TYPE rsfb_source .
    DATA ms_par TYPE ts_par.

    DATA gv_msg TYPE string .

    METHODS constructor
      IMPORTING
        !iv_ TYPE ty_fm
      RAISING
        zcx_s .
    METHODS get_desc
      RETURNING
        VALUE(rv_ok) TYPE sap_bool .

    DATA: ms_ TYPE ts_.
ENDCLASS.



CLASS ZCL_LLM_00_FUNCTION IMPLEMENTATION.


  METHOD constructor.
    mv_fm = iv_.
    IF NOT get_desc( ).
      MESSAGE e000(zcx_) WITH iv_ INTO gv_msg.
      zcx_s=>raise( sy ).
    ENDIF.
  ENDMETHOD.


  METHOD get_desc.

    CALL FUNCTION 'RPY_FUNCTIONMODULE_READ_NEW'
      EXPORTING
        functionname            = mv_fm
      IMPORTING
        global_flag             = mv_global_flag
        remote_call             = mv_remote_call
        update_task             = mv_update_task
        short_text              = mv_short_text
        function_pool           = mv_function_pool
        remote_basxml_supported = mv_remote_basxml_supported
      TABLES
        import_parameter        = ms_par-imp
        changing_parameter      = ms_par-chn
        export_parameter        = ms_par-exp
        tables_parameter        = ms_par-tab
        exception_list          = ms_par-exc
        documentation           = ms_par-doc
        source                  = ms_par-src
*      CHANGING
*       new_source              = mt_new_source
      EXCEPTIONS
        error_message           = 1
        function_not_found      = 2
        invalid_name            = 3.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
    rv_ok = abap_true.
*--------------------------------------------------------------------*


    ms_-name        = mv_fm.
    ms_-description = mv_short_text.
    ms_-parameters ?= zcl_llm_00_parameters=>new( ms_par ).

  ENDMETHOD.


  METHOD new.
    ro_ = NEW zcl_LLM_00_function( iv_ ).
  ENDMETHOD.


  METHOD zif_llm_00_function~invoke.
    rv_ = 'OK'.
  ENDMETHOD.


  METHOD zif_llm_00_json~to_json.
    "rv_ = |[ { zcl_llm_00_json=>to_json( ms_ ) } ]| .
    rv_ = zcl_llm_00_json=>to_json( ms_ ).
  ENDMETHOD.


  METHOD get_parameters.

    rs_ = ms_par.

  ENDMETHOD.
ENDCLASS.
