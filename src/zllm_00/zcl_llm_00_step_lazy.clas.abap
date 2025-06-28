CLASS zcl_llm_00_step_lazy DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_step_lazy .

    CLASS-METHODS new_from_pat
      IMPORTING
        !io_pat_sys     TYPE REF TO zif_llm_00_pat OPTIONAL
        !io_pat_usr     TYPE REF TO zif_llm_00_pat
        !io_llm         TYPE REF TO zif_llm_00_llm_lazy
        !iv_model       TYPE string OPTIONAL
        !iv_system      TYPE string OPTIONAL
        !iv_force_json  TYPE sap_bool OPTIONAL
        !iv_detect_json TYPE sap_bool OPTIONAL
      RETURNING
        VALUE(ro_)      TYPE REF TO zif_llm_00_step_lazy .
    CLASS-METHODS new_from_formula
      IMPORTING
        !io_            TYPE REF TO zif_llm_00_formula
        !io_llm         TYPE REF TO zif_llm_00_llm_lazy
        !iv_model       TYPE string OPTIONAL
        !iv_force_json  TYPE sap_bool OPTIONAL
        !iv_detect_json TYPE sap_bool OPTIONAL
      RETURNING
        VALUE(ro_)      TYPE REF TO zif_llm_00_step_lazy .
    CLASS-METHODS _debug
      IMPORTING
        !iv_ TYPE sap_bool
        !io_ TYPE REF TO zif_llm_00_trace OPTIONAL .
    CLASS-METHODS new_from_string
      IMPORTING
        !iv_sys         TYPE string OPTIONAL
        !iv_usr         TYPE string
        !io_llm         TYPE REF TO zif_llm_00_llm_lazy
        !iv_model       TYPE string OPTIONAL
        !iv_force_json  TYPE sap_bool OPTIONAL
        !iv_detect_json TYPE sap_bool OPTIONAL
      RETURNING
        VALUE(ro_)      TYPE REF TO zif_llm_00_step_lazy .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS constructor
      IMPORTING
        !io_pat_sys     TYPE REF TO zif_llm_00_pat
        !io_pat_usr     TYPE REF TO zif_llm_00_pat
        !io_llm         TYPE REF TO zif_llm_00_llm_lazy
        !iv_model       TYPE string OPTIONAL
        !iv_system      TYPE string OPTIONAL
        !iv_force_json  TYPE sap_bool OPTIONAL
        !iv_detect_json TYPE sap_bool OPTIONAL .
    METHODS detect_json
      IMPORTING
        !iv_       TYPE string
      RETURNING
        VALUE(rv_) TYPE sap_bool .
    CLASS-DATA gv_debug TYPE sap_bool .
    CLASS-DATA go_trace TYPE REF TO zif_llm_00_trace.

    METHODS _in  IMPORTING ir_   TYPE REF TO data
                           iv_id TYPE string OPTIONAL.
    METHODS _out IMPORTING ir_   TYPE REF TO data
                           iv_id TYPE string OPTIONAL.

    DATA mo_pat_sys TYPE REF TO zif_llm_00_pat .
    DATA mo_pat_usr TYPE REF TO zif_llm_00_pat .
    DATA mo_llm TYPE REF TO zif_llm_00_llm_lazy .
*   DATA: mv_json   TYPE string.
    DATA mv_model  TYPE string .
    DATA mv_system TYPE string .
    DATA mv_force_json TYPE sap_bool .
    DATA mv_detect_json TYPE sap_bool .
    DATA mo_payload_adapter TYPE REF TO zif_llm_00_payload_adapter.
ENDCLASS.



CLASS ZCL_LLM_00_STEP_LAZY IMPLEMENTATION.


  METHOD constructor.
    mo_pat_sys         = io_pat_sys.
    mo_pat_usr         = io_pat_usr.
    mo_llm             = io_llm.
    mv_model           = iv_model.
    mv_system          = iv_system.
    mv_force_json      = iv_force_json.
    mv_detect_json     = iv_detect_json.
    mo_payload_adapter = mo_llm->get_payload_adapter( ).
  ENDMETHOD.


  METHOD detect_json.
    IF mv_detect_json NE abap_true.
      RETURN.
    ENDIF.

    FIND FIRST OCCURRENCE OF 'JSON' IN iv_ IGNORING CASE.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    rv_ = abap_true.

  ENDMETHOD.


  METHOD new_from_formula.
    ro_ = NEW zcl_llm_00_step_lazy(
      io_pat_sys     = io_->get_sys( )
      io_pat_usr     = io_->get_usr( )
      io_llm         = io_llm
      iv_model       = iv_model
      iv_force_json  = iv_force_json
      iv_detect_json = iv_detect_json
    ).
  ENDMETHOD.


  METHOD new_from_pat.
    ro_ = NEW zcl_llm_00_step_lazy(
      io_pat_sys     = io_pat_sys
      io_pat_usr     = io_pat_usr
      io_llm         = io_llm
      iv_model       = iv_model
      iv_system      = iv_system
      iv_force_json  = iv_force_json
      iv_detect_json = iv_detect_json
    ).
  ENDMETHOD.


  METHOD zif_llm_00_step_lazy~collect.
    rr_ = io_->collect( ).
    me->_out(
      ir_   = rr_
      iv_id = mo_pat_usr->get_name( )
    ).
  ENDMETHOD.


  METHOD zif_llm_00_step_lazy~exec.
    DATA(lo_) = me->zif_llm_00_step_lazy~start(
      ir_ = ir_
      io_ = io_
    ).
    rr_ = me->zif_llm_00_step_lazy~collect( lo_ ).
  ENDMETHOD.


  METHOD zif_llm_00_step_lazy~start.
    IF io_ IS BOUND.
      DATA(lr_) = io_->collect( ).
    ELSE.
      lr_ = ir_.
    ENDIF.

    DATA(lv_sys_json_detect) = COND #(
      WHEN mo_pat_sys IS BOUND THEN mo_pat_sys->apply( REF #( ' ' ) )
      ELSE ''
    ).
    DATA(lv_usr_json_detect) = mo_pat_usr->apply( REF #( ' ' ) ).
    DATA(lv_json_expected) = xsdbool(
       mv_force_json             = abap_true OR
       me->detect_json( lv_sys_json_detect ) = abap_true OR
       me->detect_json( lv_usr_json_detect ) = abap_true
    ).

    IF mo_pat_sys IS BOUND.
      DATA(lv_sys) = mo_pat_sys->apply( lr_ ).
    ELSE.
      lv_sys = mv_system.
    ENDIF.
    DATA(lv_usr) = mo_pat_usr->apply( lr_ ).
    DATA lt_msg TYPE zif_llm_00_types=>tt_message_in.
    IF lv_sys IS NOT INITIAL.
      APPEND VALUE #( role = 'system' content = lv_sys ) TO lt_msg.
    ENDIF.

    IF lv_json_expected = abap_true.
      APPEND VALUE #( role = 'user' content = 'Please respond in JSON.' ) TO lt_msg.
    ENDIF.
    APPEND VALUE #( role = 'user'   content = lv_usr ) TO lt_msg.


    DATA(lo_in) = mo_payload_adapter->input(
      it_msg  = lt_msg
      iv_json = lv_json_expected
    ).

*  "functions = zcl_llm_00_array=>new( VALUE #( ( zcl_llm_00_function=>new( 'ZOAI_01_TEST_01_PARAMETER' ) ) ) )

    ro_ ?= zcl_llm_00_step_result=>new(
      iv_k             = zcl_llm_00_json=>to_json( lt_msg )
      io_json          = lo_in
      io_llm           = mo_llm
      iv_json_expected = lv_json_expected
    ).
    me->_in(
      ir_   = REF #( lt_msg ) "ls_in
      iv_id = mo_pat_usr->get_name( )
    ).
  ENDMETHOD.


  METHOD _debug.
    gv_debug = iv_.
    go_trace = io_.
  ENDMETHOD.


  METHOD _in.
    IF gv_debug NE abap_true OR go_trace IS NOT BOUND.
      RETURN.
    ENDIF.
    DATA(lv_json) = zcl_llm_00_json=>to_json( ir_ ).
    go_trace->in(
      iv_   = lv_json
      iv_id = iv_id
    ).

  ENDMETHOD.


  METHOD _out.
    IF gv_debug NE abap_true OR go_trace IS NOT BOUND.
      RETURN.
    ENDIF.
    DATA(lv_json) = zcl_llm_00_json=>to_json( ir_ ).
    go_trace->out(
      iv_   = lv_json
      iv_id = iv_id
    ).

  ENDMETHOD.


  METHOD new_from_string.
    ro_ = NEW zcl_llm_00_step_lazy(
      io_pat_sys     = zcl_llm_00_pat=>new( iv_sys )
      io_pat_usr     = zcl_llm_00_pat=>new( iv_usr )
      io_llm         = io_llm
      iv_model       = iv_model
      iv_force_json  = iv_force_json
      iv_detect_json = iv_detect_json
    ).
  ENDMETHOD.
ENDCLASS.
