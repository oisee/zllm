CLASS zcl_llm_00_step_lazy_parallel DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_step_lazy .

    ALIASES exec
      FOR zif_llm_00_step_lazy~exec .
    ALIASES start
      FOR zif_llm_00_step_lazy~start .
    ALIASES yield
      FOR zif_llm_00_step_lazy~collect .

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
        !iv_model       TYPE string   OPTIONAL
        !iv_force_json  TYPE sap_bool OPTIONAL
        !iv_detect_json TYPE sap_bool OPTIONAL
      RETURNING
        VALUE(ro_)      TYPE REF TO zif_llm_00_step_lazy .
    CLASS-METHODS _debug
      IMPORTING
        !iv_ TYPE sap_bool
        !io_ TYPE REF TO zif_llm_00_trace OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_pat_sys TYPE REF TO zif_llm_00_pat .
    DATA mo_pat_usr TYPE REF TO zif_llm_00_pat .
    DATA mo_llm TYPE REF TO zif_llm_00_llm_lazy .
*   DATA: mv_json   TYPE string.
    DATA mv_model TYPE string .
    DATA mv_system TYPE string .
*    DATA: mv_res TYPE string.
*    DATA: mr_res TYPE REF TO data.
    DATA mv_force_json TYPE sap_bool .
    DATA mv_detect_json TYPE sap_bool .

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
    METHODS start_for_table
      IMPORTING
        !it_       TYPE ANY TABLE
      RETURNING
        VALUE(ro_) TYPE REF TO zif_llm_00_step_result
      RAISING
        zcx_s .
    METHODS start_for_scalar
      IMPORTING
        !ir_       TYPE REF TO data
      RETURNING
        VALUE(ro_) TYPE REF TO zif_llm_00_step_result
      RAISING
        zcx_s .

    CLASS-DATA gv_debug TYPE sap_bool .
    CLASS-DATA go_trace TYPE REF TO zif_llm_00_trace.

    METHODS _in  IMPORTING ir_   TYPE REF TO data
                           iv_id TYPE string OPTIONAL.
    METHODS _out IMPORTING ir_   TYPE REF TO data
                           iv_id TYPE string OPTIONAL.
ENDCLASS.



CLASS ZCL_LLM_00_STEP_LAZY_PARALLEL IMPLEMENTATION.


  METHOD constructor.
    mo_pat_sys    = io_pat_sys.
    mo_pat_usr    = io_pat_usr.
    mo_llm        = io_llm.
    mv_model      = iv_model.
    mv_system     = iv_system.
    mv_force_json = iv_force_json.
    mv_detect_json = iv_detect_json.
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
    ro_ = NEW zcl_llm_00_step_lazy_parallel(
      io_pat_sys    = io_->get_sys( )
      io_pat_usr    = io_->get_usr( )
      io_llm        = io_llm
      iv_model      = iv_model
      iv_force_json = iv_force_json
      iv_detect_json = iv_detect_json
    ).
  ENDMETHOD.


  METHOD new_from_pat.
    ro_ = NEW zcl_llm_00_step_lazy_parallel(
      io_pat_sys    = io_pat_sys
      io_pat_usr    = io_pat_usr
      io_llm        = io_llm
      iv_model      = iv_model
      iv_system     = iv_system
      iv_force_json = iv_force_json
      iv_detect_json = iv_detect_json
    ).
  ENDMETHOD.


  METHOD start_for_scalar.
*    DATA: lt_step_res TYPE zcl_llm_00_flow_result=>tt_.
*    LOOP AT mt_step INTO DATA(lo_step).
*      APPEND lo_step->start( ir_ ) TO lt_step_res.
*    ENDLOOP.
*    ro_ = zcl_llm_00_flow_result=>new( lt_step_res ).
    IF mo_pat_sys IS BOUND.
      DATA(lv_sys) = mo_pat_sys->apply( ir_ ).
    ELSE.
      lv_sys = mv_system.
    ENDIF.
    DATA(lv_usr) = mo_pat_usr->apply( ir_ ).
    DATA lt_msg TYPE zcl_llm_00_chat_in=>zif_llm_00_types~tt_message_in.
    IF lv_sys IS NOT INITIAL.
      APPEND VALUE #( role = 'system' content = lv_sys ) TO lt_msg.
    ENDIF.

    DATA(lv_json_expected) = xsdbool(
      mv_force_json             = abap_true OR
      me->detect_json( lv_sys ) = abap_true OR
      me->detect_json( lv_usr ) = abap_true
    ).

    IF lv_json_expected = abap_true.
      APPEND VALUE #( role = 'user' content = 'Please respond in JSON.' ) TO lt_msg.
    ENDIF.
    APPEND VALUE #( role = 'user'   content = lv_usr ) TO lt_msg.

    DATA(ls_in) = VALUE zif_llm_00_types=>ts_chat_in(
      model    = COND #( WHEN mv_model IS INITIAL THEN mo_llm->get_config( )-model_name
                         ELSE mv_model )
      messages = lt_msg
    ).
    IF lv_json_expected = abap_true.
      ls_in-response_format = zcl_llm_00_kv=>new( VALUE #( ( k = 'type' v = 'json_object' ) ) ).
    ELSE.
    ENDIF.
    DATA(lo_in) = zcl_llm_00_chat_in=>new( ls_in ).
*  "functions = zcl_llm_00_array=>new( VALUE #( ( zcl_llm_00_function=>new( 'ZOAI_01_TEST_01_PARAMETER' ) ) ) )
*) ).
*    ro_ ?= zcl_llm_00_step_result=>new(
*      io_http          = mo_llm->send( lo_in )
*      io_llm           = mo_llm
*      iv_json_expected = lv_json_expected
*    ).
    ro_ ?= zcl_llm_00_step_result=>new(
      iv_k    = zcl_llm_00_json=>to_json( ls_in-messages )
      io_json = lo_in
      io_llm  = mo_llm
      iv_json_expected = lv_json_expected
    ).
  ENDMETHOD.


  METHOD start_for_table.
    DATA: lt_step_res TYPE zcl_llm_00_flow_result=>tt_.
    FIELD-SYMBOLS: <fs_l> TYPE any.       "line
    LOOP AT it_ ASSIGNING <fs_l>.
      APPEND me->start_for_scalar( REF #( <fs_l> ) ) TO lt_step_res.
    ENDLOOP.
    ro_ = zcl_llm_00_flow_result=>new( lt_step_res ).
  ENDMETHOD.


  METHOD zif_llm_00_step_lazy~collect.
    rr_ = io_->collect( ).
    me->_out( rr_ ).
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

    FIELD-SYMBOLS: <fs_f> TYPE any.
    IF lr_ IS NOT BOUND.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ASSIGN lr_->* TO <fs_f>.
    IF <fs_f> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lo_td TYPE REF TO cl_abap_typedescr.
    lo_td ?= cl_abap_typedescr=>describe_by_data( <fs_f> ).
    CASE lo_td->kind.
      WHEN cl_abap_typedescr=>kind_table.
        ro_ = me->start_for_table( <fs_f> ).
        me->_in( lr_ ).
        RETURN.
      WHEN cl_abap_typedescr=>kind_ref.
        ro_ = me->start( lr_ ).
        RETURN.
      WHEN OTHERS.
        ro_ = me->start_for_scalar( lr_ ).
        me->_in( lr_ ).
        RETURN.
    ENDCASE.
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
      iv_ = lv_json
      iv_id = 'STEP_LAZY_PARALLEL'
    ).

  ENDMETHOD.


  METHOD _out.
    IF gv_debug NE abap_true OR go_trace IS NOT BOUND.
      RETURN.
    ENDIF.
    DATA(lv_json) = zcl_llm_00_json=>to_json( ir_ ).
    go_trace->out(
      iv_ = lv_json
      iv_id = 'STEP_LAZY_PARALLEL'
    ).

  ENDMETHOD.
ENDCLASS.
