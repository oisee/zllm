CLASS zcl_llm_00_flow_lazy DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_flow_lazy .

    ALIASES collect
      FOR zif_llm_00_step_lazy~collect .
    ALIASES exec
      FOR zif_llm_00_flow_lazy~go .
    ALIASES next
      FOR zif_llm_00_flow_lazy~next .
    ALIASES start
      FOR zif_llm_00_step_lazy~start .
    ALIASES tt_formula
      FOR zif_llm_00_flow_lazy~tt_formula .
    ALIASES tt_pat
      FOR zif_llm_00_flow_lazy~tt_pat .
    ALIASES tt_step
      FOR zif_llm_00_flow_lazy~tt_step .

    CLASS-METHODS new_from_pat_list
      IMPORTING
        !io_pat_list TYPE REF TO zif_llm_00_pat_list
        !io_llm      TYPE REF TO zif_llm_00_llm_lazy
        !iv_model    TYPE string OPTIONAL
        !iv_system   TYPE string
      RETURNING
        VALUE(ro_)   TYPE REF TO zcl_llm_00_flow_lazy .
    CLASS-METHODS new_from_formula_list
      IMPORTING
        !io_formula_list TYPE REF TO zif_llm_00_formula_list
        !io_llm          TYPE REF TO zif_llm_00_llm_lazy
        !iv_model        TYPE string OPTIONAL
      RETURNING
        VALUE(ro_)       TYPE REF TO zcl_llm_00_flow_lazy .
    CLASS-METHODS new
      IMPORTING
        !it_       TYPE tt_step
      RETURNING
        VALUE(ro_) TYPE REF TO zif_llm_00_flow_lazy .
    CLASS-METHODS _debug
      IMPORTING
        !iv_ TYPE sap_bool
        !io_ TYPE REF TO zif_llm_00_trace OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_pat_list TYPE REF TO zif_llm_00_pat_list .
    DATA mo_form_list TYPE REF TO zif_llm_00_formula_list .
    DATA mo_llm TYPE REF TO zif_llm_00_llm_lazy .
    DATA mt_pat TYPE tt_pat .
    DATA mt_form TYPE tt_formula .
    DATA mt_step TYPE tt_step .
    DATA mv_index TYPE i .
    DATA mv_len TYPE i .
    DATA mr_in TYPE REF TO data .
    DATA mr_ TYPE REF TO data .
    DATA mv_ TYPE string .
    DATA mr_out TYPE REF TO data .
    DATA mo_step TYPE REF TO zif_llm_00_step_lazy .
    DATA mv_system TYPE string .
    DATA mv_model TYPE string .
    DATA mo_step_result_in TYPE REF TO zif_llm_00_step_result .
    DATA mo_step_result TYPE REF TO zif_llm_00_step_result .
    DATA mr_res TYPE REF TO data .
    DATA mv_res TYPE string .

    METHODS constructor
      IMPORTING
        !io_pat_list     TYPE REF TO zif_llm_00_pat_list     OPTIONAL
        !io_formula_list TYPE REF TO zif_llm_00_formula_list OPTIONAL
        !io_llm          TYPE REF TO zif_llm_00_llm_lazy     OPTIONAL
        !iv_model        TYPE string OPTIONAL
        !iv_system       TYPE string OPTIONAL
        !it_step         TYPE tt_step OPTIONAL .

    CLASS-DATA gv_debug TYPE sap_bool .
    CLASS-DATA go_trace TYPE REF TO zif_llm_00_trace.

    METHODS _in  IMPORTING ir_   TYPE REF TO data
                           iv_id TYPE string OPTIONAL.
    METHODS _out IMPORTING ir_   TYPE REF TO data
                           iv_id TYPE string OPTIONAL.
ENDCLASS.



CLASS ZCL_LLM_00_FLOW_LAZY IMPLEMENTATION.


  METHOD constructor.

    IF it_step IS SUPPLIED.
      mt_step = it_step.
    ENDIF.

    mo_pat_list  = io_pat_list.
    mo_form_list = io_formula_list.

    mo_llm     = io_llm.
    mv_model   = iv_model.
    mv_system  = iv_system.

    IF mo_pat_list IS BOUND.
      mt_pat = mo_pat_list->get( ).
      LOOP AT mt_pat REFERENCE INTO DATA(lr_pat).
        DATA(lo_step) = zcl_llm_00_step_lazy=>new_from_pat(
*       io_pat_sys = lr_pat->pat
          io_pat_usr = lr_pat->pat
          io_llm     = mo_llm
          iv_model   = mv_model
          iv_system  = iv_system
        ).
        APPEND lo_step TO mt_step.
      ENDLOOP.
    ENDIF.
    IF mo_form_list IS BOUND.
      mt_form = mo_form_list->get( ).
      LOOP AT mt_form REFERENCE INTO DATA(lr_form).
        lo_step = zcl_llm_00_step_lazy=>new_from_formula(
          io_      = lr_form->formula
          io_llm   = mo_llm
          iv_model = mv_model
        ).
        APPEND lo_step TO mt_step.
      ENDLOOP.
    ENDIF.
    mv_len   = lines( mt_step ).
    mv_index = 1.
    mo_step = VALUE #( mt_step[ mv_index ] ).
  ENDMETHOD.


  METHOD new.
    ro_ = NEW zcl_llm_00_flow_lazy(
      it_step = it_
    ).
  ENDMETHOD.


  METHOD new_from_formula_list.
    ro_ = NEW zcl_llm_00_flow_lazy(
      io_formula_list = io_formula_list
      io_llm          = io_llm
      iv_model        = iv_model
    ).
  ENDMETHOD.


  METHOD new_from_pat_list.
    ro_ = NEW zcl_llm_00_flow_lazy(
      io_pat_list = io_pat_list
      io_llm      = io_llm
      iv_model    = iv_model
      iv_system   = iv_system
    ).
  ENDMETHOD.


  METHOD zif_llm_00_flow_lazy~go.
    rr_ = me->zif_llm_00_step_lazy~exec(
      ir_ = ir_
      io_ = io_
    ).
  ENDMETHOD.


  METHOD zif_llm_00_flow_lazy~next.
    IF ir_ IS SUPPLIED AND ir_ IS BOUND.
      mr_in             = ir_.
      mr_               = ir_.
      mo_step_result_in = io_.
    ENDIF.
    mr_ = mo_step->exec( ir_ = mr_ ).
    IF er_ IS REQUESTED.
      er_ = mr_.
    ENDIF.
    IF mv_index >= mv_len.
      mr_out = mr_.
      RETURN.
    ENDIF.
    ADD 1 TO mv_index.
    rv_ = abap_true.
    mo_step = mt_step[ mv_index ].
  ENDMETHOD.


  METHOD zif_llm_00_flow_lazy~to_step.
    ro_ ?= me.
  ENDMETHOD.


  METHOD zif_llm_00_step_lazy~collect.
    IF mr_res IS BOUND.
      rr_ = mr_res.
      RETURN.
    ENDIF.
    mr_res = io_->collect( ).
    rr_ = mr_res.
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
    mr_in = ir_.
    mo_step_result_in = io_.
    DATA(lv_counter) = 0.
    LOOP AT mt_step INTO DATA(lo_step).
      ADD 1 TO lv_counter.
      IF lv_counter = 1.
        mo_step_result = lo_step->start( ir_ = mr_in io_ = io_ ).
        CONTINUE.
      ENDIF.
      mo_step_result = lo_step->start( io_ = mo_step_result ).
    ENDLOOP.
    ro_ = mo_step_result.
    me->_in( ir_ ).
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
      iv_id = 'FLOW_LAZY'
    ).

  ENDMETHOD.


  METHOD _out.
    IF gv_debug NE abap_true OR go_trace IS NOT BOUND.
      RETURN.
    ENDIF.
    DATA(lv_json) = zcl_llm_00_json=>to_json( ir_ ).
    go_trace->out(
      iv_ = lv_json
      iv_id = 'FLOW_LAZY'
    ).

  ENDMETHOD.
ENDCLASS.
