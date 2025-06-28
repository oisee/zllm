class ZCL_LLM_00_STEP_LP_SPLIT definition
  public
  final
  create private .

public section.

  interfaces ZIF_LLM_00_STEP_LAZY .

  aliases EXEC
    for ZIF_LLM_00_STEP_LAZY~EXEC .
  aliases START
    for ZIF_LLM_00_STEP_LAZY~START .
  aliases YIELD
    for ZIF_LLM_00_STEP_LAZY~COLLECT .

  class-methods NEW
    importing
      !IO_LLM type ref to ZIF_LLM_00_LLM_LAZY
    returning
      value(RO_) type ref to ZIF_LLM_00_STEP_LAZY .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS constructor
      IMPORTING
        !io_llm TYPE REF TO zif_llm_00_llm_lazy.

    DATA: mr_in      TYPE string.
    DATA: mo_step_res TYPE REF TO zif_llm_00_step_result .
    DATA: mo_string   TYPE REF TO zif_llm_00_string .
    DATA: mo_llm TYPE REF TO zif_llm_00_llm_lazy.
ENDCLASS.



CLASS ZCL_LLM_00_STEP_LP_SPLIT IMPLEMENTATION.


  METHOD constructor.
    mo_llm = io_llm.
  ENDMETHOD.


  METHOD new.
    ro_ ?= NEW zcl_llm_00_step_lp_split(
       io_llm = io_llm
    ).
  ENDMETHOD.


  METHOD zif_llm_00_step_lazy~collect.
    rr_ = io_->collect( ).
  ENDMETHOD.


  METHOD zif_llm_00_step_lazy~exec.
    DATA(lo_) = me->zif_llm_00_step_lazy~start(
      ir_ = ir_
      io_ = io_
    ).
    rr_ = me->zif_llm_00_step_lazy~collect( lo_ ).
  ENDMETHOD.


  METHOD zif_llm_00_step_lazy~start.
    ro_ ?= zcl_llm_00_step_lp_split_res=>new(
            ir_    = ir_
            io_    = io_
            io_llm = mo_llm
          ).
  ENDMETHOD.
ENDCLASS.
