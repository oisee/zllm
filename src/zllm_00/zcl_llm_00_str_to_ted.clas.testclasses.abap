
CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      cut TYPE REF TO zcl_llm_00_str_to_ted.  "class under test

    METHODS: setup.
    METHODS: teardown.
    METHODS: get_ted FOR TESTING.
    METHODS: new FOR TESTING.
    METHODS: new_by_ast_structure FOR TESTING.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.

  METHOD setup.
*    DATA:ls_ TYPE ZCOL_177s_t01.

*    cut = ZCL_llm_str_to_ted=>new(
*            io_gui       = cl_gui_container=>default_screen
*            ir_str       = REF #( ls_ )
**            itr_         =
**            iv_threshold = 6
*          ).
  ENDMETHOD.
  METHOD teardown.

  ENDMETHOD.


  METHOD get_ted.

*    DATA itr_ TYPE zcl_col_000_tvarvc=>ttr_.
*    DATA rt_ TYPE ZCL_llm_str_to_ted=>tt_.
*
*    rt_ = cut->get_ted( itr_ ).
  ENDMETHOD.


  METHOD new.

*    DATA io_gui TYPE REF TO cl_gui_container.
*    DATA ir_str TYPE REF TO data.
*    DATA itr_ TYPE zcl_col_000_tvarvc=>ttr_.
*    DATA iv_threshold TYPE i.
*    DATA ro_ TYPE REF TO ZCL_llm_str_to_ted.
*
*    ro_ = ZCL_llm_str_to_ted=>new(
*        io_gui = io_gui
*        ir_str = ir_str
**       ITR_ = itr_
**       IV_THRESHOLD = iv_Threshold
*    ).
  ENDMETHOD.


  METHOD new_by_ast_structure.

*    DATA io_gui TYPE REF TO cl_gui_container.
*    DATA ir_str TYPE REF TO data.
*    DATA itr_ TYPE zcl_col_000_tvarvc=>ttr_.
*    DATA iv_threshold TYPE i.
*    DATA ro_ TYPE REF TO ZCL_llm_str_to_ted.
*
*    ro_ = ZCL_llm_str_to_ted=>new_by_ast_structure(
*        io_gui = io_gui
*        ir_str = ir_str
**       ITR_ = itr_
**       IV_THRESHOLD = iv_Threshold
*    ).
  ENDMETHOD.

ENDCLASS.
