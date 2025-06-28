*&---------------------------------------------------------------------*
*& Report ZLLM_01_CHAIN_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zllm_00_flow_demo.
PARAMETERS: p_prmpt1 TYPE string DEFAULT 'Write a cute poem about kitten, in 8 lines.' LOWER CASE.
PARAMETERS: p_prmpt2 TYPE string DEFAULT 'Summarize this: {T} and repeat the poem, and translate poem to Japanese. And why the sun is yellow?' LOWER CASE.

CLASS lcl_ DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS new RETURNING VALUE(ro_) TYPE REF TO lcl_.
    METHODS go.
ENDCLASS.

CLASS lcl_ IMPLEMENTATION.
  METHOD new.
    ro_ = NEW lcl_( ).
  ENDMETHOD.
  METHOD go.
    TYPES: ltr_string TYPE RANGE OF string.

    DATA(lo_factory) = zcl_llm=>new( 'DEFAULT.ENV' ).
    DATA(lo_llm)     = lo_factory->get_llm( ).

    DATA(lo_step_01) = zcl_llm_00_step_lazy=>new_from_string(
      iv_usr = p_prmpt1
      io_llm = lo_llm
    ).

    DATA(lo_step_02) = zcl_llm_00_step_lazy=>new_from_string(
      iv_usr = p_prmpt2
      io_llm = lo_llm
    ).

    DATA(lo_flow) = zcl_llm_00_flow_lazy=>new(
      VALUE #(
        ( lo_step_01 )
        ( lo_step_02 )
      )
    ).

    DATA(lo_res) = lo_flow->to_step( )->start( ).
    DATA(lv_res) = lo_res->to_string( ).

    DATA(lo_md)   = zcl_llm_00_markdown=>new( ).
    DATA(lv_html) = lo_md->text( lv_res ).
    cl_demo_output=>display_html( lv_html ).

  ENDMETHOD.
ENDCLASS.


INITIALIZATION.
  DATA(go_) = lcl_=>new( ).

AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'ONLI'.
      go_->go( ).
    WHEN OTHERS.
  ENDCASE.
