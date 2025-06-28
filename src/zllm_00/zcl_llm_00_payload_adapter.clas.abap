CLASS zcl_llm_00_payload_adapter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_payload_adapter .

    CLASS-METHODS new
      IMPORTING
        !io_llm           TYPE REF TO zif_llm_00_llm_lazy
      RETURNING
        VALUE(ro_)        TYPE REF TO zif_llm_00_payload_adapter .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_LLM_00_PAYLOAD_ADAPTER IMPLEMENTATION.


  METHOD new.

    CASE io_llm->get_config( )-model_type.
      WHEN zcl_llm_00_predictoken=>gc_llm_type-gpt_reasoning.
        ro_ = zcl_llm_00_payload_adapter_o3=>new( io_llm ).
      WHEN OTHERS.
        ro_ = zcl_llm_00_payload_adapter_4o=>new( io_llm ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
