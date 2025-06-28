CLASS zcl_llm_00_payload_adapter_o3 DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_payload_adapter .

    CLASS-METHODS new
      IMPORTING
        !io_llm    TYPE REF TO zif_llm_00_llm_lazy
      RETURNING
        VALUE(ro_) TYPE REF TO zif_llm_00_payload_adapter .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mo_llm            TYPE REF TO zif_llm_00_llm_lazy.
    DATA: ms_config         TYPE zif_llm_00_llm_lazy=>ts_llm_config.
    METHODS constructor
      IMPORTING
        !io_llm TYPE REF TO zif_llm_00_llm_lazy.
ENDCLASS.



CLASS ZCL_LLM_00_PAYLOAD_ADAPTER_O3 IMPLEMENTATION.


  METHOD constructor.
    mo_llm = io_llm.
    ms_config = io_llm->get_config( ).
  ENDMETHOD.


  METHOD new.
    ro_ = NEW zcl_llm_00_payload_adapter_o3( io_llm ).
  ENDMETHOD.


  METHOD zif_llm_00_payload_adapter~input.
    DATA(ls_reasoning_in) = VALUE zif_llm_00_types=>ts_reasoning(
      model    = ms_config-model_name
      input    = it_msg
    ).
    IF iv_json = abap_true.
      ls_reasoning_in-response_format = zcl_llm_00_kv=>new( VALUE #( ( k = 'type' v = 'json_object' ) ) ).
    ENDIF.
    ro_ ?= zcl_llm_00_reasoning_in=>new( ls_reasoning_in ).
  ENDMETHOD.


  METHOD zif_llm_00_payload_adapter~output.

    "DATA(lo_chat_out) = zcl_llm_00_chat_out=>new_from_json( iv_ ).
    DATA(lo_reasoning_out) = zcl_llm_00_reasoning_out=>new_from_json( iv_ ).
    rv_ = lo_reasoning_out->get_reply( ).

  ENDMETHOD.
ENDCLASS.
