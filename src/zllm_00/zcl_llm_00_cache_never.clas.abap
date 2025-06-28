class ZCL_LLM_00_CACHE_NEVER definition
  public
  final
  create private .

public section.

  interfaces ZIF_LLM_00_CACHE .

  class-methods NEW
    returning
      value(RO_) type ref to ZIF_LLM_00_CACHE .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS constructor .
ENDCLASS.



CLASS ZCL_LLM_00_CACHE_NEVER IMPLEMENTATION.


  METHOD constructor.
  ENDMETHOD.


  METHOD new.
    ro_  ?= NEW zcl_llm_00_cache_never( ).
  ENDMETHOD.


  METHOD zif_llm_00_cache~clear.
    RETURN.
  ENDMETHOD.


  METHOD zif_llm_00_cache~clear_for_seed.
    RETURN.
  ENDMETHOD.


  METHOD zif_llm_00_cache~get.
    RETURN.
  ENDMETHOD.


  METHOD zif_llm_00_cache~invalidate.
    RETURN.
  ENDMETHOD.


  METHOD zif_llm_00_cache~put.
    RETURN.
  ENDMETHOD.


  METHOD zif_llm_00_cache~trim.
    RETURN.
  ENDMETHOD.
ENDCLASS.
