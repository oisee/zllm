class ZCL_LLM_00_CACHE definition
  public
  final
  create private .

public section.

  interfaces ZIF_LLM_00_CACHE .

  class-methods NEW
    importing
      !IV_SEED type I optional
      !IO_CODEC type ref to ZIF_LLM_00_CODEC optional
    returning
      value(RO_) type ref to ZIF_LLM_00_CACHE .
  class-methods _SET_DEFAULT_SEED
    importing
      !IV_SEED type I .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS constructor
      IMPORTING
        !iv_seed  TYPE i
        !io_codec TYPE REF TO zif_llm_00_codec.

    METHODS: commit.

    CLASS-DATA: gv_seed TYPE i.
    DATA: mv_seed TYPE i.
    DATA: mo_codec TYPE REF TO zif_llm_00_codec.
ENDCLASS.



CLASS ZCL_LLM_00_CACHE IMPLEMENTATION.


  METHOD commit.
    COMMIT WORK.
  ENDMETHOD.


  METHOD constructor.
    mv_seed  = iv_seed.
    mo_codec = io_codec.
  ENDMETHOD.


  METHOD new.
    IF iv_seed IS NOT SUPPLIED.
      DATA(lv_seed) = gv_seed.
    ELSE.
      lv_seed = iv_seed.
    ENDIF.

    IF io_codec IS NOT BOUND.
      "DATA(lo_codec) = zcl_llm_00_codec_mock=>new( ).
      "DATA(lo_codec) = zcl_llm_00_codec=>new( ).
      data(lo_codec) = io_codec.
    ELSE.
      lo_codec = io_codec.
    ENDIF.
    ro_  ?= NEW zcl_llm_00_cache(
      iv_seed  = lv_seed
      io_codec = lo_codec
    ).
  ENDMETHOD.


  METHOD zif_llm_00_cache~clear.
    "clear the whole content of the table
    DELETE FROM zllm_00_cache WHERE seed >= 0 OR seed < 0.
    me->commit( ).
  ENDMETHOD.


  METHOD zif_llm_00_cache~clear_for_seed.
    " Delete all keys for the provided seed in the database
    DELETE FROM zllm_00_cache WHERE seed = iv_seed.
    me->commit( ).
  ENDMETHOD.


  METHOD zif_llm_00_cache~get.
    DATA(lv_hash) = zcl_llm=>string_hash( k ).
    SELECT SINGLE *
      FROM zllm_00_cache
      WHERE seed = @mv_seed AND
            k    = @lv_hash
      INTO @DATA(ls_).

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    IF mo_codec IS BOUND.
      DATA(lv_v) = mo_codec->decode( ls_-v ).
    ELSE.
      lv_v = ls_-v.
    ENDIF.
    rv_ = zcl_llm=>xstring_to_string( lv_v ).
*--------------------------------------------------------------------*
    ADD 1 TO ls_-accessed.
    ls_-adate = sy-datum.
    UPDATE zllm_00_cache FROM ls_.
    me->commit( ).
  ENDMETHOD.


  METHOD zif_llm_00_cache~invalidate.
    DATA(lv_hash) = zcl_llm=>string_hash( k ).
    " Delete the cache entry with the given hash key
    DELETE FROM zllm_00_cache WHERE seed = @mv_seed AND k = @lv_hash.
    me->commit( ).
  ENDMETHOD.


  METHOD zif_llm_00_cache~put.
    DATA(lv_hash) = zcl_llm=>string_hash( k ).
    DATA(lv_v) = zcl_llm=>string_to_xstring( v ).
    IF mo_codec IS BOUND.
      lv_v = mo_codec->encode( lv_v ).
    ENDIF.

    DATA(ls_) = VALUE zllm_00_cache(
      seed = mv_seed
      k    = lv_hash
      v    = lv_v
      cdate = sy-datum
    ).
    GET TIME STAMP FIELD ls_-ts.
    MODIFY zllm_00_cache FROM @ls_.
    me->commit( ).
  ENDMETHOD.


  METHOD zif_llm_00_cache~trim.
    DATA(lv_trim_date) = CONV dats( sy-datum - 14 ).
    DELETE FROM zllm_00_cache WHERE cdate <= lv_trim_date AND adate <= lv_trim_date.
  ENDMETHOD.


  METHOD _set_default_seed.
    gv_seed = iv_seed.
  ENDMETHOD.
ENDCLASS.
