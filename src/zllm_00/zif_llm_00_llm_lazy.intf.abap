interface ZIF_LLM_00_LLM_LAZY
  public .


  types:
    BEGIN OF ts_llm_config,
           model_name  TYPE string,
           model_type  TYPE string,
           max_token   TYPE i,
           split_limit TYPE i,
         END OF ts_llm_config .
  types:
  "$. region    LLM throttle/limit {
    BEGIN OF ts_lim_in,
      req_before_pause TYPE i, "requests before pause
      pause_for        TYPE i, "pause in seconds
      tok_before_pause TYPE i, "tokens before pause
    END OF ts_lim_in .
  types:
    BEGIN OF ts_lim,
      llm_hash         TYPE hash160,
      llm_id           TYPE string,
      req_before_pause TYPE i, "requests before pause
      pause_for        TYPE i, "pause in seconds
      req_counter      TYPE i,
      tok_before_pause TYPE i, "tokens before pause
      "pause_for        TYPE i,
      tok_counter      TYPE i,
    END OF ts_lim .
  types:
    tt_lim TYPE STANDARD TABLE OF ts_lim WITH KEY llm_hash .

  "$. endregion LLM throttle/limit }
  methods Q
    importing
      !IO_ type ref to ZIF_LLM_00_JSON
      !IV_K type STRING optional
    returning
      value(RO_) type ref to ZIF_LLM_00_LLM_RESPONSE
    raising
      ZCX_S .
  methods A
    importing
      !IO_ type ref to ZIF_LLM_00_LLM_RESPONSE
    returning
      value(RV_) type STRING
    raising
      ZCX_S .
  methods GET_CONFIG
    returning
      value(RS_) type TS_LLM_CONFIG .
  methods GET_PAYLOAD_ADAPTER
    returning
      value(RO_) type ref to ZIF_LLM_00_PAYLOAD_ADAPTER .
endinterface.
