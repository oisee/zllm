CLASS zcl_llm_00_embedding DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS new
      IMPORTING
        iv_        TYPE zvdb_002e_bid "zvdb_002_vector-bid
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_embedding .
    METHODS answer
      IMPORTING
        !iv_       TYPE string
      EXPORTING
        !ev_       TYPE string
      RETURNING
        VALUE(rx_) TYPE zcl_vdb_002_lib=>ts_vector-q1b .
    METHODS rag
      IMPORTING
        !iv_       TYPE string
        !it_       TYPE zcl_vdb_002_lib=>tt_vector
      EXPORTING
        !ev_prompt TYPE string
        !et_used   TYPE zcl_vdb_002_lib=>tt_vector
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS answer_and_save
      IMPORTING
        !iv_       TYPE string
      EXPORTING
        !ev_       TYPE string
      RETURNING
        VALUE(rs_) TYPE zcl_vdb_002_lib=>ts_vector .
    METHODS embed
      IMPORTING
        !iv_       TYPE string
      RETURNING
        VALUE(rx_) TYPE zcl_vdb_002_lib=>ts_vector-q1b .
    METHODS embed_and_save
      IMPORTING
        !iv_       TYPE string
      RETURNING
        VALUE(rs_) TYPE zcl_vdb_002_lib=>ts_vector .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mo_env TYPE REF TO zcl_llm_00_dotenv.
*    DATA: mo_sdk TYPE REF TO zif_peng_azoai_sdk.
    DATA: mo_sdk_lite TYPE REF TO zcl_llm_00_sdk_lite.
    "   DATA: mo_sdk_chat  TYPE REF TO zif_peng_azoai_sdk.
    DATA: mo_lib TYPE REF TO zcl_vdb_002_lib.
    DATA: ms_aoi TYPE zcl_llm_00_dotenv=>ts_env.
    METHODS: constructor
      IMPORTING iv_ TYPE zvdb_002_vector-bid.
ENDCLASS.



CLASS ZCL_LLM_00_EMBEDDING IMPLEMENTATION.


  METHOD answer.
*    DATA:
*      ls_in  TYPE zif_peng_azoai_sdk_types=>ty_chatcompletion_input,
*      ls_out TYPE zif_peng_azoai_sdk_types=>ty_chatcompletion_output.
*
*    ls_in-temperature = '0.1'.
*    ls_in-top_p = '1.0'.
*    ls_in-messages = VALUE #(
*      ( role = 'system' content = |Behave as "prototype answer generator". Generate a sample passage that looks like an answer to a question:| )
*      ( role = 'user'   content = iv_ )
*    ).
*    TRY.
*        mo_sdk->chat_completions( )->create(
*          EXPORTING
*            deploymentid = ms_aoi-depid
*            prompts      = ls_in
*          IMPORTING
*             statuscode   = DATA(lv_statuscode)   " HTTP Response status code.
*             statusreason = DATA(lv_statusreason) " HTTP Status Reason.
*             json         = DATA(lv_json)         " JSON String received from AI engine.
*             response     = ls_out
*             error        = DATA(lv_error)        " Error information parsed from JSON string
*  ).
*     Display the results.
*
*      CATCH zcx_peng_azoai_sdk_exception. " MSPENG:Azure Open AI ABAP SDK Exception
*        CONTINUE.
*    ENDTRY.
*    IF lv_statuscode NE '200'.
*      RETURN.
*    ENDIF.
*
*    DATA(lv_answer) = VALUE #( ls_out-choices[ 1 ]-message-content OPTIONAL ) .
*    rx_ = embed( lv_answer ).
*    ev_ = lv_answer.

  ENDMETHOD.


  METHOD answer_and_save.
    DATA lv_answer TYPE string.
    DATA(lx_) = answer( EXPORTING iv_ = iv_
                        IMPORTING ev_ = lv_answer  ).
    rs_ = mo_lib->create_vector(
                   ix_        = lx_
                   iv_payload = lv_answer
    ).
    mo_lib->save_vector( rs_ ).

  ENDMETHOD.


  METHOD constructor.
    mo_env = zcl_llm_00_dotenv=>new( ).
    mo_lib = zcl_vdb_002_lib=>new( iv_  ).
    ms_aoi = VALUE zcl_llm_00_dotenv=>ts_env(
      api_url       = mo_env->v( k = 'API_URL' )
      api_ver       = mo_env->v( k = 'API_VER' )
      api_key       = mo_env->v( k = 'API_KEY' )
      api_dep       = mo_env->v( k = 'API_DEP' )
"     depid         = mo_env->v( k = 'DEPID'   )
    ).

    mo_sdk_lite = zcl_llm_00_sdk_lite=>new( ms_aoi ).

*    TRY.
*        mo_sdk = zcl_peng_azoai_sdk_factory=>get_instance( )->get_sdk(
*                   api_version = ms_aoi-api_ver
*                   api_base    = ms_aoi-api_url
*                   api_type    = zif_peng_azoai_sdk_constants=>c_apitype-azure
*                   api_key     = ms_aoi-api_key
*       ).
*      CATCH zcx_peng_azoai_sdk_exception. " MSPENG:Azure Open AI ABAP SDK Exception
*        "CONTINUE.
*    ENDTRY.

  ENDMETHOD.


  METHOD embed.
*    DATA ls_msg TYPE zif_peng_azoai_sdk_types=>ty_embeddings_input.
*    ls_msg-input = VALUE #( ( iv_ ) ).
*
*    TRY.
*        mo_sdk->embeddings( )->create(
*           EXPORTING
*             deploymentid = 'ada4text'
*             inputs       = ls_msg
*           IMPORTING
*             statuscode   = DATA(lv_statuscode)   " HTTP Response status code.
*             statusreason = DATA(lv_statusreason) " HTTP Status Reason.
*             json         = DATA(lv_json)         " JSON String received from AI engine.
*             response     = DATA(ls_response)     " Parsed and ABAP ready data structure from JSON String
*             error        = DATA(lv_error)        " Error information parsed from JSON string
*         ).
*      CATCH zcx_peng_azoai_sdk_exception. " MSPENG:Azure Open AI ABAP SDK Exception
*        "CONTINUE.
*    ENDTRY.
*    IF lv_statuscode NE '200'.
*      RETURN.
*    ENDIF.
*
*    "DATA(lt_e) = ls_response-data.
*    DATA(lr_e) = REF #( ls_response-data[ object = 'embedding' ]-embedding OPTIONAL ).
*
*    IF lr_e IS NOT BOUND.
*      RETURN.
*    ENDIF.
*
*    rx_ = mo_lib->quantize( lr_e->* ).

  ENDMETHOD.


  METHOD embed_and_save.
    DATA(lx_) = embed( iv_ ).
    rs_ = mo_lib->create_vector(
                   ix_        = lx_
                   iv_payload = iv_
    ).
    mo_lib->save_vector( rs_ ).

  ENDMETHOD.


  METHOD new.
    ro_ = NEW zcl_llm_00_embedding( iv_ ).

  ENDMETHOD.


  METHOD rag.
*    DATA(lv_lf) = cl_abap_char_utilities=>cr_lf.
*    DATA:
*      ls_in  TYPE zif_peng_azoai_sdk_types=>ty_chatcompletion_input,
*      ls_out TYPE zif_peng_azoai_sdk_types=>ty_chatcompletion_output.
*
*    DATA(lt_) = it_.
*    SORT lt_ BY rank DESCENDING.
*    DELETE lt_ WHERE rank < 512.
*
*    DATA:lv_data   TYPE string.
*    DATA:lv_prompt TYPE string.
*    DATA(lv_i) = 0.
*    LOOP AT lt_ REFERENCE INTO DATA(lr_).
*      IF strlen( lv_data ) > 20000.
*        EXIT.
*      ENDIF.
*      lv_data = |{ lv_data }{ lr_->p }{ lv_lf }|.
*      ADD 1 TO lv_i.
*    ENDLOOP.
*    APPEND LINES OF lt_ FROM 1 TO ( lv_i ) TO et_used.
*
*    lv_prompt = |####START{ lv_lf }{ lv_lf }{ lv_data }{ lv_lf }{ lv_lf }####END| && lv_lf &&
*                |####QUESTION { lv_lf }{ lv_lf } | &&
*                |Using data mentioned above, please provide the answer to: { lv_lf }| &&
*                |{ iv_ }|.
*
*    ls_in-temperature = '0.1'.
*    ls_in-top_p = '1.0'.
*    ls_in-messages = VALUE #(
*      ( role = 'system' content = | You are helpful AI Assistant. | &&
*                                  | Answer the following question, using only pieces of information that mentioned after the ####START and before ####END,| &&
*                                  | do not make things up, if you do not have information, tell the user "I do not know.". Question will be marked as ####QUESTION: :| )
*      ( role = 'user'   content = lv_prompt )
*    ).
*
*    ev_prompt = lv_prompt.
*    TRY.
*        mo_sdk->chat_completions( )->create(
*          EXPORTING
*            deploymentid = ms_aoi-depid
*            prompts      = ls_in
*          IMPORTING
*             statuscode   = DATA(lv_statuscode)   " HTTP Response status code.
*             statusreason = DATA(lv_statusreason) " HTTP Status Reason.
*             json         = DATA(lv_json)         " JSON String received from AI engine.
*             response     = ls_out
*             error        = DATA(lv_error)        " Error information parsed from JSON string
*  ).
**     Display the results.
*
*        rv_ = VALUE #( ls_out-choices[ 1 ]-message-content OPTIONAL ) .
*
*      CATCH zcx_peng_azoai_sdk_exception. " MSPENG:Azure Open AI ABAP SDK Exception
*        "CONTINUE.
*    ENDTRY.
*    IF lv_statuscode NE '200'.
*      RETURN.
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
