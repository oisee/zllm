class ZCL_LLM_00_DOTENV definition
  public
  final
  create private .

public section.

  types STRING_T type ZIF_LLM=>STRING_T .
  types TS_ENV type ZIF_LLM_00_TYPES=>TS_ENV .
  types:
    BEGIN OF ts_kv,
        k TYPE string,
        v TYPE string,
      END OF ts_kv .
  types:
    tt_kv TYPE TABLE OF ts_kv WITH KEY k .

  class-methods NEW_FROM_STRING
    importing
      !IV_ type STRING
    returning
      value(RO_) type ref to ZCL_LLM_00_DOTENV .
  class-methods NEW_FROM_PATH
    importing
      !IV_ type STRING default 'C:\TEMP\.ENV'
    returning
      value(RO_) type ref to ZCL_LLM_00_DOTENV .
  methods V
    importing
      !K type STRING
    returning
      value(RV_) type STRING .
  class-methods GET_DEFAULT_ENV_PATH
    returning
      value(RV_) type STRING .
  methods GET_CONFIG
    returning
      value(RS_) type TS_ENV .
  class-methods NEW
    importing
      !IO_ type ref to ZIF_LLM_00_FILE optional
    returning
      value(RO_) type ref to ZCL_LLM_00_DOTENV .
  class-methods GET_AZURE_OPENAI
    importing
      !IV_PATH type STRING default 'C:\TEMP\.ENV'
    returning
      value(RS_) type TS_ENV .
* Add this private method to help with URL parsing
  methods PARSE_AZURE_FULL_URL
    importing
      !IV_URL type STRING
    returning
      value(RS_) type TS_ENV .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS: constructor
      IMPORTING
        iv_ TYPE string.

    METHODS: parse.

    DATA mv_path TYPE string .
    DATA mt_kv TYPE tt_kv .
    DATA mv_   TYPE string.
ENDCLASS.



CLASS ZCL_LLM_00_DOTENV IMPLEMENTATION.


  METHOD constructor.
    mv_ = iv_.
    me->parse( ).
  ENDMETHOD.


  METHOD get_azure_openai.

    DATA: lo_ TYPE REF TO zcl_llm_00_dotenv.
* initialize the class with the path to the .env file
    lo_ = NEW zcl_llm_00_dotenv( iv_path ).

* create a structure to hold the environmental variables
    rs_ = VALUE ts_env(
      api_url       = lo_->v( k = 'API_URL' )
      api_ver       = lo_->v( k = 'API_VER' )
      api_key       = lo_->v( k = 'API_KEY' )
      api_dep       = lo_->v( k = 'API_DEPID' )
      api_dep_embed = lo_->v( k = 'API_DEPID_EMBED' )
    ).

    " Set defaults if not provided
    IF rs_-api_max_token IS INITIAL.
      rs_-api_max_token = 96000.
    ENDIF.
    IF rs_-api_token_split_limit IS INITIAL.
      rs_-api_token_split_limit = conv i( rs_-api_max_token / 3 * 2 ).
    ENDIF.

  ENDMETHOD.


  METHOD get_config.

    " First check if API_AZURE_FULL_URL is provided
    DATA(lv_azure_full_url) = v( k = 'API_AZURE_FULL_URL' ).

    IF lv_azure_full_url IS NOT INITIAL.
      " Parse the full Azure URL to extract components
      rs_ = parse_azure_full_url( lv_azure_full_url ).
    ENDIF.

    " Overlay with individual environment variables (these take precedence if provided)
    " But API_AZURE_FULL_URL components are used as defaults

    DATA(lv_api_url) = v( k = 'API_URL' ).
    IF lv_api_url IS NOT INITIAL.
      rs_-api_url = lv_api_url.
    ENDIF.

    DATA(lv_api_ver) = v( k = 'API_VER' ).
    IF lv_api_ver IS NOT INITIAL.
      rs_-api_ver = lv_api_ver.
    ENDIF.

    DATA(lv_api_key) = v( k = 'API_KEY' ).
    IF lv_api_key IS NOT INITIAL.
      rs_-api_key = lv_api_key.
    ENDIF.

    DATA(lv_api_dep) = v( k = 'API_DEP' ).
    IF lv_api_dep IS NOT INITIAL.
      rs_-api_dep = lv_api_dep.
    ENDIF.

    DATA(lv_api_dep_embed) = v( k = 'API_DEP_EMBED' ).
    IF lv_api_dep_embed IS NOT INITIAL.
      rs_-api_dep_embed = lv_api_dep_embed.
    ENDIF.

    " API_MODEL can override the model name extracted from URL (deployment name)
    DATA(lv_api_model) = v( k = 'API_MODEL' ).
    IF lv_api_model IS NOT INITIAL.
      rs_-api_model = lv_api_model.
    ENDIF.

    DATA(lv_api_max_token) = v( k = 'API_MAX_TOKEN' ).
    IF lv_api_max_token IS NOT INITIAL.
      rs_-api_max_token = lv_api_max_token.
    ENDIF.

    DATA(lv_api_token_split_limit) = v( k = 'API_TOKEN_SPLIT_LIMIT' ).
    IF lv_api_token_split_limit IS NOT INITIAL.
      rs_-api_token_split_limit = lv_api_token_split_limit.
    ENDIF.

    DATA(lv_api_format) = v( k = 'API_FORMAT' ).
    IF lv_api_format IS NOT INITIAL.
      rs_-api_format = lv_api_format.
    ENDIF.

    " Set defaults if not provided
    IF rs_-api_max_token IS INITIAL.
      rs_-api_max_token = 96000.
    ENDIF.
    IF rs_-api_token_split_limit IS INITIAL.
      rs_-api_token_split_limit = CONV i( rs_-api_max_token / 3 * 2 ).
    ENDIF.

*    rs_ = VALUE ts_env(
*      api_url               = v( k = 'API_URL' )
*      api_ver               = v( k = 'API_VER' )
*      api_key               = v( k = 'API_KEY' )
*      api_dep               = v( k = 'API_DEP' )
*      api_dep_embed         = v( k = 'API_DEP_EMBED' )
*      api_model             = v( k = 'API_MODEL' )
*      api_max_token         = v( k = 'API_MAX_TOKEN' )
*      api_token_split_limit = v( k = 'API_TOKEN_SPLIT_LIMIT' )
*      api_format            = v( k = 'API_FORMAT' )
*    ).
*
*    " Set defaults if not provided
*    IF rs_-api_max_token IS INITIAL.
*      rs_-api_max_token = 96000.
*    ENDIF.
*    IF rs_-api_token_split_limit IS INITIAL.
*      rs_-api_token_split_limit = conv i( rs_-api_max_token / 3 * 2 ).
*    ENDIF.

  ENDMETHOD.


  METHOD get_default_env_path.
    cl_gui_frontend_services=>get_platform(
      RECEIVING
        platform             = DATA(lv_os)
      EXCEPTIONS
        error_no_gui         = 1                " No GUI available
        cntl_error           = 2                " Control error
        not_supported_by_gui = 3                " GUI does not support this
        OTHERS               = 4
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    "@TODO: replace with STVARV
    CASE lv_os.
      WHEN cl_gui_frontend_services=>platform_windowsxp.
        rv_ = '%USERPROFILE%\.env'.
        "rv_ = 'C:\Temp\.env'.
      WHEN cl_gui_frontend_services=>platform_linux.
        rv_ = '$HOME/.env'.
      WHEN cl_gui_frontend_services=>platform_macosx OR
           cl_gui_frontend_services=>platform_mac.
        rv_ = '$HOME/.env'.
      WHEN OTHERS.
        rv_ = '$HOME/.env'.
    ENDCASE.

  ENDMETHOD.


  METHOD new.
    ro_ = NEW zcl_llm_00_dotenv( io_->get_string( ) ).
  ENDMETHOD.


  METHOD new_from_path.
    IF iv_ IS INITIAL.
      DATA(lv_) = get_default_env_path( ).
    ELSE.
      lv_ = iv_.
    ENDIF.
    DATA(lo_file) = zcl_llm_00_file_local=>new( lv_ ).
    ro_ = NEW zcl_llm_00_dotenv( lo_file->get_string( ) ).
  ENDMETHOD.


  METHOD new_from_string.
    ro_ = NEW zcl_llm_00_dotenv( iv_ ).
  ENDMETHOD.


  METHOD parse.
    DATA: lt_file  TYPE string_t,
          lt_split TYPE string_t.

    DATA(lv_newline) = cl_abap_char_utilities=>cr_lf.
    FIND ALL OCCURRENCES OF lv_newline IN mv_ MATCH COUNT DATA(lv_count).
    IF lv_count = 0.
      lv_newline = cl_abap_char_utilities=>newline.
    ENDIF.

    SPLIT mv_ AT lv_newline INTO TABLE lt_file.
    LOOP AT lt_file REFERENCE INTO DATA(lr_).
      DATA(lv_line) = condense( lr_->* ).
      IF lv_line IS NOT INITIAL AND NOT lv_line+0(1) = '#'.
*        SPLIT lv_line AT '=' INTO TABLE lt_split.
*        IF lines( lt_split ) = 2.
*          APPEND VALUE #( k = lt_split[ 1 ] v = lt_split[ 2 ] ) TO me->mt_kv.
*        ENDIF.
        SPLIT lv_line AT '=' INTO DATA(lv_k) DATA(lv_v).
        IF lv_k IS NOT INITIAL.
          APPEND VALUE #( k = lv_k v = lv_v ) TO me->mt_kv.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD v.
    DATA(lr_kv) = REF #( me->mt_kv[ k = k ] OPTIONAL ).
    IF lr_kv IS BOUND.
      rv_ = condense( lr_kv->v ).
    ENDIF.
  ENDMETHOD.


  METHOD parse_azure_full_url.

    " Validate URL contains required Azure OpenAI pattern
    IF iv_url NS 'azure.com/openai/'.
      " Invalid Azure OpenAI URL format
      RETURN.
    ENDIF.

    " Extract API version from query parameter
    SPLIT iv_url AT 'api-version=' INTO DATA(lv_head) DATA(lv_tail).
    rs_-api_ver = lv_tail.

    " Extract deployment name from the URL path
    " Split to get deployment name (between '/deployments/' and '/chat/')
    SPLIT lv_head AT '/deployments/' INTO DATA(lv_url_pre) DATA(lv_url_post).
    IF lv_url_post IS NOT INITIAL.
      SPLIT lv_url_post AT '/chat/' INTO DATA(lv_deployment) DATA(lv_rest).
      rs_-api_dep = lv_deployment.         " deployment name
      rs_-api_model = lv_deployment.       " default model name to deployment name
    ENDIF.

    " Extract base URL (up to '/openai/')
    SPLIT lv_head AT 'azure.com/openai/' INTO DATA(lv_base_url) lv_tail.
    IF lv_base_url IS NOT INITIAL.
      rs_-api_url = lv_base_url && 'azure.com/'. " base API URL
    ENDIF.

    " Set default token limits based on deployment name pattern
    " Check if deployment contains '4' and '1' (indicating GPT-4 models)
    IF rs_-api_dep CP '*4*1*'.
      rs_-api_max_token         = 820000.
      rs_-api_token_split_limit = 640000.
    ELSE.
      rs_-api_max_token         = 96000.
      rs_-api_token_split_limit = 64000.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
