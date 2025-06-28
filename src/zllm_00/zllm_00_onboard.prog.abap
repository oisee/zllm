*&---------------------------------------------------------------------*
*& Report ZLLM_00_ONBOARD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zllm_00_onboard MESSAGE-ID zllm_00.

TABLES: sscrfields.

" Global variables
DATA: gv_msg    TYPE string,
      gv_llm_ok TYPE sap_bool.

DATA: gv_expert_mode TYPE sap_bool VALUE abap_false.

" Selection screen with 4 LLM configurations
SELECTION-SCREEN BEGIN OF BLOCK b00 WITH FRAME TITLE TEXT-b00.
  " Default LLM
  PARAMETERS: p_defurl TYPE string DEFAULT 'https://<resource>.openai.azure.com/openai/deployments/<deployment-name>/chat/completions?api-version=YYYY-MM-DD-preview' LOWER CASE MODIF ID g1.
  PARAMETERS: p_defkey TYPE string DEFAULT '<api_key>' LOWER CASE MODIF ID g1.
  PARAMETERS: p_defen AS CHECKBOX DEFAULT 'X' MODIF ID g11 USER-COMMAND zchb.

  " Mini LLM

  PARAMETERS: p_minurl TYPE string DEFAULT 'https://<resource>.openai.azure.com/openai/deployments/<mini-deployment>/chat/completions?api-version=YYYY-MM-DD-preview' LOWER CASE MODIF ID g2.
  PARAMETERS: p_minkey TYPE string DEFAULT '<mini_api_key>' LOWER CASE MODIF ID g2.
  PARAMETERS: p_minen AS CHECKBOX MODIF ID g22 USER-COMMAND zchb.

  " Maxi LLM
  PARAMETERS: p_maxurl TYPE string DEFAULT 'https://<resource>.openai.azure.com/openai/deployments/<maxi-deployment>/chat/completions?api-version=YYYY-MM-DD-preview' LOWER CASE MODIF ID g3.
  PARAMETERS: p_maxkey TYPE string DEFAULT '<maxi_api_key>' LOWER CASE MODIF ID g3.
  PARAMETERS: p_maxen AS CHECKBOX MODIF ID g33 USER-COMMAND zchb.

  " Deep LLM
  PARAMETERS: p_dp_url TYPE string DEFAULT 'https://<resource>.openai.azure.com/openai/deployments/<deep-deployment>/chat/completions?api-version=YYYY-MM-DD-preview' LOWER CASE MODIF ID g4.
  PARAMETERS: p_dp_key TYPE string DEFAULT '<deep_api_key>' LOWER CASE MODIF ID g4.
  PARAMETERS: p_dp_en AS CHECKBOX MODIF ID g44 USER-COMMAND zchb.
SELECTION-SCREEN END OF BLOCK b00.

SELECTION-SCREEN BEGIN OF BLOCK b10 WITH FRAME TITLE TEXT-b10.
  PARAMETERS: p_test_q TYPE string DEFAULT 'Tell me a joke about a killeroo. (Mighty-Boosh)' OBLIGATORY LOWER CASE.
SELECTION-SCREEN END OF BLOCK b10.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
  PARAMETERS: p_bin_o TYPE zllm_00s_bin-bin LOWER CASE DEFAULT '$ZLLM'.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN FUNCTION KEY 1. " test all enabled
SELECTION-SCREEN FUNCTION KEY 2. " save all configs
SELECTION-SCREEN FUNCTION KEY 3. " test & save all
SELECTION-SCREEN FUNCTION KEY 4. " expert mode

" Type definitions for multi-LLM support
TYPES: BEGIN OF ts_llm_config,
         variant     TYPE string,
         suffix      TYPE string,
         description TYPE string,
         enabled     TYPE abap_bool,
         url         TYPE string,
         api_key     TYPE string,
       END OF ts_llm_config.
TYPES: tt_llm_configs TYPE TABLE OF ts_llm_config WITH DEFAULT KEY.

"$. region Local Classes {
" =======================================================================================
" Class for LLM Testing Operations - Enhanced for multiple LLMs
" =======================================================================================
CLASS lcl_llm_tester DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS: new
      RETURNING VALUE(ro_) TYPE REF TO lcl_llm_tester.

    METHODS: test_single_llm
      IMPORTING iv_variant        TYPE string
                iv_full_url       TYPE string
                iv_api_key        TYPE string
                iv_question       TYPE string
      RETURNING VALUE(rv_success) TYPE abap_bool,

      test_all_llms
        IMPORTING it_configs        TYPE tt_llm_configs
                  iv_question       TYPE string
        RETURNING VALUE(rv_success) TYPE abap_bool.

  PRIVATE SECTION.
    METHODS: constructor.
ENDCLASS.

CLASS lcl_llm_tester IMPLEMENTATION.
  METHOD new.
    ro_ = NEW #( ).
  ENDMETHOD.

  METHOD constructor.
    " Constructor implementation
  ENDMETHOD.

  METHOD test_single_llm.
    DATA: lo_output TYPE REF TO if_demo_output.

    TRY.
        " Validate inputs
        IF iv_full_url IS INITIAL OR iv_api_key IS INITIAL.
          cl_demo_output=>write( |Skipping { iv_variant }: Missing URL or API key| ).
          rv_success = abap_false.
          RETURN.
        ENDIF.

        cl_demo_output=>write( |Testing { iv_variant } LLM...| ).

        " Create dotenv object from URL and key
        DATA(lv_env_content) = |API_AZURE_FULL_URL={ iv_full_url }| && zif_llm=>cr_lf &&
                               |API_KEY={ iv_api_key }|.
        DATA(lo_dotenv) = zcl_llm_00_dotenv=>new_from_string( lv_env_content ).
        DATA(ls_parsed_config) = lo_dotenv->get_config( ).

        " Create LLM instance
        DATA(lo_llm) = zcl_llm_00_llm_lazy=>new(
          is_      = ls_parsed_config
          io_cache = zcl_llm_00_cache_never=>new( )
        ).

        " Create step and execute
        DATA(lo_step) = zcl_llm_00_step_lazy=>new_from_pat(
          io_pat_usr = zcl_llm_00_pat=>new( iv_question )
          io_llm     = lo_llm
        ).
        DATA(lr_result) = lo_step->exec( ).

        cl_demo_output=>write( |✓ { iv_variant } LLM test successful| ).
        cl_demo_output=>write( |Response: { lr_result->* }| ).
        rv_success = abap_true.

      CATCH zcx_s INTO DATA(lx_s).
        cl_demo_output=>write( |✗ { iv_variant } LLM test failed: { lx_s->get_longtext( ) }| ).
        rv_success = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD test_all_llms.
    DATA: lv_success_count TYPE i,
          lv_total_count   TYPE i.

    cl_demo_output=>new( )->begin_section( |Testing Multiple LLM Configurations| ).

    LOOP AT it_configs INTO DATA(ls_config) WHERE enabled = abap_true.
      lv_total_count = lv_total_count + 1.

      IF test_single_llm(
        iv_variant   = ls_config-variant
        iv_full_url  = ls_config-url
        iv_api_key   = ls_config-api_key
        iv_question  = iv_question
      ) = abap_true.
        lv_success_count = lv_success_count + 1.
      ENDIF.

      cl_demo_output=>write( |---| ).
    ENDLOOP.

    IF lv_total_count = 0.
      cl_demo_output=>write( |No LLM configurations enabled for testing| ).
      rv_success = abap_false.
    ELSE.
      cl_demo_output=>write( |Test Summary: { lv_success_count }/{ lv_total_count } configurations successful| ).
      rv_success = COND #( WHEN lv_success_count > 0 THEN abap_true ELSE abap_false ).
    ENDIF.

    cl_demo_output=>display( ).
  ENDMETHOD.
ENDCLASS.

" =======================================================================================
" Class for Environment File Operations - Enhanced for multiple variants
" =======================================================================================
CLASS lcl_env_manager DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS: new
      IMPORTING iv_bin     TYPE zllm_00s_bin-bin
      RETURNING VALUE(ro_) TYPE REF TO lcl_env_manager.

    METHODS: save_all_env_files
      IMPORTING it_configs        TYPE tt_llm_configs
      RETURNING VALUE(rv_success) TYPE abap_bool.

  PRIVATE SECTION.
    DATA: mv_bin TYPE zllm_00s_bin-bin.

    METHODS: constructor IMPORTING iv_bin TYPE zllm_00s_bin-bin.

    METHODS: create_env_content
      IMPORTING is_config         TYPE ts_llm_config
      RETURNING VALUE(rv_content) TYPE string.

    METHODS: save_single_env
      IMPORTING iv_filename       TYPE string
                iv_content        TYPE string
      RETURNING VALUE(rv_success) TYPE abap_bool.
ENDCLASS.

CLASS lcl_env_manager IMPLEMENTATION.
  METHOD new.
    ro_ = NEW #( iv_bin ).
  ENDMETHOD.

  METHOD constructor.
    mv_bin = iv_bin.
  ENDMETHOD.

  METHOD save_all_env_files.
    DATA: lv_success_count TYPE i,
          lv_total_count   TYPE i.

    rv_success = abap_true.

    LOOP AT it_configs INTO DATA(ls_config) WHERE enabled = abap_true.
      lv_total_count = lv_total_count + 1.

      " Skip if URL or key is empty/placeholder
      IF ls_config-url CS '<' OR ls_config-api_key CS '<' OR
         ls_config-url IS INITIAL OR ls_config-api_key IS INITIAL.
        cl_demo_output=>write( |Skipping { ls_config-variant }: Contains placeholder values| ).
        CONTINUE.
      ENDIF.

      DATA(lv_filename) = to_upper( |default{ ls_config-suffix }.env| ).
      DATA(lv_content) = create_env_content( is_config = ls_config ).

      IF save_single_env(
        iv_filename = lv_filename
        iv_content = lv_content
      ) = abap_true.
        lv_success_count = lv_success_count + 1.
        cl_demo_output=>write( |✓ Saved { lv_filename }| ).
      ELSE.
        cl_demo_output=>write( |✗ Failed to save { lv_filename }| ).
        rv_success = abap_false.
      ENDIF.
    ENDLOOP.

    IF lv_total_count = 0.
      MESSAGE i000 WITH 'No valid configurations to save'.
    ELSEIF lv_success_count = lv_total_count.
      MESSAGE i000 WITH |All { lv_success_count } environment files saved successfully|.
    ELSE.
      MESSAGE w000 WITH |{ lv_success_count }/{ lv_total_count } environment files saved|.
    ENDIF.
  ENDMETHOD.

  METHOD create_env_content.
    rv_content = |# { is_config-description }| && zif_llm=>cr_lf &&
                 |# Generated on { sy-datum DATE = USER } at { sy-uzeit TIME = USER }| && zif_llm=>cr_lf &&
                 |# Variant: { is_config-variant }{ is_config-suffix }| && zif_llm=>cr_lf &&
                 || && zif_llm=>cr_lf &&
                 |# Azure OpenAI Configuration| && zif_llm=>cr_lf &&
                 |API_AZURE_FULL_URL={ is_config-url }| && zif_llm=>cr_lf &&
                 |API_KEY={ is_config-api_key }| && zif_llm=>cr_lf &&
                 || && zif_llm=>cr_lf &&
                 |# Token Limits for { is_config-description }| && zif_llm=>cr_lf.

    " Add variant-specific configurations
    CASE is_config-suffix.
      WHEN '-min'.
        rv_content = rv_content &&
                     |API_MAX_TOKEN=1000| && zif_llm=>cr_lf &&
                     |API_TOKEN_SPLIT_LIMIT=800| && zif_llm=>cr_lf.
      WHEN '-max'.
        rv_content = rv_content &&
                     |API_MAX_TOKEN=8000| && zif_llm=>cr_lf &&
                     |API_TOKEN_SPLIT_LIMIT=6000| && zif_llm=>cr_lf.
      WHEN '-deep'.
        rv_content = rv_content &&
                     |API_MAX_TOKEN=4000| && zif_llm=>cr_lf &&
                     |API_TOKEN_SPLIT_LIMIT=3000| && zif_llm=>cr_lf &&
                     |# Deep thinking mode settings| && zif_llm=>cr_lf &&
                     |#API_TEMPERATURE=0.1| && zif_llm=>cr_lf.
      WHEN OTHERS.
        rv_content = rv_content &&
                     |#API_MAX_TOKEN=4000| && zif_llm=>cr_lf &&
                     |#API_TOKEN_SPLIT_LIMIT=3000| && zif_llm=>cr_lf.
    ENDCASE.

    rv_content = rv_content &&
                 || && zif_llm=>cr_lf &&
                 |# Optional: Override model name (defaults to deployment name from URL)| && zif_llm=>cr_lf &&
                 |#API_MODEL=gpt-4o| && zif_llm=>cr_lf &&
                 |# Optional: Embedding deployment| && zif_llm=>cr_lf &&
                 |#API_DEP_EMBED=ada4text|.
  ENDMETHOD.

  METHOD save_single_env.
    TRY.
        DATA(lo_fl) = zcl_llm_00_file_list_bin=>new_from_bin( CONV #( mv_bin ) ).
        DATA(lo_file) = zcl_llm_00_file_mock=>new(
          iv_content = iv_content
          iv_path    = iv_filename
        ).
        lo_fl->save( lo_file ).
        rv_success = abap_true.
      CATCH zcx_s INTO DATA(lx_s).
        cl_demo_output=>write( |Error saving { iv_filename }: { lx_s->get_longtext( ) }| ).
        rv_success = abap_false.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

" =======================================================================================
" Configuration Manager Class - Handles screen parameters to config mapping
" =======================================================================================
CLASS lcl_config_manager DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS: new RETURNING VALUE(ro_) TYPE REF TO lcl_config_manager.

    METHODS: get_all_configs
      RETURNING VALUE(rt_configs) TYPE tt_llm_configs.

  PRIVATE SECTION.
    METHODS: constructor.
ENDCLASS.

CLASS lcl_config_manager IMPLEMENTATION.
  METHOD new.
    ro_ = NEW #( ).
  ENDMETHOD.

  METHOD constructor.
    " Constructor implementation
  ENDMETHOD.

  METHOD get_all_configs.
    rt_configs = VALUE #(
      ( variant = 'Default' suffix = ''      description = 'Default configuration'         enabled = p_defen url = p_defurl api_key = p_defkey )
      ( variant = 'Mini'    suffix = '-min'  description = 'Mini-models for simple tasks'  enabled = p_minen url = p_minurl api_key = p_minkey )
      ( variant = 'Maxi'    suffix = '-max'  description = 'Maxi-models for complex tasks' enabled = p_maxen url = p_maxurl api_key = p_maxkey )
      ( variant = 'Deep'    suffix = '-deep' description = 'Deep Thinker configuration'    enabled = p_dp_en  url = p_dp_url  api_key = p_dp_key )
    ).
  ENDMETHOD.
ENDCLASS.

" =======================================================================================
" Main Application Class - Enhanced validation and orchestration
" =======================================================================================
CLASS lcl_main DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS: new RETURNING VALUE(ro_) TYPE REF TO lcl_main.

    METHODS: validate_all_configs
      IMPORTING it_configs      TYPE tt_llm_configs
      RETURNING VALUE(rv_valid) TYPE abap_bool,

      execute_test_all,
      execute_save_all,
      execute_test_and_save.

  PRIVATE SECTION.
    DATA: mo_config_manager TYPE REF TO lcl_config_manager,
          mo_tester         TYPE REF TO lcl_llm_tester,
          mo_env_manager    TYPE REF TO lcl_env_manager.

    METHODS: constructor.

    METHODS: validate_single_config
      IMPORTING is_config       TYPE ts_llm_config
      RETURNING VALUE(rv_valid) TYPE abap_bool.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD new.
    ro_ = NEW #( ).
  ENDMETHOD.

  METHOD constructor.
    mo_config_manager = lcl_config_manager=>new( ).
    mo_tester = lcl_llm_tester=>new( ).
    mo_env_manager = lcl_env_manager=>new( CONV #( p_bin_o ) ).
  ENDMETHOD.

  METHOD validate_single_config.
    rv_valid = abap_true.

    " Skip validation for disabled configs
    IF is_config-enabled = abap_false.
      RETURN.
    ENDIF.

    " Skip validation for placeholder values
    IF is_config-url CS '<' OR is_config-api_key CS '<'.
      RETURN.
    ENDIF.

    " Validate API key format
    DATA(lv_abc) = to_lower( sy-abcde ).
    DATA(lv_allowed) = sy-abcde && lv_abc && '0123456789-_'.
    IF is_config-api_key CN lv_allowed.
      MESSAGE e011 WITH |{ is_config-variant }: Invalid API key format|.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Validate URL format
    IF is_config-url NS 'azure.com/openai/'.
      MESSAGE e010 WITH |{ is_config-variant }: Invalid Azure OpenAI URL|.
      rv_valid = abap_false.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD validate_all_configs.
    rv_valid = abap_true.
    DATA: lv_enabled_count TYPE i.

    LOOP AT it_configs INTO DATA(ls_config).
      IF ls_config-enabled = abap_true.
        lv_enabled_count = lv_enabled_count + 1.
        IF validate_single_config( ls_config ) = abap_false.
          rv_valid = abap_false.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lv_enabled_count = 0.
      MESSAGE w000 WITH 'No LLM configurations enabled'.
      rv_valid = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD execute_test_all.
    DATA(lt_configs) = mo_config_manager->get_all_configs( ).

    IF validate_all_configs( lt_configs ) = abap_true.
      mo_tester->test_all_llms(
        it_configs  = lt_configs
        iv_question = p_test_q
      ).
    ENDIF.
  ENDMETHOD.

  METHOD execute_save_all.
    DATA(lt_configs) = mo_config_manager->get_all_configs( ).

    IF validate_all_configs( lt_configs ) = abap_true.
      mo_env_manager->save_all_env_files( lt_configs ).
    ENDIF.
  ENDMETHOD.

  METHOD execute_test_and_save.
    DATA(lt_configs) = mo_config_manager->get_all_configs( ).

    IF validate_all_configs( lt_configs ) = abap_true.
      " Test all enabled LLMs first
      IF mo_tester->test_all_llms(
        it_configs = lt_configs
        iv_question = p_test_q
      ) = abap_true.
        " If at least one test successful, save all enabled configs
        mo_env_manager->save_all_env_files( lt_configs ).
      ELSE.
        MESSAGE i000 WITH 'LLM tests failed. Environment files not saved.'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
"$. endregion }

" Global main instance
DATA: go_main TYPE REF TO lcl_main.

INITIALIZATION.
  p_bin_o = p_bin_o && '_' && sy-uname.
  go_main = lcl_main=>new( ).

  " Function key texts
  sscrfields-functxt_01 = 'Test All LLMs'.
  sscrfields-functxt_02 = 'Save All ENV'.
  sscrfields-functxt_03 = 'Test & Save All'.
  sscrfields-functxt_04 = 'Expert Mode'.

  DATA(gv_uh) = zcl_llm=>string_hash( CONV #( sy-uname ) ).

AT SELECTION-SCREEN OUTPUT.
  " Simplify screen for specific users
  LOOP AT SCREEN.
    DATA(lv_sg) = screen-group1+0(2).
    IF gv_expert_mode = 'X'.
      IF ( lv_sg = 'G2' OR lv_sg = 'G3' OR lv_sg = 'G4' ).
        "screen-input = 1.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF ( lv_sg = 'G2' OR lv_sg = 'G3' OR lv_sg = 'G4' ).
        "screen-input = 0.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF     p_defen = 'X'  AND screen-group1 = 'G1'.
      screen-input = 1.
      MODIFY SCREEN.
    ELSEIF p_defen NE 'X'  AND screen-group1 = 'G1'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

    IF     p_minen = 'X'  AND screen-group1 = 'G2'.
      screen-input = 1.
      MODIFY SCREEN.
    ELSEIF p_minen NE 'X'  AND screen-group1 = 'G2'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

    IF     p_maxen = 'X'  AND screen-group1 = 'G3'.
      screen-input = 1.
      MODIFY SCREEN.
    ELSEIF p_maxen NE 'X'  AND screen-group1 = 'G3'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

    IF     p_dp_en = 'X'  AND screen-group1 = 'G4'.
      screen-input = 1.
      MODIFY SCREEN.
    ELSEIF p_dp_en NE 'X'  AND screen-group1 = 'G4'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

    IF gv_uh NE '359EC10B9A96EB675C6C0B1F88257BAB01CD3EA1'.
      CASE screen-name.
        WHEN 'P_BIN_O'.
          screen-input = 0. "
          MODIFY SCREEN.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'ONLI'.
      MESSAGE i000 WITH 'Use function keys: F1=Test All, F2=Save All, F3=Test & Save All'.
      RETURN.

    WHEN 'FC01'. " Test All LLMs
      TRY.
          go_main->execute_test_all( ).
        CATCH zcx_s INTO DATA(lx_s).
          cl_demo_output=>write( lx_s->get_message_table( ) ).
          cl_demo_output=>write( lx_s->get_longtext( ) ).
          cl_demo_output=>display( ).
      ENDTRY.

    WHEN 'FC02'. " Save All ENV files
      TRY.
          go_main->execute_save_all( ).
        CATCH zcx_s INTO lx_s.
          cl_demo_output=>write( lx_s->get_message_table( ) ).
          cl_demo_output=>write( lx_s->get_longtext( ) ).
          cl_demo_output=>display( ).
      ENDTRY.

    WHEN 'FC03'. " Test & Save All
      TRY.
          go_main->execute_test_and_save( ).
        CATCH zcx_s INTO lx_s.
          cl_demo_output=>write( lx_s->get_message_table( ) ).
          cl_demo_output=>write( lx_s->get_longtext( ) ).
          cl_demo_output=>display( ).
      ENDTRY.

    WHEN 'FC04'. " Expert Mode (placeholder for future enhancement)
      gv_expert_mode = COND #( WHEN gv_expert_mode = 'X' THEN abap_false
        ELSE abap_true
      ).

      DATA(lv_exper_mode_txt) = COND string(
        WHEN gv_expert_mode = 'X' THEN 'On'
        ELSE 'Off'
      ).
      MESSAGE s000 WITH |Expert mode { lv_exper_mode_txt } |.
      IF gv_expert_mode = abap_false.
        CLEAR:
          "p_defen,
          p_maxen,
          p_minen,
          p_dp_en.
      ENDIF.

    WHEN OTHERS.
      " Default validation for enabled configurations
      TRY.
          DATA(lo_config_mgr) = lcl_config_manager=>new( ).
          go_main->validate_all_configs( lo_config_mgr->get_all_configs( ) ).
        CATCH zcx_s INTO lx_s.
          cl_demo_output=>write( lx_s->get_message_table( ) ).
          cl_demo_output=>write( lx_s->get_longtext( ) ).
          cl_demo_output=>display( ).
      ENDTRY.
  ENDCASE.
