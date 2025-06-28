CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_llm_00_dotenv DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      cut TYPE REF TO zcl_llm_00_dotenv.  "class under test

    METHODS: setup.

    " Basic functionality tests
    METHODS: test_new_from_string FOR TESTING.
    METHODS: tst_prs_simple_env FOR TESTING.
    METHODS: tst_prs_with_comments FOR TESTING.
    METHODS: tst_prs_empty_lines FOR TESTING.
    METHODS: test_v_existing_key FOR TESTING.
    METHODS: test_v_nonexistent_key FOR TESTING.
    METHODS: tst_get_conf__complete FOR TESTING.
    METHODS: tst_get_conf__partial FOR TESTING.
    METHODS: tst_get_conf__defaults FOR TESTING.
    METHODS: tp_azure_url_modern_model FOR TESTING.

    " Azure URL parsing tests
    METHODS: tst_prs_azure_full_url_valid FOR TESTING.
    METHODS: tst_prs_azure_url_invalid FOR TESTING.
    METHODS: tst_prs_azure_url_gpt4 FOR TESTING.
    METHODS: tst_get_conf_az_url_prec FOR TESTING.
    METHODS: tst_get_conf_model_override FOR TESTING.

    " Edge cases and error handling
    METHODS: tst_prs_malformed_lines FOR TESTING.
    METHODS: tst_prs_no_equals FOR TESTING.
    METHODS: test_get_default_env_path FOR TESTING.

ENDCLASS.

CLASS lcl_ IMPLEMENTATION.

  METHOD setup.
    " Basic setup with minimal valid data
    cut = zcl_llm_00_dotenv=>new_from_string( 'API_URL=https://test.openai.azure.com/' ).
  ENDMETHOD.

  METHOD test_new_from_string.
    " Test creating instance from string
    DATA(lv_test_content) = |API_KEY=test123{ cl_abap_char_utilities=>cr_lf }API_URL=https://example.com|.

    cut = zcl_llm_00_dotenv=>new_from_string( lv_test_content ).

    cl_abap_unit_assert=>assert_bound(
      act = cut
      msg = 'Object should be created successfully'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->v( 'API_KEY' )
      exp = 'test123'
      msg = 'API_KEY should be parsed correctly'
    ).
  ENDMETHOD.

  METHOD tst_prs_simple_env.
    " Test parsing basic key=value pairs
    DATA(lv_content) = |API_URL=https://example.openai.azure.com/{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_KEY=sk-test123{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_VER=2024-08-01-preview{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_DEP=gpt-4o-mini|.

    cut = zcl_llm_00_dotenv=>new_from_string( lv_content ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->v( 'API_URL' )
      exp = 'https://example.openai.azure.com/'
      msg = 'API_URL should be parsed correctly'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->v( 'API_KEY' )
      exp = 'sk-test123'
      msg = 'API_KEY should be parsed correctly'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->v( 'API_VER' )
      exp = '2024-08-01-preview'
      msg = 'API_VER should be parsed correctly'
    ).
  ENDMETHOD.

  METHOD tst_prs_with_comments.
    " Test parsing with comments and empty lines
    DATA(lv_content) = |# This is a comment{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_URL=https://example.com{ cl_abap_char_utilities=>cr_lf }| &&
                       |# Another comment{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_KEY=test123|.

    cut = zcl_llm_00_dotenv=>new_from_string( lv_content ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->v( 'API_URL' )
      exp = 'https://example.com'
      msg = 'Should parse values while ignoring comments'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->v( 'API_KEY' )
      exp = 'test123'
      msg = 'Should parse API_KEY correctly'
    ).
  ENDMETHOD.

  METHOD tst_prs_empty_lines.
    " Test parsing with various empty lines and whitespace
    DATA(lv_content) = |  { cl_abap_char_utilities=>cr_lf }| &&
                       |API_URL=https://example.com{ cl_abap_char_utilities=>cr_lf }| &&
                       |{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_KEY=test123{ cl_abap_char_utilities=>cr_lf }| &&
                       |  |.

    cut = zcl_llm_00_dotenv=>new_from_string( lv_content ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->v( 'API_URL' )
      exp = 'https://example.com'
      msg = 'Should handle empty lines correctly'
    ).
  ENDMETHOD.

  METHOD test_v_existing_key.
    " Test retrieving existing key
    DATA(lv_content) = |API_URL=https://test.com{ cl_abap_char_utilities=>cr_lf }API_KEY=secret123|.
    cut = zcl_llm_00_dotenv=>new_from_string( lv_content ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->v( 'API_KEY' )
      exp = 'secret123'
      msg = 'Should return correct value for existing key'
    ).
  ENDMETHOD.

  METHOD test_v_nonexistent_key.
    " Test retrieving non-existent key
    cut = zcl_llm_00_dotenv=>new_from_string( 'API_URL=https://test.com' ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->v( 'NONEXISTENT_KEY' )
      exp = ''
      msg = 'Should return empty string for non-existent key'
    ).
  ENDMETHOD.

  METHOD tst_get_conf__complete.
    " Test get_config with all values provided
    DATA(lv_content) = |API_URL=https://example.openai.azure.com/{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_VER=2024-08-01-preview{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_KEY=sk-test123{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_DEP=gpt-4o-mini{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_MODEL=custom-model{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_MAX_TOKEN=100000{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_TOKEN_SPLIT_LIMIT=80000|.

    cut = zcl_llm_00_dotenv=>new_from_string( lv_content ).
    DATA(ls_config) = cut->get_config( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_config-api_url
      exp = 'https://example.openai.azure.com/'
      msg = 'API_URL should be set correctly'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_config-api_model
      exp = 'custom-model'
      msg = 'API_MODEL should be set correctly'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = |{ ls_config-api_max_token }|
      exp = '100000'
      msg = 'API_MAX_TOKEN should be converted to number'
    ).
  ENDMETHOD.

  METHOD tst_get_conf__partial.
    " Test get_config with only some values
    DATA(lv_content) = |API_URL=https://example.com{ cl_abap_char_utilities=>cr_lf }API_KEY=test123|.

    cut = zcl_llm_00_dotenv=>new_from_string( lv_content ).
    DATA(ls_config) = cut->get_config( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_config-api_url
      exp = 'https://example.com'
      msg = 'API_URL should be set'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_config-api_key
      exp = 'test123'
      msg = 'API_KEY should be set'
    ).

    cl_abap_unit_assert=>assert_initial(
      act = ls_config-api_dep
      msg = 'API_DEP should be empty when not provided'
    ).
  ENDMETHOD.

  METHOD tst_get_conf__defaults.
    " Test that defaults are applied correctly
    cut = zcl_llm_00_dotenv=>new_from_string( 'API_URL=https://test.com' ).
    DATA(ls_config) = cut->get_config( ).

    cl_abap_unit_assert=>assert_equals(
      act = |{ ls_config-api_token_split_limit }|
      exp = `64000 `
      msg = 'Should apply default token split limit'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = |{ ls_config-api_max_token }|
      exp = `96000 `
      msg = 'Should apply default max token'
    ).
  ENDMETHOD.

  METHOD tst_prs_azure_full_url_valid.
    " Test parsing valid Azure OpenAI URL
    DATA(lv_content) = |API_AZURE_FULL_URL=https://myresource.openai.azure.com/openai/deployments/gpt-4o-mini/chat/completions?api-version=2024-08-01-preview{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_KEY=sk-test123|.

    cut = zcl_llm_00_dotenv=>new_from_string( lv_content ).
    DATA(ls_config) = cut->get_config( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_config-api_url
      exp = 'https://myresource.openai.azure.com/'
      msg = 'Should extract base URL correctly'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_config-api_ver
      exp = '2024-08-01-preview'
      msg = 'Should extract API version correctly'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_config-api_dep
      exp = 'gpt-4o-mini'
      msg = 'Should extract deployment name correctly'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_config-api_model
      exp = 'gpt-4o-mini'
      msg = 'Should set model to deployment name by default'
    ).
  ENDMETHOD.

  METHOD tst_prs_azure_url_invalid.
    " Test with invalid Azure URL (missing azure.com/openai/)
    DATA(lv_content) = |API_AZURE_FULL_URL=https://invalid-url.com/some/path{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_KEY=sk-test123|.

    cut = zcl_llm_00_dotenv=>new_from_string( lv_content ).
    DATA(ls_config) = cut->get_config( ).

    cl_abap_unit_assert=>assert_initial(
      act = ls_config-api_url
      msg = 'Should not extract URL from invalid format'
    ).
  ENDMETHOD.

  METHOD tst_prs_azure_url_gpt4.
    " Test GPT-4 model detection for higher token limits
    DATA(lv_content) = |API_AZURE_FULL_URL=https://test.openai.azure.com/openai/deployments/gpt-4-turbo-2024-04-09/chat/completions?api-version=2024-08-01-preview|.

    cut = zcl_llm_00_dotenv=>new_from_string( lv_content ).
    DATA(ls_config) = cut->get_config( ).

    cl_abap_unit_assert=>assert_equals(
      act = |{ ls_config-api_max_token }|
      exp = `96000 `
      msg = 'Should set higher token limit for GPT-4 models'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = |{ ls_config-api_token_split_limit }|
      exp = `64000 `
      msg = 'Should set higher split limit for GPT-4 models'
    ).
  ENDMETHOD.

  METHOD tst_get_conf_az_url_prec.
    " Test that individual vars take precedence over Azure URL
    DATA(lv_content) = |API_AZURE_FULL_URL=https://test.openai.azure.com/openai/deployments/gpt-35-turbo/chat/completions?api-version=2024-02-15-preview{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_URL=https://override.openai.azure.com/{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_VER=2024-08-01-preview{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_KEY=sk-test123|.

    cut = zcl_llm_00_dotenv=>new_from_string( lv_content ).
    DATA(ls_config) = cut->get_config( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_config-api_url
      exp = 'https://override.openai.azure.com/'
      msg = 'Individual API_URL should override Azure URL extraction'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_config-api_ver
      exp = '2024-08-01-preview'
      msg = 'Individual API_VER should override Azure URL extraction'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_config-api_dep
      exp = 'gpt-35-turbo'
      msg = 'Deployment should be from Azure URL when not overridden'
    ).
  ENDMETHOD.

  METHOD tst_get_conf_model_override.
    " Test that API_MODEL overrides the model extracted from deployment
    DATA(lv_content) = |API_AZURE_FULL_URL=https://test.openai.azure.com/openai/deployments/my-deployment-name/chat/completions?api-version=2024-08-01-preview{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_MODEL=gpt-4o-custom{ cl_abap_char_utilities=>cr_lf }| &&
                       |API_KEY=sk-test123|.

    cut = zcl_llm_00_dotenv=>new_from_string( lv_content ).
    DATA(ls_config) = cut->get_config( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_config-api_dep
      exp = 'my-deployment-name'
      msg = 'Deployment should be extracted from URL'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_config-api_model
      exp = 'gpt-4o-custom'
      msg = 'API_MODEL should override deployment name as model'
    ).
  ENDMETHOD.

  METHOD tst_prs_malformed_lines.
    " Test handling of malformed lines
    DATA(lv_content) = |VALID_KEY=value1{ cl_abap_char_utilities=>cr_lf }| &&
                       |MALFORMED_NO_EQUALS{ cl_abap_char_utilities=>cr_lf }| &&
                       |ANOTHER_VALID=value2{ cl_abap_char_utilities=>cr_lf }| &&
                       |=VALUE_WITHOUT_KEY|.

    cut = zcl_llm_00_dotenv=>new_from_string( lv_content ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->v( 'VALID_KEY' )
      exp = 'value1'
      msg = 'Should parse valid lines correctly'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->v( 'ANOTHER_VALID' )
      exp = 'value2'
      msg = 'Should continue parsing after malformed line'
    ).

    cl_abap_unit_assert=>assert_initial(
      act = cut->v( 'MALFORMED_NO_EQUALS' )
      msg = 'Should not set value for malformed line'
    ).
  ENDMETHOD.

  METHOD tst_prs_no_equals.
    " Test line with no equals sign
    cut = zcl_llm_00_dotenv=>new_from_string( 'JUST_A_LINE_WITHOUT_EQUALS' ).

    cl_abap_unit_assert=>assert_initial(
      act = cut->v( 'JUST_A_LINE_WITHOUT_EQUALS' )
      msg = 'Should not parse line without equals sign'
    ).
  ENDMETHOD.

  METHOD test_get_default_env_path.
    " Test getting default environment path
    DATA(lv_path) = zcl_llm_00_dotenv=>get_default_env_path( ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_path
      msg = 'Should return non-empty default path'
    ).

    " Should contain some form of .env
    cl_abap_unit_assert=>assert_true(
      act = xsdbool( lv_path CS '.env' )
      msg = 'Default path should contain .env'
    ).
  ENDMETHOD.

  METHOD tp_azure_url_modern_model.
    " Test that non-GPT models also get modern 128k defaults
    DATA(lv_content) = |API_AZURE_FULL_URL=https://test.openai.azure.com/openai/deployments/claude-3-sonnet/chat/completions?api-version=2024-08-01-preview|.

    cut = zcl_llm_00_dotenv=>new_from_string( lv_content ).
    DATA(ls_config) = cut->get_config( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_config-api_max_token
      exp = `96000 `
      msg = 'Should set modern 128k token limit for all modern models'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_config-api_token_split_limit
      exp = `64000 `
      msg = 'Should set modern 128k split limit for all modern models'
    ).
  ENDMETHOD.
ENDCLASS.
