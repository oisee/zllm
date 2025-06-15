INTERFACE zif_llm_00_types
  PUBLIC .

  TYPES: string_t TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

  "$. Region common, usage
  TYPES:
    BEGIN OF ts_usage,
      completion_tokens TYPE i,
      prompt_tokens     TYPE i,
      total_tokens      TYPE i,
    END OF ts_usage .
  "$. Endregion common, usage

  "$. Region chat out
  TYPES:
    BEGIN OF ts_function_call,
      name      TYPE string,
      arguments TYPE string,
    END OF ts_function_call .
  TYPES:
    BEGIN OF ts_message_out,
      role          TYPE string,
      content       TYPE string,
      function_call TYPE ts_function_call, "ts_function_call,
    END OF ts_message_out .
  TYPES:
    BEGIN OF ts_choices,
      text          TYPE string,
      index         TYPE i,
      finish_reason TYPE string,
      message       TYPE ts_message_out,
    END OF ts_choices .
  TYPES:
    tt_choices TYPE STANDARD TABLE OF ts_choices WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ts_chat_out,
      id                 TYPE string,
      choices            TYPE tt_choices,
      created            TYPE i,
      model              TYPE string,
      object             TYPE string,
      system_fingerprint TYPE string,
      usage              TYPE ts_usage,
    END OF ts_chat_out .
  "$. Endregion chat out

  "$. Region chat in
  TYPES:
    BEGIN OF ts_message_in,
      role    TYPE string,
      name    TYPE string,
      content TYPE string,
    END OF ts_message_in .
  TYPES:
    tt_message_in TYPE STANDARD TABLE OF ts_message_in WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ts_chat_in,
      model             TYPE string,
      response_format   TYPE REF TO zif_llm_00_json,
      messages          TYPE tt_message_in,
      max_tokens        TYPE i,
      user              TYPE string,                    "A unique identifier representing your end-user, which can help Azure OpenAI to monitor and detect abuse.
      n                 TYPE i,                         "How many chat completion choices to generate for each input message
      temperature       TYPE p LENGTH 3 DECIMALS 2,     "Temperature - Controls randomness.
      top_p             TYPE p LENGTH 3 DECIMALS 2,     "Top probabilities  - Controls randomness.
      stop              TYPE stringtab,                 "Make responses stop at a desired point, such as the end of a sentence or list
      frequency_penalty TYPE p LENGTH 3 DECIMALS 2,     "Reduce the chance of repeating a token proportionally based on how often it has appeared in the text so far
      presence_penalty  TYPE p LENGTH 3 DECIMALS 2,     "Reduce the chance of repeating any token that has appeared in the text at all so far
      functions         TYPE REF TO zif_llm_00_json,
    END OF ts_chat_in .

  TYPES:
    BEGIN OF ts_reasoning,
      model             TYPE string,
      response_format   TYPE REF TO zif_llm_00_json,
      input             TYPE tt_message_in,
      "max_tokens        TYPE i,
      user              TYPE string,                    "A unique identifier representing your end-user, which can help Azure OpenAI to monitor and detect abuse.
      n                 TYPE i,                         "How many chat completion choices to generate for each input message
      "temperature       TYPE p LENGTH 3 DECIMALS 2,     "Temperature - Controls randomness.
      top_p             TYPE p LENGTH 3 DECIMALS 2,     "Top probabilities  - Controls randomness.
      stop              TYPE stringtab,                 "Make responses stop at a desired point, such as the end of a sentence or list
      frequency_penalty TYPE p LENGTH 3 DECIMALS 2,     "Reduce the chance of repeating a token proportionally based on how often it has appeared in the text so far
      presence_penalty  TYPE p LENGTH 3 DECIMALS 2,     "Reduce the chance of repeating any token that has appeared in the text at all so far
      functions         TYPE REF TO zif_llm_00_json,
    END OF ts_reasoning .

  TYPES: BEGIN OF ts_prop,
           k TYPE string,
           v TYPE REF TO zif_llm_00_json,
         END OF ts_prop.
  TYPES tt_prop TYPE HASHED TABLE OF ts_prop WITH UNIQUE KEY k.
  TYPES: BEGIN OF ts_parameter,
           type       TYPE string,
           properties TYPE tt_prop, "REF TO zif_json,
           required   TYPE string_t,
         END OF ts_parameter.
  TYPES:
    BEGIN OF ts_function,
      name        TYPE string, "data_demonstration",
      description TYPE string, "This is the main function description",
      parameters  TYPE REF TO zif_llm_00_json,
    END OF ts_function .

  "$. Endregion chat in

  "$. Region embed in
  TYPES: BEGIN OF ts_embed_in,
           input TYPE string,
           model TYPE string,
         END OF ts_embed_in.
  "$. Endregion embed in
  "$. Region embed out
  TYPES ty_coord TYPE f .
  TYPES:
    tt_coord TYPE STANDARD TABLE OF ty_coord WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ts_embedding_out,
      object    TYPE string,
      index     TYPE i,
      embedding TYPE tt_coord,
    END OF ts_embedding_out .
  TYPES:
    tt_embedding_out TYPE STANDARD TABLE OF ts_embedding_out WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ts_embed_out,
      object TYPE string,
      data   TYPE tt_embedding_out,
      model  TYPE string,
      usage  TYPE ts_usage,
    END OF ts_embed_out .
  "$. Endregion embed out

  "$. Region q1b embedding vector
  TYPES: ty_x192(192) TYPE x.
  TYPES: ty_v TYPE ty_x192.
  TYPES: ty_vs TYPE xstring. "vector-string (to support different vector lenghts)
  "$. Endregion q1b embedding vector

  TYPES: BEGIN OF ts_env,
           api_url               TYPE string,
           api_ver               TYPE string,
           api_key               TYPE string,
           api_dep               TYPE string, "deployment for chat model
           api_dep_embed         TYPE string, "deployment for embedding model
           api_model             TYPE string,
           api_max_token         TYPE string,
           api_token_split_limit TYPE string,
           api_format            TYPE string, "blank or "response_format": { "type": "json_object" },
         END OF ts_env.

  TYPES: tt_file TYPE zif_llm_00_file_list=>tt_file.
  "  TYPES: tt_pd   TYPE STANDARD TABLE OF .
*--------------------------------------------------------------------*
  "$. Region reasoning out
  TYPES:
    BEGIN OF ts_usage_details,
      cached_tokens TYPE i,
    END OF ts_usage_details.

  TYPES:
    BEGIN OF ts_reasoning_usage_details,
      reasoning_tokens TYPE i,
    END OF ts_reasoning_usage_details.

  TYPES:
    BEGIN OF ts_reasoning_usage,
      input_tokens          TYPE i,
      output_tokens         TYPE i,
      total_tokens          TYPE i,
      input_tokens_details  TYPE ts_usage_details,
      output_tokens_details TYPE ts_reasoning_usage_details,
    END OF ts_reasoning_usage.

  TYPES:
    BEGIN OF ts_reasoning_info,
      effort  TYPE string,  " e.g. 'medium', 'high', 'low'
      summary TYPE string,
    END OF ts_reasoning_info.

  TYPES: tt_annotations TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ts_output_text,
      text        TYPE string,
      type        TYPE string,  " 'output_text'
      annotations TYPE tt_annotations,
    END OF ts_output_text.

  TYPES:
    tt_output_text TYPE STANDARD TABLE OF ts_output_text WITH DEFAULT KEY.

  TYPES: tt_summary TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ts_output_item,
      " This is a union type - can be either reasoning or message
      id      TYPE string,
      type    TYPE string,      " 'reasoning' or 'message'
      summary TYPE tt_summary,  " for reasoning type
      content TYPE tt_output_text,  " for message type
      role    TYPE string,      " for message type
      status  TYPE string,      " for message type
    END OF ts_output_item.

  TYPES:
    tt_output_items TYPE STANDARD TABLE OF ts_output_item WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ts_text_format,
      type TYPE string,  " 'text'
    END OF ts_text_format.

  TYPES:
    BEGIN OF ts_text_info,
      format TYPE ts_text_format,
    END OF ts_text_info.

  TYPES: tt_tools TYPE STANDARD TABLE OF REF TO data WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ts_reasoning_out,
      id                   TYPE string,
      object               TYPE string,     " 'response'
      created_at           TYPE i,
      model                TYPE string,     " 'o1-mini', 'o1-preview', etc.
      status               TYPE string,     " 'completed'
      output               TYPE tt_output_items,
      usage                TYPE ts_reasoning_usage,
      reasoning            TYPE ts_reasoning_info,
      error                TYPE string,
      incomplete_details   TYPE string,
      instructions         TYPE string,
      max_output_tokens    TYPE i,
      metadata             TYPE REF TO data,  " generic metadata
      parallel_tool_calls  TYPE abap_bool,
      previous_response_id TYPE string,
      service_tier         TYPE string,     " 'default'
      store                TYPE abap_bool,
      temperature          TYPE p LENGTH 3 DECIMALS 2,
      text                 TYPE ts_text_info,
      tool_choice          TYPE string,     " 'auto'
      tools                TYPE tt_tools,   " array of tools
      top_p                TYPE p LENGTH 3 DECIMALS 2,
      truncation           TYPE string,     " 'disabled'
      user                 TYPE string,
    END OF ts_reasoning_out.
  "$. Endregion reasoning out

ENDINTERFACE.
