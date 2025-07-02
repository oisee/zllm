*&---------------------------------------------------------------------*
*& Report ZLLM_00_REPL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zllm_00_repl MESSAGE-ID zllm_00.

TABLES: sscrfields, screen. " tfk047b." zcol01shbg01.  zcol_177_map,
TABLES: wwwdata. "W3OBJID

TYPE-POOLS: vrm, icon, cntb.
CLASS lcl_htm_event_handler DEFINITION DEFERRED.

CONSTANTS: gc_url_f1 TYPE string VALUE 'https://www.markdownguide.org/basic-syntax/'. ##NO_TEXT
TYPES: tv_url TYPE text1024.                               "~ Defines a type for URLs, supporting up to 1024 characters.
TYPES: ts_spl TYPE zcl_llm_00_spl=>ts_.                   "~ Type alias for a structure from class zcl_llm_00_spl, facilitating type reuse.
TYPES: tt_spl TYPE zcl_llm_00_spl=>tt_.                   "~ Defines a table type based on the structure from zcl_llm_00_spl.
TYPES:   tt_ted TYPE zcl_llm_00_str_to_ted=>tt_.          "~ Defines a table type for text editor data, facilitating batch processing.

DATA: go_llm TYPE REF TO zif_llm_00_llm_lazy.

DATA: gv_css TYPE string.                                  "~ Global variable for CSS styling as a string.
DATA: gv_cust TYPE sap_bool.                               "~ Indicator for custom processing logic, stored as a boolean.
DATA: gtr_ TYPE zcl_llm_00_tvarvc=>ttr_.                  "~ filter for names
*--------------------------------------------------------------------

DATA:
  go_gui  TYPE REF TO cl_gui_custom_container,             "~ Reference to a custom GUI container for dynamic UI elements.
  go_html TYPE REF TO cl_gui_html_viewer.                  "~ Reference to an HTML viewer control, for rendering HTML content.

DATA: go_htm TYPE REF TO cl_gui_container.                 "~ Reference to a generic container, potentially for layout management.
DATA: go_mtx TYPE REF TO cl_gui_container. "multi-split-text-editor    "~ Reference to a container for a multi-split text editor interface.
DATA: gt_spl TYPE tt_spl.                                  "~ Table of split screen configurations.
DATA: gt_ted TYPE tt_ted.                                  "~ Table for managing multiple text editor instances.
DATA: go_stt TYPE REF TO zcl_llm_00_str_to_ted. "zcl_llm_00_str_to_ted.  "~ Instance of class for converting string to text editor data.

DATA: gr_handler   TYPE REF TO /ltb/if_tr_bas_html_ctrl_hdl.     "~ Handler reference for HTML control events.
DATA: gt_html TYPE TABLE OF soli,                          "~ Table for storing HTML content in SOLI format.
      gv_html TYPE string.                                 "~ Variable for concatenated HTML content as a string.

"DATA: gr_ TYPE REF TO data.
"DATA: gs_ TYPE ts_.
DATA: mo_s1 TYPE REF TO zif_llm_00_step_lazy.
DATA: mo_s2 TYPE REF TO zif_llm_00_step_lazy.
DATA: mo_s3 TYPE REF TO zif_llm_00_step_lazy.
DATA: mo_s4 TYPE REF TO zif_llm_00_step_lazy.


TYPES: BEGIN OF ts_fm,                                     "~ Structure for function module parameters.
         i_0 TYPE string,                                  "~ String parameter for function module.
         u_0 TYPE string,                                  "~ String parameter for function module.
         s_0 TYPE string,                                  "~ String parameter for function module.
         o_0 TYPE string,                                  "~ String parameter for function module.
         i_1 TYPE string,                                  "~ String parameter for function module.
         u_1 TYPE string,                                  "~ String parameter for function module.
         s_1 TYPE string,                                  "~ String parameter for function module.
         o_1 TYPE string,                                  "~ String parameter for function module.
         i_2 TYPE string,                                  "~ String parameter for function module.
         u_2 TYPE string,                                  "~ String parameter for function module.
         s_2 TYPE string,                                  "~ String parameter for function module.
         o_2 TYPE string,                                  "~ String parameter for function module.
         i_3 TYPE string,                                  "~ String parameter for function module.
         u_3 TYPE string,                                  "~ String parameter for function module.
         s_3 TYPE string,                                  "~ String parameter for function module.
         o_3 TYPE string,                                  "~ String parameter for function module.
         i_4 TYPE string,                                  "~ String parameter for function module.
         u_4 TYPE string,                                  "~ String parameter for function module.
         s_4 TYPE string,                                  "~ String parameter for function module.
         o_4 TYPE string,                                  "~ String parameter for function module.

       END OF ts_fm.

DATA: gs_fm TYPE ts_fm.                                    "~ Global structure for function module parameters.
DATA: gr_fm TYPE REF TO ts_fm.                             "~ Reference to the global structure for function module parameters.

DATA: gv_ok TYPE sy-ucomm.                                 "~ Variable to capture user command from the UI.

DATA: gt_tpl TYPE vrm_values.                              "~ Table for storing value help values.

CLASS lcl_htm_event_handler DEFINITION CREATE PRIVATE. "event receiver for html
  "~ Definition of a private class for handling HTML events.
  PUBLIC SECTION.

    CLASS-METHODS: new
      RETURNING VALUE(ro_) TYPE REF TO lcl_htm_event_handler.
    METHODS: onsapevent FOR EVENT sapevent OF cl_gui_html_viewer       "~ Method for handling SAP event triggers from HTML viewer.
      "~ Marks the end of the class definition.
      IMPORTING action
                frame
                getdata
                postdata
                query_table.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_htm_event_handler IMPLEMENTATION.
  "~ Implementation of the class for handling HTML events.
  METHOD new.
    "~ Factory method to instantiate the event handler class.

    ro_ = NEW #( ).
  ENDMETHOD.
  METHOD onsapevent.
    "~ Method implementation for processing SAP events triggered in HTML viewer.
  ENDMETHOD.
ENDCLASS.



CLASS lcl_ DEFINITION CREATE PRIVATE.
  "~ Definition of a private class for managing application logic.
  PUBLIC SECTION.
    INTERFACES: zif_llm_00_trace.

    TYPES: BEGIN OF ts_html_viewer,
             gui TYPE string,
             hv  TYPE REF TO cl_gui_html_viewer,
           END OF ts_html_viewer.
    TYPES: tt_html_viewer TYPE STANDARD TABLE OF ts_html_viewer WITH KEY gui.

*    TYPES: ttr_template TYPE zcl_col_177_template=>ttr_template.       "~ Type definition for a template table.
*    TYPES: ts_map TYPE zcol_177_map,                       "~ Structure for mapping values.
*           tt_map TYPE TABLE OF ts_map WITH DEFAULT KEY.


    CLASS-METHODS: new RETURNING VALUE(ro_) TYPE REF TO lcl_.
    METHODS: f1.                                           "~ Method placeholder for functionality.
    METHODS: go.                                           "~ Method placeholder for functionality.
    METHODS: run       IMPORTING io_ TYPE REF TO zif_llm_00_flow_lazy RAISING zcx_s.
    METHODS: exec_step1 IMPORTING io_ TYPE REF TO zif_llm_00_step_lazy RAISING zcx_s.
    METHODS: exec_step2 IMPORTING io_ TYPE REF TO zif_llm_00_step_lazy RAISING zcx_s.
    METHODS: exec_step3 IMPORTING io_ TYPE REF TO zif_llm_00_step_lazy RAISING zcx_s.
    METHODS: exec_step4 IMPORTING io_ TYPE REF TO zif_llm_00_step_lazy RAISING zcx_s.
    METHODS: compile RETURNING VALUE(ro_) TYPE REF TO zif_llm_00_flow_lazy RAISING zcx_s.
    METHODS:
      previous_version,                                    "~ Method for navigating to the previous version.
      next_version,                                        "~ Method for navigating to the next version.
      last_version,                                        "~ Method for navigating to the last version.
      first_version,                                       "~ Method for navigating to the first version.
      previous_approved_version,                           "~ Method for navigating to the previous approved version.
      next_approved_version,                               "~ Method for navigating to the next approved version.
      send_for_approval RAISING zcx_s.                     "~ Method for sending the current version for approval.

    METHODS:
      read_defaults        RETURNING VALUE(rv_) TYPE sap_bool RAISING zcx_s.  "~ Method for reading default settings.

    METHODS: refresh_screen_status RAISING zcx_s.          "~ Method for refreshing the screen status.
    METHODS: pbo_0100 RAISING zcx_s.                       "~ Process Before Output handler for screen 0100.

    METHODS: show_log.                                     "~ Method for displaying logs.
    METHODS: sync_html
      IMPORTING v   TYPE string
                p   TYPE string
                gui TYPE string.
    METHODS: render_metadata IMPORTING is_ TYPE dfkkcoh RAISING zcx_s.      "~ Method for rendering metadata.
    METHODS: render_f1 RETURNING VALUE(rv_) TYPE tv_url.   "~ Method for rendering F1 help.

    METHODS: add_to_transport RETURNING VALUE(rv_) TYPE sap_bool RAISING zcx_s. "~ Method for adding the current session to a transport request.
    METHODS: save IMPORTING iv_confirm TYPE sap_bool DEFAULT 'X' RAISING zcx_s. "~ Method for saving the current session.
    METHODS: load RAISING zcx_s.
    METHODS: confirm_exit RETURNING VALUE(rv_) TYPE sap_bool.    "~ Method for confirming exit when unsaved changes exist.

    METHODS: split_screen.                                 "~ Method for splitting the screen into multiple sections.
    METHODS: reset_menu.                                   "~ Method for resetting the menu to its default state.

    METHODS: string_from_json IMPORTING iv_json TYPE string RETURNING VALUE(rv_) TYPE string. "~ Method for converting a JSON string into an ABAP string.

    METHODS: setup_screen.                                 "~ Method for setting up the initial screen layout.
    TYPES:
      BEGIN OF ts_tmpl,                                    "~ Structure for template data.
        i(40) TYPE c,                                      "~ Template key.
        k     TYPE string,             "~ Template body.
        v     TYPE string,             "~ Template text.
      END OF ts_tmpl.
    TYPES tt_tmpl TYPE STANDARD TABLE OF ts_tmpl WITH KEY i.
    DATA: mt_tmpl TYPE tt_tmpl.                            "~ Table for storing multiple templates.


    METHODS: sync_ted.
    METHODS: is_ted_modified RETURNING VALUE(rv_) TYPE sap_bool.
    METHODS: setup_menu.                                   "~ Method for setting up the menu.
    METHODS: load_templates.                               "~ Method for loading templates from storage.
    METHODS: add_language IMPORTING iv_ TYPE string OPTIONAL.    "~ Method for adding a new language option.
    METHODS: launch_approve IMPORTING iv_ TYPE string OPTIONAL,  "~ Method for launching the approval process.

      on_function_selected FOR EVENT function_selected OF cl_gui_toolbar    "~ Method for handling function selection events.
        "~ Marks the end of the class definition.
        IMPORTING
          sender
          fcode,

      on_function_selected_lang FOR EVENT function_selected OF cl_gui_toolbar "~ Method for handling language selection events.
        "~ Marks the end of the class definition.
        IMPORTING
          sender
          fcode.

    METHODS select_corr_key RETURNING VALUE(rs_) TYPE dfkkcoh.   "~ Method for selecting a correspondence key.
    METHODS get_dfkkcoh     IMPORTING iv_ TYPE dfkkcoh-cokey RETURNING VALUE(rs_) TYPE dfkkcoh. "~ Method for retrieving dfkkcoh data based on a key.

*    DATA: mv_template     TYPE zcol_177_vers-template.     "~ Template version.
*    DATA: mv_lang         TYPE zcol_177_vers-spras.        "~ Language version.
*    DATA: mv_dun_level    TYPE zcol_177_vers-dun_level.    "~ Dunning level version.
*    DATA: mv_cokey        TYPE zcol_177_vers-cokey.        "~ Correspondence key.
*    DATA: ms_dfkkcoh      TYPE dfkkcoh.                    "~ Structure for dfkkcoh data.
*    DATA: ms_map          TYPE zcol_177_map.               "~ Structure for mapping data.
    METHODS:
*      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING sender e_row_id e_column_id,
*      on_toolbar       FOR EVENT toolbar       OF cl_gui_alv_grid IMPORTING e_object e_interactive,
*      on_user_command  FOR EVENT user_command  OF cl_gui_alv_grid IMPORTING e_ucomm,
      on_double_click           FOR EVENT double_click  OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no. "~ Method for handling double-click events in ALV grids.
    "~ Marks the end of the class definition.

  PRIVATE SECTION.
    DATA: gv_msg TYPE string.                              "~ Private variable for storing messages.
    DATA: gv_url_f1 TYPE tv_url.                           "~ Private variable for storing the F1 help URL.
*    DATA: mo_alv_f1 TYPE REF TO zcl_eui_alv.               "~ Reference to an ALV instance for F1 help.
*    DATA: mo_alv_lang TYPE REF TO zcl_eui_alv.             "~ Reference to an ALV instance for language selection.
    TYPES: BEGIN OF ts_dfkkcoh,                            "~ Structure for dfkkcoh data.
             cotyp TYPE dfkkcoh-cotyp,                     "~ Correspondence type.
             cokey TYPE dfkkcoh-cokey,                     "~ Correspondence key.
             cdate TYPE dfkkcoh-cdate,                     "~ Correspondence date.
             ctime TYPE dfkkcoh-ctime,                     "~ Correspondence time.
             bukrs TYPE dfkkcoh-bukrs,                     "~ Company code.
             gpart TYPE dfkkcoh-gpart,                     "~ Business partner.
             vkont TYPE dfkkcoh-vkont,                     "~ Contract account.
             uname TYPE dfkkcoh-uname,                     "~ User name.
           END OF ts_dfkkcoh,
           tt_dfkkcoh TYPE STANDARD TABLE OF ts_dfkkcoh WITH KEY cotyp cokey.
    DATA: mt_cokey_f1 TYPE tt_dfkkcoh.                     "~ Table for storing correspondence keys for F1 help.
    METHODS:  get_html_viewer
      IMPORTING iv_        TYPE string
      RETURNING VALUE(ro_) TYPE REF TO cl_gui_html_viewer.
    DATA: mt_hv TYPE tt_html_viewer.
    DATA: mt_cache TYPE STANDARD TABLE OF zllm_00_cache.

ENDCLASS.
*--------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b05 WITH FRAME TITLE TEXT-b05.
  PARAMETERS p_debug1 AS CHECKBOX.
  PARAMETERS p_debug2 AS CHECKBOX.
  PARAMETERS p_debug3 AS CHECKBOX.
  PARAMETERS p_debug4 AS CHECKBOX.
  PARAMETERS p_debug5 AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b05.

SELECTION-SCREEN BEGIN OF BLOCK b10 WITH FRAME TITLE b10.
SELECTION-SCREEN END OF BLOCK b10.
*--------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b20 WITH FRAME TITLE TEXT-b20.
SELECTION-SCREEN END OF BLOCK b20.

*SELECTION-SCREEN FUNCTION KEY 1.
*SELECTION-SCREEN FUNCTION KEY 2.
*SELECTION-SCREEN FUNCTION KEY 3.
*SELECTION-SCREEN FUNCTION KEY 4.

SELECTION-SCREEN BEGIN OF SCREEN 1020 AS SUBSCREEN.
SELECTION-SCREEN END OF SCREEN 1020.

*--------------------------------------------------------------------
INITIALIZATION.
  DATA(go_log) = zcl_blog=>new( object = 'ZCX_' ).         "~ Instantiates the logging class for error handling.
  DATA(go_) = lcl_=>new( ).                                "~ Instantiates the main class for application logic.
  go_->setup_screen( ).                                    "~ Initial setup of the screen layout.

*  DATA(go_fl) = zcl_llm_00_file_list_smw0=>new_from_package(
*    iv_          = '$ZLLM_03_YAAC'
*    iv_mask      = '*.env'
*    iv_namespace = 'Z'
*  ).
*
*  DATA(go_env) = go_fl->get_by_name( 'local.env' ).
*  go_llm = zcl_llm_00_llm_lazy=>new_from_file( go_env ).
  go_llm = zcl_llm=>new( )->get_llm( ). "zcl_ray_00_llm=>new( )->get_llm( ).

MODULE pbo_0100 OUTPUT.
  go_->pbo_0100( ).                                        "~ Calls the method to process Before Output logic for screen 0100.
ENDMODULE.

MODULE pai_0100.

  CASE gv_ok.

    WHEN 'ZSAVE'.
      go_->save( ).

    WHEN 'ZLOAD'.
      go_->load( ).
    WHEN 'ZCOMPILE'.
      "go_->save( ).                                        "~ Saves the current session.
      DATA(mo_flow) = go_->compile( ).

    WHEN 'ZRUN'. "or 'F1'.
      IF mo_flow IS NOT BOUND.
        mo_flow = go_->compile( ).
      ENDIF.
      go_->run( mo_flow ).
      RETURN.

    WHEN 'Z1'. "or 'F1'.
      go_->exec_step1( mo_s1 ).
      RETURN.

    WHEN 'Z2'. "or 'F1'.
      go_->exec_step2( mo_s2 ).
      RETURN.

    WHEN 'Z3'. "or 'F1'.
      go_->exec_step3( mo_s3 ).
      RETURN.

    WHEN 'Z4'. "or 'F1'.
      go_->exec_step4( mo_s4 ).
      RETURN.

    WHEN 'ZPREVIEW'. "OR 'ZDBLCLK'.
      go_->sync_ted( ).

    WHEN OTHERS.
  ENDCASE.
  CLEAR gv_ok.
ENDMODULE.
*--------------------------------------------------------------------
CLASS lcl_ IMPLEMENTATION.
  "~ Marks the start of the class implementation.
  METHOD new.
    "~ Factory method to instantiate the main class.

    ro_ = NEW #( ).

    zcl_llm_00_step_result=>_debug(
      iv_ = p_debug1
    ).

    zcl_llm_00_flow_lazy=>_debug(
      iv_ = p_debug2
      io_ = ro_
    ).

    zcl_llm_00_flow_lazy_parallel=>_debug(
      iv_ = p_debug3
      io_ = ro_
    ).

    zcl_llm_00_step_lazy=>_debug(
      iv_ = p_debug4
      io_ = ro_
    ).

    zcl_llm_00_step_lazy_parallel=>_debug(
      iv_ = p_debug5
      io_ = ro_
    ).

  ENDMETHOD.

  METHOD setup_screen.
    "~ Sets up the initial screen layout.

  ENDMETHOD.
  METHOD add_language.
    "~ Adds a new language option.
  ENDMETHOD.

  METHOD launch_approve.
    "~ Launches the approval process.
  ENDMETHOD.
  METHOD previous_approved_version.
    "~ Navigates to the previous approved version.
  ENDMETHOD.
  METHOD next_approved_version.
    "~ Navigates to the next approved version.
  ENDMETHOD.
  METHOD previous_version.
    "~ Navigates to the previous version.
  ENDMETHOD.
  METHOD next_version.
    "~ Navigates to the next version.
  ENDMETHOD.
  METHOD last_version.
    "~ Navigates to the last version.
  ENDMETHOD.
  METHOD first_version.
    "~ Navigates to the first version.
  ENDMETHOD.

  METHOD refresh_screen_status.
    "~ Refreshes the screen status based on current context.
    DATA lt_fcode TYPE STANDARD TABLE OF sy-ucomm.
*--------------------------------------------------------------------*
    SET PF-STATUS 'ZLLM_00_REPL' EXCLUDING lt_fcode.

  ENDMETHOD.

  METHOD pbo_0100.
    "~ Process Before Output logic for screen 0100.
    go_->refresh_screen_status( ).                         "~ Refreshes the screen status.
    go_->split_screen( ). "and assign subscreens, and create text editors   "~ Splits the screen into multiple sections.
    go_->setup_menu( ).                                    "~ Sets up the menu based on the current context.

    IF gv_ok = 'F1' OR
       gv_ok = 'F2'.

      RETURN.
    ENDIF.
*    go_->render_html( ms_dfkkcoh ).                        "~ Renders HTML content based on dfkkcoh data.
  ENDMETHOD.

  METHOD send_for_approval.
    "~ Sends the current version for approval.
  ENDMETHOD.
  METHOD get_dfkkcoh.
    "~ Retrieves dfkkcoh data based on a key.
  ENDMETHOD.

  METHOD select_corr_key.
    "~ Selects a correspondence key.

  ENDMETHOD.

  METHOD  on_double_click.
    "~ Handles double-click events in ALV grids.
*      on_double_click  FOR EVENT double_click  OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.
    DATA(lr_) = REF #( mt_cache[ e_row-index ] OPTIONAL ).
    IF lr_ IS NOT BOUND.
      RETURN.
    ENDIF.
*    DATA(lo_codec) = zcl_llm_00_codec=>new( ).
*    DATA(lv_json_x)  = lo_codec->decode( lr_->v ).
    DATA(lv_json) = zcl_llm=>xstring_to_string( lr_->v ).

    zcl_llm_00_json=>from_json(
      EXPORTING
        json = lv_json
"       jsonx            = lv_json
      CHANGING
        data = gs_fm
    ).

    gt_ted[ name = 'I_1' ]-eh->update_from_ref( REF #( gs_fm-i_1 ) ).
    gt_ted[ name = 'O_1' ]-eh->update_from_ref( REF #( gs_fm-o_1 ) ).
    gt_ted[ name = 'O_2' ]-eh->update_from_ref( REF #( gs_fm-o_2 ) ).
    gt_ted[ name = 'O_3' ]-eh->update_from_ref( REF #( gs_fm-o_3 ) ).

    sync_html( v = gs_fm-o_1 p = gs_fm-u_2 gui = 'I_2' ).
    sync_html( v = gs_fm-o_2 p = gs_fm-u_3 gui = 'I_3' ).
    sync_html( v = gs_fm-o_3 p = gs_fm-u_4 gui = 'I_4' ).
    sync_html( v = gs_fm-o_4 p = '' gui = 'O_4' ).


*    mv_cokey = lr_->cokey.
**--------------------------------------------------------------------*
*    IF go_html IS NOT BOUND.
*      RETURN.
*    ENDIF.
*    ms_dfkkcoh = get_dfkkcoh( mv_cokey ).
*    TRY.
*        go_->render_html( ms_dfkkcoh ).
*      CATCH zcx_s INTO DATA(lx_s).
*        zcl_cpu=>o( lx_s ).
*    ENDTRY.
  ENDMETHOD.

  METHOD read_defaults.
    "~ Reads default settings.
  ENDMETHOD.

  METHOD load_templates.
    "~ Loads templates from storage.
  ENDMETHOD.

  METHOD f1.
    "~ Executes the F1 help functionality.

    IF gv_url_f1 IS INITIAL.
      gv_url_f1 = me->render_f1( ).                        "~ Renders the F1 help URL.
    ENDIF.
    go_html->show_url( gv_url_f1 ).                        "~ Displays the F1 help content in the HTML viewer.
    "cl_gui_cfw=>flush( ).
    "go_html->show_url_in_browser( gv_url_f1 ).
  ENDMETHOD.

  METHOD render_f1.
    "~ Renders the F1 help content.
    "DATA:lo_html TYPE REF TO cl_gui_html_viewer.
    "   DATA(lo_file) = NEW zcl_xtt_file_smw0( 'ZCOL_177-F1.MHT' ).

*    DATA(lo_file) = NEW zcl_xtt_file_smw0( 'ZCOL_177-F1.MHT' ).

*    lo_file->zif_xtt_file~get_content( IMPORTING ev_as_xstring = DATA(lvx_html) )."lo_xtt->get_raw(  ). "~ Retrieves the content of the F1 help file.
*    DATA(lv_html) = cl_bcs_convert=>xstring_to_string( "~ Converts the xstring content to a string.
*      iv_xstr = lvx_html
*      iv_cp   = '4110'   "UTF
*    ).
*
*    DATA: lt_html TYPE TABLE OF soli.                      "~ Table for storing HTML content in SOLI format.
*    CLEAR lt_html.
*
*
*    CALL FUNCTION 'SO_STRING_TO_TAB'
*      EXPORTING
*        content_str = lv_html
*      TABLES
*        content_tab = lt_html.
*
*    DATA: lv_url TYPE text1024.                            "~ Variable for storing the URL of the loaded HTML content.
*
*    go_html->load_data(                                    "~ Loads HTML content into the HTML viewer.
*      EXPORTING
*        type         = 'multipart' " Type of a MIME Object      ##NO_TEXT
*        subtype      = 'related'   " Subtype of a MIME Object   ##NO_TEXT
*        encoding     = 'UTF-8'     " Encoding for MIME Object   ##NO_TEXT
*        language     = 'E' ##NO_TEXT
*      IMPORTING
*        assigned_url = lv_url
*      CHANGING
*        data_table   = lt_html
*    ).
*
*    rv_ = lv_url.

  ENDMETHOD.

  METHOD go.
    "~ Placeholder method.
  ENDMETHOD.
  METHOD compile.
    me->sync_ted( ).
    cl_gui_cfw=>flush( ).

    DATA(lv_json) = zcl_llm_00_json=>to_json( gs_fm ).
    IF p_debug1 = 'X'.
      cl_demo_output=>display( lv_json ).
    ENDIF.

    mo_s1 = zcl_llm_00_step_lazy=>new_from_formula(
      io_            = zcl_llm_00_formula=>new_from_usr_and_sys(
                         iv_user    = gs_fm-u_1
                         iv_system  = gs_fm-s_1
                         iv_name    = 'a1'
                       )
      io_llm         = go_llm
      iv_detect_json = 'X'
    ).

    mo_s2 = zcl_llm_00_step_lazy=>new_from_formula(
      io_            = zcl_llm_00_formula=>new_from_usr_and_sys(
                         iv_user    = gs_fm-u_2
                         iv_system  = gs_fm-s_2
                         iv_name    = 'a2'
                       )
      io_llm         = go_llm
      iv_detect_json = 'X'
    ).

    mo_s3 = zcl_llm_00_step_lazy=>new_from_formula(
      io_            = zcl_llm_00_formula=>new_from_usr_and_sys(
                         iv_user    = gs_fm-u_3
                         iv_system  = gs_fm-s_3
                         iv_name    = 'a3'
                       )
      io_llm         = go_llm
      iv_detect_json = 'X'
    ).

    mo_s4 = zcl_llm_00_step_lazy=>new_from_formula(
      io_            = zcl_llm_00_formula=>new_from_usr_and_sys(
                         iv_user    = gs_fm-u_4
                         iv_system  = gs_fm-s_4
                         iv_name    = 'a4'
                       )
      io_llm         = go_llm
      iv_detect_json = 'X'
    ).

    ro_ = zcl_llm_00_flow_lazy=>new(
      VALUE #(
        ( mo_s1 )
        ( mo_s2 )
        ( mo_s3 )
        ( mo_s4 )
    )
    ).
  ENDMETHOD.

  METHOD run.
    go_->exec_step1( mo_s1 ).
    go_->exec_step2( mo_s2 ).
    go_->exec_step3( mo_s3 ).
    go_->exec_step4( mo_s4 ).
    RETURN.

*    DATA lr_out TYPE REF TO data.
*    io_->next( EXPORTING ir_ = REF #( gs_fm-i_1 )
*               IMPORTING er_ = lr_out ).
*    DATA(lv_json) = zcl_llm_00_json=>to_json( lr_out ).
*    cl_demo_output=>display_text( lv_json ).
*    WHILE io_->next( IMPORTING er_ = lr_out ).
*      lv_json = zcl_llm_00_json=>to_json( lr_out ).
*      cl_demo_output=>display_text( lv_json ).
*    ENDWHILE.
*    lv_json = zcl_llm_00_json=>to_json( lr_out ).
*    cl_demo_output=>display_text( lv_json ).
**--------------------------------------------------------------------*
*    RETURN.

    "~ Executes the main application logic.
    DATA(lo_step) = io_->to_step( ).
    DATA(lo_res) = lo_step->start( REF #( gs_fm-i_1 ) ).
    DATA(lv_out) = lo_res->to_string( ).
*    lv_json = zcl_llm_00_json=>to_json( lr_out ).
    DATA(lo_md) = zcl_llm_00_markdown=>new( ).
    cl_demo_output=>display_html( lo_md->text( lv_out ) ).

  ENDMETHOD.

  METHOD exec_step1.
    mo_flow = me->compile( ).
    gs_fm-i_1 = condense( to_lower( gs_fm-i_1 ) ).
    DATA(lo_res) = io_->start( zcl_llm_00_json=>js_to_dref( gs_fm-i_1 ) ).
    DATA(lv_out) = lo_res->to_string( ).
    gs_fm-o_1 = lv_out.
    gs_fm-i_2 = condense( lv_out ).
*   gt_ted[ name = 'I_2' ]-eh->update_from_ref( REF #( gs_fm-i_2 ) ).
    sync_html( v = gs_fm-o_1 p = gs_fm-u_2 gui = 'I_2' ).
*   sync_html( v = gs_fm-o_1 p = '' gui = 'O_1' ).
    gt_ted[ name = 'O_1' ]-eh->update_from_ref( REF #( gs_fm-o_1 ) ).
    cl_gui_cfw=>flush( ).                                "~ Flushes the GUI to ensure all changes are rendered.
  ENDMETHOD.
  METHOD exec_step2.
    mo_flow = me->compile( ).
    DATA(lo_res) = io_->start( zcl_llm_00_json=>js_to_dref( gs_fm-i_2 ) ).
    DATA(lv_out) = lo_res->to_string( ).
    gs_fm-o_2 = lv_out.
    gs_fm-i_3 = condense( lv_out ).
*   gt_ted[ name = 'I_3' ]-eh->update_from_ref( REF #( gs_fm-i_3 ) ).
    sync_html( v = gs_fm-o_2 p = gs_fm-u_3 gui = 'I_3' ).
*   sync_html( v = gs_fm-o_2 p = '' gui = 'O_2' ).
    gt_ted[ name = 'O_2' ]-eh->update_from_ref( REF #( gs_fm-o_2 ) ).
    cl_gui_cfw=>flush( ).                                "~ Flushes the GUI to ensure all changes are rendered.
  ENDMETHOD.
  METHOD exec_step3.
    mo_flow = me->compile( ).
    "DATA(lo_res) = io_->start( REF #( gs_fm-i_3 ) ).
    DATA(lo_res) = io_->start( zcl_llm_00_json=>js_to_dref( gs_fm-i_3 ) ).
    DATA(lv_out) = lo_res->to_string( ).
*   DATA(lo_md) = zcl_llm_00_markdown=>new( ).
    gs_fm-o_3 = lv_out.
    gs_fm-i_4 = condense( lv_out ).
*    gt_ted[ name = 'I_4' ]-eh->update_from_ref( REF #( gs_fm-i_4 ) ).
    sync_html( v = gs_fm-o_3 p = gs_fm-u_4 gui = 'I_4' ).
*    sync_html( v = gs_fm-o_3 p = '' gui = 'O_3' ).
    gt_ted[ name = 'O_3' ]-eh->update_from_ref( REF #( gs_fm-o_3 ) ).
    cl_gui_cfw=>flush( ).                                "~ Flushes the GUI to ensure all changes are rendered.
  ENDMETHOD.
  METHOD exec_step4.
    mo_flow = me->compile( ).
    DATA(lo_res) = io_->start( zcl_llm_00_json=>js_to_dref( gs_fm-i_4 ) ).
    DATA(lv_out) = lo_res->to_string( ).
*   DATA(lo_md) = zcl_llm_00_markdown=>new( ).
    gs_fm-o_4 = lv_out.
*   gs_fm-i_2 = lv_out.
*    gt_ted[ name = 'O_4' ]-eh->update_from_ref( REF #( gs_fm-o_4 ) ).
    sync_html( v = gs_fm-o_4 p = '' gui = 'O_4' ).
    cl_gui_cfw=>flush( ).                                "~ Flushes the GUI to ensure all changes are rendered.
  ENDMETHOD.
  METHOD reset_menu.
    "~ Resets the menu to its default state.

    RETURN.

*    DATA(lo_toolbar)   = mo_gos_menu->get_toolbar( ).
*    lo_toolbar->delete_all_buttons( ).
*    DATA(lo_container) = mo_gos_menu->get_container( ).
*    lo_container->free( ).
*    FREE mo_gos_menu.
*    go_->read_defaults( ).
*    me->setup_menu( ).

  ENDMETHOD.
  METHOD string_from_json.
    "~ Converts a JSON string into an ABAP string.
*
*    zcl_eui_conv=>from_json(
*      EXPORTING
*        iv_json = iv_json
*        iv_mode = zcl_eui_conv=>mc_json_mode-standard
*      IMPORTING
*        ex_data = rv_
*        ev_ok   = DATA(lv_ok) ).
*    IF lv_ok <> abap_true.
*      zcl_cpu=>ok( ).
*    ENDIF.

  ENDMETHOD.

  METHOD sync_ted.
    LOOP AT gt_ted REFERENCE INTO DATA(gr_ted) WHERE editor IS BOUND AND eh IS BOUND.
      gr_ted->eh->sync_text( ).                          "~ Synchronizes text editor content.
    ENDLOOP.
    cl_gui_cfw=>flush( ).                                "~ Flushes the GUI to ensure all changes are rendered.
  ENDMETHOD.

  METHOD is_ted_modified.
    LOOP AT gt_ted REFERENCE INTO DATA(lr_ted) WHERE editor IS BOUND AND eh IS BOUND.
      IF lr_ted->eh->sync_text( ) = abap_true AND rv_ = abap_false. "~ Synchronizes text editor content.
        rv_ = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD setup_menu.
    "~ Sets up the menu based on the current context.
*    IF go_aqo IS NOT BOUND.
*      RETURN.
*    ENDIF.
*    IF mo_gos_menu IS BOUND.
*      RETURN.
*    ENDIF.
**--------------------------------------------------------------------
*    mt_menu = VALUE #(
*      ( function  = 'BTN_MENU'                                 butn_type = cntb_btype_menu  icon = icon_compare )
*     "( function  = 'BTN_MENU_00'    par_function = 'BTN_MENU' butn_type = cntb_btype_group icon = icon_okay    ) "disabled = abap_true
*      ( function  = 'BTN_MENU_00'    par_function = 'BTN_MENU' text = 'Retrieve Version: MM/DD TEXT'(t01) icon = icon_okay    ) "disabled = abap_true
*      )
*   .
*    LOOP AT gt_ted REFERENCE INTO DATA(lr_ted).
*      APPEND VALUE #( function  = lr_ted->name par_function = 'BTN_MENU' text = lr_ted->name butn_type = cntb_btype_menu ) TO mt_menu.
*      DATA(ls_) = go_aqo->mt_field_value[ name = lr_ted->name ].
*      DATA(lt_) = VALUE zcl_eui_menu=>tt_menu(
*        FOR ls_v IN ls_-value INDEX INTO lv_i (
*         "function  = |{ lr_ted->name }-{ lv_i }| par_function = lr_ted->name  text = |{ ls_v-changed DATE = ISO } { me->string_from_json( ls_v-h_value ) }|
*          function  = |{ lr_ted->name }-{ lv_i }| par_function = lr_ted->name  text = |{ ls_v-changed+4(2) }/{ ls_v-changed+6(2) } { me->string_from_json( ls_v-h_value ) }|
*        )
*      ).
*      LOOP AT lt_ REFERENCE INTO DATA(lr_).
*        IF strlen( lr_->text ) >= 38.
*          lr_->text+38(2) = '..'.
*        ENDIF.
*      ENDLOOP.
*      APPEND LINES OF lt_ TO mt_menu.
*    ENDLOOP.
*
*    mo_gos_menu = NEW #( io_handler = me ).
*    DATA(lo_menu) = mo_gos_menu->create_toolbar(
*      EXPORTING
*        it_menu        = mt_menu
*    ).
*
**      ( function  = 'BTN_MENU_01'    par_function = 'BTN_MENU'     text = 'Menu 01'      butn_type = cntb_btype_menu )
**      ( function  = 'BTN_MENU_02'    par_function = 'BTN_MENU'     text = 'Menu 02'      butn_type = cntb_btype_menu )
**      ( function  = 'BTN_MENU_01_01' par_function = 'BTN_MENU_01'  text = 'Menu 01-01' )
**      ( function  = 'BTN_MENU_01_02' par_function = 'BTN_MENU_01'  text = 'Menu 01-02' )
**      ( function  = 'BTN_MENU_02_01' par_function = 'BTN_MENU_02'  text = 'Menu 02-01' )
**      ( function  = 'BTN_MENU_02_02' par_function = 'BTN_MENU_02'  text = 'Menu 02-02' )
*
*
  ENDMETHOD.

  METHOD on_function_selected_lang.
    "~ Handles language selection events.
  ENDMETHOD.

  METHOD on_function_selected.
    "~ Handles function selection events.
  ENDMETHOD.

  METHOD add_to_transport.
    "~ Adds the current session to a transport request.

  ENDMETHOD.

  METHOD load.
*    SELECT *
*      FROM  zllm_00_cache
*      WHERE seed = 5050
*      INTO TABLE @mt_cache.
*    IF mt_cache IS INITIAL.
*      RETURN.
*    ENDIF.
*    DATA(lv_) = NEW zcl_eui_alv(
*      ir_table   = REF #( mt_cache )
*      is_layout  = VALUE #( zebra = 'X' no_toolbar = 'X' )
*      it_mod_catalog = VALUE lvc_t_fcat( ( fieldname = 'PROCESSED' tech = 'X' edit = 'X' ) )
*    )->popup( )->show( io_handler = me ).

  ENDMETHOD.

  METHOD save.
    me->sync_ted( ).

    DATA(lo_cache) = zcl_llm_00_cache=>new(
      iv_seed  = 5050
      io_codec = zcl_llm_00_codec_mock=>new( )
    ).

    DATA(lv_session) = zcl_llm_00_json=>to_json( gs_fm ).
    DATA(lv_res) = lo_cache->get( lv_session ).
    IF lv_res IS NOT INITIAL.
      RETURN.
    ENDIF.

    lo_cache->put(
      EXPORTING
        k = lv_session
        v = lv_session
    ).

    "~ Saves the current session.
*    DATA(lv_is_modified) = abap_false.
*    LOOP AT gt_ted REFERENCE INTO DATA(gr_ted).
*      IF gr_ted->eh->sync_text( ) = abap_true AND lv_is_modified = abap_false.
*        lv_is_modified = abap_true.
*      ENDIF.
*    ENDLOOP.
*    IF lv_is_modified IS INITIAL.
*      MESSAGE w013.
*      RETURN.
*    ENDIF.
*    cl_gui_cfw=>flush( ).      "DATA(lt_) = go_aqo->mt_fld_value.
  ENDMETHOD.

  METHOD confirm_exit.
    "~ Confirms exit when unsaved changes exist.
*    rv_ = zcl_eui_screen=>confirm( "~ Displays a confirmation dialog for unsaved changes.
*      iv_title    = 'Unsaved changes'(t05)
*      iv_question = 'There are unsaved changes, do you want to exit?'(t10)
*      iv_icon_1   = 'ICON_CANCEL'
*      iv_icon_2   = '' ).
  ENDMETHOD.

  METHOD split_screen.
    "~ Splits the screen into multiple sections for different content.

    IF gt_ted IS NOT INITIAL.
      "   IF go_spl IS NOT BOUND.
      RETURN.
    ENDIF.

    gs_fm-i_0 = 'Input for the step ->'.
    gs_fm-s_0 = 'System instruction for the step ->'.
    gs_fm-u_0 = 'User instruction for the step ->'.
    gs_fm-o_0 = 'Output for the step, will be used as an input for the next step. o = Step( s(i) + u(i) )'.

    gs_fm-i_1 = `Retrofuturistic Landscapes Urban Days Of Life Comedy`.
    gs_fm-s_1 = |You are the best storyteller. | && zif_llm=>n &&
                |                   | &&            zif_llm=>n &&
                |respond in JSON:   | &&            zif_llm=>n &&
                |                   | &&            zif_llm=>n &&
                |\{                  | &&           zif_llm=>n &&
                | "intro": <....>,  | &&            zif_llm=>n &&
                | "middle": <....>, | &&            zif_llm=>n &&
                | "outro": <....>   | &&            zif_llm=>n &&
                |\}                  |.


    gs_fm-u_1 = 'Generate a plan for the story in the genre: {T}'.

*--------------------------------------------------------------------*
    gs_fm-s_2 = `You are the best storyteller.                                        ` && zif_llm=>n &&
                `                                                                     ` && zif_llm=>n &&
                `respond in JSON:                                                     ` && zif_llm=>n &&
                `                                                                     ` && zif_llm=>n &&
                `{  "C": [                                                            ` && zif_llm=>n &&
                `      {"name": "<name/>", "role":"<role/>", "visual":"<visual/>" },  ` && zif_llm=>n &&
                `],                                                                   ` && zif_llm=>n &&
                `"P": [                                                               ` && zif_llm=>n &&
                `      {"part": "<PartName/>", "content", "<content/>" },             ` && zif_llm=>n &&
                `      {"part": "<PartName/>", "content", "<content/>" },             ` && zif_llm=>n &&
                `                                                                     ` && zif_llm=>n &&
                `]                                                                    ` && zif_llm=>n &&
                `}`.

    gs_fm-u_2 = `For each of the sections:                          ` && zif_llm=>n &&
                `                                                   ` && zif_llm=>n &&
                `Intro:                                             ` && zif_llm=>n &&
                `                                                   ` && zif_llm=>n &&
                `{T-INTRO}                                          ` && zif_llm=>n &&
                `                                                   ` && zif_llm=>n &&
                `Middle:                                            ` && zif_llm=>n &&
                `{T-MIDDLE}                                         ` && zif_llm=>n &&
                `                                                   ` && zif_llm=>n &&
                `Outro:                                             ` && zif_llm=>n &&
                `{T-OUTRO}                                          ` && zif_llm=>n &&
                `                                                   ` && zif_llm=>n &&
                `Generate a List of Characters and expant the Plot. ` && zif_llm=>n &&
                `Please generate the visual description for each character.`.
*--------------------------------------------------------------------*
    gs_fm-s_3 = `You are the best storyteller. `.
    gs_fm-u_3 = `Here is the context:` && zif_llm=>n &&
                `# Characters:` && zif_llm=>n &&
                `                              ` && zif_llm=>n &&
                `## Name: {T-C-NAME}              ` && zif_llm=>n &&
                `## Role: {T-C-ROLE}` && zif_llm=>n &&
                `## Visual: {T-C-VISUAL}` && zif_llm=>n &&
                `----` && zif_llm=>n &&
                `` && zif_llm=>n &&
                `# PLOT:` && zif_llm=>n &&
                `` && zif_llm=>n &&
                `## {T-P-PART} :` && zif_llm=>n &&
                `{T-P-CONTENT}` && zif_llm=>n &&
                `---` && zif_llm=>n &&
                `Please write a short funny story for the context Above, in a format of russian traditional absurdist anecdote`.
*--------------------------------------------------------------------*
    gs_fm-s_4 = 'You are the best literature critic.'.
    gs_fm-u_4 = `Here is the [STORY] Written by one of the young Authors: ` && zif_llm=>n &&
                `                                                         ` && zif_llm=>n &&
                `[STORY]                                                  ` && zif_llm=>n &&
                `                                                         ` && zif_llm=>n &&
                `{T}                                                      ` && zif_llm=>n &&
                `                                                         ` && zif_llm=>n &&
                `[/STORY]                                                 ` && zif_llm=>n &&
                `Please Summarize it and give feedback                    ` .

    gr_fm  = REF #( gs_fm ).

    go_gui = NEW #( 'ZGUI01' ).
    DATA(lo_spl) = zcl_llm_00_spl=>new( go_gui ).        "~ Instantiates a new split screen object.
*    gt_spl = lo_spl->split( '|').
*--------------------------------------------------------------------
*   go_htm = gt_spl[ 2 ]-gui.
*   go_mtx = gt_spl[ 1 ]-gui.
    go_mtx = go_gui. "gt_spl[ 1 ]-gui.
    TRY.
        "       go_stt = zcl_llm_00_str_to_ted=>new(
        go_stt = zcl_llm_00_str_to_ted=>new(                  "~ Instantiates a new string to text editor converter.
                   io_gui       = go_mtx
                   ir_str       = gr_fm
                   itr_         = CORRESPONDING #( zcl_llm_00_list=>new_from_string( 'I_1;S*;U*;O*;!*_0;!O_4' )->r( ) )
                   iv_threshold = 7
                   iv_max       = 24
                 ).
      CATCH zcx_s INTO DATA(lx_s). " z-exception static check (base).
        "zcl_cpu=>ok( ).                                    "~ Handles exceptions during instantiation.
    ENDTRY.

    gt_ted = go_stt->get_ted( ).                           "~ Retrieves text editor instances.

*    gt_ted[ 4 ]-

    sync_html( v = gs_fm-i_0 p = '' gui = 'I_0' ).
    sync_html( v = gs_fm-u_0 p = '' gui = 'U_0' ).
    sync_html( v = gs_fm-s_0 p = '' gui = 'S_0' ).
    sync_html( v = gs_fm-o_0 p = '' gui = 'O_0' ).

  ENDMETHOD.
  METHOD render_metadata.
    "~ Renders metadata content.
  ENDMETHOD.
  METHOD get_html_viewer.
    ro_ = VALUE #( mt_hv[ gui = iv_ ]-hv OPTIONAL ).
    IF ro_ IS BOUND.
      RETURN.
    ELSE.
      DATA(lo_gui) = VALUE #( gt_ted[ name = iv_ ]-gui OPTIONAL ).
      APPEND VALUE #(
        gui = iv_
        hv  = NEW cl_gui_html_viewer( lo_gui )
      ) TO mt_hv.
    ENDIF.
    ro_ = VALUE #( mt_hv[ gui = iv_ ]-hv OPTIONAL ).
  ENDMETHOD.
*  METHOD sync_html.
*    DATA(lr_ted) = REF #( gt_ted[ name = gui ] OPTIONAL ).
*    IF lr_ted IS NOT BOUND.
*      RETURN.
*    ENDIF.
*
*    IF p IS NOT INITIAL.
*
*      DATA(lo_pat) = zcl_llm_00_pat=>new(
*        iv_     = p
**       iv_prefix  = '{'
**       iv_postfix = '}'
**       iv_root = 'T'
*        iv_name = gui
*      ).
*      DATA(lr_dref) = zcl_llm_00_json=>js_to_dref( v ).
*      DATA(lv_md) = lo_pat->apply( lr_dref ).
*    ELSE.
*      lv_md = v.
*    ENDIF.
*    DATA(lv_html) = zcl_llm_00_markdown=>new( )->text( lv_md ).
*
**--------------------------------------------------------------------*
*    DATA(lo_html) = me->get_html_viewer( gui ).
*
*    DATA lt_html TYPE soli_tab.
*    CALL FUNCTION 'SO_STRING_TO_TAB'
*      EXPORTING
*        content_str = lv_html
*      TABLES
*        content_tab = lt_html.
*    DATA: lv_url TYPE text1024.
*    lo_html->load_data(
*      IMPORTING
*        assigned_url = lv_url
*      CHANGING
*        data_table   = lt_html
*    ).
*    lo_html->show_url( url = lv_url ).
*
**    cl_abap_browser=>close_browser( ).
**
**    cl_abap_browser=>show_html(
**      EXPORTING
**         html_string  = lv_html
**         container    = lr_ted->gui
**    ).
*  ENDMETHOD.
  METHOD sync_html.
    DATA(lr_ted) = REF #( gt_ted[ name = gui ] OPTIONAL ).
    IF lr_ted IS NOT BOUND.
      RETURN.
    ENDIF.

    IF p IS NOT INITIAL.
      DATA(lo_pat) = zcl_llm_00_pat=>new(
        iv_     = p
        iv_name = gui
      ).
      DATA(lr_dref) = zcl_llm_00_json=>js_to_dref( v ).
      DATA(lv_md) = lo_pat->apply( lr_dref ).
    ELSE.
      lv_md = v.
    ENDIF.

    " Convert markdown to HTML
    DATA(lv_html_body) = zcl_llm_00_markdown=>new( )->text( lv_md ).

    " Wrap HTML with proper UTF-8 encoding headers
    DATA(lv_html) = |<!DOCTYPE html>| && cl_abap_char_utilities=>newline &&
                    |<html lang="en">| && cl_abap_char_utilities=>newline &&
                    |<head>| && cl_abap_char_utilities=>newline &&
                    |<meta charset="UTF-8">| && cl_abap_char_utilities=>newline &&
                    |<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">| && cl_abap_char_utilities=>newline &&
                    |<meta name="viewport" content="width=device-width, initial-scale=1.0">| && cl_abap_char_utilities=>newline &&
                    |<title>{ gui }</title>| && cl_abap_char_utilities=>newline &&
                    |</head>| && cl_abap_char_utilities=>newline &&
                    |<body>| && cl_abap_char_utilities=>newline &&
                    lv_html_body &&
                    |</body>| && cl_abap_char_utilities=>newline &&
                    |</html>|.

    DATA(lo_html) = me->get_html_viewer( gui ).

    " Convert to SOLI table for HTML viewer
    DATA lt_html TYPE soli_tab.
    CALL FUNCTION 'SO_STRING_TO_TAB'
      EXPORTING
        content_str = lv_html
      TABLES
        content_tab = lt_html.

    DATA: lv_url TYPE text1024.

    " Load with explicit UTF-8 encoding
    lo_html->load_data(
      EXPORTING
        type         = 'text'
        subtype      = 'html'
        encoding     = 'UTF-8'
*       language     = 'E'
      IMPORTING
        assigned_url = lv_url
      CHANGING
        data_table   = lt_html
    ).

    lo_html->show_url( url = lv_url ).
  ENDMETHOD.
  METHOD show_log.
    "~ Displays logs if any exist.

    IF go_log->get_analyser( )->has_any( ).                "~ Checks if the log contains any entries.
      go_log->display( ).                                  "~ Displays the log entries.
    ENDIF.
  ENDMETHOD.

  METHOD zif_llm_00_trace~in.
    cl_demo_output=>display_text( |in for { iv_id }:| && zif_llm=>n && iv_ ).
  ENDMETHOD.

  METHOD zif_llm_00_trace~out.
    cl_demo_output=>display_text( |out for { iv_id }:| && zif_llm=>n && iv_ ).
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Module  PAI_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100_exit INPUT.
  IF go_->is_ted_modified( ).
    IF go_->confirm_exit( ) NE abap_true.                  "~ Confirms exit when unsaved changes exist.
      RETURN.
    ENDIF.
  ENDIF.
*--------------------------------------------------------------------
  SET SCREEN 0.
  LEAVE SCREEN.

ENDMODULE.

AT SELECTION-SCREEN.
  TRY.
      CASE sy-ucomm.
        WHEN 'ONLI'.
          CALL SCREEN '0100'.
        WHEN OTHERS.
      ENDCASE.
    CATCH zcx_s INTO DATA(gx_s).
      go_log->add( gx_s ).                                 "~ Adds exceptions to the log.
      go_log->display( ).                                  "~ Displays the log entries.
  ENDTRY.

AT SELECTION-SCREEN OUTPUT.
  RETURN.
  go_->split_screen( ).                                    "~ Splits the screen into multiple sections for different content.
