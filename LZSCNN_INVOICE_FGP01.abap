*&---------------------------------------------------------------------*
*&  Include           LZSCNN_INVOICE_FGP01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_alv_grid DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_grid DEFINITION INHERITING FROM cl_gui_alv_grid .

  PUBLIC SECTION .

    METHODS set_delay_time .

ENDCLASS .                    "lcl_alv_grid DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_alv_grid IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_grid IMPLEMENTATION .

  METHOD set_delay_time.
    CALL METHOD me->set_delay_change_selection( 1 ).

  ENDMETHOD. "set_delay_time

ENDCLASS .                    "lcl_alv_grid IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION .
  PUBLIC SECTION .
    DATA vendor TYPE lifnr .
    DATA bukrs TYPE bukrs .
    DATA waers  TYPE waers .
    DATA waers_bukrs TYPE waers .

    DATA lo_alv_grid         TYPE REF TO lcl_alv_grid . "cl_gui_alv_grid,
    DATA lt_data_output TYPE STANDARD TABLE OF zscnn_invoice .

    CLASS-DATA:   container TYPE scrfname VALUE 'CC_TABLE' .

    CLASS-METHODS: main  .

    METHODS: constructor ,
             init_controls ,
             refresh_initial_output ,
             update_foreign_balance CHANGING change TYPE  lvc_s_modi OPTIONAL ,
             update_balance  CHANGING change TYPE  lvc_s_modi OPTIONAL ,
             refresh_alv_grid ,
             get_exchange_rate IMPORTING f_curr TYPE waers
                                         l_curr TYPE waers
                               RETURNING value(ex_rate) TYPE kursf ,
             change_by_waers IMPORTING use_foreign_curr TYPE abap_bool OPTIONAL ,
             convert_amount IMPORTING check TYPE abap_bool OPTIONAL
                                      f_curr TYPE waers
                                      f_amount TYPE ze_amt_for_mod
                                      l_curr TYPE waers
                            RETURNING value(amount) TYPE ze_amt_for_mod  ,
             enter_action ,
             save_action ,
             clear_screen .

  PRIVATE SECTION .

    DATA lo_custom_container TYPE REF TO cl_gui_custom_container.
    DATA lt_fieldcatalog TYPE lvc_t_fcat  .
    DATA dcpfm           TYPE xudcpfm .
    DATA screen_value    TYPE xuvalue .

    METHODS show_alv_grid .
    METHODS get_user_data .
    METHODS get_user_screen_parameter .
    METHODS prepare_fieldcatalog .

    METHODS handle_data_changed FOR EVENT data_changed
                         OF cl_gui_alv_grid
                         IMPORTING er_data_changed
                                   e_onf4
                                   e_onf4_before
                                   e_onf4_after .

    METHODS handle_delayed_changed_sel_cb
            FOR EVENT delayed_changed_sel_callback OF cl_gui_alv_grid .

    METHODS handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
                 IMPORTING e_ucomm .

    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
                                 IMPORTING e_row_id e_column_id es_row_no .

    METHODS handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
                           IMPORTING e_object e_interactive .

    METHODS call_posting_clearing .

    METHODS fill_header IMPORTING i_count TYPE count_pi OPTIONAL
                       CHANGING c_ftpost TYPE isjp_t_ftpost .
    METHODS fill_vendor IMPORTING i_count TYPE count_pi OPTIONAL
                   CHANGING c_ftpost TYPE isjp_t_ftpost .
    METHODS fill_taxes IMPORTING i_count TYPE count_pi OPTIONAL
                       CHANGING c_ftpost TYPE isjp_t_ftpost .
    METHODS fill_clear IMPORTING i_data TYPE zscnn_invoice
                       CHANGING c_ftclear TYPE epic_t_ebr_ftclear .

    METHODS fill_change_qty IMPORTING i_count TYPE count_pi OPTIONAL
                            CHANGING c_data TYPE zscnn_invoice
                                     c_ftpost TYPE isjp_t_ftpost .

    METHODS fill_change_amount IMPORTING i_count TYPE count_pi OPTIONAL
                                         i_data TYPE zscnn_invoice
                               CHANGING c_ftpost TYPE isjp_t_ftpost .

    METHODS get_diff_foreign_amount CHANGING c_count TYPE count_pi OPTIONAL
                                             c_ftpost TYPE isjp_t_ftpost .

    METHODS fill_change_qty_with_diff IMPORTING i_count TYPE count_pi OPTIONAL
                                       CHANGING c_data TYPE zscnn_invoice
                                                c_ftpost TYPE isjp_t_ftpost .

    METHODS fill_change_amount_with_diff IMPORTING i_count TYPE count_pi OPTIONAL
                                                   i_data TYPE zscnn_invoice
                                         CHANGING c_ftpost TYPE isjp_t_ftpost .

    METHODS update_invoice IMPORTING i_blntab TYPE blntab
                           RETURNING value(r_updated) TYPE abap_bool .

ENDCLASS .                    "lcl_application DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION .
  METHOD prepare_fieldcatalog .
    FIELD-SYMBOLS <fs_fieldcatalog> TYPE lvc_s_fcat .

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = c_structure_output
      CHANGING
        ct_fieldcat            = lt_fieldcatalog
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
              TYPE sy-msgty
              NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
    ENDIF.


    LOOP AT lt_fieldcatalog ASSIGNING <fs_fieldcatalog> .

      CASE <fs_fieldcatalog>-fieldname .
        WHEN 'QTY_MOD' .

          <fs_fieldcatalog>-edit = 'X' .

        WHEN 'AMT_MOD' .

          <fs_fieldcatalog>-edit = 'X' .

        WHEN 'QTY_INV' OR 'AMT_INV' OR
             'QTY_INI' OR 'AMT_INI' OR
             'AUGBL_BUKRS' OR 'AUGBL_GJAHR' OR 'AUGBL_BUZEI' OR
             'AMT_FOR_MOD'  OR 'AMT_REM'.

          <fs_fieldcatalog>-tech = 'X' .

        WHEN 'BELNR' OR 'AUGBL' .
          <fs_fieldcatalog>-hotspot = 'X' .
        WHEN 'USE_CHECK' .
          <fs_fieldcatalog>-checkbox = 'X' .
          <fs_fieldcatalog>-edit = 'X' .
          <fs_fieldcatalog>-outputlen = '2' .
      ENDCASE .
    ENDLOOP .

  ENDMETHOD.                    "prepare_fieldcatalog

  METHOD get_exchange_rate .

    DATA: lv_ex_r        TYPE tcurr-ukurs,
          lv_ff        TYPE tcurr-ffact,
          lv_lf        TYPE tcurr-tfact,
          lv_vfd       TYPE datum .


    CALL FUNCTION 'READ_EXCHANGE_RATE'
      EXPORTING
        date             = bkpf-budat
        foreign_currency = f_curr
        local_currency   = l_curr
        type_of_rate     = 'M'
      IMPORTING
        exchange_rate    = ex_rate
        foreign_factor   = lv_ff
        local_factor     = lv_lf
        valid_from_date  = lv_vfd
      EXCEPTIONS
        no_rate_found    = 1
        no_factors_found = 2
        no_spread_found  = 3
        derived_2_times  = 4
        overflow         = 5
        zero_rate        = 6
        OTHERS           = 7.
    IF sy-subrc <> 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF .

  ENDMETHOD .                    "get_exchange_rate
  METHOD convert_amount .
    DATA: lv_ex_r        TYPE tcurr-ukurs,
          lv_ex_r_aux    TYPE tcurr-ukurs,
          lv_ff          TYPE tcurr-ffact,
          lv_lf          TYPE tcurr-tfact,
          lv_vfd         TYPE datum,
          ld_erate(12)   TYPE c.

    DATA: from_amount TYPE bseg-dmbtr ,
          to_amount TYPE bseg-wrbtr.

    CALL FUNCTION 'READ_EXCHANGE_RATE'
      EXPORTING
        date             = sy-datum
        foreign_currency = f_curr
        local_currency   = l_curr
        type_of_rate     = 'M'
      IMPORTING
        exchange_rate    = lv_ex_r
        foreign_factor   = lv_ff
        local_factor     = lv_lf
        valid_from_date  = lv_vfd
      EXCEPTIONS
        no_rate_found    = 1
        no_factors_found = 2
        no_spread_found  = 3
        derived_2_times  = 4
        overflow         = 5
        zero_rate        = 6
        OTHERS           = 7.

    IF sy-subrc <> 0 .
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    lv_ex_r_aux = lv_ex_r * -1 .
    IF lv_ex_r NE bkpf-kursf AND lv_ex_r_aux NE bkpf-kursf .

      from_amount = f_amount .

      IF lv_ex_r < 0 .

        CALL FUNCTION 'CONVERT_CURRENCY_BY_RATE'
          EXPORTING
            from_amount   = from_amount
            from_currency = l_curr
            from_factor   = lv_ff
            rate          = bkpf-kursf
            to_currency   = f_curr
            to_factor     = lv_lf
          IMPORTING
            to_amount     = to_amount
          EXCEPTIONS
            no_rate_found = 1
            OTHERS        = 2.
        IF sy-subrc <> 0.

          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        amount = to_amount .
      ELSE.

        ld_erate =  bkpf-kursf / ( lv_ff / lv_lf ).
        amount = f_amount * ld_erate.
      ENDIF.

    ELSE.

      CALL FUNCTION 'CONVERT_AMOUNT_TO_CURRENCY'
        EXPORTING
          foreign_currency = f_curr
          foreign_amount   = f_amount
          local_currency   = l_curr
        IMPORTING
          local_amount     = amount
        EXCEPTIONS
          error            = 1
          OTHERS           = 2.

      IF sy-subrc <> 0 .
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF .

    ENDIF.
  ENDMETHOD .                    "convert_amount
  METHOD refresh_initial_output .
    FIELD-SYMBOLS <fs_data_output> TYPE zscnn_invoice .

    LOOP AT lt_data_output ASSIGNING <fs_data_output> .

      IF <fs_data_output>-qty_inv IS INITIAL.
        <fs_data_output>-qty_mod = <fs_data_output>-qty .
        <fs_data_output>-qty_ini = <fs_data_output>-qty .
      ELSE.
        <fs_data_output>-qty_mod = <fs_data_output>-qty .
      ENDIF.

      IF <fs_data_output>-amt_inv IS INITIAL.
        <fs_data_output>-amt_mod = <fs_data_output>-amt .
        <fs_data_output>-amt_ini = <fs_data_output>-amt .
      ELSE.
*        <fs_data_output>-amt_mod = <fs_data_output>-qty * <fs_data_output>-price .
        <fs_data_output>-amt_mod = <fs_data_output>-amt_rem .
      ENDIF.
      lo_application->bukrs = <fs_data_output>-bukrs.
    ENDLOOP .
  ENDMETHOD.                    "refresh_initial_output
  METHOD change_by_waers .
    FIELD-SYMBOLS <fs_fieldcatalog> TYPE lvc_s_fcat .

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = c_structure_output
      CHANGING
        ct_fieldcat            = lt_fieldcatalog
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
              TYPE sy-msgty
              NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
    ENDIF.


    LOOP AT lt_fieldcatalog ASSIGNING <fs_fieldcatalog> .

      CASE <fs_fieldcatalog>-fieldname .

        WHEN 'AMT_MOD' .

          IF use_foreign_curr EQ abap_true  .
            <fs_fieldcatalog>-edit = ' ' .
          ELSE.
            <fs_fieldcatalog>-edit = 'X' .
          ENDIF.
        WHEN 'AMT_FOR_MOD' .

          IF use_foreign_curr EQ abap_true  .
            <fs_fieldcatalog>-edit = 'X' .
            <fs_fieldcatalog>-tech = ' ' .
          ELSE.
            <fs_fieldcatalog>-edit = ' ' .
            <fs_fieldcatalog>-tech = 'X' .
          ENDIF.

      ENDCASE .
    ENDLOOP .

    me->lo_alv_grid->set_frontend_fieldcatalog(
      EXPORTING it_fieldcatalog = lt_fieldcatalog ) .

    lo_application->refresh_alv_grid( ) .
  ENDMETHOD.                    "change_by_waers

  METHOD refresh_alv_grid .
    DATA: lw_stable TYPE lvc_s_stbl.

    lw_stable-row = 'X'.
    lw_stable-col = 'X'.

    IF lo_alv_grid IS BOUND .

      CALL METHOD lo_alv_grid->refresh_table_display
        EXPORTING
          is_stable = lw_stable
        EXCEPTIONS
          finished  = 1
          OTHERS    = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid
                TYPE sy-msgty
                NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
      ENDIF.

    ENDIF .
  ENDMETHOD.                    "refresh_alv_grid
  METHOD show_alv_grid .
    DATA lw_exclude_functions TYPE ui_func.
    DATA lt_exclude_functions TYPE ui_functions .
    DATA lw_layout TYPE lvc_s_layo .
    DATA lw_variant TYPE disvariant .

    lo_application->prepare_fieldcatalog( ).

    lw_layout-zebra = 'X'.
    lw_layout-sel_mode = 'A' .

    lw_variant-report = sy-repid.


    lw_exclude_functions = cl_gui_alv_grid=>mc_fg_edit .
    APPEND lw_exclude_functions TO lt_exclude_functions .


    CALL METHOD lo_alv_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = lw_layout
        is_variant                    = lw_variant
        i_save                        = 'A'
        it_toolbar_excluding          = lt_exclude_functions
      CHANGING
        it_outtab                     = lt_data_output
        it_fieldcatalog               = lt_fieldcatalog
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
              TYPE sy-msgty
              NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
    ENDIF.

*   RAISE EVENT TOOLBAR TO SHOW THE MODIFIED TOOLBAR
    CALL METHOD lo_alv_grid->set_toolbar_interactive.
  ENDMETHOD.                    "show_alv_grid

  METHOD constructor .

    me->get_user_data( ).

    me->get_user_screen_parameter( ) .
  ENDMETHOD .                    "constructor
  METHOD main .

    CREATE OBJECT lo_application .

    CALL SCREEN 2000 .
  ENDMETHOD .                    "main
  METHOD get_user_data.
    DATA ls_usr01 TYPE usr01 .

    CALL FUNCTION 'CETA_USR01_READ'
      EXPORTING
        bname     = sy-uname
      IMPORTING
        usr01_exp = ls_usr01
      EXCEPTIONS
        no_entry  = 1
        OTHERS    = 2.

    IF sy-subrc = 0.

      me->dcpfm = ls_usr01-dcpfm .
    ELSE.
      MESSAGE ID sy-msgid
          TYPE sy-msgty
          NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
    ENDIF .

  ENDMETHOD .                    "get_user_data
  METHOD init_controls .

    IF lo_custom_container IS INITIAL .

      CREATE OBJECT lo_custom_container
        EXPORTING
          container_name              = container
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid
                TYPE sy-msgty
                NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
      ENDIF.

      CREATE OBJECT lo_alv_grid
        EXPORTING
          i_parent          = lo_custom_container
*         i_parent          = cl_gui_custom_container=>screen0
          i_appl_events     = abap_true
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid
                TYPE sy-msgty
                NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
      ENDIF.

      CALL METHOD lo_alv_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified
        EXCEPTIONS
          error      = 1
          OTHERS     = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid
                TYPE sy-msgty
                NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
      ENDIF.

      CALL METHOD lo_alv_grid->register_delayed_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_delayed_change_select
        EXCEPTIONS
          error      = 1
          OTHERS     = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid
                TYPE sy-msgty
                NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
      ENDIF.

      CALL METHOD lo_alv_grid->set_delay_time( ).
      SET HANDLER lo_application->handle_data_changed FOR lo_alv_grid.
      SET HANDLER lo_application->handle_delayed_changed_sel_cb
                                 FOR lo_alv_grid.
      SET HANDLER lo_application->handle_hotspot_click FOR lo_alv_grid.
      SET HANDLER lo_application->handle_toolbar FOR lo_alv_grid .
      SET HANDLER lo_application->handle_user_command FOR lo_alv_grid .

      lo_application->show_alv_grid( ) .
    ENDIF .

  ENDMETHOD .                    "init_controls
  METHOD enter_action .
    DATA lt_data TYPE STANDARD TABLE OF zzscnn_gr .

    FIELD-SYMBOLS <fs_data_output> TYPE zscnn_invoice .

    IF lo_application->lt_data_output[] IS INITIAL OR
       lo_application->vendor NE lfa1-lifnr .

      SELECT *
        FROM zzscnn_gr
        INTO CORRESPONDING FIELDS OF TABLE lt_data_output
      WHERE lifnr EQ lfa1-lifnr
      AND belnr NE ' '
      AND buzei NE ' '
      AND augbl EQ ' ' .

      SORT lt_data_output BY bill belnr buzei .

      LOOP AT lt_data_output ASSIGNING <fs_data_output> .

        IF <fs_data_output>-qty_inv IS INITIAL.
          <fs_data_output>-qty_mod = <fs_data_output>-qty .
          <fs_data_output>-qty_ini = <fs_data_output>-qty .
        ELSE.
          <fs_data_output>-qty_mod = <fs_data_output>-qty - <fs_data_output>-qty_inv  .
          <fs_data_output>-qty =  <fs_data_output>-qty - <fs_data_output>-qty_inv  .
        ENDIF.

        IF <fs_data_output>-amt_inv IS INITIAL.
          <fs_data_output>-amt_mod = <fs_data_output>-amt .
          <fs_data_output>-amt_ini = <fs_data_output>-amt .
        ELSE.
*          <fs_data_output>-amt_mod = <fs_data_output>-qty * <fs_data_output>-price .
*          <fs_data_output>-amt = <fs_data_output>-qty * <fs_data_output>-price .

          <fs_data_output>-amt_mod = <fs_data_output>-amt_rem .
          <fs_data_output>-amt = <fs_data_output>-amt_rem .

        ENDIF.
        lo_application->bukrs = <fs_data_output>-bukrs.
      ENDLOOP .

      SELECT SINGLE waers
        FROM t001
        INTO lo_application->waers_bukrs
        WHERE bukrs = lo_application->bukrs .

      bkpf-waers = lo_application->waers_bukrs .
      lo_application->refresh_alv_grid( ) .
      lo_application->vendor = lfa1-lifnr .
      CLEAR rf05a-azsal .
    ENDIF.
  ENDMETHOD .                    "enter_action

  METHOD save_action .
    DATA lw_mwskz TYPE mwskz .

    lo_application->lo_alv_grid->check_changed_data( ) .

    IF bkpf-bldat IS INITIAL OR bkpf-budat IS INITIAL.
*     Enter date
      MESSAGE e159(3f).
    ENDIF.

    IF lfa1-lifnr IS INITIAL .
      MESSAGE e011(zdsd) WITH text-001 .
      EXIT .
    ENDIF .

    IF bseg-wrbtr IS INITIAL .
      MESSAGE e011(zdsd) WITH text-002 .
      EXIT .
    ENDIF .

*    IF bseg-wmwst IS INITIAL .
    IF bseg-mwskz IS INITIAL .
      MESSAGE e011(zdsd) WITH text-004 .
      EXIT.
    ENDIF.

    IF NOT bseg-mwskz IS INITIAL .
      SELECT SINGLE mwskz
        INTO lw_mwskz
        FROM t030k
        WHERE ktopl = c_chart_of_account AND
              ktosl = c_transaction_key AND
              mwskz = bseg-mwskz .
      IF sy-subrc <> 0 .
        MESSAGE e011(zdsd) WITH text-004 .
        EXIT .
      ENDIF.
    ENDIF .

    IF NOT rf05a-azsal IS INITIAL .
      MESSAGE e011(zdsd) WITH text-005 .
      EXIT .
    ENDIF .

    me->call_posting_clearing( ) .
  ENDMETHOD .                    "save_action

  METHOD clear_screen .

    CLEAR bkpf .
    CLEAR bseg .
    CLEAR rf05a .
    CLEAR lfa1 .
    CLEAR lo_application->lt_data_output[] .
    CLEAR lo_application->waers_bukrs .

    lo_application->change_by_waers( ).
    lo_application->refresh_alv_grid( ) .
  ENDMETHOD.                    "clear_screen
  METHOD fill_header .
    DATA lw_ftpost TYPE ftpost .

    lw_ftpost-count = 1.
    lw_ftpost-stype = c_vendor_account_type .
    lw_ftpost-fnam = 'BKPF-BLDAT'.
    lw_ftpost-fval = bkpf-bldat .
    CONCATENATE lw_ftpost-fval+6(2) '.' lw_ftpost-fval+4(2) '.'  lw_ftpost-fval(4) INTO lw_ftpost-fval.
    APPEND lw_ftpost TO c_ftpost.

    lw_ftpost-fnam = 'BKPF-BUDAT'.
    lw_ftpost-fval = bkpf-budat.
    CONCATENATE lw_ftpost-fval+6(2) '.'  lw_ftpost-fval+4(2) '.' lw_ftpost-fval(4) INTO lw_ftpost-fval.
    APPEND lw_ftpost TO c_ftpost.

    lw_ftpost-fnam = 'BKPF-BLART'.
    lw_ftpost-fval = c_doc_type .
    APPEND lw_ftpost TO c_ftpost.

    lw_ftpost-fnam = 'BKPF-BUKRS'.
    lw_ftpost-fval = c_company_code .
    APPEND lw_ftpost TO c_ftpost.

    lw_ftpost-fnam = 'BKPF-XBLNR'.
    lw_ftpost-fval = bkpf-xblnr .
    APPEND lw_ftpost TO c_ftpost .


    IF NOT bkpf-waers IS INITIAL .
      lw_ftpost-fnam = 'BKPF-WAERS'.
      lw_ftpost-fval = bkpf-waers .
      APPEND lw_ftpost TO c_ftpost.
    ELSE.
      lw_ftpost-fnam = 'BKPF-WAERS'.
      lw_ftpost-fval = c_curr_cny.
      APPEND lw_ftpost TO c_ftpost .
    ENDIF.

    IF bkpf-waers NE me->waers_bukrs .
      lw_ftpost-fnam = 'BKPF-KURSF'.
      lw_ftpost-fval = bkpf-kursf .

      IF me->dcpfm <> 'X'.
        REPLACE ALL OCCURRENCES OF '.' IN lw_ftpost-fval WITH ','.
      ENDIF.

      CONDENSE lw_ftpost-fval .

      APPEND lw_ftpost TO c_ftpost.
    ENDIF.
  ENDMETHOD.                    "fill_header

  METHOD fill_taxes .
    DATA lw_ftpost TYPE ftpost .

    DATA lv_wrbtr TYPE bseg-wrbtr.
    DATA lv_tax_account TYPE saknr .

    IF NOT bseg-mwskz IS INITIAL .
      SELECT SINGLE konts
        FROM t030k
        INTO lv_tax_account
        WHERE ktopl = c_chart_of_account
          AND ktosl = c_transaction_key
          AND mwskz = bseg-mwskz .

      IF sy-subrc = 0 .
        lw_ftpost-count = i_count .
        lw_ftpost-stype = 'P'.
        lw_ftpost-fnam = 'RF05A-NEWKO'.

        lw_ftpost-fval = lv_tax_account .
        APPEND lw_ftpost TO c_ftpost.
      ENDIF .
      lw_ftpost-fnam = 'RF05A-NEWBS'.
      lw_ftpost-fval = c_debit_entry .
      APPEND lw_ftpost TO c_ftpost.

      lw_ftpost-fnam = 'RF05A-NEWUM'.
      lw_ftpost-fval = ' ' .
      APPEND lw_ftpost TO c_ftpost.

      lw_ftpost-fnam = 'BSEG-WRBTR'.
      MOVE bseg-wmwst TO lv_wrbtr .
      lw_ftpost-fval = lv_wrbtr .
      CONDENSE lw_ftpost-fval .

      IF me->dcpfm <> 'X'.
        REPLACE ALL OCCURRENCES OF '.' IN lw_ftpost-fval WITH ','.
      ENDIF.

      APPEND lw_ftpost TO c_ftpost.

      lw_ftpost-fnam = 'BSEG-MWSKZ'.
      lw_ftpost-fval = bseg-mwskz .
      APPEND lw_ftpost TO c_ftpost.
    ENDIF.
  ENDMETHOD .                  "fill_taxes
  METHOD fill_vendor .
    DATA lw_ftpost TYPE ftpost .

    DATA lv_wrbtr  TYPE bseg-wrbtr .

    lw_ftpost-count = 1.
    lw_ftpost-stype = 'P'.

    lw_ftpost-fnam = 'RF05A-NEWKO'.
    lw_ftpost-fval = lo_application->vendor.
    APPEND lw_ftpost TO c_ftpost.

    lw_ftpost-fnam = 'RF05A-NEWBS'.
    lw_ftpost-fval = c_invoice .
    APPEND lw_ftpost TO c_ftpost.

    lw_ftpost-fnam = 'RF05A-NEWUM'.
    lw_ftpost-fval = ' ' .
    APPEND lw_ftpost TO c_ftpost.

    lw_ftpost-fnam = 'BSEG-WRBTR'.
    MOVE bseg-wrbtr TO lv_wrbtr .
    lw_ftpost-fval = lv_wrbtr .
    CONDENSE lw_ftpost-fval .

    IF me->dcpfm <> 'X'.
      REPLACE ALL OCCURRENCES OF '.' IN lw_ftpost-fval WITH ','.
    ENDIF.

    APPEND lw_ftpost TO c_ftpost.

  ENDMETHOD .                    "fill_vendor

  METHOD fill_clear .
    DATA lw_ftclear TYPE ftclear .

    lw_ftclear-agkoa = c_vendor_account_type .
    lw_ftclear-agbuk = c_company_code .
*    lw_ftclear-xnops = 'X'.
    lw_ftclear-agums = c_special_gl .
    CONCATENATE i_data-belnr i_data-gjahr i_data-buzei INTO lw_ftclear-selvon.
    lw_ftclear-selfd = 'BELNR'.
    APPEND lw_ftclear TO c_ftclear .
  ENDMETHOD .                    "fill_clear

  METHOD fill_change_qty .
    DATA lw_ftpost     TYPE ftpost .
    DATA lv_price_qty  TYPE ze_amt_for_mod  .
    DATA lv_wrbtr     TYPE bseg-wrbtr .

    DATA lv_resi_part TYPE char1 .

    lw_ftpost-count = i_count .
    lw_ftpost-stype = 'P'.
    lw_ftpost-fnam = 'RF05A-NEWKO'.
    lw_ftpost-fval = lo_application->vendor.
    APPEND lw_ftpost TO c_ftpost.

    lw_ftpost-fnam = 'RF05A-NEWBS'.
    lw_ftpost-fval = c_special_gl_credit .
    APPEND lw_ftpost TO c_ftpost.

    lw_ftpost-fnam = 'RF05A-NEWUM'.
    lw_ftpost-fval = c_special_gl.
    APPEND lw_ftpost TO c_ftpost .

    IF bkpf-waers NE me->waers_bukrs .

      lv_price_qty = c_data-qty_mod * c_data-price .

      lv_wrbtr =  c_data-amt - lv_price_qty.

**********************************************************************
      lw_ftpost-fnam = 'BSEG-DMBTR'.
      lw_ftpost-fval = lv_wrbtr .

      CONDENSE lw_ftpost-fval .

      IF me->dcpfm <> 'X'.
        REPLACE ALL OCCURRENCES OF '.' IN lw_ftpost-fval WITH ','.
      ENDIF.
      APPEND lw_ftpost TO c_ftpost .
**********************************************************************

*      lv_price_qty = lv_wrbtr .
*
*      lv_wrbtr = me->convert_amount( f_curr = me->waers_bukrs
*                                     f_amount = lv_price_qty
*                                     l_curr = bkpf-waers ) .
*
*      lw_ftpost-fval = lv_wrbtr .
*      CONDENSE lw_ftpost-fval .
*
*      IF me->dcpfm <> 'X'.
*        REPLACE ALL OCCURRENCES OF '.' IN lw_ftpost-fval WITH ','.
*      ENDIF.
*      lw_ftpost-fnam = 'BSEG-WRBTR'.
*      APPEND lw_ftpost TO c_ftpost.
*AMount Remain
      c_data-amt_rem = lv_wrbtr .
    ELSE.

      lv_wrbtr = c_data-amt - c_data-amt_mod .

      lw_ftpost-fval = lv_wrbtr .
      CONDENSE lw_ftpost-fval .

      IF me->dcpfm <> 'X'.
        REPLACE ALL OCCURRENCES OF '.' IN lw_ftpost-fval WITH ','.
      ENDIF.
      lw_ftpost-fnam = 'BSEG-WRBTR'.
      APPEND lw_ftpost TO c_ftpost.

*AMount Remain
      c_data-amt_rem = lv_wrbtr .

    ENDIF.

    lw_ftpost-fnam = 'BSEG-REBZG' .
    lw_ftpost-fval = c_data-belnr .
    APPEND lw_ftpost TO c_ftpost.

    lw_ftpost-fnam = 'BSEG-REBZJ' .
    lw_ftpost-fval = c_data-gjahr .
    APPEND lw_ftpost TO c_ftpost.

    lw_ftpost-fnam = 'BSEG-REBZZ' .
    lw_ftpost-fval = c_data-buzei .
    APPEND lw_ftpost TO c_ftpost .

    lv_resi_part = 'V' .
    EXPORT lv_resi_part FROM lv_resi_part TO MEMORY ID 'RESI_PART'.

    lw_ftpost-fnam = 'BSEG-ZFBDT' .
    lw_ftpost-fval = bkpf-bldat .
    CONCATENATE lw_ftpost-fval+6(2) '.' lw_ftpost-fval+4(2) '.'  lw_ftpost-fval(4) INTO lw_ftpost-fval.
    APPEND lw_ftpost TO c_ftpost .

  ENDMETHOD .                    "fill_change_qty

  METHOD fill_change_amount .
    DATA lw_ftpost TYPE ftpost .
    DATA lv_wrbtr  TYPE bseg-wrbtr .
    DATA lv_account_diff TYPE ze_diff_saknr .
    DATA l_aux TYPE char25.
    DATA lv_item_code TYPE ze_item_code .

    lw_ftpost-count = i_count .
    lw_ftpost-stype = 'P'.
    lw_ftpost-fnam = 'RF05A-NEWKO'.

    lv_item_code = i_data-item_code.
    TRANSLATE lv_item_code TO UPPER CASE.

    SELECT SINGLE saknr_diff
      INTO lv_account_diff
      FROM zzscnngr_saknr
      WHERE item_code = lv_item_code .

    IF sy-subrc = 0 .
      lw_ftpost-fval = lv_account_diff .
      APPEND lw_ftpost TO c_ftpost.
    ELSE.
      MESSAGE e011(zdsd) WITH lv_item_code text-010 .
      EXIT.
    ENDIF.

    IF bkpf-waers NE me->waers_bukrs .

      lv_wrbtr = (  me->convert_amount( f_curr = bkpf-waers
                                        f_amount = i_data-amt_for_mod
                                        l_curr = me->waers_bukrs   )  -  i_data-amt )  .


      IF lv_wrbtr > 0 .
        lw_ftpost-fval = c_debit_entry . "'40' .
      ELSE.
        lw_ftpost-fval = c_credit_entry . "'50' .
        lv_wrbtr = lv_wrbtr * -1 .
      ENDIF .
      lw_ftpost-fnam = 'RF05A-NEWBS'.
      APPEND lw_ftpost TO c_ftpost.

**********************************************************************
      lw_ftpost-fnam = 'BSEG-DMBTR'.
      lw_ftpost-fval = lv_wrbtr .
      CONDENSE lw_ftpost-fval .

      IF me->dcpfm <> 'X'.
        REPLACE ALL OCCURRENCES OF '.' IN lw_ftpost-fval WITH ','.
      ENDIF.
      APPEND lw_ftpost TO c_ftpost.
**********************************************************************

    ELSE.
      lv_wrbtr =  i_data-amt_mod - i_data-amt .


      IF lv_wrbtr > 0 .
        lw_ftpost-fval = c_debit_entry . "'40' .
      ELSE.
        lw_ftpost-fval = c_credit_entry . "'50' .
        lv_wrbtr = lv_wrbtr * -1 .
      ENDIF .
      lw_ftpost-fnam = 'RF05A-NEWBS'.
      APPEND lw_ftpost TO c_ftpost.



      lw_ftpost-fnam = 'BSEG-WRBTR'.
      lw_ftpost-fval = lv_wrbtr .
      CONDENSE lw_ftpost-fval .

      IF me->dcpfm <> 'X'.
        REPLACE ALL OCCURRENCES OF '.' IN lw_ftpost-fval WITH ','.
      ENDIF.
      APPEND lw_ftpost TO c_ftpost.



    ENDIF .



    lw_ftpost-fnam = 'RF05A-NEWUM'.
    lw_ftpost-fval = ' ' .
    APPEND lw_ftpost TO c_ftpost .


    lw_ftpost-fnam = 'BSEG-MWSKZ' .
    lw_ftpost-fval = bseg-mwskz .
    APPEND lw_ftpost TO c_ftpost .
  ENDMETHOD.                    "fill_change_amount

  METHOD fill_change_amount_with_diff .

    DATA lw_ftpost TYPE ftpost .
    DATA: lv_wrbtr  TYPE bseg-wrbtr,
          lv_wrbtr_aux  TYPE bseg-wrbtr .
    DATA lv_account_diff TYPE ze_diff_saknr .
    DATA l_aux TYPE char25.

    DATA lv_item_code TYPE ze_item_code .

    DATA lv_price_qty  TYPE ze_amt_for_mod .

    lw_ftpost-count = i_count .
    lw_ftpost-stype = 'P'.
    lw_ftpost-fnam = 'RF05A-NEWKO'.

    lv_item_code = i_data-item_code.

    TRANSLATE lv_item_code TO UPPER CASE.

    SELECT SINGLE saknr_diff
      INTO lv_account_diff
      FROM zzscnngr_saknr
      WHERE item_code = lv_item_code .

    IF sy-subrc = 0 .
      lw_ftpost-fval = lv_account_diff .
      APPEND lw_ftpost TO c_ftpost.
    ELSE.
      MESSAGE e011(zdsd) WITH lv_item_code text-010 .
      EXIT.
    ENDIF.


    IF bkpf-waers NE me->waers_bukrs .

      lv_price_qty = i_data-qty_mod * i_data-price .

      lv_wrbtr =  me->convert_amount( f_curr = bkpf-waers
                                      f_amount = i_data-amt_for_mod
                                      l_curr = me->waers_bukrs ) .

      lv_price_qty = lv_wrbtr - lv_price_qty .


      lv_wrbtr =  me->convert_amount( f_curr = me->waers_bukrs
                                      f_amount = lv_price_qty
                                      l_curr = bkpf-waers  ) .

**********************************************************************
      lw_ftpost-fnam = 'BSEG-DMBTR'.
      IF lv_price_qty < 0 .
        lv_price_qty = lv_price_qty * -1 .
      ENDIF.
      lw_ftpost-fval = lv_price_qty .
      CONDENSE lw_ftpost-fval .

      IF me->dcpfm <> 'X'.
        REPLACE ALL OCCURRENCES OF '.' IN lw_ftpost-fval WITH ','.
      ENDIF.
      APPEND lw_ftpost TO c_ftpost.
**********************************************************************

    ELSE.
      lv_wrbtr = i_data-qty_mod * i_data-price .

      lv_wrbtr = i_data-amt_mod - lv_wrbtr .

      lv_wrbtr_aux = lv_wrbtr.
      IF lv_wrbtr_aux < 0.
        lv_wrbtr_aux = lv_wrbtr_aux * -1.
      ENDIF.
      lw_ftpost-fnam = 'BSEG-WRBTR'.
      lw_ftpost-fval = lv_wrbtr_aux .
      CONDENSE lw_ftpost-fval .

      IF me->dcpfm <> 'X'.
        REPLACE ALL OCCURRENCES OF '.' IN lw_ftpost-fval WITH ','.
      ENDIF.

      APPEND lw_ftpost TO c_ftpost.
    ENDIF .

    IF lv_wrbtr > 0 .
      lw_ftpost-fval = c_debit_entry . "'40' .
    ELSE.
      lw_ftpost-fval = c_credit_entry . "'50' .
      lv_wrbtr = lv_wrbtr * -1 .
    ENDIF .
    lw_ftpost-fnam = 'RF05A-NEWBS'.
    APPEND lw_ftpost TO c_ftpost.

    lw_ftpost-fnam = 'RF05A-NEWUM'.
    lw_ftpost-fval = ' ' .
    APPEND lw_ftpost TO c_ftpost .

    lw_ftpost-fnam = 'BSEG-MWSKZ' .
    lw_ftpost-fval = bseg-mwskz .
    APPEND lw_ftpost TO c_ftpost .

  ENDMETHOD.                    "fill_change_amount_with_diff

  METHOD fill_change_qty_with_diff .
    DATA lw_ftpost     TYPE ftpost .
    DATA lv_price_qty  TYPE ze_amt_for_mod  .
    DATA lv_wrbtr      TYPE bseg-wrbtr .

    DATA lv_resi_part TYPE char1 .


    lw_ftpost-count = i_count .
    lw_ftpost-stype = 'P'.
    lw_ftpost-fnam = 'RF05A-NEWKO'.
    lw_ftpost-fval = lo_application->vendor.
    APPEND lw_ftpost TO c_ftpost.

    lw_ftpost-fnam = 'RF05A-NEWBS'.
    lw_ftpost-fval = c_special_gl_credit ." '39' .
    APPEND lw_ftpost TO c_ftpost.

    lw_ftpost-fnam = 'RF05A-NEWUM'.
    lw_ftpost-fval = c_special_gl . "'Q' .
    APPEND lw_ftpost TO c_ftpost .

    IF bkpf-waers NE me->waers_bukrs .

      lv_price_qty = c_data-qty_mod * c_data-price .

      lv_wrbtr =  c_data-amt - lv_price_qty.

      lv_price_qty = lv_wrbtr .

**********************************************************************
      lw_ftpost-fnam = 'BSEG-DMBTR'.
      lw_ftpost-fval = lv_price_qty .
      CONDENSE lw_ftpost-fval .

      IF me->dcpfm <> 'X'.
        REPLACE ALL OCCURRENCES OF '.' IN lw_ftpost-fval WITH ','.
      ENDIF.
      APPEND lw_ftpost TO c_ftpost .
**********************************************************************

*      lv_wrbtr =  me->convert_amount( f_curr = me->waers_bukrs
*                                      f_amount = lv_price_qty
*                                      l_curr = bkpf-waers )  .
*
*      lw_ftpost-fval = lv_wrbtr .
*
*      CONDENSE lw_ftpost-fval .
*
*      IF me->dcpfm <> 'X'.
*        REPLACE ALL OCCURRENCES OF '.' IN lw_ftpost-fval WITH ','.
*      ENDIF.
*
*      lw_ftpost-fnam = 'BSEG-WRBTR'.
*      APPEND lw_ftpost TO c_ftpost.

*      Amount remain
      c_data-amt_rem = lv_price_qty .
    ELSE.

      lv_wrbtr = c_data-qty_mod * c_data-price .

      lv_wrbtr =  c_data-amt - lv_wrbtr .

      lw_ftpost-fval = lv_wrbtr .

      CONDENSE lw_ftpost-fval .

      IF me->dcpfm <> 'X'.
        REPLACE ALL OCCURRENCES OF '.' IN lw_ftpost-fval WITH ','.
      ENDIF.

      lw_ftpost-fnam = 'BSEG-WRBTR'.
      APPEND lw_ftpost TO c_ftpost.
*      Amount remain
      c_data-amt_rem = lv_wrbtr.
    ENDIF .

    lw_ftpost-fnam = 'BSEG-REBZG' .
    lw_ftpost-fval = c_data-belnr .
    APPEND lw_ftpost TO c_ftpost.

    lw_ftpost-fnam = 'BSEG-REBZJ' .
    lw_ftpost-fval = c_data-gjahr .
    APPEND lw_ftpost TO c_ftpost.

    lw_ftpost-fnam = 'BSEG-REBZZ' .
    lw_ftpost-fval = c_data-buzei .
    APPEND lw_ftpost TO c_ftpost .

    lv_resi_part = 'V' .
    EXPORT lv_resi_part FROM lv_resi_part TO MEMORY ID 'RESI_PART'.


    lw_ftpost-fnam = 'BSEG-ZFBDT' .
    lw_ftpost-fval = bkpf-bldat .
    CONCATENATE lw_ftpost-fval+6(2) '.' lw_ftpost-fval+4(2) '.'  lw_ftpost-fval(4) INTO lw_ftpost-fval.
    APPEND lw_ftpost TO c_ftpost .
  ENDMETHOD .                    "fill_change_qty_with_diff
  METHOD call_posting_clearing .
    DATA lt_ftclear TYPE STANDARD TABLE OF ftclear .
    DATA lw_ftclear TYPE ftclear .
    DATA lt_blntab TYPE STANDARD TABLE OF blntab .
    DATA lw_blntab TYPE blntab .
    DATA lt_ftpost TYPE STANDARD TABLE OF ftpost .
    DATA lw_ftpost TYPE ftpost .
    DATA lt_fttax TYPE STANDARD TABLE OF fttax  .
    DATA lw_fttax TYPE fttax .

    DATA lt_rowindex TYPE lvc_t_row.

    DATA lv_wrbtr TYPE bseg-wrbtr.
    DATA lv_count TYPE count_pi .
    DATA lv_augbl TYPE augbl .
    DATA lv_lines TYPE i .
    DATA lv_dif_flag TYPE abap_bool .

    DATA e_subrc TYPE sy-subrc .
    DATA e_msgid TYPE sy-msgid .
    DATA e_msgno TYPE sy-msgno .
    DATA e_msgty TYPE sy-msgty .
    DATA e_msgv1 TYPE sy-msgv1 .
    DATA e_msgv2 TYPE sy-msgv2 .
    DATA e_msgv3 TYPE sy-msgv3 .
    DATA e_msgv4 TYPE sy-msgv4 .

    FIELD-SYMBOLS <fs_data_output> TYPE zscnn_invoice .

    READ TABLE lt_data_output WITH KEY use_check = 'X' TRANSPORTING NO FIELDS .

    IF sy-subrc <> 0.
      MESSAGE e011(zdsd) WITH text-006 .
    ENDIF.

    me->fill_header( CHANGING c_ftpost = lt_ftpost ) .

    me->fill_vendor( CHANGING c_ftpost = lt_ftpost ) .

    lv_count = lv_count + 1 .

    LOOP AT lt_data_output ASSIGNING <fs_data_output> WHERE use_check = 'X' .


      IF <fs_data_output>-qty_mod NE <fs_data_output>-qty AND
         <fs_data_output>-amt_mod NE <fs_data_output>-amt .

        CLEAR lv_wrbtr .
        lv_wrbtr = <fs_data_output>-qty_mod * <fs_data_output>-price .

        IF  <fs_data_output>-amt_mod EQ lv_wrbtr AND ( bkpf-waers EQ me->waers_bukrs ) .

*If quantity and amount is not equal but if we multiple the price unit with the modify quantity
*we get the same value on modified amount, so the user only modified the quantities.
*   QTY <> QTY_MOD
*   AMOUT NOT MODIFIED

          lv_count = lv_count + 1 .
          <fs_data_output>-augbl_buzei = lv_count .

          me->fill_change_qty( EXPORTING i_count = lv_count
                               CHANGING c_data = <fs_data_output>
                                        c_ftpost = lt_ftpost ) .

          me->fill_clear( EXPORTING i_data = <fs_data_output>
                          CHANGING c_ftclear  = lt_ftclear ) .
          CONTINUE .
        ELSE.

*If quantity and amount is not equal but if we multiple the price unit with the modify quantity
*we not get the same value on modified amount, so the user modified the quantity and the amount .
*   QTY <> QTY_MOD
*   AMT <> AMT_MOD

          lv_count = lv_count + 1 .
          <fs_data_output>-augbl_buzei = lv_count .

          me->fill_change_qty_with_diff( EXPORTING i_count = lv_count
                                         CHANGING c_data = <fs_data_output>
                                                  c_ftpost = lt_ftpost ) .


          lv_count = lv_count + 1 .
          me->fill_change_amount_with_diff( EXPORTING i_count = lv_count
                                                      i_data = <fs_data_output>
                                            CHANGING c_ftpost = lt_ftpost ) .


          me->fill_clear( EXPORTING i_data = <fs_data_output>
                          CHANGING c_ftclear  = lt_ftclear ) .
          CONTINUE .
        ENDIF.
      ENDIF.

*The user didn't change the quantities and the amount so we have a complete clearing line.
*   QTY = QTY_MOD
*   AMT = AMT_MOD
      IF <fs_data_output>-qty_mod EQ <fs_data_output>-qty AND
             <fs_data_output>-amt_mod EQ <fs_data_output>-amt .

        me->fill_clear( EXPORTING i_data = <fs_data_output>
                        CHANGING c_ftclear  = lt_ftclear ) .
        CONTINUE .
      ENDIF.

*IF the user only change the amount value .
      IF <fs_data_output>-amt_mod NE <fs_data_output>-amt AND
         <fs_data_output>-qty_mod EQ <fs_data_output>-qty .

        lv_count = lv_count + 1 .
        <fs_data_output>-augbl_buzei = lv_count .

        me->fill_change_amount( EXPORTING i_count = lv_count
                                         i_data = <fs_data_output>
                               CHANGING c_ftpost = lt_ftpost ) .


        me->fill_clear( EXPORTING i_data = <fs_data_output>
                        CHANGING c_ftclear  = lt_ftclear ) .
        CONTINUE .
      ENDIF.

*Qty Not Equal
*Amt Equal
      IF <fs_data_output>-amt_mod EQ <fs_data_output>-amt AND
         <fs_data_output>-qty_mod NE <fs_data_output>-qty .

        lv_count = lv_count + 1 .
        <fs_data_output>-augbl_buzei = lv_count .

        me->fill_change_qty( EXPORTING i_count = lv_count
                             CHANGING c_data = <fs_data_output>
                                      c_ftpost = lt_ftpost ) .

        me->fill_clear( EXPORTING i_data = <fs_data_output>
                        CHANGING c_ftclear  = lt_ftclear ) .

        CONTINUE .
      ENDIF.
    ENDLOOP .

    me->get_diff_foreign_amount( CHANGING c_count = lv_count
                                          c_ftpost = lt_ftpost ) .

    lv_count = lv_count + 1 .

    me->fill_taxes( EXPORTING i_count = lv_count
                    CHANGING c_ftpost = lt_ftpost ) .


    IF me->screen_value EQ 'Y' .
      CALL FUNCTION 'POSTING_INTERFACE_START'
        EXPORTING
          i_function         = c_function
          i_mode             = c_mode_a
        EXCEPTIONS
          client_incorrect   = 1
          function_invalid   = 2
          group_name_missing = 3
          mode_invalid       = 4
          update_invalid     = 5
          OTHERS             = 6.
    ELSE.
      CALL FUNCTION 'POSTING_INTERFACE_START'
        EXPORTING
          i_function         = c_function
          i_mode             = c_mode_n
        EXCEPTIONS
          client_incorrect   = 1
          function_invalid   = 2
          group_name_missing = 3
          mode_invalid       = 4
          update_invalid     = 5
          OTHERS             = 6.
    ENDIF .

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
              TYPE sy-msgty
              NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
    ENDIF.


    CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
      EXPORTING
        i_auglv                    = c_auglv
        i_tcode                    = c_tcode
*       i_xsimu                    = 'X'
      IMPORTING
        e_msgid                    = e_msgid
        e_msgno                    = e_msgno
        e_msgty                    = e_msgty
        e_msgv1                    = e_msgv1
        e_msgv2                    = e_msgv2
        e_msgv3                    = e_msgv3
        e_msgv4                    = e_msgv4
        e_subrc                    = e_subrc
      TABLES
        t_blntab                   = lt_blntab
        t_ftclear                  = lt_ftclear
        t_ftpost                   = lt_ftpost
        t_fttax                    = lt_fttax
      EXCEPTIONS
        clearing_procedure_invalid = 1
        clearing_procedure_missing = 2
        table_t041a_empty          = 3
        transaction_code_invalid   = 4
        amount_format_error        = 5
        too_many_line_items        = 6
        company_code_invalid       = 7
        screen_not_found           = 8
        no_authorization           = 9
        OTHERS                     = 10.


    IF e_subrc <> 0.

      ROLLBACK WORK .

    ELSE.


      READ TABLE lt_blntab INTO lw_blntab INDEX 1 .

      IF sy-subrc = 0 .
        IF ( abap_false = lo_application->update_invoice( lw_blntab ) ) .

          MESSAGE e011(zdsd) WITH text-009 .

        ENDIF.
      ENDIF .
    ENDIF.

    IF NOT e_msgid IS INITIAL .
      MESSAGE ID e_msgid
              TYPE e_msgty
              NUMBER e_msgno
              WITH e_msgv1 e_msgv2 e_msgv3 e_msgv4 .
    ELSE.
      MESSAGE ID sy-msgid
              TYPE sy-msgty
              NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
    ENDIF.

    CALL FUNCTION 'POSTING_INTERFACE_END'
      EXCEPTIONS
        session_not_processable = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
              TYPE sy-msgty
              NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
    ENDIF.

    IF e_subrc = 0 .
      lo_application->show_alv_grid( ) .

      lo_application->clear_screen( ) .
    ENDIF.

    CLEAR:  sy-msgid, sy-msgty , sy-msgno, sy-msgv1, sy-msgv2, sy-msgv3, sy-msgv4 .
    CLEAR:  e_msgid, e_msgty , e_msgno, e_msgv1, e_msgv2, e_msgv3, e_msgv4 .
  ENDMETHOD .                    "call_posting_clearing

  METHOD update_invoice  .
    DATA lw_data TYPE zzscnn_gr .
    DATA lv_amout_invoiced TYPE ze_amt_inv .
    DATA lv_qty_invoiced TYPE ze_qty_inv .

    FIELD-SYMBOLS <fs_data_output> TYPE zscnn_invoice .

    LOOP AT lt_data_output ASSIGNING <fs_data_output> WHERE use_check = 'X'  .

      <fs_data_output>-augbl = i_blntab-belnr .
      <fs_data_output>-augbl_bukrs = i_blntab-bukrs .
      <fs_data_output>-augbl_gjahr = i_blntab-gjahr .

      IF <fs_data_output>-qty_mod EQ <fs_data_output>-qty .

        lv_amout_invoiced = lv_qty_invoiced * <fs_data_output>-price  .

        UPDATE zzscnn_gr
          SET augbl = i_blntab-belnr
              augbl_bukrs = i_blntab-bukrs
              augbl_gjahr = i_blntab-gjahr
              qty_inv = <fs_data_output>-qty_mod
              amt_inv = lv_amout_invoiced
          WHERE belnr = <fs_data_output>-belnr AND
                buzei = <fs_data_output>-buzei .

        IF sy-subrc = 0 .

          COMMIT WORK AND WAIT .

          r_updated = abap_true .
        ENDIF.
      ELSE.

        SELECT SINGLE *
          FROM zzscnn_gr
          INTO lw_data
          WHERE belnr = <fs_data_output>-belnr AND
                buzei = <fs_data_output>-buzei .

        IF sy-subrc = 0 .
          CLEAR lv_amout_invoiced  .
          CLEAR lv_qty_invoiced .
          lv_qty_invoiced = <fs_data_output>-qty_mod + <fs_data_output>-qty_inv .
          lv_amout_invoiced = lv_qty_invoiced * <fs_data_output>-price  .

*Update old Line with the new partial document
          UPDATE zzscnn_gr
            SET belnr = i_blntab-belnr
                bukrs = i_blntab-bukrs
                gjahr = i_blntab-gjahr
                buzei = <fs_data_output>-augbl_buzei
                qty_inv = lv_qty_invoiced
                amt_inv = lv_amout_invoiced
                amt_rem = <fs_data_output>-amt_rem
            WHERE belnr = <fs_data_output>-belnr AND
                  buzei = <fs_data_output>-buzei .

          IF sy-subrc = 0 .
            COMMIT WORK  AND WAIT .
            r_updated = abap_true .
          ENDIF.

*Add a new line with the clearing doc
          lw_data-augbl = i_blntab-belnr .
          lw_data-augbl_bukrs = i_blntab-bukrs .
          lw_data-augbl_gjahr = i_blntab-gjahr .
          lw_data-qty_inv = lw_data-qty .

          INSERT INTO zzscnn_gr VALUES lw_data .

          IF sy-subrc = 0 .
            COMMIT WORK AND WAIT .
            r_updated = abap_true .
          ENDIF .
        ENDIF.
      ENDIF .
    ENDLOOP.
  ENDMETHOD.                    "update_invoice
  METHOD handle_data_changed .
    DATA: lw_change TYPE lvc_s_modi .

    DATA lv_amount TYPE ze_amout .

    DATA lv_qtd_mod TYPE ze_qty_mod .

    FIELD-SYMBOLS <fs_data_output> TYPE zscnn_invoice .

    LOOP AT er_data_changed->mt_good_cells INTO lw_change.

      CASE lw_change-fieldname .

        WHEN 'QTY_MOD'.

          READ TABLE lt_data_output ASSIGNING <fs_data_output> INDEX lw_change-row_id  .
          IF sy-subrc = 0 .
            CONDENSE lw_change-value .
            lv_qtd_mod = lw_change-value .

*            Validate the quantity
            IF lv_qtd_mod > <fs_data_output>-qty .

              CALL METHOD er_data_changed->add_protocol_entry
                EXPORTING
                  i_msgid     = 'ZDSD'
                  i_msgty     = 'E'
                  i_msgno     = '011'
                  i_msgv1     = lv_qtd_mod
                  i_msgv2     = '>'
                  i_msgv3     = <fs_data_output>-qty
                  i_fieldname = 'QTY_MOD'
                  i_row_id    = lw_change-row_id
                  i_tabix     = lw_change-tabix.
            ELSE.

              CLEAR <fs_data_output>-amt_for_mod.
              <fs_data_output>-amt_mod = <fs_data_output>-price * lv_qtd_mod .

              lo_application->refresh_alv_grid( ) .

            ENDIF.
          ENDIF .

        WHEN 'USE_CHECK'.
          CLEAR: rf05a-azsal , lv_amount .

          IF bkpf-waers NE me->waers_bukrs .

            me->update_foreign_balance( CHANGING change = lw_change ) .

          ELSE.

            me->update_balance( CHANGING change = lw_change ) .

          ENDIF .

        WHEN 'AMT_FOR_MOD'.

          READ TABLE lt_data_output ASSIGNING <fs_data_output> INDEX lw_change-row_id  .

          lv_amount  = lw_change-value .

          <fs_data_output>-amt_mod = lo_application->convert_amount( f_curr = bkpf-waers
                                                                     f_amount = lv_amount
                                                                     l_curr = c_curr_cny )  .

      ENDCASE .
    ENDLOOP .
    lo_application->refresh_alv_grid( ) .
  ENDMETHOD.                    "handle_data_changed
  METHOD handle_delayed_changed_sel_cb.
    DATA new_code	TYPE syucomm .
    DATA rc TYPE i .


    IF bkpf-waers NE lo_application->waers_bukrs .

      lo_application->update_foreign_balance( ) .

    ELSE.

      lo_application->update_balance( ) .

    ENDIF .

    new_code = 'UPD_BAL' .

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = new_code
      IMPORTING
        rc       = rc.

    cl_gui_cfw=>dispatch( ) .
    cl_gui_cfw=>flush( ) .

  ENDMETHOD .                    "handle_delayed_changed_sel_cb
  METHOD handle_toolbar  .
*  CONSTANTS FOR BUTTON TYPE
    CONSTANTS: c_select_all   TYPE ui_func VALUE 'SEL_ALL' ,
               c_deselect_all   TYPE ui_func VALUE 'DES_ALL' ,
    c_button_normal           TYPE i VALUE 0,
    c_menu_and_default_button TYPE i VALUE 1,
    c_menu                    TYPE i VALUE 2,
    c_separator               TYPE i VALUE 3,
    c_radio_button            TYPE i VALUE 4,
    c_checkbox                TYPE i VALUE 5,
    c_menu_entry              TYPE i VALUE 6.

    DATA lw_toolbar  TYPE stb_button.

*   APPEND SEPARATOR TO THE NORMAL TOOLBAR
    CLEAR lw_toolbar.
    MOVE c_separator  TO lw_toolbar-butn_type.
    APPEND lw_toolbar TO e_object->mt_toolbar.

    CLEAR lw_toolbar.
    MOVE c_select_all    TO lw_toolbar-function.
    MOVE icon_select_all TO lw_toolbar-icon.
    MOVE text-007        TO lw_toolbar-quickinfo.
    MOVE ' '             TO lw_toolbar-text.
    MOVE ' '             TO lw_toolbar-disabled.
    APPEND lw_toolbar    TO e_object->mt_toolbar .

    CLEAR lw_toolbar.
    MOVE c_deselect_all    TO lw_toolbar-function.
    MOVE icon_deselect_all TO lw_toolbar-icon.
    MOVE text-008          TO lw_toolbar-quickinfo.
    MOVE ' '               TO lw_toolbar-text.
    MOVE ' '               TO lw_toolbar-disabled.
    APPEND lw_toolbar      TO e_object->mt_toolbar .

  ENDMETHOD .                    "handle_toolbar
  METHOD handle_hotspot_click .
    DATA lw_data_aux TYPE zscnn_invoice .
    DATA lt_bkpf TYPE STANDARD TABLE OF bkpf .
    DATA lw_bkpf TYPE bkpf .


    CASE e_column_id .
      WHEN 'BELNR' .

        READ TABLE lt_data_output INTO lw_data_aux INDEX e_row_id .

        IF sy-subrc = 0 .
          SELECT bukrs belnr gjahr
            FROM bkpf
            INTO CORRESPONDING FIELDS OF TABLE lt_bkpf
            WHERE belnr = lw_data_aux-belnr AND
                  gjahr = lw_data_aux-gjahr AND
                  bukrs = lw_data_aux-bukrs .
          IF sy-subrc = 0 .

            SORT lt_bkpf BY gjahr ASCENDING .

            READ TABLE lt_bkpf INTO lw_bkpf INDEX 1 .

            SET PARAMETER ID: 'BLN' FIELD lw_bkpf-belnr ,
                              'BUK' FIELD lw_bkpf-bukrs ,
                              'GJR' FIELD lw_bkpf-gjahr .
            CALL TRANSACTION  'FB03' AND SKIP FIRST SCREEN .
          ENDIF .
        ENDIF .
      WHEN 'AUGBL' .

        READ TABLE lt_data_output INTO lw_data_aux INDEX e_row_id .

        IF sy-subrc = 0 .
          SELECT bukrs belnr gjahr
            FROM bkpf
            INTO CORRESPONDING FIELDS OF TABLE lt_bkpf
            WHERE belnr = lw_data_aux-augbl AND
                  gjahr = lw_data_aux-gjahr AND
                  bukrs = lw_data_aux-bukrs .

          IF sy-subrc = 0 .

            SORT lt_bkpf BY gjahr ASCENDING .

            READ TABLE lt_bkpf INTO lw_bkpf INDEX 1 .

            SET PARAMETER ID: 'BLN' FIELD lw_bkpf-belnr ,
                              'BUK' FIELD lw_bkpf-bukrs ,
                              'GJR' FIELD lw_bkpf-gjahr .
            CALL TRANSACTION  'FB03' AND SKIP FIRST SCREEN .
          ENDIF.
        ENDIF.
    ENDCASE .
  ENDMETHOD .                    "handle_hotspot_click

  METHOD handle_user_command .
    DATA lv_amount TYPE ze_amout.
    FIELD-SYMBOLS <fs_data_output> TYPE zscnn_invoice .


    CLEAR: rf05a-azsal , lv_amount .

    CASE e_ucomm .
      WHEN 'SEL_ALL' .
        LOOP AT lt_data_output ASSIGNING  <fs_data_output> .
          rf05a-azsal = ( rf05a-azsal + ( <fs_data_output>-amt_mod - <fs_data_output>-amt ) ) .
          lv_amount = lv_amount + <fs_data_output>-amt .
          <fs_data_output>-use_check = 'X' .
        ENDLOOP .

      WHEN 'DES_ALL' .
        LOOP AT lt_data_output ASSIGNING  <fs_data_output> .
          CLEAR <fs_data_output>-use_check .
        ENDLOOP .
    ENDCASE .

    lo_application->refresh_alv_grid( ) .

    rf05a-azsal = ( bseg-wrbtr * -1 ) + bseg-wmwst + rf05a-azsal + lv_amount  .

    IF rf05a-azsal IS INITIAL  .
      rf05a-ampel =  green_status .
    ELSEIF rf05a-azsal > 0 .
      rf05a-ampel =  red_status .
    ELSEIF rf05a-azsal < 0 .
      rf05a-ampel =  yellow_status.
    ENDIF .
  ENDMETHOD .                    "handle_user_command

  METHOD update_foreign_balance.


    DATA lv_amount TYPE ze_amout .

    FIELD-SYMBOLS <fs_data_output> TYPE zscnn_invoice .
    CLEAR: rf05a-azsal , lv_amount .

    LOOP AT lo_application->lt_data_output ASSIGNING <fs_data_output> WHERE use_check = 'X' .
      rf05a-azsal = ( rf05a-azsal + ( <fs_data_output>-amt_for_mod ) ) .
*      lv_amount = lv_amount + <fs_data_output>-amt_for_mod .
    ENDLOOP .

    IF NOT change IS INITIAL  .

      READ TABLE lt_data_output ASSIGNING <fs_data_output> INDEX change-row_id  .
      IF sy-subrc = 0 .
        CONDENSE change-value .
        IF change-value EQ 'X' .
          rf05a-azsal = ( rf05a-azsal + ( <fs_data_output>-amt_for_mod ) ) .
*          lv_amount = lv_amount + <fs_data_output>-amt_for_mod .
        ELSE.
          rf05a-azsal = ( rf05a-azsal - ( <fs_data_output>-amt_for_mod ) ) .
*          lv_amount = lv_amount - <fs_data_output>-amt_for_mod .
        ENDIF.
      ENDIF .

    ENDIF .

    rf05a-azsal = ( bseg-wrbtr * -1 ) + bseg-wmwst + rf05a-azsal  .

    IF rf05a-azsal IS INITIAL  .
      rf05a-ampel =  green_status .
    ELSEIF rf05a-azsal > 0 .
      rf05a-ampel =  red_status .
    ELSEIF rf05a-azsal < 0 .
      rf05a-ampel =  yellow_status.
    ENDIF .
  ENDMETHOD .                    "update_foreign_balance

  METHOD update_balance .
    DATA lv_amount TYPE ze_amout .

    FIELD-SYMBOLS <fs_data_output> TYPE zscnn_invoice .

    CLEAR: rf05a-azsal , lv_amount .


    LOOP AT lo_application->lt_data_output ASSIGNING <fs_data_output> WHERE use_check = 'X' .
      rf05a-azsal = ( rf05a-azsal + ( <fs_data_output>-amt_mod - <fs_data_output>-amt ) ) .
      lv_amount = lv_amount + <fs_data_output>-amt .

    ENDLOOP .

    IF NOT change IS INITIAL  .
      READ TABLE lt_data_output ASSIGNING <fs_data_output> INDEX change-row_id  .
      IF sy-subrc = 0 .
        CONDENSE change-value .
        IF change-value EQ 'X' .
          rf05a-azsal = ( rf05a-azsal + ( <fs_data_output>-amt_mod - <fs_data_output>-amt ) ) .
          lv_amount = lv_amount + <fs_data_output>-amt .
        ELSE.
          rf05a-azsal = ( rf05a-azsal + ( <fs_data_output>-amt_mod - <fs_data_output>-amt ) ) .
          lv_amount = lv_amount - <fs_data_output>-amt .
        ENDIF.
      ENDIF .


    ENDIF .
    rf05a-azsal = ( bseg-wrbtr * -1 ) + bseg-wmwst + rf05a-azsal + lv_amount  .

    IF rf05a-azsal IS INITIAL  .
      rf05a-ampel =  green_status .
    ELSEIF rf05a-azsal > 0 .
      rf05a-ampel =  red_status .
    ELSEIF rf05a-azsal < 0 .
      rf05a-ampel =  yellow_status.
    ENDIF .
  ENDMETHOD .                    "update_balance

  METHOD get_diff_foreign_amount  .
    DATA lv_wrbtr       TYPE wrbtr .
    DATA lv_conv_wrbtr  TYPE wrbtr .
    DATA lv_pswsl       TYPE pswsl .
    DATA lv_item_code   TYPE ze_item_code .
    DATA lv_account_diff TYPE ze_diff_saknr .
    DATA lw_ftpost TYPE ftpost .

    FIELD-SYMBOLS <fs_data_output> TYPE zscnn_invoice .

    IF bkpf-waers NE me->waers_bukrs .
      LOOP AT lt_data_output ASSIGNING <fs_data_output> WHERE use_check = 'X' .

        SELECT SINGLE wrbtr pswsl
          FROM bseg
          INTO (lv_wrbtr,lv_pswsl)
          WHERE bukrs = <fs_data_output>-bukrs
            AND belnr = <fs_data_output>-belnr
            AND gjahr = <fs_data_output>-gjahr
            AND buzei = <fs_data_output>-buzei .

        IF sy-subrc = 0 AND lv_pswsl EQ bkpf-waers.

          lv_wrbtr = (  me->convert_amount( f_curr = me->waers_bukrs
                                            f_amount = <fs_data_output>-amt
                                            l_curr = bkpf-waers ) - lv_wrbtr ) .

          IF lv_wrbtr <> 0 .

            c_count = c_count + 1 .
            lw_ftpost-count = c_count .
            lw_ftpost-stype = 'P'.
            lw_ftpost-fnam = 'RF05A-NEWKO'.

            lv_item_code = <fs_data_output>-item_code.

            TRANSLATE lv_item_code TO UPPER CASE.

            SELECT SINGLE saknr_diff
              INTO lv_account_diff
              FROM zzscnngr_saknr
              WHERE item_code = lv_item_code .

            IF sy-subrc = 0 .
              lw_ftpost-fval = lv_account_diff .
              APPEND lw_ftpost TO c_ftpost.
            ELSE.
              MESSAGE e011(zdsd) WITH lv_item_code text-010 .
              EXIT.
            ENDIF.

            IF lv_wrbtr > 0 .
              lw_ftpost-fval = c_debit_entry . "'40' .
            ELSE.
              lw_ftpost-fval = c_credit_entry . "'50' .
              lv_wrbtr = lv_wrbtr * -1 .
            ENDIF .
            lw_ftpost-fnam = 'RF05A-NEWBS'.
            APPEND lw_ftpost TO c_ftpost.

            lw_ftpost-fnam = 'BSEG-WRBTR'.
            lw_ftpost-fval = lv_wrbtr .
            CONDENSE lw_ftpost-fval .

            IF me->dcpfm <> 'X'.
              REPLACE ALL OCCURRENCES OF '.' IN lw_ftpost-fval WITH ','.
            ENDIF.
            APPEND lw_ftpost TO c_ftpost.

            lv_wrbtr = '0.01' .
            lw_ftpost-fnam = 'BSEG-DMBTR' .
            lw_ftpost-fval = lv_wrbtr .
            CONDENSE lw_ftpost-fval .

            IF me->dcpfm <> 'X'.
              REPLACE ALL OCCURRENCES OF '.' IN lw_ftpost-fval WITH ','.
            ENDIF.
            APPEND lw_ftpost TO c_ftpost.


            lw_ftpost-fnam = 'RF05A-NEWUM'.
            lw_ftpost-fval = ' ' .
            APPEND lw_ftpost TO c_ftpost .

            lw_ftpost-fnam = 'BSEG-MWSKZ' .
            lw_ftpost-fval = bseg-mwskz .
            APPEND lw_ftpost TO c_ftpost .


          ENDIF .
        ENDIF .

      ENDLOOP .
    ENDIF .
  ENDMETHOD .                    "get_diff_foreign_amount

  METHOD get_user_screen_parameter .

    SELECT SINGLE parva
      INTO me->screen_value
      FROM usr05
      WHERE bname = sy-uname
      AND parid = c_invoice_param .
  ENDMETHOD .                    "get_user_screen_parameter
ENDCLASS .                    "lcl_application IMPLEMENTATION
