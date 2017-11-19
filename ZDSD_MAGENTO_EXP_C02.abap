*----------------------------------------------------------------------*
* OBJECT NAME           : ZDSD_MAGENTO_EXP_C02
* PROGRAM TITLE         : Program Implementations
* AUTHOR                : Andrei Gidilica
* DATE                  : 07/11/2017
* RICEFW OBJECT         :
* CHANGE REQUEST NUMBER :
* DESCRIPTION           : Magento Exports
*----------------------------------------------------------------------*
* MOD. NO.| DATE | USER | CHANGE REFERENCE *
*----------------------------------------------------------------------*
CLASS lcl_appl IMPLEMENTATION.
  METHOD start.
* object instance
    TRY.
        get_inst( ir_matnr = ir_matnr
                  ir_mtart = ir_mtart
                  ir_werks = ir_werks
                  ir_date  = ir_date
                  iv_pltyp = iv_pltyp
                  iv_file  = iv_file
                  iv_lcle  = iv_lcle
                  iv_ftpe  = iv_ftpe
                  iv_show  = iv_show )->process( ).
      CATCH zcx_core_exception.
        MESSAGE ID sy-msgid
              TYPE sy-msgty
            NUMBER sy-msgno
              WITH sy-msgv1
                   sy-msgv2
                   sy-msgv3
                   sy-msgv4.
        LEAVE LIST-PROCESSING.
    ENDTRY.
  ENDMETHOD.

  METHOD get_inst.
    IF NOT mo_obj IS BOUND.
      CREATE OBJECT mo_obj
        EXPORTING
          ir_matnr = ir_matnr
          ir_mtart = ir_mtart
          ir_werks = ir_werks
          ir_date  = ir_date
          iv_pltyp = iv_pltyp
          iv_file  = iv_file
          iv_lcle  = iv_lcle
          iv_ftpe  = iv_ftpe
          iv_show  = iv_show.
    ENDIF.

    ro_obj = mo_obj.
  ENDMETHOD.

  METHOD constructor.
    ms_sel-r_matnr = ir_matnr.
    ms_sel-r_mtart = ir_mtart.
    ms_sel-r_werks = ir_werks.
    ms_sel-r_date  = ir_date.
    ms_sel-v_pltyp = iv_pltyp.
    ms_sel-v_file  = iv_file.
    ms_sel-v_lcle  = iv_lcle.
    ms_sel-v_ftpe  = iv_ftpe.
    ms_sel-v_show  = iv_show.

* movement - delv GI
    ms_sel-v_bwart = zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
                       iv_constant_id = mc_const_id
                       iv_vakey1      = mc_bwart
                       iv_type        = mc_param ).

* active flag - in stock
    ms_sel-v_actv = zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
                      iv_constant_id = mc_const_id
                      iv_vakey1      = mc_instk
                      iv_type        = mc_param ).

* sales organization
    ms_sel-v_vkorg = zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
                       iv_constant_id = mc_const_id
                       iv_vakey1      = mc_vkorg
                       iv_type        = mc_param ).

* distribution channel
    ms_sel-v_vtweg = zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
                       iv_constant_id = mc_const_id
                       iv_vakey1      = mc_vtweg
                       iv_type        = mc_param ).

* division
    ms_sel-v_spart = zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
                       iv_constant_id = mc_const_id
                       iv_vakey1      = mc_spart
                       iv_type        = mc_param ).

* document type
    ms_sel-v_auart = zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
                       iv_constant_id = mc_const_id
                       iv_vakey1      = mc_auart
                       iv_type        = mc_param ).

* currency
    ms_sel-v_waerk = zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
                       iv_constant_id = mc_const_id
                       iv_vakey1      = mc_waerk
                       iv_type        = mc_param ).

* item number
    ms_sel-v_posnr = zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
                       iv_constant_id = mc_const_id
                       iv_vakey1      = mc_posnr
                       iv_type        = mc_param ).

* required quantity
    ms_sel-v_reqty = zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
                       iv_constant_id = mc_const_id
                       iv_vakey1      = mc_reqty
                       iv_type        = mc_param ).

* customer
    ms_sel-v_kunnr = zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
                       iv_constant_id = mc_const_id
                       iv_vakey1      = mc_kunnr
                       iv_type        = mc_param ).

* condition from
    ms_sel-v_condf = zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
                       iv_constant_id = mc_const_id
                       iv_vakey1      = mc_condf
                       iv_type        = mc_param ).

* condition to
    ms_sel-v_condt = zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
                       iv_constant_id = mc_const_id
                       iv_vakey1      = mc_condt
                       iv_type        = mc_param ).

* FTP host
    ms_sel-s_ftp-host = zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
                          iv_constant_id = mc_const_id
                          iv_vakey1      = mc_host
                          iv_type        = mc_param ).

* FTP user
    ms_sel-s_ftp-usr = zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
                         iv_constant_id = mc_const_id
                         iv_vakey1      = mc_usr
                         iv_type        = mc_param ).

* FTP password
    ms_sel-s_ftp-pwd = zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
                         iv_constant_id = mc_const_id
                         iv_vakey1      = mc_pwd
                         iv_type        = mc_param ).

* FTP path
    ms_sel-s_ftp-path = zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
                          iv_constant_id = mc_const_id
                          iv_vakey1      = mc_path
                          iv_type        = mc_param ).

    CONCATENATE ms_sel-s_ftp-path sy-uzeit sy-datum mc_ext
           INTO ms_sel-s_ftp-path.
  ENDMETHOD.

  METHOD process.
* products selection
    mat_sel( ).

* output preparation
    out_prep( ).

* store file either on local pc or on FTP loc.
    save_file( ).

* ALV display
    IF ms_sel-v_show EQ abap_true.
      alv( ).
    ENDIF.
  ENDMETHOD.

  METHOD mat_sel.
    DATA: lt_mat TYPE tty_mat.

* products selection
    SELECT matnr werks
      FROM marc
      INTO TABLE lt_mat
      PACKAGE SIZE mc_p_size
      WHERE matnr IN ms_sel-r_matnr
      AND   werks IN ms_sel-r_werks.

* filter products by material type
      filter_by_mat_typ(
        CHANGING
          ct_mat = lt_mat ).

* filter products by document posting date
      filter_by_post_date(
        CHANGING
          ct_mat = lt_mat ).

* storage location stock
      calc_mat_stock( lt_mat ).

      INSERT LINES OF lt_mat INTO TABLE ms_wd-t_mat.
      CLEAR lt_mat.
    ENDSELECT.

    IF ms_wd-t_mat IS INITIAL.
      MESSAGE i001(zsd_exp) INTO mv_dummy.
      RAISE EXCEPTION TYPE zcx_core_exception.
    ENDIF.
  ENDMETHOD.

  METHOD filter_by_mat_typ.
    DATA: lt_mat_s TYPE tty_mat_s,
          lt_mat_h TYPE tty_mat_h,
          lr_mat   TYPE REF TO ty_mat.

    lt_mat_h = ct_mat.
    SORT lt_mat_h BY matnr.
    DELETE ADJACENT DUPLICATES FROM lt_mat_h
                          COMPARING matnr.

    IF NOT lt_mat_h IS INITIAL.
      SELECT matnr
        FROM mara
        INTO TABLE lt_mat_s
        FOR ALL ENTRIES IN lt_mat_h
        WHERE matnr =  lt_mat_h-matnr
        AND   mtart IN ms_sel-r_mtart.

      IF sy-subrc EQ 0.
        LOOP AT ct_mat REFERENCE INTO lr_mat.
          READ TABLE lt_mat_s TRANSPORTING NO FIELDS
                              WITH TABLE KEY matnr = lr_mat->matnr.

          IF sy-subrc NE 0.
            DELETE ct_mat.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD filter_by_post_date.
    DATA: lt_mat_d  TYPE tty_mat_d,
          lt_mat_dh TYPE tty_mat_dh,
          lr_mat    TYPE REF TO ty_mat,
          lr_mat_d  TYPE REF TO ty_mat_d,
          lr_mat_dh TYPE REF TO ty_mat_dh,
          lv_del    TYPE flag.

    IF ct_mat IS INITIAL.
      RETURN.
    ENDIF.

    SELECT mblnr mjahr
           matnr werks
      FROM mseg
      INTO TABLE lt_mat_d
      FOR ALL ENTRIES IN ct_mat
      WHERE bwart = ms_sel-v_bwart
      AND   matnr = ct_mat-matnr
      AND   werks = ct_mat-werks.

    IF sy-subrc EQ 0.
      SORT lt_mat_d BY mblnr mjahr.
      DELETE ADJACENT DUPLICATES FROM lt_mat_d
                            COMPARING mblnr mjahr.

      IF NOT lt_mat_d IS INITIAL.
        SELECT mblnr mjahr budat
          FROM mkpf
          INTO TABLE lt_mat_dh
          FOR ALL ENTRIES IN lt_mat_d
          WHERE mblnr = lt_mat_d-mblnr
          AND   mjahr = lt_mat_d-mjahr.

        IF sy-subrc EQ 0.
          LOOP AT ct_mat REFERENCE INTO lr_mat.
            lv_del = abap_false.
            LOOP AT lt_mat_d REFERENCE INTO lr_mat_d WHERE matnr = lr_mat->matnr
                                                     AND   werks = lr_mat->werks.
              READ TABLE lt_mat_dh REFERENCE INTO lr_mat_dh
                                   WITH TABLE KEY mblnr = lr_mat_d->mblnr
                                                  mjahr = lr_mat_d->mjahr.

              IF sy-subrc EQ 0.
                IF NOT lr_mat_dh->budat IN ms_sel-r_date.
                  lv_del = abap_true.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDLOOP.

            IF lv_del = abap_true.
              DELETE ct_mat.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD calc_mat_stock.
    DATA: lt_mat_st   TYPE tty_mat_st,
          lr_mat_st   TYPE REF TO ty_mat_st,
          lr_mat_st_s TYPE REF TO ty_mat_st_s,
          ls_mat_st_s TYPE ty_mat_st_s,
          lt_mat      TYPE tty_mat_h.

    lt_mat = it_mat.
    SORT lt_mat BY matnr.
    DELETE ADJACENT DUPLICATES FROM lt_mat
                          COMPARING matnr.

    IF lt_mat IS INITIAL.
      RETURN.
    ENDIF.

    SELECT matnr werks
           lgort labst
      FROM mard
      INTO TABLE lt_mat_st
      FOR ALL ENTRIES IN lt_mat
      WHERE matnr = lt_mat-matnr
      AND   werks IN ms_sel-r_werks.

    IF sy-subrc EQ 0.
      LOOP AT lt_mat_st REFERENCE INTO lr_mat_st.
        READ TABLE ms_wd-t_mat_st_s REFERENCE INTO lr_mat_st_s
                                    WITH TABLE KEY matnr = lr_mat_st->matnr.

        IF sy-subrc EQ 0.
          lr_mat_st_s->labst = lr_mat_st_s->labst + lr_mat_st->labst.
        ELSE.
          CLEAR ls_mat_st_s.
          ls_mat_st_s-matnr = lr_mat_st->matnr.
          ls_mat_st_s-labst = lr_mat_st->labst.
          INSERT ls_mat_st_s INTO TABLE ms_wd-t_mat_st_s.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD calc_price.
    DATA: ls_ord_h   TYPE bapisdhead,
          lt_ord_i   TYPE tty_ord_i,
          lt_ord_p   TYPE tty_ord_p,
          lt_ord_s   TYPE tty_ord_s,
          lt_ord_c   TYPE tty_ord_c,
          lr_ord_c   TYPE REF TO bapicond,
          lv_s_calc  TYPE flag,
          lv_c_val   TYPE bapikwert1,
          lv_c_val_s TYPE bapikwert1,
          lv_date    TYPE datab.

* header
    order_head(
      IMPORTING
        es_ord_h = ls_ord_h ).

* item
    order_item(
      EXPORTING
        iv_matnr = iv_matnr
      IMPORTING
        et_ord_i = lt_ord_i ).

* order partners
    order_part(
      IMPORTING
        et_ord_p = lt_ord_p ).

* item scheduling lines
    order_sch(
      IMPORTING
        et_ord_s = lt_ord_s ).

    CALL FUNCTION 'BAPI_SALESORDER_SIMULATE'
      EXPORTING
        order_header_in    = ls_ord_h
      TABLES
        order_items_in     = lt_ord_i
        order_partners     = lt_ord_p
        order_schedule_in  = lt_ord_s
        order_condition_ex = lt_ord_c.

    lv_s_calc = abap_false.
    LOOP AT lt_ord_c REFERENCE INTO lr_ord_c.
* from condition ZPRG
      IF lr_ord_c->cond_type EQ ms_sel-v_condf.
        lv_s_calc = abap_true.
      ENDIF.

* To condition Z064
      IF lr_ord_c->cond_type EQ ms_sel-v_condt.
        lv_c_val_s = lv_c_val + lr_ord_c->condvalue.
        SELECT SINGLE datab
          FROM konh
          INTO lv_date
          WHERE knumh = lr_ord_c->cond_no.
        EXIT.
      ENDIF.

      IF lv_s_calc EQ abap_true.
        lv_c_val = lv_c_val + lr_ord_c->condvalue.
      ENDIF.
    ENDLOOP.

    cs_out-s_price = lv_c_val.
    CONDENSE cs_out-s_price NO-GAPS.
    cs_out-d_price = lv_c_val_s.
    CONDENSE cs_out-d_price NO-GAPS.
    cs_out-datab = lv_date.
  ENDMETHOD.

  METHOD order_head.
    CLEAR es_ord_h.

    CALL FUNCTION 'CONVERSION_EXIT_AUART_INPUT'
      EXPORTING
        input  = ms_sel-v_auart
      IMPORTING
        output = es_ord_h-doc_type.
    es_ord_h-sales_org  = ms_sel-v_vkorg.
    es_ord_h-distr_chan = ms_sel-v_vtweg.
    es_ord_h-division   = ms_sel-v_spart.
    es_ord_h-price_list = ms_sel-v_pltyp.
    es_ord_h-currency   = ms_sel-v_waerk.
  ENDMETHOD.

  METHOD order_item.
    DATA: ls_item TYPE bapiitemin.

    CLEAR et_ord_i.
    ls_item-itm_number = ms_sel-v_posnr.
    ls_item-material   = iv_matnr.
    APPEND ls_item TO et_ord_i.
  ENDMETHOD.

  METHOD order_sch.
    DATA: ls_item_s TYPE bapischdl.

    CLEAR et_ord_s.
    ls_item_s-itm_number = ms_sel-v_posnr.
    ls_item_s-req_qty    = ms_sel-v_reqty.
    APPEND ls_item_s TO et_ord_s.
  ENDMETHOD.

  METHOD order_part.
    DATA: ls_part  TYPE bapipartnr,
          lv_kunnr TYPE kunnr.

    CLEAR et_ord_p.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ms_sel-v_kunnr
      IMPORTING
        output = lv_kunnr.

    CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
      EXPORTING
        input  = mc_shipto
      IMPORTING
        output = ls_part-partn_role.
    ls_part-partn_numb = lv_kunnr.
    APPEND ls_part TO et_ord_p.

    CLEAR ls_part.
    CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
      EXPORTING
        input  = mc_soldto
      IMPORTING
        output = ls_part-partn_role.
    ls_part-partn_numb = lv_kunnr.
    APPEND ls_part TO et_ord_p.
  ENDMETHOD.

  METHOD out_prep.
    DATA: lr_mat      TYPE REF TO ty_mat,
          ls_out      TYPE zds_mangento_exp,
          lr_mat_st_s TYPE REF TO ty_mat_st_s.

    LOOP AT ms_wd-t_mat REFERENCE INTO lr_mat.
      AT NEW matnr.
        CLEAR ls_out.
        ls_out-matnr = lr_mat->matnr.

        READ TABLE ms_wd-t_mat_st_s REFERENCE INTO lr_mat_st_s
                                    WITH TABLE KEY matnr = lr_mat->matnr.
        IF sy-subrc EQ 0.
          ls_out-labst = lr_mat_st_s->labst.
          CONDENSE ls_out-labst NO-GAPS.
        ENDIF.

        ls_out-in_stk = ms_sel-v_actv.
        ls_out-cm_stk = ms_sel-v_actv.

        calc_price(
          EXPORTING
            iv_matnr = lr_mat->matnr
          CHANGING
            cs_out   = ls_out ).

        APPEND ls_out TO ms_wd-t_out.
      ENDAT.
    ENDLOOP.
  ENDMETHOD.

  METHOD alv.
    DATA: lo_tab   TYPE REF TO cl_salv_table,
          lo_funct TYPE REF TO cl_salv_functions_list,
          lo_cols  TYPE REF TO cl_salv_columns_table.

    IF ms_wd-t_out IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            list_display = if_salv_c_bool_sap=>false
          IMPORTING
            r_salv_table = lo_tab
          CHANGING
            t_table      = ms_wd-t_out ).
      CATCH cx_salv_msg.
        RAISE EXCEPTION TYPE zcx_core_exception.
    ENDTRY.

    IF NOT lo_tab IS BOUND.
      RETURN.
    ENDIF.

    lo_funct = lo_tab->get_functions( ).
    lo_funct->set_all( if_salv_c_bool_sap=>true ).
    lo_cols = lo_tab->get_columns( ).
    lo_cols->set_optimize( abap_true ).

    lo_tab->display( ).
  ENDMETHOD.

  METHOD save_file.
    DATA: lt_data TYPE soli_tab,
          lr_out  TYPE REF TO zds_mangento_exp.             "#EC NEEDED

    CASE abap_true.
      WHEN ms_sel-v_lcle.
* save excel file on local pc
        zdcl_file_tools=>save_to_xlsx_file(
          EXPORTING
            iv_fhead = abap_true
            iv_file  = ms_sel-v_file
          CHANGING
            ct_input = ms_wd-t_out ).

* delete header line
        LOOP AT ms_wd-t_out REFERENCE INTO lr_out.
          DELETE ms_wd-t_out.
          EXIT.
        ENDLOOP.

      WHEN ms_sel-v_ftpe.
        conv_tab_to_txt(
          IMPORTING
            et_data = lt_data ).

* save excel file on FTP
        zdcl_file_tools=>ftp_file_write(
          EXPORTING
            is_ftp_config = ms_sel-s_ftp
            iv_char_m     = abap_true
            it_data       = lt_data ).
    ENDCASE.
  ENDMETHOD.

  METHOD conv_tab_to_txt.
    DATA: lr_out   TYPE REF TO zds_mangento_exp,
          lr_str   TYPE REF TO cl_abap_structdescr,
          ls_line  TYPE soli,
          lv_comps TYPE i,
          lv_count TYPE i.

    FIELD-SYMBOLS <lfs_any> TYPE any.

    CLEAR et_data.

    lr_str ?= cl_abap_typedescr=>describe_by_name( 'ZDS_MANGENTO_EXP' ).
    DESCRIBE TABLE lr_str->components LINES lv_comps.

* header line
    head_line_add(
      EXPORTING
        iv_comps = lv_comps
      IMPORTING
        et_data  = et_data ).

    LOOP AT ms_wd-t_out REFERENCE INTO lr_out.
      CLEAR: ls_line, lv_count.

      DO lv_comps TIMES.
        lv_count = lv_count + 1.
        ASSIGN COMPONENT lv_count OF STRUCTURE lr_out->* TO <lfs_any>.
        IF <lfs_any> IS ASSIGNED.
          CONCATENATE ls_line-line <lfs_any> mc_sep
                 INTO ls_line-line.
        ENDIF.
        UNASSIGN <lfs_any>.
      ENDDO.

      CONDENSE ls_line-line NO-GAPS.
      APPEND ls_line TO et_data.
    ENDLOOP.
  ENDMETHOD.

  METHOD head_line_add.
    DATA: lt_fcat TYPE slis_t_fieldcat_alv,
          lr_fcat TYPE REF TO slis_fieldcat_alv,
          lr_any  TYPE REF TO data,
          ls_line TYPE soli.

    FIELD-SYMBOLS: <lfs_any> TYPE any,
                   <lfs_val> TYPE any.

    CLEAR et_data.

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZDS_MANGENTO_EXP'
      CHANGING
        ct_fieldcat            = lt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_core_exception.
    ELSE.
      CREATE DATA lr_any TYPE ('ZDS_MANGENTO_EXP').
      ASSIGN lr_any->* TO <lfs_any>.
      IF <lfs_any> IS ASSIGNED.
        LOOP AT lt_fcat REFERENCE INTO lr_fcat.
          ASSIGN COMPONENT lr_fcat->fieldname OF STRUCTURE <lfs_any> TO <lfs_val>.
          IF <lfs_val> IS ASSIGNED.
            <lfs_val> = lr_fcat->seltext_l.
          ENDIF.
        ENDLOOP.

        DO iv_comps TIMES.
          ASSIGN COMPONENT sy-index OF STRUCTURE <lfs_any> TO <lfs_val>.
          IF <lfs_val> IS ASSIGNED.
            CONCATENATE ls_line-line <lfs_val> mc_sep
                   INTO ls_line-line.
          ENDIF.
          UNASSIGN <lfs_val>.
        ENDDO.

        CONDENSE ls_line-line NO-GAPS.
        APPEND ls_line TO et_data.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD scr_out.
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN mc_s_finp OR
             mc_s_ftxt.
          CASE iv_ftpe.
            WHEN abap_true.
              screen-active = '0'.
            WHEN abap_false.
              screen-active = '1'.
          ENDCASE.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.                    "scr_out

  METHOD save_dialog.
    DATA: lv_fname    TYPE string,
          lv_fpath    TYPE string,
          lv_fullpath TYPE string.

    CLEAR ev_file.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        default_extension         = mc_f_ext
        default_file_name         = ' '
      CHANGING
        filename                  = lv_fname
        path                      = lv_fpath
        fullpath                  = lv_fullpath
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5 ).

    IF sy-subrc EQ 0.
      ev_file = lv_fullpath.
    ELSE.
      MESSAGE ID sy-msgid
            TYPE sy-msgty
          NUMBER sy-msgno
            WITH sy-msgv1
                 sy-msgv2
                 sy-msgv3
                 sy-msgv4.
    ENDIF.
  ENDMETHOD.                    "save_dialog

  METHOD init.
    DATA: ls_date  TYPE datum_range,
          ls_plant TYPE range_s_werks_d,
          ls_mtart TYPE zds_mtart,
          lt_sel_p TYPE tty_sel_par,
          lr_sel_p TYPE REF TO zswtaconstval.

    CLEAR: ev_pltyp,
           er_date,
           er_plant,
           er_mtart.

* price list
    ev_pltyp = zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
                 iv_constant_id = mc_const_id
                 iv_vakey1      = mc_pltyp
                 iv_type        = mc_param ).

* last stock movement date
    ls_date-sign   = 'I'.
    ls_date-option = 'LE'.
    CALL FUNCTION 'CCM_GO_BACK_MONTHS'
      EXPORTING
        currdate   = sy-datum
        backmonths = mc_1_year
      IMPORTING
        newdate    = ls_date-low.
    APPEND ls_date TO er_date.

* plant
    CLEAR lt_sel_p.
    zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
      EXPORTING
        iv_constant_id    = mc_const_id
        iv_vakey1         = mc_werks
        iv_type           = mc_param
      CHANGING
        ct_sel_par_values = lt_sel_p ).

    LOOP AT lt_sel_p REFERENCE INTO lr_sel_p.
      CLEAR ls_plant.
      ls_plant-sign   = 'I'.
      ls_plant-option = 'EQ'.
      ls_plant-low    = lr_sel_p->low.
      APPEND ls_plant TO er_plant.
    ENDLOOP.

* material type
    CLEAR lt_sel_p.
    zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
      EXPORTING
        iv_constant_id    = mc_const_id
        iv_vakey1         = mc_mtart
        iv_type           = mc_param
      CHANGING
        ct_sel_par_values = lt_sel_p ).

    LOOP AT lt_sel_p REFERENCE INTO lr_sel_p.
      CLEAR ls_mtart.
      ls_mtart-sign   = 'I'.
      ls_mtart-option = 'EQ'.
      ls_mtart-low = lr_sel_p->low.
      APPEND ls_mtart TO er_mtart.
    ENDLOOP.
  ENDMETHOD.

  METHOD check.
    IF iv_ucomm EQ mc_rb.
      RETURN.
    ENDIF.

    IF ir_date IS INITIAL.
      SET CURSOR FIELD 'S_DATE-LOW'.
      MESSAGE e055(00).
    ENDIF.

    IF iv_pltyp IS INITIAL.
      SET CURSOR FIELD 'P_PLTYP'.
      MESSAGE e055(00).
    ENDIF.

    IF iv_lcle EQ abap_true.
      IF iv_file IS INITIAL.
        SET CURSOR FIELD 'P_FILE'.
        MESSAGE e055(00).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.                    "lcl_appl IMPLEMENTATION
