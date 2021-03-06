FORM order_confirmation_tt.  "USING rc .

  DATA lt_timetickets        TYPE STANDARD TABLE OF bapi_pp_timeticket .
  DATA lt_goodsmovements     TYPE STANDARD TABLE OF bapi2017_gm_item_create .
  DATA lt_link_conf_goodsmov TYPE STANDARD TABLE OF bapi_link_conf_goodsmov .
  DATA lt_detail_return      TYPE STANDARD TABLE OF bapi_coru_return .
  DATA ls_timetickets        TYPE bapi_pp_timeticket .
  DATA ls_goodsmovements     TYPE bapi2017_gm_item_create .
  DATA ls_link_conf_goodsmov TYPE bapi_link_conf_goodsmov .
  DATA ls_detail_return      TYPE bapi_coru_return .
  DATA ls_mseg               TYPE mseg .
  DATA ls_bapiret1           TYPE bapiret1 .

  ls_timetickets-orderid = ea-aufnr .
  ls_timetickets-postg_date = sy-datum .
  ls_timetickets-fin_conf = 1 .
  ls_timetickets-clear_res = 'X' .

  ls_timetickets-operation = zcl_harnessing_core=>zif_harnessing_core~constant_values_get(
                                                     iv_constant_id = mc_cutting_plan
                                                     iv_vakey1 = mc_vornr
                                                     iv_type = 'P' ) .
  ls_timetickets-ex_created_date = sy-datum .
  ls_timetickets-yield = 1 .
  ls_timetickets-exec_start_date = sy-datum .
  ls_timetickets-exec_fin_date = sy-datum .
  APPEND ls_timetickets TO lt_timetickets.

  LOOP AT gt_mtab INTO gs_mtab .
    ls_goodsmovements-material = gs_mtab-matnr .
    ls_goodsmovements-plant = gs_mtab-werks .
    ls_goodsmovements-stge_loc = gs_mtab-lgort .
    ls_goodsmovements-entry_qnt = gs_mtab-bdmng .
    LOOP AT imseg INTO ls_mseg WHERE charg NE gs_mtab-charg.
      ls_goodsmovements-batch = ls_mseg-charg.
    ENDLOOP.

    ls_goodsmovements-reserv_no = gs_mtab-reserv_no .
    ls_goodsmovements-res_item = gs_mtab-res_item .
  ENDLOOP.

  ls_goodsmovements-move_type = 261 .
  APPEND ls_goodsmovements TO lt_goodsmovements .

  ls_link_conf_goodsmov-index_confirm = 1 .
  ls_link_conf_goodsmov-index_goodsmov = 1 .
  APPEND ls_link_conf_goodsmov TO lt_link_conf_goodsmov .

  CALL FUNCTION 'BAPI_PRODORDCONF_CREATE_TT'
    IMPORTING
      return             = ls_bapiret1
    TABLES
      timetickets        = lt_timetickets
      goodsmovements     = lt_goodsmovements
      link_conf_goodsmov = lt_link_conf_goodsmov
      detail_return      = lt_detail_return.

  READ TABLE lt_detail_return INTO ls_detail_return WITH KEY id = 'RU' number = '100' .
  IF sy-subrc = 0 .
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    messtab-msgtyp = ls_detail_return-type .
    messtab-msgid = ls_detail_return-id .
    messtab-msgnr = ls_detail_return-number .
    messtab-msgv1 = ls_detail_return-message_v1 .
    messtab-msgv2 = ls_detail_return-message_v2 .
    messtab-msgv3 = ls_detail_return-message_v3 .
    messtab-msgv4 = ls_detail_return-message_v4 .
    PERFORM log_add USING 'MSG'.
  ELSE.
    LOOP AT lt_detail_return INTO ls_detail_return .
      messtab-msgtyp = ls_detail_return-type .
      messtab-msgid = ls_detail_return-id .
      messtab-msgnr = ls_detail_return-number .
      messtab-msgv1 = ls_detail_return-message_v1 .
      messtab-msgv2 = ls_detail_return-message_v2 .
      messtab-msgv3 = ls_detail_return-message_v3 .
      messtab-msgv4 = ls_detail_return-message_v4 .
      PERFORM log_add USING 'MSG'.
    ENDLOOP.
  ENDIF.
ENDFORM.