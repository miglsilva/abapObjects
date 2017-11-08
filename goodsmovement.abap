ORM goodmovements_pa .

  DATA lt_item TYPE STANDARD TABLE OF bapi2017_gm_item_create .
  DATA lt_return TYPE STANDARD TABLE OF bapiret2 .
  DATA ls_header TYPE bapi2017_gm_head_01 .
  DATA ls_code TYPE bapi2017_gm_code .
  DATA ls_item TYPE bapi2017_gm_item_create .
  DATA ls_mseg TYPE mseg .
  DATA ls_return TYPE bapiret2 .
  DATA lv_materialdocument TYPE mblnr .

  ls_header-pstng_date = sy-datum .
  ls_header-doc_date = sy-datum .

  ls_code-gm_code = '06' .

  LOOP AT gt_mtab INTO gs_mtab .
    ls_item-material = gs_mtab-matnr .
    ls_item-plant = gs_mtab-werks .
    ls_item-stge_loc = gs_mtab-lgort .
    ls_item-entry_qnt = gs_mtab-bdmng .
    LOOP AT imseg INTO ls_mseg WHERE charg NE gs_mtab-charg.
      ls_item-batch = ls_mseg-charg.
    ENDLOOP.
  ENDLOOP.

  ls_item-move_type = '261' .
  ls_item-orderid = ea-aufnr .
  ls_item-profit_ctr = '101PC01' .
  APPEND ls_item TO lt_item .

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = ls_header
      goodsmvt_code    = ls_code
    IMPORTING
      materialdocument = lv_materialdocument
    TABLES
      goodsmvt_item    = lt_item
      return           = lt_return.

  IF NOT lv_materialdocument IS INITIAL .

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    PERFORM fill_msgv USING '2' text-003
                                gs_mtab-charg
                                gs_mtab-zzrollnr ' '.
    PERFORM fill_msgv USING '3' text-004 xmenge ' ' ' '.
    PERFORM fill_msgv USING '4' text-005 ea-pck_rolle_neu ' ' ' '.
    PERFORM log_add USING 'MB11/261 -'.
  ELSE.
    LOOP AT lt_return INTO ls_return .
      messtab-msgtyp = ls_return-type .
      messtab-msgid = ls_return-id .
      messtab-msgnr = ls_return-number .
      messtab-msgv1 = ls_return-message_v1 .
      messtab-msgv2 = ls_return-message_v2 .
      messtab-msgv3 = ls_return-message_v3 .
      messtab-msgv4 = ls_return-message_v4 .
      PERFORM log_add USING 'MSG'.
    ENDLOOP.
  ENDIF.

ENDFORM .