FORM delivery_batch_assign .
  DATA lt_item_data     TYPE STANDARD TABLE OF bapiobdlvitemchg .
  DATA lt_item_control  TYPE STANDARD TABLE OF bapiobdlvitemctrlchg .
  DATA lt_return        TYPE STANDARD TABLE OF bapiret2 .
  DATA ls_item_control  TYPE bapiobdlvitemctrlchg .
  DATA ls_header_control TYPE bapiobdlvhdrctrlchg .
  DATA ls_item_data     TYPE bapiobdlvitemchg .
  DATA ls_header_data   TYPE bapiobdlvhdrchg .
  DATA ls_mseg          TYPE mseg .
  DATA ls_return        TYPE bapiret2 .

  IF NOT ea-vbeln IS INITIAL AND
     NOT ea-posnr IS INITIAL .

    ls_header_data-deliv_numb = ea-vbeln .
    ls_header_control-deliv_numb = ea-vbeln .

    ls_item_data-deliv_numb = ea-vbeln .
    ls_item_data-deliv_item = ea-posnr.

* Assign NEW BATCH
    LOOP AT gt_mtab INTO gs_mtab .
      ls_item_data-material = gs_mtab-matnr .
      LOOP AT imseg INTO ls_mseg WHERE charg NE gs_mtab-charg.
        ls_item_data-batch = ls_mseg-charg.
      ENDLOOP.
    ENDLOOP.

    APPEND ls_item_data TO lt_item_data.

    ls_item_control-deliv_numb = ea-vbeln .
    ls_item_control-deliv_item = ea-posnr.
    ls_item_control-chg_delqty = 'X'.
    APPEND ls_item_control TO lt_item_control.

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
      EXPORTING
        header_data    = ls_header_data
        header_control = ls_header_control
        delivery       = ea-vbeln
      TABLES
        item_data      = lt_item_data
        item_control   = lt_item_control
        return         = lt_return.

    READ TABLE lt_return INTO ls_return WITH KEY id = 'VL' number = '311' .
    IF sy-subrc = 0 .

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      messtab-msgtyp = ls_return-type .
      messtab-msgid = ls_return-id .
      messtab-msgnr = ls_return-number .
      messtab-msgv1 = ls_return-message_v1 .
      messtab-msgv2 = ls_return-message_v2 .
      messtab-msgv3 = ls_return-message_v3 .
      messtab-msgv4 = ls_return-message_v4 .
      PERFORM log_add USING 'MSG'.
      PERFORM fill_msgv USING '2' text-003
                                  ls_item_data-batch ' ' ' ' .
      PERFORM log_add USING 'BATCH/ASSIGN/DELIVERY -'.

      PERFORM update_zdis .
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
  ENDIF.
ENDFORM.