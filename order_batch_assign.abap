FORM order_batch_assign USING i_rc .
  FIELD-SYMBOLS <fs_mtab> TYPE ty_mtab .
  DATA lt_component TYPE STANDARD TABLE OF bapi_order_component .
  DATA lt_methods   TYPE STANDARD TABLE OF bapi_alm_order_method .
  DATA lt_comp      TYPE STANDARD TABLE OF bapi_alm_order_component .
  DATA lt_comp_x    TYPE bapi_alm_order_component_ut .
  DATA lt_return    TYPE STANDARD TABLE OF bapiret2 .
  DATA ls_objects   TYPE bapi_pp_order_objects .
  DATA ls_methods   TYPE bapi_alm_order_method .
  DATA ls_component TYPE bapi_order_component .
  DATA ls_return    TYPE bapiret2 .
  DATA ls_comp      TYPE bapi_alm_order_component .
  DATA ls_comp_x    TYPE bapi_alm_order_component_up  .
  DATA ls_mseg      TYPE mseg.

  ls_objects-components = 'X'.
  CALL FUNCTION 'BAPI_PRODORD_GET_DETAIL'
    EXPORTING
      number        = ea-aufnr
      order_objects = ls_objects
    IMPORTING
      return        = ls_return
    TABLES
      component     = lt_component.

  READ TABLE lt_component INTO ls_component WITH KEY  material = mchb-matnr .

  IF sy-subrc = 0.
    ls_methods-refnumber = '000001'.
    ls_methods-objecttype = 'COMPONENT'.
    ls_methods-method = 'CHANGE'.
    ls_methods-objectkey = ea-aufnr .
    APPEND ls_methods TO lt_methods.

    ls_methods-refnumber = '000001'.
    ls_methods-objecttype = ''.
    ls_methods-method = 'SAVE'.
    ls_methods-objectkey = ea-aufnr .
    APPEND ls_methods TO lt_methods.

    ls_comp-reserv_no = ls_component-reservation_number .
    ls_comp-res_item = ls_component-reservation_item .
    ls_comp-res_type = ls_component-reservation_type .
    ls_comp-item_number = ls_component-item_number.
    ls_comp-material = ls_component-material.
    ls_comp-plant = ls_component-prod_plant.
    ls_comp-stge_loc = ls_component-storage_location.
    ls_comp-item_cat = ls_component-item_category.
    ls_comp-activity = ls_component-operation.
    ls_comp-original_quantity = ls_component-entry_quantity .
    ls_comp-requirement_quantity = ls_component-entry_quantity .
    ls_comp-requirement_quantity_unit = ls_component-base_uom.

* Assign NEW BATCH
    LOOP AT gt_mtab ASSIGNING <fs_mtab> .
      LOOP AT imseg INTO ls_mseg WHERE charg NE <fs_mtab>-charg.
        ls_comp-batch = ls_mseg-charg.
        APPEND ls_comp TO lt_comp.
      ENDLOOP.
      <fs_mtab>-reserv_no = ls_component-reservation_number .
      <fs_mtab>-res_item = ls_component-reservation_item .
    ENDLOOP.

    ls_comp_x-item_number = 'X'.
    ls_comp_x-material = 'X'.
    ls_comp_x-plant = 'X'.
    ls_comp_x-stge_loc = 'X'.
    ls_comp_x-item_cat = 'X'.
*    ls_comp_x-activity = 'X'.
    ls_comp_x-original_quantity = 'X'.
    ls_comp_x-requirement_quantity = 'X'.
    ls_comp_x-requirement_quantity_unit = 'X'.
    ls_comp_x-batch = 'X'.
    APPEND ls_comp_x TO lt_comp_x.

    CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
      TABLES
        it_methods      = lt_methods
        it_component    = lt_comp
        it_component_up = lt_comp_x
        return          = lt_return.

    READ TABLE lt_return INTO ls_return WITH KEY id = 'IW' number = '080' .
    IF sy-subrc = 0 .

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      PERFORM fill_msgv USING '2' text-003
                                  ls_comp-batch ' ' ' ' .
      PERFORM log_add USING 'BATCH/ASSIGN/ORDER -'.

      messtab-msgtyp = ls_return-type .
      messtab-msgid = ls_return-id .
      messtab-msgnr = ls_return-number .
      messtab-msgv1 = ls_return-message_v1 .
      messtab-msgv2 = ls_return-message_v2 .
      messtab-msgv3 = ls_return-message_v3 .
      messtab-msgv4 = ls_return-message_v4 .
      PERFORM log_add USING 'MSG'.

      PERFORM update_zdis .

      i_rc = 0 .
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
      i_rc = 4 .
    ENDIF.
  ENDIF.
ENDFORM.