*&---------------------------------------------------------------------*
*&  Include           LZSCNN_INVOICE_FGI01
*&---------------------------------------------------------------------*

*&SPWIZARD: INPUT MODULE FOR TS 'MYTABSTRIP'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE mytabstrip_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_mytabstrip-tab1.
      g_mytabstrip-pressed_tab = c_mytabstrip-tab1.
    WHEN c_mytabstrip-tab2.
      g_mytabstrip-pressed_tab = c_mytabstrip-tab2.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "MYTABSTRIP_ACTIVE_TAB_GET INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2000 INPUT.

  cl_gui_cfw=>flush( ) .

  CASE ok_code.
    WHEN 'ENTE' .
      lo_application->enter_action( ) .
    WHEN 'SICH' . "SAVE
      lo_application->save_action( ) .
    WHEN 'UPD_BAL' .
    WHEN 'BT_CLR' .
      lo_application->clear_screen( ) .
    WHEN 'CANCEL'.
      SET SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      SET SCREEN 0.

  ENDCASE .

  CLEAR ok_code .
  cl_gui_cfw=>flush( ) .
  cl_gui_cfw=>dispatch( ) .
ENDMODULE.                 " USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHANGE_VALUE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE change_value INPUT.

  IF bkpf-waers NE lo_application->waers_bukrs .
    lo_application->update_foreign_balance( ) .
  ELSE.
    lo_application->update_balance( ) .

  ENDIF .
ENDMODULE.                 " CHANGE_VALUE  INPUT

*----------------------------------------------------------------------*
*  MODULE change_column INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE change_column INPUT .
  DATA lv_ex_rate TYPE kursf .

  IF NOT bkpf-waers IS INITIAL .
    IF NOT lo_application->vendor IS INITIAL .

      SELECT SINGLE waers
        FROM t001
        INTO lo_application->waers_bukrs
        WHERE bukrs = lo_application->bukrs .

      IF lo_application->waers_bukrs NE bkpf-waers .
        lo_application->change_by_waers( abap_true ).


        lv_ex_rate =  lo_application->get_exchange_rate( f_curr = bkpf-waers
                                                         l_curr = c_curr_cny ) .

        IF bkpf-kursf IS INITIAL OR lo_application->waers NE bkpf-waers .

          bkpf-kursf = lv_ex_rate .
        ENDIF.
      ELSE.
        lo_application->refresh_initial_output( ) .
        lo_application->change_by_waers( ).

        CLEAR bkpf-kursf .
      ENDIF .
    ENDIF.
  ELSE.
    lo_application->refresh_initial_output( ) .
    lo_application->change_by_waers( ).

    SELECT SINGLE waers
      FROM t001
      INTO lo_application->waers_bukrs
      WHERE bukrs = lo_application->bukrs .

    bkpf-waers = lo_application->waers_bukrs.
  ENDIF.

  lo_application->waers = bkpf-waers .

ENDMODULE .                    "change_column INPUT

*----------------------------------------------------------------------*
*  MODULE change_rate
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE change_rate .
  FIELD-SYMBOLS <fs_data_output> TYPE zscnn_invoice.

  lv_ex_rate =  lo_application->get_exchange_rate( f_curr = bkpf-waers
                                                   l_curr = c_curr_cny ) .

  IF bkpf-kursf IS INITIAL OR lo_application->waers NE bkpf-waers .

    bkpf-kursf = lv_ex_rate .
  ENDIF.


  IF NOT bkpf-waers IS INITIAL .

    LOOP AT lo_application->lt_data_output ASSIGNING <fs_data_output>.

      IF NOT <fs_data_output>-amt_for_mod IS INITIAL .

        <fs_data_output>-amt_mod = lo_application->convert_amount(
                                                           f_curr = bkpf-waers
                                                           f_amount = <fs_data_output>-amt_for_mod
                                                           l_curr = c_curr_cny )  .
      ENDIF.

    ENDLOOP .
    lo_application->refresh_alv_grid( ) .
  ENDIF.

ENDMODULE .                    "change_rate
