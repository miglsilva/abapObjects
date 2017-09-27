*----------------------------------------------------------------------*
***INCLUDE LZSCNN_INVOICE_FGO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  INIT_CONTROLS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_controls OUTPUT.

  SET PF-STATUS 'STATUS_2000' .
  SET TITLEBAR 'GUI_2000' .
  IF lo_application IS BOUND .
    lo_application->init_controls( ) .
  ENDIF.
ENDMODULE.                 " INIT_CONTROLS  OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TS 'MYTABSTRIP'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE mytabstrip_active_tab_set OUTPUT.
  mytabstrip-activetab = g_mytabstrip-pressed_tab.
  CASE g_mytabstrip-pressed_tab.
    WHEN c_mytabstrip-tab1.
      g_mytabstrip-subscreen = '0100'.
    WHEN c_mytabstrip-tab2.
      g_mytabstrip-subscreen = '0200'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "MYTABSTRIP_ACTIVE_TAB_SET OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_0100 OUTPUT.

*  bkpf-bldat = sy-datum .
*  bkpf-budat = sy-datum .

  IF bkpf-bldat IS INITIAL .
    bkpf-bldat = sy-datum .
  ENDIF .

  IF bkpf-budat IS INITIAL .
    bkpf-budat = sy-datum .
  ENDIF .

*  rf05a-ampel =  green_status .
ENDMODULE.                 " INIT_0100  OUTPUT
