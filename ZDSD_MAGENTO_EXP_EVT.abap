*----------------------------------------------------------------------*
* OBJECT NAME           : ZDSD_MAGENTO_EXP_EVT
* PROGRAM TITLE         : Program Events
* AUTHOR                : Andrei Gidilica
* DATE                  : 07/11/2017
* RICEFW OBJECT         :
* CHANGE REQUEST NUMBER :
* DESCRIPTION           : Magento Exports
*----------------------------------------------------------------------*
* MOD. NO.| DATE | USER | CHANGE REFERENCE *
*----------------------------------------------------------------------*
INITIALIZATION.
  lcl_appl=>init(
    IMPORTING
      ev_pltyp = p_pltyp
      er_date  = s_date[]
      er_plant = s_werks[]
      er_mtart = s_mtart[] ).

AT SELECTION-SCREEN OUTPUT.
  lcl_appl=>scr_out( p_ftpe ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_appl=>save_dialog(
    IMPORTING
      ev_file = p_file ).

AT SELECTION-SCREEN.
  lcl_appl=>check(
    EXPORTING
      iv_ucomm = sy-ucomm
      iv_pltyp = p_pltyp
      ir_date  = s_date[]
      iv_file  = p_file
      iv_lcle  = p_lcle ).

START-OF-SELECTION.
  lcl_appl=>start(
    EXPORTING
      ir_matnr = s_matnr[]
      ir_mtart = s_mtart[]
      ir_werks = s_werks[]
      ir_date  = s_date[]
      iv_pltyp = p_pltyp
      iv_file  = p_file
      iv_lcle  = p_lcle
      iv_ftpe  = p_ftpe
      iv_show  = p_show ).
