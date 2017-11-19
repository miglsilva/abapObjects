*----------------------------------------------------------------------*
* OBJECT NAME           : ZDSD_MAGENTO_EXP_TOP
* PROGRAM TITLE         : Selection Screen
* AUTHOR                : Andrei Gidilica
* DATE                  : 07/11/2017
* RICEFW OBJECT         :
* CHANGE REQUEST NUMBER :
* DESCRIPTION           : Magento Exports
*----------------------------------------------------------------------*
* MOD. NO.| DATE | USER | CHANGE REFERENCE *
*----------------------------------------------------------------------*

REPORT zdsd_magento_exp.

INCLUDE zdsd_magento_exp_c01.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_matnr FOR lcl_appl=>ms_mara-matnr,
                s_mtart FOR lcl_appl=>ms_mara-mtart,
                s_werks FOR lcl_appl=>ms_marc-werks,
                s_date  FOR lcl_appl=>ms_mara-ersda NO INTERVALS
                                                    NO-EXTENSION.
PARAMETERS: p_pltyp TYPE pltyp.

SELECTION-SCREEN SKIP 1.
PARAMETERS: p_ftpe TYPE c RADIOBUTTON GROUP file USER-COMMAND upl
                                                 DEFAULT 'X',
            p_lcle TYPE c RADIOBUTTON GROUP file.
PARAMETERS: p_file TYPE localfile.
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_show AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.