FUNCTION-POOL zscnn_invoice_fg.             "MESSAGE-ID ..

* INCLUDE LZSCNN_INVOICE_FGD...              " Local class definition

CONSTANTS c_auglv TYPE  auglv VALUE 'UMBUCHNG' .
CONSTANTS c_tcode TYPE sy-tcode VALUE 'FB05' .
CONSTANTS c_doc_type TYPE blart VALUE 'KR' .
CONSTANTS c_structure_output TYPE tabname VALUE 'ZSCNN_INVOICE'  .
CONSTANTS green_status TYPE char4 VALUE '@08@' .
CONSTANTS yellow_status TYPE char4 VALUE '@09@' .
CONSTANTS red_status TYPE char4 VALUE '@0A@' .
CONSTANTS c_chart_of_account TYPE ktopl VALUE 'ZDAG' .
CONSTANTS c_transaction_key TYPE ktosl VALUE 'VST' .
CONSTANTS c_mode_n  TYPE allgazmd VALUE 'N' .
CONSTANTS c_mode_a  TYPE allgazmd VALUE 'A' .
CONSTANTS c_function TYPE funct_pi VALUE 'C' .
CONSTANTS c_vendor_account_type TYPE koart VALUE 'K' .
CONSTANTS c_company_code TYPE bukrs VALUE '0212' .
CONSTANTS c_special_gl TYPE agums VALUE 'Q' .
CONSTANTS c_curr_cny TYPE waers VALUE 'CNY' .
CONSTANTS c_debit_entry TYPE bschl VALUE '40' .
CONSTANTS c_credit_entry TYPE bschl VALUE '50' .
CONSTANTS c_invoice TYPE bschl VALUE '31' .
CONSTANTS c_special_gl_credit TYPE bschl VALUE '39' .
CONSTANTS c_invoice_param TYPE memoryid VALUE 'Z_INVOICE_SCREEN' .

DATA ok_code TYPE sy-ucomm .
TABLES: lfa1, bkpf , bseg , rf05a .

CLASS lcl_application DEFINITION DEFERRED.

DATA lo_application TYPE REF TO lcl_application .

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'MYTABSTRIP'
CONSTANTS: BEGIN OF c_mytabstrip,
             tab1 LIKE sy-ucomm VALUE 'MYTABSTRIP_FC1',
             tab2 LIKE sy-ucomm VALUE 'MYTABSTRIP_FC2',
           END OF c_mytabstrip.
*&SPWIZARD: DATA FOR TABSTRIP 'MYTABSTRIP'

CONTROLS:  mytabstrip TYPE TABSTRIP.
DATA:      BEGIN OF g_mytabstrip,
             subscreen   LIKE sy-dynnr,
             prog        LIKE sy-repid VALUE 'SAPLZSCNN_INVOICE_FG',
             pressed_tab LIKE sy-ucomm VALUE c_mytabstrip-tab1,
           END OF g_mytabstrip.
