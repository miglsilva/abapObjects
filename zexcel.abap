**&---------------------------------------------------------------------*
**& Report  Excel
**&
**&---------------------------------------------------------------------*
**&
**&
**&---------------------------------------------------------------------*
*
report ZEXCEL.


*** CSV READ
types:
  tv_data(256) type c,

  begin of ts_data,
    value_0001 type tv_data,
    value_0002 type tv_data,
    value_0003 type tv_data,
    value_0004 type tv_data,
    value_0005 type tv_data,
    value_0006 type tv_data,
    value_0007 type tv_data,
    value_0008 type tv_data,
    value_0009 type tv_data,
    value_0010 type tv_data,
    value_0011 type tv_data,
    value_0012 type tv_data,
    value_0013 type tv_data,
    value_0014 type tv_data,
    value_0015 type tv_data,
    value_0016 type tv_data,
    value_0017 type tv_data,
    value_0018 type tv_data,
    value_0019 type tv_data,
    value_0020 type tv_data,
  end of ts_data,

  tt_data     type table of ts_data,
  tv_index(4) type n.

constants:
  co_max_col type i value 20,
  co_max_row type i value 9999.

data: lv_workdir        type string .

*&---------------------------------------------------------------------*
*&      SELECTION-SCREEN
*&---------------------------------------------------------------------*
selection-screen begin of line.
selection-screen comment (33) tx_fname for field pa_fname.
parameters pa_fname type localfile obligatory lower case memory id fnm default 'C:\TEMP\2017-2-9 DTC.XLSX'.
selection-screen end of line.

selection-screen skip.

selection-screen begin of line.
selection-screen comment (83) tx_read0.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (53) tx_read1 for field pa_read1.
parameters pa_read1 radiobutton group rad1.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (53) tx_read2 for field pa_read2.
parameters pa_read2 radiobutton group rad1.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (53) tx_read3 for field pa_read3.
parameters pa_read3 radiobutton group rad1.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (53) tx_read4 for field pa_read4.
parameters pa_read4 radiobutton group rad1.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (53) tx_read5 for field pa_read5.
parameters pa_read5 radiobutton group rad1.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (53) tx_read6 for field pa_read6.
parameters pa_read6 radiobutton group rad1.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (53) tx_read8 for field pa_read8.
parameters pa_read8 radiobutton group rad1.
selection-screen end of line.

selection-screen skip.

selection-screen begin of line.
selection-screen comment (53) tx_read7 for field pa_read7.
parameters pa_read7 radiobutton group rad1.
selection-screen end of line.




at selection-screen on value-request for pa_fname .
  lv_workdir = pa_fname .
  cl_gui_frontend_services=>directory_browse( exporting initial_folder  = lv_workdir
                                              changing  selected_folder = lv_workdir ).
  pa_fname = lv_workdir.

at selection-screen output.
  tx_fname = 'Path and name of Excel to open'.
  tx_read0 = 'Function:'.
  tx_read1 = 'ALSM_EXCEL_TO_INTERNAL_TABLE (max nº of lines 9999)'.
  tx_read2 = 'FILE_READ_AND_CONVERT_SAP_DATA'.
  tx_read3 = 'IMPORT_FROM_SPREADSHEET'.
  tx_read4 = 'KCD_EXCEL_OLE_TO_INT_CONVERT'.
  tx_read5 = 'TEXT_CONVERT_XLS_TO_SAP'.
  tx_read6 = 'UPLOAD_XLS_FILE_2_ITAB'.
  tx_read7 = 'OBJECT ORIENTED'.
  tx_read8 = 'ABAP2XLSX'.

*&---------------------------------------------------------------------*
*&      START-OF-SELECTION
*&---------------------------------------------------------------------*
start-of-selection.

  data:
    st      type i,
    ft      type i,
    tt      type i,
    lt_data type tt_data.


  get run time field st.



  if pa_read1 = 'X'.
    write: / 'Function ALSM_EXCEL_TO_INTERNAL_TABLE'.
    skip.
    perform excel_read1 changing lt_data.

  elseif pa_read2 = 'X'.
    write: / 'Function FILE_READ_AND_CONVERT_SAP_DATA'.
    skip.
    perform excel_read2 changing lt_data.

  elseif pa_read3 = 'X'.
    write: / 'Function IMPORT_FROM_SPREADSHEET'.
    skip.
    perform excel_read3 changing lt_data.

  elseif pa_read4 = 'X'.
    write: / 'Function KCD_EXCEL_OLE_TO_INT_CONVERT'.
    skip.
    perform excel_read4 changing lt_data.

  elseif pa_read5 = 'X'.
    write: / 'Function TEXT_CONVERT_XLS_TO_SAP'.
    skip.
    perform excel_read5 changing lt_data.

  elseif pa_read6 = 'X'.
    write: / 'Function UPLOAD_XLS_FILE_2_ITAB'.
    skip.
    perform excel_read6 changing lt_data.

  elseif pa_read7 = 'X'.
    write: / 'OBJECT ORIENTED'.
    skip.
    perform excel_read7 changing lt_data.

  elseif pa_read8 = 'X'.
    write: / 'ABAP2XLSX'.
    skip.
    perform excel_read8 changing lt_data.
  endif.

  get run time field ft.

  tt = ft - st.

  write: / 'Runtime', tt.

  perform itab_display using lt_data.

*&---------------------------------------------------------------------*
*&      Form  excel_read1
*&---------------------------------------------------------------------*
form excel_read1 changing pt_data type tt_data.

  data:
    lt_excel type standard table of alsmex_tabline,
    ls_excel type alsmex_tabline,
    lv_data  type tv_data,
    lv_error type string.

  call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    exporting
      filename                = pa_fname
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = co_max_col
      i_end_row               = co_max_row
    tables
      intern                  = lt_excel
    exceptions
      inconsistent_parameters = 1
      upload_ole              = 2
      others                  = 3.

  if sy-subrc <> 0.
    write: / 'SY-SUBRC = ', sy-subrc.
    message id sy-msgid type 'E' number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into lv_error.
    write: / lv_error.
    stop.
  endif.

  loop at lt_excel into ls_excel.
    lv_data = ls_excel-value.
    perform itab_insert_value using ls_excel-col ls_excel-row lv_data
                              changing pt_data.
  endloop.
endform.                    "excel_read1

*&---------------------------------------------------------------------*
*&      Form  excel_read2
*&---------------------------------------------------------------------*
form excel_read2 changing pt_data type tt_data.

  data:
    lv_fname type filename-fileintern,
    lv_error type string.

  lv_fname = pa_fname.
  call function 'FILE_READ_AND_CONVERT_SAP_DATA'
    exporting
      i_filename           = lv_fname
      i_servertyp          = 'OLE2'
      i_fileformat         = 'XLS'
*     I_FIELD_SEPERATOR    =
*     I_LINE_HEADER        =
    tables
      i_tab_receiver       = pt_data
    exceptions
      file_not_found       = 1
      close_failed         = 2
      authorization_failed = 3
      open_failed          = 4
      conversion_failed    = 5
      others               = 6.

  if sy-subrc <> 0.
    write: / 'SY-SUBRC = ', sy-subrc.
    message id sy-msgid type 'E' number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into lv_error.
    write: / lv_error.
    stop.
  endif.
endform.                    "excel_read2

*&---------------------------------------------------------------------*
*&      Form  excel_read3
*&---------------------------------------------------------------------*
form excel_read3 changing pt_data type tt_data.

  data:
    lv_url(256) type c,
    ls_range    type soi_dimension_item,
    lt_range    type soi_dimension_table,
    lt_excel    type soi_generic_table,
    ls_excel    type soi_generic_item,
    lv_data     type tv_data,
    lv_col      type tv_index,
    lv_row      type tv_index.

  concatenate 'file://' pa_fname into lv_url.

  ls_range-row     = 1.
  ls_range-column  = 1.
  ls_range-rows    = co_max_row.
  ls_range-columns = co_max_col.
  append ls_range to lt_range.

  call function 'IMPORT_FROM_SPREADSHEET'
    exporting
      item_url      = lv_url
      document_type = 'Excel.Sheet'
    tables
      data_table    = lt_excel
      ranges        = lt_range.

  loop at lt_excel into ls_excel.
    lv_data = ls_excel-value.
    lv_col = ls_excel-column.
    lv_row = ls_excel-row.
    perform itab_insert_value using lv_col lv_row lv_data
                              changing pt_data.
  endloop.
endform.                    "excel_read3

*&---------------------------------------------------------------------*
*&      Form  excel_read4
*&---------------------------------------------------------------------*
form excel_read4 changing pt_data type tt_data.

  data:
    lt_excel type kcde_intern,
    ls_excel type kcde_intern_struc,
    lv_data  type tv_data,
    lv_error type string.

  call function 'KCD_EXCEL_OLE_TO_INT_CONVERT'
    exporting
      filename                = pa_fname
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = co_max_col
      i_end_row               = co_max_row
    tables
      intern                  = lt_excel
    exceptions
      inconsistent_parameters = 1
      upload_ole              = 2
      others                  = 3.

  if sy-subrc <> 0.
    write: / 'SY-SUBRC = ', sy-subrc.
    message id sy-msgid type 'E' number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into lv_error.
    write: / lv_error.
    stop.
  endif.

  loop at lt_excel into ls_excel.
    lv_data = ls_excel-value.
    perform itab_insert_value using ls_excel-col ls_excel-row lv_data
                              changing pt_data.
  endloop.
endform.                    "excel_read4

*&---------------------------------------------------------------------*
*&      Form  excel_read5
*&---------------------------------------------------------------------*
form excel_read5 changing pt_data type tt_data.

  data:
    lt_raw_data(4096) type c occurs 0,
    lt_data           type standard table of string,
    lv_error          type string.

  call function 'TEXT_CONVERT_XLS_TO_SAP'
    exporting
*     I_FIELD_SEPERATOR    =
*     I_LINE_HEADER        =
      i_tab_raw_data       = lt_raw_data
      i_filename           = pa_fname
    tables
      i_tab_converted_data = pt_data
    exceptions
      conversion_failed    = 1
      others               = 2.

  if sy-subrc <> 0.
    write: / 'SY-SUBRC = ', sy-subrc.
    message id sy-msgid type 'E' number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into lv_error.
    write: / lv_error.
    stop.
  endif.
endform.                    "excel_read5

*&---------------------------------------------------------------------*
*&      Form  excel_read6
*&---------------------------------------------------------------------*
form excel_read6 changing pt_data type tt_data.

  data:
  lv_error          type string.

  call function 'UPLOAD_XLS_FILE_2_ITAB'
    exporting
      i_filename = pa_fname
    tables
      e_itab     = pt_data
    exceptions
      file_error = 1
      others     = 2.

  if sy-subrc <> 0.
    write: / 'SY-SUBRC = ', sy-subrc.
    message id sy-msgid type 'E' number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into lv_error.
    write: / lv_error.
    stop.
  endif.
endform.                    "excel_read6

*&---------------------------------------------------------------------*
*&      Form  excel_read7
*&---------------------------------------------------------------------*
form excel_read7 changing pt_data type tt_data.

  data:
    lo_control     type ref to i_oi_container_control,
    lo_error       type ref to i_oi_error,
    lo_container   type ref to cl_gui_custom_container,
    lo_document    type ref to i_oi_document_proxy,
    lo_spreadsheet type ref to i_oi_spreadsheet,
    lv_url         type char256,
    lv_has_sheet   type i.

* 1. control get
  call method c_oi_container_control_creator=>get_container_control
    importing
      control = lo_control
      error   = lo_error.

  if lo_error->has_failed = 'X'.
    call method lo_error->raise_message( type = 'E' ).
  endif.

* 2. container create
  create object lo_container
    exporting
*     parent                      =
      container_name              = 'CONTAINER'
*     style                       =
*     lifetime                    = lifetime_default
*     repid                       =
*     dynnr                       =
*     no_autodef_progid_dynnr     =
    exceptions
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      others                      = 6.

  assert sy-subrc = 0.

* 3. control init with container
  call method lo_control->init_control
    exporting
*     dynpro_nr                = SY-DYNNR
*     gui_container            = ' '
      inplace_enabled          = 'X'
*     inplace_mode             = 0
*     inplace_resize_documents = ' '
      inplace_scroll_documents = 'X'
*     inplace_show_toolbars    = 'X'
*     no_flush                 = ' '
*     parent_id                = cl_gui_cfw=>dynpro_0
      r3_application_name      = 'Excel'
*     register_on_close_event  = ' '
*     register_on_custom_event = ' '
*     rep_id                   = SY-REPID
*     shell_style              = 1384185856
      parent                   = lo_container
*     name                     =
*     autoalign                = 'x'
    importing
      error                    = lo_error
    exceptions
      javabeannotsupported     = 1
      others                   = 2.

  assert sy-subrc = 0.

  if lo_error->has_failed = 'X'.
    call method lo_error->raise_message( type = 'E' ).
  endif.

* 4. control get proxy
  call method lo_control->get_document_proxy
    exporting
*     document_format    = 'NATIVE'
      document_type  = soi_doctype_excel_sheet " 'Excel.Sheet'
*     no_flush       = ' '
*     register_container = ' '
    importing
      document_proxy = lo_document
      error          = lo_error.

  if lo_error->has_failed = 'X'.
    call method lo_error->raise_message( type = 'E' ).
  endif.

* 5. document open
  concatenate 'FILE://' pa_fname into lv_url.

  call method lo_document->open_document
    exporting
*     document_title   = ' '
      document_url  = lv_url
*     no_flush      = ' '
      open_inplace  = 'X'
      open_readonly = 'X'
*     protect_document = ' '
*     onsave_macro  = ' '
*     startup_macro = ''
*     user_info     =
    importing
      error         = lo_error.

  if lo_error->has_failed = 'X'.
    call method lo_error->raise_message( type = 'E' ).
  endif.

* 6. spreadsheet check exists
  call method lo_document->has_spreadsheet_interface
*   EXPORTING
*     no_flush     = ' '
    importing
      error        = lo_error
      is_available = lv_has_sheet.

  if lo_error->has_failed = 'X'.
    call method lo_error->raise_message( type = 'E' ).
  endif.

  if not lv_has_sheet is initial.

*   7. spreadsheet get
    call method lo_document->get_spreadsheet_interface
      exporting
        no_flush        = ' '
      importing
        error           = lo_error
        sheet_interface = lo_spreadsheet.

    if lo_error->has_failed = 'X'.
      call method lo_error->raise_message( type = 'E' ).
    endif.

*   8. data read
    data:
      lv_row_start type i value 1,   " first row
      lv_row_block type i value 100, " number of rows read in one block (range)
      lv_row_empty type i value 0,   " count of empty rows at the end of block
      lt_value     type soi_generic_table,
      ls_value     type soi_generic_item,
      lt_rangesdef type soi_dimension_table,
      ls_rangesdef type soi_dimension_item,
      lt_ranges    type soi_range_list,
      ls_data      type ts_data,
      lv_col_index type tv_index,
      lv_fieldname type string.

    field-symbols:
    <fs_value> type tv_data.

    while lv_row_empty < 5. " max empty rows

*     range create
      clear ls_rangesdef.
      clear lt_rangesdef.
      ls_rangesdef-row     = lv_row_start.
      ls_rangesdef-column  = 1.
      ls_rangesdef-rows    = lv_row_block.
      ls_rangesdef-columns = co_max_col.
      insert ls_rangesdef into table lt_rangesdef.

*     data read from range
      call method lo_spreadsheet->get_ranges_data
        exporting
*         no_flush  = ' '
*         all       = ' '
*         updating  = -1
          rangesdef = lt_rangesdef
        importing
          contents  = lt_value
          error     = lo_error
        changing
          ranges    = lt_ranges.

      if lo_error->has_failed = 'X'.
        call method lo_error->raise_message( type = 'E' ).
      endif.

*     data takeover
      loop at lt_value into ls_value.

        at new row.
          clear ls_data.
        endat.

        lv_col_index = ls_value-column.
        concatenate 'VALUE_' lv_col_index into lv_fieldname.
        assign component lv_fieldname of structure ls_data to <fs_value>.
        assert sy-subrc = 0.
        <fs_value> = ls_value-value.

        at end of row.
          append ls_data to pt_data.

          if ls_data is initial.
            add 1 to lv_row_empty.
          else.
            clear lv_row_empty.
          endif.
        endat.
      endloop.

      add lv_row_block to lv_row_start.
    endwhile.

*   data delete empty rows at the end
    data:
    lv_index type sy-tabix.

    lv_index = lines( pt_data ).

    while lv_index > 0.
      read table pt_data into ls_data index lv_index.
      if ls_data is initial.
        delete pt_data index lv_index.
      else.
        exit.
      endif.
      subtract 1 from lv_index.
    endwhile.
  else.
    message e323(bf00) with pa_fname raising file_error.
  endif.

* 9. final cleaning
  if not lo_spreadsheet is initial.
    free lo_spreadsheet.
  endif.

  if not lo_document is initial.
    call method lo_document->close_document.
    call method lo_document->release_document.
    free lo_document.
  endif.

  if not lo_control is initial.
    call method lo_control->destroy_control.
    free lo_control.
  endif.
endform.                    "excel_read7

form excel_read8 changing pt_data type tt_data.
  data: excel           type ref to zcl_excel,
        lo_excel_writer type ref to zif_excel_writer,
        reader          type ref to zif_excel_reader,
        worksheet       type ref to zcl_excel_worksheet.

  data: lv_data  type tv_data,
        ls_excel type zexcel_s_cell_data.

  data: ex  type ref to zcx_excel,
        msg type string.

  try .
      create object reader type zcl_excel_reader_2007.
      excel = reader->load_file( pa_fname ).

      worksheet = excel->get_active_worksheet( ).

      loop at worksheet->sheet_content into ls_excel .
        lv_data = ls_excel-cell_value.
        perform itab_insert_value_v2 using ls_excel-cell_column ls_excel-cell_row lv_data
                            changing pt_data.

      endloop .

    catch zcx_excel into ex.    " Exceptions for ABAP2XLSX
      msg = ex->get_text( ).
      write: / msg.
  endtry.

endform.

form itab_insert_value_v2 using pi_col   type zexcel_cell_column
                                pi_row   type zexcel_cell_row
                                pi_value type tv_data
                       changing pt_data  type tt_data.

  data:
    lv_fieldname type string,
    lv_col(4)    type n.

  field-symbols:
    <fs_data>  type ts_data,
    <fs_value> type tv_data.

  check pi_value <> ''.

  while pi_row > lines( pt_data ).
    insert initial line into table pt_data.
  endwhile.

  read table pt_data assigning <fs_data> index pi_row.
  assert sy-subrc = 0.
  lv_col = pi_col .
  concatenate 'VALUE_' lv_col into lv_fieldname.
  assign component lv_fieldname of structure <fs_data> to <fs_value>.
  assert sy-subrc = 0.
  <fs_value> = pi_value.
endform.
*&---------------------------------------------------------------------*
*&      Form  itab_insert_value
*&---------------------------------------------------------------------*
form itab_insert_value using    pi_col   type tv_index
                                pi_row   type tv_index
                                pi_value type tv_data
                       changing pt_data  type tt_data.

  data:
  lv_fieldname type string.

  field-symbols:
    <fs_data>  type ts_data,
    <fs_value> type tv_data.

  check pi_value <> ''.

  while pi_row > lines( pt_data ).
    insert initial line into table pt_data.
  endwhile.

  read table pt_data assigning <fs_data> index pi_row.
  assert sy-subrc = 0.
  concatenate 'VALUE_' pi_col into lv_fieldname.
  assign component lv_fieldname of structure <fs_data> to <fs_value>.
  assert sy-subrc = 0.
  <fs_value> = pi_value.
endform.                    "itab_insert_value

*&---------------------------------------------------------------------*
*&      Form  itab_display
*&---------------------------------------------------------------------*
form itab_display using pt_data  type tt_data.

  data:
    lv_count type i,
    ls_data  type ts_data,
    lv_value type tv_data.

  lv_count = lines( pt_data ).

  write: / 'Number of lines', lv_count.
  skip.

  loop at pt_data into ls_data.

    write / ''. " sy-tabix.
    do co_max_col times varying lv_value from ls_data-value_0001
                                         next ls_data-value_0002.
      write: lv_value(10).
    enddo.
  endloop.
endform.                    "itab_display