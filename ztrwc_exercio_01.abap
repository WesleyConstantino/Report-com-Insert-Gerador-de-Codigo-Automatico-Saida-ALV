REPORT zestudos_1.

*&---------------------------------------------------------------------*
*                              Types                                   *
*&---------------------------------------------------------------------*
*------* ty_arquivo
TYPES:BEGIN OF ty_arquivo,
        linha(2000) TYPE c,
      END OF ty_arquivo,

*------* ty_out
      BEGIN OF ty_out,
       id            TYPE ztestudos_1-id,
       servidor      TYPE ztestudos_1-servidor,
       num_instancia TYPE ztestudos_1-num_instancia,
      END OF ty_out.

*&---------------------------------------------------------------------*
*                        Tabelas  Internas                             *
*&---------------------------------------------------------------------*
DATA: it_arquivo TYPE TABLE OF ty_arquivo,
      it_out     TYPE TABLE OF ztestudos_1.

*&---------------------------------------------------------------------*
*                            Workareas                                 *
*&---------------------------------------------------------------------*
DATA: wa_arquivo TYPE ty_arquivo,
      wa_out     LIKE LINE OF it_out.

*&---------------------------------------------------------------------*
*                            Variaveis                                 *
*&---------------------------------------------------------------------*
*------* VARIÁVEIS PARA O POPUP DE SELEÇÃO DE ARQUIVOS
DATA: it_files TYPE filetable,
      wa_files TYPE file_table,
      vg_rc    TYPE i.
*&---------------------------------------------------------------------*
*                         Tela de seleção                              *
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-000.
PARAMETERS: p_file(1024) TYPE c. "OBLIGATORY. "Caminho do Arquivo para Upload
SELECTION-SCREEN END OF BLOCK b0.

*Evento caminho de upload
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM zf_exibe_popup_caminho_upload.

*Início da execusão
START-OF-SELECTION.
  IF  p_file IS INITIAL.
    MESSAGE 'Insira o caminho do upload do arquivo!' TYPE 'S' DISPLAY LIKE 'W'.
  ELSE.
    PERFORM: zf_gui_upload,
             zf_split_upload.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBE_POPUP_CAMINHO_UPLOAD
*&---------------------------------------------------------------------*
FORM zf_exibe_popup_caminho_upload .

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    CHANGING
      file_table              = it_files
      rc                      = vg_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE it_files INTO wa_files INDEX 1.
    p_file = wa_files-filename.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*                          zf_gui_upload                               *
*&---------------------------------------------------------------------*
FORM  zf_gui_upload.

  DATA vl_file  TYPE string.

  vl_file = p_file.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = vl_file
      filetype                = 'ASC'
    TABLES
      data_tab                = it_arquivo
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  IF sy-subrc <> 0.
    MESSAGE 'Erro no upload!' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  zf_split_upload
*&---------------------------------------------------------------------*
FORM zf_split_upload.
  DATA: vl_netpr(11) TYPE c.

  IF it_arquivo[] IS NOT INITIAL.
    LOOP AT it_arquivo INTO wa_arquivo.
*--* Início *--* Ignora a primeira linha do arquivo CSV do upload.
      IF sy-tabix EQ '1'.
        CONTINUE.
      ENDIF.
*--* Fim *--*

* Validar formato do arquivo
    IF strlen( wa_arquivo-linha ) NE 11.
      MESSAGE s398(00) WITH 'Formato do Aquivo Inválido!' DISPLAY LIKE 'E'.
      EXIT.
    ELSE.
       wa_out-servidor       = wa_arquivo-linha+0(6). "Faço o split partindo da posição 44 da linha até a posição 45.
       wa_out-num_instancia  = wa_arquivo-linha+6(2).
       wa_out-id             = wa_arquivo-linha+8(3).

      APPEND wa_out TO it_out.
      CLEAR  wa_out.
    ENDIF.
    ENDLOOP.

    LOOP AT it_out INTO wa_out.
    "Update
      INSERT ztestudos_1 FROM wa_out.
      CLEAR  wa_out.
    "Commit
     IF sy-subrc IS INITIAL.
      COMMIT WORK AND WAIT. "COMMIT WORK AND WAIT dá commit no banco de dados

      MESSAGE s208(00) WITH 'SALVO COM SUCESSO!'.
    ELSE.
      ROLLBACK WORK. "ROLLBACK WORK desfaz tudo o que aconteceu na operação
      MESSAGE s208(00) WITH  'ERRO AO GRAVAR!' DISPLAY LIKE 'E'.
    ENDIF.
   ENDLOOP.

  ENDIF.
ENDFORM.
