REPORT ztrwc_exercio_01.

*&---------------------------------------------------------------------*
*                            Tabelas                                   *
*&---------------------------------------------------------------------*
TABLES: zewes_boletim.

*&---------------------------------------------------------------------*
*                                 TYPES                                *
*&---------------------------------------------------------------------*
TYPES:
*------* ty_zewes_boletim
  BEGIN OF ty_zewes_boletim,
    codigo TYPE zewes_boletim-codigo,
    nome   TYPE zewes_boletim-nome,
    nota1  TYPE zewes_boletim-nota1,
    nota2  TYPE zewes_boletim-nota2,
    nota3  TYPE zewes_boletim-nota3,
    nota4  TYPE zewes_boletim-nota4,
  END OF ty_zewes_boletim,
*------* ty_out
  BEGIN OF ty_out,
    nome   TYPE zewes_boletim-nome,
    codigo TYPE zewes_boletim-codigo,
    nota1  TYPE zewes_boletim-nota1,
    nota2  TYPE zewes_boletim-nota2,
    nota3  TYPE zewes_boletim-nota3,
    nota4  TYPE zewes_boletim-nota4,
    media  TYPE zewes_boletim-media,
  END OF ty_out.

*&---------------------------------------------------------------------*
*                        Tabelas Internas                              *
*&---------------------------------------------------------------------*
DATA: t_zewes_boletim TYPE TABLE OF ty_zewes_boletim,
      t_out           TYPE TABLE OF ty_out.

*&---------------------------------------------------------------------*
*                           Workareas                                  *
*&---------------------------------------------------------------------*
DATA: wa_zewes_boletim_insert TYPE zewes_boletim,
      wa_zewes_boletim        LIKE LINE OF t_zewes_boletim,
      wa_out                  LIKE LINE OF t_out.

*&---------------------------------------------------------------------*
*                         Tela de seleção                              *
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-000.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
*PARAMETERS
PARAMETERS: p_nome  TYPE zewes_boletim-nome   MODIF ID ins,
            p_nota1 TYPE zewes_boletim-nota1  MODIF ID ins,
            p_nota2 TYPE zewes_boletim-nota2  MODIF ID ins,
            p_nota3 TYPE zewes_boletim-nota3  MODIF ID ins,
            p_nota4 TYPE zewes_boletim-nota4  MODIF ID ins.
*SELECT-OPTIONS
SELECT-OPTIONS: s_nome   FOR zewes_boletim-nome NO-EXTENSION NO INTERVALS MODIF ID alv,
                s_codigo FOR zewes_boletim-codigo MODIF ID alv.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF LINE.
*Radiobuttons
PARAMETERS: rb_ins RADIOBUTTON GROUP gr1 DEFAULT 'X' USER-COMMAND comando.
SELECTION-SCREEN COMMENT 5(7) text-004 FOR FIELD rb_ins.
PARAMETERS: rb_alv  RADIOBUTTON GROUP gr1.
SELECTION-SCREEN COMMENT 14(10) text-005 FOR FIELD rb_alv.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b0.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modifica_tela.

*Início da execusão
START-OF-SELECTION.
  CASE rb_alv.
    WHEN 'X'.
      PERFORM: zf_select,
               zf_monta_t_out,
               zf_exibe_alv_poo.
    WHEN ' '.
      PERFORM zf_insert.
  ENDCASE.

*&---------------------------------------------------------------------*
*                          FORM zf_select                              *
*&---------------------------------------------------------------------*
FORM zf_select.
*------* t_zewes_boletim
  SELECT codigo
         nome
         nota1
         nota2
         nota3
         nota4
  FROM zewes_boletim
  INTO TABLE t_zewes_boletim
  WHERE nome   IN s_nome AND
        codigo IN s_codigo.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE s398(00) WITH 'Não há registros!' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*                          FORM zf_insert                              *
*&---------------------------------------------------------------------*
FORM zf_insert.

  CLEAR: wa_zewes_boletim_insert.
  wa_zewes_boletim_insert-nome   = p_nome.
  wa_zewes_boletim_insert-nota1  = p_nota1.
  wa_zewes_boletim_insert-nota2  = p_nota2.
  wa_zewes_boletim_insert-nota3  = p_nota3.
  wa_zewes_boletim_insert-nota4  = p_nota4.

  PERFORM: zf_gera_codigo.
  INSERT zewes_boletim FROM wa_zewes_boletim_insert.

  IF sy-subrc IS INITIAL.
    COMMIT WORK AND WAIT. "COMMIT WORK AND WAIT dá commit no banco de dados

    MESSAGE s208(00) WITH 'SALVO COM SUCESSO!'.
  ELSE.
    ROLLBACK WORK. "ROLLBACK WORK desfaz tudo o que aconteceu na operação
    MESSAGE s208(00) WITH 'ERRO AO GRAVAR!'DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*                       FORM zf_gera_codigo                            *
*&---------------------------------------------------------------------*
FORM zf_gera_codigo.
  DATA: lv_codigo TYPE i.
  CLEAR: lv_codigo.

  SELECT SINGLE MAX( codigo )
  FROM zewes_boletim
  INTO lv_codigo.

  IF sy-subrc EQ 0.
    wa_zewes_boletim_insert-codigo = lv_codigo + 1.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_T_OUT
*&---------------------------------------------------------------------*
FORM zf_monta_t_out .

  DATA: vl_media TYPE i,
        vl_nota1 TYPE i,
        vl_nota2 TYPE i,
        vl_nota3 TYPE i,
        vl_nota4 TYPE i.

  CLEAR: wa_zewes_boletim,
         wa_out.

  LOOP AT t_zewes_boletim INTO wa_zewes_boletim.

    wa_out-codigo = wa_zewes_boletim-codigo.
    wa_out-nome   = wa_zewes_boletim-nome.
    wa_out-nota1  = wa_zewes_boletim-nota1.
    wa_out-nota2  = wa_zewes_boletim-nota2.
    wa_out-nota3  = wa_zewes_boletim-nota3.
    wa_out-nota4  = wa_zewes_boletim-nota4.

    CLEAR: vl_nota1,
           vl_nota2,
           vl_nota3,
           vl_nota4.

    vl_nota1 = wa_out-nota1.
    vl_nota2 = wa_out-nota2.
    vl_nota3 = wa_out-nota3.
    vl_nota4 = wa_out-nota4.

    CALL FUNCTION 'ZFWES_MEDIA'
      EXPORTING
        nota1 = vl_nota1
        nota2 = vl_nota2
        nota3 = vl_nota3
        nota4 = vl_nota4
      IMPORTING
        media = vl_media.

    wa_out-media = vl_media.

    APPEND wa_out TO t_out.
    CLEAR: wa_out,
           wa_zewes_boletim.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MODIFICA_TELA
*&---------------------------------------------------------------------*
FORM modifica_tela .
  LOOP AT SCREEN.
*Inserir
    IF rb_ins EQ 'X'.
      IF screen-group1 EQ 'INS'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
      ENDIF.
      IF screen-group1 EQ 'ALV'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.
    ENDIF.
*Visualizar ALV
    IF rb_alv EQ 'X'.
      IF screen-group1 EQ 'ALV'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
      ENDIF.
      IF screen-group1 EQ 'INS'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_exibe_alv_poo
*&---------------------------------------------------------------------*
FORM zf_exibe_alv_poo.

  DATA: lo_table     TYPE REF TO cl_salv_table,  "Acessar a classe "cl_salv_table"
        lo_header    TYPE REF TO cl_salv_form_layout_grid,   "Para criação do header
        lo_columns   TYPE REF TO cl_salv_columns_table,  "Ajustar tamanho dos subtítulos
        lo_functions TYPE REF TO cl_salv_functions,
        lo_display   TYPE REF TO cl_salv_display_settings,
        lo_column    TYPE REF TO cl_salv_column.


  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_table "Tabela local
                             CHANGING t_table = t_out ).

      lo_functions = lo_table->get_functions( ). "Ativar met codes
      lo_functions->set_all( abap_true ).

      CREATE OBJECT lo_header. "É necessário que criemos o objeto header

      lo_header->add_row( ).
      lo_header->create_header_information( row = 1 column = 1 text = 'Boletins dos Alunos' ). "Texto grande do header

      lo_display = lo_table->get_display_settings( ).
      lo_display->set_striped_pattern( abap_true ).

      lo_table->set_top_of_list( lo_header ).

      lo_columns = lo_table->get_columns( ). "Ajustar tamanho dos subtítulos
      lo_columns->set_optimize( abap_true ). "Ajustar tamanho dos subtítulos


      lo_table->display( ) . "O dispay é fundamental para a exibição do ALV

    CATCH cx_salv_msg
          cx_root.

      MESSAGE s398(00) WITH 'Erro ao exibir tabela' DISPLAY LIKE 'E'.

  ENDTRY.

ENDFORM.
