REPORT zteste.

INITIALIZATION.
PERFORM: zf_select.

*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBE_POPUP_CAMINHO_UPLOAD
*&---------------------------------------------------------------------*
FORM zf_select.

   SELECT MAX( id )  "Seleciona o maior valor existente no campo ID
     FROM ztestudos_1
     INTO @DATA(v_id).

v_id = v_id + 1.

ENDFORM.
