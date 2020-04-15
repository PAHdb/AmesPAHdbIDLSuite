; docformat = 'rst'

;+
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Private:
;
; :Author:
;   Dr. Christiaan Boersma
;
; :Copyright:
;   BSD licensed
;
; :History:
;   Changes::
;
;     12-31-2015
;     First version of the file. Christiaan Boersma.
;-


PRO AmesPAHdbIDLSuite_GUI_Button_Checkbox::SetState,state

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF self.ischecked EQ state THEN RETURN

  self.ischecked = state

  WIDGET_CONTROL,self.checkbox,SET_VALUE=state EQ 0B ? !AmesPAHdbIDLSuite_GUI_Button_Checkbox_UnChecked : !AmesPAHdbIDLSuite_GUI_Button_Checkbox_Checked
END

FUNCTION AmesPAHdbIDLSuite_GUI_Button_Checkbox::EventHandler,Event

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF Event.id EQ self.checkbox THEN BEGIN

     self->SetState,(self.ischecked ? 0B : 1B)

     RETURN,{AmesPAHdbIDLSuite_GUI_Button_Checkbox_Event, id:Event.handler, top:Event.top, handler:0L, checked:self.ischecked}
  ENDIF ELSE IF Event.id EQ self.button THEN BEGIN

     RETURN,{AmesPAHdbIDLSuite_GUI_Button_Checkbox_Button_Event, id:Event.handler, top:Event.top, handler:0L}
  ENDIF
END

FUNCTION Eventhandler__AmesPAHdbIDLSuite_GUI_Button_Checkbox,Event

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  WIDGET_CONTROL,Event.handler,GET_UVALUE=obj

  RETURN,CALL_METHOD('EventHandler', obj, Event)
END

PRO AmesPAHdbIDLSuite_GUI_Button_Checkbox::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2
END

FUNCTION AmesPAHdbIDLSuite_GUI_Button_Checkbox::Init,parent,Base=Base,VALUE=value,IsChecked=IsChecked,YOFFSET=YOFFSET

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.base = WIDGET_BASE(parent, EVENT_FUNC='EventHandler__AmesPAHdbIDLSuite_GUI_Button_Checkbox', UVALUE=self, YOFFSET=YOFFSET)

  DEFSYSV,'!AmesPAHdbIDLSuite_GUI_Button_Checkbox_UnChecked',EXISTS=yn

  IF NOT yn THEN BEGIN

     old_d = !D.NAME

     SET_PLOT,'Z'

     DEVICE,SET_RESOLUTION=[24, 32],SET_PIXEL_DEPTH=24,DECOMPOSED=0

     ERASE,192

     XYOUTS,12,12,'[ ]',/DEVICE,ALIGN=0.5,COLOR=0

     DEFSYSV,'!AmesPAHdbIDLSuite_GUI_Button_Checkbox_UnChecked',TVRD(TRUE=3),1B

     ERASE,192

     XYOUTS,12,12,'[X]',/DEVICE,ALIGN=0.5,COLOR=0

     DEFSYSV,'!AmesPAHdbIDLSuite_GUI_Button_Checkbox_Checked',TVRD(TRUE=3),1B

     SET_PLOT,old_d
  ENDIF

  IF KEYWORD_SET(IsChecked) THEN self.ischecked = 1B

  self.checkbox = WIDGET_BUTTON(self.base, VALUE=self.ischecked ? !AmesPAHdbIDLSuite_GUI_Button_Checkbox_Checked : !AmesPAHdbIDLSuite_GUI_Button_Checkbox_UnChecked, /FLAT, SCR_XSIZE=24, SCR_YSIZE=32)

  self.button = WIDGET_BUTTON(self.base, VALUE=value, /FLAT, SCR_XSIZE=100, SCR_YSIZE=32, XOFFSET=24)

  Base = self.base

  RETURN,1
END

PRO AmesPAHdbIDLSuite_GUI_Button_Checkbox__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  void = {AmesPAHdbIDLSuite_GUI_Button_Checkbox, $
          base:0L, $
          checkbox:0L, $
          button:0L, $
          ischecked:0B}
END

; END OF amespahdbidlsuite_gui_button_checkbox__define.pro
