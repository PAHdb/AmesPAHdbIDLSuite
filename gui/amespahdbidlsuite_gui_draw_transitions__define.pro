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


PRO AmesPAHdbIDLSuite_GUI_Draw_Transitions::EventHandler,Event

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF Event.id EQ self.base AND PTR_VALID(self.delegate) AND self.uid NE 0 THEN BEGIN

     IF Event.type EQ 0 AND Event.Press EQ 1 THEN BEGIN

        self.xy = [Event.x, Event.y]

        WIDGET_CONTROL,self.base,/DRAW_MOTION_EVENTS
     ENDIF ELSE IF Event.type EQ 1 THEN BEGIN

        WSET,self.window

        DEFSYSV,'!AmesPAHdbIDLSuite_Plot_Managed_Window_ID',self.window

        IF Event.Release EQ 1 THEN BEGIN

           transitions = (*self.delegate)->GetTransitionsByUID(self.uid)

           coords = CONVERT_COORD([self.xy[0], Event.x], [self.xy[1], Event.y], /DEVICE, /TO_DATA)

           coords[1] *= 10D^(!AmesPAHdbIDLSuite_Plot_P.YORDER)

           IF coords[0,0] LT coords[0,1] THEN BEGIN

              tmp = coords[0,1]

              coords[0,1] = coords[0,0]

              coords[0,0] = tmp
           ENDIF

           IF coords[1,0] GT coords[1,1] THEN BEGIN

              tmp = coords[1,1]

              coords[1,1] = coords[1,0]

              coords[1,0] = tmp
           ENDIF

           IF coords[1,0] LT 0 THEN coords[1,0] = 0

           transitions->Plot,XRANGE=coords[0,0:1],YRANGE=coords[1,0:1],/XSTYLE,/YSTYLE,Legend=0,CHARSIZE=1

           OBJ_DESTROY,transitions

           WIDGET_CONTROL,self.base,DRAW_MOTION_EVENTS=0
        ENDIF ELSE IF Event.Release EQ 4 THEN BEGIN

           transitions = (*self.delegate)->GetTransitionsByUID(self.uid)

           transitions->Plot,Legend=0,CHARSIZE=1.0

           OBJ_DESTROY,transitions
        ENDIF

        WSET,self.pixmap

        DEVICE,COPY=[0, 0, 300, 200, 0, 0, self.window]
     ENDIF ELSE IF Event.type EQ 2 THEN BEGIN

        WSET,self.window

        DEFSYSV,'!AmesPAHdbIDLSuite_Plot_Managed_Window_ID',self.window

        DEVICE,COPY=[0, 0, 300, 200, 0, 0, self.pixmap]

        PLOTS,[Event.x, Event.x, self.xy[0], self.xy[0]],[self.xy[1], Event.y, Event.y, self.xy[1]],/CONTINUE,/DEVICE,COLOR='00ff00'x
     ENDIF
  ENDIF
END

PRO EventHandler__AmesPAHdbIDLSuite_GUI_Draw_Transitions,Event

  COMPILE_OPT IDL2

  ON_ERROR,2

  WIDGET_CONTROL,Event.handler,GET_UVALUE=obj

  CALL_METHOD,'EventHandler',obj,Event
END

PRO AmesPAHdbIDLSuite_GUI_Draw_Transitions::Update,UID

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.delegate) THEN RETURN

  WIDGET_CONTROL,self.base,GET_VALUE=window

  self.window = window

  DEFSYSV,'!AmesPAHdbIDLSuite_Plot_Managed_Window_ID',self.window

  WSET,self.window

  transitions = (*self.delegate)->getTransitionsByUID(UID)

  transitions->Plot,Legend=0,CHARSIZE=1

  OBJ_DESTROY,transitions

  WSET,self.pixmap

  DEVICE,COPY=[0, 0, 300, 200, 0, 0, self.window]

  WSET,self.window

  self.uid = UID
END

PRO  AmesPAHdbIDLSuite_GUI_Draw_Transitions::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_GUI_View::Cleanup

  WDELETE,self.pixmap

  DEFSYSV,'!AmesPAHdbIDLSuite_Plot_Managed_Window_ID',-2
END

FUNCTION AmesPAHdbIDLSuite_GUI_Draw_Transitions::Init,parent,_Extra=extra

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF self->AmesPAHdbIDLSuite_GUI_View::Init(_Extra=extra) EQ 0 THEN RETURN,0

  self.base = WIDGET_DRAW(parent, GRAPHICS_LEVEL=1, /BUTTON_EVENTS, /EXPOSE_EVENTS, RETAIN=2, SCR_XSIZE=300, SCR_YSIZE=200, EVENT_PRO='EventHandler__AmesPAHdbIDLSuite_GUI_Draw_Transitions', UVALUE=self)

  WINDOW,/FREE,/PIXMAP,XSIZE=300,YSIZE=200

  self.pixmap = !D.WINDOW

  RETURN,1
END

PRO AmesPAHdbIDLSuite_GUI_Draw_Transitions__DEFINE,class

  COMPILE_OPT IDL2

  ON_ERROR,2

  class = {AmesPAHdbIDLSuite_GUI_Draw_Transitions, $
           INHERITS AmesPAHdbIDLSuite_GUI_View, $
           uid:0L, $
           xy:FLTARR(2), $
           window:0, $
           pixmap:0}
END

; END OF amespahdbidlsuite_gui_draw_transitions__define.pro
