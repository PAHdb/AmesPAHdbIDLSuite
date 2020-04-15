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


PRO AmesPAHdbIDLSuite_GUI_Draw_Geometry::EventHandler,Event

  COMPILE_OPT IDL2

  ON_ERROR,2

   IF Event.id EQ self.base AND OBJ_VALID(self.grModel) THEN BEGIN

        updated = self.grTrackball->Update(Event, TRANSFORM=trackXform)

        IF (updated) THEN BEGIN

           self.grModel->GetProperty,TRANSFORM=modelXform

           self.grModel->SetProperty,TRANSFORM=modelXform # trackXform

           self.grWindow->Draw,self.grView
       ENDIF
   ENDIF
END

PRO EventHandler__AmesPAHdbIDLSuite_GUI_Draw_Geometry,Event

  COMPILE_OPT IDL2

  ON_ERROR,2

  WIDGET_CONTROL,Event.handler,GET_UVALUE=obj

  CALL_METHOD,'EventHandler',obj,Event
END

PRO AmesPAHdbIDLSuite_GUI_Draw_Geometry::Update,UID

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.delegate) THEN RETURN

  IF NOT OBJ_VALID(self.grWindow) THEN BEGIN

     WIDGET_CONTROL,self.base,GET_VALUE=window

     self.grWindow = window

     self.grView = OBJ_NEW('IDLgrView', PROJECTION=2, COLOR=[192, 192, 192])

     self.grWindow->Draw,self.grView

     self.grTrackball = OBJ_NEW('Trackball', [100, 100], 100)
  ENDIF

  geometry = (*self.delegate)->GetGeometryByUID(UID)

  geometry->Diagonalize

  IF OBJ_VALID(self.grModel) THEN OBJ_DESTROY,self.grModel

  self.grModel = geometry->Structure(/OBJ)

  GET_BOUNDS,self.grModel,x,y,z

  max = 1.1 * MAX(ABS([x, y, z]))

  grAmbient = OBJ_NEW('IDLgrLight', COLOR=[255,255,255], INTENSITY=0.95)

  self.grModel->Add,grAmbient

  grDirectional = OBJ_NEW('IDLgrLight', TYPE=2, COLOR=[255,255,255], DIRECTION=[0,0,0], LOCATION=[0,1,0])

  self.grModel->Add,grDirectional

  self.grView->SetProperty,VIEWPLANE_RECT=[-max, -max, 2 * max, 2 * max], ZCLIP=[max, -max], EYE=2.5*max

  self.grView->Add,self.grModel

  self.grWindow->Draw,self.grView

  OBJ_DESTROY,geometry

  self.uid = UID
END

PRO AmesPAHdbIDLSuite_GUI_Draw_Geometry::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF OBJ_VALID(self.grView) THEN OBJ_DESTROY,self.grView

  IF OBJ_VALID(self.grTrackball) THEN OBJ_DESTROY,self.grTrackball

  IF OBJ_VALID(self.grModel) THEN OBJ_DESTROY,self.grModel

  self->AmesPAHdbIDLSuite_GUI_View::Cleanup
END

FUNCTION AmesPAHdbIDLSuite_GUI_Draw_Geometry::Init,parent,_Extra=extra

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF self->AmesPAHdbIDLSuite_GUI_View::Init(_Extra=extra) EQ 0 THEN RETURN,0

  self.base = WIDGET_DRAW(parent, GRAPHICS_LEVEL=2, /BUTTON_EVENTS, /MOTION_EVENTS, /EXPOSE_EVENTS, RETAIN=2, SCR_XSIZE=200, SCR_YSIZE=200, XSIZE=200, YSIZE=200, EVENT_PRO='EventHandler__AmesPAHdbIDLSuite_GUI_Draw_Geometry', UVALUE=self)

  RETURN,1
END

PRO AmesPAHdbIDLSuite_GUI_Draw_Geometry__DEFINE,class

  COMPILE_OPT IDL2

  ON_ERROR,2

  class = {AmesPAHdbIDLSuite_GUI_Draw_Geometry, $
           INHERITS AmesPAHdbIDLSuite_GUI_View, $
           uid:0L, $
           grWindow:OBJ_NEW(), $
           grView:OBJ_NEW(), $
           grModel:OBJ_NEW(), $
           grTrackball:OBJ_NEW()}
END

; END OF amespahdbidlsuite_gui_draw_geometry__define.pro
