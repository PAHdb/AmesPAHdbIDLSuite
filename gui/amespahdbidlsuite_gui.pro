; docformat = 'rst'

;+
;
; Procedure for starting the AmesPAHdbIDLSuite Graphical User Interface.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Start the AmesPAHdbIDLSuite Graphical User Interface::
;
;      IDL> amespahdbidlsuite_gui
;
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

;+
; Starts the AmesPAHdbIDLSuite Graphical User Interface.
;
; :Keywords:
;   Filename: in, optional, type=string
;     Database XML-file
;
; :Categories:
;   GUI
;-
PRO AmesPAHdbIDLSuite_GUI,Filename=Filename

  application = OBJ_NEW('AmesPAHdbIDLSuite_GUI_Application_Main', Filename=Filename)

  IF NOT OBJ_VALID(application) THEN RETURN

  ; application destroys self on quit
  application->Run
END
