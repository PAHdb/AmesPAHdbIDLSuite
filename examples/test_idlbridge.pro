; docformat = 'rst'

;+
;
; This is an example of testing the IDL_IDLBridge components provided
; by the Suite, built around the functionality provided by the
; AmesPAHdbIDLSuite and should help confirm that the it has been
; properly installed. The source code is annotated to guide users and
; developers in the inner workings of the suite.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Call the procedure directly::
;
;     IDL> test_idlbridge
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
;     08-19-2019
;     Documentation added. Christiaan Boersma.
;-

;+
; Procedure testing the IDL_IDLBridge functionallity.
;
; :Categories:
;   Example
;-
PRO TEST_IDLBRIDGE

  ; read in the default database defined by the environement variable
  ; !AMESPAHDEFAULTDB or the system variable AMESPAHDEFAULTDB. use the
  ; keyword FILENAME if these have not been set
  pahdb = OBJ_NEW('AmesPAHdbIDLSuite')

  ; retrieve the transitions for all PAHs
  transitions = pahdb->getTransitionsByUID( -1 )

  ; use an average photon energy of 6 eV (in CGS units)
  energy = 6D * 1.6021765D-12

  ; initialize the first timer
  timer1 = SYSTIME(/SECONDS)

  ; perform the regular cascade

  transitions->Cascade,energy,IDLBridge=0

  ; stop the first timer
  timer1 = SYSTIME(/SECONDS) - timer1

  ; discard transitions object
  OBJ_DESTROY,transitions

  ; retrieve all transitions from the database
  transitions = pahdb->getTransitionsByUID( -1 )

  ; initialize the second timer
  timer2 = SYSTIME(/SECONDS)

  ; perform cascade using IDLBridge
  transitions->Cascade,energy

  ; stop second timer
  timer2 = SYSTIME(/SECONDS) - timer2

  ; clean up
  OBJ_DESTROY,[transitions, pahdb]

  ; print results
  IF timer1 LT 1 THEN PRINT,FORMAT='(A-30, ": ", A0)',"REGULAR TIME",STRING(FORMAT='(I-3)', timer1*1D3)+" MILLISECONDS" $
  ELSE IF timer1 LT 60 THEN PRINT,FORMAT='(A-30, ": ", A0)',"REGULAR TIME",STRING(FORMAT='(I02)',timer1)+" SECONDS" $
  ELSE IF timer1 LT 3600 THEN PRINT,FORMAT='(A-30, ": ", A0)',"REGULAR TIME",STRING(FORMAT='(I02,":",I02)',timer1/60,timer1 MOD 60)+" MINUTES" $
  ELSE IF timer1 LT 86400 THEN PRINT,FORMAT='(A-30, ": ", A0)',"REGULAR TIME",STRING(FORMAT='(I02,":",I02,":",I02)',timer1/3600,(timer1 MOD 3600)/60,(timer1 MOD 3600) MOD 60)+" HOURS" $
  ELSE PRINT,FORMAT='(A-30, ": ", A0)',"REGULAR TIME",STRING(FORMAT='(I03)',timer1/86400E)+" DAYS"

  IF timer2 LT 1 THEN PRINT,FORMAT='(A-30, ": ", A0)',"IDL_BRIDGE TIME",STRING(FORMAT='(I-3)', timer2*1D3)+" MILLISECONDS" $
  ELSE IF timer2 LT 60 THEN PRINT,FORMAT='(A-30, ": ", A0)',"IDL_BRIDGE TIME",STRING(FORMAT='(I02)',timer2)+" SECONDS" $
  ELSE IF timer2 LT 3600 THEN PRINT,FORMAT='(A-30, ": ", A0)',"IDL_BRIDGE TIME",STRING(FORMAT='(I02,":",I02)',timer2/60,timer2 MOD 60)+" MINUTES" $
  ELSE IF timer2 LT 86400 THEN PRINT,FORMAT='(A-30, ": ", A0)',"IDL_BRIDGE TIME",STRING(FORMAT='(I02,":",I02,":",I02)',timer2/3600,(timer2 MOD 3600)/60,(timer2 MOD 3600) MOD 60)+" HOURS" $
  ELSE PRINT,FORMAT='(A-30, ": ", A0)',"IDL_BRIDGE TIME",STRING(FORMAT='(I03)',timer2/86400E)+" DAYS"

  PRINT,FORMAT='(A-30, ": ", A0)',"SPEED-UP",STRING(FORMAT='(F3.1,"x")', timer1/timer2)

END
