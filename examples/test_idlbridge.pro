; NASA Ames PAH IR Spectroscopic Database
;
; This is an example of testing the IDL_IDLBridge components 
; provided by the Suite and should help confirm that the
; AmesPAHdbIDLSuite has been properly installed.
; 
; Additional information can be found at
; http://www.astrochem.org/pahdb, in Bauschlicher et al. 2010, The
; Astrophysical Journal Supplement Series, 189, 341 and in Boersma et
; al. 2014, The Astrophysical Journal Supplement Series, 211, 8.
;
; USAGE
;   test_idl_bridge
;
PRO TEST_IDLBRIDGE

  ; read in the default database defined by the environement variable
  ; !AMESPAHDEFAULTDB or the system variable AMESPAHDEFAULTDB. use
  ; the keyword FILENAME if these have not been set
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
