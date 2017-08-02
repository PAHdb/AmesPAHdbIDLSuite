;+
; FUNCTION_NAME:
;    AmesPAHdbIDLSuite_CREATE_KURUCZ_STELLARMODEL_S
;
; CONTACT:
;
;    Updated versions of the NASA Ames PAH IR Spectroscopic
;    Database and more information can be found at:
;    http://www.astrochem.org/pahdb
;
; DESCRIPTION: Helper function for creating a
;    AmesPAHdb_StellarModel_S struct
;
; CATEGORY:
;    HELPER
;
; EXAMPLE
;   model = $
;     AMESPAHDBIDLSUITE_CREATE_KURUCZ_STELLARMODEL_S(angstroms, flem)
;
; MODIFICATION HISTORY
;
;   02-01-2015
;   First version of the file. Christiaan Boersma.
;-

FUNCTION AmesPAHdbIDLSuite_CREATE_KURUCZ_STELLARMODEL_S,angstroms,flem

  PRINT
  PRINT,"========================================================="
  PRINT," CONVERTING FLEM [erg/s/cm^2/A] TO CGS [erg/s/cm^2/cm^-1/sr] "
  PRINT,"========================================================="
  PRINT

  ;convert flem [erg/s/cm^2/A] to cgs [erg/s/cm^2/cm^-1/sr]
  intensity = 1D-8 * REVERSE(flem * angstroms^2) / (4 * !DPI)

  PRINT
  PRINT,"========================================================="
  PRINT,"      CONVERTING ANGSTROM [A] TO WAVENUMBERS [/cm]       "
  PRINT,"========================================================="
  PRINT
  
  ; convert angstroms [A] to wavenumbers [/cm]
  wavenumbers = 1D8 / REVERSE(angstroms)

  model = REPLICATE({AmesPAHdb_StellarModel_S, $
                     frequency:0D, $
                     intensity:0D}, N_ELEMENTS(wavenumbers))

  model.frequency = wavenumbers

  model.intensity = intensity

  RETURN,model
END
