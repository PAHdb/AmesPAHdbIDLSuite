; docformat = 'rst'

;+
;
; Helper function for creating an AmesPAHdb_StellarModel_S struct.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Creating an AmesPAHdb_StellarModel_S struct:
;
;     IDL> model = AMESPAHDBIDLSUITE_CREATE_KURUCZ_STELLARMODEL_S(angstroms, flam)
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
;     02-01-2015
;     First version of the file. Christiaan Boersma.
;     11-18-2015
;     Fixed FLEM to FLAM. Christiaan Boersma.
;-

;+
; Creates an AmesPAHdb_StellarModel_S struct, converting flam
; [erg/s/cm^2/A] to CGS [erg/s/cm^2/cm^-1/sr] and wavelength
; [angstrom] to wavenumber [/cm]
;
; :Returns:
;   Structure
;
; :Params:
;   angstroms: in, required, type="double array"
;     Wavelength grid in Angstrom
;   flam: in, required, type="double array"
;     Associated flam
;
; :Categories:
;   HELPER
;-
FUNCTION AmesPAHdbIDLSuite_CREATE_KURUCZ_STELLARMODEL_S,angstroms,flam

  PRINT
  PRINT,"========================================================="
  PRINT," CONVERTING FLAM [erg/s/cm^2/A] TO CGS [erg/s/cm^2/cm^-1/sr] "
  PRINT,"========================================================="
  PRINT

  ;convert flam [erg/s/cm^2/A] to cgs [erg/s/cm^2/cm^-1/sr]
  intensity = 1D-8 * REVERSE(flam * angstroms^2) / (4 * !DPI)

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

; END OF amespahdbidlsuite_create_kurucz_stellarmodel_s.pro