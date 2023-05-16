; docformat = 'rst'

;+
;
; XML parser for reading a NASA Ames PAH IR Spectroscopic Database's
; XML file.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Obtaining and plotting the experimental spectrum of the PAH species
;   with UID = 18::
;
;     IDL> dbi = OBJ_NEW('AmesPAHdbIDLSuite_XMLParser', Filename='experimental.xml')
;     IDL> transitions = dbi->GetTransitions
;     IDL> select = WHERE(transitions.uid EQ 18)
;     IDL> PLOT,transitions[select].frequency,transitions[select].intensity
;     IDL> OBJ_DESTROY,[transitions, dbi]
;
; :Properties:
;   SCHEMA_CHECKING: in, optional, type=int
;     Whether to check the structure of the XML
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
;     05-09-2023
;     Access proper struct for database filename in ENDDOCUMENT. Christiaan
;     Boersma.
;     05-12-2022
;     Sort content by UID. Increased some buffer sizes. Christiaan
;     Boersma.
;     11-15-2019
;     Added status property, which is set to 1B when FATALERROR needed
;     to be called and can be queried with the new
;     STATUS-method. Christiaan Boersma.
;     02-10-2019
;     Now expecting laboratory spectrum to be base64 encoded.
;     Christiaan Boersma.
;     02-08-2019
;     Renamed .nhex to .nbytes and binaryx to bytesx for
;     clarity. Christiaan Boersma.
;     08-21-2016
;     Remove CONVERT_ASCII dependence in ENDELEMENT. Christiaan Boersma.
;     07-29-2016
;     Parse scale attribute for frequencies. Christiaan Boersma.
;     10-04-2015
;     There is no energy-tag. Chrisiaan Boersma.
;     02-01-2015
;     First version of the file. Christiaan Boersma.
;
; :Private:
;-

;+
; Returns the parsed database.
;
; :Returns:
;   anonymous struct
;
; :Categories:
;   SET/GET
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_XMLParser::GetDatabase

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,{filename:self.database.filename, $
          type:self.database.type, $
          version:self.database.version, $
          date:self.database.date,$
          full:self.database.full, $
          comment:self.database.comment, $
          sizes:self.sizes, $
          data:{species:*self.database.data.species, $
                comments:*self.database.data.comments, $
                references:*self.database.data.references, $
                transitions:*self.database.data.transitions, $
                geometries:*self.database.data.geometries, $
                laboratory:*self.database.data.laboratory}}
END

;+
; Returns the parsed species.
;
; :Returns:
;   AmesPAHdb_Species_S array
;
; :Categories:
;   SET/GET
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_XMLParser::GetSpecies

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,*self.database.data.species
END

;+
; Returns the parsed transitions.
;
; :Returns:
;   AmesPAHdb_Transition_S array
;
; :Categories:
;   SET/GET
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_XMLParser::GetTransitions

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,*self.database.data.transitions
END

;+
; Returns the parsed laboratory spectrum.
;
; :Returns:
;   AmesPAHdb_Laboratory_S array
;
; :Categories:
;   SET/GET
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_XMLParser::GetLaboratory

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,*self.database.data.laboratory
END

;+
; Returns the parsed geometries.
;
; :Returns:
;   AmesPAHdb_Geometry_S array
;
; :Categories:
;   SET/GET
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_XMLParser::GetGeometries

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,*self.database.data.geometries
END

;+
; Returns the parsed comments.
;
; :Returns:
;   AmesPAHdb_Comment_S array
;
; :Categories:
;   SET/GET
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_XMLParser::GetComments

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,*self.database.data.comments
END

;+
; Returns the parsed references.
;
; :Returns:
;   AmesPAHdb_Reference_S array
;
; :Categories:
;   SET/GET
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_XMLParser::GetReferences

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,*self.database.data.references
END

;+
; Parse Filename.
;
; :Params:
;   Filename: optional, type=string
;     Name of the XML-file to parse
;
; :Keywords:
;   URL: in, optional, type=string
;     URL of the XML-file to parse
;   XML_STRING: in, optional, type=string
;     XML-string to parse
;
; :Categories:
;   PARSING
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_XMLParser::ParseFile,Filename,URL=URL,XML_STRING=XML_STRING

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.database.filename = Filename

  self->IDLffXMLSAX::ParseFile,Filename,URL=URL,XML=XML_STRING
END

;+
; Finish document parsing.
;
; :Categories:
;   PARSING
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_XMLParser::EndDocument

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF self.sizes.nspecies GT 0 THEN BEGIN

    *self.database.data.species = (*self.database.data.species)[0:self.sizes.nspecies++]

    *self.database.data.species = (*self.database.data.species)[SORT((*self.database.data.species).uid)]

  ENDIF

  IF self.sizes.ncomments GT 0 THEN BEGIN

    *self.database.data.comments = (*self.database.data.comments)[0:self.sizes.ncomments++]

    *self.database.data.comments = (*self.database.data.comments)[SORT((*self.database.data.comments).uid)]

  ENDIF

  IF self.sizes.nreferences GT 0 THEN BEGIN

    *self.database.data.references = (*self.database.data.references)[0:self.sizes.nreferences++]

    *self.database.data.references = (*self.database.data.references)[SORT((*self.database.data.references).uid)]

  ENDIF

  IF self.sizes.ntransitions GT 0 THEN BEGIN

    *self.database.data.transitions = (*self.database.data.transitions)[0:self.sizes.ntransitions++]

    *self.database.data.transitions = (*self.database.data.transitions)[SORT((*self.database.data.transitions).uid)]

  ENDIF

  IF self.sizes.ngeometries GT 0 THEN BEGIN

    *self.database.data.geometries = (*self.database.data.geometries)[0:self.sizes.ngeometries++]

    *self.database.data.geometries = (*self.database.data.geometries)[SORT((*self.database.data.geometries).uid)]

  ENDIF

  IF self.sizes.nlaboratory GT 0 THEN BEGIN

    *self.database.data.laboratory = (*self.database.data.laboratory)[0:self.sizes.nlaboratory++]

    *self.database.data.laboratory = (*self.database.data.laboratory)[SORT((*self.database.data.laboratory).uid)]

  ENDIF

  PRINT
  PRINT,"========================================================="
  PRINT,"         NASA Ames PAH IR SPECTROSCOPIC DATABASE"
  PRINT,"========================================================="

  PRINT
  PRINT,"========================================================="
  PRINT,FORMAT='(A-30, ": ", A0)',"FILENAME",""+self.database.filename
  PRINT,FORMAT='(A-30, ": ", A0)',"DATABASE",""+self.database.type+" ("+(self.database.full ? "complete" : "partial")+")"
  timer = SYSTIME(/SECONDS) - self.timer
  IF timer LT 1 THEN PRINT,FORMAT='(A-30, ": ", A0)',"PARSE TIME",STRING(FORMAT='(I-3)',timer*1D3)+" MILLISECONDS" $
  ELSE IF timer LT 60 THEN PRINT,FORMAT='(A-30, ": ", A0)',"PARSE TIME",STRING(FORMAT='(I02)',timer)+" SECONDS" $
  ELSE IF timer LT 3600 THEN PRINT,FORMAT='(A-30, ": ", A0)',"PARSE TIME",STRING(FORMAT='(I02,":",I02)',timer/60,timer MOD 60)+" MINUTES" $
  ELSE IF timer LT 86400 THEN PRINT,FORMAT='(A-30, ": ", A0)',"PARSE TIME",STRING(FORMAT='(I02,":",I02,":",I02)',timer/3600,(timer MOD 3600)/60,(timer MOD 3600) MOD 60)+" HOURS" $
  ELSE PRINT,FORMAT='(A-30, ": ", A0)',"PARSE TIME",STRING(FORMAT='(I03)',timer/86400E)+" DAYS"
  PRINT,FORMAT='(A-30, ": ", A0)',"VERSION (DATE)",""+self.database.version+" ("+self.database.date+")"
  PRINT,FORMAT='(A-30, ": ", I-0)',"TOTAL NUMBER OF SPECIES",self.sizes.nspecies
  PRINT,FORMAT='(A-30, ": ", I-0)',"TOTAL NUMBER OF TRANSITIONS",self.sizes.ntransitions
  PRINT,FORMAT='(A-30,": ", A0)',"COMMENT",self.database.comment
  PRINT,"========================================================="
  PRINT
END

;+
; Finish element parsing.
;
; :Params:
;   URI: in, required, type=string
;     Namespace of element
;   Local: in, required, type=string
;     Name of element without namespace
;   Name: in, required, type=string
;     Name of element as found in the XML-file
;
; :Categories:
;   PARSING
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_XMLParser::EndElement,URI,Local,Name

  COMPILE_OPT IDL2

  ON_ERROR,2

  CASE Name OF

     'frequency' : BEGIN

        IF self.mode GT 0 THEN (*self.database.data.transitions)[self.sizes.ntransitions].frequency = DOUBLE(self.chardata) ELSE BEGIN

           (*self.storage).bytes1 = PTR_NEW(IDL_Base64(self.chardata))
        ENDELSE
     END

     'intensity' : BEGIN

        IF self.mode GT 0 THEN (*self.database.data.transitions)[self.sizes.ntransitions].intensity = DOUBLE(self.chardata) ELSE BEGIN

           (*self.storage).bytes2 = PTR_NEW(IDL_Base64(self.chardata))
        ENDELSE
     END

     'symmetry' : BEGIN

        IF self.mode GT 0 THEN (*self.database.data.transitions)[self.sizes.ntransitions].symmetry = self.chardata $
        ELSE (*self.database.data.species)[self.sizes.nspecies].symm = self.chardata
     END

     'position' : (*self.database.data.geometries)[self.sizes.ngeometries].position = FIX(self.chardata)

     'x' : (*self.database.data.geometries)[self.sizes.ngeometries].x = DOUBLE(self.chardata)

     'y' : (*self.database.data.geometries)[self.sizes.ngeometries].y = DOUBLE(self.chardata)

     'z' : (*self.database.data.geometries)[self.sizes.ngeometries].z = DOUBLE(self.chardata)

     'type' : BEGIN
        ; 16    17    18    19    20     21     22
        ; nc:0, nh:0, nn:0, no:0, nmg:0, nsi:0, nfe:0
        ; 6     1     7     8     12     14     26
        key = [-1, 16, -1, -1, -1, -1, 15, 17, 18, -1, -1, -1, 19, -1, 20, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 21]
        ;      0   1   2   3   4   5   6   7   8    9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26

        type = FIX(self.chardata)

        (*self.database.data.geometries)[self.sizes.ngeometries].type = type

        (*self.database.data.species)[self.sizes.nspecies].(key[type])++
     END

     'mode' : self.mode = 0

     'formula' : (*self.database.data.species)[self.sizes.nspecies].formula = self.chardata

     'charge' : (*self.database.data.species)[self.sizes.nspecies].charge = FIX(self.chardata)

     'n_solo' : (*self.database.data.species)[self.sizes.nspecies].nsolo = FIX(self.chardata)

     'n_duo' : (*self.database.data.species)[self.sizes.nspecies].nduo = FIX(self.chardata)

     'n_trio' : (*self.database.data.species)[self.sizes.nspecies].ntrio = FIX(self.chardata)

     'n_quartet' : (*self.database.data.species)[self.sizes.nspecies].nquartet = FIX(self.chardata)

     'n_quintet' : (*self.database.data.species)[self.sizes.nspecies].nquintet = FIX(self.chardata)

     'n_ch2' : (*self.database.data.species)[self.sizes.nspecies].nch2 = FIX(self.chardata)

     'n_chx' : (*self.database.data.species)[self.sizes.nspecies].nchx = FIX(self.chardata)

     'weight' : (*self.database.data.species)[self.sizes.nspecies].weight = DOUBLE(self.chardata)

     'total_e' : (*self.database.data.species)[self.sizes.nspecies].energy = DOUBLE(self.chardata)

     'vib_e' : (*self.database.data.species)[self.sizes.nspecies].zeropoint = DOUBLE(self.chardata)

     'method' : (*self.database.data.species)[self.sizes.nspecies].method = self.chardata

     'comment' : BEGIN

        IF self.sizes.nspecies GT -1 THEN BEGIN

           (*self.database.data.comments)[self.sizes.ncomments].str = self.chardata
        ENDIF ELSE self.database.comment = self.chardata
     END

     'reference' : (*self.database.data.references)[self.sizes.nreferences].str = self.chardata

     'laboratory' : BEGIN

        nfloats = N_ELEMENTS(*(*self.storage)[0].bytes1) / 4

        self.sizes.nlaboratory += nfloats

        struct = REPLICATE({AmesPAHdb_Laboratory_Spectrum_S, $
                            uid:0L, $
                            frequency:0D, $
                            intensity:0D}, nfloats)

        struct.uid = self.current

        struct.frequency = DOUBLE(FLOAT(*(*self.storage).bytes1, 0, nfloats))

        struct.intensity = DOUBLE(FLOAT(*(*self.storage).bytes2, 0, nfloats))

        (*self.database.data.laboratory) = [(*self.database.data.laboratory), struct]

        PTR_FREE,(*self.storage).bytes1

        PTR_FREE,(*self.storage).bytes2

        PTR_FREE,self.storage
     END

     ELSE :

  ENDCASE
END

;+
; Start element parsing.
;
; :Params:
;   URI: in, required, type=string
;     Namespace of element
;   Local: in, required, type=string
;     Name of element without namespace
;   Name: in, required, type=string
;     Name of element as found in the XML-file
;   Attr: in, optional, type="string array (1D)"
;    Attribute names
;   Value: in, optional, type="string array (1D)"
;    Attributed values associated with Names
;
; :Categories:
;   PARSING
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_XMLParser::StartElement,URI,Local,Name,Attr,Value

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.chardata = ''

  CASE Name OF

     'mode' : BEGIN

        self.mode = 1

        self.sizes.ntransitions++

        IF self.buffers.ntransitions EQ self.sizes.ntransitions THEN BEGIN

           self.buffers.ntransitions *= 2L

           *self.database.data.transitions = [*self.database.data.transitions, REPLICATE({AmesPAHdb_Transition_S, uid:0L, $
                                                                                          frequency:0D, $
                                                                                          scale:0D, $
                                                                                          intensity:0D, $
                                                                                          symmetry:''}, self.sizes.ntransitions)]
        ENDIF

        (*self.database.data.transitions)[self.sizes.ntransitions].uid = self.current
     END

     'frequency' : BEGIN

        IF SIZE(Attr, /TYPE) GT 0 THEN (*self.database.data.transitions)[self.sizes.ntransitions].scale = DOUBLE(Value[WHERE(Attr EQ 'scale')])
     END

     'atom' : BEGIN

        self.sizes.ngeometries++

        IF self.buffers.ngeometries EQ self.sizes.ngeometries THEN BEGIN

           self.buffers.ngeometries *= 2L

           *self.database.data.geometries = [*self.database.data.geometries, REPLICATE({AmesPAHdb_Geometry_S, $
                                                                                        uid:0L,  $
                                                                                        position:0L, $
                                                                                        x:0D, $
                                                                                        y:0D, $
                                                                                        z:0D, $
                                                                                        type:0},  self.sizes.ngeometries)]
        ENDIF

        (*self.database.data.geometries)[self.sizes.ngeometries].uid = self.current
     END

     'comment' : BEGIN

        IF self.sizes.nspecies GT -1 THEN BEGIN

           self.sizes.ncomments++

           IF self.buffers.ncomments EQ self.sizes.ncomments THEN BEGIN

              self.buffers.ncomments *= 2L

              *self.database.data.comments = [*self.database.data.comments, REPLICATE({AmesPAHdb_Comment_S, $
                                                                                       uid:0L, $
                                                                                       type:'', $
                                                                                       str:''},  self.sizes.ncomments)]
           ENDIF

           IF SIZE(Attr, /TYPE) GT 0 THEN (*self.database.data.comments)[self.sizes.ncomments].type = Value[WHERE(Attr EQ 'type')]

           (*self.database.data.comments)[self.sizes.ncomments].uid = self.current
        ENDIF
     END

     'references' : BEGIN
        self.sizes.nreferences++

        IF self.buffers.nreferences EQ self.sizes.nreferences THEN BEGIN

           self.buffers.nreferences *= 2L

           *self.database.data.references = [*self.database.data.references, REPLICATE({AmesPAHdb_Reference_S, $
                                                                                        uid:0L, $
                                                                                        str:''},  self.sizes.nreferences)]
        ENDIF

        (*self.database.data.references)[self.sizes.nreferences].uid = self.current
     END

     'specie' : BEGIN

        self.current = Value[WHERE(Attr EQ 'uid')]

        self.sizes.nspecies++

        IF self.buffers.nspecies EQ self.sizes.nspecies THEN BEGIN

           self.buffers.nspecies *= 2L

           *self.database.data.species = [*self.database.data.species, REPLICATE({AmesPAHdb_Property_S, $
                                                                                  uid:0L, $
                                                                                  formula:'', $
                                                                                  charge:0, $
                                                                                  symm:'', $
                                                                                  method:'', $
                                                                                  weight:0D, $
                                                                                  energy:0D, $
                                                                                  zeropoint:0D, $
                                                                                  nsolo:0, $
                                                                                  nduo:0, $
                                                                                  ntrio:0, $
                                                                                  nquartet:0, $
                                                                                  nquintet:0, $
                                                                                  nch2:0, $
                                                                                  nchx:0, $
                                                                                  nc:0, $
                                                                                  nh:0, $
                                                                                  nn:0, $
                                                                                  no:0, $
                                                                                  nmg:0, $
                                                                                  nsi:0, $
                                                                                  nfe:0},  self.sizes.nspecies)]
        ENDIF

        (*self.database.data.species)[self.sizes.nspecies].uid = self.current
     END

     'laboratory' : self.storage = PTR_NEW({bytes1:PTR_NEW(), $
                                            bytes2:PTR_NEW()})

     'pahdatabase' : BEGIN

        self.database.type = Value[WHERE(Attr EQ 'database')]

        self.database.version = Value[WHERE(Attr EQ 'version')]

        self.database.date = Value[WHERE(Attr EQ 'date')]

        IF Value[WHERE(Attr EQ 'full')] EQ 'true' THEN self.database.full = 1 $
        ELSE self.database.full = 0
     END

     ELSE:
  ENDCASE
END

;+
; Parse text.
;
; :Params:
;   data: in, required, type=string
;     Character data
;
; :Categories:
;   PARSING
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_XMLParser::Characters,data

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.chardata += data
END

;+
; Parse whitespace between elements.
;
; :Params:
;   data: in, required, type=string
;     Whitespace text
;
; :Categories:
;   PARSING
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_XMLParser::IgnorableWhitespace,data

  COMPILE_OPT IDL2

  ON_ERROR,2
END

;+
; Start document parsing.
;
; :Categories:
;   PARSING
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_XMLParser::StartDocument

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.timer = SYSTIME(/SECONDS)
END

;+
; Handle recoverable parse error.
;
; :Params:
;   SystemID: in, required, type=string
;     Namespace of associated text
;   LineNumber: in, required, type=long
;     Line numer where the error occurred
;   ColumnNumber: in, required, type=long
;     Column number where the error occurred
;   Message: in, optional, type=string
;    Error message
;
; :Categories:
;   PARSING
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_XMLParser::Error,SystemID,LineNumber,ColumnNumber,Message

  COMPILE_OPT IDL2

  ON_ERROR,2

  PRINT
  PRINT,"=============================================="
  PRINT," NON FATAL ERROR: "+STRING(FORMAT='(A0,X,"(",A0,X,": line ",I0,", column:",X,I0,")")',Message,SystemID,LineNumber,ColumnNumber)
  PRINT,"=============================================="
  PRINT
END

;+
; Handle fatal parse error.
;
; :Params:
;   SystemID: in, required, type=string
;     Namespace of associated text
;   LineNumber: in, required, type=long
;     Line numer where the error occurred
;   ColumnNumber: in, required, type=long
;     Column number where the error occurred
;   Message: in, optional, type=string
;    Error message
;
; :Categories:
;   PARSING
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_XMLParser::FatalError,SystemID,LineNumber,ColumnNumber,Message

  COMPILE_OPT IDL2

  ON_ERROR,2

  PRINT
  PRINT,"=============================================="
  PRINT," FATAL ERROR: "+STRING(FORMAT='(A0,X,"(",A0,": line ",I0,", column:",X,I0,")")',Message,SystemID,LineNumber,ColumnNumber)
  PRINT,"=============================================="
  PRINT

  self.status = 1B
END

;+
; Return the status of a AmesPAHdbIDLSuite_XMLParser-instance
;
; :Returns:
;   byte
;
; :Categories:
;   CLASS
;
;-
FUNCTION AmesPAHdbIDLSuite_XMLParser::Status

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,self.status
END

;+
; Clean-up an AmesPAHdbIDLSuite_XMLParser-instance
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_XMLParser::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.storage) THEN PTR_FREE,self.storage

  ntags = N_TAGS(self.database.data)

  FOR i = 0, ntags - 1 DO BEGIN

    IF PTR_VALID(self.database.data.(i)) THEN PTR_FREE,self.database.data.(i)
  ENDFOR

  self->IDLffXMLSAX::Cleanup
END

;+
; Create an AmesPAHdbIDLSuite_XMLParser-instance.
;
; :Returns:
;   AmesPAHdbIDLSuite_XMLParser-instance
;
; :Keywords:
;   SCHEMA_CHECKING: in, optional, type=int
;     Whether to check the structure of the XML
;
; :Categories:
;   CLASS
;-
FUNCTION AmesPAHdbIDLSuite_XMLParser::Init,SCHEMA_CHECKING=Schema_Checking

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.buffers = {AmesPAHdb_Size_S, $
                  nspecies:256L, $
                  ncomments:256L, $
                  nreferences:256L, $
                  ntransitions:256L, $
                  ngeometries:256L, $
                  nlaboratory:256L}

  self.database.data.species = PTR_NEW(REPLICATE({AmesPAHdb_Property_S, $
                                                  uid:0L, $
                                                  formula:'', $
                                                  charge:0, $
                                                  symm:'', $
                                                  method:'', $
                                                  weight:0D, $
                                                  energy:0D, $
                                                  zeropoint:0D, $
                                                  nsolo:0, $
                                                  nduo:0, $
                                                  ntrio:0, $
                                                  nquartet:0, $
                                                  nquintet:0, $
                                                  nch2:0, $
                                                  nchx:0, $
                                                  nc:0, $
                                                  nh:0, $
                                                  nn:0, $
                                                  no:0, $
                                                  nmg:0, $
                                                  nsi:0, $
                                                  nfe:0}, self.buffers.nspecies))

  self.database.data.comments = PTR_NEW(REPLICATE({AmesPAHdb_Comment_S, uid:0L, $
                                                   type:'', $
                                                   str:''}, self.buffers.ncomments))

  self.database.data.references = PTR_NEW(REPLICATE({AmesPAHdb_Reference_S, uid:0L, $
                                                     str:''}, self.buffers.nreferences))

  self.database.data.transitions = PTR_NEW(REPLICATE({AmesPAHdb_Transition_S, uid:0L, $
                                                      frequency:0D, $
                                                      scale:0D, $
                                                      intensity:0D, $
                                                      symmetry:''}, self.buffers.ntransitions))

  self.database.data.laboratory = PTR_NEW(REPLICATE({AmesPAHdb_Laboratory_Spectrum_S, $
                                           uid:0L, $
                                           frequency:0D, $
                                           intensity:0D}, self.buffers.nlaboratory))

  self.database.data.geometries = PTR_NEW(REPLICATE({AmesPAHdb_Geometry_S, $
                                                     uid:0L, $
                                                     position:0L, $
                                                     x:0D, $
                                                     y:0D, $
                                                     z:0D, $
                                                     type:0}, self.buffers.ngeometries))

  self.sizes = {AmesPAHdb_Size_S, $
                nspecies:-1L, $
                ncomments:-1L, $
                nreferences:-1L, $
                ntransitions:-1L, $
                ngeometries:-1L, $
                nlaboratory:-1L}

  self.mode = 0

  RETURN,self->IDLFFXMLSAX::Init(SCHEMA_CHECKING=Schema_Checking)
END

;+
; Defines the AmesPAHdbIDLSuite_XMLParser Class.
;
; :Fields:
;   storage: type=pointer
;     Store decoded a laboratory spectrum
;   status: type=byte
;     Status of the parsing
;   timer: type=double
;     Parsing start time
;   chardata: type=string
;    Character data buffer
;   mode: type=int
;     Whether in a transition 'mode'-element
;   buffers: type=AmesPAHdb_Size_s
;     Track buffer sizes
;   sizes: type=AmesPAHdb_Size_S
;     Track sizes
;   current: type=long
;     Current UID being processed
;   database: type=AmesPAHdb_S
;     Database meta-data
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_XMLParser__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  data = {AmesPAHdb_Data_S, $
          species:PTR_NEW(), $
          comments:PTR_NEW(), $
          references:PTR_NEW(), $
          transitions:PTR_NEW(), $
          geometries:PTR_NEW(), $
          laboratory:PTR_NEW()}

  database = {AmesPAHdb_S, $
              filename:'', $
              type:'', $
              version:'', $
              date:'', $
              full:0, $
              comment:'', $
              data:data}

  size = {AmesPAHdb_Size_S, $
          nspecies:0L, $
          ncomments: 0L, $
          nreferences: 0L, $
          ntransitions:0L, $
          ngeometries:0L, $
          nlaboratory:0L}

  void = {AmesPAHdbIDLSuite_XMLParser, $
          INHERITS IDLffXMLSAX, $
          status:0B, $
          storage:PTR_NEW(), $
          timer:0d, $
          chardata:'', $
          mode:0, $
          buffers:size, $
          sizes:size, $
          current:0L, $
          database:database}
END

; END OF amespahdbidlsuite_xmlparser__define.pro
