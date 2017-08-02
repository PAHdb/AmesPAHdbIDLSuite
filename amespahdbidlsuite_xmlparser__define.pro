;+
; CLASS_NAME:
;    AmesPAHdbIDLSuite_XMLParser
;
; CONTACT:
;
;    Updated versions of the NASA Ames PAH IR Spectroscopic 
;    Database and more information can be found at:
;    http://www.astrochem.org/pahdb
;
; DESCRIPTION:
;    XML parser for reading the NASA Ames PAH IR Spectroscopic
;    Database's XML file
;
; CATEGORY:
;    MAIN
;
; SUPERCLASSES:
;   IDLFFXMLSAX
;
; SUBCLASSES:
;    NONE
;
; REQUIRED CLASSES:
;   IDLFFXMLSAX
;
; OPTIONAL:
;
; CREATION:
;    db = OBJ_NEW('AmesPAHdbIDLSuite_XMLParser')
;
; METHODS
;    PUBLIC:
;      PARSEFILE (PROCEDURE)
;      GETDATABASE (FUNCTION)
;      GETSPECIES (FUNCTION)
;      GETTRANSITIONS (FUNCTION)
;      GETLABORATORY (FUNCTION)
;      GETGEOMETRIES (FUNCTION)
;      GETCOMMENTS (FUNCTION)
;      GETREFERENCES (FUNCTION)
;
;    PRIVATE:
;      STARTDOCUMENT (PROCEDURE)
;      ENDDOCUMENT (PROCEDURE)
;      STARTELEMENT (PROCEDURE)
;      ENDELEMENT (PROCEDURE)
;      IGNORABLEWHITESPACE (PROCEDURE)
;      CHARACTERS (PROCEDURE)
;      ERROR (PROCEDURE)
;      FATALERROR (PROCEDURE)
;      
; EXAMPLE
;    db = OBJ_NEW('AmesPAHdbIDL_SuiteXMLParser', Filename='pahdb-complete-experimental-v1.00.xml')
;    transitions = db->GetTransitions
;    select = WHERE(transitions.uid EQ 18)
;    PLOT,transitions[select].frequency,transitions[select].intensity
;    OBJ_DESTROY,[db]
;
; MODIFICATION HISTORY
;
;   10-04-2015
;   There is no energy-tag. Chrisiaan Boersma
;   02-01-2015
;   First version of the file. Christiaan Boersma.
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

FUNCTION AmesPAHdbIDLSuite_XMLParser::GetSpecies

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,*self.database.data.species
END

FUNCTION AmesPAHdbIDLSuite_XMLParser::GetTransitions

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,*self.database.data.transitions
END

FUNCTION AmesPAHdbIDLSuite_XMLParser::GetLaboratory

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,*self.database.data.laboratory
END

FUNCTION AmesPAHdbIDLSuite_XMLParser::GetGeometries

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,*self.database.data.geometries
END

FUNCTION AmesPAHdbIDLSuite_XMLParser::GetComments

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,*self.database.data.comments
END

FUNCTION AmesPAHdbIDLSuite_XMLParser::GetReferences

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,*self.database.data.references
END

PRO AmesPAHdbIDLSuite_XMLParser::ParseFile,Filename,URL=URL,XML_STRING=XML_STRING

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.database.filename = Filename

  self->IDLffXMLSAX::ParseFile,Filename,URL=URL,XML=XML_STRING
END

PRO AmesPAHdbIDLSuite_XMLParser::EndDocument

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF self.sizes.nspecies GT 0 THEN *self.database.data.species = (*self.database.data.species)[0:self.sizes.nspecies++]

  IF self.sizes.ncomments GT 0 THEN *self.database.data.comments = (*self.database.data.comments)[0:self.sizes.ncomments++]

  IF self.sizes.nreferences GT 0 THEN *self.database.data.references = (*self.database.data.references)[0:self.sizes.nreferences++]

  IF self.sizes.ntransitions GT 0 THEN *self.database.data.transitions = (*self.database.data.transitions)[0:self.sizes.ntransitions++]

  IF self.sizes.ngeometries GT 0 THEN *self.database.data.geometries = (*self.database.data.geometries)[0:self.sizes.ngeometries++]

  IF self.sizes.nlaboratory GT 0 THEN *self.database.data.laboratory = (*self.database.data.laboratory)[1:self.sizes.nlaboratory++]

  PRINT
  PRINT,"========================================================="
  PRINT,"         NASA Ames PAH IR SPECTROSCOPIC DATABASE"
  PRINT,"========================================================="
  PRINT
  PRINT,"========================================================="
  PRINT,FORMAT='(A-30, ": ", A0)',"FILENAME",""+self.filename
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

PRO AmesPAHdbIDLSuite_XMLParser::EndElement,URI,Local,Name

  COMPILE_OPT IDL2

  ON_ERROR,2

  CASE Name OF

     'frequency' : BEGIN

        IF self.mode GT 0 THEN (*self.database.data.transitions)[self.sizes.ntransitions].frequency = DOUBLE(self.chardata) ELSE BEGIN

           (*self.storage)[0].nhex = STRLEN(self.chardata) / 2

           (*self.storage)[0].binary1 = PTR_NEW(BYTARR((*self.storage)[0].nhex))

           READS,FORMAT='('+STRTRIM(STRING((*self.storage)[0].nhex), 2)+'(Z2))',self.chardata,*(*self.storage)[0].binary1
        ENDELSE
     END

     'intensity' : BEGIN

        IF self.mode GT 0 THEN (*self.database.data.transitions)[self.sizes.ntransitions].intensity = DOUBLE(self.chardata) ELSE BEGIN

           (*self.storage)[0].nhex = STRLEN(self.chardata) / 2

           (*self.storage)[0].binary2 = PTR_NEW(BYTARR((*self.storage)[0].nhex))

           IF FILE_WHICH('convert_ascii.so', /INCLUDE_CURRENT_DIR) NE '' THEN *(*self.storage)[0].binary2 = CONVERT_ASCII(self.chardata, /FLOAT, /BIG_ENDIAN) ELSE $
              READS,FORMAT='('+STRING((*self.storage)[0].nhex)+'(Z2))',self.chardata,*(*self.storage)[0].binary2
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
        key = [-1, 17, -1, -1, -1, -1, 16, 18, 19, -1, -1, -1, 20, -1, 21, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 22]
        ;      0   1   2   3   4   5   6   7   8    9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26

        type = FIX(self.chardata)

        (*self.database.data.geometries)[self.sizes.ngeometries].type = type

        (*self.database.data.species)[self.sizes.nspecies].(key[type])++
     END

     'mode' : self.mode = 0

     'formula' : (*self.database.data.species)[self.sizes.nspecies].formula = self.chardata

     'charge' : (*self.database.data.species)[self.sizes.nspecies].charge = FIX(self.chardata)

     'scale' : (*self.database.data.species)[self.sizes.nspecies].scale = DOUBLE(self.chardata)

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

        nfloats = (*self.storage)[0].nhex / 4

        self.sizes.nlaboratory += nfloats

        struct = REPLICATE({AmesPAHdb_Laboratory_Spectrum_S, $
                            uid:0L, $
                            frequency:0D, $
                            intensity:0D}, nfloats)

        struct.uid = self.current

        struct.frequency = DOUBLE(FLOAT(*(*self.storage)[0].binary1, 0, nfloats))

        struct.intensity = DOUBLE(FLOAT(*(*self.storage)[0].binary2, 0, nfloats))

        (*self.database.data.laboratory) = [(*self.database.data.laboratory), struct]

        PTR_FREE,(*self.storage)[0].binary1

        PTR_FREE,(*self.storage)[0].binary2

        PTR_FREE,self.storage
     END

     ELSE :

  ENDCASE
END

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

           *self.database.data.transitions = [*self.database.data.transitions, REPLICATE({AmesPAHdb_Transistion_S, $
                                                                                          uid:0L, $
                                                                                          frequency:0D, $
                                                                                          intensity:0D, $
                                                                                          symmetry:''}, self.sizes.ntransitions)]
        ENDIF

        (*self.database.data.transitions)[self.sizes.ntransitions].uid = self.current
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
                                                                                  scale:0D, $
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

     'laboratory' : self.storage = PTR_NEW({nhex:0L, $
                                            binary1:PTR_NEW(), $
                                            binary2:PTR_NEW()})

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

PRO AmesPAHdbIDLSuite_XMLParser::Characters,data

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.chardata += data
END

PRO AmesPAHdbIDLSuite_XMLParser::IgnorableWhitespace,data

  COMPILE_OPT IDL2

  ON_ERROR,2
END

PRO AmesPAHdbIDLSuite_XMLParser::StartDocument

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.timer = SYSTIME(/SECONDS)
END

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


PRO AmesPAHdbIDLSuite_XMLParser::Error,SystemID,LineNumber,ColumnNumber,Message

  COMPILE_OPT IDL2

  ON_ERROR,2

  PRINT
  PRINT,"=============================================="
  PRINT," NON FATAL ERROR: "+STRING(FORMAT='(A0,X,"(",A0,X,": line ",I0,", column:",X,I0,")")',Message,SystemID,LineNumber,ColumnNumber)
  PRINT,"=============================================="
  PRINT
END

PRO AmesPAHdbIDLSuite_XMLParser::FatalError,SystemID,LineNumber,ColumnNumber,Message

  COMPILE_OPT IDL2

  ON_ERROR,2

  PRINT
  PRINT,"=============================================="
  PRINT," FATAL ERROR: "+STRING(FORMAT='(A0,X,"(",A0,": line ",I0,", column:",X,I0,")")',Message,SystemID,LineNumber,ColumnNumber)
  PRINT,"=============================================="
  PRINT
END

FUNCTION AmesPAHdbIDLSuite_XMLParser::Init,SCHEMA_CHECKING=Schema_Checking

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.buffers = {AmesPAHdb_Size_S, $
                  nspecies:16L, $
                  ncomments: 16L, $
                  nreferences: 16L, $
                  ntransitions:16L, $
                  ngeometries:16L, $
                  nlaboratory:16L}

  self.database.data.species = PTR_NEW(REPLICATE({AmesPAHdb_Property_S, $
                                                  uid:0L, $
                                                  formula:'', $
                                                  charge:0, $
                                                  scale:0D, $
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
