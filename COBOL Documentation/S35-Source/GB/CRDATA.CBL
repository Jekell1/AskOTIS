       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CRDATA.
      ******************************************************************
      *
      *  DIR :   GB
      *
      *  DESC:   CREATE JUST THE DIRECTORIES FOR /USR/DATA/R1/XX????/
      *          
      *
      * REVS:
      * BAH 20191104 PUT THIS PROGRAM BACK JUST TO CREATE THE BRANCH
      *              DATA DIRECTORY AND ITS SUB DIRECTORIES 
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B1.
       OBJECT-COMPUTER. 3B1.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/CRDATA.CBL S35 02/23/24-11:43:24 >".

      *=================================================================
      *
      *    P R O G R A M - W O R K E R S
      *
      *=================================================================
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01PW.CPY".
       COPY "LIBGB/GB01PW_SQL.CPY".
       COPY "LIBGB/GBWSPW.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

      *-----------------------------------------------------------------
      * WSS-FILE-PATH = /USR/DATA/R#/XX####/LP/ICFILE
      *                 |----BEG----||-MID-||-----FILE----|
      *                                        |---SQL----|
      *
      *-----------------------------------------------------------------
       01  WSS-FILE-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                          REDEFINES WSS-FILE-PATH.
           03  WSS-FILE-PATH-BEG           PIC X(13).
           03  WSS-FILE-PATH-OWNST         PIC X(02).
           03  WSS-FILE-PATH-OWNBR         PIC 9(04).
           03  FILLER                      PIC X(01).
           03  WSS-FILE-PATH-FILE          PIC X(15).
           03  FILLER                      REDEFINES WSS-FILE-PATH-FILE.
               05  FILLER                  PIC X(03).
               05  WSS-FILE-PATH-SQL       PIC X(12).
    
      *=================================================================
      *
      *    D A T A - P A T H
      *
      *=================================================================
       01  DATA-PATH.
           03  FILLER                      PIC X(1)        VALUE "/".
           03  FIL-PATH                    PIC X(18)       VALUE SPACES.

       01  DATA-PATHX REDEFINES DATA-PATH.
           03  PATH-START                  PIC X(10).
           03  PATH-MACHINE                PIC X(2).
           03  PATH-SLASH1                 PIC X(1).
           03  PATH-BRANCH                 PIC X(6).

       01  DATA-PATHZ REDEFINES DATA-PATH.
           03  PATH-TEST-ACCESS            PIC X(13).

      *=================================================================
      *
      *    M I S C E L L E N A O U S - W O R K E R S
      *
      *=================================================================
       01  ERRCD                      PIC X           VALUE SPACES.
       01  ERROR-FLAGS.
           03  BRANCH-ERROR-FLAG      PIC X(1)        VALUE "N".
               88  BRANCH-ERROR-FLAG-NO               VALUE "N".
               88  BRANCH-ERROR-FLAG-YES              VALUE "Y".

           03  TOTAL-ERROR-FLAG       PIC X(1)        VALUE "N".
               88  TOTAL-ERROR-FLAG-NO                VALUE "N".
               88  TOTAL-ERROR-FLAG-YES               VALUE "Y".

       01  CREATE-ERROR-MESSAGE.
           03  FILLER                 PIC X(40)       VALUE
               "ERROR(S) OCCURED WHILE CREATING FILE(S) ".
           03  FILLER                 PIC X(12)       VALUE
               "FOR BRANCH: ".

       01  PATH-EXISTS                PIC X(1)        VALUE SPACES.

       01  LEGEND-BUFFER-LIST.
           03  LEGEND                 PIC X(78)       VALUE SPACES.
           03  LEGEND-LIST            PIC X(9)        VALUE "LEGEND:B.".
           03  LEGEND2-LIST           PIC X(7)        VALUE "LEGEND.".


      *=================================================================
      *
      *    P R O G R A M - S T A T U S
      *
      *-----------------------------------------------------------------
      *    A UNIVERSIAL FLAG TO KEEP TRACK OF THE STATUS OF THE PROGRAM.
      *=================================================================
       01  PROG-STATUS              PIC X(1)          VALUE "C".
           88  PROG-STATUS-CONTINUE                   VALUE "C".
           88  PROG-STATUS-ERROR-OCCURED              VALUE "E".
           88  PROG-STATUS-EXIT-PROGRAM               VALUE "X".
           88  PROG-STATUS-NEXT-BRANCH                VALUE "N".


       01  BEG-BRANCH                 PIC 9(4)     VALUE ZEROES.
       01  END-BRANCH                 PIC 9(4)     VALUE ZEROES.

       01  ENT-GROUP                  PIC X(4)     VALUE SPACES.
       01  GROUP-FLAG                 PIC X(1)     VALUE SPACES.
           88  GROUP-FLAG-NO                       VALUE "N".
           88  GROUP-FLAG-YES                      VALUE "Y".

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CRDATA_CMD.CPY".
      *  "/$REL/SH/EC /$REL/SH/CRDATA.SH".
       01  CRDATA-CMD.
      *    03  FILLER            PIC X    VALUE "/".
      *    03  CRDATA-REL        PIC X(7).
      *    03  FILLER            PIC X(4) VALUE "/SH/".
      *    03  CRDATA-EC         PIC X(3) VALUE "EC ".
           03  FILLER            PIC X    VALUE "/".
           03  CRDATA-REL2       PIC X(7).
           03  FILLER            PIC X(4) VALUE "/SH/".
           03  CRDATA-SHELL      PIC X(10) VALUE "CRDATA.SH ".
           03  CRDATA-FIL        PIC X(18).

       01  CRDATA-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/CRDATA_DEF.CPY".
       COPY "LIBGB/CRDATA_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGB/CRDATA_SCN.CPY".
    
       PROCEDURE DIVISION 
             USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CRDATA" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
      *            CELL WIDTH 10
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS CRDATA-WINDOW-HANDLE.

           MOVE EXT-FILPATH TO DATA-PATH.
      D    STOP MESS.

       MAIN-MODULE.

           PERFORM ENTER-PARAMETERS.

           IF ( PROG-STATUS-EXIT-PROGRAM )
              GO TO EXIT-MAIN-PROGRAM.

           PERFORM PROCESS-ENTERED-RANGE.

           GO TO MAIN-MODULE.

       EXIT-MAIN-PROGRAM.

           GO TO END-ROUTINE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       BEGIN-ENTER-PARAMETERS.

           PERFORM FORM-RESET.

       ENTER-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "R  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY ) GO TO BEGIN-ENTER-PARAMETERS.
           IF ( F7-KEY ) GO TO END-ENTER-PARAMETERS.
           IF ( F6-KEY )
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO ENTER-BRANCH
              ELSE
                 MOVE "S  A000BEG-BRANCH." TO VDU
                 MOVE GRSCAN-KEY           TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BRANCH.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( WGR-WORD-ZERO )
              MOVE ZEROES TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

           IF ( WGR-TYPE = "A" )
              MOVE "Y"    TO GROUP-FLAG
              MOVE ZEROES TO BEG-BRANCH END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"         TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH END-BRANCH.

           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.

           IF ( BEG-BRANCH = ZEROES )
              MOVE "INVALID BRANCH NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" )
              GO TO ENTER-BRANCH.

      *-----------------------------------------------------------------
       SEND-BRANCH-GROUP-DESC.

           IF ( GROUP-FLAG-YES )
              MOVE ENT-GROUP  TO GR-ID
              PERFORM OPEN-GR1-FILE
              PERFORM READ-GR1-FILE
              PERFORM CLOSE-GR1-FILE
              IF ( IO-GOOD )
                 MOVE "S  A000BRNAME." TO VDU
                 MOVE GR-NAME          TO VDUBUF
                 PERFORM SCREENIO
              ELSE
                 MOVE "INVALID GROUP ID" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-BRANCH.

           IF ( GROUP-FLAG-NO )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              PERFORM CLOSE-BR-FILE
              IF ( IO-GOOD )
                 MOVE "S  A000BRNAME." TO VDU
                 MOVE BR-NAME          TO VDUBUF
                 PERFORM SCREENIO
              ELSE
                 MOVE "INVALID BRANCH NUMBER" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-BRANCH.

       SEND-DATA-PATH-DESC.

           IF ( GROUP-FLAG-YES )
              MOVE SPACES         TO PATH-MACHINE
              MOVE SPACES         TO PATH-SLASH1
              MOVE SPACES         TO PATH-BRANCH
           ELSE
              MOVE BR-MACHINE     TO PATH-MACHINE
              MOVE "/"            TO PATH-SLASH1
              MOVE BR-DATA-PATH   TO PATH-BRANCH
              MOVE EXT-FILPATH-BASE TO FILPATH-BASE
              MOVE BR-MACHINE     TO FILPATH-MACHINE
              MOVE BR-DATA-PATH   TO FILPATH-DATA
              MOVE DATA-PATH      TO VDUBUF DIR-ACCESS-BUF
              PERFORM DIR-ACCESS-CALL
              IF NOT ( STAT-BAD )
                 MOVE "Y"                        TO PATH-EXISTS
                 MOVE "DATA PATH EXISTS"         TO LEGEND
                 PERFORM SEND-LEGEND.

           MOVE "S  A000DATA." TO VDU.
           MOVE DATA-PATH      TO VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-CREATE.
           MOVE LEGEND-ENTER-ACTION TO LEGEND.
           PERFORM SEND-LEGEND.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BRANCH.
           IF (        F1-KEY ) GO TO BEGIN-ENTER-PARAMETERS.
           IF (        F7-KEY ) GO TO END-ENTER-PARAMETERS.
           IF ( NOT ENTER-KEY ) GO TO ENTER-CREATE.

           IF VDUBUF = "N"
              GO TO EXIT-PROG.

           IF VDUBUF NOT = "Y"
              GO TO ENTER-CREATE.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

           GO TO EXIT-ENTER-PARAMETERS.

       END-ENTER-PARAMETERS.

           MOVE "X" TO PROG-STATUS.

       EXIT-ENTER-PARAMETERS.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - E N T E R E D - R A N G E
      *
      ******************************************************************
       PROCESS-ENTERED-RANGE SECTION.
      *-----------------------------------------------------------------
       BEGIN-PROCESS-RANGE.

           MOVE "N" TO BRANCH-ERROR-FLAG
                        TOTAL-ERROR-FLAG.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           IF ( GROUP-FLAG-NO )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
       NEXT-GROUP-LOC.

           IF ( GROUP-FLAG-NO )
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF ( WGR-SRESULT = ZEROES )
              GO TO END-PROCESS-RANGE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.

           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-BAD ) OR ( BR-NO NOT = BEG-BRANCH )
              GO TO END-PROCESS-RANGE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE BR-MACHINE   TO PATH-MACHINE.
           MOVE "/"          TO PATH-SLASH1.
           MOVE BR-DATA-PATH TO PATH-BRANCH.

           MOVE "S  A000DATA." TO VDU.
           MOVE DATA-PATH      TO VDUBUF.
           PERFORM SCREENIO.

      D    STOP MESS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  TEST PATH ACCESS
      *  --- TO PREVENT BOMBING OUT IF BRANCH'S FILES NOT ON THIS SYSTEM
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE DATA-PATH TO DIR-ACCESS-BUF.
           MOVE PATH-TEST-ACCESS TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "N"                        TO PATH-EXISTS
              MOVE "DATA PATH DOES NOT EXIST" TO LEGEND
           ELSE
              MOVE "Y"                        TO PATH-EXISTS
              MOVE "DATA PATH EXISTS"         TO LEGEND.

           PERFORM SEND-LEGEND.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE "CREATING BRANCH DIRECTORIES ....." TO LEGEND.
           PERFORM SEND-LEGEND.

      * /$REL/SH/EC /$REL/SH/SH/CRDATA.SH
           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
      *    MOVE GETENV-BUF TO CRDATA-REL CRDATA-REL2.
           MOVE GETENV-BUF TO CRDATA-REL2.
      *    CALL "C$TOLOWER" USING CRDATA-EC, VALUE 3.
           CALL "C$TOLOWER" USING CRDATA-SHELL, VALUE 10.
           MOVE FIL-PATH TO CRDATA-FIL.
      D    STOP MESS.
           MOVE CRDATA-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.


           IF ( GROUP-FLAG-YES        ) AND
              ( BRANCH-ERROR-FLAG-YES )
              MOVE SPACES TO LEGEND
              PERFORM SEND-LEGEND
              STRING CREATE-ERROR-MESSAGE, BR-NO
                     DELIMITED "  " INTO MESS
              PERFORM SEND-MESS.

           GO TO NEXT-GROUP-LOC.

      *=================================================================
       END-PROCESS-RANGE.

           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GR1-FILE.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

       EXIT-PROCESS-RANGE.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ************************************************************
       FORM-RESET SECTION.
      ************************************************************
      * MUST BE HERE FOR F1 CLEAR TO WORK
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CRDATA-SCREEN UPON CRDATA-WINDOW-HANDLE.
           MOVE CRDATA-FORM-FIELDS TO WR-BUF. |MJD001
           MOVE CRDATA-FORM-LIST TO WR-LIST. |MJD001
           PERFORM CALL-WRFORM. |MJD001


      ************************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************************
           MOVE CRDATA-HELP--FILE TO HELP-LINK-FILE.
           MOVE CRDATA-HELP--DIR TO HELP-LINK-DIR.

      ************************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/CRDATA_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    M I S C E L L A N O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISC SECTION.
      *-----------------------------------------------------------------
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SEND-LEGEND2.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND2-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       EXIT-PROG.

           MOVE "GB/INSTMU"  TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN |MJD001
           DESTROY CRDATA-SCREEN.
           DESTROY CRDATA-WINDOW-HANDLE.
           EXIT PROGRAM.
