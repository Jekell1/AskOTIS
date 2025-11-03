       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TZMAIN.
      ******************************************************************
      *
      *  DESC:   TIME ZONE FILE MAINTENANCE
      *
      * REV:
      * BLV 010129 INCREASE DISPLAY BUF & ENTRY SPEC FROM 9- TO Z9-, WAS
      *            TRUNCATING 2 DIGIT TIME ZONE FACTORS (FOR EX. HAWAII)
      * AJM 130120 A30 CONVERSION -
      * BAH 190710 ADDED CLEAR ROUTINE, USE LPTZCL
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./MN/TZMAIN.CBL J35 04/04/24-10:11:54 >".

      ******************************************************************
      *
      *     P R O G R A M     W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01TZ.CPY".
       COPY "LIBLP/LP01TZ_SQL.CPY".
       COPY "LIBLP/LPWSTZ.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       01  ERRCD           PIC X       VALUE " ".
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).

       01  CHAN-DATA       VALUE SPACES.
           03  CHAN        PIC X(4).

       01  TZMAIN-WINDOW-HANDLE  PIC X(10).
       01  ELE             PIC 9(3).
       01  FIRST-TIME      PIC 9       VALUE 0.
       01  HOLD-TZ1-KEY.
           03  HOLD-TZ-CODE   PIC 9(03).
           03  HOLD-TZ-PREFIX PIC 9(03).

       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  ONE             PIC S9.

       01  ORIGINAL-TZ-REC      PIC X(TZ-SIZE).
       01  CHANGED-TZ-REC       PIC X(TZ-SIZE).

      ******************************************************************
      *     SCREEN DISPLAY BUFFER
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X(40).
           03  F02         PIC X(02).
           03  F021        PIC X(20).
           03  F03         PIC X(01).
           03  F031        PIC X(20).
           03  F04         PIC X(01).
           03  F041        PIC X(20).
           03  F05         PIC Z9-.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(32) VALUE
           "010 020 021 030 031 040 041 050.".

      ******************************************************************
      *     ELEMENT SPECIFICATIONS
      ******************************************************************
       78  MAX                      VALUE 05.
       01  SPEC-TABLE.
      * 1. AREA CODE DESCRIPTION
           03  FILLER      PIC X(8) VALUE "ENNA400 ".
      * 2. STATE
           03  FILLER      PIC X(8) VALUE "ENNA020 ".
      * 3. TIME ZONE CODE
           03  FILLER      PIC X(8) VALUE "ENNA010 ".
      * 4. DAYLIGHT SAVINGS FLAG
           03  FILLER      PIC X(8) VALUE "ENNA010 ".
      * 5. TIME ZONE OFFSET
           03  FILLER      PIC X(8) VALUE "ENNS020N".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

      /*****************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/TZSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/TZMAIN_DEF.CPY".
       COPY "LIBMN/TZMAIN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/TZMAIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N   P R O G R A M    M O D U L E
      *
      ******************************************************************
       A000-MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TZMAIN"      TO VDU-FORM-NAME.
           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
                   HANDLE IS TZMAIN-WINDOW-HANDLE.


       A010-MAIN-MODULE.
           PERFORM FORM-RESET.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME.

           PERFORM B010-REC-TZ-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM B020-ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM B030-WRITE-MODULE.
           IF IO-TYPE = "DEL"
             PERFORM B040-DELETE-MODULE.

           GO TO A010-MAIN-MODULE.

      ******************************************************************
       B010-REC-TZ-MODULE SECTION.
      ******************************************************************
       B010-REC-TZ-MODULE-NO.
           MOVE 0      TO NEW-FLAG TZSCAN-TZ.
           MOVE SPACES TO IO-TYPE.

           MOVE "F7-EXIT, F6-SCAN, ENTER AREA CODE:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN030AC." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUNUM0 TO TZ-CODE HOLD-TZ-CODE.

           IF F6-KEY
              PERFORM TZ-SCAN
              IF IO-FG NOT = 0
                 GO TO B010-REC-TZ-MODULE-NO
              ELSE
                 MOVE "SN N030AC." TO VDU
                 MOVE TZSCANW-AC TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE "SN N030PF." TO VDU
                 MOVE TZSCANW-PF TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE TZSCAN-KEY TO TZ1-KEY
                 GO TO READ-TZFILE.

           IF F7-KEY
             MOVE "END" TO IO-TYPE
             GO TO END-B010-REC-TZ-MODULE.

           IF NOT ENTER-KEY
              GO TO B010-REC-TZ-MODULE-NO.

           IF VDUNUM0 = 0
              GO TO B010-REC-TZ-MODULE-NO.


       B010-ENTER-PREFIX.
           MOVE "F7-EXIT, F6-SCAN, ENTER PREFIX:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN030PF." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUNUM0 TO TZ-PREFIX HOLD-TZ-PREFIX.
           IF F7-KEY
             MOVE "END" TO IO-TYPE
             GO TO END-B010-REC-TZ-MODULE.

           IF F6-KEY
              MOVE TZ-CODE TO TZSCANW-AC
              MOVE "Y"     TO TZSCAN-PF-ONLY
              PERFORM TZ-SCAN
              IF IO-FG NOT = 0
                 GO TO B010-REC-TZ-MODULE-NO
              ELSE
                 MOVE "SN N030AC." TO VDU
                 MOVE TZSCANW-AC TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE "SN N030PF." TO VDU
                 MOVE TZSCANW-PF TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE TZSCAN-KEY TO TZ1-KEY
                 GO TO READ-TZFILE.

           IF UP-KEY
              GO TO B010-REC-TZ-MODULE-NO.

           IF NOT ENTER-KEY
              GO TO B010-ENTER-PREFIX.

       READ-TZFILE.
           PERFORM GET-TZ.
           IF IO-GOOD
              MOVE TZ-REC TO ORIGINAL-TZ-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.

       END-B010-REC-TZ-MODULE.
           EXIT.

      ******************************************************************
       TZ-SCAN SECTION.
      ******************************************************************
           MOVE "WI/TZSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH TZSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( TZSCAN-STATUS NOT = "00" ) OR TZSCAN-KEY = 0
              MOVE 9 TO IO-FG
              GO TO TZ-SCAN-EXIT.

           MOVE 0               TO IO-FG.
       TZ-SCAN-EXIT.
           EXIT.

      ******************************************************************
      *    B 0 2 0 - E N T R Y - M O D U L E
      *
      *      NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                 1 - INITIAL NEW RECORD ENTRY
      *                 2 - MODIFY NEW RECORD
      ******************************************************************
       B020-ENTRY-MODULE SECTION.
           MOVE SPACES  TO IO-TYPE.
           MOVE 1       TO ONE.

       B020-CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO B020-MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ELE < 1
              MOVE 1 TO ELE ONE.

           IF ELE NOT > MAX
              GO TO B020-ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *-----------------------------------------------------------------
       B020-MODIFY-ROUTINE.
           MOVE "F1, 'DELETE', ENTER FIELD # :" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-B020-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO B020-MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-B020-ENTRY-MODULE.

           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-B020-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF CHNO NOT NUMERIC
              GO TO B020-MODIFY-ROUTINE.

           IF CHNO = 0
              GO TO B020-MODIFY-ROUTINE.

           IF CHNO > MAX
              GO TO B020-MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
       B020-ENTER-ELE.

           IF ELE = 3 OR 4
              PERFORM TEST-VALID-FIELDS.

           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO B020-MODIFY-ROUTINE.

           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO B020-CREATE-ROUTINE
              ELSE
                 IF CHKEY NOT = CHAN
                    GO TO B020-MODIFY-ROUTINE.

           EVALUATE ELE
              WHEN 1 MOVE "F1-CLR, ENTER AREA CODE DESCRIPTION:"
                          TO MESS
              WHEN 2 MOVE "F1-CLR, ENTER STATE FOR THE AREA CODE:"
                          TO MESS
              WHEN 3 MOVE "F1-CLR, ENTER THE TIME ZONE CODE:"
                          TO MESS
              WHEN 4 MOVE "F1-CLR, IS DAYLIGHT SAVINGS TIME OBSERVED?:"
                          TO MESS
              WHEN 5 MOVE "F1-CLR, ENTER THE TIME ZONE OFFSET FACTOR:"
                          TO MESS
           END-EVALUATE.
              PERFORM SEND-LEGEND.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE 0         TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       B020-TEST-F4.
           IF UP-KEY
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       B020-TEST-F5.
           IF DOWN-KEY
              MOVE 1 TO ONE
              GO TO B020-GOTO-FIELD.

      *-----------------------------------------------------------------
       B020-TEST-F6.
           IF F6-KEY
              GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       B020-TEST-F1.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-B020-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO B020-ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       B020-GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 DEPENDING ON ELE.

           GO TO END-B020-ENTRY-MODULE.

      *-----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS
      *
      *-----------------------------------------------------------------
       F-01.
           MOVE VDUBUF TO TZ-DESC.
           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO TZ-STATE ST-CODE.
           PERFORM GET-ST
           IF IO-BAD
              MOVE ST-DESC TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           MOVE "S  A020021." TO VDU.
           MOVE ST-DESC TO VDUBUF.
           PERFORM SCREENIO.
           PERFORM TEST-VALID-FIELDS.
           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO TZ-ZONE
           IF NOT TZ-IS-VALID
              MOVE " PLEASE ENTER A VALID TIME ZONE CODE" TO MESS
              PERFORM SEND-MESS
              GO TO B020-ENTER-ELE.

           MOVE "S  A020031." TO VDU.
           EVALUATE TRUE
              WHEN TZ-IS-EASTERN    MOVE "EASTERN"  TO VDUBUF
                                    MOVE -5 TO TZ-FACTOR
              WHEN TZ-IS-CENTRAL    MOVE "CENTRAL"  TO VDUBUF
                                    MOVE -6 TO TZ-FACTOR
              WHEN TZ-IS-MOUNTAIN   MOVE "MOUNTAIN" TO VDUBUF
                                    MOVE -7 TO TZ-FACTOR
              WHEN TZ-IS-PACIFIC    MOVE "PACIFIC"  TO VDUBUF
                                    MOVE -8 TO TZ-FACTOR
              WHEN TZ-IS-ALASKA     MOVE "ALASKA"   TO VDUBUF
                                    MOVE -9 TO TZ-FACTOR
              WHEN TZ-IS-HAWAII     MOVE "HAWAII"   TO VDUBUF
                                    MOVE -10 TO TZ-FACTOR
              WHEN OTHER            MOVE "INVALID"  TO VDUBUF
           END-EVALUATE.
           PERFORM SCREENIO.
           MOVE "S  S010050." TO VDU
           MOVE TZ-FACTOR TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM TEST-VALID-FIELDS.

           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO TZ-DLS.

           IF NOT ( TZ-DLS = "Y" OR "N" )
              PERFORM INVALID-CODE
              GO TO B020-ENTER-ELE.

           MOVE "S  A020041." TO VDU.
           IF TZ-DLS = "Y"
              MOVE "OBSERVED" TO VDUBUF
           ELSE
              MOVE "NOT OBSERVED" TO VDUBUF.
           PERFORM SCREENIO.

           GO TO B020-CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUNUM0 TO TZ-FACTOR.
           GO TO B020-CREATE-ROUTINE.

       END-B020-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       B030-WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-TZ1-FILE.

           IF ( NEW-FLAG NOT = 0 )
              GO TO B030-WRITE-RECORD.

           MOVE TZ-REC TO CHANGED-TZ-REC.

           IF ( ORIGINAL-TZ-REC = CHANGED-TZ-REC )
              GO TO END-B030-WRITE-MODULE.

           PERFORM READ-TZ1-FILE.

           IF ( IO-FG NOT = 0 )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-B030-WRITE-MODULE.

           IF ( TZ-REC NOT = ORIGINAL-TZ-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-B030-WRITE-MODULE.

           MOVE CHANGED-TZ-REC TO TZ-REC.
           PERFORM REWRITE-TZ1-FILE.
           GO TO END-B030-WRITE-MODULE.

      *-----------------------------------------------------------------
       B030-WRITE-RECORD.
           PERFORM WRITE-TZ1-FILE.

           IF ( IO-FG NOT = 0 )
              PERFORM ALREADY-CREATED.

       END-B030-WRITE-MODULE.
           PERFORM CLOSE-TZ1-FILE.

      ******************************************************************
       B040-DELETE-MODULE SECTION.
      ******************************************************************
           IF ( NEW-FLAG NOT = 0 )
              GO TO END-B040-DELETE-MODULE.

           PERFORM OPEN-TZ1-FILE.
           PERFORM READ-TZ1-FILE.

           IF ( IO-FG NOT = 0 )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-B040-DELETE-MODULE.

           IF ( TZ-REC NOT = ORIGINAL-TZ-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-B040-DELETE-MODULE.

           PERFORM DELETE-TZ1-FILE.

       END-B040-DELETE-MODULE.
           PERFORM CLOSE-TZ1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1                     TO NEW-FLAG.
           MOVE "S  A000NEW."         TO VDU.
           MOVE "*** NEW RECORD ***"  TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-TZ-FILE.

           MOVE HOLD-TZ1-KEY  TO TZ1-KEY.

           MOVE 0         TO ELE.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES               TO DISPLAY-BUF.
           MOVE TZ-DESC              TO F01.
           MOVE TZ-STATE             TO F02 ST-CODE.
           PERFORM GET-ST.
           MOVE ST-DESC              TO F021.
           MOVE TZ-ZONE              TO F03.
           EVALUATE TRUE
              WHEN TZ-IS-EASTERN    MOVE "EASTERN"  TO F031
              WHEN TZ-IS-CENTRAL    MOVE "CENTRAL"  TO F031
              WHEN TZ-IS-MOUNTAIN   MOVE "MOUNTAIN" TO F031
              WHEN TZ-IS-PACIFIC    MOVE "PACIFIC"  TO F031
              WHEN TZ-IS-ALASKA     MOVE "ALASKA"   TO F031
              WHEN TZ-IS-HAWAII     MOVE "HAWAII"   TO F031
              WHEN OTHER            MOVE "INVALID"  TO F031
           END-EVALUATE.
           MOVE TZ-DLS               TO F04.
           IF TZ-DLS = "Y"
              MOVE "OBSERVED"        TO F041
           ELSE
              MOVE "NOT OBSERVED"    TO F041.
           MOVE TZ-FACTOR            TO F05.

           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF  TO WR-BUF.
           PERFORM CALL-WRFORM.

      ******************************************
       TEST-VALID-FIELDS SECTION.
      ******************************************
           IF TZ-STATE = "HI" OR (TZ-STATE = "IN" AND TZ-IS-EASTERN)
              MOVE "S  A010040." TO VDU
              MOVE "N" TO VDUBUF TZ-DLS
              PERFORM SCREENIO
              MOVE "S  A020041." TO VDU
              IF TZ-DLS = "Y"
                 MOVE "OBSERVED" TO VDUBUF
              ELSE
                 MOVE "NOT OBSERVED" TO VDUBUF
              END-IF
              PERFORM SCREENIO
              MOVE "N" TO CHFG(4)
           ELSE
              MOVE " " TO CHFG(4).

           IF TZ-STATE = "AK"
              MOVE "S  A010030." TO VDU
              MOVE "A" TO VDUBUF TZ-ZONE
              PERFORM SCREENIO
              MOVE "S  A200031." TO VDU
              MOVE "ALASKA"   TO VDUBUF
              PERFORM SCREENIO
              MOVE "N" TO CHFG(3)
           ELSE
           IF TZ-STATE = "HI"
              MOVE "S  A010030." TO VDU
              MOVE "H" TO VDUBUF TZ-ZONE
              PERFORM SCREENIO
              MOVE "S  A200031." TO VDU
              MOVE "HAWAII"   TO VDUBUF
              PERFORM SCREENIO
              MOVE "N" TO CHFG(3)
           ELSE
              MOVE " " TO CHFG(3).

      ******************************************************************
      *
      *     C O P Y   L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPTZCL.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO TZMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TZMAIN-SYSDATE.
           DISPLAY TZMAIN-SCREEN UPON TZMAIN-WINDOW-HANDLE.
           MOVE TZMAIN-FORM-FIELDS TO WR-BUF.
           MOVE TZMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE TZMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE TZMAIN-HELP--DIR  TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/TZMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/TMMSG.CPY".

      *-----------------------------------------------------------------
       GET-TZ.
           PERFORM OPEN-TZ1-FILE.
           PERFORM READ-TZ1-FILE.
           PERFORM CLOSE-TZ1-FILE.
      *-----------------------------------------------------------------
       GET-ST.
           PERFORM OPEN-ST1-FILE.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.
           IF IO-BAD
              MOVE "INVALID STATE CODE" TO ST-DESC.
      *-----------------------------------------------------------------
       GET-BR.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE "S  A400MSEL."  TO VDU.
           MOVE MESS            TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       INVALID-CODE.
           MOVE " ENTER 'Y' OR 'N'" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-TZ1-FILE.
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTZGS_SQL.CPY".
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPTZ1IN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "LP/LPCPMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TZMAIN-SCREEN.
           DESTROY TZMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
